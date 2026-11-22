{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.TLS.IO where

import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isRight)
import Data.IORef
import Network.HTTP2.Client (HTTP2Error, pattern ConnectionIsTimeout)
import Network.Socket
import Network.Socket.BufferPool
import qualified Network.Socket.ByteString as NSB
import Network.TLS hiding (HostName)
import System.IO.Error (isEOFError)
import qualified System.IO.Error as E

import Network.HTTP2.TLS.Server.Settings
import Network.HTTP2.TLS.Watchdog

----------------------------------------------------------------

-- HTTP2: confReadN == recvTLS
-- TLS:   recvData  == contextRecv == backendRecv

----------------------------------------------------------------

-- | A receiving function which gives up when the 'STM' action fires
--   first.  It waits in 'STM', so the timeout's kill reaches it on
--   Windows too.
mkRecvTCP :: Settings -> STM () -> Socket -> IO (IO ByteString)
mkRecvTCP settings giveUp sock = do
    recvWith' <- mkRecvTCPWith settings sock
    return $ recvWith' giveUp >>= orTimeout

-- | A receiving function which takes what it gives up on at each call.
mkRecvTCPWith :: Settings -> Socket -> IO (STM g -> IO (Either g ByteString))
mkRecvTCPWith Settings{..} sock = do
    pool <- newBufferPool settingsReadBufferLowerLimit settingsReadBufferSize
    return $ receiveWith sock pool

orTimeout :: Either () a -> IO a
orTimeout = either (\_ -> E.throwIO ConnectionIsTimeout) return

sendTCP :: Socket -> ByteString -> IO ()
sendTCP sock = NSB.sendAll sock

-- | 'sendTCP' which gives up when the 'STM' action fires first.
sendTCPWith :: Socket -> ByteString -> STM g -> IO (Either g ())
sendTCPWith = NSB.sendAllWith

-- | Giving up on what the user gives, or with 'ConnectionIsTimeout' on
--   what the connection gives.
orExpired
    :: (STM (Either g ()) -> IO (Either (Either g ()) a))
    -> STM g
    -> STM ()
    -> IO (Either g a)
orExpired io giveUp expired = do
    r <- io $ (Left <$> giveUp) `orElse` (Right <$> expired)
    case r of
        Right x -> return $ Right x
        Left (Left g) -> return $ Left g
        Left (Right ()) -> E.throwIO ConnectionIsTimeout

----------------------------------------------------------------

-- | Sending and receiving functions.
--   Tiemout is reset when they return.
--   One exception is the slowloris attach prevention.
--   See 'settingsSlowlorisSize'.
data IOBackend = IOBackend
    { send :: ByteString -> IO ()
    -- ^ Sending.
    , sendWith :: forall g. ByteString -> STM g -> IO (Either g ())
    -- ^ Sending which gives up when the 'STM' action succeeds first,
    --   returning what it gave as 'Left'.  Part of the data may have gone
    --   by then, so the connection is to be closed.
    --
    --   @since 0.6.0
    , sendMany :: [ByteString] -> IO ()
    -- ^ Sending many.
    , sendManyWith :: forall g. [ByteString] -> STM g -> IO (Either g ())
    -- ^ 'sendMany' which gives up as 'sendWith' does.
    --
    --   @since 0.6.0
    , recv :: IO ByteString
    -- ^ Receiving.
    , recvWith :: forall g. STM g -> IO (Either g ByteString)
    -- ^ Receiving which gives up when the 'STM' action succeeds first,
    --   returning what it gave as 'Left'.  Part of a TLS record may have
    --   been read by then, so the connection is to be closed.
    --
    --   @since 0.6.0
    , requestSock :: Socket
    -- ^ The socket for the request
    , mySockAddr :: SockAddr
    , peerSockAddr :: SockAddr
    }

-- | Reporting progress to the watchdog of the connection.  A read too
--   small to be anything but a slowloris does not count; see
--   'settingsSlowlorisSize'.
watchedIOBackend :: TLSWatchdog -> Settings -> IOBackend -> IOBackend
watchedIOBackend wd Settings{..} IOBackend{..} =
    IOBackend
        { send = send'
        , sendWith = sendWith'
        , sendMany = sendMany'
        , sendManyWith = sendManyWith'
        , recv = recv'
        , recvWith = recvWith'
        , ..
        }
  where
    send' bs = send bs >> progress wd
    sendWith' :: ByteString -> STM g -> IO (Either g ())
    sendWith' bs giveUp = do
        r <- sendWith bs giveUp
        when (isRight r) $ progress wd
        return r
    sendMany' bss = sendMany bss >> progress wd
    sendManyWith' :: [ByteString] -> STM g -> IO (Either g ())
    sendManyWith' bss giveUp = do
        r <- sendManyWith bss giveUp
        when (isRight r) $ progress wd
        return r
    recv' = do
        bs <- recv
        tickleIfLarge bs
        return bs
    recvWith' :: STM g -> IO (Either g ByteString)
    recvWith' giveUp = do
        r <- recvWith giveUp
        either (\_ -> return ()) tickleIfLarge r
        return r
    tickleIfLarge bs = when (BS.length bs > settingsSlowlorisSize) $ progress wd

-- | The 'TVar's are the ones given to 'mkBackend': what a send and a
--   receive of the backend give up on.
tlsIOBackend
    :: TVar (STM ()) -> TVar (STM ()) -> Context -> Socket -> IO IOBackend
tlsIOBackend giveUpVar recvGiveUpVar ctx sock = do
    mysa <- getSocketName sock
    peersa <- getPeerName sock
    return $
        IOBackend
            { send = sendTLS ctx
            , sendWith = \bs -> givingUpWith giveUpVar $ sendTLS ctx bs
            , sendMany = sendManyTLS ctx
            , sendManyWith = \bss -> givingUpWith giveUpVar $ sendManyTLS ctx bss
            , recv = recvTLS ctx
            , recvWith = givingUpWith recvGiveUpVar $ recvTLS ctx
            , requestSock = sock
            , mySockAddr = mysa
            , peerSockAddr = peersa
            }

-- | Running a TLS action whose backend gives up on what the 'TVar'
--   holds, which is the 'STM' action for the while.
givingUpWith :: TVar (STM ()) -> IO a -> STM g -> IO (Either g a)
givingUpWith var io giveUp = do
    atomically $ writeTVar var $ void giveUp
    ((Right <$> io) `E.catch` gaveUp giveUp)
        `E.finally` atomically (writeTVar var retry)

-- | Everything here also gives up once the watchdog has given up on the
--   connection, rather than waiting for the peer for ever.
tcpIOBackend :: Settings -> TLSWatchdog -> Socket -> IO IOBackend
tcpIOBackend Settings{..} wd sock = do
    pool <- newBufferPool settingsReadBufferLowerLimit settingsReadBufferSize
    mysa <- getSocketName sock
    peersa <- getPeerName sock
    let expired = timedOutSTM wd
        recvWith' :: STM g -> IO (Either g ByteString)
        recvWith' = receiveWith sock pool
    return $
        IOBackend
            { send = \bs -> sendTCPWith sock bs expired >>= orTimeout
            , sendWith = \bs giveUp -> orExpired (sendTCPWith sock bs) giveUp expired
            , sendMany = \_ -> return ()
            , sendManyWith = \bss giveUp ->
                orExpired (sendTCPWith sock $ BS.concat bss) giveUp expired
            , recv = recvWith' expired >>= orTimeout
            , recvWith = \giveUp -> orExpired recvWith' giveUp expired
            , requestSock = sock
            , mySockAddr = mysa
            , peerSockAddr = peersa
            }

----------------------------------------------------------------

sendTLS :: Context -> ByteString -> IO ()
sendTLS ctx = afterCloseNotify . sendData ctx . LBS.fromStrict

sendManyTLS :: Context -> [ByteString] -> IO ()
sendManyTLS ctx = afterCloseNotify . sendData ctx . LBS.fromChunks

-- | tls answers the peer's close_notify with its own, so a send after it
--   has nowhere to go: dropped, as the GOAWAY which closes the connection.
afterCloseNotify :: IO () -> IO ()
afterCloseNotify = E.handle $ \e -> case e of
    PostHandshake Error_EOF -> return ()
    _ -> E.throwIO e

-- TLS version of recv (decrypting) without a cache.
recvTLS :: Context -> IO ByteString
recvTLS ctx = E.handle onEOF $ recvData ctx
  where
    onEOF e
        | Just (PostHandshake Error_EOF) <- E.fromException e = return ""
        | Just ioe <- E.fromException e, isEOFError ioe = return ""
        | otherwise = E.throwIO e

----------------------------------------------------------------

-- | A send of the backend gives up on what the first 'TVar' holds, which
--   the http2 library sets once it sends through 'sendWith', and a
--   receive on what the second holds, which 'recvWith' sets.
--   Both also give up once the watchdog has given up on the connection,
--   which is how the TLS handshake is timed out, or the last 'STM' action
--   has succeeded.
mkBackend
    :: Settings
    -> TLSWatchdog
    -> TVar (STM ())
    -> TVar (STM ())
    -> STM ()
    -> Socket
    -> IO TLSBackend
mkBackend settings wd giveUpVar recvGiveUpVar giveUpAll sock = do
    let expired = timedOutSTM wd `orElse` giveUpAll
        send' bs = do
            giveUp <- readTVarIO giveUpVar
            sendTCPWith sock bs (giveUp `orElse` expired) >>= orTimeout
    recvWith' <- mkRecvTCPWith settings sock
    -- What the caller of 'backendRecvWith' gives up on, during its call.
    stopRef <- newIORef Nothing
    let recv' = do
            giveUp <- readTVarIO recvGiveUpVar
            stop <- maybe retry id <$> readIORef stopRef
            r <- recvWith' $ (True <$ stop) `orElse` (False <$ (giveUp `orElse` expired))
            case r of
                Right bs -> return bs
                -- Given up on by the caller of 'RecvWith', which sees it.
                Left True -> return ""
                Left False -> E.throwIO ConnectionIsTimeout
    recvN <- makeRecvN "" recv'
    let backendRecvWith = RecvWith $ \n stop -> do
            given <- newTVarIO Nothing
            writeIORef stopRef $ Just $ stop >>= writeTVar given . Just
            bs <- recvN n `E.finally` writeIORef stopRef Nothing
            maybe (Right bs) Left <$> readTVarIO given
        backend =
            Backend
                { backendFlush = return ()
                , backendClose =
                    gracefulClose sock 5000 `E.catchIOError` \_ -> return ()
                , backendSend = send'
                , backendRecv = recvN
                }
    return $ TLSBackend backend backendRecvWith

-- | A 'Backend' whose receive can also be given up on, so that 'bye'
--   waits for the last handshake messages without being interrupted.
data TLSBackend = TLSBackend Backend RecvWith

instance HasBackend TLSBackend where
    initializeBackend _ = return ()
    getBackend (TLSBackend b _) = b
    getBackendRecvWith (TLSBackend _ r) = Just r

-- | close_notify, as a last word: a send which has been given up on is
--   not an error here.  Given up on after a second, through the send's
--   'TVar': handed over to http2, nothing else bounded it.
byeGivingUp :: TVar (STM ()) -> Context -> IO ()
byeGivingUp giveUpVar ctx = do
    late <- registerDelay 1000000
    atomically $ writeTVar giveUpVar $ readTVar late >>= check
    -- close_notify is best effort: a peer gone first is no failure.
    bye ctx `E.catch` \e -> when (isAsync e) $ E.throwIO e
  where
    isAsync :: E.SomeException -> Bool
    isAsync e = case E.fromException e of
        Just (E.SomeAsyncException _) -> True
        Nothing -> False

-- | What a backend gave up with: the 'STM' action's, if it is what gave
--   up, rather than the connection's.
gaveUp :: STM g -> HTTP2Error -> IO (Either g a)
gaveUp giveUp ConnectionIsTimeout = do
    mg <- atomically $ (Just <$> giveUp) `orElse` return Nothing
    maybe (E.throwIO ConnectionIsTimeout) (return . Left) mg
gaveUp _ e = E.throwIO e
