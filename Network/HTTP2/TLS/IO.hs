{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.TLS.IO where

import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isRight)
import Data.IORef
import Network.HTTP2.Client (pattern ConnectionIsTimeout)
import Network.Socket
import Network.Socket.BufferPool
import qualified Network.Socket.ByteString as NSB
import Network.TLS hiding (HostName)
import System.IO.Error (isEOFError)

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

-- | Sending and receiving through TLS, giving up as the 'STM' action says.
tlsIOBackend :: Context -> Socket -> IO IOBackend
tlsIOBackend ctx sock = do
    mysa <- getSocketName sock
    peersa <- getPeerName sock
    return $
        IOBackend
            { send = sendTLS ctx
            , sendWith = \bs ->
                respectingGracefulClose ctx (Right ()) . sendDataWith ctx (LBS.fromStrict bs)
            , sendMany = sendManyTLS ctx
            , sendManyWith = \bss ->
                respectingGracefulClose ctx (Right ()) . sendDataWith ctx (LBS.fromChunks bss)
            , recv = recvTLS ctx
            , recvWith = recvTLSWith ctx
            , requestSock = sock
            , mySockAddr = mysa
            , peerSockAddr = peersa
            }

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

-- | A send after the peer's close_notify has nowhere to go: dropped,
--   as the GOAWAY which closes the connection.
sendTLS :: Context -> ByteString -> IO ()
sendTLS ctx = respectingGracefulClose ctx () . sendData ctx . LBS.fromStrict

sendManyTLS :: Context -> [ByteString] -> IO ()
sendManyTLS ctx = respectingGracefulClose ctx () . sendData ctx . LBS.fromChunks

-- | Dropping a send which fails with EOF, even without close_notify.
afterCloseNotify :: IO () -> IO ()
afterCloseNotify = E.handle $ \e -> case e of
    PostHandshake Error_EOF -> return ()
    _ -> E.throwIO e
{-# DEPRECATED afterCloseNotify "Use respectingGracefulClose of tls" #-}

-- TLS version of recv (decrypting) without a cache.
recvTLS :: Context -> IO ByteString
recvTLS ctx = E.handle (onEOF return) $ recvData ctx

-- | 'recvTLS' which gives up between TLS records once the 'STM' action
--   succeeds.
recvTLSWith :: Context -> STM g -> IO (Either g ByteString)
recvTLSWith ctx giveUp = E.handle (onEOF (return . Right)) $ recvDataWith ctx giveUp

onEOF :: (ByteString -> IO a) -> E.SomeException -> IO a
onEOF eof e
    | Just (PostHandshake Error_EOF) <- E.fromException e = eof ""
    | Just ioe <- E.fromException e, isEOFError ioe = eof ""
    | otherwise = E.throwIO e

----------------------------------------------------------------

-- | Everything gives up once the watchdog has given up on the connection,
--   which is how the TLS handshake is timed out, or the 'STM' action has
--   succeeded.  'RecvWith' and 'SendWith' also give up on what tls says.
mkBackend :: Settings -> TLSWatchdog -> STM () -> Socket -> IO TLSBackend
mkBackend settings wd giveUpAll sock = do
    let expired = timedOutSTM wd `orElse` giveUpAll
        send' bs = sendTCPWith sock bs expired >>= orTimeout
        sendWith' = SendWith $ \bs stop -> orExpired (sendTCPWith sock bs) stop expired
    recvWith' <- mkRecvTCPWith settings sock
    -- What the caller of 'RecvWith' gives up on, during its call.
    stopRef <- newIORef Nothing
    let recv' = do
            stop <- maybe retry id <$> readIORef stopRef
            r <- recvWith' $ (True <$ stop) `orElse` (False <$ expired)
            case r of
                Right bs -> return bs
                -- Given up on by the caller of 'RecvWith', which sees it.
                Left True -> return ""
                Left False -> E.throwIO ConnectionIsTimeout
    recvN <- makeRecvN "" recv'
    let recvWith'' = RecvWith $ \n stop -> do
            given <- newTVarIO Nothing
            writeIORef stopRef $ Just $ stop >>= writeTVar given . Just
            bs <- recvN n `E.finally` writeIORef stopRef Nothing
            -- Bytes received before giving up are answered, not lost.
            mg <- readTVarIO given
            return $ case mg of
                Just g | BS.null bs -> Left g
                _ -> Right bs
        backend =
            Backend
                { backendFlush = return ()
                , -- The socket is network-run's to close, as the protocol
                  -- on top says; tls calls this only from contextClose.
                  backendClose = return ()
                , backendSend = send'
                , backendRecv = recvN
                }
    return $ TLSBackend backend recvWith'' sendWith'

-- | A 'Backend' whose receive and send can also be given up on.
data TLSBackend = TLSBackend Backend RecvWith SendWith

instance HasBackend TLSBackend where
    initializeBackend _ = return ()
    getBackend (TLSBackend b _ _) = b
    getBackendRecvWith (TLSBackend _ r _) = Just r
    getBackendSendWith (TLSBackend _ _ s) = Just s

-- | close_notify, as a last word, given up on after a second: handed over
--   to http2, nothing else bounded it.
byeGivingUp :: Context -> IO ()
byeGivingUp ctx = do
    late <- registerDelay 1000000
    -- close_notify is best effort: a peer gone first is no failure.
    byeWith ctx (readTVar late >>= check) `E.catch` \e -> when (isAsync e) $ E.throwIO e
  where
    isAsync :: E.SomeException -> Bool
    isAsync e = case E.fromException e of
        Just (E.SomeAsyncException _) -> True
        Nothing -> False
