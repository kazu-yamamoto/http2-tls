{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.TLS.IO where

import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Either (isRight)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Network.Socket
import Network.Socket.BufferPool
import qualified Network.Socket.ByteString as NSB
import Network.TLS hiding (HostName)
import Network.HTTP2.Client (HTTP2Error (..))
import System.IO.Error (isEOFError)
import qualified System.IO.Error as E
import qualified System.TimeManager as T

import Network.HTTP2.TLS.Server.Settings

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
mkRecvTCPWith :: Settings -> Socket -> IO (STM () -> IO (Either () ByteString))
mkRecvTCPWith Settings{..} sock = do
    pool <- newBufferPool settingsReadBufferLowerLimit settingsReadBufferSize
    return $ receiveWith sock pool

orTimeout :: Either () a -> IO a
orTimeout = either (\_ -> E.throwIO ConnectionIsTimeout) return

sendTCP :: Socket -> ByteString -> IO ()
sendTCP sock = NSB.sendAll sock

-- | 'sendTCP' which gives up when the 'STM' action fires first.
sendTCPWith :: Socket -> STM () -> ByteString -> IO (Either () ())
sendTCPWith sock giveUp bs = NSB.sendAllWith sock bs giveUp

----------------------------------------------------------------

-- | Sending and receiving functions.
--   Tiemout is reset when they return.
--   One exception is the slowloris attach prevention.
--   See 'settingsSlowlorisSize'.
data IOBackend = IOBackend
    { send :: ByteString -> IO ()
    -- ^ Sending.
    , sendWith :: STM () -> ByteString -> IO (Either () ())
    -- ^ Sending which gives up when the 'STM' action fires first,
    --   answering 'Left'.  Part of the data may have gone by then, so the
    --   connection is to be closed.
    --
    --   @since 0.5.7
    , sendMany :: [ByteString] -> IO ()
    -- ^ Sending many.
    , recv :: IO ByteString
    -- ^ Receiving.
    , recvWith :: STM () -> IO (Either () ByteString)
    -- ^ Receiving which gives up when the 'STM' action fires first,
    --   answering 'Left'.  Part of a TLS record may have been read by
    --   then, so the connection is to be closed.
    --
    --   @since 0.5.7
    , requestSock :: Socket
    -- ^ The socket for the request
    , mySockAddr :: SockAddr
    , peerSockAddr :: SockAddr
    }

timeoutIOBackend :: T.Handle -> Settings -> IOBackend -> IOBackend
timeoutIOBackend th Settings{..} IOBackend{..} =
    IOBackend
        { send = send'
        , sendWith = sendWith'
        , sendMany = sendMany'
        , recv = recv'
        , recvWith = recvWith'
        , ..
        }
  where
    send' bs = send bs >> T.tickle th
    sendWith' giveUp bs = do
        r <- sendWith giveUp bs
        when (isRight r) $ T.tickle th
        return r
    sendMany' bss = sendMany bss >> T.tickle th
    recv' = do
        bs <- recv
        tickleIfLarge bs
        return bs
    recvWith' giveUp = do
        r <- recvWith giveUp
        either (\_ -> return ()) tickleIfLarge r
        return r
    tickleIfLarge bs = when (BS.length bs > settingsSlowlorisSize) $ T.tickle th

-- | The 'TVar's are the ones given to 'mkBackend': what a send and a
--   receive of the backend give up on.
tlsIOBackend :: TVar (STM ()) -> TVar (STM ()) -> Context -> Socket -> IO IOBackend
tlsIOBackend giveUpVar recvGiveUpVar ctx sock = do
    mysa <- getSocketName sock
    peersa <- getPeerName sock
    return $
        IOBackend
            { send = sendTLS ctx
            , sendWith = \giveUp bs -> do
                atomically $ writeTVar giveUpVar giveUp
                (Right () <$ sendTLS ctx bs) `E.catch` gaveUp
            , sendMany = sendManyTLS ctx
            , recv = recvTLS ctx
            , recvWith = \giveUp -> do
                atomically $ writeTVar recvGiveUpVar giveUp
                ((Right <$> recvTLS ctx) `E.catch` gaveUp)
                    `E.finally` atomically (writeTVar recvGiveUpVar retry)
            , requestSock = sock
            , mySockAddr = mysa
            , peerSockAddr = peersa
            }

tcpIOBackend :: Settings -> Socket -> IO IOBackend
tcpIOBackend settings sock = do
    recvWith' <- mkRecvTCPWith settings sock
    mysa <- getSocketName sock
    peersa <- getPeerName sock
    return $
        IOBackend
            { send = sendTCP sock
            , sendWith = sendTCPWith sock
            , sendMany = \_ -> return ()
            , recv = recvWith' retry >>= orTimeout
            , recvWith = recvWith'
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
mkBackend :: Settings -> TVar (STM ()) -> TVar (STM ()) -> Socket -> IO Backend
mkBackend settings giveUpVar recvGiveUpVar sock = do
    let send' bs = do
            giveUp <- readTVarIO giveUpVar
            sendTCPWith sock giveUp bs
                >>= either (\_ -> E.throwIO ConnectionIsTimeout) return
    recvWith' <- mkRecvTCPWith settings sock
    let recv' = readTVarIO recvGiveUpVar >>= recvWith' >>= orTimeout
    recvN <- makeRecvN "" recv'
    return
        Backend
            { backendFlush = return ()
            , backendClose =
                gracefulClose sock 5000 `E.catchIOError` \_ -> return ()
            , backendSend = send'
            , backendRecv = recvN
            }

-- | close_notify, as a last word: a send which the http2 library has
--   given up on is not an error here.
byeGivingUp :: Context -> IO ()
byeGivingUp ctx = bye ctx `E.catch` \e -> () <$ gaveUp e

gaveUp :: HTTP2Error -> IO (Either () a)
gaveUp ConnectionIsTimeout = return $ Left ()
gaveUp e = E.throwIO e
