{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.TLS.IO where

import qualified Control.Exception as E
import Control.Monad (void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Network.Socket
import Network.Socket.BufferPool
import qualified Network.Socket.ByteString as NSB
import Network.TLS hiding (HostName)
import Network.HTTP2.Client (HTTP2Error (ConnectionIsTimeout))
import System.IO.Error (isEOFError)
import qualified System.IO.Error as E

import Network.HTTP2.TLS.Server.Settings
import Network.HTTP2.TLS.Watchdog

----------------------------------------------------------------

-- HTTP2: confReadN == recvTLS
-- TLS:   recvData  == contextRecv == backendRecv

----------------------------------------------------------------

-- | A receiving function which gives up when the watchdog has given up
--   on the connection, rather than waiting for the peer for ever.
mkRecvTCP :: Settings -> TLSWatchdog -> Socket -> IO (IO ByteString)
mkRecvTCP Settings{..} wd sock = do
    pool <- newBufferPool settingsReadBufferLowerLimit settingsReadBufferSize
    return $ do
        r <- receiveWith sock pool (timedOutSTM wd)
        case r of
            Left () -> E.throwIO ConnectionIsTimeout
            Right bs -> return bs

sendTCP :: Socket -> ByteString -> IO ()
sendTCP sock = NSB.sendAll sock

----------------------------------------------------------------

-- | Sending and receiving functions.
--   Tiemout is reset when they return.
--   One exception is the slowloris attach prevention.
--   See 'settingsSlowlorisSize'.
data IOBackend = IOBackend
    { send :: ByteString -> IO ()
    -- ^ Sending.
    , sendMany :: [ByteString] -> IO ()
    -- ^ Sending many.
    , recv :: IO ByteString
    -- ^ Receiving.
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
    IOBackend send' sendMany' recv' requestSock mySockAddr peerSockAddr
  where
    send' bs = send bs >> progress wd
    sendMany' bss = sendMany bss >> progress wd
    recv' = do
        bs <- recv
        when (BS.length bs > settingsSlowlorisSize) $ progress wd
        return bs

tlsIOBackend :: Context -> Socket -> IO IOBackend
tlsIOBackend ctx sock = do
    mysa <- getSocketName sock
    peersa <- getPeerName sock
    return $
        IOBackend
            { send = sendTLS ctx
            , sendMany = sendManyTLS ctx
            , recv = recvTLS ctx
            , requestSock = sock
            , mySockAddr = mysa
            , peerSockAddr = peersa
            }

tcpIOBackend :: Settings -> TLSWatchdog -> Socket -> IO IOBackend
tcpIOBackend settings wd sock = do
    recv' <- mkRecvTCP settings wd sock
    mysa <- getSocketName sock
    peersa <- getPeerName sock
    return $
        IOBackend
            { send = void . NSB.send sock
            , sendMany = \_ -> return ()
            , recv = recv'
            , requestSock = sock
            , mySockAddr = mysa
            , peerSockAddr = peersa
            }

----------------------------------------------------------------

sendTLS :: Context -> ByteString -> IO ()
sendTLS ctx = sendData ctx . LBS.fromStrict

sendManyTLS :: Context -> [ByteString] -> IO ()
sendManyTLS ctx = sendData ctx . LBS.fromChunks

-- TLS version of recv (decrypting) without a cache.
recvTLS :: Context -> IO ByteString
recvTLS ctx = E.handle onEOF $ recvData ctx
  where
    onEOF e
        | Just (PostHandshake Error_EOF) <- E.fromException e = return ""
        | Just ioe <- E.fromException e, isEOFError ioe = return ""
        | otherwise = E.throwIO e

----------------------------------------------------------------

mkBackend :: Settings -> TLSWatchdog -> Socket -> IO Backend
mkBackend settings wd sock = do
    let send' = sendTCP sock
    recv' <- mkRecvTCP settings wd sock
    recvN <- makeRecvN "" recv'
    return
        Backend
            { backendFlush = return ()
            , backendClose =
                gracefulClose sock 5000 `E.catchIOError` \_ -> return ()
            , backendSend = send'
            , backendRecv = recvN
            }
