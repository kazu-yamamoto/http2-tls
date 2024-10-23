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
import System.IO.Error (isEOFError)
import qualified System.TimeManager as T

import Network.HTTP2.TLS.Server.Settings

----------------------------------------------------------------

-- HTTP2: confReadN == recvTLS
-- TLS:   recvData  == contextRecv == backendRecv

----------------------------------------------------------------

mkRecvTCP :: Settings -> Socket -> IO (IO ByteString)
mkRecvTCP Settings{..} sock = do
    pool <- newBufferPool settingsReadBufferLowerLimit settingsReadBufferSize
    return $ receive sock pool

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

timeoutIOBackend :: T.Handle -> Settings -> IOBackend -> IOBackend
timeoutIOBackend th Settings{..} IOBackend{..} =
    IOBackend send' sendMany' recv' requestSock mySockAddr peerSockAddr
  where
    send' bs = send bs >> T.tickle th
    sendMany' bss = sendMany bss >> T.tickle th
    recv' = do
        bs <- recv
        when (BS.length bs > settingsSlowlorisSize) $ T.tickle th
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

tcpIOBackend :: Settings -> Socket -> IO IOBackend
tcpIOBackend settings sock = do
    recv' <- mkRecvTCP settings sock
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

mkBackend :: Settings -> Socket -> IO Backend
mkBackend settings sock = do
    let send' = sendTCP sock
    recv' <- mkRecvTCP settings sock
    recvN <- makeRecvN "" recv'
    return
        Backend
            { backendFlush = return ()
            , backendClose =
                gracefulClose sock 5000 `E.catch` \(E.SomeException _) -> return ()
            , backendSend = send'
            , backendRecv = recvN
            }
