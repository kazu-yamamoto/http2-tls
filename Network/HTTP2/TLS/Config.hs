{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.TLS.Config where

import Data.ByteString (ByteString)
import Foreign.Marshal.Alloc (free, mallocBytes)
import Network.HTTP2.Client (Config)
import qualified Network.HTTP2.Client as C
import Network.Socket (SockAddr)
import Network.Socket.BufferPool
import qualified System.TimeManager as T

import Network.HTTP2.TLS.Server.Settings

allocConfigForServer
    :: Settings
    -> T.Manager
    -> (ByteString -> IO ())
    -> IO ByteString
    -> SockAddr
    -> SockAddr
    -> IO Config
allocConfigForServer Settings{..} mgr send recv mysa peersa = do
    buf <- mallocBytes settingsSendBufferSize
    recvN <- makeRecvN "" recv
    let config =
            C.defaultConfig
                { C.confWriteBuffer = buf
                , C.confBufferSize = settingsSendBufferSize
                , C.confSendAll = send
                , C.confReadN = recvN
                , C.confPositionReadMaker = C.defaultPositionReadMaker
                , C.confTimeoutManager = mgr
                , C.confMySockAddr = mysa
                , C.confPeerSockAddr = peersa
                , C.confReadNTimeout = False
                }
    return config

-- | Deallocating the resource of the simple configuration.
freeConfigForServer :: Config -> IO ()
freeConfigForServer conf = free $ C.confWriteBuffer conf

allocConfigForClient
    :: Int -> (ByteString -> IO ()) -> IO ByteString -> SockAddr -> SockAddr -> IO Config
allocConfigForClient tout send recv mysa peersa = do
    let wbufsiz = 4096 -- fixme
    buf <- mallocBytes wbufsiz
    recvN <- makeRecvN "" recv
    -- A global manager does not exist.
    -- So, a timeout manager is created per connection.
    -- The receiver of http2 uses it as an idle timeout.
    mgr <- T.initialize (tout * 1000000)
    let config =
            C.defaultConfig
                { C.confWriteBuffer = buf
                , C.confBufferSize = wbufsiz
                , C.confSendAll = send
                , C.confReadN = recvN
                , C.confPositionReadMaker = C.defaultPositionReadMaker
                , C.confTimeoutManager = mgr
                , C.confMySockAddr = mysa
                , C.confPeerSockAddr = peersa
                , C.confReadNTimeout = False
                }
    return config

-- | Deallocating the resource of the simple configuration.
freeConfigForClient :: Config -> IO ()
freeConfigForClient conf = free $ C.confWriteBuffer conf
