{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.TLS.Config where

import Data.ByteString (ByteString)
import Foreign.Marshal.Alloc (free, mallocBytes)
import Network.HTTP2.Client (
    Config (..),
    defaultPositionReadMaker,
 )
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
            Config
                { confWriteBuffer = buf
                , confBufferSize = settingsSendBufferSize
                , confSendAll = send
                , confReadN = recvN
                , confPositionReadMaker = defaultPositionReadMaker
                , confTimeoutManager = mgr
                , confMySockAddr = mysa
                , confPeerSockAddr = peersa
                }
    return config

-- | Deallocating the resource of the simple configuration.
freeConfigForServer :: Config -> IO ()
freeConfigForServer conf = free $ confWriteBuffer conf

allocConfigForClient
    :: (ByteString -> IO ()) -> IO ByteString -> SockAddr -> SockAddr -> IO Config
allocConfigForClient send recv mysa peersa = do
    let wbufsiz = 4096 -- fixme
    buf <- mallocBytes wbufsiz
    recvN <- makeRecvN "" recv
    -- A global manager does not exist.
    -- So, a timeout manager is created per connection.
    mgr <- T.initialize 30000000 -- fixme
    let config =
            Config
                { confWriteBuffer = buf
                , confBufferSize = wbufsiz
                , confSendAll = send
                , confReadN = recvN
                , confPositionReadMaker = defaultPositionReadMaker
                , confTimeoutManager = mgr
                , confMySockAddr = mysa
                , confPeerSockAddr = peersa
                }
    return config

-- | Deallocating the resource of the simple configuration.
freeConfigForClient :: Config -> IO ()
freeConfigForClient Config{..} = do
    free confWriteBuffer
    T.killManager confTimeoutManager
