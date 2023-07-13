{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.TLS.Config where

import Data.ByteString (ByteString)
import Foreign.Marshal.Alloc (free, mallocBytes)
import Network.HTTP2.Client (
    Config (..),
    defaultPositionReadMaker,
 )
import Network.Socket.BufferPool
import qualified System.TimeManager as T

import Network.HTTP2.TLS.Settings

allocConfig
    :: Settings -> T.Manager -> (ByteString -> IO ()) -> IO ByteString -> IO Config
allocConfig Settings{..} mgr send recv = do
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
                }
    return config

-- | Deallocating the resource of the simple configuration.
freeConfig :: Config -> IO ()
freeConfig conf = free $ confWriteBuffer conf


allocConfig' :: (ByteString -> IO ()) -> IO ByteString -> IO Config
allocConfig' send recv = do
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
                }
    return config

-- | Deallocating the resource of the simple configuration.
freeConfig' :: Config -> IO ()
freeConfig' Config{..} = do
    free confWriteBuffer
    T.killManager confTimeoutManager
