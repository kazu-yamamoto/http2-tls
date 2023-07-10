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
