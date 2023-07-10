{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP2.TLS.Config where

import Data.ByteString (ByteString)
import Foreign.Marshal.Alloc (free, mallocBytes)
import Network.HTTP2.Client (
    Config (..),
    defaultPositionReadMaker,
 )
import Network.Socket.BufferPool
import qualified System.TimeManager as T

allocConfig
    :: T.Manager -> Int -> (ByteString -> IO ()) -> IO ByteString -> IO Config
allocConfig mgr sendbufsiz send recv = do
    buf <- mallocBytes sendbufsiz
    recvN <- makeRecvN "" recv
    let config =
            Config
                { confWriteBuffer = buf
                , confBufferSize = sendbufsiz
                , confSendAll = send
                , confReadN = recvN
                , confPositionReadMaker = defaultPositionReadMaker
                , confTimeoutManager = mgr
                }
    return config

-- | Deallocating the resource of the simple configuration.
freeConfig :: Config -> IO ()
freeConfig conf = free $ confWriteBuffer conf
