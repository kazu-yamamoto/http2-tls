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

allocConfig :: Int -> (ByteString -> IO ()) -> IO ByteString -> IO Config
allocConfig sendbufsiz send recv = do
    buf <- mallocBytes sendbufsiz
    timmgr <- T.initialize $ 30 * 1000000
    recvN <- makeRecvN "" recv
    let config =
            Config
                { confWriteBuffer = buf
                , confBufferSize = sendbufsiz
                , confSendAll = send
                , confReadN = recvN
                , confPositionReadMaker = defaultPositionReadMaker
                , confTimeoutManager = timmgr
                }
    return config

-- | Deallocating the resource of the simple configuration.
freeConfig :: Config -> IO ()
freeConfig conf = do
    free $ confWriteBuffer conf
    T.killManager $ confTimeoutManager conf
