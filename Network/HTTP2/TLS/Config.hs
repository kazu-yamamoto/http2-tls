{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.TLS.Config where

import Control.Concurrent.STM (STM)
import qualified Control.Exception as E
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Foreign.Marshal.Alloc (free, mallocBytes)
import Network.HTTP2.Client (Config)
import qualified Network.HTTP2.Client as C
import Network.Socket (SockAddr)
import Network.Socket.BufferPool

import Network.HTTP2.TLS.Server.Settings

allocConfigForServer
    :: Settings
    -> (ByteString -> IO ())
    -> (STM () -> ByteString -> IO (Either () ()))
    -> IO ByteString
    -> (STM () -> IO (Either () ByteString))
    -> SockAddr
    -> SockAddr
    -> IO Config
allocConfigForServer Settings{..} send sendWith recv recvWith mysa peersa = do
    buf <- mallocBytes settingsSendBufferSize
    (recvN, recvNWith) <- makeRecvNs recv recvWith
    let config =
            C.defaultConfig
                { C.confWriteBuffer = buf
                , C.confBufferSize = settingsSendBufferSize
                , C.confSendAll = send
                , C.confSendAllWith = Just sendWith
                , C.confReadN = recvN
                , C.confReadNWith = Just recvNWith
                , C.confPositionReadMaker = C.defaultPositionReadMaker
                , C.confTimeout = settingsTimeout * 1000000
                , C.confMySockAddr = mysa
                , C.confPeerSockAddr = peersa
                }
    return config

-- | 'makeRecvN' over the receive, and the same over the one which gives
--   up, sharing what is left over.  One given up on part-way answers with
--   an empty string, as at end of input.
makeRecvNs
    :: IO ByteString
    -> (STM () -> IO (Either () ByteString))
    -> IO (RecvN, STM () -> RecvN)
makeRecvNs recv recvWith = do
    stopRef <- newIORef Nothing
    let recv' = readIORef stopRef >>= maybe recv (fmap (either (const "") id) . recvWith)
    recvN <- makeRecvN "" recv'
    let recvNWith stop n = do
            writeIORef stopRef $ Just stop
            bs <- recvN n `E.finally` writeIORef stopRef Nothing
            return $ if BS.length bs == n then bs else ""
    return (recvN, recvNWith)

-- | Deallocating the resource of the simple configuration.
freeConfigForServer :: Config -> IO ()
freeConfigForServer conf = free $ C.confWriteBuffer conf

allocConfigForClient
    :: Int
    -> (ByteString -> IO ())
    -> (STM () -> ByteString -> IO (Either () ()))
    -> IO ByteString
    -> (STM () -> IO (Either () ByteString))
    -> SockAddr
    -> SockAddr
    -> IO Config
allocConfigForClient tout send sendWith recv recvWith mysa peersa = do
    let wbufsiz = 4096 -- fixme
    buf <- mallocBytes wbufsiz
    (recvN, recvNWith) <- makeRecvNs recv recvWith
    let config =
            C.defaultConfig
                { C.confWriteBuffer = buf
                , C.confBufferSize = wbufsiz
                , C.confSendAll = send
                , C.confSendAllWith = Just sendWith
                , C.confReadN = recvN
                , C.confReadNWith = Just recvNWith
                , C.confPositionReadMaker = C.defaultPositionReadMaker
                , C.confTimeout = tout * 1000000
                , C.confMySockAddr = mysa
                , C.confPeerSockAddr = peersa
                }
    return config

-- | Deallocating the resource of the simple configuration.
freeConfigForClient :: Config -> IO ()
freeConfigForClient conf = free $ C.confWriteBuffer conf
