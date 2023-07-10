{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.TLS.Server (
    run,
    runH2C,
    Server,
    HostName,
    PortNumber,
    runTLS,
    IOBackend(..),
) where

import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Default.Class (def)
import Network.HTTP2.Server (Server)
import qualified Network.HTTP2.Server as H2Server
import Network.Run.TCP
import Network.Socket (
    HostName,
    PortNumber,
 )
import Network.Socket.BufferPool
import qualified Network.Socket.ByteString as NSB
import Network.TLS hiding (HostName)
import qualified UnliftIO.Exception as E

import Network.HTTP2.TLS.Config
import Network.HTTP2.TLS.IO
import Network.HTTP2.TLS.Supported

run :: Credentials -> HostName -> PortNumber -> Server -> IO ()
run creds host port server = runTLS creds host port "h2" $ run' server

runTLS
    :: Credentials
    -> HostName
    -> PortNumber
    -> ByteString
    -- ^ ALPN
    -> (IOBackend -> IO a)
    -> IO a
runTLS creds host port alpn action  = do
    runTCPServer (Just host) (show port) $ \sock -> do
        backend <- mkBackend sock
        E.bracket (contextNew backend params) bye $ \ctx -> do
            handshake ctx
            action $ IOBackend (sendTLS ctx) (sendManyTLS ctx) (recvTLS ctx)
  where
    params = getServerParams creds alpn

runH2C :: HostName -> PortNumber -> Server -> IO ()
runH2C host port server = do
    runTCPServer (Just host) (show port) $ \sock -> do
        pool <- newBufferPool 2048 16384
        let send = void . NSB.send sock
            recv = receive sock pool
        run' server $ IOBackend send (\_ -> return ()) recv

run' :: Server -> IOBackend -> IO ()
run' server IOBackend{..} =
    E.bracket
        (allocConfig 4096 send recv)
        freeConfig
        (\conf -> H2Server.run conf server)

----------------------------------------------------------------

getServerParams
    :: Credentials
    -> ByteString
    -> ServerParams
getServerParams creds alpn =
    def
        { serverSupported = supported
        , serverShared = shared
        , serverHooks = hooks
        }
  where
    shared =
        def
            { sharedCredentials = creds
            --            , sharedSessionManager = undefined
            }
    supported = strongSupported
    hooks =
        def
            { onALPNClientSuggest = Just $ selectALPN alpn
            }

selectALPN :: ByteString -> [ByteString] -> IO ByteString
selectALPN key xs
    | key `elem` xs = return key
    | otherwise = return ""
