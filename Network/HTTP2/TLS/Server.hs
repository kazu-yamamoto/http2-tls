{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.TLS.Server (
    run,
    runH2C,
    Server,
    HostName,
    PortNumber,
    runTLS,
    IOBackend (..),
) where

import Data.ByteString (ByteString)
import Data.Default.Class (def)
import Network.HTTP2.Server (Server)
import qualified Network.HTTP2.Server as H2Server
import Network.Run.TCP
import Network.Socket (
    HostName,
    PortNumber,
 )
import Network.TLS hiding (HostName)
import qualified System.TimeManager as T
import qualified UnliftIO.Exception as E

import Network.HTTP2.TLS.Config
import Network.HTTP2.TLS.IO
import Network.HTTP2.TLS.Supported

runTLS
    :: Credentials
    -> HostName
    -> PortNumber
    -> ByteString
    -- ^ ALPN
    -> (T.Manager -> IOBackend -> IO a)
    -> IO a
runTLS creds host port alpn action = T.withManager 30000000 $ \mgr ->
    runTCPServer (Just host) (show port) $ \sock -> do
        th <- T.registerKillThread mgr $ return ()
        backend <- mkBackend sock
        E.bracket (contextNew backend params) bye $ \ctx -> do
            handshake ctx
            let iobackend = timeoutIOBackend th 50 $ tlsIOBackend ctx
            action mgr iobackend
  where
    params = getServerParams creds alpn

run :: Credentials -> HostName -> PortNumber -> Server -> IO ()
run creds host port server = runTLS creds host port "h2" $ run' server

runH2C :: HostName -> PortNumber -> Server -> IO ()
runH2C host port server = T.withManager 30000000 $ \mgr ->
    runTCPServer (Just host) (show port) $ \sock -> do
        th <- T.registerKillThread mgr $ return ()
        iobackend0 <- tcpIOBackend sock
        let iobackend = timeoutIOBackend th 50 iobackend0
        run' server mgr iobackend

run' :: Server -> T.Manager -> IOBackend -> IO ()
run' server mgr IOBackend{..} =
    E.bracket
        (allocConfig mgr 4096 send recv)
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
