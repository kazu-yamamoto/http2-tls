{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.TLS.Server (
    -- * Runners
    run,
    runH2C,
    Server,
    HostName,
    PortNumber,
    runTLS,

    -- * Settings
    Settings,
    defaultSettings,
    settingsTimeout,
    settingsSendBufferSize,
    settingsSlowlorisSize,
    settingReadBufferSize,
    settingReadBufferLowerLimit,

    -- * IO backend
    IOBackend,
    send,
    sendMany,
    recv,
) where

import Data.ByteString (ByteString)
import Data.Default.Class (def)
import Network.HTTP2.Server (Server)
import qualified Network.HTTP2.Server as H2Server
import Network.Run.TCP.Timeout
import Network.Socket (
    HostName,
    PortNumber,
 )
import Network.TLS hiding (HostName)
import qualified System.TimeManager as T
import qualified UnliftIO.Exception as E

import Network.HTTP2.TLS.Config
import Network.HTTP2.TLS.IO
import Network.HTTP2.TLS.Settings
import Network.HTTP2.TLS.Supported

runTLS
    :: Settings
    -> Credentials
    -> HostName
    -> PortNumber
    -> ByteString
    -- ^ ALPN
    -> (T.Manager -> IOBackend -> IO a)
    -> IO a
runTLS settings@Settings{..} creds host port alpn action =
    runTCPServer settingsTimeout (Just host) (show port) $ \mgr th sock -> do
        backend <- mkBackend settings sock
        E.bracket (contextNew backend params) bye $ \ctx -> do
            handshake ctx
            let iobackend = timeoutIOBackend th 50 $ tlsIOBackend ctx
            action mgr iobackend
  where
    params = getServerParams creds alpn

run :: Settings -> Credentials -> HostName -> PortNumber -> Server -> IO ()
run settings creds host port server =
    runTLS settings creds host port "h2" $ run' settings server

runH2C :: Settings -> HostName -> PortNumber -> Server -> IO ()
runH2C settings@Settings{..} host port server =
    runTCPServer settingsTimeout (Just host) (show port) $ \mgr th sock -> do
        iobackend0 <- tcpIOBackend settings sock
        let iobackend = timeoutIOBackend th 50 iobackend0
        run' settings server mgr iobackend

run' :: Settings -> Server -> T.Manager -> IOBackend -> IO ()
run' settings server mgr IOBackend{..} =
    E.bracket
        (allocConfig settings mgr send recv)
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
