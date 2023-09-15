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
    settingsReadBufferSize,
    settingsReadBufferLowerLimit,
    settingsKeyLogger,

    -- * IO backend
    IOBackend,
    send,
    sendMany,
    recv,
    mySockAddr,
    peerSockAddr,
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
import Network.HTTP2.TLS.Server.Settings
import Network.HTTP2.TLS.Supported

-- | Running a TLS client.
--   'IOBackend' provides sending and receiving functions
--   with timeout based on 'Settings'.
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
            iobackend <- timeoutIOBackend th settings <$> tlsIOBackend ctx sock
            action mgr iobackend
  where
    params = getServerParams creds alpn settingsKeyLogger

-- | Running an HTTP\/2 client over TLS (over TCP).
--   ALPN is "h2".
run :: Settings -> Credentials -> HostName -> PortNumber -> Server -> IO ()
run settings creds host port server =
    runTLS settings creds host port "h2" $ run' settings server

-- | Running an HTTP\/2 client over TCP.
runH2C :: Settings -> HostName -> PortNumber -> Server -> IO ()
runH2C settings@Settings{..} host port server =
    runTCPServer settingsTimeout (Just host) (show port) $ \mgr th sock -> do
        iobackend0 <- tcpIOBackend settings sock
        let iobackend = timeoutIOBackend th settings iobackend0
        run' settings server mgr iobackend

run' :: Settings -> Server -> T.Manager -> IOBackend -> IO ()
run' settings server mgr IOBackend{..} =
    E.bracket
        (allocConfigForServer settings mgr send recv mySockAddr peerSockAddr)
        freeConfigForServer
        (\conf -> H2Server.run conf server)

----------------------------------------------------------------

getServerParams
    :: Credentials
    -> ByteString
    -> (String -> IO ())
    -> ServerParams
getServerParams creds alpn keyLogger =
    def
        { serverSupported = supported
        , serverShared = shared
        , serverHooks = hooks
        , serverDebug = debug
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
    debug =
        def
            { debugKeyLogger = keyLogger
            }

selectALPN :: ByteString -> [ByteString] -> IO ByteString
selectALPN key xs
    | key `elem` xs = return key
    | otherwise = return ""
