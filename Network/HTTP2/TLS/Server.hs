{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.TLS.Server (
    -- * Runners
    run,
    runWithSocket,
    runH2C,
    runH2CWithSocket,
    Server,
    HostName,
    PortNumber,
    runTLS,
    runTLSWithSocket,

    -- * Settings
    Settings,
    defaultSettings,
    settingsTimeout,
    settingsSendBufferSize,
    settingsSlowlorisSize,
    settingsReadBufferSize,
    settingsReadBufferLowerLimit,
    settingsKeyLogger,
    settingsNumberOfWorkers,
    settingsConcurrentStreams,
    settingsConnectionWindowSize,
    settingsStreamWindowSize,
    settingsSessionManager,
    settingsEarlyDataSize,

    -- * IO backend
    IOBackend,
    send,
    sendMany,
    recv,
    mySockAddr,
    peerSockAddr,

    -- * Internal
    runIO,
    runIOH2C,
    Stream,
    ServerIO (..),
) where

import Data.ByteString (ByteString)
import Data.Default.Class (def)
import Network.HTTP2.Server (
    Server,
    connectionWindowSize,
    defaultServerConfig,
    initialWindowSize,
    maxConcurrentStreams,
    numberOfWorkers,
    settings,
 )
import qualified Network.HTTP2.Server as H2Server
import Network.HTTP2.Server.Internal (ServerIO, Stream)
import qualified Network.HTTP2.Server.Internal as H2I
import Network.Run.TCP.Timeout
import Network.Socket (
    HostName,
    PortNumber,
    Socket,
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
runTLS settings creds host port alpn action =
    runTCPServer
        (settingsTimeout settings)
        (Just host)
        (show port)
        $ \mgr th sock -> do
            backend <- mkBackend settings sock
            E.bracket (contextNew backend params) bye $ \ctx -> do
                handshake ctx
                iobackend <- timeoutIOBackend th settings <$> tlsIOBackend ctx sock
                action mgr iobackend
  where
    params = getServerParams settings creds alpn

-- | Running a TLS client.
--   'IOBackend' provides sending and receiving functions
--   with timeout based on 'Settings'.
runTLSWithSocket
    :: Settings
    -> Credentials
    -> Socket
    -> ByteString
    -- ^ ALPN
    -> (T.Manager -> IOBackend -> IO a)
    -> IO a
runTLSWithSocket settings creds s alpn action =
    runTCPServerWithSocket
        (settingsTimeout settings)
        s
        $ \mgr th sock -> do
            backend <- mkBackend settings sock
            E.bracket (contextNew backend params) bye $ \ctx -> do
                handshake ctx
                iobackend <- timeoutIOBackend th settings <$> tlsIOBackend ctx sock
                action mgr iobackend
  where
    params = getServerParams settings creds alpn

-- | Running an HTTP\/2 client over TLS (over TCP).
--   ALPN is "h2".
run :: Settings -> Credentials -> HostName -> PortNumber -> Server -> IO ()
run settings creds host port server =
    runTLS settings creds host port "h2" $ run' settings server

-- | Running an HTTP\/2 client over TLS (over TCP).
--   ALPN is "h2".
runWithSocket :: Settings -> Credentials -> Socket -> Server -> IO ()
runWithSocket settings creds s server =
    runTLSWithSocket settings creds s "h2" $ run' settings server

-- | Running an HTTP\/2 client over TCP.
runH2C :: Settings -> HostName -> PortNumber -> Server -> IO ()
runH2C settings@Settings{..} host port server =
    runTCPServer
        settingsTimeout
        (Just host)
        (show port)
        $ \mgr th sock -> do
            iobackend0 <- tcpIOBackend settings sock
            let iobackend = timeoutIOBackend th settings iobackend0
            run' settings server mgr iobackend

-- | Running an HTTP\/2 client over TCP.
runH2CWithSocket :: Settings -> Socket -> Server -> IO ()
runH2CWithSocket settings@Settings{..} s server =
    runTCPServerWithSocket
        settingsTimeout
        s
        $ \mgr th sock -> do
            iobackend0 <- tcpIOBackend settings sock
            let iobackend = timeoutIOBackend th settings iobackend0
            run' settings server mgr iobackend

run' :: Settings -> Server -> T.Manager -> IOBackend -> IO ()
run' settings0@Settings{..} server mgr IOBackend{..} =
    E.bracket
        (allocConfigForServer settings0 mgr send recv mySockAddr peerSockAddr)
        freeConfigForServer
        (\conf -> H2Server.run sconf conf server)
  where
    sconf =
        defaultServerConfig
            { numberOfWorkers = settingsNumberOfWorkers
            , connectionWindowSize = settingsConnectionWindowSize
            , settings =
                (settings defaultServerConfig)
                    { initialWindowSize = settingsStreamWindowSize
                    , maxConcurrentStreams = Just settingsConcurrentStreams
                    }
            }

runIO
    :: Settings
    -> Credentials
    -> Socket
    -> (ServerIO -> IO (IO ()))
    -> IO ()
runIO settings creds s action =
    runTLSWithSocket settings creds s "h2" $ \mgr iobackend ->
        runIO' settings action mgr iobackend

runIO'
    :: Settings -> (ServerIO -> IO (IO ())) -> T.Manager -> IOBackend -> IO ()
runIO' settings0@Settings{..} action mgr IOBackend{..} =
    E.bracket
        (allocConfigForServer settings0 mgr send recv mySockAddr peerSockAddr)
        freeConfigForServer
        (\conf -> H2I.runIO sconf conf action)
  where
    sconf =
        defaultServerConfig
            { numberOfWorkers = settingsNumberOfWorkers
            , connectionWindowSize = settingsConnectionWindowSize
            , settings =
                (settings defaultServerConfig)
                    { initialWindowSize = settingsStreamWindowSize
                    , maxConcurrentStreams = Just settingsConcurrentStreams
                    }
            }

runIOH2C
    :: Settings -> Socket -> (ServerIO -> IO (IO ())) -> IO ()
runIOH2C settings0@Settings{..} s action =
    runTCPServerWithSocket
        settingsTimeout
        s
        $ \mgr th sock -> do
            iobackend0 <- tcpIOBackend settings0 sock
            let iobackend = timeoutIOBackend th settings0 iobackend0
            runIO' settings0 action mgr iobackend

----------------------------------------------------------------

getServerParams
    :: Settings
    -> Credentials
    -> ByteString
    -> ServerParams
getServerParams Settings{..} creds alpn =
    def
        { serverSupported = supported
        , serverShared = shared
        , serverHooks = hooks
        , serverDebug = debug
        , serverEarlyDataSize = settingsEarlyDataSize
        }
  where
    shared =
        def
            { sharedCredentials = creds
            , sharedSessionManager = settingsSessionManager
            }
    supported = strongSupported
    hooks =
        def
            { onALPNClientSuggest = Just $ selectALPN alpn
            }
    debug =
        def
            { debugKeyLogger = settingsKeyLogger
            }

selectALPN :: ByteString -> [ByteString] -> IO ByteString
selectALPN key xs
    | key `elem` xs = return key
    | otherwise = return ""
