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

    -- ** Rate limits
    settingsPingRateLimit,
    settingsEmptyFrameRateLimit,
    settingsSettingsRateLimit,
    settingsRstRateLimit,

    -- * IO backend
    IOBackend,
    send,
    sendMany,
    recv,
    requestSock,
    mySockAddr,
    peerSockAddr,

    -- * Internal
    runIO,
    runIOH2C,
    Stream,
    ServerIO (..),
) where

import Control.Concurrent (myThreadId)
import qualified Control.Exception as E
import Data.ByteString (ByteString)
import Network.HTTP2.Client (HTTP2Error (ConnectionIsTimeout))
import Network.HTTP2.Server (
    Server,
    connectionWindowSize,
    defaultServerConfig,
    emptyFrameRateLimit,
    initialWindowSize,
    maxConcurrentStreams,
    pingRateLimit,
    rstRateLimit,
    settings,
    settingsRateLimit,
 )
import qualified Network.HTTP2.Server as H2Server
import Network.HTTP2.Server.Internal (ServerIO, Stream)
import qualified Network.HTTP2.Server.Internal as H2I
import Network.Run.TCP (runTCPServer, runTCPServerWithSocket)
import Network.Socket (
    HostName,
    PortNumber,
    Socket,
 )
import Network.TLS hiding (HostName)

import Network.HTTP2.TLS.Config
import Network.HTTP2.TLS.IO
import Network.HTTP2.TLS.Watchdog
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
    -> (IOBackend -> IO ())
    -> IO ()
runTLS settings creds host port alpn action =
    runTCPServer (Just host) (show port) $ \sock -> supervising settings $ \wd -> do
        backend <- mkBackend settings wd sock
        ctx <- contextNew backend params
        handshake ctx
        iobackend <- watchedIOBackend wd settings <$> tlsIOBackend ctx sock
        -- The connection is the http2 library's from here.
        handOver wd
        action iobackend
        bye ctx
  where
    params = getServerParams settings creds alpn

-- | Running an action with the watchdog of this connection.
--
--   It starts before the TLS handshake, which is the part of a
--   connection where the peer is least known, and is handed over to the
--   http2 library once that library has the connection.
supervising :: Settings -> (TLSWatchdog -> IO a) -> IO a
supervising Settings{..} action = do
    wd <- newTLSWatchdog $ settingsTimeout * 1000000
    tid <- myThreadId
    withWatchdog wd (E.throwTo tid ConnectionIsTimeout) $ action wd

-- | Running a TLS client.
--   'IOBackend' provides sending and receiving functions
--   with timeout based on 'Settings'.
runTLSWithSocket
    :: Settings
    -> Credentials
    -> Socket
    -> ByteString
    -- ^ ALPN
    -> (IOBackend -> IO ())
    -> IO ()
runTLSWithSocket settings creds s alpn action =
    runTCPServerWithSocket s $ \sock -> supervising settings $ \wd -> do
        backend <- mkBackend settings wd sock
        ctx <- contextNew backend params
        handshake ctx
        iobackend <- watchedIOBackend wd settings <$> tlsIOBackend ctx sock
        -- The connection is the http2 library's from here: it supervises
        -- it with a watchdog of its own, made from 'confTimeout'.
        handOver wd
        r <- action iobackend
        bye ctx
        return r
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
runH2C settings host port server =
    runTCPServer (Just host) (show port) $ \sock -> supervising settings $ \wd -> do
        iobackend0 <- tcpIOBackend settings wd sock
        let iobackend = watchedIOBackend wd settings iobackend0
        handOver wd
        run' settings server iobackend

-- | Running an HTTP\/2 client over TCP.
runH2CWithSocket :: Settings -> Socket -> Server -> IO ()
runH2CWithSocket settings s server =
    runTCPServerWithSocket s $ \sock -> supervising settings $ \wd -> do
        iobackend0 <- tcpIOBackend settings wd sock
        let iobackend = watchedIOBackend wd settings iobackend0
        handOver wd
        run' settings server iobackend

run' :: Settings -> Server -> IOBackend -> IO ()
run' settings0@Settings{..} server IOBackend{..} =
    E.bracket
        (allocConfigForServer settings0 send recv mySockAddr peerSockAddr)
        freeConfigForServer
        (\conf -> H2Server.run sconf conf server)
  where
    sconf =
        defaultServerConfig
            { connectionWindowSize = settingsConnectionWindowSize
            , settings =
                (settings defaultServerConfig)
                    { initialWindowSize = settingsStreamWindowSize
                    , maxConcurrentStreams = Just settingsConcurrentStreams
                    , pingRateLimit = settingsPingRateLimit
                    , emptyFrameRateLimit = settingsEmptyFrameRateLimit
                    , settingsRateLimit = settingsSettingsRateLimit
                    , rstRateLimit = settingsRstRateLimit
                    }
            }

runIO
    :: Settings
    -> Credentials
    -> Socket
    -> (ServerIO Stream -> IO (IO ()))
    -> IO ()
runIO settings creds s action =
    runTLSWithSocket settings creds s "h2" $ \iobackend ->
        runIO' settings action iobackend

runIO'
    :: Settings -> (ServerIO Stream -> IO (IO ())) -> IOBackend -> IO ()
runIO' settings0@Settings{..} action IOBackend{..} =
    E.bracket
        (allocConfigForServer settings0 send recv mySockAddr peerSockAddr)
        freeConfigForServer
        (\conf -> H2I.runIO sconf conf action)
  where
    sconf =
        defaultServerConfig
            { connectionWindowSize = settingsConnectionWindowSize
            , settings =
                (settings defaultServerConfig)
                    { initialWindowSize = settingsStreamWindowSize
                    , maxConcurrentStreams = Just settingsConcurrentStreams
                    , pingRateLimit = settingsPingRateLimit
                    , emptyFrameRateLimit = settingsEmptyFrameRateLimit
                    , settingsRateLimit = settingsSettingsRateLimit
                    , rstRateLimit = settingsRstRateLimit
                    }
            }

runIOH2C
    :: Settings -> Socket -> (ServerIO Stream -> IO (IO ())) -> IO ()
runIOH2C settings0 s action =
    runTCPServerWithSocket s $ \sock -> supervising settings0 $ \wd -> do
        iobackend0 <- tcpIOBackend settings0 wd sock
        let iobackend = watchedIOBackend wd settings0 iobackend0
        handOver wd
        runIO' settings0 action iobackend

----------------------------------------------------------------

getServerParams
    :: Settings
    -> Credentials
    -> ByteString
    -> ServerParams
getServerParams Settings{..} creds alpn =
    defaultParamsServer
        { serverSupported = supported
        , serverShared = shared
        , serverHooks = hooks
        , serverDebug = debug
        , serverEarlyDataSize = settingsEarlyDataSize
        }
  where
    shared =
        defaultShared
            { sharedCredentials = creds
            , sharedSessionManager = settingsSessionManager
            }
    supported = strongSupported
    hooks =
        defaultServerHooks
            { onALPNClientSuggest = Just $ selectALPN alpn
            }
    debug =
        defaultDebugParams
            { debugKeyLogger = settingsKeyLogger
            }

selectALPN :: ByteString -> [ByteString] -> IO ByteString
selectALPN key xs
    | key `elem` xs = return key
    | otherwise = return ""
