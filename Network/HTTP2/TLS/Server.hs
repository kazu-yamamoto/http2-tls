{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
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
    settingsInstallShutdownHandler,
    settingsClose,

    -- * IO backend
    IOBackend,
    send,
    sendWith,
    sendMany,
    recv,
    recvWith,
    requestSock,
    mySockAddr,
    peerSockAddr,

    -- * Internal
    runIO,
    runIOH2C,
    Stream,
    ServerIO (..),
) where

import Control.Concurrent.STM (STM, atomically, check, newTVarIO, readTVar, retry, writeTVar)
import qualified Control.Exception as E
import Control.Monad (when)
import Data.ByteString (ByteString)
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
import Network.Run.TCP (
    runTCPServerWithSettings,
    runTCPServerWithSocketAndSettings,
 )
import qualified Network.Run.TCP as TCP
import Network.Socket (
    HostName,
    PortNumber,
    Socket,
    close,
    unsafeGracefulClose,
 )
import Network.TLS hiding (HostName)

import Network.HTTP2.TLS.Config
import Network.HTTP2.TLS.IO
import Network.HTTP2.TLS.Server.Settings
import Network.HTTP2.TLS.Supported
import Network.HTTP2.TLS.Watchdog

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
runTLS = runTLS' False

-- | 'runTLS', handing the connection over to http2 after the handshake
--   if told so.  Otherwise the watchdog goes on supervising the action.
runTLS'
    :: Bool
    -> Settings
    -> Credentials
    -> HostName
    -> PortNumber
    -> ByteString
    -> (IOBackend -> IO ())
    -> IO ()
runTLS' h2 settings0 creds host port alpn action = do
    (settings, stopping) <- goingAway settings0
    runTCPServerWithSettings
        (tcpSettings settings)
        (Just host)
        (show port)
        $ \sock -> supervising settings $ \wd -> do
            giveUpVar <- newTVarIO retry
            recvGiveUpVar <- newTVarIO retry
            (stopHandshake, handshaken) <- handshakeStop stopping
            backend <- mkBackend settings wd giveUpVar recvGiveUpVar stopHandshake sock
            ctx <- contextNew backend params
            -- No HTTP/2 yet to close gracefully for: closed at once.
            handshake ctx `E.onException` close sock
            handshaken
            iobackend <- watchedIOBackend wd settings <$> tlsIOBackend giveUpVar recvGiveUpVar ctx sock
            -- The http2 library supervises the connection from here with a
            -- watchdog of its own.
            when h2 $ handOver wd
            action iobackend
            byeGivingUp giveUpVar ctx
  where
    params = getServerParams settings0 creds alpn

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
runTLSWithSocket = runTLSWithSocket' False

-- | 'runTLSWithSocket', handing over as 'runTLS'' does.
runTLSWithSocket'
    :: Bool
    -> Settings
    -> Credentials
    -> Socket
    -> ByteString
    -> (IOBackend -> IO ())
    -> IO ()
runTLSWithSocket' h2 settings0 creds s alpn action = do
    (settings, stopping) <- goingAway settings0
    runTCPServerWithSocketAndSettings
        (tcpSettings settings)
        s
        $ \sock -> supervising settings $ \wd -> do
            giveUpVar <- newTVarIO retry
            recvGiveUpVar <- newTVarIO retry
            (stopHandshake, handshaken) <- handshakeStop stopping
            backend <- mkBackend settings wd giveUpVar recvGiveUpVar stopHandshake sock
            ctx <- contextNew backend params
            -- No HTTP/2 yet to close gracefully for: closed at once.
            handshake ctx `E.onException` close sock
            handshaken
            iobackend <- watchedIOBackend wd settings <$> tlsIOBackend giveUpVar recvGiveUpVar ctx sock
            -- The http2 library supervises the connection from here with a
            -- watchdog of its own.
            when h2 $ handOver wd
            r <- action iobackend
            byeGivingUp giveUpVar ctx
            return r
  where
    params = getServerParams settings0 creds alpn

-- | Running an HTTP\/2 client over TLS (over TCP).
--   ALPN is "h2".
run :: Settings -> Credentials -> HostName -> PortNumber -> Server -> IO ()
run settings0 creds host port server = do
    (settings, goAway) <- goingAway settings0
    runTLS' True (byHTTP2 settings) creds host port "h2" $ run' settings goAway server

-- | Running an HTTP\/2 client over TLS (over TCP).
--   ALPN is "h2".
runWithSocket :: Settings -> Credentials -> Socket -> Server -> IO ()
runWithSocket settings0 creds s server = do
    (settings, goAway) <- goingAway settings0
    runTLSWithSocket' True (byHTTP2 settings) creds s "h2" $ run' settings goAway server

-- | Running an HTTP\/2 client over TCP.
runH2C :: Settings -> HostName -> PortNumber -> Server -> IO ()
runH2C settings0 host port server = do
    (settings, goAway) <- goingAway settings0
    runTCPServerWithSettings
        (tcpSettings $ byHTTP2 settings)
        (Just host)
        (show port)
        $ \sock -> supervising settings $ \wd -> do
            iobackend0 <- tcpIOBackend settings wd sock
            let iobackend = watchedIOBackend wd settings iobackend0
            handOver wd
            run' settings goAway server iobackend

-- | Running an HTTP\/2 client over TCP.
runH2CWithSocket :: Settings -> Socket -> Server -> IO ()
runH2CWithSocket settings0 s server = do
    (settings, goAway) <- goingAway settings0
    runTCPServerWithSocketAndSettings
        (tcpSettings $ byHTTP2 settings)
        s
        $ \sock -> supervising settings $ \wd -> do
            iobackend0 <- tcpIOBackend settings wd sock
            let iobackend = watchedIOBackend wd settings iobackend0
            handOver wd
            run' settings goAway server iobackend

run' :: Settings -> STM () -> Server -> IOBackend -> IO ()
run' settings0@Settings{..} goAway server IOBackend{..} =
    E.bracket
        (allocConfigForServer settings0 send sendWith recv recvWith mySockAddr peerSockAddr)
        freeConfigForServer
        (\conf -> H2Server.runWith goAway sconf conf server)
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
runIO settings0 creds s action = do
    (settings, goAway) <- goingAway settings0
    runTLSWithSocket' True (byHTTP2 settings) creds s "h2" $ \iobackend ->
        runIO' settings goAway action iobackend

runIO'
    :: Settings
    -> STM ()
    -> (ServerIO Stream -> IO (IO ()))
    -> IOBackend
    -> IO ()
runIO' settings0@Settings{..} goAway action IOBackend{..} =
    E.bracket
        (allocConfigForServer settings0 send sendWith recv recvWith mySockAddr peerSockAddr)
        freeConfigForServer
        (\conf -> H2I.runIOWith goAway sconf conf action)
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
runIOH2C settings0 s action = do
    (settings, goAway) <- goingAway settings0
    runTCPServerWithSocketAndSettings
        (tcpSettings $ byHTTP2 settings)
        s
        $ \sock -> supervising settings $ \wd -> do
            iobackend0 <- tcpIOBackend settings wd sock
            let iobackend = watchedIOBackend wd settings iobackend0
            handOver wd
            runIO' settings goAway action iobackend

----------------------------------------------------------------

-- | Running an action with the watchdog of this connection.
--
--   It starts before the TLS handshake, which is the part of a
--   connection where the peer is least known, and is handed over once
--   the action has the connection.
supervising :: Settings -> (TLSWatchdog -> IO a) -> IO a
supervising Settings{..} action = do
    wd <- newTLSWatchdog $ settingsTimeout * 1000000
    supervisedBy wd $ action wd

tcpSettings :: Settings -> TCP.ServerSettings
tcpSettings settings =
    TCP.defaultServerSettings
        { TCP.settingsInstallShutdownHandler = settingsInstallShutdownHandler settings
        , TCP.settingsClose = settingsClose settings
        }

-- | For the entry points which run http2: closing gracefully, or the
--   reset for a request sent after GOAWAY can wipe GOAWAY out.  Unsafe,
--   since http2 waits for its threads.
byHTTP2 :: Settings -> Settings
byHTTP2 settings = settings{settingsClose = \s -> unsafeGracefulClose s 5000}

-- | Giving up a handshake once the server is told to stop, and nothing
--   after it: what the connection does next is its protocol's to end.
handshakeStop :: STM () -> IO (STM (), IO ())
handshakeStop stopping = do
    handshaking <- newTVarIO True
    return
        ( (readTVar handshaking >>= check) >> stopping
        , atomically $ writeTVar handshaking False
        )

-- | Stopping the server also tells each HTTP\/2 connection to go away:
--   GOAWAY, and closing once the streams it has taken are done.
goingAway :: Settings -> IO (Settings, STM ())
goingAway settings = do
    var <- newTVarIO False
    let install stop =
            settingsInstallShutdownHandler settings $
                stop >> atomically (writeTVar var True)
    return
        (settings{settingsInstallShutdownHandler = install}, readTVar var >>= check)

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
        , serverEarlyDataSize = fromIntegral settingsEarlyDataSize
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
