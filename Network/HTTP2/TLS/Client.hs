{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Running an HTTP\/2 client over TLS.
module Network.HTTP2.TLS.Client (
    -- * Runners
    run,
    runH2C,
    Client,
    HostName,
    Authority,
    PortNumber,
    runTLS,

    -- ** Generalized API
    ClientConfig,
    defaultClientConfig,
    defaultAuthority,
    runWithConfig,
    runH2CWithConfig,
    runTLSWithConfig,

    -- ** Giving up
    runGivingUp,
    runH2CGivingUp,
    runTLSGivingUp,

    -- * Settings
    Settings,
    defaultSettings,
    settingsKeyLogger,
    settingsValidateCert,
    settingsOnServerCertificate,
    settingsCAStore,
    settingsAddrInfoFlags,
    settingsCacheLimit,
    settingsConcurrentStreams,
    settingsConnectionWindowSize,
    settingsStreamWindowSize,
    settingsServerNameOverride,
    settingsUseServerNameIndication,
    settingsSessionManager,
    settingsWantSessionResume,
    settingsWantSessionResumeList,
    settingsOpenClientSocket,
    settingsUseEarlyData,
    settingsOnServerFinished,
    settingsTimeout,

    -- ** Rate limits
    settingsPingRateLimit,
    settingsEmptyFrameRateLimit,
    settingsSettingsRateLimit,
    settingsRstRateLimit,
) where

import Control.Concurrent (myThreadId)
import Control.Concurrent.STM (STM, atomically, newTVarIO, orElse, readTVar, retry, writeTVar)
import qualified Control.Exception as E
import Control.Monad (join, void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS.C8
import Data.Maybe (fromMaybe)
import Data.X509.CertificateStore (isEmptyCertificateStore)
import Network.HTTP2.Client (Authority, Client, ClientConfig)
import qualified Network.HTTP2.Client as H2Client
import Network.Run.TCP (runTCPClientWithSettings)
import qualified Network.Run.TCP as TCP
import Network.Socket
import Network.TLS hiding (HostName)
import System.X509 (getSystemCertificateStore)

import Network.HTTP2.TLS.Client.Settings
import Network.HTTP2.TLS.Config
import Network.HTTP2.TLS.IO
import qualified Network.HTTP2.TLS.Server.Settings as Server
import Network.HTTP2.TLS.Supported
import Network.HTTP2.TLS.Watchdog (TLSWatchdog, handOver, newTLSWatchdog, timedOutSTM, withWatchdog)

----------------------------------------------------------------
-- Default API

run :: Settings -> HostName -> PortNumber -> Client a -> IO a
run settings serverName port client =
    runWithConfig
        (defaultClientConfig settings $ defaultAuthority serverName)
        settings
        serverName
        port
        client

runTLS
    :: Settings
    -> HostName
    -> PortNumber
    -> ByteString
    -- ^ ALPN
    -> (Context -> SockAddr -> SockAddr -> IO a)
    -> IO a
runTLS settings serverName port alpn action =
    runTLSWithConfig
        (defaultClientConfig settings $ defaultAuthority serverName)
        settings
        serverName
        port
        alpn
        action

runH2C :: Settings -> HostName -> PortNumber -> Client a -> IO a
runH2C settings serverName port client =
    runH2CWithConfig
        (defaultClientConfig settings $ defaultAuthority serverName)
        settings
        serverName
        port
        client

----------------------------------------------------------------
-- Generalized API

-- | Running a TLS client.
runTLSWithConfig
    :: ClientConfig
    -> Settings
    -> HostName
    -> PortNumber
    -> ByteString
    -- ^ ALPN
    -> (Context -> SockAddr -> SockAddr -> IO a)
    -> IO a
runTLSWithConfig cliconf settings serverName port alpn action =
    runTLSWithBackend False cliconf settings serverName port alpn retry $ \ctx IOBackend{..} ->
        action ctx mySockAddr peerSockAddr

-- | 'runTLSWithConfig', giving up once the 'STM' action succeeds:
--   connecting, the TLS handshake, and what the action sends and
--   receives.  What the action gave is returned as 'Left'.  An action
--   already done by then is not given up on.
--
--   @since 0.6.0
runTLSGivingUp
    :: ClientConfig
    -> Settings
    -> HostName
    -> PortNumber
    -> ByteString
    -- ^ ALPN
    -> (Context -> SockAddr -> SockAddr -> IO a)
    -> STM g
    -> IO (Either g a)
runTLSGivingUp cliconf settings serverName port alpn action giveUp =
    givingUp giveUp $ \giveUp' ->
        runTLSWithBackend False cliconf settings serverName port alpn giveUp' $ \ctx IOBackend{..} ->
            action ctx mySockAddr peerSockAddr

-- | 'runTLSWithConfig' handing over an 'IOBackend' too, whose send and
--   receive can be given up on, as the server's.  With 'True', the 'STM'
--   action covers connecting and the handshake only: http2 gives up on the
--   rest by itself, with a GOAWAY that a backend given up on cannot send.
runTLSWithBackend
    :: Bool
    -> ClientConfig
    -> Settings
    -> HostName
    -> PortNumber
    -> ByteString
    -> STM ()
    -> (Context -> IOBackend -> IO a)
    -> IO a
runTLSWithBackend h2 cliconf settings@Settings{..} serverName port alpn giveUp action = do
    -- 'settingsTimeout' covers connecting and the handshake too, which
    -- could wait for ever on a silent server.  http2 times out the rest.
    wd <- newTLSWatchdog $ settingsTimeout * 1000000
    tid <- myThreadId
    withWatchdog wd (E.throwTo tid H2Client.ConnectionIsTimeout) $
      runTCPClientWithSettings (tcpSettings settings giveUp wd) serverName (show port) $ \sock -> do
        params <- getClientParams settings sni port alpn
        giveUpVar <- newTVarIO retry
        recvGiveUpVar <- newTVarIO retry
        giveUpAllVar <- newTVarIO giveUp
        backend <-
            mkBackend Server.defaultSettings wd giveUpVar recvGiveUpVar (join $ readTVar giveUpAllVar) sock
        ctx <- contextNew backend params
        handshakeWithin wd ctx
        handOver wd
        when h2 $ atomically $ writeTVar giveUpAllVar retry
        iobackend <- tlsIOBackend giveUpVar recvGiveUpVar ctx sock
        r <- action ctx iobackend
        byeGivingUp giveUpVar ctx
        return r
  where
    sni = fromMaybe (H2Client.authority cliconf) $ settingsServerNameOverride

-- | Running an HTTP\/2 client over TLS (over TCP).
runWithConfig
    :: ClientConfig -> Settings -> HostName -> PortNumber -> Client a -> IO a
runWithConfig cliconf settings serverName port client =
    runTLSWithBackend True cliconf settings serverName port "h2" retry $ \_ iobackend ->
        run' cliconf' settings iobackend client retry
  where
    cliconf' :: ClientConfig
    cliconf' = cliconf{H2Client.scheme = "https"}

-- | 'runWithConfig', giving up once the 'STM' action succeeds:
--   connecting, the TLS handshake, and the HTTP\/2 connection, which is
--   closed with a GOAWAY of NO_ERROR.  What the action gave is returned
--   as 'Left'.  A client already done by then is not given up on.
--
--   @since 0.6.0
runGivingUp
    :: ClientConfig
    -> Settings
    -> HostName
    -> PortNumber
    -> Client a
    -> STM g
    -> IO (Either g a)
runGivingUp cliconf settings serverName port client giveUp =
    givingUp giveUp $ \giveUp' ->
        runTLSWithBackend True cliconf settings serverName port "h2" giveUp' $ \_ iobackend ->
            run' cliconf' settings iobackend client giveUp'
  where
    cliconf' :: ClientConfig
    cliconf' = cliconf{H2Client.scheme = "https"}

-- | Running an HTTP\/2 client over TCP.
runH2CWithConfig
    :: ClientConfig -> Settings -> HostName -> PortNumber -> Client a -> IO a
runH2CWithConfig cliconf settings serverName port client =
    runH2C' cliconf settings serverName port client retry

-- | 'runH2CWithConfig', giving up as 'runGivingUp' does.
--
--   @since 0.6.0
runH2CGivingUp
    :: ClientConfig
    -> Settings
    -> HostName
    -> PortNumber
    -> Client a
    -> STM g
    -> IO (Either g a)
runH2CGivingUp cliconf settings serverName port client giveUp =
    givingUp giveUp $ runH2C' cliconf settings serverName port client

runH2C'
    :: ClientConfig -> Settings -> HostName -> PortNumber -> Client a -> STM () -> IO a
runH2C' cliconf settings@Settings{..} serverName port client giveUp = do
    wd <- newTLSWatchdog $ settingsTimeout * 1000000
    tid <- myThreadId
    withWatchdog wd (E.throwTo tid H2Client.ConnectionIsTimeout) $
      runTCPClientWithSettings (tcpSettings settings giveUp wd) serverName (show port) $ \sock -> do
        handOver wd
        iobackend <- tcpIOBackend Server.defaultSettings wd sock
        run' cliconf' settings iobackend client giveUp
  where
    cliconf' :: ClientConfig
    cliconf' = cliconf{H2Client.scheme = "http"}

run' :: ClientConfig -> Settings -> IOBackend -> Client a -> STM () -> IO a
run' cliconf Settings{..} IOBackend{..} client giveUp =
    E.bracket
        ( allocConfigForClient
            settingsTimeout
            send
            sendWith
            recv
            recvWith
            mySockAddr
            peerSockAddr
        )
        freeConfigForClient
        ( \conf ->
            H2Client.runGivingUp cliconf conf client giveUp
                >>= either (\_ -> E.throwIO GivenUp) return
        )

tcpSettings :: Settings -> STM () -> TLSWatchdog -> TCP.Settings
tcpSettings Settings{..} giveUp wd =
    TCP.defaultSettings
        { TCP.settingsOpenClientSocket = \ai ->
            settingsOpenClientSocket ai ((Left <$> giveUp) `orElse` (Right <$> timedOutSTM wd))
                >>= either (either (\_ -> E.throwIO GivenUp) (\_ -> E.throwIO H2Client.ConnectionIsTimeout)) return
        }

-- | The handshake, telling a timeout as 'H2Client.ConnectionIsTimeout'
--   rather than as the 'HandshakeFailed' tls wraps it in.
handshakeWithin :: TLSWatchdog -> Context -> IO ()
handshakeWithin wd ctx = handshake ctx `E.catch` \e -> do
    late <- atomically $ (True <$ timedOutSTM wd) `orElse` return False
    if late then E.throwIO H2Client.ConnectionIsTimeout else E.throwIO (e :: TLSException)

-- | Thrown inside once given up on, and turned into 'Left' outside.
data GivenUp = GivenUp deriving (Show)

instance E.Exception GivenUp

-- | Running an action with what it gives up on, and returning what the
--   'STM' action gave as 'Left' once it is given up on.  An action
--   already done by then is not given up on.
givingUp :: STM g -> (STM () -> IO a) -> IO (Either g a)
givingUp giveUp action = do
    r <- E.try $ action $ void giveUp
    case r of
        Right x -> return $ Right x
        -- Whatever the action ended with, it was given up on.  Not the
        -- caller being killed, though: that goes on.
        Left (e :: E.SomeException)
            | Just (_ :: E.SomeAsyncException) <- E.fromException e -> E.throwIO e
            | otherwise -> do
                mg <- atomically $ (Just <$> giveUp) `orElse` return Nothing
                maybe (E.throwIO e) (return . Left) mg

defaultClientConfig
    :: Settings
    -> Authority
    -> ClientConfig
defaultClientConfig Settings{..} auth =
    H2Client.defaultClientConfig
        { H2Client.scheme = "https"
        , H2Client.authority = auth
        , H2Client.cacheLimit = settingsCacheLimit
        , H2Client.connectionWindowSize = settingsConnectionWindowSize
        , H2Client.settings =
            (H2Client.settings $ H2Client.defaultClientConfig)
                { H2Client.initialWindowSize = settingsStreamWindowSize
                , H2Client.maxConcurrentStreams = Just settingsConcurrentStreams
                , H2Client.pingRateLimit = settingsPingRateLimit
                , H2Client.emptyFrameRateLimit = settingsEmptyFrameRateLimit
                , H2Client.settingsRateLimit = settingsSettingsRateLimit
                , H2Client.rstRateLimit = settingsRstRateLimit
                }
        }

-- | Default authority
--
-- When we connect to a server, we can distinguish between three names, all of
-- which may be different:
--
-- 1. The 'HostName', used for the DNS lookup to get the server's IP
-- 2. The HTTP2 @:authority@ pseudo-header
-- 3. The TLS SNI (Server Name Indicator).
--    This is different from (2) only in exceptional circumstances, see
--    'settingsServerNameOverride'.
--
-- In /most/ cases, however, all three names are identical, and so the default
-- 'Authority' is simply equal to the 'ServerName'.
defaultAuthority :: HostName -> Authority
defaultAuthority = id

----------------------------------------------------------------

getClientParams
    :: Settings
    -> HostName
    -- ^ Server name (for TLS SNI)
    -> PortNumber
    -- ^ Port number
    -- This is not used for validation, but improves caching; see documentation of
    -- [ServiceID](https://hackage.haskell.org/package/x509-validation-1.6.12/docs/Data-X509-Validation.html#t:ServiceID).
    -> ByteString
    -- ^ ALPN
    -> IO ClientParams
getClientParams Settings{..} sni port alpn = do
    caStore <-
        if settingsValidateCert
            then
                if isEmptyCertificateStore settingsCAStore
                    then getSystemCertificateStore
                    else return settingsCAStore
            else return mempty
    let shared =
            defaultShared
                { sharedValidationCache = validateCache
                , sharedCAStore = caStore
                , sharedSessionManager = settingsSessionManager
                }
    -- RFC 4366 mandates UTF-8 for SNI
    -- <https://datatracker.ietf.org/doc/html/rfc4366#section-3.1>
    return
        (defaultParamsClient sni (BS.C8.pack $ show port))
            { clientSupported = supported
            , clientWantSessionResume = settingsWantSessionResume
            , clientWantSessionResumeList = settingsWantSessionResumeList
            , clientUseServerNameIndication = settingsUseServerNameIndication
            , clientShared = shared
            , clientHooks = hooks
            , clientDebug = debug
            , clientUseEarlyData = settingsUseEarlyData
            }
  where
    supported = strongSupported
    hooks =
        defaultClientHooks
            { onSuggestALPN = return $ Just [alpn]
            , onServerFinished = settingsOnServerFinished
            , onServerCertificate = settingsOnServerCertificate
            }
    validateCache
        | settingsValidateCert = defaultValidationCache
        | otherwise =
            ValidationCache
                (\_ _ _ -> return ValidationCachePass)
                (\_ _ _ -> return ())
    debug =
        defaultDebugParams
            { debugKeyLogger = settingsKeyLogger
            }
