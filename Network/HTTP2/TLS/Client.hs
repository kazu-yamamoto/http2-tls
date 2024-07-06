{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

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

    -- * Settings
    Settings,
    defaultSettings,
    settingsKeyLogger,
    settingsValidateCert,
    settingsCAStore,
    settingsAddrInfoFlags,
    settingsCacheLimit,
    settingsConcurrentStreams,
    settingsConnectionWindowSize,
    settingsStreamWindowSize,
    settingsServerNameOverride,
    settingsSessionManager,
    settingsWantSessionResume,
    settingsWantSessionResumeList,
    settingsUseEarlyData,
    settingsOnServerFinished,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS.C8
import Data.Default.Class (def)
import Data.Maybe (fromMaybe)
import Data.X509.Validation (validateDefault)
import Network.HTTP2.Client (Authority, Client, ClientConfig)
import qualified Network.HTTP2.Client as H2Client
import Network.Run.TCP hiding (Settings)
import Network.Socket
import Network.TLS hiding (HostName)
import qualified UnliftIO.Exception as E

import Network.HTTP2.TLS.Client.Settings
import Network.HTTP2.TLS.Config
import Network.HTTP2.TLS.IO
import qualified Network.HTTP2.TLS.Server.Settings as Server
import Network.HTTP2.TLS.Supported

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
    runTCPClient serverName (show port) $ \sock -> do
        mysa <- getSocketName sock
        peersa <- getPeerName sock
        ctx <- contextNew sock params
        handshake ctx
        r <- action ctx mysa peersa
        bye ctx
        return r
  where
    -- TLS client parameters
    params :: ClientParams
    params =
        getClientParams
            settings
            ( fromMaybe (H2Client.authority cliconf) $
                settingsServerNameOverride settings
            )
            port
            alpn

-- | Running an HTTP\/2 client over TLS (over TCP).
runWithConfig
    :: ClientConfig -> Settings -> HostName -> PortNumber -> Client a -> IO a
runWithConfig cliconf settings serverName port client =
    runTLSWithConfig cliconf settings serverName port "h2" $ \ctx mysa peersa ->
        run' cliconf' (sendTLS ctx) (recvTLS ctx) mysa peersa client
  where
    cliconf' :: ClientConfig
    cliconf' = cliconf{H2Client.scheme = "https"}

-- | Running an HTTP\/2 client over TCP.
runH2CWithConfig :: ClientConfig -> HostName -> PortNumber -> Client a -> IO a
runH2CWithConfig cliconf serverName port client =
    runTCPClient serverName (show port) $ \sock -> do
        mysa <- getSocketName sock
        peersa <- getPeerName sock
        recv <- mkRecvTCP Server.defaultSettings sock
        run' cliconf' (sendTCP sock) recv mysa peersa client
  where
    cliconf' :: ClientConfig
    cliconf' = cliconf{H2Client.scheme = "http"}

run'
    :: ClientConfig
    -> (ByteString -> IO ())
    -> IO ByteString
    -> SockAddr
    -> SockAddr
    -> Client a
    -> IO a
run' cliconf send recv mysa peersa client =
    E.bracket
        (allocConfigForClient send recv mysa peersa)
        freeConfigForClient
        (\conf -> H2Client.run cliconf conf client)

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
    -> ClientParams
getClientParams Settings{..} serverName port alpn =
    -- RFC 4366 mandates UTF-8 for SNI
    -- <https://datatracker.ietf.org/doc/html/rfc4366#section-3.1>
    (defaultParamsClient serverName (BS.C8.pack $ show port))
        { clientSupported = supported
        , clientWantSessionResume = settingsWantSessionResume
        , clientWantSessionResumeList = settingsWantSessionResumeList
        , clientUseServerNameIndication = True
        , clientShared = shared
        , clientHooks = hooks
        , clientDebug = debug
        , clientUseEarlyData = settingsUseEarlyData
        }
  where
    shared =
        def
            { sharedValidationCache = validateCache
            , sharedCAStore = settingsCAStore
            , sharedSessionManager = settingsSessionManager
            }
    supported = strongSupported
    hooks =
        def
            { onSuggestALPN = return $ Just [alpn]
            , onServerCertificate = validateCert
            , onServerFinished = settingsOnServerFinished
            }
    validateCache
        | settingsValidateCert = def
        | otherwise =
            ValidationCache
                (\_ _ _ -> return ValidationCachePass)
                (\_ _ _ -> return ())
    validateCert
        | settingsValidateCert = validateDefault
        | otherwise = \_ _ _ _ -> return []
    debug =
        def
            { debugKeyLogger = settingsKeyLogger
            }
