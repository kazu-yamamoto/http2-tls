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
    PortNumber,
    runTLS,

    -- ** Generalized API
    ClientConfig,
    defaultClientConfig,
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
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS.C8
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.UTF8 as BS.UTF8
import Data.Default.Class (def)
import Data.Maybe (fromMaybe)
import Data.X509.Validation (validateDefault)
import Network.HTTP2.Client (Client, ClientConfig)
import qualified Network.HTTP2.Client as H2Client
import Network.Socket
import Network.TLS hiding (HostName)
import qualified UnliftIO.Exception as E

import Network.HTTP2.TLS.Client.Settings
import Network.HTTP2.TLS.Config
import Network.HTTP2.TLS.IO
import Network.HTTP2.TLS.Internal (gclose)
import qualified Network.HTTP2.TLS.Server.Settings as Server
import Network.HTTP2.TLS.Supported

----------------------------------------------------------------
-- Default API

run :: Settings -> HostName -> PortNumber -> Client a -> IO a
run settings serverName port client =
    runWithConfig
        (defaultClientConfig settings serverName)
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
        (defaultClientConfig settings serverName)
        settings
        serverName
        port
        alpn
        action

runH2C :: Settings -> HostName -> PortNumber -> Client a -> IO a
runH2C settings serverName port client =
    runH2CWithConfig
        (defaultClientConfig settings serverName)
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
    E.bracket open gclose $ \sock -> do
        mysa <- getSocketName sock
        peersa <- getPeerName sock
        E.bracket (contextNew sock params) bye $ \ctx -> do
            handshake ctx
            action ctx mysa peersa
  where
    open :: IO Socket
    open = openTCP (settingsAddrInfoFlags settings) serverName port

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
    E.bracket open close $ \sock -> do
        mysa <- getSocketName sock
        peersa <- getPeerName sock
        recv <- mkRecvTCP Server.defaultSettings sock
        run' cliconf' (sendTCP sock) recv mysa peersa client
  where
    open = openTCP (settingsAddrInfoFlags defaultSettings) serverName port

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
    -> HostName
    -- ^ Authority
    -> ClientConfig
defaultClientConfig Settings{..} serverName =
    H2Client.defaultClientConfig
        { H2Client.scheme = "https"
        , H2Client.authority = C8.pack serverName
        , H2Client.cacheLimit = settingsCacheLimit
        , H2Client.connectionWindowSize = settingsConnectionWindowSize
        , H2Client.settings =
            (H2Client.settings $ H2Client.defaultClientConfig)
                { H2Client.initialWindowSize = settingsStreamWindowSize
                , H2Client.maxConcurrentStreams = Just settingsConcurrentStreams
                }
        }

openTCP :: [AddrInfoFlag] -> HostName -> PortNumber -> IO Socket
openTCP flags h p = do
    ai <- makeAddrInfo flags h p
    sock <- openSocket ai
    connect sock $ addrAddress ai
    return sock

makeAddrInfo :: [AddrInfoFlag] -> HostName -> PortNumber -> IO AddrInfo
makeAddrInfo flags nh p = do
    let hints =
            defaultHints
                { addrFlags = flags
                , addrSocketType = Stream
                }
    let np = show p
    head <$> getAddrInfo (Just hints) (Just nh) (Just np)

----------------------------------------------------------------

getClientParams
    :: Settings
    -> ByteString
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
    (defaultParamsClient (BS.UTF8.toString serverName) (BS.C8.pack $ show port))
        { clientSupported = supported
        , clientWantSessionResume = Nothing
        , clientUseServerNameIndication = True
        , clientShared = shared
        , clientHooks = hooks
        , clientDebug = debug
        }
  where
    shared =
        def
            { sharedValidationCache = validateCache
            , sharedCAStore = settingsCAStore
            }
    supported = strongSupported
    hooks =
        def
            { onSuggestALPN = return $ Just [alpn]
            , onServerCertificate = validateCert
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
