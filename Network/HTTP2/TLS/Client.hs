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
import qualified Data.ByteString.Char8 as C8
import Data.Default.Class (def)
import Data.X509.Validation (validateDefault)
import Network.HTTP2.Client (
    Client,
    ClientConfig (..),
    defaultClientConfig,
    initialWindowSize,
    maxConcurrentStreams,
    settings,
 )
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

-- | Running a TLS client.
runTLS
    :: Settings
    -> HostName
    -> PortNumber
    -> ByteString
    -- ^ ALPN
    -> (Context -> SockAddr -> SockAddr -> IO a)
    -> IO a
runTLS settings serverName port alpn action =
    E.bracket open gclose $ \sock -> do
        mysa <- getSocketName sock
        peersa <- getPeerName sock
        E.bracket (contextNew sock params) bye $ \ctx -> do
            handshake ctx
            action ctx mysa peersa
  where
    open = openTCP (settingsAddrInfoFlags settings) serverName port
    params = getClientParams settings serverName alpn

-- | Running an HTTP\/2 client over TLS (over TCP).
run :: Settings -> HostName -> PortNumber -> Client a -> IO a
run settings serverName port client =
    runTLS settings serverName port "h2" $ \ctx mysa peersa ->
        run' settings "https" serverName (sendTLS ctx) (recvTLS ctx) mysa peersa client

-- | Running an HTTP\/2 client over TCP.
runH2C :: Settings -> HostName -> PortNumber -> Client a -> IO a
runH2C settings serverName port client =
    E.bracket open close $ \sock -> do
        mysa <- getSocketName sock
        peersa <- getPeerName sock
        recv <- mkRecvTCP Server.defaultSettings sock
        run' settings "http" serverName (sendTCP sock) recv mysa peersa client
  where
    open = openTCP (settingsAddrInfoFlags defaultSettings) serverName port

run'
    :: Settings
    -> ByteString
    -> HostName
    -> (ByteString -> IO ())
    -> IO ByteString
    -> SockAddr
    -> SockAddr
    -> Client a
    -> IO a
run' Settings{..} schm serverName send recv mysa peersa client =
    E.bracket
        (allocConfigForClient send recv mysa peersa)
        freeConfigForClient
        (\conf -> H2Client.run cliconf conf client)
  where
    cliconf =
        defaultClientConfig
            { scheme = schm
            , authority = C8.pack serverName
            , cacheLimit = settingsCacheLimit
            , connectionWindowSize = settingsConnectionWindowSize
            , settings =
                (settings defaultClientConfig)
                    { initialWindowSize = settingsStreamWindowSize
                    , maxConcurrentStreams = Just settingsConcurrentStreams
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
    -> HostName
    -> ByteString
    -- ^ ALPN
    -> ClientParams
getClientParams Settings{..} serverName alpn =
    (defaultParamsClient serverName "")
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
