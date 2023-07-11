{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
import qualified Data.ByteString.Char8 as C8
import Data.Default.Class (def)
import Network.HTTP2.Client (
    Client,
    ClientConfig (..),
 )
import qualified Network.HTTP2.Client as H2Client
import Network.Socket
import Network.TLS hiding (HostName)
import qualified System.TimeManager as T
import qualified UnliftIO.Exception as E

import Network.HTTP2.TLS.Config
import Network.HTTP2.TLS.IO
import Network.HTTP2.TLS.Settings
import Network.HTTP2.TLS.Supported

----------------------------------------------------------------

runTLS
    :: Settings
    -> HostName
    -> PortNumber
    -> ByteString
    -- ^ ALPN
    -> (T.Manager -> IOBackend -> IO a)
    -> IO a
runTLS settings@Settings{..} serverName port alpn action =
    T.withManager' (settingsTimeout * 1000000) $ \mgr ->
        E.bracket open close $ \sock -> do
            th <- T.registerKillThread mgr $ return ()
            backend <- mkBackend settings sock
            E.bracket (contextNew backend params) bye $ \ctx -> do
                handshake ctx
                let iobackend = timeoutIOBackend th settingsSlowlorisSize $ tlsIOBackend ctx
                action mgr iobackend
  where
    open = openTCP serverName port
    params = getClientParams serverName alpn False

run :: Settings -> HostName -> PortNumber -> Client a -> IO a
run settings serverName port client =
    runTLS settings serverName port "h2" $ run' settings "https" serverName client

runH2C :: Settings -> HostName -> PortNumber -> Client a -> IO a
runH2C settings@Settings{..} serverName port client =
    T.withManager' (settingsTimeout * 1000000) $ \mgr ->
        E.bracket open close $ \sock -> do
            th <- T.registerKillThread mgr $ return ()
            iobackend0 <- tcpIOBackend settings sock
            let iobackend = timeoutIOBackend th settingsSlowlorisSize iobackend0
            run' settings "http" serverName client mgr iobackend
  where
    open = openTCP serverName port

run'
    :: Settings
    -> ByteString
    -> HostName
    -> Client a
    -> T.Manager
    -> IOBackend
    -> IO a
run' settings schm serverName client mgr IOBackend{..} =
    E.bracket
        (allocConfig settings mgr send recv)
        freeConfig
        (\conf -> H2Client.run cliconf conf client)
  where
    cliconf =
        ClientConfig
            { scheme = schm
            , authority = C8.pack serverName
            , cacheLimit = 20
            }

openTCP :: HostName -> PortNumber -> IO Socket
openTCP h p = do
    ai <- makeAddrInfo h p
    sock <- openSocket ai
    connect sock $ addrAddress ai
    return sock

makeAddrInfo :: HostName -> PortNumber -> IO AddrInfo
makeAddrInfo nh p = do
    let hints =
            defaultHints
                { addrFlags = [AI_ADDRCONFIG, AI_NUMERICHOST, AI_NUMERICSERV]
                , addrSocketType = Stream
                }
    let np = show p
    head <$> getAddrInfo (Just hints) (Just nh) (Just np)

----------------------------------------------------------------

getClientParams
    :: HostName
    -> ByteString
    -- ^ ALPN
    -> Bool
    -- ^ Checking server certificates
    -> ClientParams
getClientParams serverName alpn validate =
    (defaultParamsClient serverName "")
        { clientSupported = supported
        , clientWantSessionResume = Nothing
        , clientUseServerNameIndication = True
        , clientShared = shared
        , clientHooks = hooks
        }
  where
    shared =
        def
            { sharedValidationCache = validateCache
            }
    supported = strongSupported
    hooks =
        def
            { onSuggestALPN = return $ Just [alpn]
            }
    validateCache
        | validate = def
        | otherwise =
            ValidationCache
                (\_ _ _ -> return ValidationCachePass)
                (\_ _ _ -> return ())
