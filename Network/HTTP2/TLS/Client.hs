{-# LANGUAGE OverloadedStrings #-}
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
import qualified UnliftIO.Exception as E

import Network.HTTP2.TLS.Config
import Network.HTTP2.TLS.IO
import Network.HTTP2.TLS.Settings
import Network.HTTP2.TLS.Supported

----------------------------------------------------------------

-- | Running a TLS client.
runTLS
    :: HostName
    -> PortNumber
    -> ByteString
    -- ^ ALPN
    -> (Context -> IO a)
    -> IO a
runTLS serverName port alpn action =
    E.bracket open close $ \sock -> do
        E.bracket (contextNew sock params) bye $ \ctx -> do
            handshake ctx
            action ctx
  where
    open = openTCP serverName port
    params = getClientParams serverName alpn False

-- | Running an HTTP\/2 client over TLS (over TCP).
run :: HostName -> PortNumber -> Client a -> IO a
run serverName port client =
    runTLS serverName port "h2" $ \ctx ->
        run' "https" serverName (sendTLS ctx) (recvTLS ctx) client

-- | Running an HTTP\/2 client over TCP.
runH2C :: HostName -> PortNumber -> Client a -> IO a
runH2C serverName port client =
    E.bracket open close $ \sock -> do
        recv <- mkRecvTCP defaultSettings sock
        run' "http" serverName (sendTCP sock) recv client
  where
    open = openTCP serverName port

run'
    :: ByteString
    -> HostName
    -> (ByteString -> IO ())
    -> IO ByteString
    -> Client a
    -> IO a
run' schm serverName send recv client =
    E.bracket
        (allocConfigForClient send recv)
        freeConfigForClient
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
