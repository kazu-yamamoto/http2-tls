{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.TLS.Client (
    run,
    runH2C,
    Client,
    HostName,
    PortNumber,
    runTLS,
    IOBackend (..),
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
import Network.HTTP2.TLS.Supported

----------------------------------------------------------------

run :: HostName -> PortNumber -> Client a -> IO a
run serverName port client =
    runTLS serverName port "h2" $ run' "https" serverName client

runTLS
    :: HostName
    -> PortNumber
    -> ByteString
    -- ^ ALPN
    -> (IOBackend -> IO a)
    -> IO a
runTLS serverName port alpn action = E.bracket open close $ \sock -> do
    backend <- mkBackend sock
    E.bracket (contextNew backend params) bye $ \ctx -> do
        handshake ctx
        action $ IOBackend (sendTLS ctx) (sendManyTLS ctx) (recvTLS ctx)
  where
    open = openTCP serverName port
    params = getClientParams serverName alpn False

runH2C :: HostName -> PortNumber -> Client a -> IO a
runH2C serverName port client = E.bracket open close $ \sock -> do
    let send = sendTCP sock
    recv <- mkRecvTCP sock
    run' "http" serverName client $ IOBackend send (\_ -> return ()) recv
  where
    open = openTCP serverName port

run'
    :: ByteString
    -> HostName
    -> Client a
    -> IOBackend
    -> IO a
run' schm serverName client IOBackend{..} =
    E.bracket (allocConfig 4096 send recv) freeConfig $ \conf ->
        H2Client.run cliconf conf client
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
