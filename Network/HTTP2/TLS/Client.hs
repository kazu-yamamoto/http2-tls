{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP2.TLS.Client (
    run,
    Client,
    -- * Low level
    getTLSParams,
    recvTLS,
    sendTLS,
    sendManyTLS,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import Data.Default.Class (def)
import Foreign.Marshal.Alloc (free, mallocBytes)
import Network.HTTP2.Client (
    Client,
    ClientConfig (..),
    Config (..),
    defaultPositionReadMaker,
 )
import qualified Network.HTTP2.Client as H2Client
import Network.Socket
import Network.Socket.BufferPool
import Network.TLS hiding (HostName)
import Network.TLS.Extra
import System.IO.Error (isEOFError)
import qualified System.TimeManager as T
import qualified UnliftIO.Exception as E

----------------------------------------------------------------

run :: HostName -> PortNumber -> Client a -> IO a
run serverName port client = E.bracket open close $ \sock ->
    E.bracket (contextNew sock params) bye $ \ctx -> do
        handshake ctx
        E.bracket (allocConfig ctx 4096) freeConfig $ \conf ->
            H2Client.run cliconf conf client
  where
    open = openTCP serverName port
    params = getTLSParams serverName "h2" False
    cliconf =
        ClientConfig
            { scheme = "https"
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

getTLSParams
    :: HostName
    -> ByteString
    -- ^ ALPN
    -> Bool
    -- ^ Checking server certificates
    -> ClientParams
getTLSParams serverName alpn validate =
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
    supported =
        def -- TLS.Supported
            { supportedVersions = [TLS13, TLS12]
            , supportedCiphers = ciphersuite_strong
            , supportedCompressions = [nullCompression]
            , supportedSecureRenegotiation = True
            , supportedClientInitiatedRenegotiation = False
            , supportedSession = True
            , supportedFallbackScsv = True
            , supportedGroups = [X25519, P256, P384]
            }
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

----------------------------------------------------------------

sendTLS :: Context -> ByteString -> IO ()
sendTLS ctx = sendData ctx . LBS.fromStrict

sendManyTLS :: Context -> [ByteString] -> IO ()
sendManyTLS ctx = sendData ctx . LBS.fromChunks

-- TLS version of recv (decrypting) without a cache.
recvTLS :: Context -> IO ByteString
recvTLS ctx = E.handle onEOF $ recvData ctx
  where
    onEOF e
        | Just Error_EOF <- E.fromException e = return ""
        | Just ioe <- E.fromException e, isEOFError ioe = return ""
        | otherwise = E.throwIO e

allocConfig :: Context -> Int -> IO Config
allocConfig ctx bufsiz = do
    buf <- mallocBytes bufsiz
    timmgr <- T.initialize $ 30 * 1000000
    recvN <- makeRecvN "" $ recvTLS ctx
    let config =
            Config
                { confWriteBuffer = buf
                , confBufferSize = bufsiz
                , confSendAll = sendTLS ctx
                , confReadN = recvN
                , confPositionReadMaker = defaultPositionReadMaker
                , confTimeoutManager = timmgr
                }
    return config

-- | Deallocating the resource of the simple configuration.
freeConfig :: Config -> IO ()
freeConfig conf = do
    free $ confWriteBuffer conf
    T.killManager $ confTimeoutManager conf
