{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP2.TLS.Client (
    run,
    runH2C,
    Client,

    -- * Low level
    getTLSParams,
    recvTLS,
    sendTLS,
    sendManyTLS,
) where

import Control.Monad (void)
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
import qualified Network.Socket.ByteString as NSB
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
        let send = sendTLS ctx
            recv = recvTLS ctx
        run' "https" serverName send recv client
  where
    open = openTCP serverName port
    params = getTLSParams serverName "h2" False

runH2C :: HostName -> PortNumber -> Client a -> IO a
runH2C serverName port client = E.bracket open close $ \sock -> do
    pool <- newBufferPool 2048 16384
    let send = void . NSB.send sock
        recv = receive sock pool
    run' "http" serverName send recv client
  where
    open = openTCP serverName port

run'
    :: ByteString
    -> String
    -> (ByteString -> IO ())
    -> IO ByteString
    -> Client a
    -> IO a
run' schm serverName send recv client =
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

allocConfig :: Int -> (ByteString -> IO ()) -> IO ByteString -> IO Config
allocConfig sendbufsiz send recv = do
    buf <- mallocBytes sendbufsiz
    timmgr <- T.initialize $ 30 * 1000000
    recvN <- makeRecvN "" recv
    let config =
            Config
                { confWriteBuffer = buf
                , confBufferSize = sendbufsiz
                , confSendAll = send
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
