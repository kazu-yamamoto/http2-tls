{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP2.TLS.Server (
    run,
    runH2C,
    Server,
    HostName,
    PortNumber,

    -- * Low level
    getServerParams,
) where

import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Default.Class (def)
import Network.HTTP2.Server (Server)
import qualified Network.HTTP2.Server as H2Server
import Network.Run.TCP
import Network.Socket (
    HostName,
    PortNumber,
 )
import Network.Socket.BufferPool
import qualified Network.Socket.ByteString as NSB
import Network.TLS hiding (HostName)
import qualified UnliftIO.Exception as E

import Network.HTTP2.TLS.Config
import Network.HTTP2.TLS.IO
import Network.HTTP2.TLS.Supported

run :: Credentials -> HostName -> PortNumber -> Server -> IO ()
run creds host port server = do
    runTCPServer (Just host) (show port) $ \sock ->
        E.bracket (contextNew sock params) bye $ \ctx -> do
            handshake ctx
            let send = sendTLS ctx
                recv = recvTLS ctx
            run' send recv server
  where
    params = getServerParams creds

runH2C :: HostName -> PortNumber -> Server -> IO ()
runH2C host port server = do
    runTCPServer (Just host) (show port) $ \sock -> do
        pool <- newBufferPool 2048 16384
        let send = void . NSB.send sock
            recv = receive sock pool
        run' send recv server

run' :: (ByteString -> IO ()) -> IO ByteString -> Server -> IO ()
run' send recv server =
    E.bracket
        (allocConfig 4096 send recv)
        freeConfig
        (\conf -> H2Server.run conf server)

----------------------------------------------------------------

getServerParams
    :: Credentials
    -> ServerParams
getServerParams creds =
    def
        { serverSupported = supported
        , serverShared = shared
        , serverHooks = hooks
        }
  where
    shared =
        def
            { sharedCredentials = creds
            --            , sharedSessionManager = undefined
            }
    supported = strongSupported
    hooks =
        def
            { onALPNClientSuggest = Just alpn
            }

alpn :: [ByteString] -> IO ByteString
alpn xs
    | "h2" `elem` xs = return "h2"
    | otherwise = return "" -- fixme
