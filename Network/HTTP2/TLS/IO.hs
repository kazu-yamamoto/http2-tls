{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP2.TLS.IO where

import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Either (isRight)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Network.Socket
import Network.Socket.BufferPool
import qualified Network.Socket.ByteString as NSB
import Network.TLS hiding (HostName)
import Network.HTTP2.Client (HTTP2Error (..))
import System.IO.Error (isEOFError)
import qualified System.IO.Error as E
import qualified System.TimeManager as T

import Network.HTTP2.TLS.Server.Settings

----------------------------------------------------------------

-- HTTP2: confReadN == recvTLS
-- TLS:   recvData  == contextRecv == backendRecv

----------------------------------------------------------------

-- | A receiving function which gives up when the 'STM' action fires
--   first.  It waits in 'STM', so the timeout's kill reaches it on
--   Windows too.
mkRecvTCP :: Settings -> STM () -> Socket -> IO (IO ByteString)
mkRecvTCP Settings{..} giveUp sock = do
    pool <- newBufferPool settingsReadBufferLowerLimit settingsReadBufferSize
    return $ do
        r <- receiveWith sock pool giveUp
        case r of
            Left () -> E.throwIO ConnectionIsTimeout
            Right bs -> return bs

sendTCP :: Socket -> ByteString -> IO ()
sendTCP sock = NSB.sendAll sock

-- | 'sendTCP' which gives up when the 'STM' action fires first.
sendTCPWith :: Socket -> STM () -> ByteString -> IO (Either () ())
sendTCPWith sock giveUp bs = NSB.sendAllWith sock bs giveUp

----------------------------------------------------------------

-- | Sending and receiving functions.
--   Tiemout is reset when they return.
--   One exception is the slowloris attach prevention.
--   See 'settingsSlowlorisSize'.
data IOBackend = IOBackend
    { send :: ByteString -> IO ()
    -- ^ Sending.
    , sendWith :: STM () -> ByteString -> IO (Either () ())
    -- ^ Sending which gives up when the 'STM' action fires first,
    --   answering 'Left'.  Part of the data may have gone by then, so the
    --   connection is to be closed.
    --
    --   @since 0.5.7
    , sendMany :: [ByteString] -> IO ()
    -- ^ Sending many.
    , recv :: IO ByteString
    -- ^ Receiving.
    , requestSock :: Socket
    -- ^ The socket for the request
    , mySockAddr :: SockAddr
    , peerSockAddr :: SockAddr
    }

timeoutIOBackend :: T.Handle -> Settings -> IOBackend -> IOBackend
timeoutIOBackend th Settings{..} IOBackend{..} =
    IOBackend
        { send = send'
        , sendWith = sendWith'
        , sendMany = sendMany'
        , recv = recv'
        , ..
        }
  where
    send' bs = send bs >> T.tickle th
    sendWith' giveUp bs = do
        r <- sendWith giveUp bs
        when (isRight r) $ T.tickle th
        return r
    sendMany' bss = sendMany bss >> T.tickle th
    recv' = do
        bs <- recv
        when (BS.length bs > settingsSlowlorisSize) $ T.tickle th
        return bs

-- | The 'TVar' is the one given to 'mkBackend': what a send of the
--   backend gives up on.
tlsIOBackend :: TVar (STM ()) -> Context -> Socket -> IO IOBackend
tlsIOBackend giveUpVar ctx sock = do
    mysa <- getSocketName sock
    peersa <- getPeerName sock
    return $
        IOBackend
            { send = sendTLS ctx
            , sendWith = \giveUp bs -> do
                atomically $ writeTVar giveUpVar giveUp
                (Right () <$ sendTLS ctx bs) `E.catch` gaveUp
            , sendMany = sendManyTLS ctx
            , recv = recvTLS ctx
            , requestSock = sock
            , mySockAddr = mysa
            , peerSockAddr = peersa
            }

tcpIOBackend :: Settings -> Socket -> IO IOBackend
tcpIOBackend settings sock = do
    recv' <- mkRecvTCP settings retry sock
    mysa <- getSocketName sock
    peersa <- getPeerName sock
    return $
        IOBackend
            { send = sendTCP sock
            , sendWith = sendTCPWith sock
            , sendMany = \_ -> return ()
            , recv = recv'
            , requestSock = sock
            , mySockAddr = mysa
            , peerSockAddr = peersa
            }

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
        | Just (PostHandshake Error_EOF) <- E.fromException e = return ""
        | Just ioe <- E.fromException e, isEOFError ioe = return ""
        | otherwise = E.throwIO e

----------------------------------------------------------------

-- | A send of the backend gives up on what the 'TVar' holds, which the
--   http2 library sets once it sends through 'sendWith'.
mkBackend :: Settings -> TVar (STM ()) -> Socket -> IO Backend
mkBackend settings giveUpVar sock = do
    let send' bs = do
            giveUp <- readTVarIO giveUpVar
            sendTCPWith sock giveUp bs
                >>= either (\_ -> E.throwIO ConnectionIsTimeout) return
    recv' <- mkRecvTCP settings retry sock
    recvN <- makeRecvN "" recv'
    return
        Backend
            { backendFlush = return ()
            , backendClose =
                gracefulClose sock 5000 `E.catchIOError` \_ -> return ()
            , backendSend = send'
            , backendRecv = recvN
            }

-- | close_notify, as a last word: a send which the http2 library has
--   given up on is not an error here.
byeGivingUp :: Context -> IO ()
byeGivingUp ctx = bye ctx `E.catch` \e -> () <$ gaveUp e

gaveUp :: HTTP2Error -> IO (Either () a)
gaveUp ConnectionIsTimeout = return $ Left ()
gaveUp e = E.throwIO e
