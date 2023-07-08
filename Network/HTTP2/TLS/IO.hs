{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP2.TLS.IO where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Network.TLS hiding (HostName)
import System.IO.Error (isEOFError)
import qualified UnliftIO.Exception as E

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
