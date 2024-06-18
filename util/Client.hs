{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Client where

import Control.Concurrent.Async
import qualified Control.Exception as E
import Control.Monad (when)
import qualified Data.ByteString.Char8 as C8
import Data.CaseInsensitive (foldedCase)
import Network.HTTP.Semantics
import Network.HTTP.Types

import Network.HTTP2.Client

client :: Int -> [Path] -> Client ()
client n0 paths sendRequest _aux = do
    ex <- E.try $ foldr1 concurrently_ $ map (client' n0 sendRequest) paths
    case ex of
        Right () -> return ()
        Left e -> print (e :: HTTP2Error)

client' :: Int -> SendRequest -> Path -> IO ()
client' n0 sendRequest path = loop n0
  where
    req = requestNoBody methodGet path []
    loop 0 = return ()
    loop n = do
        sendRequest req $ \rsp -> do
            print $ responseStatus rsp
            mapM_ (\(k, v) -> C8.putStrLn (foldedCase (tokenKey k) <> ": " <> v)) $
                fst $
                    responseHeaders
                        rsp
            consume rsp
        loop (n - 1)
    consume rsp = do
        x <- getResponseBodyChunk rsp
        when (x /= "") $ consume rsp
