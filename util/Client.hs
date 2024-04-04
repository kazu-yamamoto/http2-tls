{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Client where

import Control.Concurrent.Async
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Types

import Network.HTTP2.Client

client :: [Path] -> Client ()
client paths sendRequest _aux = do
    let client' path = do
            let req = requestNoBody methodGet path []
            sendRequest req $ \rsp -> do
                print $ responseStatus rsp
                getResponseBodyChunk rsp >>= C8.putStrLn
    ex <- E.try $ foldr1 concurrently_ $ map client' paths
    case ex of
        Right () -> return ()
        Left e -> print (e :: HTTP2Error)
