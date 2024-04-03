{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.HTTP2.TLS.Server
import Network.TLS (Credentials (..), credentialLoadX509)
import System.Environment
import System.Exit

import Server

main :: IO ()
main = do
    args <- getArgs
    (host, port) <- case args of
        [h, p] -> return (h, p)
        _ -> do
            putStrLn "server <addr> <port>"
            exitFailure
    let optCertFile = "/Users/kazu/http/servercert.pem"
        optKeyFile = "/Users/kazu/http/serverkey.pem"
    Right cred@(!_cc, !_priv) <- credentialLoadX509 optCertFile optKeyFile
    let settings = defaultSettings
        creds = Credentials [cred]
    run settings creds host (read port) server
