module Main where

import Network.HTTP2.TLS.Client
import System.Environment
import System.Exit

import Client

main :: IO ()
main = do
    args <- getArgs
    (host, port) <- case args of
        [h, p] -> return (h, p)
        _ -> do
            putStrLn "client <addr> <port>"
            exitFailure
    let settings = defaultSettings{settingsValidateCert = False}
    run settings host (read port) client
