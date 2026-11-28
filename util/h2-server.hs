{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Network.HTTP2.TLS.Server
import Network.TLS (Credentials (..), credentialLoadX509)
import Network.TLS.SessionTicket
import System.Console.GetOpt
import System.Environment
import System.Exit
#if !defined(mingw32_HOST_OS)
import System.Posix.Signals (Handler (..), installHandler, sigINT, sigTERM)
#endif

import Server

options :: [OptDescr (Options -> Options)]
options =
    [ Option
        ['l']
        ["key-log-file"]
        (ReqArg (\file o -> o{optKeyLogFile = Just file}) "<file>")
        "a file to store negotiated secrets"
    , Option
        ['c']
        ["cert"]
        (ReqArg (\fl o -> o{optCertFile = fl}) "<file>")
        "certificate file"
    , Option
        ['k']
        ["key"]
        (ReqArg (\fl o -> o{optKeyFile = fl}) "<file>")
        "key file"
    ]

showUsageAndExit :: String -> IO a
showUsageAndExit msg = do
    putStrLn msg
    putStrLn $ usageInfo usage options
    exitFailure

serverOpts :: [String] -> IO (Options, [String])
serverOpts argv =
    case getOpt Permute options argv of
        (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
        (_, _, errs) -> showUsageAndExit $ concat errs

data Options = Options
    { optKeyLogFile :: Maybe FilePath
    , optCertFile :: FilePath
    , optKeyFile :: FilePath
    }
    deriving (Show)

defaultOptions :: Options
defaultOptions =
    Options
        { optKeyLogFile = Nothing
        , optCertFile = "servercert.pem"
        , optKeyFile = "serverkey.pem"
        }

usage :: String
usage = "Usage: h2-server [OPTION] <addr> <port>"

main :: IO ()
main = do
    args <- getArgs
    (Options{..}, ips) <- serverOpts args
    (host, port) <- case ips of
        [h, p] -> return (h, p)
        _ -> showUsageAndExit usage
    cred@(!_cc, !_priv) <-
        credentialLoadX509 optCertFile optKeyFile >>= either showUsageAndExit return
    sm <- newSessionTicketManager defaultConfig
    let keylog = case optKeyLogFile of
            Nothing -> settingsKeyLogger defaultSettings
            Just file -> \msg -> appendFile file (msg ++ "\n")
        settings =
            defaultSettings
                { settingsKeyLogger = keylog
                , settingsSessionManager = sm
                , settingsEarlyDataSize = 4096
                , settingsInstallShutdownHandler = installShutdownHandler
                }
        creds = Credentials [cred]
    run settings creds host (read port) server

-- | SIGINT and SIGTERM stop the server: each connection is sent GOAWAY
-- and closed once its streams are done.
installShutdownHandler :: IO () -> IO ()
#if defined(mingw32_HOST_OS)
installShutdownHandler _ = return ()
#else
installShutdownHandler stop = do
    _ <- installHandler sigINT (Catch stop) Nothing
    _ <- installHandler sigTERM (Catch stop) Nothing
    return ()
#endif
