{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as C8
import Data.IORef
import Network.HTTP2.TLS.Client
import Network.TLS
import System.Console.GetOpt
import System.Environment
import System.Exit

import Client

data Options = Options
    { optKeyLogFile :: Maybe FilePath
    , optValidate :: Bool
    , optResumption :: Bool
    , opt0RTT :: Bool
    , optNumOfReqs :: Int
    }
    deriving (Show)

defaultOptions :: Options
defaultOptions =
    Options
        { optKeyLogFile = Nothing
        , optValidate = False
        , optResumption = False
        , opt0RTT = False
        , optNumOfReqs = 1
        }

usage :: String
usage = "Usage: h2-client [OPTION] addr port [path]"

options :: [OptDescr (Options -> Options)]
options =
    [ Option
        ['l']
        ["key-log-file"]
        (ReqArg (\file o -> o{optKeyLogFile = Just file}) "<file>")
        "a file to store negotiated secrets"
    , Option
        ['e']
        ["validate"]
        (NoArg (\o -> o{optValidate = True}))
        "validate server's certificate"
    , Option
        ['R']
        ["resumption"]
        (NoArg (\o -> o{optResumption = True}))
        "try session resumption"
    , Option
        ['Z']
        ["0rtt"]
        (NoArg (\o -> o{opt0RTT = True}))
        "try sending early data"
    , Option
        ['n']
        ["number-of-requests"]
        (ReqArg (\n o -> o{optNumOfReqs = read n}) "<n>")
        "specify the number of requests"
    ]

showUsageAndExit :: String -> IO a
showUsageAndExit msg = do
    putStrLn msg
    putStrLn $ usageInfo usage options
    exitFailure

clientOpts :: [String] -> IO (Options, [String])
clientOpts argv =
    case getOpt Permute options argv of
        (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
        (_, _, errs) -> showUsageAndExit $ concat errs

main :: IO ()
main = do
    args <- getArgs
    (Options{..}, ips) <- clientOpts args
    (host, port, paths) <- case ips of
        [] -> showUsageAndExit usage
        _ : [] -> showUsageAndExit usage
        h : p : [] -> return (h, read p, ["/"])
        h : p : ps -> return (h, read p, C8.pack <$> ps)
    ref <- newIORef Nothing
    let keylog msg = case optKeyLogFile of
            Nothing -> return ()
            Just file -> appendFile file (msg ++ "\n")
        settings =
            defaultSettings
                { settingsValidateCert = optValidate
                , settingsKeyLogger = keylog
                , settingsSessionManager = sessionRef ref
                }
    run settings host port $ client optNumOfReqs paths
    when (optResumption || opt0RTT) $ do
        mr <- readIORef ref
        case mr of
            Nothing -> do
                putStrLn "No session data"
                exitFailure
            _ -> do
                let settings2 =
                        defaultSettings
                            { settingsValidateCert = optValidate
                            , settingsKeyLogger = keylog
                            , settingsWantSessionResume = mr
                            , settingsUseEarlyData = opt0RTT
                            }
                run settings2 host port $ client optNumOfReqs paths

sessionRef :: IORef (Maybe (SessionID, SessionData)) -> SessionManager
sessionRef ref =
    noSessionManager
        { sessionEstablish = \sid sdata -> do
            writeIORef ref $ Just (sid, sdata)
            return Nothing
        }
