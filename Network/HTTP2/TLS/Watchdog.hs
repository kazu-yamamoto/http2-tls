{-# LANGUAGE TypeFamilies #-}

-- | What this package records about a connection it is establishing, and
--   how long it gives the peer to make progress.
--
--   There is one rule: the peer must keep the connection moving.  A read
--   large enough not to be a slowloris, or a write that returned, is
--   progress and restarts the timer; anything else and the connection is
--   given up on.
--
--   This covers the connection up to where the http2 library takes it
--   over, which is where 'handOver' is called: from there, what the
--   connection is doing and how long it may take are http2's to decide,
--   and it supervises the connection with a watchdog of its own.
module Network.HTTP2.TLS.Watchdog (
    Establishing,
    TLSWatchdog,
    newTLSWatchdog,
    progress,
    timedOutSTM,
    handOver,
    withWatchdog,
) where

import Control.Concurrent.STM (STM, retry)
import Control.Monad (when)
import Data.Word (Word64)
import System.Watchdog hiding (
    handOver,
    isTimedOut,
    newWatchdog,
    timedOutSTM,
    withWatchdog,
 )
import qualified System.Watchdog as W

-- | A connection of this package's, until http2 has it.
data Establishing

-- | The watchdog of one such connection.
--
--   With no timeout there is nothing to decide, and recording progress
--   costs nothing: the flag is here, rather than in the context, so that
--   a read on a hot path is not even a transaction.
data TLSWatchdog = TLSWatchdog !Bool !(Watchdog Establishing)

-- | Creating one with a timeout in microseconds. Zero or less means the
--   connection never times out.
newTLSWatchdog :: Int -> IO TLSWatchdog
newTLSWatchdog us = do
    wd <- W.newWatchdog $ EstablishingContext (fromIntegral $ max 0 us) 0
    -- Nothing to watch, so no thread watches it.
    when (us <= 0) $ W.handOver wd
    return $ TLSWatchdog (us > 0) wd

instance WatchdogFor Establishing where
    data ContextFor Establishing = EstablishingContext
        { estTimeout :: Word64
        , estProgress :: Int
        }

    decide mold new = return $ case mold of
        -- The watchdog starting: the connection has yet to do anything,
        -- and what it is not doing is already on the clock.
        Nothing -> restart
        Just old
            | estProgress old == estProgress new -> Ignore
            | otherwise -> restart
      where
        restart = setTimeout $ estTimeout new

-- | The connection moved.
progress :: TLSWatchdog -> IO ()
progress (TLSWatchdog False _) = return ()
progress (TLSWatchdog True wd) =
    update (\c -> c{estProgress = estProgress c + 1}) wd

-- | Succeeding once the connection has timed out.
timedOutSTM :: TLSWatchdog -> STM ()
timedOutSTM (TLSWatchdog False _) = retry
timedOutSTM (TLSWatchdog True wd) = W.timedOutSTM wd

-- | Handing the connection to the http2 library.
handOver :: TLSWatchdog -> IO ()
handOver (TLSWatchdog _ wd) = W.handOver wd

-- | Supervising an action with the watchdog thread.
withWatchdog :: TLSWatchdog -> IO () -> IO a -> IO a
withWatchdog (TLSWatchdog False _) _ action = action
withWatchdog (TLSWatchdog True wd) lastResort action =
    W.withWatchdog wd lastResort action
