-- | Compare the sequential control-handler shutdown loop against concurrent
-- bounded cancellation.
--
-- Usage:
-- control-shutdown-bench IMPLEMENTATION WORKERS TIMEOUT_US CLEANUP_US SAMPLES +RTS -T
module Main (main) where

import Control.Concurrent
    ( newEmptyMVar
    , putMVar
    , takeMVar
    , threadDelay
    )
import Control.Concurrent.Async
    ( Async
    , cancel
    , mapConcurrently_
    , withAsync
    )
import Control.Exception (evaluate)
import Control.Exception.Safe
    ( finally
    , uninterruptibleMask_
    )
import Control.Monad (replicateM, void)
import Data.List (sort)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
    ( RTSStats(..)
    , getRTSStats
    , getRTSStatsEnabled
    )
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import System.Timeout (timeout)
import Text.Printf (printf)

data Implementation
    = Sequential
    | Concurrent

data Sample = Sample
    { wallMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Integer
    }

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    if enabled
        then pure ()
        else die "RTS statistics are disabled; run with +RTS -T"
    getArgs >>= \case
        [implementationArg, workersArg, timeoutArg, cleanupArg, samplesArg] -> do
            implementation <- parseImplementation implementationArg
            workerCount <- parsePositive "worker count" workersArg
            timeoutMicros <- parsePositive "timeout" timeoutArg
            cleanupMicros <- parsePositive "cleanup delay" cleanupArg
            sampleCount <- parsePositive "sample count" samplesArg
            samples <-
                mapM
                    (const $
                        runSample
                            implementation
                            workerCount
                            timeoutMicros
                            cleanupMicros)
                    [1 .. sampleCount]
            let result = median samples
            printf
                "%s,%d,%d,%d,%.3f,%.3f,%d\n"
                implementationArg
                workerCount
                timeoutMicros
                cleanupMicros
                result.wallMillis
                result.cpuMillis
                result.allocatedBytes
        _ ->
            die $
                "usage: control-shutdown-bench IMPLEMENTATION WORKERS "
                    <> "TIMEOUT_US CLEANUP_US SAMPLES\n"
                    <> "implementations: sequential, concurrent"

parseImplementation :: String -> IO Implementation
parseImplementation = \case
    "sequential" -> pure Sequential
    "concurrent" -> pure Concurrent
    other -> die ("unknown implementation: " <> other)

parsePositive :: String -> String -> IO Int
parsePositive label raw =
    case reads raw of
        [(value, "")]
            | value > 0 -> pure value
        _ -> die ("invalid " <> label <> ": " <> raw)

runSample :: Implementation -> Int -> Int -> Int -> IO Sample
runSample implementation workerCount timeoutMicros cleanupMicros = do
    started <- replicateM workerCount newEmptyMVar
    let workerBody ready =
            (putMVar ready () >> threadDelay maxBound)
                `finally`
                    uninterruptibleMask_ (threadDelay cleanupMicros)
    withWorkers (map workerBody started) \workers -> do
        mapM_ takeMVar started
        measure do
            case implementation of
                Sequential ->
                    mapM_ (cancelOnlyWithin timeoutMicros) workers
                Concurrent ->
                    mapConcurrently_
                        (cancelOnlyWithin timeoutMicros)
                        workers
            pure (length workers)

withWorkers :: [IO ()] -> ([Async ()] -> IO a) -> IO a
withWorkers actions use = go actions []
  where
    go [] workers =
        use (reverse workers)
    go (action : remaining) workers =
        withAsync action \worker ->
            go remaining (worker : workers)

cancelOnlyWithin :: Int -> Async a -> IO ()
cancelOnlyWithin timeoutMicros worker =
    void $ timeout timeoutMicros (cancel worker)

measure :: IO Int -> IO Sample
measure action = do
    performGC
    beforeStats <- getRTSStats
    beforeCPU <- getCPUTime
    beforeWall <- getMonotonicTimeNSec
    result <- action
    _ <- evaluate result
    afterWall <- getMonotonicTimeNSec
    afterCPU <- getCPUTime
    performGC
    afterStats <- getRTSStats
    pure
        Sample
            { wallMillis =
                fromIntegral (afterWall - beforeWall) / 1_000_000
            , cpuMillis =
                fromIntegral (afterCPU - beforeCPU) / 1_000_000_000
            , allocatedBytes =
                fromIntegral
                    ( afterStats.allocated_bytes
                        - beforeStats.allocated_bytes
                    )
            }

median :: [Sample] -> Sample
median samples =
    Sample
        { wallMillis = middle (sort (map (.wallMillis) samples))
        , cpuMillis = middle (sort (map (.cpuMillis) samples))
        , allocatedBytes = middle (sort (map (.allocatedBytes) samples))
        }
  where
    middle values = values !! (length values `div` 2)
