module Main (main) where

import Agent.Concurrent (mapConcurrentlyBounded)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (replicateConcurrently_)
import Control.Exception (evaluate)
import Control.Monad (forM, forM_, replicateM_)
import Control.Concurrent.STM
    ( atomically
    , modifyTVar'
    , newTQueueIO
    , newTVarIO
    , readTQueue
    , readTVarIO
    , writeTQueue
    )
import qualified Data.Map.Strict as Map
import Data.List (sort)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
    ( RTSStats(..)
    , getRTSStats
    , getRTSStatsEnabled
    )
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

-- | One delayed operation models an independent discovery probe (for
-- example, reading metadata for a candidate file or skill).  The result is
-- deliberately consumed by the benchmark so neither traversal can be
-- optimized away.
data DiscoveryResult = DiscoveryResult
    { discoveryIndex :: !Int
    , discoveryChecksum :: !Int
    }

data Sample = Sample
    { elapsedMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Integer
    }

data Scenario = Scenario
    { taskCount :: !Int
    , workerLimit :: !Int
    , delayMicros :: !Int
    , sampleCount :: !Int
    }

main :: IO ()
main = do
    statsEnabled <- getRTSStatsEnabled
    if statsEnabled
        then pure ()
        else die "concurrent-discovery-bench requires +RTS -T"
    scenarios <- getArgs >>= parseScenarios
    putStrLn "mode,tasks,limit,delay_us,samples,elapsed_ms,cpu_ms,allocated_bytes"
    forM_ scenarios \scenario -> do
        serial <- measureMany scenario.sampleCount statsEnabled (runSerial scenario)
        oldMap <- measureMany scenario.sampleCount statsEnabled (runLegacy scenario)
        bounded <- measureMany scenario.sampleCount statsEnabled (runSlots scenario)
        printResult "serial" scenario serial
        printResult "old-map" scenario oldMap
        printResult "new-slots" scenario bounded

parseScenarios :: [String] -> IO [Scenario]
parseScenarios [] =
    pure
        [ Scenario tasks limit 1000 5
        | tasks <- [8, 32, 128]
        , limit <- [1, 4, 8, 16]
        ]
parseScenarios [tasksArg, limitArg, delayArg, samplesArg] = do
    tasks <- parsePositive "task count" tasksArg
    limit <- parsePositive "worker limit" limitArg
    delay <- parseNonNegative "delay microseconds" delayArg
    samples <- parsePositive "sample count" samplesArg
    pure [Scenario tasks limit delay samples]
parseScenarios _ =
    die
        "usage: concurrent-discovery-bench [TASKS LIMIT DELAY_US SAMPLES]\n\
        \without arguments, runs tasks 8,32,128 x limits 1,4,8,16 (1ms delay)"

parsePositive :: String -> String -> IO Int
parsePositive label raw =
    case reads raw of
        [(value, "")]
            | value > 0 -> pure value
        _ -> die ("invalid " <> label <> ": " <> raw)

parseNonNegative :: String -> String -> IO Int
parseNonNegative label raw =
    case reads raw of
        [(value, "")]
            | value >= 0 -> pure value
        _ -> die ("invalid " <> label <> ": " <> raw)

runSerial :: Scenario -> IO Int
runSerial scenario =
    consumeResults <$> mapM (discover scenario.delayMicros) [1 .. scenario.taskCount]

runSlots :: Scenario -> IO Int
runSlots scenario =
    consumeResults
        <$> mapConcurrentlyBounded
            scenario.workerLimit
            (discover scenario.delayMicros)
            [1 .. scenario.taskCount]

-- Keep the former result-table implementation in the benchmark so allocation
-- and CPU deltas can be measured against the slot-based implementation.
runLegacy :: Scenario -> IO Int
runLegacy scenario =
    consumeResults
        <$> legacyMapConcurrentlyBounded
            scenario.workerLimit
            (discover scenario.delayMicros)
            [1 .. scenario.taskCount]

legacyMapConcurrentlyBounded :: Int -> (a -> IO b) -> [a] -> IO [b]
legacyMapConcurrentlyBounded _ _ [] = pure []
legacyMapConcurrentlyBounded requested action values = do
    let workerCount = min (max 1 requested) (length values)
    queue <- newTQueueIO
    results <- newTVarIO Map.empty
    atomically do
        forM_ (zip [0 :: Int ..] values) $
            writeTQueue queue . Just
        replicateM_ workerCount (writeTQueue queue Nothing)
    let worker =
            atomically (readTQueue queue) >>= \case
                Nothing -> pure ()
                Just (index, value) -> do
                    result <- action value
                    atomically $
                        modifyTVar' results (Map.insert index result)
                    worker
    replicateConcurrently_ workerCount worker
    completed <- readTVarIO results
    pure
        [ completed Map.! index
        | index <- [0 .. length values - 1]
        ]

discover :: Int -> Int -> IO DiscoveryResult
discover delayMicros index = do
    threadDelay delayMicros
    pure
        DiscoveryResult
            { discoveryIndex = index
            , discoveryChecksum = index * 31 + delayMicros
            }

consumeResults :: [DiscoveryResult] -> Int
consumeResults =
    foldr
        (\result total ->
            total
                + result.discoveryIndex
                + result.discoveryChecksum)
        0

measureMany :: Int -> Bool -> IO Int -> IO Sample
measureMany sampleCount statsEnabled action = do
    samples <- forM [1 .. sampleCount] \_ -> measureOne statsEnabled action
    pure (median samples)

measureOne :: Bool -> IO Int -> IO Sample
measureOne statsEnabled action = do
    performGC
    beforeStats <- if statsEnabled then Just <$> getRTSStats else pure Nothing
    beforeCpu <- getCPUTime
    beforeWall <- getMonotonicTimeNSec
    result <- action
    evaluate result
    afterWall <- getMonotonicTimeNSec
    afterCpu <- getCPUTime
    performGC
    afterStats <- if statsEnabled then Just <$> getRTSStats else pure Nothing
    pure
        Sample
            { elapsedMillis =
                fromIntegral (afterWall - beforeWall) / 1.0e6
            , cpuMillis =
                fromIntegral (afterCpu - beforeCpu) / 1.0e9
            , allocatedBytes = allocatedDelta beforeStats afterStats
            }

allocatedDelta :: Maybe RTSStats -> Maybe RTSStats -> Integer
allocatedDelta (Just beforeStats) (Just afterStats) =
    fromIntegral (afterStats.allocated_bytes - beforeStats.allocated_bytes)
allocatedDelta _ _ = 0

printResult :: String -> Scenario -> Sample -> IO ()
printResult mode scenario sample =
    printf
        "%s,%d,%d,%d,%d,%.3f,%.3f,%d\n"
        mode
        scenario.taskCount
        scenario.workerLimit
        scenario.delayMicros
        scenario.sampleCount
        sample.elapsedMillis
        sample.cpuMillis
        sample.allocatedBytes

median :: [Sample] -> Sample
median samples =
    Sample
        { elapsedMillis = middle (sort (map (.elapsedMillis) samples))
        , cpuMillis = middle (sort (map (.cpuMillis) samples))
        , allocatedBytes = middle (sort (map (.allocatedBytes) samples))
        }
  where
    middle values = values !! (length values `div` 2)
