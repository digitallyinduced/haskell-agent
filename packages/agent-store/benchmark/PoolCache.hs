{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Agent.Store.PoolCache
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently, replicateConcurrently_)
import Control.Concurrent.MVar
import Control.Concurrent.STM
    ( atomically
    , modifyTVar'
    , newEmptyTMVarIO
    , newTQueueIO
    , newTVarIO
    , putTMVar
    , readTQueue
    , readTMVar
    , readTVarIO
    , writeTQueue
    )
import qualified Control.Exception as Exception
import Control.Exception (evaluate)
import Control.Monad (forM_, replicateM_, unless)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (RTSStats(..), getRTSStats, getRTSStatsEnabled)
import System.Environment (getArgs)
import System.Exit (die)
import System.CPUTime (getCPUTime)
import System.Mem (performGC)
import Text.Printf (printf)
import Text.Read (readMaybe)

data Sample = Sample
    { elapsedMillis :: !Double
    , cpuMillis :: !Double
    , openCount :: !Int
    , allocatedBytes :: !Integer
    }

main :: IO ()
main =
    getArgs >>= \case
        [countArg, delayArg, samplesArg] -> do
            count <- parsePositive "role count" countArg
            delayMillis <- parseNonNegative "open delay milliseconds" delayArg
            sampleCount <- parsePositive "sample count" samplesArg
            statsEnabled <- getRTSStatsEnabled
            unless statsEnabled $
                die "pool-cache-bench requires +RTS -T for allocation stats"
            benchmark statsEnabled "old-different" sampleCount
                (oldDifferent count delayMillis)
            benchmark statsEnabled "new-different" sampleCount
                (newDifferent count delayMillis)
            benchmark statsEnabled "new-same-role" sampleCount
                (newSameRole count delayMillis)
            benchmark statsEnabled "production-close" sampleCount
                (productionClose count)
            benchmark statsEnabled "old-slots" sampleCount
                (oldSlots count)
            benchmark statsEnabled "new-slots" sampleCount
                (newSlots count)
        _ ->
            fail "usage: pool-cache-bench ROLE_COUNT DELAY_MS SAMPLE_COUNT"

benchmark :: Bool -> String -> Int -> IO Sample -> IO ()
benchmark statsEnabled label sampleCount action = do
    samples <- mapM (const action) [1 .. sampleCount]
    let medianElapsed =
            sort (map (.elapsedMillis) samples)
                !! (sampleCount `div` 2)
        medianCpu =
            sort (map (.cpuMillis) samples)
                !! (sampleCount `div` 2)
        medianOpens =
            sort (map (.openCount) samples)
                !! (sampleCount `div` 2)
        medianAllocated =
            sort (map (.allocatedBytes) samples)
                !! (sampleCount `div` 2)
    -- Keep the argument in the API so callers cannot accidentally benchmark
    -- without the allocation guard when adding another scenario.
    unless statsEnabled $
        die "pool-cache-bench requires +RTS -T for allocation stats"
    printf "%s,%.3f,%.3f,%d,%d\n"
        label medianElapsed medianCpu medianOpens medianAllocated

oldDifferent :: Int -> Int -> IO Sample
oldDifferent count delayMillis = do
    state <- newMVar Map.empty
    opens <- newMVar (0 :: Int)
    measure opens $
        mapConcurrently
            (\key ->
                modifyMVar state \entries ->
                    case Map.lookup key entries of
                        Just resource -> pure (entries, resource)
                        Nothing -> do
                            recordOpen opens delayMillis
                            pure (Map.insert key key entries, key))
            [1 .. count]

-- Close only the result-collection workload, retaining a faithful copy of the
-- previous dense Map implementation as the baseline.
productionClose :: Int -> IO Sample
productionClose count = do
    opens <- newMVar 0
    cache <- benchmarkCache opens 0
    _ <- mapConcurrently (acquirePoolCache cache) [1 .. count]
    measure opens (closePoolCache cache)

oldSlots :: Int -> IO Sample
oldSlots count = do
    opens <- newMVar 0
    measure opens $
        legacyMapConcurrentlyBounded 8
            (\value -> pure (value :: Int))
            [1 .. count]
            >>= evaluate . checksum

newSlots :: Int -> IO Sample
newSlots count = do
    opens <- newMVar 0
    measure opens $
        slotMapConcurrentlyBounded 8
            (\value -> pure (value :: Int))
            [1 .. count]
            >>= evaluate . checksum

newDifferent :: Int -> Int -> IO Sample
newDifferent count delayMillis = do
    opens <- newMVar (0 :: Int)
    cache <- benchmarkCache opens delayMillis
    measure opens $
        mapConcurrently (acquirePoolCache cache) [1 .. count]

newSameRole :: Int -> Int -> IO Sample
newSameRole count delayMillis = do
    opens <- newMVar (0 :: Int)
    cache <- benchmarkCache opens delayMillis
    measure opens $
        mapConcurrently
            (const (acquirePoolCache cache (1 :: Int)))
            [1 .. count]

benchmarkCache :: MVar Int -> Int -> IO (PoolCache Int String Int)
benchmarkCache opens delayMillis =
    newPoolCache
        8
        "closed"
        Exception.displayException
        (\key -> recordOpen opens delayMillis >> pure (Right key))
        (const (pure ()))

recordOpen :: MVar Int -> Int -> IO ()
recordOpen opens delayMillis = do
    modifyMVar_ opens (pure . (+ 1))
    threadDelay (delayMillis * 1_000)

measure :: MVar Int -> IO a -> IO Sample
measure opens action = do
    performGC
    beforeStats <- getRTSStats
    beforeCpu <- getCPUTime
    started <- getMonotonicTimeNSec
    _ <- action
    finished <- getMonotonicTimeNSec
    afterCpu <- getCPUTime
    performGC
    afterStats <- getRTSStats
    count <- readMVar opens
    pure Sample
        { elapsedMillis =
            fromIntegral (finished - started) / 1.0e6
        , cpuMillis =
            fromIntegral (afterCpu - beforeCpu) / 1.0e9
        , openCount = count
        , allocatedBytes =
            fromIntegral
                (afterStats.allocated_bytes - beforeStats.allocated_bytes)
        }

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

slotMapConcurrentlyBounded :: Int -> (a -> IO b) -> [a] -> IO [b]
slotMapConcurrentlyBounded _ _ [] = pure []
slotMapConcurrentlyBounded requested action values = do
    let workerCount = min (max 1 requested) (length values)
    queue <- newTQueueIO
    slots <- mapM (const newEmptyTMVarIO) values
    atomically do
        forM_ (zip slots values) $
            writeTQueue queue . Just
        replicateM_ workerCount (writeTQueue queue Nothing)
    let worker =
            atomically (readTQueue queue) >>= \case
                Nothing -> pure ()
                Just (slot, value) -> do
                    result <- action value
                    result' <- evaluate result
                    atomically (putTMVar slot result')
                    worker
    replicateConcurrently_ workerCount worker
    mapM (atomically . readTMVar) slots

checksum :: [Int] -> Int
checksum = foldl' (\total value -> total * 31 + value) 0

parsePositive :: String -> String -> IO Int
parsePositive label raw =
    case readMaybe raw of
        Just value | value > 0 -> pure value
        _ -> fail ("invalid " <> label <> ": " <> raw)

parseNonNegative :: String -> String -> IO Int
parseNonNegative label raw =
    case readMaybe raw of
        Just value | value >= 0 -> pure value
        _ -> fail ("invalid " <> label <> ": " <> raw)
