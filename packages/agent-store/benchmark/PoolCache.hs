{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Agent.Store.PoolCache
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar
import qualified Control.Exception as Exception
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Time.Clock
import System.Environment (getArgs)
import Text.Printf (printf)
import Text.Read (readMaybe)

data Sample = Sample
    { elapsedMillis :: !Double
    , openCount :: !Int
    }

main :: IO ()
main =
    getArgs >>= \case
        [countArg, delayArg, samplesArg] -> do
            count <- parsePositive "role count" countArg
            delayMillis <- parsePositive "open delay milliseconds" delayArg
            sampleCount <- parsePositive "sample count" samplesArg
            benchmark "old-different" sampleCount
                (oldDifferent count delayMillis)
            benchmark "new-different" sampleCount
                (newDifferent count delayMillis)
            benchmark "new-same-role" sampleCount
                (newSameRole count delayMillis)
        _ ->
            fail "usage: pool-cache-bench ROLE_COUNT DELAY_MS SAMPLE_COUNT"

benchmark :: String -> Int -> IO Sample -> IO ()
benchmark label sampleCount action = do
    samples <- mapM (const action) [1 .. sampleCount]
    let medianElapsed =
            sort (map (.elapsedMillis) samples)
                !! (sampleCount `div` 2)
        medianOpens =
            sort (map (.openCount) samples)
                !! (sampleCount `div` 2)
    printf "%s,%.3f,%d\n" label medianElapsed medianOpens

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
    started <- getCurrentTime
    _ <- action
    finished <- getCurrentTime
    count <- readMVar opens
    pure Sample
        { elapsedMillis =
            realToFrac (diffUTCTime finished started) * 1_000
        , openCount = count
        }

parsePositive :: String -> String -> IO Int
parsePositive label raw =
    case readMaybe raw of
        Just value | value > 0 -> pure value
        _ -> fail ("invalid " <> label <> ": " <> raw)
