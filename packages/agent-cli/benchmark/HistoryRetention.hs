module Main (main) where

import Agent.CLI.TUI.Bridge (trimHistory)
import Control.Exception (evaluate)
import Control.Monad (forM)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
    ( GCDetails(..)
    , RTSStats(..)
    , getRTSStats
    , getRTSStatsEnabled
    )
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)

data Workload
    = Unbounded
    | Bounded

data Sample = Sample
    { elapsedMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Integer
    , liveBytes :: !Integer
    }

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    if enabled
        then pure ()
        else die "RTS statistics are disabled; run with +RTS -T"
    getArgs >>= \case
        [workloadArg, entryCountArg, entrySizeArg, sampleCountArg] -> do
            workload <- parseWorkload workloadArg
            entryCount <- parsePositive "entry count" entryCountArg
            entrySize <- parsePositive "entry size" entrySizeArg
            sampleCount <- parsePositive "sample count" sampleCountArg
            samples <- forM [1 .. sampleCount] \sampleIndex ->
                measure workload sampleIndex entryCount entrySize
            let sample = median samples
            printf
                "%s,%d,%d,%.3f,%.3f,%d,%d\n"
                workloadArg
                entryCount
                entrySize
                sample.elapsedMillis
                sample.cpuMillis
                sample.allocatedBytes
                sample.liveBytes
        _ ->
            die $
                "usage: history-retention-bench WORKLOAD ENTRIES ENTRY_SIZE SAMPLES\n"
                    <> "workloads: unbounded, bounded\n"
                    <> "output: workload,entries,entry_size,elapsed_ms,cpu_ms,"
                    <> "allocated_bytes,live_bytes"

parseWorkload :: String -> IO Workload
parseWorkload = \case
    "unbounded" -> pure Unbounded
    "bounded" -> pure Bounded
    other -> die ("unknown workload: " <> other)

parsePositive :: String -> String -> IO Int
parsePositive label raw =
    case reads raw of
        [(value, "")]
            | value > 0 -> pure value
        _ -> die ("invalid " <> label <> ": " <> raw)

measure :: Workload -> Int -> Int -> Int -> IO Sample
measure workload sampleIndex entryCount entrySize = do
    performGC
    beforeStats <- getRTSStats
    beforeCpu <- getCPUTime
    beforeElapsed <- getMonotonicTimeNSec
    history <- buildHistory workload sampleIndex entryCount entrySize
    residentChecksum <- evaluate (checksumHistory history)
    performGC
    afterStats <- getRTSStats
    -- Keep the selected history live through the measured collection.
    finalChecksum <- evaluate (checksumHistory history)
    afterElapsed <- getMonotonicTimeNSec
    afterCpu <- getCPUTime
    _ <- evaluate (residentChecksum + finalChecksum)
    pure Sample
        { elapsedMillis =
            fromIntegral (afterElapsed - beforeElapsed) / 1.0e6
        , cpuMillis =
            fromIntegral (afterCpu - beforeCpu) / 1.0e9
        , allocatedBytes =
            fromIntegral
                (afterStats.allocated_bytes - beforeStats.allocated_bytes)
        , liveBytes =
            fromIntegral afterStats.gc.gcdetails_live_bytes
        }

buildHistory :: Workload -> Int -> Int -> Int -> IO [Text]
buildHistory workload sampleIndex entryCount entrySize = do
    let full =
            [ Text.pack (show sampleIndex <> ":" <> show entryIndex <> ":")
                <> Text.replicate entrySize "x"
            | entryIndex <- [1 .. entryCount]
            ]
    _ <- evaluate (checksumHistory full)
    pure case workload of
        Unbounded -> full
        Bounded -> trimHistory full

checksumHistory :: [Text] -> Int
checksumHistory =
    foldl'
        (\checksum text ->
            Text.foldl'
                (\value character -> value * 33 + fromEnum character)
                checksum
                text)
        5381

median :: [Sample] -> Sample
median samples =
    Sample
        { elapsedMillis = middle (sort (map (.elapsedMillis) samples))
        , cpuMillis = middle (sort (map (.cpuMillis) samples))
        , allocatedBytes = middle (sort (map (.allocatedBytes) samples))
        , liveBytes = middle (sort (map (.liveBytes) samples))
        }
  where
    middle values = values !! (length values `div` 2)
