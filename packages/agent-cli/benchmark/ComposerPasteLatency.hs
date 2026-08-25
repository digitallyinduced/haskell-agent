module Main (main) where

import Agent.CLI.TUI.Composer (wrapDraft, wrapDraftWindow)
import Control.Exception (evaluate)
import Control.Monad (forM)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Stats
    ( RTSStats(..)
    , getRTSStats
    , getRTSStatsEnabled
    )
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)

data Workload = DuplicateLayout | SharedLayout | WindowedLayout

data Sample = Sample
    { elapsedMillis :: !Double
    , allocatedBytes :: !Integer
    }

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    if enabled
        then pure ()
        else die "RTS statistics are disabled; run with +RTS -T"
    getArgs >>= \case
        [workloadArg, lineCountArg, lineSizeArg, widthArg, sampleCountArg] -> do
            workload <- parseWorkload workloadArg
            lineCount <- parsePositive "line count" lineCountArg
            lineSize <- parsePositive "line size" lineSizeArg
            width <- parsePositive "composer width" widthArg
            sampleCount <- parsePositive "sample count" sampleCountArg
            samples <- forM [1 .. sampleCount] \sampleIndex -> do
                let character =
                        Text.singleton
                            (toEnum (fromEnum 'a' + sampleIndex `mod` 26))
                    line = Text.replicate lineSize character
                    draft = Text.intercalate "\n" (replicate lineCount line)
                    cursor = Text.length draft
                _ <- evaluate (Text.foldl' (\n char -> n + fromEnum char) cursor draft)
                measure
                    (runWorkload workload width draft cursor sampleIndex)
            let sample = median samples
            printf
                "%s,%d,%d,%d,%.3f,%d\n"
                workloadArg
                lineCount
                lineSize
                width
                sample.elapsedMillis
                sample.allocatedBytes
        _ ->
            die $
                "usage: composer-paste-latency-bench "
                    <> "WORKLOAD LINES LINE_SIZE WIDTH SAMPLES\n"
                    <> "workloads: duplicate-layout, shared-layout, "
                    <> "windowed-layout"

parseWorkload :: String -> IO Workload
parseWorkload = \case
    "duplicate-layout" -> pure DuplicateLayout
    "shared-layout" -> pure SharedLayout
    "windowed-layout" -> pure WindowedLayout
    other -> die ("unknown workload: " <> other)

parsePositive :: String -> String -> IO Int
parsePositive label raw =
    case reads raw of
        [(value, "")]
            | value > 0 -> pure value
        _ -> die ("invalid " <> label <> ": " <> raw)

runWorkload :: Workload -> Int -> Text -> Int -> Int -> IO Int
runWorkload workload width draft cursor sampleIndex =
    case workload of
        DuplicateLayout -> do
            first <- evaluate
                (layoutChecksum width draft (max 0 (cursor - sampleIndex)))
            -- A nearby cursor prevents compiler common-subexpression
            -- elimination while preserving the old renderer's two full passes.
            second <- evaluate
                (layoutChecksum
                    width
                    draft
                    (max 0 (cursor - sampleIndex - 1)))
            pure (first + second)
        SharedLayout ->
            evaluate
                (layoutChecksum width draft (max 0 (cursor - sampleIndex)))
        WindowedLayout ->
            evaluate
                (windowedLayoutChecksum
                    width
                    draft
                    (max 0 (cursor - sampleIndex)))

{-# NOINLINE layoutChecksum #-}
layoutChecksum :: Int -> Text -> Int -> Int
layoutChecksum width draft cursor =
    let (rows, (row, column)) = wrapDraft width draft cursor
    in sum (map Text.length rows) + row + column

{-# NOINLINE windowedLayoutChecksum #-}
windowedLayoutChecksum :: Int -> Text -> Int -> Int
windowedLayoutChecksum width draft cursor =
    let (rows, (row, column)) = wrapDraftWindow 8 width draft cursor
    in sum (map Text.length rows) + row + column

measure :: IO Int -> IO Sample
measure action = do
    performGC
    beforeStats <- getRTSStats
    beforeTime <- getCPUTime
    checksum <- action
    _ <- evaluate checksum
    afterTime <- getCPUTime
    performGC
    afterStats <- getRTSStats
    pure Sample
        { elapsedMillis =
            fromIntegral (afterTime - beforeTime) / 1.0e9
        , allocatedBytes =
            fromIntegral
                (afterStats.allocated_bytes - beforeStats.allocated_bytes)
        }

median :: [Sample] -> Sample
median samples =
    sortOn (.elapsedMillis) samples !! (length samples `div` 2)
