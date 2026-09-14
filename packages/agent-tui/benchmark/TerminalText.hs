{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified TerminalTextCandidate as Current
import qualified TerminalTextBaseline as Baseline
import Control.DeepSeq (force)
import Control.Exception (evaluate) -- safe-exceptions does not export evaluate.
import Control.Exception.Safe (bracket)
import Control.Monad (forM, unless)
import Data.IORef (newIORef, readIORef)
import Data.List (foldl', sort)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import Foreign.StablePtr (freeStablePtr, newStablePtr)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    unless enabled (die "run with +RTS -T")
    args <- getArgs
    case args of
        [mode, scenario, sizeArg, countArg, samplesArg] -> do
            size <- positive sizeArg
            count <- positive countArg
            sampleCount <- positive samplesArg
            render <- case mode of
                "old" -> pure Baseline.displayTerminalText
                "new" -> pure Current.displayTerminalText
                _ -> die "mode must be old or new"
            body <- fixture scenario size
            -- Validate independently, before constructing measurement input.
            unless (Baseline.displayTerminalText body == Current.displayTerminalText body)
                (die "output mismatch")
            measurements <- forM [1 .. sampleCount] $ \sample -> do
                inputs <- evaluate $ force
                    [ body <> Text.pack (show sample <> ":" <> show i)
                    | i <- [1 .. count]
                    ]
                inputRef <- newIORef inputs
                performGC
                before <- getRTSStats
                cpuBefore <- getCPUTime
                wallBefore <- getMonotonicTimeNSec
                freshInputs <- readIORef inputRef
                outputs <- evaluate $ force (map render freshInputs)
                _ <- evaluate (checksumTexts outputs)
                wallAfter <- getMonotonicTimeNSec
                cpuAfter <- getCPUTime
                -- A stable pointer makes liveness explicit even if GHC shares
                -- repeated checksum expressions across the collection.
                after <- bracket (newStablePtr (inputs, outputs)) freeStablePtr $ \_ -> do
                    performGC
                    getRTSStats
                pure
                    [ fromIntegral (wallAfter - wallBefore) / 1e6
                    , fromIntegral (cpuAfter - cpuBefore) / 1e9
                    , fromIntegral (allocated_bytes after - allocated_bytes before)
                    , fromIntegral (gcdetails_live_bytes (gc after))
                    ]
            let med :: Int -> Double
                med col = let xs = sort (map (!! col) measurements)
                          in xs !! (length xs `div` 2)
            printf "%s,%s,%d,%d,%d,%.3f,%.3f,%.0f,%.0f\n"
                mode scenario size count sampleCount
                (med 0) (med 1) (med 2) (med 3)
        _ -> die "usage: terminal-text-bench old|new ascii|mixed|unicode|control|late-unicode SIZE COUNT SAMPLES"

positive :: String -> IO Int
positive raw = case reads raw of
    [(n, "")] | n > 0 -> pure n
    _ -> die "expected positive integer"

fixture :: String -> Int -> IO Text
fixture scenario size = case scenario of
    "ascii" -> pure (fill "module Main where\nmain = putStrLn \"hello world\"\n")
    "mixed" -> pure (fill "Code review: value = 42; café 漢字 e\x0301\n")
    "unicode" -> pure (fill "漢字🙂👩\x200d💻e\x0301 1\xfe0f\x20e3\n")
    "control" -> pure (fill "\ESC[31moutput\BEL\t\r\DEL\n")
    "late-unicode" -> pure (Text.replicate size "x" <> "é")
    _ -> die "unknown scenario"
  where
    fill chunk = Text.take size (Text.replicate (size `div` Text.length chunk + 1) chunk)

checksumTexts :: [Text] -> Int
checksumTexts = foldl' (\acc text -> Text.foldl' (\n c -> n * 33 + fromEnum c) acc text) 5381
