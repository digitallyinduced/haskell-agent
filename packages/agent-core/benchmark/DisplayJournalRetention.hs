{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- Cumulative streamed tool snapshots, measured while the response attempt is
-- still live. Unlike a prebuilt input list, the producer cannot root obsolete
-- snapshots independently of the journal. Run independent processes for RSS.
module Main (main) where

import Agent.Loop.DisplayJournal
import Agent.Loop.Output (LoopEvent(..))
import Agent.ToolDispatch (ToolCall(..), functionToolCall)
-- safe-exceptions does not export evaluate.
import Control.Exception (evaluate)
import Control.Exception.Safe (bracket)
import Control.Monad (forM_, unless)
import Data.Char (ord)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as Text
import Foreign.StablePtr (newStablePtr, freeStablePtr, deRefStablePtr)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)

data Workload = Arguments | Output | Mixed | TextOnly
    deriving (Eq, Show)

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    unless enabled (die "run with +RTS -T")
    getArgs >>= \case
        [kind, countArg, stepArg, callsArg, samplesArg] -> do
            workload <- case kind of
                "arguments" -> pure Arguments
                "output" -> pure Output
                "mixed" -> pure Mixed
                "text" -> pure TextOnly
                _ -> die "workload: arguments | output | mixed | text"
            count <- positive countArg
            step <- positive stepArg
            calls <- positive callsArg
            samples <- positive samplesArg
            forM_ [1 .. samples] $ \sample ->
                measure workload count step calls sample
        _ -> die "usage: journal-retention WORKLOAD SNAPSHOTS_PER_CALL CHUNK_BYTES CALLS SAMPLES +RTS -T"

positive :: String -> IO Int
positive value = case reads value of
    [(n, "")] | n > 0 -> pure n
    _ -> die "dimensions must be positive integers"

measure :: Workload -> Int -> Int -> Int -> Int -> IO ()
measure workload count step calls sample = do
    performGC
    before <- getRTSStats
    wall0 <- getMonotonicTimeNSec
    cpu0 <- getCPUTime
    bracket (build workload count step calls sample >>= newStablePtr)
        freeStablePtr $ \root -> do
            cpu1 <- getCPUTime
            wall1 <- getMonotonicTimeNSec
            -- Pin the raw, as-yet-unprojected journal across this collection.
            performGC
            admitted <- getRTSStats
            projectionWall0 <- getMonotonicTimeNSec
            projectionCpu0 <- getCPUTime
            journal <- deRefStablePtr root
            checksum <- evaluate $
                foldl' (\acc event -> acc + eventChecksum event) 0
                    (displayEventsFromJournal journal)
            projectionCpu1 <- getCPUTime
            projectionWall1 <- getMonotonicTimeNSec
            performGC
            projected <- getRTSStats
            printf "workload=%s snapshots=%d chunk_bytes=%d calls=%d sample=%d admission_wall_ms=%.3f admission_cpu_ms=%.3f admission_allocated_bytes=%d admission_live_bytes=%d projection_wall_ms=%.3f projection_cpu_ms=%.3f projection_allocated_bytes=%d projected_live_bytes=%d checksum=%d\n"
                (show workload) count step calls sample
                (fromIntegral (wall1 - wall0) / 1e6 :: Double)
                (fromIntegral (cpu1 - cpu0) / 1e9 :: Double)
                (allocated_bytes admitted - allocated_bytes before)
                (gcdetails_live_bytes (gc admitted))
                (fromIntegral (projectionWall1 - projectionWall0) / 1e6 :: Double)
                (fromIntegral (projectionCpu1 - projectionCpu0) / 1e9 :: Double)
                (allocated_bytes projected - allocated_bytes admitted)
                (gcdetails_live_bytes (gc projected))
                checksum

-- Payload construction is intentionally included in both variants. Each call
-- advances one independent cumulative ASCII snapshot per round; no input event
-- list survives. Projection occurs only AFTER the admission-live measurement.
{-# NOINLINE build #-}
build :: Workload -> Int -> Int -> Int -> Int -> IO DisplayJournal
build workload count step calls salt = go 1 emptyDisplayJournal
  where
    go !roundIndex !journal
        | roundIndex > count = pure journal
        | otherwise = do
            updated <- callLoop roundIndex 1 journal
            go (roundIndex + 1) updated
    callLoop !roundIndex !identifier !journal
        | identifier > calls = pure journal
        | otherwise = do
            let callId = Text.pack ("call-" <> show identifier)
                chunk = Text.singleton (toEnum (97 + (salt + identifier) `mod` 26))
                size = if workload == TextOnly then step else roundIndex * step
                payload = Text.replicate size chunk
                call = functionToolCall callId "apply_patch" payload
            -- Force every payload to model a decoded provider snapshot, even
            -- when the journal implementation stores event payloads lazily.
            _ <- evaluate (Text.length payload)
            updated <- evaluate $ case workload of
                Arguments -> recordDisplayEvent (ToolArgumentsUpdated call) journal
                Output -> recordDisplayEvent (ToolOutputUpdated callId payload) journal
                Mixed ->
                    recordDisplayEvent (ToolOutputUpdated callId payload) $
                        recordDisplayEvent (ToolArgumentsUpdated call) journal
                TextOnly -> recordDisplayEvent (TextDelta payload) journal
            callLoop roundIndex (identifier + 1) updated

eventChecksum :: LoopEvent -> Int
eventChecksum = \case
    TextDelta text -> checksumText text
    ToolArgumentsUpdated call -> checksumText call.arguments
    ToolUpdated call -> checksumText call.arguments
    ToolOutputUpdated _ output -> checksumText output
    _ -> 1

checksumText :: Text -> Int
checksumText = Text.foldl' (\acc character -> acc + ord character) 0
