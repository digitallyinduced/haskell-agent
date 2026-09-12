module Main (main) where

import qualified Agent.Loop.DisplayJournal as New
import qualified JournalBefore as Old
import Agent.Loop.Output (LoopEvent(..))
import Agent.ToolDispatch (ToolCall(..), functionToolCall)
-- safe-exceptions does not export evaluate; no exception handling is needed here.
import Control.Exception (evaluate)
import Control.Monad (forM_, unless)
import Data.Char (ord)
import Data.IORef
import Data.List (inits)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import System.CPUTime
import System.Environment (getArgs)
import System.Mem (performGC)
import Text.Printf (printf)

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    unless enabled $ fail "use +RTS -T"
    args <- getArgs
    case args of
        [mode, shape, countArg, sizeArg, samplesArg] -> do
            let count = read countArg
                size = read sizeArg
                samples = read samplesArg
            unless (count > 0 && size > 0 && samples > 0) $ fail "positive sizes required"
            unless (shape `elem` ["arguments", "interleaved", "text"])
                $ fail "unknown shape"
            verify
            expected <- verifyWorkload shape count size
            -- Warm up each consumer; all measured inputs are freshly generated.
            forM_ ["old", "new"] \variant -> build variant shape count size >>= projectChecksum
            putStrLn "variant,shape,count,size,sample,cpu_ms,wall_ms,allocated_bytes,live_bytes"
            forM_ [1 .. samples :: Int] \sample ->
                forM_ (if odd sample then ["old", "new"] else ["new", "old"]) \variant ->
                    if mode == "all" || mode == variant then measure variant shape count size sample expected else pure ()
        _ -> fail "usage: bench old|new|all arguments|interleaved|text COUNT SIZE SAMPLES"

{-# NOINLINE verifyWorkload #-}
verifyWorkload :: String -> Int -> Int -> IO Int
verifyWorkload shape count size = do
    old <- build "old" shape count size >>= id
    new <- build "new" shape count size >>= id
    unless (old == new) $ fail "workload projection mismatch"
    projectChecksum (pure old)

-- Keep the IORef behind closures across the collection, without retaining an
-- input event list. Every snapshot payload is generated and forced on arrival.
{-# NOINLINE build #-}
build :: String -> String -> Int -> Int -> IO (IO [LoopEvent])
build mode shape count size = case mode of
    "old" -> go Old.emptyDisplayJournal Old.recordDisplayEvent Old.displayEventsFromJournal
    "new" -> go New.emptyDisplayJournal New.recordDisplayEvent New.displayEventsFromJournal
    _ -> fail "unknown mode"
  where
    go empty record project = do
        ref <- newIORef empty
        forM_ [1 .. count] \index -> do
            let callId = if shape == "interleaved" && even index then "b" else "a"
                payload = Text.replicate (if shape == "text" then size else min 65536 (index * size))
                    (Text.singleton (toEnum (97 + index `mod` 26)))
                event = if shape == "text" then TextDelta payload
                    else ToolArgumentsUpdated (functionToolCall callId "apply_patch" payload)
            _ <- evaluate (eventChecksum event)
            modifyIORef' ref (record event)
        pure (project <$> readIORef ref)

measure :: String -> String -> Int -> Int -> Int -> Int -> IO ()
measure mode shape count size sample expected = do
    performGC
    before <- getRTSStats
    cpu0 <- getCPUTime
    wall0 <- getMonotonicTimeNSec
    project <- build mode shape count size
    -- Include natural GC, but keep this diagnostic full collection outside clocks.
    wall1 <- getMonotonicTimeNSec
    cpu1 <- getCPUTime
    performGC
    after <- getRTSStats
    actual <- projectChecksum project
    unless (actual == expected) $ fail "measured workload checksum mismatch"
    printf "%s,%s,%d,%d,%d,%.6f,%.6f,%d,%d\n" mode shape count size sample
        (fromIntegral (cpu1 - cpu0) / 1e9 :: Double)
        (fromIntegral (wall1 - wall0) / 1e6 :: Double)
        (allocated_bytes after - allocated_bytes before)
        (gcdetails_live_bytes (gc after))

projectChecksum :: IO [LoopEvent] -> IO Int
projectChecksum project = do
    events <- project
    evaluate (sum (map eventChecksum events))

eventChecksum :: LoopEvent -> Int
eventChecksum = \case
    TextDelta text -> checksum text
    ToolUpdated call -> checksum call.arguments
    ToolArgumentsUpdated call -> checksum call.arguments
    _ -> 0
  where checksum = Text.foldl' (\n c -> n + ord c) 0

verify :: IO ()
verify = do
    let call = functionToolCall "a" "apply_patch" "x"
        events =
            [ ToolUpdated call, ToolArgumentsUpdated call, TextDelta "x"
            , ToolOutputUpdated "a" "1", ToolOutputUpdated "a" "2"
            , ResponseRestarted "retry", ToolUpdated call
            , ToolArgumentsUpdated call, ToolRetracted "a", TextDelta "y"
            ]
    forM_ (inits events) \prefix -> do
        let old = foldl (flip Old.recordDisplayEvent) Old.emptyDisplayJournal prefix
            new = foldl (flip New.recordDisplayEvent) New.emptyDisplayJournal prefix
        unless (Old.displayEventsFromJournal old == New.displayEventsFromJournal new)
            $ fail "prefix projection mismatch"
        unless (Old.displayEventsFromJournal (Old.discardCurrentDisplayAttempt old)
                == New.displayEventsFromJournal (New.discardCurrentDisplayAttempt new))
            $ fail "discard projection mismatch"
