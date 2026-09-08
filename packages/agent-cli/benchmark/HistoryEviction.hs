{-# LANGUAGE BangPatterns #-}

-- | End-to-end page merge, budget enforcement, index construction and output
-- consumption. Both implementations are compiled in this component with -O2.
module Main (main) where

import Agent.CLI.TUI.History
import Agent.TUI.Model (BlockId(..), BlockKind(..), BlockState(..), UiBlock(..))
import Control.Exception (evaluate)
import Control.Monad (forM, unless)
import Data.IORef (newIORef, readIORef)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (RTSStats(..), getRTSStats, getRTSStatsEnabled)
import HistoryEvictionBaseline (applyHistoryPageBaseline)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)

data Scenario = Turns | Blocks | Bytes | Append | Page | NoEviction
    deriving (Eq)

data Input = Input !HistoryPage !HistoryWindow

data Sample = Sample
    { elapsedMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Double
    }

type Apply = HistoryPage -> HistoryWindow -> Either HistoryPageRejection HistoryWindow

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    unless enabled (die "run with +RTS -T")
    getArgs >>= \case
        [mode, scenarioArg, countArg, sizeArg, samplesArg] -> do
            apply <- case mode of
                "old" -> pure applyHistoryPageBaseline
                "new" -> pure applyHistoryPage
                _ -> die "mode must be old or new"
            scenario <- case scenarioArg of
                "turns" -> pure Turns
                "blocks" -> pure Blocks
                "bytes" -> pure Bytes
                "append" -> pure Append
                "page" -> pure Page
                "no-eviction" -> pure NoEviction
                _ -> die "scenario must be turns, blocks, bytes, append, page or no-eviction"
            count <- positive countArg
            size <- positive sizeArg
            samples <- positive samplesArg
            -- Separate fixtures prevent this check from precomputing a timed result.
            let validation = makeInputs scenario count size 0
            unless (all equivalent validation) $
                die "baseline/production results differ"
            results <- forM [1 .. samples] (measure apply scenario count size)
            printf "%s,%s,%d,%d,%d,%.6f,%.6f,%.0f\n"
                mode scenarioArg count size samples
                (middle (map (.elapsedMillis) results))
                (middle (map (.cpuMillis) results))
                (middle (map (.allocatedBytes) results))
        _ -> die "usage: history-eviction-bench old|new SCENARIO TURNS BODY_BYTES SAMPLES"
  where
    equivalent (Input page window) =
        applyHistoryPageBaseline page window == applyHistoryPage page window
    middle values = sort values !! (length values `div` 2)
    positive raw = case reads raw of
        [(n, "")] | n > 0 -> pure n
        _ -> die ("expected positive integer: " <> raw)

measure :: Apply -> Scenario -> Int -> Int -> Int -> IO Sample
measure apply scenario count size sample = do
    let inputs = makeInputs scenario count size sample
    -- Allocate and force all input payloads and existing indexes before timing.
    _ <- evaluate (foldl' (\n input -> n + inputChecksum input) 0 inputs)
    ref <- newIORef inputs
    performGC
    beforeStats <- getRTSStats
    beforeCpu <- getCPUTime
    beforeWall <- getMonotonicTimeNSec
    freshInputs <- readIORef ref
    !checksum <- evaluate (runInputs apply freshInputs)
    afterWall <- getMonotonicTimeNSec
    afterCpu <- getCPUTime
    -- Collect after stopping the clocks so RTS allocated_bytes includes the
    -- unfinished nursery, without timing an artificial major collection.
    performGC
    afterStats <- getRTSStats
    _ <- evaluate checksum
    let operations = fromIntegral (length inputs)
    pure Sample
        { elapsedMillis = fromIntegral (afterWall - beforeWall) / 1e6 / operations
        , cpuMillis = fromIntegral (afterCpu - beforeCpu) / 1e9 / operations
        , allocatedBytes =
            fromIntegral (afterStats.allocated_bytes - beforeStats.allocated_bytes)
                / operations
        }

{-# NOINLINE runInputs #-}
runInputs :: Apply -> [Input] -> Int
runInputs apply = foldl' step 0
  where
    step !n (Input page window) = case apply page window of
        Left rejection -> error (show rejection)
        Right result -> n + windowChecksum result

-- Twenty independent operations per sample; alternating directions cover
-- both edges. Append models a latest turn arriving into a full window.
makeInputs :: Scenario -> Int -> Int -> Int -> [Input]
makeInputs scenario count size sample =
    [ makeInput operation | operation <- [1 .. 20] ]
  where
    makeInput operation =
        let older = scenario /= Append && even operation
            existingCount =
                if scenario == Append || scenario == Page then count else min 120 count
            incomingCount = case scenario of
                Append -> 1
                Page -> 30
                _ -> count
            total = existingCount + incomingCount
            -- Start within budget, including the small-input cases.
            keep = max existingCount (total `div` 4)
            turns = Seq.fromList
                [ makeTurn (sample * 20 + operation) size ident
                | ident <- [1 .. total]
                ]
            (incoming, existing)
                | older = Seq.splitAt incomingCount turns
                | otherwise =
                    let (old, new) = Seq.splitAt existingCount turns
                    in (new, old)
            maxTurns = case scenario of
                Turns -> keep
                Append -> count
                Page -> count
                _ -> total
            maxBlocks = if scenario == Blocks then keep * 4 else total * 4
            maxBytes =
                if scenario == Bytes then keep * 4 * (114 + 2 * size) else maxBound
            window =
                setHistoryWindowTurns existing $
                    emptyHistoryWindow (HistoryGeneration 1) maxTurns maxBlocks maxBytes
            page = HistoryPage
                { historyPageGeneration = HistoryGeneration 1
                , historyPageDirection = if older then HistoryOlder else HistoryNewer
                , historyPageTurns = incoming
                , historyPageGenerationStart = HistoryCursor 1
                , historyPageTotalTurns = fromIntegral total
                , historyPageHasOlder = False
                , historyPageHasNewer = False
                }
        in Input page window

makeTurn :: Int -> Int -> Int -> HistoryTurn
makeTurn salt size ident =
    HistoryTurn
        { historyTurnCursor = HistoryCursor (fromIntegral ident)
        , historyTurnCharts = Map.empty
        , historyTurnBlocks = Seq.fromList
            [ UiBlock
                { blockId = BlockId (ident * 4 + offset)
                , blockKind = BlockAssistant
                , blockTitle = "assistant"
                , blockBody =
                    Text.take size $
                        Text.pack (show salt <> ":" <> show ident <> ":" <> show offset <> ":")
                            <> Text.replicate size "x"
                , blockState = BlockComplete
                , blockExpanded = True
                , blockInspectionGroupable = False
                , blockCallId = Nothing
                , blockTimestamp = ""
                , blockDetail = ""
                }
            | offset <- [0 .. 3]
            ]
        }

inputChecksum :: Input -> Int
inputChecksum (Input page window) =
    foldl' (\n turn -> n + turnChecksum turn) 0 page.historyPageTurns
        + windowChecksum window

windowChecksum :: HistoryWindow -> Int
windowChecksum window =
    foldl' (\n turn -> n + turnChecksum turn) 0 window.historyWindowTurns
        + Map.foldl' (\n turn -> n + turnChecksum turn) 0 window.historyWindowTurnsByCursor
        + Map.foldl' (\n block -> n + blockChecksum block) 0 window.historyWindowBlocksById
        + Set.size window.historyWindowPending
        + fromEnum window.historyWindowHasOlder
        + fromEnum window.historyWindowHasNewer

turnChecksum :: HistoryTurn -> Int
turnChecksum turn =
    let HistoryCursor cursor = turn.historyTurnCursor
    in fromIntegral cursor + foldl' (\n block -> n + blockChecksum block) 0 turn.historyTurnBlocks

blockChecksum :: UiBlock -> Int
blockChecksum block =
    let BlockId ident = block.blockId
    in ident
        + Text.length block.blockBody
        + Text.length block.blockTitle
        + Text.length block.blockTimestamp
        + Text.length block.blockDetail
        + maybe 0 Text.length block.blockCallId
