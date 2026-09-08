{-# LANGUAGE BlockArguments, DuplicateRecordFields, LambdaCase, NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot, OverloadedStrings, RecordWildCards #-}
module Main (main) where

import Agent.CLI.AgentViewport (AgentTarget(..))
import Agent.CLI.ChartImage (chartResultImage)
import Agent.CLI.Interrupt (CtrlCDecision(..))
import Agent.CLI.TUI.App
import Agent.CLI.TUI.History
import Agent.CLI.TUI.ImagePreview
import Agent.CLI.TUI.Types
import Agent.Loop (ImageAttachment(..))
import Agent.Tools.RenderChart (renderChartResult)
import Agent.TUI.Model
import Agent.TUI.Motion (MotionMode(..))
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (atomically, readTVar, retry)
import Control.Exception (evaluate)
import Control.Monad (forM, unless)
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)

data Sample = Sample
    { elapsedMilliseconds :: !Double
    , cpuMilliseconds :: !Double
    , allocatedBytes :: !Integer
    }

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    unless enabled (die "Run with +RTS -T")
    getArgs >>= \case
        [workload, chartsArgument, pointsArgument, samplesArgument] -> do
            charts <- positive chartsArgument
            points <- positive pointsArgument
            samples <- positive samplesArgument
            unless (charts <= 64 && points <= 2000)
                (die "At most 64 charts and 2000 points per chart")
            results <- forM [1 .. samples] \sampleIndex -> do
                page <- makePage sampleIndex charts points
                runtime <- newBenchmarkRuntime
                let initial = initialFullscreenAppState runtime [] AgentRoot [] 0
                _ <- evaluate initial
                -- Complete all input construction before the measured interval.
                _ <- evaluate (sum
                    [ Text.length envelope
                    | turn <- toList page.historyPageTurns
                    , envelope <- Map.elems turn.historyTurnCharts
                    ])
                measure case workload of
                    "synchronous" -> do
                        let restored = synchronousRestoration (resetHistoryPage page initial)
                        unless (Map.size restored.appSubmittedImagePreviews == charts)
                            (die "Baseline did not retain every requested chart")
                        evaluate (previewChecksum restored)
                    "event" -> do
                        let reset = resetHistoryPage page initial
                        queueHistoryChartPreviews reset
                        requests <- atomically (readTVar runtime.runtimeHistoryChartRequests)
                        unless (length requests == charts)
                            (die "Event did not queue every requested chart")
                        evaluate (length requests + Map.size reset.appSubmittedImagePreviews)
                    "worker-total" -> do
                        let reset = resetHistoryPage page initial
                        queueHistoryChartPreviews reset
                        withAsync (runHistoryChartWorker runtime) \_ -> do
                            events <- atomically do
                                let AppEventMailbox mailbox = runtime.runtimeMailbox
                                pending <- readTVar mailbox
                                let prepared =
                                        [ (generation, blockId, preview)
                                        | PendingEvent (AppHistoryChartPrepared generation blockId preview)
                                            <- toList pending.mailboxPendingEvents
                                        ]
                                if length prepared == charts then pure prepared else retry
                            let restored = foldl'
                                    (\state (generation, blockId, preview) ->
                                        applyHistoryChartPreview generation blockId preview state)
                                    reset events
                            unless (Map.size restored.appSubmittedImagePreviews == charts)
                                (die "Worker did not retain every requested chart")
                            evaluate (previewChecksum restored)
                    _ -> die "Workloads: synchronous, event, worker-total"
            printf "%s,%d,%d,%d,%.3f,%.3f,%d\n" workload charts points samples
                (median (map (.elapsedMilliseconds) results))
                (median (map (.cpuMilliseconds) results))
                (median (map (.allocatedBytes) results))
        _ -> die "Usage: history-chart-restoration WORKLOAD CHARTS POINTS SAMPLES +RTS -T"
  where
    positive text = case reads text of
        [(number, "")] | number > 0 -> pure number
        _ -> die ("Invalid positive integer: " <> text)

-- Frozen synchronous chart-restoration algorithm from 98ae13c. Both variants
-- use the same current reset/remapping, rasterizer, PNG encoder and cache limits.
-- The tested pages contain only charts and fit the normal history/cache budgets.
synchronousRestoration :: AppState -> AppState
synchronousRestoration state =
    state { appSubmittedImagePreviews =
        retainSubmittedImagePreviewsForBlocks blockIds restored }
  where
    blockIds = [block.blockId
        | turn <- toList state.appHistoryWindow.historyWindowTurns
        , block <- toList turn.historyTurnBlocks]
    candidates = reverse
        [ (block.blockId, envelope)
        | turn <- toList state.appHistoryWindow.historyWindowTurns
        , block <- toList turn.historyTurnBlocks
        , Just envelope <- [Map.lookup block.blockId turn.historyTurnCharts]
        ]
    restored = foldl' restore state.appSubmittedImagePreviews
        (take submittedImagePreviewCountBudget candidates)
    restore previews (blockId, envelope)
        | Map.member blockId previews = previews
        | otherwise = case chartResultImage envelope >>= prepareNativeTuiImagePreview of
            Left _ -> previews
            Right preview -> Map.insert blockId [preview] previews

previewChecksum :: AppState -> Int
previewChecksum state = sum
    [ BS.foldl' (\checksum byte -> checksum * 33 + fromIntegral byte) 5381
        preview.previewKittyAttachment.imageBytes
    | previews <- Map.elems state.appSubmittedImagePreviews
    , preview <- previews
    ]

measure :: IO Int -> IO Sample
measure action = do
    performGC
    beforeStats <- getRTSStats
    beforeCpu <- getCPUTime
    beforeElapsed <- getMonotonicTimeNSec
    checksum <- action
    _ <- evaluate checksum
    afterElapsed <- getMonotonicTimeNSec
    afterCpu <- getCPUTime
    -- Collect afterwards for accurate sub-allocation-area byte accounting;
    -- this collection is excluded from the reported execution interval.
    performGC
    afterStats <- getRTSStats
    pure Sample
        { elapsedMilliseconds = fromIntegral (afterElapsed - beforeElapsed) / 1e6
        , cpuMilliseconds = fromIntegral (afterCpu - beforeCpu) / 1e9
        , allocatedBytes = fromIntegral (afterStats.allocated_bytes - beforeStats.allocated_bytes)
        }

median :: Ord a => [a] -> a
median values = sort values !! (length values `div` 2)

makePage :: Int -> Int -> Int -> IO HistoryPage
makePage sampleIndex charts points = do
    turns <- forM [0 .. charts - 1] \chartIndex -> do
        let title = "Chart " <> Text.pack (show sampleIndex <> "-" <> show chartIndex)
            input = "{\"version\":1,\"kind\":\"line\",\"title\":\"" <> title
                <> "\",\"x_axis\":{\"type\":\"number\"},\"y_axis\":{},\"series\":[{\"name\":\"Requests\",\"points\":["
                <> Text.intercalate ","
                    [ "{\"x\":" <> Text.pack (show index) <> ",\"y\":"
                        <> Text.pack (show ((index * 13 + chartIndex + sampleIndex) `mod` 101)) <> "}"
                    | index <- [1 .. points] ]
                <> "]}]}"
        envelope <- either (fail . Text.unpack) pure (renderChartResult input)
        let block = UiBlock
                { blockId = BlockId 1, blockKind = BlockTool, blockTitle = "Chart"
                , blockBody = title, blockTimestamp = "", blockDetail = ""
                , blockState = BlockComplete, blockExpanded = False
                , blockCallId = Nothing, blockInspectionGroupable = False
                }
        pure (HistoryTurn (HistoryCursor (fromIntegral chartIndex)) (Seq.singleton block)
            (Map.singleton (BlockId 1) envelope))
    pure (HistoryPage (HistoryGeneration 0) HistoryNewer (Seq.fromList turns)
        (HistoryCursor 0) (fromIntegral charts) False False)

newBenchmarkRuntime :: IO FullscreenRuntime
newBenchmarkRuntime = do
    input <- newFullscreenInputBuffer
    newFullscreenRuntime input (pure ()) (const (pure ())) (pure WarnExit)
        (const (pure True)) (const (pure ())) (const (pure ()))
        (pure (AgentRoot, [])) (const (pure ())) (pure ())
        (const (pure ())) MotionFull True initialUiState
