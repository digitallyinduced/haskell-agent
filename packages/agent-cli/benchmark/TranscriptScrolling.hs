{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Benchmark production retained-history redraws. The old workload preserves
-- the pre-chunking renderer so changes remain directly comparable.
module Main (main) where

import Agent.CLI.AgentViewport (AgentEntry(..), AgentTarget(..))
import Agent.CLI.Interrupt (CtrlCDecision(..))
import Agent.CLI.TUI.App
    ( initialFullscreenAppState
    , drawBlock
    , drawConversationBlocks
    , drawTranscript
    , newFullscreenInputBuffer
    , newFullscreenRuntimeWithSyntaxLoader
    )
import Agent.CLI.TUI.History
    ( HistoryCursor(..)
    , HistoryGeneration(..)
    , HistoryTurn(..)
    , HistoryWindow(..)
    , emptyHistoryWindow
    , setHistoryWindowTurns
    )
import Agent.CLI.TUI.Transcript
    ( coalesceInspectionBlocks
    , transcriptChunkSize
    )
import Agent.CLI.TUI.Types (AppState(..), Name(..))
import Agent.TUI.Model
    ( BlockId(..)
    , BlockKind(..)
    , BlockState(..)
    , UiBlock(..)
    , UiState(..)
    , initialUiState
    )
import Agent.TUI.Motion (MotionMode(..))
import Brick
    ( Padding(..)
    , ViewportType(..)
    , Widget
    , attrMap
    , cached
    , hBox
    , padBottom
    , padLeftRight
    , renderFinal
    , txt
    , txtWrap
    , vBox
    , viewport
    )
import Brick.Types (RenderState)
import Control.DeepSeq (force)
import Control.Monad (replicateM)
import Data.Foldable (toList)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (RTSStats(..), getRTSStats)
import qualified Graphics.Vty as V
import Graphics.Vty.PictureToSpans (displayOpsForPic)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Mem (performGC)

data Workload
    = HistoryPerBlock
    | HistoryChunkCache
    | PerBlockCache
    | ChunkCache

data Sample = Sample
    { elapsedMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Integer
    }

main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        [workloadText, blockCountText, bodyLinesText, sampleCountText] -> do
            workload <- parseWorkload workloadText
            let blockCount = read blockCountText
                bodyLines = read bodyLinesText
                sampleCount = read sampleCountText
            rawState <- benchmarkState blockCount bodyLines
            let state = prepareHistory workload rawState 0
            let widgetForFrame = productionWidget workload state
            initialState <- warmCache (widgetForFrame 0)
            stateRef <- newIORef initialState
            samples <-
                replicateM sampleCount $
                    measure redrawsPerSample widgetForFrame stateRef
            printSample workloadText blockCount bodyLines sampleCount redrawsPerSample
                (median samples)
            coldSamples <- mapM
                (\frame -> measureAction $
                    warmCache (productionWidget workload
                        (prepareHistory workload rawState frame) frame))
                [1 .. sampleCount]
            printSample (workloadText <> "-setup-first-render")
                blockCount bodyLines sampleCount 1 (median coldSamples)
        _ ->
            error
                "usage: transcript-scrolling-bench \
                \(history-per-block|history-chunk-cache|\
                \per-block-cache|chunk-cache) \
                \BLOCKS BODY_LINES SAMPLES"

parseWorkload :: String -> IO Workload
parseWorkload = \case
    "history-per-block" -> pure HistoryPerBlock
    "history-chunk-cache" -> pure HistoryChunkCache
    "per-block-cache" -> pure PerBlockCache
    "chunk-cache" -> pure ChunkCache
    other -> error ("unknown workload: " <> other)

{-# NOINLINE productionWidget #-}
productionWidget :: Workload -> AppState -> Int -> Widget Name
productionWidget workload state frame =
    viewport ConversationViewport Vertical $ padLeftRight 2 $
        case workload of
            HistoryPerBlock -> oldHistoryWidget framedState
            HistoryChunkCache -> drawTranscript framedState
            PerBlockCache -> syntheticTranscriptWidget PerBlockCache framedState
            ChunkCache -> syntheticTranscriptWidget ChunkCache framedState
  where
    framedState = state
        { appUi = state.appUi{uiElapsedMillis = frame} }

-- Include the changed setter in cold measurements, not just warm redraws.
-- Input blocks/runtime are prepared outside timing; indexes/projection are not.
{-# NOINLINE prepareHistory #-}
prepareHistory :: Workload -> AppState -> Int -> AppState
prepareHistory workload state frame =
    state
        { appUi = state.appUi{uiElapsedMillis = frame}
        , appHistoryWindow = case workload of
            HistoryChunkCache -> setHistoryWindowTurns turns window
            HistoryPerBlock -> window
                { historyWindowTurnsByCursor = Map.fromList
                    [(turn.historyTurnCursor, turn) | turn <- toList turns]
                , historyWindowBlocksById = Map.fromList
                    [(block.blockId, block)
                    | turn <- toList turns
                    , block <- toList turn.historyTurnBlocks]
                }
            PerBlockCache -> window
            ChunkCache -> window
        }
  where
    window = state.appHistoryWindow
    turns = window.historyWindowTurns

-- The renderer replaced by the optimization: normalize and flatten the whole
-- history on every frame, then combine one cached image per block.
oldHistoryWidget :: AppState -> Widget Name
oldHistoryWidget state =
    vBox
        [ vBox $
            map
                (drawBlock state AgentRoot state.appUi)
                historicalBlocks
        , drawConversationBlocks state AgentRoot state.appUi
        ]
  where
    historicalBlocks =
        concatMap
            (toList . coalesceInspectionBlocks . (.historyTurnBlocks))
            (toList state.appHistoryWindow.historyWindowTurns)

-- Retain the original synthetic microbenchmark modes for longitudinal
-- comparisons. Production history measurements above remain the default
-- modes used to evaluate this change.
syntheticTranscriptWidget :: Workload -> AppState -> Widget Name
syntheticTranscriptWidget workload state =
    case workload of
        PerBlockCache -> vBox blocks
        ChunkCache ->
            vBox
                [ case (widgets, blockChunk) of
                    (_, firstBlock Seq.:<| _)
                        | length widgets == transcriptChunkSize ->
                            cached
                                (ConversationChunkCache
                                    AgentRoot
                                    firstBlock.blockId
                                    (Seq.index blockChunk
                                        (Seq.length blockChunk - 1)).blockId)
                                rendered
                    _ -> rendered
                | blockChunk <- chunks
                , let widgets = fmap syntheticBlock (toList blockChunk)
                      rendered = vBox widgets
                ]
        _ -> error "syntheticTranscriptWidget: production workload"
  where
    historyBlocks =
        foldMap (.historyTurnBlocks) state.appHistoryWindow.historyWindowTurns
    chunks = chunksOfSeq transcriptChunkSize historyBlocks
    blocks = fmap syntheticBlock (toList historyBlocks)
    syntheticBlock block =
        cached
            (ConversationBlockCache
                AgentRoot block.blockId False False Nothing) $
            padBottom (Pad 1) $
                hBox [txt "  ", txtWrap block.blockBody]

benchmarkState :: Int -> Int -> IO AppState
benchmarkState blockCount bodyLines = do
    input <- newFullscreenInputBuffer
    runtime <-
        newFullscreenRuntimeWithSyntaxLoader
            (pure (Left "disabled in transcript benchmark"))
            input
            (pure ())
            (const (pure ()))
            (pure WarnExit)
            (const (pure True))
            (const (pure ()))
            (const (pure ()))
            (pure (AgentRoot, [rootEntry]))
            (const (pure ()))
            (pure ())
            (const (pure ()))
            MotionOff
            False
            initialUiState
    let base =
            initialFullscreenAppState runtime [] AgentRoot [rootEntry] 0
        turns =
            Seq.fromList
                [ HistoryTurn
                    { historyTurnCursor = HistoryCursor (fromIntegral cursor)
                    , historyTurnBlocks = Seq.fromList turnBlocks
                    }
                | (cursor, turnBlocks) <-
                    zip [0 :: Int ..] (chunksOf 10 blocks)
                ]
        history =
            (emptyHistoryWindow
                    (HistoryGeneration 0)
                    (max 1 (Seq.length turns))
                    (max 1 blockCount)
                    maxBound)
                { historyWindowTurns = turns }
    let !_ = force (show turns)
    pure base{appHistoryWindow = history}
  where
    body =
        Text.intercalate "\n" $
            take bodyLines (cycle representativeLines)
    blocks =
        [ UiBlock
            { blockId = BlockId (-index)
            , blockKind =
                if index `mod` 5 == 0
                    then BlockTool
                    else BlockAssistant
            , blockTitle =
                if index `mod` 5 == 0
                    then "read_file"
                    else "Assistant"
            , blockBody = body
            , blockTimestamp = ""
            , blockDetail =
                if index `mod` 5 == 0
                    then "packages/agent-cli/src/Agent/CLI/TUI/Render.hs"
                    else ""
            , blockState = BlockComplete
            , blockExpanded = False
            , blockCallId = Nothing
            , blockInspectionGroupable = False
            }
        | index <- [1 .. blockCount]
        ]

rootEntry :: AgentEntry
rootEntry =
    AgentEntry
        { agentTarget = AgentRoot
        , agentPath = "/root"
        , agentStatus = "running"
        , agentModel = Nothing
        , agentSteps = []
        , agentTranscript = []
        , agentConversation = initialUiState
        }

representativeLines :: [Text]
representativeLines =
    [ "Representative **Markdown** text with a [link](https://example.com)."
    , "- A retained list item that wraps across the viewport."
    , "```haskell"
    , "rendered = cached key (markdownWidget body)"
    , "```"
    ]

redrawsPerSample :: Int
redrawsPerSample = 25

benchmarkRegion :: V.DisplayRegion
benchmarkRegion = (100, 32)

warmCache :: Widget Name -> IO (RenderState Name)
warmCache widget = do
    let (renderState, picture, _, _) =
            renderFinal
                (attrMap V.defAttr [])
                [widget]
                benchmarkRegion
                (const Nothing)
                emptyRenderState
    let !_ = force (show (displayOpsForPic picture benchmarkRegion))
    pure renderState

measure
    :: Int
    -> (Int -> Widget Name)
    -> IORef (RenderState Name)
    -> IO Sample
measure iterations widgetForFrame stateRef =
    measureAction (redraw iterations 0)
  where
    redraw remaining checksum
        | remaining <= 0 = pure $! checksum
        | otherwise = do
            renderState <- readIORef stateRef
            let widget = widgetForFrame (iterations - remaining)
                (nextState, picture, _, extents) =
                    renderFinal
                        (attrMap V.defAttr [])
                        [widget]
                        benchmarkRegion
                        (const Nothing)
                        renderState
                !rendered =
                    force
                        ( show (displayOpsForPic picture benchmarkRegion)
                        , length extents
                        )
            writeIORef stateRef $! nextState
            redraw
                (remaining - 1)
                (checksum + length (fst rendered) + snd rendered)

measureAction :: IO a -> IO Sample
measureAction action = do
    performGC
    beforeStats <- getRTSStats
    beforeCpu <- getCPUTime
    beforeTime <- getMonotonicTimeNSec
    result <- action
    result `seq` pure ()
    afterTime <- getMonotonicTimeNSec
    afterCpu <- getCPUTime
    -- Flush nursery allocation into RTS counters, outside the timed interval.
    performGC
    afterStats <- getRTSStats
    pure Sample
        { elapsedMillis = fromIntegral (afterTime - beforeTime) / 1.0e6
        , cpuMillis = fromIntegral (afterCpu - beforeCpu) / 1.0e9
        , allocatedBytes =
            fromIntegral
                (allocated_bytes afterStats - allocated_bytes beforeStats)
        }

median :: [Sample] -> Sample
median samples =
    sortOn (.elapsedMillis) samples !! (length samples `div` 2)

printSample :: String -> Int -> Int -> Int -> Int -> Sample -> IO ()
printSample workload blockCount bodyLines sampleCount redrawCount sample =
    putStrLn $
        unwords
            [ workload
            , "blocks=" <> show blockCount
            , "body_lines=" <> show bodyLines
            , "redraws_per_sample=" <> show redrawCount
            , "samples=" <> show sampleCount
            , "elapsed_ms=" <> show sample.elapsedMillis
            , "cpu_ms=" <> show sample.cpuMillis
            , "allocated_bytes=" <> show sample.allocatedBytes
            ]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf size values =
    let (prefix, suffix) = splitAt size values
    in prefix : chunksOf size suffix

chunksOfSeq :: Int -> Seq.Seq a -> [Seq.Seq a]
chunksOfSeq _ Seq.Empty = []
chunksOfSeq size values =
    let (prefix, suffix) = Seq.splitAt size values
    in prefix : chunksOfSeq size suffix

emptyRenderState :: RenderState Name
emptyRenderState =
    read
        "RS {viewportMap = fromList [], rsScrollRequests = [], \
        \observedNames = fromList [], renderCache = fromList [], \
        \clickableNames = [], requestedVisibleNames_ = fromList [], \
        \reportedExtents = fromList []}"

-- Brick exposes Read but not the constructor/all fields of RenderState.
-- Its empty representation contains no names, so this benchmark-only instance
-- deliberately rejects every name rather than inventing a production parser.
instance Read Name where
    readsPrec _ _ = []
