{-# LANGUAGE BangPatterns #-}

-- | Offline long-conversation scroll benchmark. Only cursor-derived pages are
-- materialized in history traces. Live traces retain the current conversation,
-- as a long active turn can. PostgreSQL query
-- latency and terminal I/O are deliberately outside this renderer benchmark.
--
-- The baseline/production modes isolate the root conversation viewport, with
-- the same production transcript widgets, scrollbar and message footer.
-- Application mode exercises complete drawApp and can also compare separately
-- built baseline/candidate binaries. Verification emits exact attributed
-- characters, visible extents and viewport geometry for differential checks.
module Main (main) where

import Agent.CLI.AgentViewport (AgentEntry(..), AgentTarget(..))
import Agent.CLI.Interrupt (CtrlCDecision(..))
import Agent.CLI.TUI.App
    ( applyLoadedHistoryPage
    , conversationScrollbarRenderer
    , drawApp
    , drawConversationBlocks
    , drawTranscriptChunks
    , drawTranscriptChunksWithPadding
    , fullscreenApp
    , initialFullscreenAppState
    , newFullscreenInputBuffer
    , newFullscreenRuntimeWithSyntaxLoader
    , resetHistoryPage
    )
import Agent.CLI.TUI.History
    ( HistoryCursor(..)
    , HistoryDirection(..)
    , HistoryGeneration(..)
    , HistoryPage(..)
    , HistoryTurn(..)
    , HistoryWindow(..)
    , historyWindowCursors
    , historyWindowLoadedBlocks
    , historyWindowLoadedBytes
    , historyWindowLoadedTurns
    )
import Agent.CLI.TUI.MeasuredViewport (measuredViewportWithFooter)
import Agent.CLI.TUI.Transcript (transcriptChunks)
import qualified Agent.CLI.TUI.Scroll as Scroll
import Agent.CLI.TUI.Types (AppState(..), Name(..))
import Agent.TUI.Model
    ( BlockId(..)
    , BlockKind(..)
    , BlockState(..)
    , Focus(..)
    , UiBlock(..)
    , UiState(..)
    , initialUiState
    )
import Agent.TUI.Motion (MotionMode(..))
import qualified Agent.TUI.Theme as Theme
import Brick
    ( App(..)
    , BrickEvent(..)
    , EventM
    , Extent(..)
    , Location(..)
    , Result(..)
    , VScrollBarOrientation(..)
    , Viewport(..)
    , Widget(..)
    , clickable
    , continueWithoutRedraw
    , customMain
    , defaultMain
    , get
    , halt
    , invalidateCache
    , lookupViewport
    , makeVisible
    , neverShowCursor
    , padLeftRight
    , reportExtent
    , txt
    , vScrollBy
    , vScrollToBeginning
    , vScrollToEnd
    , viewportScroll
    , withAttr
    , withVScrollBarRenderer
    , withVScrollBars
    )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Main as Brick
import Brick.Widgets.Center (hCenter)
import Control.Concurrent.STM (atomically, newTChanIO, retry)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Exception.Safe (bracket)
import Control.Monad (forM, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (modify')
import Data.Char (ord)
import Data.Foldable (toList)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Data.Word (Word64)
import Debug.Trace (traceShow)
import Foreign.StablePtr (freeStablePtr, newStablePtr)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
    ( GCDetails(..)
    , RTSStats(..)
    , getRTSStats
    , getRTSStatsEnabled
    )
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Output.Mock as VMock
import Graphics.Vty.PictureToSpans (displayOpsForPic)
import Graphics.Vty.Span (SpanOp(..))
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)
import System.Mem (performGC)
import Text.Printf (printf)

data Renderer = Baseline | Production | Application | LiveBaseline deriving (Eq, Show)
data Trace = Local | Paging | Resize | Interaction | Live | LiveResize | LiveInteraction deriving (Eq, Show)
data ScriptEvent = Begin | Step !Int | Complete

data Configuration = Configuration
    { renderer :: !Renderer
    , trace :: !Trace
    , totalTurns :: !Int
    , bodyLines :: !Int
    , steps :: !Int
    , sampleCount :: !Int
    , verify :: !Bool
    }

data TimingStart = TimingStart !Word64 !Integer !Word64
data Measurement = Measurement
    { wallMillis :: !Double
    , cpuMillis :: !Double
    , allocation :: !Word64
    , retainedBytes :: !Word64
    }

data Counters = Counters
    { pageLoads :: !Int
    , evictedTurns :: !Int
    , maximumLoadedTurns :: !Int
    , maximumLoadedBlocks :: !Int
    , distinctViewportTops :: !(Set.Set Int)
    }

data SampleResult = SampleResult !Measurement !Measurement !Counters

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    arguments <- getArgs
    case arguments of
        ["smoke", turns, body] -> do
            configuration <- parseConfiguration ["application", "local", turns, body, "1", "1"]
            smoke configuration
        ["smoke-live", turns, body] -> do
            configuration <- parseConfiguration ["application", "live", turns, body, "1", "1"]
            smoke configuration
        _ -> benchmark arguments

smoke :: Configuration -> IO ()
smoke configuration = do
    initial <- prepare configuration
    _ <- defaultMain fullscreenApp
        { appStartEvent = vScrollToEnd (viewportScroll ConversationViewport)
        , appHandleEvent = \case
            VtyEvent (V.EvKey (V.KChar 'q') []) -> halt
            event -> appHandleEvent fullscreenApp event
        } initial
    pure ()

benchmark :: [String] -> IO ()
benchmark arguments = do
    enabled <- getRTSStatsEnabled
    unless enabled (die "enable RTS statistics with +RTS -T")
    configuration <- parseConfiguration arguments
    results <- forM [1 .. configuration.sampleCount] $
        const (runSample configuration)
    unless configuration.verify do
        printf "renderer=%s trace=%s turns=%d body_lines=%d steps=%d samples=%d\n"
            (show configuration.renderer) (show configuration.trace)
            configuration.totalTurns configuration.bodyLines
            configuration.steps configuration.sampleCount
        printMeasurements "setup_first_frame" [setup | SampleResult setup _ _ <- results]
        printMeasurements "scroll_trace" [scroll | SampleResult _ scroll _ <- results]
        case results of
            SampleResult _ _ counters : _ ->
                printf "pages=%d evicted_turns=%d max_loaded_turns=%d max_loaded_blocks=%d distinct_scroll_positions=%d\n"
                    counters.pageLoads counters.evictedTurns
                    counters.maximumLoadedTurns counters.maximumLoadedBlocks
                    (Set.size counters.distinctViewportTops)
            [] -> pure ()

parseConfiguration :: [String] -> IO Configuration
parseConfiguration arguments = do
    let (verification, remaining) = case arguments of
            "verify" : rest -> (True, rest)
            _ -> (False, arguments)
    case remaining of
        [rendererText, traceText, turnsText, linesText, stepsText, samplesText] -> do
            selectedRenderer <- case rendererText of
                "baseline" -> pure Baseline
                "production" -> pure Production
                "application" -> pure Application
                "live-baseline" -> pure LiveBaseline
                _ -> die "renderer must be baseline, production, application or live-baseline"
            selectedTrace <- case traceText of
                "local" -> pure Local
                "paging" -> pure Paging
                "resize" -> pure Resize
                "interaction" -> pure Interaction
                "live" -> pure Live
                "live-resize" -> pure LiveResize
                "live-interaction" -> pure LiveInteraction
                _ -> die "trace must be local, paging, resize, interaction, live, live-resize or live-interaction"
            when (selectedRenderer == LiveBaseline && not (isLiveTrace selectedTrace)) $
                die "live-baseline requires a live trace"
            Configuration selectedRenderer selectedTrace
                <$> positive turnsText <*> positive linesText
                <*> positive stepsText <*> positive samplesText
                <*> pure verification
        _ -> die $
            "usage: conversation-scrolling-bench [verify] "
                <> "(baseline|production|application|live-baseline) "
                <> "(local|paging|resize|interaction|live|live-resize|live-interaction) "
                <> "TOTAL_TURNS BODY_LINES STEPS SAMPLES"
  where
    positive raw = case reads raw of
        [(value, "")] | value > 0 -> pure value
        _ -> die ("expected a positive integer: " <> raw)

runSample :: Configuration -> IO SampleResult
runSample configuration = do
    events <- newBChan (configuration.steps + 2)
    mapM_ (writeBChan events) $
        Begin : map Step [0 .. configuration.steps - 1] <> [Complete]
    boundsRef <- newIORef (100, 32)
    (_, mockOutput) <- VMock.mockTerminal (100, 32)
    inputEvents <- newTChanIO
    frameRef <- newIORef (0 :: Int)
    countersRef <- newIORef (Counters 0 0 0 0 Set.empty)
    startRef <- newIORef Nothing
    resultRef <- newIORef Nothing
    let output = mockOutput
            { V.displayBounds = readIORef boundsRef
            , V.setDisplayBounds = writeIORef boundsRef
            }
        vty = V.Vty
            { V.update = \picture -> do
                bounds <- readIORef boundsRef
                let !score = force (pictureScore bounds picture)
                score `seq` pure ()
                frame <- readIORef frameRef
                when configuration.verify do
                    print ("picture" :: Text, frame, bounds, attributedRows bounds picture)
                writeIORef frameRef (frame + 1)
            , V.nextEvent = atomically retry
            , V.nextEventNonblocking = pure Nothing
            , V.inputIface = V.Input
                { V.eventChannel = inputEvents
                , V.shutdownInput = pure ()
                , V.restoreInputState = pure ()
                , V.inputLogMsg = const (pure ())
                }
            , V.outputIface = output
            , V.refresh = pure ()
            , V.shutdown = pure ()
            , V.isShutdown = pure False
            }
    performGC
    setupStart <- timingStart
    initial <- prepare configuration
    recordWindow countersRef initial.appHistoryWindow
    let application = App
            { appDraw = map (forceWidget configuration.verify) . drawWidgets configuration.renderer
            , appChooseCursor = neverShowCursor
            , appHandleEvent = handleScript configuration boundsRef countersRef
                setupStart startRef resultRef
            , appStartEvent =
                vScrollToEnd (viewportScroll ConversationViewport)
            , appAttrMap = Brick.appAttrMap fullscreenApp
            }
    _ <- customMain vty (pure vty) (Just events) application initial
    readIORef resultRef >>= \case
        Just result -> pure result
        Nothing -> die "scroll benchmark did not finish"

handleScript
    :: Configuration
    -> IORef V.DisplayRegion
    -> IORef Counters
    -> TimingStart
    -> IORef (Maybe (TimingStart, Measurement))
    -> IORef (Maybe SampleResult)
    -> BrickEvent Name ScriptEvent
    -> EventM Name AppState ()
handleScript configuration boundsRef countersRef setupStart startRef resultRef = \case
    AppEvent Begin -> do
        state <- get
        renderState <- Brick.getRenderState
        setup <- liftIO $ finishMeasurement setupStart (state, renderState)
        start <- liftIO timingStart
        liftIO (writeIORef startRef (Just (start, setup)))
        continueWithoutRedraw
    AppEvent (Step frame) -> do
        observeFrame configuration countersRef frame
        case configuration.trace of
            interaction | interaction `elem` [Interaction, LiveInteraction] -> do
                state <- get
                let chunks = if isLiveTrace configuration.trace
                        then transcriptChunks state.appUi.uiBlocks
                        else state.appHistoryWindow.historyWindowTranscriptChunks
                    chunkIndex = (frame `div` 6) `mod` max 1 (length chunks)
                    ident = case drop chunkIndex chunks of
                        chunk : _ -> case
                            [block.blockId | block <- toList chunk, block.blockKind == BlockAssistant] of
                                first : _ -> Just first
                                [] -> Nothing
                        [] -> Nothing
                    phase = frame `mod` 6
                    control = (\block -> CodeCopy AgentRoot block 1) <$> ident
                modify' \current -> current
                    { appHistorySelectedBlock = if phase == 1 && not (isLiveTrace configuration.trace)
                        then ident else Nothing
                    , appUi = current.appUi
                        { uiSelectedBlock = if phase == 1 && isLiveTrace configuration.trace
                            then ident else Nothing }
                    , appHoveredControl = if phase `elem` [3, 4] then control else Nothing
                    , appPressedControl = if phase == 4 then control else Nothing
                    }
                case ident of
                    Just block -> makeVisible (ConversationBlock AgentRoot block)
                    Nothing -> scrollLocally frame
            Paging | frame `mod` 8 == 0 -> loadNextPage configuration countersRef frame
            resize | resize `elem` [Resize, LiveResize] && frame `mod` 12 == 0 -> do
                let bounds = [(100,32),(76,24),(120,40),(92,28)]
                        !! ((frame `div` 12) `mod` 4)
                liftIO (writeIORef boundsRef bounds)
                invalidateCache
                scrollLocally frame
            _ -> scrollLocally frame
    AppEvent Complete -> do
        observeFrame configuration countersRef configuration.steps
        state <- get
        renderState <- Brick.getRenderState
        liftIO (validateWindow state.appHistoryWindow)
        starting <- liftIO (readIORef startRef)
        case starting of
            Nothing -> liftIO (die "scroll benchmark has no timing start")
            Just (start, setup) -> do
                scroll <- liftIO $ finishMeasurement start (state, renderState)
                counters <- liftIO (readIORef countersRef)
                when (configuration.steps >= 4
                    && Set.size counters.distinctViewportTops < 2) $
                    liftIO (die "scroll trace did not move the viewport")
                when (configuration.trace == Paging && configuration.steps >= 17
                    && configuration.totalTurns > 200 && counters.evictedTurns == 0) $
                    liftIO (die "paging trace did not evict old turns")
                liftIO (writeIORef resultRef (Just (SampleResult setup scroll counters)))
        halt
    _ -> continueWithoutRedraw

scrollLocally :: Int -> EventM Name AppState ()
scrollLocally frame =
    vScrollBy (viewportScroll ConversationViewport) $
        if even (frame `div` 25) then -3 else 3

observeFrame :: Configuration -> IORef Counters -> Int -> EventM Name AppState ()
observeFrame configuration countersRef frame = do
    current <- lookupViewport ConversationViewport
    let top = case current of
            Just (VP _ position _ _) -> position
            Nothing -> 0
    liftIO $ modifyIORef' countersRef \counters ->
        counters
            { distinctViewportTops =
                Set.insert top counters.distinctViewportTops }
    when configuration.verify do
        state <- get
        visibleExtents <- mapM Brick.lookupExtent $
            [ConversationViewportExtent, ConversationViewport, ConversationLatest]
                <> concat
                    [ [ConversationBlock AgentRoot ident, ConversationMessage AgentRoot ident]
                    | ident <- Map.keys state.appHistoryWindow.historyWindowBlocksById
                        <> map (.blockId) (toList state.appUi.uiBlocks)
                    ]
        liftIO $ print
            ( "geometry" :: Text, frame, current
            , toList (historyWindowCursors state.appHistoryWindow)
            , [extent | Just extent <- visibleExtents]
            )

-- Materialize just one production-sized page. Directional sweeps reverse
-- every six requests and at corpus boundaries, revisiting evicted history.
loadNextPage :: Configuration -> IORef Counters -> Int -> EventM Name AppState ()
loadNextPage configuration countersRef frame = do
    previous <- get
    let requested = if even (frame `div` 48) then HistoryOlder else HistoryNewer
        window = previous.appHistoryWindow
        direction
            | pageAvailable requested window = requested
            | otherwise = reverseDirection requested
    case pageForWindow configuration direction window of
        Nothing -> scrollLocally frame
        Just page -> do
            let anchor = edgeBlock direction window
                next = applyLoadedHistoryPage page previous
                previousCursors = Set.fromList (toList (historyWindowCursors window))
                nextCursors = Set.fromList $
                    toList (historyWindowCursors next.appHistoryWindow)
                removed = Set.size (Set.difference previousCursors nextCursors)
            liftIO (validateWindow next.appHistoryWindow)
            -- Match handleHistoryLoadedEvent: a page invalidates the complete
            -- image cache and preserves visibility of the old edge block.
            modify' (const next)
            invalidateCache
            case anchor of
                Nothing -> case direction of
                    HistoryOlder -> vScrollToBeginning (viewportScroll ConversationViewport)
                    HistoryNewer -> vScrollToEnd (viewportScroll ConversationViewport)
                Just ident -> makeVisible (ConversationBlock AgentRoot ident)
            liftIO do
                recordWindow countersRef next.appHistoryWindow
                modifyIORef' countersRef \counters ->
                    counters
                        { pageLoads = counters.pageLoads + 1
                        , evictedTurns = counters.evictedTurns + removed
                        }

pageAvailable :: HistoryDirection -> HistoryWindow -> Bool
pageAvailable HistoryOlder = (.historyWindowHasOlder)
pageAvailable HistoryNewer = (.historyWindowHasNewer)

reverseDirection :: HistoryDirection -> HistoryDirection
reverseDirection HistoryOlder = HistoryNewer
reverseDirection HistoryNewer = HistoryOlder

edgeBlock :: HistoryDirection -> HistoryWindow -> Maybe BlockId
edgeBlock direction window = do
    turn <- case direction of
        HistoryOlder -> window.historyWindowTurns Seq.!? 0
        HistoryNewer -> window.historyWindowTurns
            Seq.!? (Seq.length window.historyWindowTurns - 1)
    block <- case direction of
        HistoryOlder -> turn.historyTurnBlocks Seq.!? 0
        HistoryNewer -> turn.historyTurnBlocks
            Seq.!? (Seq.length turn.historyTurnBlocks - 1)
    pure block.blockId

pageForWindow :: Configuration -> HistoryDirection -> HistoryWindow -> Maybe HistoryPage
pageForWindow configuration direction window
    | not (pageAvailable direction window) = Nothing
    | otherwise = do
        turn <- case direction of
            HistoryOlder -> window.historyWindowTurns Seq.!? 0
            HistoryNewer -> window.historyWindowTurns
                Seq.!? (Seq.length window.historyWindowTurns - 1)
        let HistoryCursor cursor = turn.historyTurnCursor
            (start, end) = case direction of
                HistoryOlder ->
                    (max 0 (fromIntegral cursor - pageSize), fromIntegral cursor)
                HistoryNewer ->
                    (fromIntegral cursor + 1,
                        min configuration.totalTurns (fromIntegral cursor + 1 + pageSize))
        pure (fixturePage configuration direction start end)

-- Runtime.HistorySource.sessionUiPageSize is internal; keep this value
-- synchronized with the production initial and subsequent page size.
pageSize :: Int
pageSize = 80

isLiveTrace :: Trace -> Bool
isLiveTrace selected = selected `elem` [Live, LiveResize, LiveInteraction]

prepare :: Configuration -> IO AppState
prepare configuration = do
    input <- newFullscreenInputBuffer
    runtime <- newFullscreenRuntimeWithSyntaxLoader
        (pure (Left "disabled in conversation scroll benchmark")) input
        (pure ()) (const (pure ())) (pure WarnExit) (const (pure True))
        (const (pure ())) (const (pure ())) (pure (AgentRoot, [rootEntry]))
        (const (pure ())) (pure ()) (const (pure ())) MotionOff False initialUiState
    let base = initialFullscreenAppState runtime [] AgentRoot [rootEntry] 0
        initial = resetHistoryPage
            (fixturePage configuration HistoryNewer
                (max 0 (configuration.totalTurns - pageSize)) configuration.totalTurns)
            base
        -- Local/resize traces measure a saturated production window rather
        -- than an artificially unbounded transcript. Paging starts with the
        -- normal 80-turn page and includes growth/eviction in the interval.
        populated = if configuration.trace == Paging
            then initial
            else populate 2 initial
        conversation = if isLiveTrace configuration.trace
            then base
                { appUi = base.appUi
                    { uiBlocks = foldMap
                        ((.historyTurnBlocks) . fixtureTurn configuration)
                        [0 .. configuration.totalTurns - 1] }
                }
            else populated
        ui = conversation.appUi
        state = conversation
            { appUi = ui { uiFollow = False, uiFocus = FocusScrollback } }
    validateWindow state.appHistoryWindow
    _ <- evaluate $ foldl'
        (\total turn -> total + foldl'
            (\size block -> size + Text.length block.blockBody) 0 turn.historyTurnBlocks)
        0 state.appHistoryWindow.historyWindowTurns
    _ <- evaluate $ foldl'
        (\size block -> size + Text.length block.blockBody) 0 state.appUi.uiBlocks
    pure state
  where
    populate :: Int -> AppState -> AppState
    populate 0 state = state
    populate remaining state = case pageForWindow configuration HistoryOlder state.appHistoryWindow of
        Nothing -> state
        Just page -> populate (remaining - 1) (applyLoadedHistoryPage page state)

rootEntry :: AgentEntry
rootEntry = AgentEntry
    { agentTarget = AgentRoot
    , agentPath = "/root"
    , agentStatus = "idle"
    , agentModel = Nothing
    , agentSteps = []
    , agentTranscript = []
    , agentConversation = initialUiState
    }

fixturePage :: Configuration -> HistoryDirection -> Int -> Int -> HistoryPage
fixturePage configuration direction start end = HistoryPage
    { historyPageGeneration = HistoryGeneration 0
    , historyPageDirection = direction
    , historyPageTurns = Seq.fromList
        [fixtureTurn configuration cursor | cursor <- [start .. end - 1]]
    , historyPageGenerationStart = HistoryCursor 0
    , historyPageTotalTurns = fromIntegral configuration.totalTurns
    , historyPageHasOlder = start > 0
    , historyPageHasNewer = end < configuration.totalTurns
    }

fixtureTurn :: Configuration -> Int -> HistoryTurn
fixtureTurn configuration cursor = HistoryTurn
    { historyTurnCursor = HistoryCursor (fromIntegral cursor)
    , historyTurnCharts = Map.empty
    , historyTurnBlocks = Seq.fromList
        [ fixtureBlock offset kind title
        | (offset, kind, title) <-
            [ (0, BlockUser, "You")
            , (1, BlockAssistant, "Assistant")
            , (2, BlockInspect, "Read")
            , (3, BlockTool, "shell_command")
            , (4, BlockAssistant, "Assistant")
            , (5, BlockAssistant, "Assistant")
            ]
        ]
    }
  where
    body = Text.intercalate "\n" $
        Text.pack ("Turn " <> show cursor <> ": **retained conversation**")
            : take (configuration.bodyLines - 1) (cycle representativeLines)
    fixtureBlock offset kind title = UiBlock
        { blockId = BlockId (cursor * 6 + offset + 1)
        , blockKind = kind
        , blockTitle = title
        , blockBody = body
        , blockTimestamp = ""
        , blockDetail = if kind == BlockInspect then "src/Agent/Transcript.hs" else ""
        , blockState = BlockComplete
        , blockExpanded = False
        , blockCallId = Nothing
        , blockInspectionGroupable = False
        }

representativeLines :: [Text]
representativeLines =
    [ "- A representative Markdown list item with enough text to wrap at narrower widths."
    , "A [documentation link](https://example.com/reference) and `inline code`."
    , "```haskell"
    , "renderTranscript state = measuredViewport state"
    , "```"
    ]

validateWindow :: HistoryWindow -> IO ()
validateWindow window = do
    let cursors = toList (historyWindowCursors window)
        loaded = historyWindowLoadedTurns window
        retainedBlockIds = Set.fromList
            [ block.blockId
            | turn <- toList window.historyWindowTurns
            , block <- toList turn.historyTurnBlocks
            ]
    unless (loaded <= window.historyWindowMaxTurns) $
        die "history turn budget exceeded"
    unless (loaded <= 1 || historyWindowLoadedBlocks window <= window.historyWindowMaxBlocks) $
        die "history block budget exceeded"
    unless (loaded <= 1 || historyWindowLoadedBytes window <= window.historyWindowMaxBytes) $
        die "history byte budget exceeded"
    unless (Set.size (Set.fromList cursors) == length cursors
        && and (zipWith (<) cursors (drop 1 cursors))) $
        die "history contains duplicate or unordered turns"
    unless (Map.keysSet window.historyWindowTurnsByCursor == Set.fromList cursors
        && Map.keysSet window.historyWindowBlocksById == retainedBlockIds) $
        die "history lookup maps retained evicted turns or blocks"
    case cursors of
        [] -> pure ()
        HistoryCursor first : rest -> do
            let expected = map HistoryCursor [first .. first + fromIntegral (length rest)]
            unless (cursors == expected) (die "history contains a cursor gap")

recordWindow :: IORef Counters -> HistoryWindow -> IO ()
recordWindow ref window =
    modifyIORef' ref \counters -> counters
        { maximumLoadedTurns = max counters.maximumLoadedTurns
            (historyWindowLoadedTurns window)
        , maximumLoadedBlocks = max counters.maximumLoadedBlocks
            (historyWindowLoadedBlocks window)
        }

drawWidgets :: Renderer -> AppState -> [Widget Name]
drawWidgets Application = drawApp
drawWidgets selected = \state ->
    [ reportExtent ConversationViewportExtent $
        withVScrollBarRenderer conversationScrollbarRenderer $
            withVScrollBars OnRight $
                measuredViewportWithFooter ConversationViewport 1 (Just (drawFooter state)) $
                    case selected of
                        Baseline -> map (padLeftRight 2) (drawTranscriptChunks state)
                        Production -> drawTranscriptChunksWithPadding 2 state
                        -- Preserve the former live layout: all cached chunks
                        -- are composed into one image before viewport cropping.
                        LiveBaseline ->
                            [padLeftRight 2 (drawConversationBlocks state AgentRoot state.appUi)]
    ]
-- Same scrolled-away footer policy as Render.Workspace.conversationViewport.
drawFooter :: AppState -> Int -> [Extent Name] -> Widget Name
drawFooter state bottom measuredExtents =
    case Scroll.conversationMessagesBelowLabel hasNewer messageCount of
        Nothing
            | otherContentBelow -> indicator "↓ Latest output · Click to resume"
            | otherwise -> txt " "
        Just label -> indicator label
  where
    messageCount = Scroll.conversationMessagesBelow bottom
        [ (top, height)
        | extent <- measuredExtents
        , ConversationMessage target _ <- [extent.extentName]
        , target == AgentRoot
        , let Location (_, top) = extent.extentUpperLeft
        , let (_, height) = extent.extentSize
        ]
    hasNewer = state.appHistoryWindow.historyWindowHasNewer && or
        [ height > 0 && top + height > bottom
        | extent <- measuredExtents
        , extent.extentName == ConversationNewerGap
        , let Location (_, top) = extent.extentUpperLeft
        , let (_, height) = extent.extentSize
        ]
    otherContentBelow = or
        [ height > 0 && top + height > bottom
        | extent <- measuredExtents
        , ConversationBlock target _ <- [extent.extentName]
        , target == AgentRoot
        , let Location (_, top) = extent.extentUpperLeft
        , let (_, height) = extent.extentSize
        ]
    indicator label = hCenter $
        clickable ConversationLatest $
            withAttr Theme.thinkingAttr (txt label)

pictureScore :: V.DisplayRegion -> V.Picture -> Int
pictureScore region picture =
    sum
        [ case span of
            TextSpan attr outputWidth charWidth text ->
                length (show attr) + outputWidth + charWidth
                    + LazyText.foldl' (\total character -> total + ord character) 0 text
            Skip width -> width
            RowEnd width -> width
        | row <- toList (displayOpsForPic picture region)
        , span <- toList row
        ]

forceWidget :: Bool -> Widget Name -> Widget Name
forceWidget verify widget = Widget (hSize widget) (vSize widget) do
    result <- render widget
    let !extentScore = sum
            [ length (show extent.extentName) + left + top + width + height
            | extent <- result.extents
            , let Location (left, top) = extent.extentUpperLeft
            , let (width, height) = extent.extentSize
            ]
    extentScore `seq`
        (if verify then traceShow ("extents" :: Text, result.extents) else id)
            (pure result)

attributedRows :: V.DisplayRegion -> V.Picture -> [[(V.Attr, Char)]]
attributedRows region picture =
    [ concatMap spanCharacters (toList row)
    | row <- toList (displayOpsForPic picture region)
    ]
  where
    spanCharacters (TextSpan attr _ _ text) =
        [(attr, character) | character <- LazyText.unpack text]
    spanCharacters (Skip width) = replicate width (V.defAttr, ' ')
    spanCharacters (RowEnd width) = replicate width (V.defAttr, ' ')

timingStart :: IO TimingStart
timingStart = do
    stats <- getRTSStats
    cpu <- getCPUTime
    wall <- getMonotonicTimeNSec
    pure (TimingStart wall cpu stats.allocated_bytes)

finishMeasurement :: TimingStart -> a -> IO Measurement
finishMeasurement (TimingStart beforeWall beforeCpu beforeAllocation) retained = do
    afterWall <- getMonotonicTimeNSec
    afterCpu <- getCPUTime
    -- Collection is outside the interval. Keep precisely the current app and
    -- Brick cache alive; do not retain prior frames, pages or the full corpus.
    after <- bracket (newStablePtr retained) freeStablePtr \_ -> do
        performGC
        getRTSStats
    pure Measurement
        { wallMillis = fromIntegral (afterWall - beforeWall) / 1e6
        , cpuMillis = fromIntegral (afterCpu - beforeCpu) / 1e9
        , allocation = after.allocated_bytes - beforeAllocation
        , retainedBytes = after.gc.gcdetails_live_bytes
        }

printMeasurements :: String -> [Measurement] -> IO ()
printMeasurements label measurements =
    printf "%s wall_ms=%.6f cpu_ms=%.6f allocated_bytes=%d retained_bytes=%d\n"
        label (middle (map (.wallMillis) measurements))
        (middle (map (.cpuMillis) measurements))
        (middle (map (.allocation) measurements))
        (middle (map (.retainedBytes) measurements))
  where
    middle values = sort values !! (length values `div` 2)
