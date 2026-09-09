{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -O0 -Wno-unused-imports #-}
module Agent.CLI.TUI.App.Run where

import Agent.CLI.TUI.Keyboard (mkKeyboardVty)

import Agent.CLI.Clipboard ( formatImageSize )
import Agent.CLI.Dictation ( DictationControl(..)
    , DictationResult(..)
    , dictateWithTarget
    , insertDictation
    )
import Agent.CLI.Secret (sanitizeSecretPromptText)
import Agent.CLI.Artifact (fencedCodeBlock)
import Agent.CLI.Input ( ReplLine(..)
    , readReplHistory
    , terminalTextWidth
    , truncateDisplayText
    )
import Agent.CLI.AgentViewport ( AgentEntry(..)
    , AgentStep(..)
    , AgentStepState(..)
    , AgentTarget(..)
    , agentDisplayName
    , agentEntryTreeLabelWithGlyphModel
    , agentStatusGlyph
    , lookupAgentEntry
    )
import Agent.CLI.Interrupt (CtrlCDecision(..))
import Agent.CLI.ImagePreview ( ImagePreviewProtocol(..)
    , detectImagePreviewProtocol
    , kittyDeleteImageSequence
    , kittyPlacedImageSequence
    , positionImagePayload
    )
import Agent.CLI.Command ( SkillCommand , SlashCatalog(..)
    , defaultSlashCatalog
    , slashCatalogWithSkills
    )
import Agent.CLI.Permission (PermissionChoice(..))
import Agent.CLI.Resume ( ResumeBrowser(..)
    , ResumeEntry(..)
    , applyResumeSearchResults
    , beginResumeSearch
    , cycleResumeSource
    , endResumeSearch
    , groupResumeEntries
    , insertResumeSearch
    , moveResumeBrowser
    , removeResumeEntry
    , replaceResumeEntry
    , resumeRelativeAge
    , resumeSourceLabel
    , selectedResumeBrowser
    , setResumeDeletePending
    , setResumeNotice
    , toggleResumeExpanded
    , visibleResumeBrowser
    )
import Agent.CLI.Render (formatElapsed)
import Agent.CLI.Style (motionGlyphSet)
import Agent.CLI.WindowTitle (oscWindowTitleBytes)
import Agent.CLI.Status (formatTokenUsage)
import Agent.CLI.Timestamp (currentShortMessageTimestamp)
import Agent.CLI.Terminal ( TerminalCapabilities(..)
    , detectTerminalCapabilities
    , kittyAltCsiBodies
    , kittyCtrlCsiBodies
    , kittyCtrlUnderscoreCsiBodies
    , kittyKeyboardDisambiguatePush
    , kittyKeyboardPop
    , kittySuperVCsiBodies
    , osc22MousePointer
    , shiftEnterCsiBodies
    , terminalSupportsMousePointer
    , wrapTerminalPassthrough
    )
import qualified Agent.TUI.Theme as Theme
import qualified Agent.CLI.TUI.Bridge as Bridge
import qualified Agent.CLI.TUI.Composer as Composer
import Agent.CLI.TUI.History ( HistoryCursor(..)
    , HistoryDirection(..)
    , HistoryGeneration(..)
    , HistoryPage(..)
    , HistoryRequest(..)
    , HistoryTurn(..)
    , HistoryWindow(..)
    , appendHistoryTurn
    , applyHistoryPage
    , clearHistoryRequest
    , emptyHistoryWindow
    , historyWindowBlock
    , historyWindowOlderAvailable
    , historyWindowRequest
    , unarchivedLiveStart
    , historyWindowSetAnchors
    , markHistoryRequest
    , setHistoryWindowTurns
    )
import Agent.CLI.TUI.LambdaArt ( lambdaArtWidget )
import Agent.CLI.TUI.Motion ( advanceCompletionFlashes , appMotionTiming , completionFlashTransitions , elapsedMillisSince , hasBackgroundActivity , isBackgroundAgentActive , motionDemandFor , motionDemandForTerminalFocus , motionModeForTerminalFocus , nativeProgressKeepaliveDue , nextMotionSchedule , turnCompletionRequiresRedraw , uiEventRestartsMotionSchedule , userActionPending )
import Agent.CLI.TUI.Render ( agentEntryWindow , agentPaneEntryLimit , agentPaneVisible , applyChildConversationUiEvent , choiceRowColumns , conversationUiForTarget , conversationScrollbarRenderer , drawApp , fullscreenBounds , fullscreenSurface , onboardingVisibleRowIndices , normalizeTextOverlayInsertion , maskedSecretText , quickStartRows , quickStartVisible , repositoryHeaderText , resumeSearchCursorColumn , selectedAgentConversation , textOverlayDisplayText )
import Agent.CLI.TUI.ImagePreview ( NativePreviewPlacement(..)
    , TuiImagePreview(..)
    , nativePreviewPlacements
    , prepareTuiImagePreview
    , previewCountForWidth
    , previewCellSize
    , renderTuiImagePreview
    , sameNativePreviewLayout
    )
import Agent.TUI.Markdown ( codeWidgetWithSyntaxHighlighting , markdownWidgetWithLinks , markdownWidgetWithSyntaxHighlightingAndLinks )
import Agent.TUI.FencedCode ( FencedBlock(..)
    , fencedBlocks
    )
import Agent.TUI.TextWidth ( clampGraphemeCursor , displayTerminalText , nextGraphemeBoundary , previousGraphemeBoundary )
import Agent.Syntax ( SyntaxHighlighter , loadSyntaxLanguage , newSyntaxHighlighter , resolveFenceLanguage )
import qualified Agent.CLI.TUI.Scroll as Scroll
import qualified Agent.CLI.TUI.Transcript as Transcript
import Agent.CLI.TUI.Types
import Agent.TUI.Model
import Agent.TUI.Motion ( MotionDemand(..)
    , MotionMode(..)
    , backgroundIndicator
    , completionFlashDurationMillis
    , foregroundIndicator
    , quietIndicator
    , waitingIndicator
    )
import Agent.TUI.Presentation ( permissionToolCallPromptRelative )
import Agent.Loop (ImageAttachment(..), LoopEvent(..))
import Agent.ToolDispatch (ToolCall(..))
import Brick
import qualified Brick.Types as B
import Brick.BChan ( newBChan , writeBChan )
import Brick.Widgets.Border (borderWithLabel)
import qualified Brick.Widgets.Border as Border
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center (center, centerLayer, hCenter)
import Codec.Picture (pixelAt)
import Control.Applicative ((<|>))
import Control.Concurrent.Async
    ( Async
    , concurrently_
    , poll
    , wait
    , waitCatch
    , withAsync
    , withAsyncWithUnmask
    )
import Control.Concurrent (threadDelay)
import Control.Monad (forever, unless, void, when, (>=>))
import Control.Concurrent.STM ( STM , atomically , check , flushTQueue , newEmptyTMVarIO , newTQueueIO , newTVarIO , orElse , putTMVar , readTVar , readTMVar , readTQueue , registerDelay , retry , takeTMVar , writeTQueue , writeTVar )
import Agent.CLI.Recap ( autoRecapAwayThreshold , autoRecapIdleThreshold , autoRecapRetryInterval )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (modify')
import Control.Exception.Safe (finally, onException, throwIO, tryAny)
import Control.Exception (AsyncException(UserInterrupt))
import Data.Char (isControl, isSpace)
import Data.Foldable (toList)
import Data.IORef ( atomicModifyIORef' , modifyIORef' , newIORef , readIORef , writeIORef )
import Data.List ( find , findIndex , intersperse , nub , sort , sortOn )
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe, maybeToList)
import System.Timeout (timeout)
import Data.Sequence (Seq, ViewL(..), ViewR(..), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as Vty
import System.Environment (lookupEnv)
import System.Info (os)
import System.IO (stdout)
import System.Posix.Process (getProcessID)
import System.Process (callProcess)
import Agent.CLI.TUI.App.Runtime
import Agent.CLI.TUI.App.Mailbox
import Agent.CLI.TUI.App.History
import Agent.CLI.TUI.App.Event

-- | The UI owns the session worker until it has terminated, including when
-- the UI fails. Closing input is a nonblocking, idempotent operation, not an
-- EOF insertion into the bounded prompt queue.
--
-- The grace interval bounds cooperative shutdown only. If it expires, scope
-- exit cancels and joins the worker; we never abandon it or kill the host
-- process. Resource finalizers must themselves provide interruptible cleanup.
withFullscreenWorker :: (Async a -> IO ()) -> IO a -> (Async a -> IO ()) -> IO a
withFullscreenWorker closeChannels workerAction runUi =
    withAsyncWithUnmask (\unmask -> unmask workerAction) \worker -> do
        runUi worker `finally` closeChannels worker
        timeout 2_000_000 (wait worker) >>= \case
            Just result -> pure result
            Nothing -> throwIO UserInterrupt

closeFullscreenChannels :: FullscreenRuntime -> Async a -> IO ()
closeFullscreenChannels runtime worker = do
    workerResult <- poll worker
    atomically do
        closeAppEventMailbox runtime.runtimeMailbox
        -- Completed workers may return a session/provider transition. The
        -- outer flow reuses their input buffer, including queued prompts.
        case workerResult of
            Nothing -> Composer.closeFullscreenInputBuffer runtime.runtimeInput
            Just _ -> pure ()

runFullscreen :: FullscreenRuntime -> IO a -> IO a
runFullscreen runtime workerAction = do
    history <- readReplHistory
    (initialAgent, initialAgents) <- runtime.runtimeAgentSnapshot
    initialClock <- getMonotonicTimeNSec
    terminal <- detectTerminalCapabilities stdout
    let makeVty = do
            vty <- mkKeyboardVty fullscreenVtyConfig
            let setupVty = do
                    let output = V.outputIface vty
                    -- Without this mode terminals paste image clipboard
                    -- fallbacks (paths, URLs, or other text representations)
                    -- as ordinary key events, so the composer renders them as
                    -- text. Vty turns the bracketed sequence into one EvPaste
                    -- that we can classify.
                    when (V.supportsMode output V.BracketedPaste) $
                        V.setMode output V.BracketedPaste True
                    when (V.supportsMode output V.Mouse) $
                        V.setMode output V.Mouse True
                    when (V.supportsMode output V.Focus) $
                        V.setMode output V.Focus True
                    -- Vty deliberately leaves OSC 8 output disabled by
                    -- default even when rendered attributes contain URLs.
                    when (V.supportsMode output V.Hyperlink) $
                        V.setMode output V.Hyperlink True
                    when (V.supportsMode output V.Focus) $
                        V.setMode output V.Focus True
                    wrapped <-
                        wrapNativePreviewVty runtime vty
                            >>= wrapFullscreenKeyboardVty
                                terminal.terminalKittyKeyboard
                            >>= wrapMarkdownLinkCursorVty terminal
                    applyStoredFullscreenWindowTitle
                        runtime
                        (V.outputIface wrapped)
                    pure wrapped
            setupVty `onException` V.shutdown vty
    withTrackedVtyBuilder makeVty \buildVty -> do
        initialVty <- buildVty
        let
            initialState =
                initialFullscreenAppState
                    runtime
                    history
                    initialAgent
                    initialAgents
                    initialClock
            (initialDemand, initialDelay) =
                appMotionTiming initialState
        atomically $
            writeTVar
                runtime.runtimeMotionSchedule
                (initialDemand, initialDelay, 0)
        withFullscreenWorker
            (closeFullscreenChannels runtime)
            workerAction
            \worker ->
                runFullscreenUi
                    runtime
                    worker
                    initialVty
                    buildVty
                    initialState
                    initialAgent
                    initialAgents
  where
    runFullscreenUi
            runtime worker initialVty buildVty initialState
            initialAgent initialAgents =
        withAsync uiTicker \_uiTicker ->
            withAsync (agentTicker (initialAgent, initialAgents)) \_agentTicker ->
                withAsync (eventPump runtime) \_eventPump ->
                    withAsync (recapTicker runtime) \_recapTicker ->
                        withAsync
                            (concurrently_ historyLoader (runHistoryChartWorker runtime))
                            \_historyLoader ->
                            withAsync
                                dictationWorker
                                \_dictationWorker ->
                                withAsync
                                    (runSyntaxHighlighterForRuntime runtime)
                                    \_syntaxLoader ->
                                    withAsync
                                        (void (waitCatch worker)
                                            >> enqueueAppEvent runtime AppStop)
                                        \_notifier -> do
                                            finalState <-
                                                customMain
                                                    initialVty
                                                    buildVty
                                                    (Just runtime.runtimeEvents)
                                                    fullscreenApp
                                                    initialState
                                                `finally`
                                                    (closeFullscreenChannels runtime worker
                                                        >> runtime.runtimeNativeProgress False)
                                            mapM_
                                                (`Composer.requestDictationStop` True)
                                                finalState.appDictation
    recapTicker _runtime = forever do
        threadDelay 20_000_000
        enqueueAppEvent runtime AppRecapPoll

    uiTicker = waitForDemand
      where
        waitForDemand = do
            (demand, delayMicros, generation) <- atomically do
                schedule@(current, _, _) <-
                    readTVar runtime.runtimeMotionSchedule
                if current == MotionNone then retry else pure schedule
            tickActive demand delayMicros generation

        tickActive demand delayMicros generation = do
            timer <- registerDelay delayMicros
            outcome <- atomically $
                (do
                    current <-
                        readTVar runtime.runtimeMotionSchedule
                    check (current /= (demand, delayMicros, generation))
                    pure (Left current))
                    `orElse`
                (do
                    ready <- readTVar timer
                    check ready
                    Right
                        <$> readTVar runtime.runtimeMotionSchedule)
            case outcome of
                Left (MotionNone, _, _) ->
                    waitForDemand
                Left (active, nextDelay, nextGeneration) ->
                    tickActive active nextDelay nextGeneration
                Right (MotionNone, _, _) ->
                    waitForDemand
                Right (active, nextDelay, nextGeneration) -> do
                    enqueueMotionTick runtime
                    tickActive active nextDelay nextGeneration

    agentTicker previous = do
        threadDelay 500000
        next <- tryAny runtime.runtimeAgentSnapshot
        previous' <- case next of
            Left _ -> pure previous
            Right snapshot
                | snapshot == previous -> pure previous
                | otherwise -> do
                    enqueueAppEvent runtime
                        (uncurry AppAgentSnapshot snapshot)
                    pure snapshot
        agentTicker previous'

    historyLoader = do
        request <- atomically (readTQueue runtime.runtimeHistoryRequests)
        source <- readIORef runtime.runtimeHistorySource
        result <- case source of
            Nothing ->
                pure (Left "Session history is unavailable.")
            Just current ->
                tryAny (current.historySourceLoad request) >>= \case
                    Left err ->
                        pure (Left (Text.pack (show err)))
                    Right loaded ->
                        pure loaded
        let normalized =
                fmap
                    (\page ->
                        page
                            { historyPageGeneration =
                                request.historyRequestGeneration
                            , historyPageDirection =
                                request.historyRequestDirection
                            })
                    result
        enqueueAppEvent runtime
            (AppHistoryLoaded request normalized)
        historyLoader

    dictationWorker = forever do
        job <- atomically (readTQueue runtime.runtimeDictationJobs)
        actions <- readIORef runtime.runtimeSessionActions
        result <- case actions.sessionDictationTarget of
            Nothing ->
                pure $ DictationFailed
                    "Dictation is unavailable for the active session"
            Just target ->
                dictateWithTarget
                    target
                    DictationControl
                        { dictationWaitForStop =
                            job.dictationJobWaitForStop
                        , dictationOnRecording =
                            enqueueAppEvent runtime $
                                AppDictationRecording
                                    job.dictationJobRecordingSession
                        , dictationOnTranscript =
                            enqueueAppEvent runtime . AppDictationPartial
                        }
        enqueueAppEvent runtime $
            AppDictationFinished $
                case result of
                    DictationTranscript transcript -> Right transcript
                    DictationFailed message -> Left message

-- | Restore the terminal cursor before every fullscreen Vty lifecycle ends.
-- This includes Brick suspension, which rebuilds Vty later, and protects
-- terminals that retain OSC 22 pointer state after the alternate screen.
wrapMarkdownLinkCursorVty
    :: TerminalCapabilities
    -> V.Vty
    -> IO V.Vty
wrapMarkdownLinkCursorVty terminal vty
    | not (terminalSupportsMousePointer terminal.terminalKind) = pure vty
    | otherwise =
        pure vty
            { V.shutdown = do
                alreadyShutdown <- V.isShutdown vty
                unless alreadyShutdown $
                    V.outputByteBuffer
                        (V.outputIface vty)
                        ( TextEncoding.encodeUtf8
                            ( wrapTerminalPassthrough
                                terminal.terminalInsideTmux
                                (osc22MousePointer terminal.terminalKind False)
                            )
                        )
                        `finally` V.shutdown vty
            }

-- | Construct the retained application state shared by the live entry point
-- and renderer tests. Generated tests should start from the same defaults as
-- a real fullscreen session instead of assembling an approximate state.
initialFullscreenAppState
    :: FullscreenRuntime
    -> [Text]
    -> AgentTarget
    -> [AgentEntry]
    -> Word64
    -> AppState
initialFullscreenAppState runtime history initialAgent initialAgents initialClock =
    AppState
        { appUi = runtime.runtimeInitial
        , appHistoryWindow =
            emptyHistoryWindow
                (HistoryGeneration 0)
                historyWindowTurnBudget
                historyWindowBlockBudget
                historyWindowByteBudget
        , appHistorySelectedBlock = Nothing
        , appHistoryLiveStart = Nothing
        , appNextHistoryBlockId = -1
        , appPermissionReply = Nothing
        , appRuntime = runtime
        , appTheme = runtime.runtimeTheme
        , appSlashIndex = 0
        , appChoice = Nothing
        , appResume = Nothing
        , appTextPrompt = Nothing
        , appMetaConsole = Nothing
        , appSlashDismissed = False
        , appPasted = False
        , appHistory = Bridge.trimHistory history
        , appHistoryIndex = Nothing
        , appHistoryDraft = ""
        , appKillBuffer = ""
        , appKillChain = False
        , appUndo = []
        , appDictation = Nothing
        , appSlashCatalog = defaultSlashCatalog
        , appImagePreviews = []
        , appComposerOwnsImagePreviews = False
        , appSubmittedImagePreviews = Map.empty
        , appAgentSelected = initialAgent
        , appAgentEntries = initialAgents
        , appPullRequestURL = Nothing
        , appAgentHover = Nothing
        , appMarkdownLinkHovered = False
        , appHoveredControl = Nothing
        , appHoveredLine = Nothing
        , appPressedControl = Nothing
        , appWorkerStopped = False
        , appConversationAnchor = Nothing
        , appFocusLostAt = Nothing
        , appAutoRecapShownThisAway = False
        , appLastAutoRecapAttemptAt = Nothing
        , appLastTurnCompletedAt = Nothing
        , appConversationReflowQueued = False
        , appWindowTitle = Nothing
        , appMotionElapsedMillis = 0
        , appCompletionFlashes = Map.empty
        , appMotionScheduleReset = False
        , appClockNanos = initialClock
        , appNativeProgressKeepaliveBucket = 0
        , appSyntaxHighlighter = Nothing
        , appSyntaxRequested = Set.empty
        , appTerminalFocus = TerminalFocusUnknown
        }
