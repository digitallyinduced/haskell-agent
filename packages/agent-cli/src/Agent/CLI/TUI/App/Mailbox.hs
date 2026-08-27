{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -O0 -Wno-unused-imports #-}
module Agent.CLI.TUI.App.Mailbox where

import Agent.CLI.Clipboard ( formatImageSize )
import Agent.CLI.Dictation ( DictationControl(..)
    , DictationResult(..)
    , dictateWith
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
    , shiftEnterCsiBodies
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
import Control.Concurrent.Async (wait, waitCatch, withAsync)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, unless, void, when, (>=>))
import Control.Concurrent.STM ( STM , atomically , check , flushTQueue , newEmptyTMVarIO , newTQueueIO , newTVarIO , orElse , putTMVar , readTVar , readTMVar , readTQueue , registerDelay , retry , takeTMVar , writeTQueue , writeTVar )
import Agent.CLI.Recap ( autoRecapAwayThreshold , autoRecapIdleThreshold , autoRecapRetryInterval )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (modify')
import Control.Exception.Safe (finally, mask, onException, throwIO, tryAny)
import Control.Exception (AsyncException(UserInterrupt))
import Data.Char (isControl, isSpace)
import Data.Foldable (toList)
import Data.IORef ( atomicModifyIORef' , modifyIORef' , newIORef , readIORef , writeIORef )
import Data.List ( find , findIndex , intersperse , nub , sort , sortOn )
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe, maybeToList)
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

-- | Move events from the producer-facing mailbox into Brick. UI updates are
-- collected for one frame so a fast token stream causes at most one redraw
-- every ~16 ms. Blocking on Brick's bounded channel only blocks this pump,
-- never the model/tool worker publishing into the mailbox.
eventPump :: FullscreenRuntime -> IO ()
eventPump runtime = loop
  where
    loop = do
        pending <- atomically (takePendingAppEvent runtime.runtimeMailbox)
        delivered <- case pending of
            PendingUi first -> do
                threadDelay uiFrameDelayMicros
                rest <- atomically $
                    takePendingUiEventPrefix
                        (uiFrameBatchLimit - 1)
                        runtime.runtimeMailbox
                pure (AppUiBatch
                    (pendingUiEvent first :| map pendingUiEvent rest))
            PendingEvent event ->
                pure event
        writeBChan runtime.runtimeEvents delivered
        loop

uiFrameDelayMicros :: Int
uiFrameDelayMicros = 16000

uiFrameBatchLimit :: Int
uiFrameBatchLimit = 256

enqueueAppEvent :: FullscreenRuntime -> AppEvent -> IO ()
enqueueAppEvent runtime event =
    atomically do
        let AppEventMailbox pendingRef = runtime.runtimeMailbox
        pending <- readTVar pendingRef
        writeTVar pendingRef (appendAppEvent event pending)

enqueueMotionTick :: FullscreenRuntime -> IO ()
enqueueMotionTick runtime =
    atomically do
        queued <- readTVar runtime.runtimeMotionTickQueued
        unless queued do
            writeTVar runtime.runtimeMotionTickQueued True
            let AppEventMailbox pendingRef = runtime.runtimeMailbox
            pending <- readTVar pendingRef
            writeTVar pendingRef (appendAppEvent AppMotionTick pending)

appendAppEvent :: AppEvent -> Seq PendingAppEvent -> Seq PendingAppEvent
appendAppEvent event pending = case event of
    AppUi (UiLoop (TextDelta delta)) ->
        case Seq.viewr pending of
            rest :> PendingUi (PendingTextDeltas deltas) ->
                rest |> PendingUi (PendingTextDeltas (deltas |> delta))
            _ ->
                pending |> PendingUi
                    (PendingTextDeltas (Seq.singleton delta))
    AppUi (UiLoop (ReasoningDelta delta)) ->
        case Seq.viewr pending of
            rest :> PendingUi (PendingReasoningDeltas deltas) ->
                rest |> PendingUi
                    (PendingReasoningDeltas (deltas |> delta))
            _ ->
                pending |> PendingUi
                    (PendingReasoningDeltas (Seq.singleton delta))
    AppUi uiEvent ->
        appendExactUiEvent uiEvent pending
    _ ->
        appendExactAppEvent event pending

appendExactUiEvent
    :: UiEvent
    -> Seq PendingAppEvent
    -> Seq PendingAppEvent
appendExactUiEvent event pending =
    case Seq.viewr pending of
        rest :> PendingUi (PendingExactUi previous)
            | Just merged <- Bridge.mergeUiEvents previous event ->
                rest |> PendingUi (PendingExactUi merged)
        _ ->
            pending |> PendingUi (PendingExactUi event)

appendExactAppEvent
    :: AppEvent
    -> Seq PendingAppEvent
    -> Seq PendingAppEvent
appendExactAppEvent AppMotionTick pending
    | any isPendingMotionTick pending =
        pending
  where
    isPendingMotionTick = \case
        PendingEvent AppMotionTick -> True
        _ -> False
appendExactAppEvent event pending =
    case (Seq.viewr pending, event) of
        ( rest :> PendingEvent (AppAgentSnapshot _ _)
            , AppAgentSnapshot selected entries
            ) ->
                rest |> PendingEvent (AppAgentSnapshot selected entries)
        (rest :> PendingEvent (AppSetWindowTitle _), AppSetWindowTitle title) ->
            rest |> PendingEvent (AppSetWindowTitle title)
        (rest :> PendingEvent (AppDictationPartial _), AppDictationPartial text) ->
            rest |> PendingEvent (AppDictationPartial text)
        _ ->
            pending |> PendingEvent event

takePendingAppEvent :: AppEventMailbox -> STM PendingAppEvent
takePendingAppEvent (AppEventMailbox pendingRef) = do
    pending <- readTVar pendingRef
    case Seq.viewl pending of
        EmptyL -> retry
        event :< rest -> do
            writeTVar pendingRef rest
            pure event

takePendingUiEventPrefix
    :: Int
    -> AppEventMailbox
    -> STM [PendingUiEvent]
takePendingUiEventPrefix limit (AppEventMailbox pendingRef) = do
    pending <- readTVar pendingRef
    let (events, rest) = go limit [] pending
    writeTVar pendingRef rest
    pure events
  where
    go remaining acc pending
        | remaining <= 0 = (reverse acc, pending)
        | otherwise =
            case Seq.viewl pending of
                PendingUi event :< rest ->
                    go (remaining - 1) (event : acc) rest
                _ ->
                    (reverse acc, pending)

pendingUiEvent :: PendingUiEvent -> UiEvent
pendingUiEvent = \case
    PendingExactUi event -> event
    PendingTextDeltas deltas ->
        UiLoop (TextDelta (Text.concat (toList deltas)))
    PendingReasoningDeltas deltas ->
        UiLoop (ReasoningDelta (Text.concat (toList deltas)))
