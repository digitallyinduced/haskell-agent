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
import Agent.Subagents.Types ( SubagentId(..) )
import Agent.CLI.Interrupt (CtrlCDecision(..))
import Agent.CLI.ImagePreview ( ImagePreviewProtocol(..)
    , detectImagePreviewProtocol
    , kittyDeleteImageSequence
    , kittyPlacedImageSequence
    , positionImagePayload
    )
import Agent.CLI.Command ( SkillCommand(..) , SlashCatalog(..)
    , SlashCommand(..)
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
import Agent.TUI.Presentation
    ( TodoDisplayLine(..)
    , permissionToolCallPromptRelative
    )
import Agent.Loop
    ( ImageAttachment(..)
    , LoopEvent(..)
    , TurnCompletion(..)
    , TurnOutput(..)
    )
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallResult(..)
    , ToolResultImage(..)
    , toolCallResultImages
    )
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
import qualified Data.ByteString as BS
import Data.Char (isControl, isSpace)
import Data.Foldable (toList)
import Data.IORef ( atomicModifyIORef' , modifyIORef' , newIORef , readIORef , writeIORef )
import Data.List ( find , findIndex , foldl' , intersperse , nub , sort , sortOn )
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe, maybeToList)
import Data.Sequence (Seq, ViewL(..), ViewR(..), (<|), (><), (|>))
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
enqueueAppEvent runtime = \case
    AppUi (UiLoop (TextDelta text)) ->
        enqueueStreamingText runtime False text
    AppUi (UiLoop (ReasoningDelta text)) ->
        enqueueStreamingText runtime True text
    event ->
        atomically (enqueueMailboxEvent runtime.runtimeMailbox event)

enqueueStreamingText :: FullscreenRuntime -> Bool -> Text -> IO ()
enqueueStreamingText runtime reasoning = go
  where
    go text
        | Text.null text =
            enqueueChunk Text.empty
        | Text.length text <= appEventMailboxTextChunkCodeUnits =
            enqueueChunk (Text.copy text)
        | otherwise = do
            let (chunk0, rest) =
                    Text.splitAt appEventMailboxTextChunkCodeUnits text
            enqueueChunk (Text.copy chunk0)
            if Text.null rest then pure () else go rest
    enqueueChunk text =
        atomically
            (enqueueMailboxEvent
                runtime.runtimeMailbox
                (AppUi
                    (UiLoop
                        (if reasoning
                            then ReasoningDelta text
                            else TextDelta text))))

enqueueMotionTick :: FullscreenRuntime -> IO ()
enqueueMotionTick runtime =
    atomically do
        queued <- readTVar runtime.runtimeMotionTickQueued
        unless queued do
            writeTVar runtime.runtimeMotionTickQueued True
            enqueueMailboxEvent runtime.runtimeMailbox AppMotionTick

-- Keep Brick's downstream queue shallow: it is count-bounded rather than
-- byte-bounded, so a large capacity would let the pump move many heavyweight
-- events outside the accounted mailbox before rendering catches up.
appEventChannelCapacity :: Int
appEventChannelCapacity = 1

-- These budgets cover retained producer-side events. The Brick channel may
-- additionally hold 'appEventChannelCapacity' entries and the pump may hold
-- one event while its sink is blocked.
appEventMailboxCapacity :: Int
appEventMailboxCapacity = 4096

appEventMailboxPayloadBudgetBytes :: Int
appEventMailboxPayloadBudgetBytes = 16 * 1024 * 1024

-- Structural events are lossless and receive a small reserve so a stream
-- which fills the normal budget cannot prevent its finish/retraction/stop
-- event from being published. The reserve itself is bounded.
appEventMailboxControlReserve :: Int
appEventMailboxControlReserve = 256

appEventMailboxControlReserveBytes :: Int
appEventMailboxControlReserveBytes = 2 * 1024 * 1024

appEventMailboxTextChunkCodeUnits :: Int
appEventMailboxTextChunkCodeUnits =
    (appEventMailboxPayloadBudgetBytes - 64) `div` 4

enqueueMailboxEvent :: AppEventMailbox -> AppEvent -> STM ()
enqueueMailboxEvent (AppEventMailbox stateRef) event = do
    state <- readTVar stateRef
    let (pending, payloadBytes) =
            appendAppEventAccounted
                event
                state.mailboxPendingEvents
                state.mailboxPendingBytes
        count = Seq.length pending
        control = isControlAppEvent event
        countLimit =
            appEventMailboxCapacity
                + if control then appEventMailboxControlReserve else 0
        byteLimit =
            appEventMailboxPayloadBudgetBytes
                + if control then appEventMailboxControlReserveBytes else 0
        -- An indivisible event may itself exceed the budget. Refusing that
        -- first event would retry forever even though the mailbox is empty;
        -- admitting exactly one lets the consumer make progress while still
        -- preventing any additional payload from accumulating behind it.
        firstOversizedSingleton =
            Seq.null state.mailboxPendingEvents
                && count == 1
        oversizedKeyedReplacement =
            Seq.length state.mailboxPendingEvents == 1
                && count == 1
                && isJust (appEventCoalesceKey event)
    check
        ( count <= countLimit
            && ( payloadBytes <= byteLimit
                || firstOversizedSingleton
                || oversizedKeyedReplacement
               )
        )
    writeTVar stateRef state
        { mailboxPendingEvents = pending
        , mailboxPendingCount = count
        , mailboxPendingBytes = payloadBytes
        , mailboxHighWaterCount =
            max state.mailboxHighWaterCount count
        , mailboxHighWaterBytes =
            max state.mailboxHighWaterBytes payloadBytes
        }

appendAppEventAccounted
    :: AppEvent
    -> Seq PendingAppEvent
    -> Int
    -> (Seq PendingAppEvent, Int)
appendAppEventAccounted event pending oldBytes =
    let updated = appendAppEvent event pending
        -- Appending normally only adds the new event. Coalescing may replace
        -- an older keyed value; find the exact delta without rescanning every
        -- retained text chunk on the common adjacent-streaming path.
        bytes = case event of
            AppUi (UiLoop (TextDelta delta)) ->
                saturatingAdd oldBytes (logicalTextChunkBytes delta)
            AppUi (UiLoop (ReasoningDelta delta)) ->
                saturatingAdd oldBytes (logicalTextChunkBytes delta)
            _ ->
                pendingAppEventsLogicalBytes updated
    in (updated, bytes)

appEventCoalesceKey :: AppEvent -> Maybe PendingEventCoalesceKey
appEventCoalesceKey = \case
    AppUi uiEvent ->
        pendingEventCoalesceKey
            (PendingUi (PendingExactUi uiEvent))
    event ->
        pendingEventCoalesceKey (PendingEvent event)

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
            appendLatestPendingEvent
                (PendingUi (PendingExactUi event))
                pending

appendExactAppEvent
    :: AppEvent
    -> Seq PendingAppEvent
    -> Seq PendingAppEvent
appendExactAppEvent event pending =
    appendLatestPendingEvent (PendingEvent event) pending

appendLatestPendingEvent
    :: PendingAppEvent
    -> Seq PendingAppEvent
    -> Seq PendingAppEvent
appendLatestPendingEvent event pending =
    case pendingEventCoalesceKey event of
        Nothing -> pending |> event
        Just key ->
            removeLatestAfterBarrier
                ((== Just key) . pendingEventCoalesceKey)
                pending
                |> event

removeLatestAfterBarrier
    :: (PendingAppEvent -> Bool)
    -> Seq PendingAppEvent
    -> Seq PendingAppEvent
removeLatestAfterBarrier matches = go Seq.empty
  where
    go suffix remaining =
        case Seq.viewr remaining of
            EmptyR -> suffix
            rest :> event
                | pendingEventBarrier event ->
                    remaining >< suffix
                | matches event ->
                    rest >< suffix
                | otherwise ->
                    go (event <| suffix) rest

data PendingEventCoalesceKey
    = CoalesceActivity
    | CoalesceToolUpdate !Text
    | CoalesceToolOutput !Text
    | CoalesceAgentSnapshot
    | CoalesceWindowTitle
    | CoalesceDictationPartial
    | CoalesceConversationReflow
    | CoalesceImagePlacements
    | CoalesceMotionTick
    | CoalesceRecapPoll
    | CoalesceSyntaxHighlighter
    deriving (Eq)

pendingEventCoalesceKey
    :: PendingAppEvent
    -> Maybe PendingEventCoalesceKey
pendingEventCoalesceKey = \case
    PendingUi (PendingExactUi (UiLoop loopEvent)) ->
        case loopEvent of
            ActivityUpdated _ -> Just CoalesceActivity
            ToolUpdated call -> Just (CoalesceToolUpdate call.callId)
            ToolOutputUpdated callId _ ->
                Just (CoalesceToolOutput callId)
            _ -> Nothing
    PendingEvent appEvent ->
        case appEvent of
            AppAgentSnapshot _ _ -> Just CoalesceAgentSnapshot
            AppSetWindowTitle _ -> Just CoalesceWindowTitle
            AppDictationPartial _ -> Just CoalesceDictationPartial
            AppConversationReflow -> Just CoalesceConversationReflow
            AppSyncSubmittedImagePlacements ->
                Just CoalesceImagePlacements
            AppMotionTick -> Just CoalesceMotionTick
            AppRecapPoll -> Just CoalesceRecapPoll
            AppSyntaxHighlighterChanged ->
                Just CoalesceSyntaxHighlighter
            _ -> Nothing
    _ -> Nothing

-- Latest-value updates may move across one another and text/reasoning deltas,
-- but never across lifecycle, history, modal, input, or stop boundaries.
pendingEventBarrier :: PendingAppEvent -> Bool
pendingEventBarrier event =
    case event of
        PendingUi (PendingTextDeltas _) -> False
        PendingUi (PendingReasoningDeltas _) -> False
        PendingUi (PendingExactUi (UiLoop loopEvent)) ->
            case loopEvent of
                TextDelta _ -> False
                ReasoningDelta _ -> False
                ActivityUpdated _ -> False
                ToolUpdated _ -> False
                ToolOutputUpdated _ _ -> False
                _ -> True
        PendingUi (PendingExactUi _) -> True
        PendingEvent _ ->
            case pendingEventCoalesceKey event of
                Just _ -> False
                Nothing -> True

takePendingAppEvent :: AppEventMailbox -> STM PendingAppEvent
takePendingAppEvent (AppEventMailbox stateRef) = do
    state <- readTVar stateRef
    case Seq.viewl state.mailboxPendingEvents of
        EmptyL -> retry
        event :< rest -> do
            writeTVar stateRef state
                { mailboxPendingEvents = rest
                , mailboxPendingCount =
                    max 0 (state.mailboxPendingCount - 1)
                , mailboxPendingBytes =
                    max 0
                        ( state.mailboxPendingBytes
                            - pendingAppEventLogicalBytes event
                        )
                }
            pure event

takePendingUiEventPrefix
    :: Int
    -> AppEventMailbox
    -> STM [PendingUiEvent]
takePendingUiEventPrefix limit (AppEventMailbox stateRef) = do
    state <- readTVar stateRef
    let (events, rest) =
            go limit [] state.mailboxPendingEvents
        removedBytes =
            sum
                (map
                    (pendingAppEventLogicalBytes . PendingUi)
                    events)
    writeTVar stateRef state
        { mailboxPendingEvents = rest
        , mailboxPendingCount =
            max 0 (state.mailboxPendingCount - length events)
        , mailboxPendingBytes =
            max 0 (state.mailboxPendingBytes - removedBytes)
        }
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

pendingAppEventsLogicalBytes :: Seq PendingAppEvent -> Int
pendingAppEventsLogicalBytes =
    foldl'
        (\total event ->
            saturatingAdd total (pendingAppEventLogicalBytes event))
        0

pendingAppEventLogicalBytes :: PendingAppEvent -> Int
pendingAppEventLogicalBytes = \case
    PendingUi event -> pendingUiEventLogicalBytes event
    PendingEvent event -> appEventLogicalBytes event

pendingUiEventLogicalBytes :: PendingUiEvent -> Int
pendingUiEventLogicalBytes = \case
    PendingTextDeltas deltas ->
        foldl' (\size text -> saturatingAdd size (logicalTextChunkBytes text)) 0 deltas
    PendingReasoningDeltas deltas ->
        foldl' (\size text -> saturatingAdd size (logicalTextChunkBytes text)) 0 deltas
    PendingExactUi event ->
        uiEventLogicalBytes event

uiEventLogicalBytes :: UiEvent -> Int
uiEventLogicalBytes = \case
    UiLoop event ->
        case event of
            TextDelta text -> logicalTextBytes text
            ReasoningDelta text -> logicalTextBytes text
            ActivityUpdated text -> logicalTextBytes text
            ProviderLimitUpdated
                { providerLimitText = text
                } ->
                logicalTextBytes text
            WarningRaised text -> logicalTextBytes text
            ResponseRestarted text -> logicalTextBytes text
            TurnStarted -> 128
            TurnFinished output -> turnOutputLogicalBytes output
            ToolStarted call -> toolCallLogicalBytes call
            ToolUpdated call -> toolCallLogicalBytes call
            ToolArgumentsUpdated call -> toolCallLogicalBytes call
            ToolOutputUpdated callId output ->
                logicalTextsBytes [callId, output]
            ToolFinished result -> toolCallResultLogicalBytes result
            ToolRetracted callId -> logicalTextBytes callId
            ResponseAttemptDiscarded -> 128
            ResponseAttemptFailed -> 128
            NativeAgentStarted agent parent prompt model ->
                logicalTextsBytes
                    ([agent, prompt] <> maybeToList parent <> maybeToList model)
            NativeAgentOutput agent output ->
                logicalTextsBytes [agent, output]
            NativeAgentFinished agent _ -> logicalTextBytes agent
    UiUserSubmitted text -> logicalTextBytes text
    UiDraftSubmitted -> 128
    UiInputSteered text -> logicalTextBytes text
    UiInputQueued text -> logicalTextBytes text
    UiInputPromoted text -> logicalTextBytes text
    UiQueuedInputStarted -> 128
    UiSetDraft text _ -> logicalTextBytes text
    UiSetPrompt prompt -> promptStateLogicalBytes prompt
    UiSetPromptTarget model account ->
        logicalTextBytes model + logicalTextBytes account
    UiSetPromptEffort text -> logicalTextBytes text
    UiSetPromptLimitStatus status ->
        maybe 128 (logicalTextBytes . (.promptLimitText)) status
    UiSetContextUsage _ _ -> 128
    UiSetAwaitingInput _ -> 128
    UiSetRepository branch cwd root ->
        logicalTextsBytes [branch, cwd, root]
    UiSetNotice notice ->
        maybe 128 (logicalTextBytes . (.noticeText)) notice
    UiMoveSelection _ -> 128
    UiSelectBlock _ -> 128
    UiActivateBlock _ -> 128
    UiToggleSelected -> 128
    UiFocusChanged _ -> 128
    UiPermissionShown text -> logicalTextBytes text
    UiPermissionMoved _ -> 128
    UiPermissionHidden -> 128
    UiHistory text -> logicalTextBytes text
    UiAssistantHistory text -> logicalTextBytes text
    UiSystemMessage text -> logicalTextBytes text
    UiRecapStarted -> 128
    UiRecapReady text -> logicalTextBytes text
    UiRecapUnavailable text -> logicalTextBytes text
    UiErrorMessage text -> logicalTextBytes text
    UiRetryCountdown prefix _ suffix ->
        logicalTextsBytes [prefix, suffix]
    UiConversationCleared -> 128
    UiSetFollow _ -> 128
    UiTurnEnded _ -> 128
    UiTurnRestarted -> 128

appEventLogicalBytes :: AppEvent -> Int
appEventLogicalBytes = \case
    AppUi event -> uiEventLogicalBytes event
    AppUiBatch events ->
        foldl'
            (\size event -> saturatingAdd size (uiEventLogicalBytes event))
            0
            events
    AppAskPermission prompt _ ->
        saturatingAdd 256 (logicalTextBytes prompt)
    AppAskChoice _ title body _ rows _ ->
        saturatingAdd 256
            (saturatingAdd
                (logicalTextsBytes [title, body])
                (textRowsLogicalBytes rows))
    AppAskFilterChoice title _ rows _ ->
        saturatingAdd 256
            (saturatingAdd
                (logicalTextBytes title)
                (textRowsLogicalBytes rows))
    AppAskAdjustableFilterChoice title _ rows _ ->
        saturatingAdd 256 $
            saturatingAdd
                (logicalTextBytes title)
                ( foldl'
                    (\size (label, detail, values, _) ->
                        saturatingAdd size
                            (logicalTextsBytes (label : detail : values)))
                    0
                    rows
                )
    AppAskText _ title body draft _ ->
        saturatingAdd 256 (logicalTextsBytes [title, body, draft])
    AppAskResume browser _ _ _ _ ->
        max opaqueAppEventLogicalBytes
            (saturatingAdd 512 (resumeBrowserLogicalBytes browser))
    AppSuspend{} -> opaqueAppEventLogicalBytes
    AppSetSlashCatalog catalog ->
        saturatingAdd 512 (slashCatalogLogicalBytes catalog)
    AppSetSkillCommands skills ->
        saturatingAdd 256
            (foldl'
                (\size skill ->
                    saturatingAdd size (skillCommandLogicalBytes skill))
                0
                skills)
    AppSetModelIds modelIds ->
        saturatingAdd 256 (logicalTextsBytes modelIds)
    AppCommandPaletteSelected action ->
        case action of
            CommandPaletteSubmit text ->
                saturatingAdd 128 (logicalTextBytes text)
            CommandPaletteInsert text ->
                saturatingAdd 128 (logicalTextBytes text)
            CommandPaletteDismiss -> 128
    AppSetTheme{} ->
        256
    AppAgentSnapshot target entries ->
        foldl'
            (\size entry ->
                saturatingAdd size
                    (agentEntryLogicalBytes entry))
            (agentTargetLogicalBytes target)
            entries
    AppSetWindowTitle text -> logicalTextBytes text
    AppDictationPartial text -> logicalTextBytes text
    AppDictationFinished result ->
        either logicalTextBytes logicalTextBytes result
    AppSetImagePreviews previews ->
        imagePreviewPairsLogicalBytes previews
    AppCommitImagePreviews previews ->
        imagePreviewPairsLogicalBytes previews
    AppToolImage callId preview ->
        saturatingAdd
            (logicalTextBytes callId)
            (imagePreviewLogicalBytes preview)
    AppSyntaxHighlighterChanged -> 256
    AppHistoryReset page ->
        saturatingAdd 256 (historyPageLogicalBytes page)
    AppHistoryLoaded _ result ->
        saturatingAdd 256 $
            either logicalTextBytes historyPageLogicalBytes result
    AppHistoryCommitted _ turn _ ->
        saturatingAdd 256 (historyTurnLogicalBytes turn)
    AppHistoryLiveStarted -> 256
    AppConversationReflow -> 256
    AppSyncSubmittedImagePlacements -> 256
    AppMotionTick -> 256
    AppRecapPoll -> 256
    AppStop -> 256

opaqueAppEventLogicalBytes :: Int
opaqueAppEventLogicalBytes =
    saturatingAdd
        appEventMailboxPayloadBudgetBytes
        (appEventMailboxControlReserveBytes + 1)

toolCallLogicalBytes :: ToolCall -> Int
toolCallLogicalBytes call =
    saturatingAdd 256 $
        logicalTextsBytes [call.callId, call.name, call.arguments]

toolCallResultLogicalBytes :: ToolCallResult -> Int
toolCallResultLogicalBytes result =
    saturatingAdd 256 $
        saturatingAdd
            (logicalTextsBytes [result.callId, result.output])
            (foldl'
                (\size image ->
                    saturatingAdd size
                        (logicalTextsBytes
                            (image.imageUrl : maybeToList image.imageDetail)))
                0
                (toolCallResultImages result))

turnOutputLogicalBytes :: TurnOutput -> Int
turnOutputLogicalBytes output =
    saturatingAdd 512 $
        saturatingAdd
            (logicalTextsBytes
                (output.responseId : maybeToList output.assistantText))
            (saturatingAdd
                (foldl'
                    (\size call ->
                        saturatingAdd size (toolCallLogicalBytes call))
                    0
                    output.toolCalls)
                (case output.completion of
                    TurnCompleted -> 0
                    TurnIncomplete reason _ -> logicalTextBytes reason))

promptStateLogicalBytes :: PromptState -> Int
promptStateLogicalBytes prompt =
    saturatingAdd 256 $
        logicalTextsBytes
            ( [ prompt.promptModel
              , prompt.promptEffort
              , prompt.promptMode
              , prompt.promptAccount
              ]
                <> prompt.promptEffortOptions
                <> maybe
                    []
                    (pure . (.promptLimitText))
                    prompt.promptLimitStatus
            )

uiBlockLogicalBytes :: UiBlock -> Int
uiBlockLogicalBytes block =
    saturatingAdd 256 $
        logicalTextsBytes
            ( [ block.blockTitle
              , block.blockBody
              , block.blockTimestamp
              , block.blockDetail
              ]
                <> maybeToList block.blockCallId
            )

uiStateLogicalBytes :: UiState -> Int
uiStateLogicalBytes ui =
    foldl'
        saturatingAdd
        1024
        [ foldl'
            (\size block ->
                saturatingAdd size (uiBlockLogicalBytes block))
            0
            ui.uiBlocks
        , logicalTextBytes ui.uiDraft
        , logicalTextsBytes ui.uiQueuedInputs
        , logicalTextBytes ui.uiActivity
        , promptStateLogicalBytes ui.uiPrompt
        , logicalTextsBytes [ui.uiBranch, ui.uiCwd, ui.uiWorkspaceRoot]
        , maybe 0
            (logicalTextBytes . (.permissionSummary))
            ui.uiPermission
        , maybe 0 (logicalTextBytes . (.noticeText)) ui.uiNotice
        , maybe
            0
            (\retry ->
                logicalTextsBytes
                    [ retry.retryCountdownPrefix
                    , retry.retryCountdownSuffix
                    ])
            ui.uiRetryCountdown
        , Map.foldlWithKey'
            (\size callId (_, call) ->
                saturatingAdd size $
                    saturatingAdd
                        (logicalTextBytes callId)
                        (toolCallLogicalBytes call))
            0
            ui.uiToolCalls
        , Map.foldl'
            (\size group ->
                saturatingAdd size $
                    foldl'
                        (\groupSize item ->
                            saturatingAdd groupSize $
                                saturatingAdd 192 $
                                    logicalTextsBytes
                                        [ item.inspectionCallId
                                        , item.inspectionToolName
                                        , item.inspectionTitle
                                        , item.inspectionBody
                                        ])
                        128
                        group.inspectionGroupItems)
            0
            ui.uiInspectionGroups
        , foldl'
            (\size todo ->
                saturatingAdd size
                    (saturatingAdd
                        128
                        (logicalTextsBytes [todo.todoLineText])))
            0
            ui.uiTodos
        ]

agentEntryLogicalBytes :: AgentEntry -> Int
agentEntryLogicalBytes entry =
    foldl'
        saturatingAdd
        512
        [ agentTargetLogicalBytes entry.agentTarget
        , logicalTextsBytes
            (entry.agentPath : entry.agentStatus : entry.agentTranscript)
        , maybe 0 logicalTextBytes entry.agentModel
        , foldl'
            (\size step ->
                saturatingAdd size $
                    saturatingAdd 128 $
                        saturatingAdd
                            (logicalTextBytes step.agentStepTitle)
                            (maybe 0 logicalTextBytes step.agentStepDetail))
            0
            entry.agentSteps
        , uiStateLogicalBytes entry.agentConversation
        ]

historyTurnLogicalBytes :: HistoryTurn -> Int
historyTurnLogicalBytes turn =
    saturatingAdd 256 $
        foldl'
            (\size block ->
                saturatingAdd size (uiBlockLogicalBytes block))
            0
            turn.historyTurnBlocks

historyPageLogicalBytes :: HistoryPage -> Int
historyPageLogicalBytes page =
    saturatingAdd 512 $
        foldl'
            (\size turn ->
                saturatingAdd size (historyTurnLogicalBytes turn))
            0
            page.historyPageTurns

resumeBrowserLogicalBytes :: ResumeBrowser -> Int
resumeBrowserLogicalBytes browser =
    foldl'
        saturatingAdd
        512
        [ logicalTextBytes browser.resumeBrowserQuery
        , maybe 0 logicalTextBytes browser.resumeBrowserAppliedQuery
        , maybe 0 logicalTextBytes browser.resumeBrowserExpanded
        , maybe 0 logicalTextBytes browser.resumeBrowserDeletePending
        , maybe 0 logicalTextBytes browser.resumeBrowserNotice
        , foldl'
            (\size entry ->
                saturatingAdd size (resumeEntryLogicalBytes entry))
            0
            browser.resumeBrowserAll
        ]

resumeEntryLogicalBytes :: ResumeEntry -> Int
resumeEntryLogicalBytes entry =
    saturatingAdd 512 $
        logicalTextsBytes
            ( [ entry.resumeId
              , entry.resumeTitle
              , entry.resumeModel
              , entry.resumeCwd
              , entry.resumeProject
              , entry.resumeWhen
              , entry.resumeProvider
              , entry.resumePrompt
              ]
                <> maybeToList entry.resumeRecap
                <> maybeToList entry.resumeLastTurnSummary
                <> maybeToList entry.resumeMatch
                <> entry.resumeTranscript
            )

textRowsLogicalBytes :: [(Text, Text)] -> Int
textRowsLogicalBytes =
    foldl'
        (\size (title, detail) ->
            saturatingAdd size (logicalTextsBytes [title, detail]))
        0

agentTargetLogicalBytes :: AgentTarget -> Int
agentTargetLogicalBytes = \case
    AgentRoot -> 64
    AgentChild (SubagentId identifier) -> logicalTextChunkBytes identifier
    AgentNative identifier -> logicalTextChunkBytes identifier

skillCommandLogicalBytes :: SkillCommand -> Int
skillCommandLogicalBytes skill =
    saturatingAdd 256 $
        logicalTextsBytes
            ( [ skill.skillCommandName
              , skill.skillCommandSummary
              , skill.skillCommandSource
              ]
                <> maybeToList skill.skillCommandArgumentHint
            )

slashCommandLogicalBytes :: SlashCommand -> Int
slashCommandLogicalBytes command =
    saturatingAdd 256 $
        logicalTextsBytes
            ( [ command.slashName
              , command.slashUsage
              , command.slashSummary
              ]
                <> command.slashAliases
                <> command.slashRequiredTools
            )

slashCatalogLogicalBytes :: SlashCatalog -> Int
slashCatalogLogicalBytes catalog =
    foldl'
        saturatingAdd
        512
        [ logicalTextsBytes catalog.slashCatalogToolNames
        , foldl'
            (\size command ->
                saturatingAdd size (slashCommandLogicalBytes command))
            0
            catalog.slashCatalogCommands
        , foldl'
            (\size skill ->
                saturatingAdd size (skillCommandLogicalBytes skill))
            0
            catalog.slashCatalogSkills
        , logicalTextsBytes catalog.slashCatalogModelIds
        ]

imagePreviewPairsLogicalBytes
    :: [(ImageAttachment, TuiImagePreview)]
    -> Int
imagePreviewPairsLogicalBytes =
    foldl'
        (\size pair ->
            saturatingAdd size (imagePreviewPairLogicalBytes pair))
        0

imagePreviewPairLogicalBytes
    :: (ImageAttachment, TuiImagePreview)
    -> Int
imagePreviewPairLogicalBytes (attachment, preview) =
    saturatingAdd
        (imageAttachmentLogicalBytes attachment)
        (imagePreviewLogicalBytes preview)

imageAttachmentLogicalBytes :: ImageAttachment -> Int
imageAttachmentLogicalBytes ImageAttachment{imageMime, imageBytes} =
    saturatingAdd
        (logicalTextBytes imageMime)
        (BS.length imageBytes)

-- The sampled ANSI bitmap is capped at 96x64 RGB pixels. Charge a rounded
-- overhead for it without forcing the lazy sample, plus the larger of the
-- source-size estimate and the actual Kitty payload retained by the preview.
imagePreviewLogicalBytes :: TuiImagePreview -> Int
imagePreviewLogicalBytes preview =
    saturatingAdd
        (32 * 1024)
        (saturatingAdd
            (logicalTextBytes preview.previewMime)
            (max
                (max 0 preview.previewBytes)
                (BS.length
                    preview.previewKittyAttachment.imageBytes)))

logicalTextsBytes :: Foldable f => f Text -> Int
logicalTextsBytes =
    foldl'
        (\size text ->
            saturatingAdd size $
                saturatingAdd 64 (logicalTextBytes text))
        0

logicalTextBytes :: Text -> Int
logicalTextBytes text
    | Text.length text >= maxBound `div` 4 = maxBound
    | otherwise = Text.length text * 4

logicalTextChunkBytes :: Text -> Int
logicalTextChunkBytes text =
    saturatingAdd 64 (logicalTextBytes text)

saturatingAdd :: Int -> Int -> Int
saturatingAdd left right
    | right > maxBound - left = maxBound
    | otherwise = left + right

isControlAppEvent :: AppEvent -> Bool
isControlAppEvent = \case
    AppUi (UiLoop event) ->
        case event of
            TextDelta _ -> False
            ReasoningDelta _ -> False
            ActivityUpdated _ -> False
            ToolUpdated _ -> False
            ToolOutputUpdated _ _ -> False
            _ -> True
    AppAgentSnapshot _ _ -> False
    AppSetWindowTitle _ -> False
    AppDictationPartial _ -> False
    AppConversationReflow -> False
    AppSyncSubmittedImagePlacements -> False
    AppMotionTick -> False
    AppRecapPoll -> False
    AppSyntaxHighlighterChanged -> False
    _ -> True
