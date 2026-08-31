{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -O0 -Wno-unused-imports #-}
module Agent.CLI.TUI.App.Reduce where

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
import Agent.CLI.TUI.App.Runtime
import Agent.CLI.TUI.App.Mailbox
import Agent.CLI.TUI.App.History

handleUiEvents :: NonEmpty UiEvent -> EventM Name AppState ()
handleUiEvents uiEvents = do
    stored <- get
    viewportBounds <-
        if stored.appAgentSelected == AgentRoot
            then
                lookupViewport ConversationViewport >>= \case
                    Just (VP _ top (_, height) (_, contentHeight)) ->
                        pure (Just (top, height, contentHeight))
                    Nothing ->
                        pure Nothing
            else pure Nothing
    let reconciledFollow =
            if stored.appAgentSelected == AgentRoot
                then
                    Scroll.reconcileConversationFollow
                        stored.appUi.uiFollow
                        viewportBounds
                else stored.appUi.uiFollow
        initial =
            stored
                { appUi =
                    stored.appUi
                        { uiFollow = reconciledFollow
                        }
                }
    timestamp <- liftIO currentShortMessageTimestamp
    renderedContentHeight <-
        if any isSubmittedPrompt uiEvents
            then conversationUnpaddedContentHeight
            else pure 0
    let
        (final, nativeProgress, shouldFollow, shouldInvalidate) =
            foldl'
                (applyOne timestamp renderedContentHeight)
                (initial, Nothing, False, False)
                uiEvents
    put final
    when (any (== UiConversationCleared) uiEvents) $
        clearSubmittedImagePlacements final.appRuntime
    case nativeProgress of
        Nothing -> pure ()
        Just active ->
            liftIO (final.appRuntime.runtimeNativeProgress active)
    when shouldInvalidate invalidateCache
    when shouldFollow $
        case final.appConversationAnchor of
            Just _ -> do
                when (any isSubmittedPrompt uiEvents) $
                    vScrollToEnd (viewportScroll ConversationViewport)
                queueConversationReflow
            Nothing ->
                vScrollToEnd (viewportScroll ConversationViewport)
  where
    applyOne
        timestamp
        renderedContentHeight
        (state, previousProgress, followed, invalidated)
        uiEvent =
            let
                unstamped =
                    applyUiEvent uiEvent $
                        applyConversationUiEvent
                            renderedContentHeight
                            uiEvent
                            state
                next =
                    unstamped
                        { appUi =
                            timestampNewMessageBlocks
                                (Seq.length state.appUi.uiBlocks)
                                timestamp
                                unstamped.appUi
                        }
                progress =
                    case Bridge.nativeProgressSignal
                        (userActionPending next)
                        uiEvent
                        next.appUi of
                        Nothing -> previousProgress
                        signal -> signal
                follows =
                    followed
                        || (Bridge.eventFollows uiEvent
                            && next.appUi.uiFollow)
                invalidates =
                    invalidated || uiEvent == UiConversationCleared
            in (next, progress, follows, invalidates)

applyConversationUiEvent :: Int -> UiEvent -> AppState -> AppState
applyConversationUiEvent renderedContentHeight uiEvent state =
    case uiEvent of
        UiUserSubmitted text ->
            state
                { appConversationAnchor =
                    Just $
                        Scroll.startConversationAnchor
                            (BlockId state.appUi.uiNextBlockId)
                            text
                            (if null state.appUi.uiBlocks
                                then 0
                                else renderedContentHeight)
                , appHistoryLiveStart =
                    case state.appHistoryLiveStart of
                        Just start -> Just start
                        Nothing -> Just (Seq.length state.appUi.uiBlocks)
                }
        UiConversationCleared ->
            state
                { appConversationAnchor = Nothing
                , appConversationReflowQueued = False
                , appHistoryWindow =
                    emptyHistoryWindow
                        state.appHistoryWindow.historyWindowGeneration
                        historyWindowTurnBudget
                        historyWindowBlockBudget
                        historyWindowByteBudget
                , appHistorySelectedBlock = Nothing
                , appHistoryLiveStart = Nothing
                , appNextHistoryBlockId = -1
                , appSubmittedImagePreviews = Map.empty
                }
        _ -> state

applyUiEvent :: UiEvent -> AppState -> AppState
applyUiEvent uiEvent state =
    let
        previousUi = state.appUi
        nextUi = reduceUi uiEvent previousUi
        retainedFlashes =
            case uiEvent of
                UiConversationCleared ->
                    Map.empty
                UiTurnRestarted ->
                    retainExistingFlashes
                        nextUi
                        state.appCompletionFlashes
                _ ->
                    state.appCompletionFlashes
        transitionIds
            | uiEventCanCompleteBlocks uiEvent =
                completionFlashTransitions previousUi nextUi
            | otherwise =
                []
        newFlashes =
            if state.appRuntime.runtimeMotionMode == MotionOff
                then Map.empty
                else Map.fromList
                    [ (blockId, completionFlashDurationMillis)
                    | blockId <- transitionIds
                    ]
        restartSchedule =
            uiEventRestartsMotionSchedule
                uiEvent
                previousUi
                nextUi
                newFlashes
        nextState0 =
            state
                { appUi = nextUi
                , appAutoRecapShownThisAway =
                    case uiEvent of
                        UiRecapReady _ -> True
                        _ -> state.appAutoRecapShownThisAway
                , appLastTurnCompletedAt =
                    case uiEvent of
                        UiLoop (TurnFinished _) -> Just state.appClockNanos
                        UiTurnEnded BlockComplete -> Just state.appClockNanos
                        _ -> state.appLastTurnCompletedAt
                , appCompletionFlashes =
                    Map.union newFlashes retainedFlashes
                , appMotionScheduleReset =
                    state.appMotionScheduleReset || restartSchedule
                , appNativeProgressKeepaliveBucket =
                    if nextUi.uiElapsedMillis < previousUi.uiElapsedMillis
                        then 0
                        else state.appNativeProgressKeepaliveBucket
                }
        nextState =
            case state.appChoice of
                Just choice
                    | choiceClosesOnUiTransition
                        previousUi
                        nextUi
                        choice ->
                        nextState0
                            { appChoice = Nothing
                            , appChoiceReply = Nothing
                            }
                _ -> nextState0
    in Composer.applyComposerUiEvent uiEvent nextState

-- | Turn-scoped choices, such as the live effort selector, become invalid
-- when their turn stops running. Ordinary idle dialogs remain open, and a
-- model round that continues into tools keeps the selector visible.
choiceClosesOnUiTransition
    :: UiState
    -> UiState
    -> ChoiceOverlay
    -> Bool
choiceClosesOnUiTransition previous next choice =
    choice.choiceCloseOnTurnEnd
        && previous.uiRunning
        && not next.uiRunning

retainExistingFlashes
    :: UiState
    -> Map.Map BlockId Int
    -> Map.Map BlockId Int
retainExistingFlashes ui =
    Map.filterWithKey
        (\blockId _ ->
            any ((== blockId) . (.blockId))
                (toList ui.uiBlocks))

uiEventCanCompleteBlocks :: UiEvent -> Bool
uiEventCanCompleteBlocks = \case
    UiLoop (ToolFinished _) -> True
    UiLoop (TurnFinished _) -> True
    UiLoop (ResponseRestarted _) -> True
    UiSetAwaitingInput True -> True
    UiTurnEnded _ -> True
    _ -> False

advanceAppTime :: Word64 -> AppState -> AppState
advanceAppTime now state =
    let
        (elapsedMillis, nextClock) =
            elapsedMillisSince state.appClockNanos now
    in state
        { appUi = advanceUiTime elapsedMillis state.appUi
        , appMotionElapsedMillis =
            state.appMotionElapsedMillis + elapsedMillis
        , appCompletionFlashes =
            advanceCompletionFlashes
                elapsedMillis
                state.appCompletionFlashes
        , appClockNanos = nextClock
        }

advanceAppClockNow :: EventM Name AppState ()
advanceAppClockNow = do
    now <- liftIO getMonotonicTimeNSec
    modify' (advanceAppTime now)

noteTerminalFocusLost :: EventM Name AppState ()
noteTerminalFocusLost = do
    now <- liftIO getMonotonicTimeNSec
    state <- get
    liftIO $
        atomicModifyIORef'
            state.appRuntime.runtimeSyntaxHighlighter
            \syntaxState ->
                ( SyntaxHighlighterInactive
                    (syntaxHighlighterGeneration syntaxState + 1)
                , ()
                )
    liftIO $ atomically $ void $ flushTQueue
        state.appRuntime.runtimeSyntaxRequests
    modify' \state ->
        state
            { appTerminalFocus = TerminalUnfocused
            , appFocusLostAt = Just now
            , appAutoRecapShownThisAway = False
            , appLastAutoRecapAttemptAt = Nothing
            , appSyntaxHighlighter = Nothing
            , appSyntaxRequested = Set.empty
            }
    invalidateCache

noteTerminalFocusGained :: EventM Name AppState ()
noteTerminalFocusGained = do
    maybeRequestAutoRecap
    state <- get
    liftIO $
        atomicModifyIORef'
            state.appRuntime.runtimeSyntaxHighlighter
            \case
                SyntaxHighlighterInactive generation ->
                    (SyntaxHighlighterUnloaded generation, ())
                active ->
                    (active, ())
    modify' \state ->
        state
            { appTerminalFocus = TerminalFocused
            , appFocusLostAt = Nothing
            , appMotionScheduleReset = True
            , appSyntaxRequested = Set.empty
            }
    requestVisibleSyntaxLanguages
    invalidateCache
    getVtyHandle >>= liftIO . V.refresh

maybeRequestAutoRecap :: EventM Name AppState ()
maybeRequestAutoRecap = do
    now <- liftIO getMonotonicTimeNSec
    state <- get
    when (shouldRequestAutoRecap now state) do
        modify' \current ->
            current { appLastAutoRecapAttemptAt = Just now }
        liftIO state.appRuntime.runtimeRecap

shouldRequestAutoRecap :: Word64 -> AppState -> Bool
shouldRequestAutoRecap now state =
    state.appTerminalFocus == TerminalUnfocused
        && not state.appAutoRecapShownThisAway
        && not (userActionPending state)
        && not state.appUi.uiRunning
        && not (hasBackgroundActivity state.appAgentEntries)
        && elapsedSeconds now state.appFocusLostAt >= autoRecapAwayThreshold
        && elapsedSeconds now state.appLastTurnCompletedAt
            >= autoRecapIdleThreshold
        && ( case state.appLastAutoRecapAttemptAt of
                Nothing -> True
                Just attempted ->
                    elapsedSeconds now (Just attempted)
                        >= autoRecapRetryInterval
           )

elapsedSeconds :: Word64 -> Maybe Word64 -> NominalDiffTime
elapsedSeconds _ Nothing = 0
elapsedSeconds now (Just started) =
    realToFrac (now - started) / 1_000_000_000

applyLocalUiEvent :: UiEvent -> EventM Name AppState ()
applyLocalUiEvent event =
    applyLocalUiEventWith event id

applyLocalUiEventWith
    :: UiEvent
    -> (AppState -> AppState)
    -> EventM Name AppState ()
applyLocalUiEventWith event update = do
    advanceAppClockNow
    modify' (update . applyUiEvent event)

refreshNativeProgressKeepalive :: EventM Name AppState ()
refreshNativeProgressKeepalive = do
    state <- get
    let bucket = state.appUi.uiElapsedMillis `div` 5000
    when
        (nativeProgressKeepaliveDue
            (userActionPending state)
            state.appNativeProgressKeepaliveBucket
            state.appUi) do
        liftIO (state.appRuntime.runtimeNativeProgress True)
        modify' \current ->
            current { appNativeProgressKeepaliveBucket = bucket }

eventMayExposeSyntax :: BrickEvent Name AppEvent -> Bool
eventMayExposeSyntax = \case
    AppEvent (AppUi uiEvent) ->
        uiEventMayExposeSyntax uiEvent
    AppEvent (AppUiBatch uiEvents) ->
        any uiEventMayExposeSyntax uiEvents
    AppEvent AppSyntaxHighlighterChanged -> True
    AppEvent (AppHistoryReset _) -> True
    AppEvent (AppHistoryLoaded _ _) -> True
    AppEvent (AppHistoryCommitted _ _ _) -> True
    AppEvent AppHistoryLiveStarted -> True
    AppEvent (AppAgentSnapshot _ _) -> True
    _ -> False

uiEventMayExposeSyntax :: UiEvent -> Bool
uiEventMayExposeSyntax = \case
    UiLoop (TextDelta delta) ->
        Text.isInfixOf "```" delta
            || Text.isInfixOf "~~~" delta
    UiLoop (ReasoningDelta _) -> False
    UiLoop (ActivityUpdated _) -> False
    UiLoop (WarningRaised _) -> False
    UiLoop (ToolOutputUpdated _ _) -> False
    UiSetDraft _ _ -> False
    UiSetPrompt _ -> False
    UiSetPromptTarget _ _ -> False
    UiSetPromptEffort _ -> False
    UiSetPromptLimitStatus _ -> False
    UiSetContextUsage _ _ -> False
    UiSetAwaitingInput _ -> False
    UiSetRepository _ _ _ -> False
    UiSetNotice _ -> False
    UiMoveSelection _ -> False
    UiSelectBlock _ -> False
    UiActivateBlock _ -> False
    UiToggleSelected -> False
    UiFocusChanged _ -> False
    UiPermissionShown _ -> False
    UiPermissionMoved _ -> False
    UiPermissionHidden -> False
    UiSetFollow _ -> False
    _ -> True

requestVisibleSyntaxLanguages :: EventM Name AppState ()
requestVisibleSyntaxLanguages = do
    state <- get
    let
        languages =
            syntaxLanguagesForBlocks (visibleConversationBlocks state)
        missing =
            Set.difference languages state.appSyntaxRequested
    when
        ( state.appTerminalFocus /= TerminalUnfocused
            && not (Set.null missing)
        ) do
        liftIO $
            atomically $
                mapM_
                    (writeTQueue state.appRuntime.runtimeSyntaxRequests)
                    (Set.toList missing)
        modify' \current ->
            current
                { appSyntaxRequested =
                    Set.union current.appSyntaxRequested missing
                }

visibleConversationBlocks :: AppState -> [UiBlock]
visibleConversationBlocks state =
    conversationBlocks state.appAgentSelected state

syntaxLanguagesForBlocks :: [UiBlock] -> Set.Set Text
syntaxLanguagesForBlocks =
    Set.fromList . concatMap syntaxLanguagesForBlock

syntaxLanguagesForBlock :: UiBlock -> [Text]
syntaxLanguagesForBlock block =
    case block.blockKind of
        BlockAssistant ->
            mapMaybe
                (resolveFenceLanguage . (.fencedInfo))
                (fencedBlocks block.blockBody)
        BlockShell
            | Just language <- blockCodeLanguage block ->
                [language]
        _ -> []

resumeNativeProgressIfRunning :: EventM Name AppState ()
resumeNativeProgressIfRunning = do
    state <- get
    when
        (state.appUi.uiRunning
            && not (userActionPending state)) do
        liftIO (state.appRuntime.runtimeNativeProgress True)
        modify' \current ->
            current
                { appNativeProgressKeepaliveBucket =
                    current.appUi.uiElapsedMillis `div` 5000
                }


conversationUnpaddedContentHeight :: EventM Name AppState Int
conversationUnpaddedContentHeight =
    lookupViewport ConversationViewport >>= \case
        Just (VP _ _ _ (_, contentHeight)) -> do
            reserveRows <- lookupExtent ConversationReserve >>= \case
                Just (Extent _ _ (_, rows)) -> pure rows
                Nothing -> pure 0
            pure (max 0 (contentHeight - reserveRows))
        Nothing -> pure 0

queueConversationReflow :: EventM Name AppState ()
queueConversationReflow = do
    state <- get
    unless state.appConversationReflowQueued do
        modify' \current -> current { appConversationReflowQueued = True }
        liftIO $ enqueueAppEvent state.appRuntime AppConversationReflow

clearSubmittedImagePlacements :: FullscreenRuntime -> EventM Name AppState ()
clearSubmittedImagePlacements runtime = do
    previous <- liftIO $ readIORef runtime.runtimeSubmittedImagePlacements
    when (not (null previous)) $
        liftIO do
            writeIORef runtime.runtimeSubmittedImagePlacements []
            modifyIORef' runtime.runtimeImagePreviewRevision (+ 1)

isSubmittedPrompt :: UiEvent -> Bool
isSubmittedPrompt = \case
    UiUserSubmitted _ -> True
    _ -> False

conversationBlocks :: AgentTarget -> AppState -> [UiBlock]
conversationBlocks target state =
    case target of
        AgentRoot ->
            concatMap (toList . (.historyTurnBlocks))
                (toList state.appHistoryWindow.historyWindowTurns)
                <> toList state.appUi.uiBlocks
        AgentChild _ ->
            maybe [] (toList . (.uiBlocks)) (conversationUiForTarget target state)
        AgentNative _ ->
            maybe [] (toList . (.uiBlocks)) (conversationUiForTarget target state)

-- | Resolve the transcript block that carries an agent-displayed image.
-- Nested code-mode calls run under synthetic @code-mode:@ call ids that never
-- own a block, so they attach to the newest in-flight tool block: the exec
-- cell that invoked them.
toolImageBlockId :: Text -> UiState -> Maybe BlockId
toolImageBlockId callId ui =
    case blockForCall callId of
        Just block -> Just block.blockId
        Nothing ->
            case Seq.findIndexR ((== Just callId) . (.blockCallId)) ui.uiBlocks of
                Just index -> (.blockId) <$> Seq.lookup index ui.uiBlocks
                Nothing -> newestRunningTool
  where
    blockForCall wanted = do
        (blockIndex, _) <- Map.lookup wanted ui.uiToolCalls
        block <- Seq.lookup blockIndex ui.uiBlocks
        if block.blockCallId == Just wanted
            then Just block
            else Nothing
    newestRunningTool =
        case
            [ block.blockId
            | (active, _) <- Map.toList ui.uiToolCalls
            , Just block <- [blockForCall active]
            , block.blockState == BlockRunning
            , block.blockKind `elem` [BlockTool, BlockInspect, BlockShell]
            ]
        of
            [] -> Nothing
            candidates -> Just (maximum candidates)
