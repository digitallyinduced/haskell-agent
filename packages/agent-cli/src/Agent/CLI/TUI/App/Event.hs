{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -O0 -Wno-unused-imports #-}
module Agent.CLI.TUI.App.Event where

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
import Agent.CLI.TUI.Motion ( advanceCompletionFlashes , appMotionTiming , completionFlashTransitions , completionRequiresRedraw , elapsedMillisSince , hasBackgroundActivity , isBackgroundAgentActive , motionDemandFor , motionDemandForTerminalFocus , motionModeForTerminalFocus , nativeProgressKeepaliveDue , nextMotionSchedule , uiEventRestartsMotionSchedule , userActionPending )
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
import Agent.CLI.TUI.App.Reduce hiding (queueConversationReflow)
import Agent.CLI.TUI.App.Overlay hiding (handleCtrlC)
import Agent.CLI.TUI.App.Navigation hiding (handleCtrlC)

fullscreenApp :: App AppState AppEvent Name
fullscreenApp = App
    { appDraw = drawApp
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = do
        state <- get
        liftIO state.appRuntime.runtimeFirstFrame
        vScrollToEnd (viewportScroll ConversationViewport)
    , appAttrMap = \state ->
        if state.appRuntime.runtimeColor
            then Theme.terminalDefault
            else Theme.monochrome
    }

handleEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleEvent event = do
    advanceAppClockNow
    stateBeforeEvent <- get
    when (isMotionTick event) refreshNativeProgressKeepalive
    handleEventInner event
    when (eventMayExposeSyntax event) requestVisibleSyntaxLanguages
    state <- get
    let visible =
            isNothing state.appTextPrompt
                && isNothing state.appChoice
                && isNothing state.appResume
                && isNothing state.appUi.uiPermission
                && isNothing state.appAgentHover
    liftIO do
        previous <-
            readIORef state.appRuntime.runtimeImagePreviewVisible
        when (previous /= visible) do
            writeIORef
                state.appRuntime.runtimeImagePreviewVisible
                visible
            modifyIORef'
                state.appRuntime.runtimeImagePreviewRevision
                (+ 1)
    syncMotionDemand
    stateAfterMotionSync <- get
    when
        ( stateAfterMotionSync.appTerminalFocus == TerminalUnfocused
            && not
                (completionRequiresRedraw
                    stateBeforeEvent.appUi
                    stateBeforeEvent.appAgentEntries
                    stateAfterMotionSync.appUi
                    stateAfterMotionSync.appAgentEntries)
        ) $
        continueWithoutRedraw
  where
    isMotionTick = \case
        AppEvent AppMotionTick -> True
        _ -> False

syncMotionDemand :: EventM Name AppState ()
syncMotionDemand = do
    advanceAppClockNow
    state <- get
    let
        (demand, delayMicros) = appMotionTiming state
        resetSchedule = state.appMotionScheduleReset
    liftIO $
        atomically do
            current <-
                readTVar state.appRuntime.runtimeMotionSchedule
            let next =
                    nextMotionSchedule
                        resetSchedule
                        demand
                        delayMicros
                        current
            when (next /= current) $
                writeTVar
                    state.appRuntime.runtimeMotionSchedule
                    next
    modify' \current ->
        current
            { appMotionScheduleReset = False }

handleEventInner :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleEventInner event = case event of
    AppEvent AppMotionTick -> do
        state <- get
        liftIO $
            atomically $
                writeTVar
                    state.appRuntime.runtimeMotionTickQueued
                    False
    AppEvent AppRecapPoll ->
        maybeRequestAutoRecap
    AppEvent AppStop -> do
        state <- get
        liftIO $
            mapM_
                (`Composer.requestDictationStop` True)
                state.appDictation
        modify' \current ->
            current
                { appWorkerStopped = True
                , appDictation = Nothing
                }
        halt
    AppEvent (AppSetSlashCatalog catalog) -> do
        state <- get
        if state.appSlashCatalog == catalog
            then pure ()
            else modify' \current -> current
                { appSlashCatalog = catalog
                , appSlashIndex = 0
                , appSlashDismissed = False
                }
    AppEvent (AppSetSkillCommands skills) -> do
        state <- get
        if state.appSlashCatalog.slashCatalogSkills == skills
            then pure ()
            else
                let catalog =
                        slashCatalogWithSkills skills state.appSlashCatalog
                in modify' \current -> current
                    { appSlashCatalog = catalog
                    , appSlashIndex = 0
                    , appSlashDismissed = False
                    }
    AppEvent (AppSetModelIds modelIds) -> do
        state <- get
        if state.appSlashCatalog.slashCatalogModelIds == modelIds
            then pure ()
            else
                let catalog =
                        state.appSlashCatalog
                            { slashCatalogModelIds = modelIds
                            }
                in modify' \current -> current
                    { appSlashCatalog = catalog
                    , appSlashIndex = 0
                    , appSlashDismissed = False
                    }
    AppEvent (AppSetImagePreviews prepared) ->
        do
            state <- get
            previous <-
                liftIO $
                    readIORef state.appRuntime.runtimeImagePreviews
            let unchanged = map fst previous == map fst prepared
            liftIO do
                when (not unchanged) do
                    writeIORef
                        state.appRuntime.runtimeImagePreviews
                        prepared
                    modifyIORef'
                        state.appRuntime.runtimeImagePreviewRevision
                        (+ 1)
            modify' \current ->
                current
                    { appImagePreviews = map snd prepared
                    }
    AppEvent (AppCommitImagePreviews prepared) -> do
        state <- get
        let previews = map snd prepared
            nextBlockId = BlockId state.appUi.uiNextBlockId
            submitted =
                if null previews
                    then Map.delete
                        nextBlockId
                        state.appSubmittedImagePreviews
                    else Map.insert
                        nextBlockId
                        previews
                        state.appSubmittedImagePreviews
        liftIO do
            writeIORef state.appRuntime.runtimeImagePreviews []
            modifyIORef'
                state.appRuntime.runtimeImagePreviewRevision
                (+ 1)
        modify' \current ->
            current
                { appImagePreviews = []
                , appSubmittedImagePreviews = submitted
                }
        queueConversationReflow
    AppEvent (AppDictationPartial text) -> do
        state <- get
        when (isJust state.appDictation) $
            applyLocalUiEvent
                (UiSetNotice (Just (Composer.dictationProgressNotice text)))
    AppEvent (AppDictationFinished result) -> do
        state <- get
        aborted <-
            case state.appDictation of
                Just session ->
                    liftIO (readIORef session.dictationAbort)
                Nothing ->
                    pure False
        modify' \current -> current { appDictation = Nothing }
        if aborted
            then applyLocalUiEvent $
                UiSetNotice $
                    Just (infoNotice "Dictation cancelled.")
            else case result of
                Left message ->
                    applyLocalUiEvent $
                        UiSetNotice $
                            Just $
                                warningNotice ("Dictation failed: " <> message)
                Right transcript -> do
                    let ui = state.appUi
                        (draft, cursor) =
                            insertDictation ui.uiDraft ui.uiCursor transcript
                    applyLocalUiEvent (UiSetDraft draft cursor)
                    applyLocalUiEvent $
                        UiSetNotice $
                            Just $
                                successNotice "Dictation inserted."
    AppEvent (AppSetWindowTitle title) -> do
        vty <- getVtyHandle
        liftIO (writeOutputWindowTitle (V.outputIface vty) title)
        modify' \current -> current { appWindowTitle = Just title }
    AppEvent AppSyntaxHighlighterChanged -> do
        state <- get
        when (state.appTerminalFocus /= TerminalUnfocused) do
            highlighter <-
                liftIO $
                    readIORef state.appRuntime.runtimeSyntaxHighlighter
            modify' \current ->
                current
                    { appSyntaxHighlighter =
                        case highlighter of
                            SyntaxHighlighterActive _ loaded -> loaded
                            SyntaxHighlighterUnloaded _ -> Nothing
                            SyntaxHighlighterInactive _ -> Nothing
                    }
            invalidateCache
    AppEvent (AppHistoryReset page) -> do
        modify' (resetHistoryPage page)
        invalidateCache
        queueConversationReflow
    AppEvent (AppHistoryLoaded request result) -> do
        state <- get
        when
            (request.historyRequestGeneration
                == state.appHistoryWindow.historyWindowGeneration)
            do
                let anchorBlock =
                        historyPageAnchorBlock
                            request.historyRequestDirection
                            state.appHistoryWindow
                case result of
                    Left err ->
                        modify' \current ->
                            applyUiEvent
                                (UiSetNotice
                                    (Just (warningNotice
                                        ("Could not load session history: "
                                            <> err))))
                                (clearHistoryPending request current)
                    Right page ->
                        modify' (applyLoadedHistoryPage page)
                invalidateCache
                case anchorBlock of
                    Nothing -> pure ()
                    Just blockId ->
                        makeVisible
                            (ConversationBlock AgentRoot blockId)
                queueConversationReflow
    AppEvent AppHistoryLiveStarted ->
        modify' \state ->
            state
                { appHistoryLiveStart =
                    case state.appHistoryLiveStart of
                        Just start -> Just start
                        Nothing ->
                            Just (Seq.length state.appUi.uiBlocks)
                }
    AppEvent (AppHistoryCommitted generation turn commit) -> do
        state <- get
        let currentGeneration =
                state.appHistoryWindow.historyWindowGeneration
            applicable = case commit of
                HistoryCommitAppend ->
                    currentGeneration == generation
                _ ->
                    currentGeneration < generation
        when applicable do
            modify'
                (commitLiveHistoryTurn turn commit
                    . setHistoryGeneration generation)
            invalidateCache
            resolveConversationFollow
            queueConversationReflow
    AppEvent (AppAgentSnapshot selected entries) -> do
        state <- get
        let normalized =
                Bridge.normalizeAgentSelection selected entries
            mergedEntries =
                preserveAgentConversationView
                    normalized
                    state.appAgentEntries
                    entries
            selectionChanged =
                state.appAgentSelected /= normalized
            selectedConversationChanged =
                case normalized of
                    AgentRoot -> False
                    target ->
                        fmap (.agentConversation)
                            (lookupAgentEntry target state.appAgentEntries)
                            /= fmap (.agentConversation)
                                (lookupAgentEntry target mergedEntries)
        if state.appAgentSelected == normalized
            && state.appAgentEntries == mergedEntries
            then pure ()
            else do
                modify' \current ->
                    current
                        { appAgentSelected = normalized
                        , appAgentEntries = mergedEntries
                        , appAgentHover =
                            if normalized /= current.appAgentSelected
                                || length entries <= 1
                                || agentLayoutTargets mergedEntries
                                    /= agentLayoutTargets
                                        current.appAgentEntries
                                then Nothing
                                else
                                    current.appAgentHover >>= \hover ->
                                        hover <$
                                            lookupAgentEntry
                                                hover.agentHoverTarget
                                                mergedEntries
                        }
                when selectedConversationChanged invalidateCache
                if selectionChanged
                    then resumeConversationFollow
                    else when
                        (selectedConversationChanged
                            && state.appUi.uiFollow)
                        do
                            vScrollToEnd
                                (viewportScroll ConversationViewport)
                            queueConversationReflow
                when
                    ((length state.appAgentEntries > 1)
                        /= (length mergedEntries > 1))
                    do
                        invalidateCache
                        queueConversationReflow
    AppEvent (AppUi uiEvent) ->
        handleUiEvents (uiEvent :| [])
    AppEvent (AppUiBatch uiEvents) ->
        handleUiEvents uiEvents
    AppEvent AppConversationReflow -> do
        modify' \state ->
            state { appConversationReflowQueued = False }
        reflowConversation
        state <- get
        liftIO $
            enqueueAppEvent
                state.appRuntime
                AppSyncSubmittedImagePlacements
    AppEvent AppSyncSubmittedImagePlacements ->
        syncSubmittedImagePlacements
    AppEvent (AppAskPermission summary reply) -> do
        state <- get
        liftIO (state.appRuntime.runtimeNativeProgress False)
        applyLocalUiEventWith
            (UiPermissionShown summary)
            \current ->
                current
                    { appPermissionReply = Just reply
                    , appAgentHover = Nothing
                    }
    AppEvent (AppAskChoice presentation title body initial rows reply) -> do
        state <- get
        liftIO (state.appRuntime.runtimeNativeProgress False)
        modify' \state ->
            state
                { appChoice = Just ChoiceOverlay
                    { choicePresentation = presentation
                    , choiceTitle = title
                    , choiceBody = body
                    , choiceIndex =
                        max 0 (min (max 0 (length rows - 1)) initial)
                    , choiceRows = rows
                    , choiceCloseOnTurnEnd = False
                    }
                , appChoiceReply = Just (atomically . putTMVar reply)
                , appAgentHover = Nothing
                }
        vScrollToBeginning (viewportScroll OverlayViewport)
    AppEvent
        (AppAskResume browser loadEntry deleteEntry searchEntries reply) -> do
        state <- get
        liftIO (state.appRuntime.runtimeNativeProgress False)
        modify' \state ->
            state
                { appResume = Just ResumeOverlay
                    { resumeOverlayBrowser = browser
                    }
                , appResumeReply = Just reply
                , appResumeLoad = Just loadEntry
                , appResumeDelete = Just deleteEntry
                , appResumeSearch = Just searchEntries
                , appAgentHover = Nothing
                }
        vScrollToBeginning (viewportScroll ResumeViewport)
    AppEvent (AppAskText mode title body initial reply) -> do
        state <- get
        liftIO (state.appRuntime.runtimeNativeProgress False)
        modify' \state ->
            state
                { appTextPrompt = Just TextOverlay
                    { textTitle = title
                    , textBody = body
                    , textDraft = initial
                    , textCursor = Text.length initial
                    , textInputMode = mode
                    }
                , appTextReply = Just reply
                , appAgentHover = Nothing
                }
        vScrollToBeginning (viewportScroll OverlayViewport)
    AppEvent (AppSuspend action reply) -> do
        state <- get
        suspendAndResume do
            result <- tryAny action
            mapM_
                (setFullscreenWindowTitle state.appRuntime)
                state.appWindowTitle
            atomically (putTMVar reply result)
            pure state
                { appAgentHover = Nothing
                , appTerminalFocus = TerminalFocusUnknown
                , appMotionScheduleReset = True
                }
    MouseDown name button _ _ -> do
        unless (isAgentHoverSurface name) clearAgentHover
        state <- get
        case state.appResume of
            Just _ ->
                case (name, button) of
                    (ResumeRow sessionId, V.BLeft) ->
                        Composer.handleControlMouseDown
                            (ResumeRow sessionId)
                    (_, V.BScrollUp) ->
                        handleResumeKey
                            (V.EvMouseDown 0 0 V.BScrollUp [])
                    (_, V.BScrollDown) ->
                        handleResumeKey
                            (V.EvMouseDown 0 0 V.BScrollDown [])
                    _ -> pure ()
            Nothing ->
                case ( state.appTextPrompt
                     , state.appChoice
                     , state.appUi.uiPermission
                     ) of
                    (Just _, _, _) ->
                        case button of
                            V.BScrollUp ->
                                vScrollBy
                                    (viewportScroll OverlayViewport)
                                    (-mouseScrollLines)
                            V.BScrollDown ->
                                vScrollBy
                                    (viewportScroll OverlayViewport)
                                    mouseScrollLines
                            _ -> pure ()
                    (Nothing, Nothing, Nothing) ->
                        case (name, button) of
                            (ComposerModel, V.BLeft) ->
                                Composer.handleControlMouseDown ComposerModel
                            (ComposerEffort, V.BLeft) ->
                                Composer.handleControlMouseDown ComposerEffort
                            (ComposerMode, V.BLeft) ->
                                Composer.handleControlMouseDown ComposerMode
                            (ComposerAccount, V.BLeft) ->
                                Composer.handleControlMouseDown ComposerAccount
                            (name@ComposerImageRemove{}, V.BLeft) ->
                                Composer.handleControlMouseDown name
                            (name, V.BLeft)
                                | isQuickStartControl name ->
                                    Composer.handleControlMouseDown name
                            (CodeCopy target blockId codeIndex, V.BLeft) ->
                                Composer.handleControlMouseDown
                                    (CodeCopy target blockId codeIndex)
                            (SlashRow index, V.BLeft) ->
                                Composer.activateSlashAt
                                    applyLocalUiEventWith
                                    handleCtrlC
                                    scrollConversationPage
                                    index
                            (SlashRow _, V.BScrollUp) ->
                                Composer.handleComposerKey
                                    applyLocalUiEventWith
                                    handleCtrlC
                                    scrollConversationPage
                                    (V.EvKey V.KUp [])
                            (SlashRow _, V.BScrollDown) ->
                                Composer.handleComposerKey
                                    applyLocalUiEventWith
                                    handleCtrlC
                                    scrollConversationPage
                                    (V.EvKey V.KDown [])
                            (AgentRow target, V.BLeft) -> do
                                clearAgentHover
                                selectAgentView target
                            (AgentPopover target, V.BLeft) -> do
                                keepAgentHover target
                                selectAgentView target
                            (link@MarkdownLink{}, V.BLeft) ->
                                Composer.handleControlMouseDown link
                            _ -> handleMouseDown name button
                    (Nothing, Just _, _) ->
                        case (name, button) of
                            (ChoiceRow index, V.BLeft) ->
                                Composer.handleControlMouseDown (ChoiceRow index)
                            (ChoiceRow _, V.BScrollUp) ->
                                handleChoiceKey (V.EvKey V.KUp [])
                            (ChoiceRow _, V.BScrollDown) ->
                                handleChoiceKey (V.EvKey V.KDown [])
                            (_, V.BScrollUp) ->
                                vScrollBy
                                    (viewportScroll OverlayViewport)
                                    (-mouseScrollLines)
                            (_, V.BScrollDown) ->
                                vScrollBy
                                    (viewportScroll OverlayViewport)
                                    mouseScrollLines
                            _ -> pure ()
                    (Nothing, Nothing, Just _) ->
                        case (name, button) of
                            (PermissionRow index, V.BLeft) ->
                                resolvePermission (permissionChoiceAt index)
                            _ -> pure ()
    -- The patched vty-unix backend represents no-button pointer motion as
    -- MouseUp Nothing so Brick can route it through clickable extents.
    MouseUp (AgentRow target) Nothing _ ->
        rememberAgentHover target
    MouseUp (AgentPopover target) Nothing _ ->
        keepAgentHover target
    MouseUp AgentPane Nothing _ ->
        pure ()
    MouseUp link@MarkdownLink{} (Just V.BLeft) _ -> do
        clearAgentHover
        Composer.handleControlMouseUp link (activateControl link)
    MouseUp name button _
        | isInteractiveControl name
        , button == Just V.BLeft || button == Nothing -> do
            clearAgentHover
            Composer.handleControlMouseUp name (activateControl name)
    MouseUp _ Nothing _ ->
        modify' \state ->
            state
                { appHoveredControl = Nothing
                , appAgentHover = Nothing
                }
    VtyEvent (V.EvMouseDown _ _ V.BLeft _) ->
        modify' \state ->
            state
                { appHoveredControl = Nothing
                , appAgentHover = Nothing
                }
    VtyEvent (V.EvMouseUp _ _ _) ->
        modify' \state ->
            state
                { appHoveredControl = Nothing
                , appPressedControl = Nothing
                , appAgentHover = Nothing
                }
    VtyEvent V.EvLostFocus ->
        noteTerminalFocusLost
    VtyEvent V.EvGainedFocus ->
        noteTerminalFocusGained >> resolveConversationFollow
    VtyEvent V.EvResize{} -> do
        clearAgentHover
        invalidateCache
        -- A focused resize can leave cells from the previous geometry. Hidden
        -- terminals defer the reset until their focus-gained refresh.
        state <- get
        when (state.appTerminalFocus /= TerminalUnfocused) $
            getVtyHandle >>= liftIO . V.refresh
        queueConversationReflow
    VtyEvent vtyEvent -> do
        clearAgentHover
        state <- get
        case state.appResume of
            Just _ -> handleResumeKey vtyEvent
            Nothing ->
                case (state.appTextPrompt, state.appChoice, state.appUi.uiPermission) of
                    (Just _, _, _) -> handleTextPromptKey vtyEvent
                    (Nothing, Just _, _) -> handleChoiceKey vtyEvent
                    (Nothing, Nothing, Just _) -> handlePermissionKey vtyEvent
                    (Nothing, Nothing, Nothing) -> handleNormalKey vtyEvent
    _ -> pure ()

handlePermissionKey :: V.Event -> EventM Name AppState ()
handlePermissionKey = \case
    V.EvKey V.KUp [] -> movePermission (-1)
    V.EvKey V.KDown [] -> movePermission 1
    V.EvKey V.KBackTab [] -> movePermission (-1)
    V.EvKey (V.KChar '\t') [] -> movePermission 1
    V.EvKey (V.KChar 'y') [] -> resolvePermission PermissionAllowOnce
    V.EvKey (V.KChar 'A') [] -> resolvePermission PermissionAllowAll
    V.EvKey (V.KChar 'a') [] -> resolvePermission PermissionAllowTool
    V.EvKey (V.KChar 'n') [] -> resolvePermission PermissionDeny
    V.EvKey V.KEsc [] -> resolvePermission PermissionDeny
    V.EvKey (V.KChar 'c') modifiers
        | V.MCtrl `elem` modifiers -> do
            _ <- handleCtrlC
            resolvePermission PermissionDeny
    V.EvKey V.KEnter [] -> do
        state <- get
        let choice = case state.appUi.uiPermission of
                Just permission ->
                    permissionChoiceAt permission.permissionIndex
                Nothing -> PermissionDeny
        resolvePermission choice
    _ -> pure ()
  where
    movePermission delta =
        applyLocalUiEvent (UiPermissionMoved delta)

permissionChoiceAt :: Int -> PermissionChoice
permissionChoiceAt = \case
    0 -> PermissionAllowOnce
    1 -> PermissionAllowAll
    2 -> PermissionAllowTool
    _ -> PermissionDeny

resolvePermission
    :: PermissionChoice
    -> EventM Name AppState ()
resolvePermission choice = do
    state <- get
    case state.appPermissionReply of
        Nothing -> pure ()
        Just reply ->
            liftIO $ atomically (putTMVar reply (Just choice))
    applyLocalUiEventWith UiPermissionHidden \current ->
        current { appPermissionReply = Nothing }
    resumeNativeProgressIfRunning

handleCtrlC :: EventM Name AppState CtrlCDecision
handleCtrlC = do
    state <- get
    decision <- liftIO state.appRuntime.runtimeCtrlC
    case decision of
        SoftCancel ->
            applyLocalUiEvent $
                UiSetNotice $
                    Just $
                        warningNotice
                            "Interrupted; press Ctrl-C again to exit."
        WarnExit ->
            applyLocalUiEvent $
                UiSetNotice $
                    Just $
                        warningNotice
                            "Press Ctrl-C again to exit."
        ForceExit ->
            liftIO (throwIO UserInterrupt)
    pure decision
