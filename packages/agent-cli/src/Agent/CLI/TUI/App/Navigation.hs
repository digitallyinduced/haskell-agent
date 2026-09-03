{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -O0 -Wno-unused-imports #-}
module Agent.CLI.TUI.App.Navigation where

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
import Agent.CLI.TUI.App.Reduce hiding
    ( queueConversationReflow, conversationBlocks )

handleNormalKey :: V.Event -> EventM Name AppState ()
handleNormalKey event = do
    state <- get
    case state.appDictation of
        Just session ->
            Composer.handleDictationKey handleCtrlC session event
        Nothing
            | Bridge.isSendNowKey event ->
                Composer.handleComposerKey
                    applyLocalUiEventWith
                    handleCtrlC
                    scrollConversationPage
                    event
            | otherwise ->
                case event of
                    V.EvMouseDown _ _ V.BScrollUp _ ->
                        scrollConversationBy (-mouseScrollLines)
                    V.EvMouseDown _ _ V.BScrollDown _ ->
                        scrollConversationBy mouseScrollLines
                    _ ->
                        case state.appUi.uiFocus of
                            FocusScrollback -> handleScrollbackKey event
                            FocusComposer ->
                                Composer.handleComposerKey
                                    applyLocalUiEventWith
                                    handleCtrlC
                                    scrollConversationPage
                                    event
                            FocusPermission -> pure ()

rememberAgentHover :: AgentTarget -> EventM Name AppState ()
rememberAgentHover target = do
    rowExtent <- lookupExtent (AgentRow target)
    paneExtent <- lookupExtent AgentPane
    case rowExtent of
        Nothing -> pure ()
        Just extent ->
            let Location (rowX, rowY) = extent.extentUpperLeft
                fallbackPaneUpperLeft =
                    Location (max 0 (rowX - 2), max 0 (rowY - 2))
                fallbackPaneWidth = extentWidth extent + 4
            in
            modify' \state ->
                state
                    { appAgentHover =
                        Just AgentHover
                            { agentHoverTarget = target
                            , agentHoverUpperLeft = extent.extentUpperLeft
                            , agentHoverPaneUpperLeft =
                                maybe
                                    fallbackPaneUpperLeft
                                    (.extentUpperLeft)
                                    paneExtent
                            , agentHoverPaneWidth =
                                maybe
                                    fallbackPaneWidth
                                    extentWidth
                                    paneExtent
                            }
                    }
  where
    extentWidth extent = fst extent.extentSize

keepAgentHover :: AgentTarget -> EventM Name AppState ()
keepAgentHover target = do
    state <- get
    case state.appAgentHover of
        Just hover
            | hover.agentHoverTarget == target -> pure ()
        _ -> rememberAgentHover target

clearAgentHover :: EventM Name AppState ()
clearAgentHover =
    modify' \state ->
        state { appAgentHover = Nothing }

selectAgentView :: AgentTarget -> EventM Name AppState ()
selectAgentView target = do
    state <- get
    liftIO (state.appRuntime.runtimeAgentSelect target)
    when (state.appAgentSelected /= target) do
        modify' \current ->
            current { appAgentSelected = target }
        requestVisibleSyntaxLanguages
        resumeConversationFollow

isAgentHoverSurface :: Name -> Bool
isAgentHoverSurface = \case
    AgentPane -> True
    AgentRow _ -> True
    AgentPopover _ -> True
    _ -> False

agentLayoutTargets :: [AgentEntry] -> [AgentTarget]
agentLayoutTargets =
    map (.agentTarget) . sortOn (.agentPath)

preserveAgentConversationView
    :: AgentTarget
    -> [AgentEntry]
    -> [AgentEntry]
    -> [AgentEntry]
preserveAgentConversationView selected previous =
    map preserve
  where
    preserve incoming
        | incoming.agentTarget /= selected = incoming
        | otherwise =
            case lookupAgentEntry incoming.agentTarget previous of
                Nothing -> incoming
                Just old ->
                    incoming
                        { agentConversation =
                            mergeConversationView
                                old.agentConversation
                                incoming.agentConversation
                        }

mergeConversationView :: UiState -> UiState -> UiState
mergeConversationView previous incoming
    | Seq.null incoming.uiBlocks =
        incoming
            { uiTodos =
                if null incoming.uiTodos
                    then previous.uiTodos
                    else incoming.uiTodos
            }
    | otherwise =
        incoming
            { uiBlocks = mergedBlocks
            , uiSelectedBlock = selected
            , uiSelectedBlockIndex =
                selected >>= (`Map.lookup` incoming.uiBlockIndices)
            , uiTodos =
                if null incoming.uiTodos
                    then previous.uiTodos
                    else incoming.uiTodos
            }
  where
    previousBlocks =
        Map.fromList
            [ (block.blockId, block)
            | block <- toList previous.uiBlocks
            ]
    mergedBlocks =
        fmap
            (\block ->
                case Map.lookup block.blockId previousBlocks of
                    Just old
                        | sameConversationBlock old block ->
                            block
                                { blockExpanded =
                                    preservedBlockExpansion old block
                                }
                    _ -> block)
            incoming.uiBlocks
    selected =
        case previous.uiSelectedBlock of
            Just ident
                | Just old <- Map.lookup ident previousBlocks
                , Just new <-
                    Map.lookup ident incoming.uiBlockIndices
                        >>= \index -> Seq.lookup index incoming.uiBlocks
                , sameConversationBlock old new ->
                    Just ident
            _ -> incoming.uiSelectedBlock

preservedBlockExpansion :: UiBlock -> UiBlock -> Bool
preservedBlockExpansion previous incoming
    | previous.blockKind == BlockShell
    , previous.blockState `elem` [BlockStreaming, BlockRunning]
    , incoming.blockState `notElem` [BlockStreaming, BlockRunning] =
        incoming.blockExpanded
    | otherwise = previous.blockExpanded

sameConversationBlock :: UiBlock -> UiBlock -> Bool
sameConversationBlock previous incoming =
    previous.blockKind == incoming.blockKind
        && previous.blockTitle == incoming.blockTitle
        && case (previous.blockCallId, incoming.blockCallId) of
            (Just oldCall, Just newCall) -> oldCall == newCall
            (Nothing, Nothing) ->
                previous.blockBody == incoming.blockBody
            _ -> False

handleMouseDown :: Name -> V.Button -> EventM Name AppState ()
handleMouseDown name button =
    case button of
        V.BScrollUp -> scrollConversationBy (-mouseScrollLines)
        V.BScrollDown -> scrollConversationBy mouseScrollLines
        V.BLeft -> case name of
            ConversationBlock target ident ->
                activateConversationBlock target ident
            ComposerArea ->
                applyLocalUiEvent (UiFocusChanged FocusComposer)
            _ -> pure ()
        _ -> pure ()

mouseScrollLines :: Int
mouseScrollLines = 3

handleScrollbackKey :: V.Event -> EventM Name AppState ()
handleScrollbackKey = \case
    V.EvKey V.KUp [] -> moveBlock (-1)
    V.EvKey V.KDown [] -> moveBlock 1
    V.EvKey V.KPageUp [] -> scrollConversationPage Up
    V.EvKey V.KPageDown [] -> scrollConversationPage Down
    V.EvKey (V.KChar 'k') modifiers
        | V.MCtrl `elem` modifiers -> scrollConversationBy (-1)
    V.EvKey (V.KChar 'j') modifiers
        | V.MCtrl `elem` modifiers -> scrollConversationBy 1
    V.EvKey (V.KChar 'u') modifiers
        | V.MCtrl `elem` modifiers -> scrollConversationHalfPage Up
    V.EvKey (V.KChar 'd') modifiers
        | V.MCtrl `elem` modifiers -> scrollConversationHalfPage Down
    V.EvKey V.KHome [] -> do
        leaveFollow
        requestHistoryPage HistoryOlder
        vScrollToBeginning scroll
        queueConversationReflow
    V.EvKey V.KEnd [] -> resumeConversationFollow
    V.EvKey (V.KChar 'g') [] -> do
        leaveFollow
        requestHistoryPage HistoryOlder
        vScrollToBeginning scroll
        queueConversationReflow
    V.EvKey (V.KChar 'G') [] -> resumeConversationFollow
    V.EvKey V.KLeft [] -> toggle
    V.EvKey V.KRight [] -> toggle
    V.EvKey V.KEnter [] -> toggle
    V.EvKey (V.KChar 'y') [] -> copySelected
    V.EvKey (V.KChar ' ') [] -> focusComposer
    V.EvKey (V.KChar '\t') [] -> focusComposer
    V.EvKey V.KEsc [] -> focusComposer
    _ -> pure ()
  where
    scroll = viewportScroll ConversationViewport
    moveBlock delta = do
        state <- get
        let target = state.appAgentSelected
            blocks = conversationBlocks target state
            selected =
                selectedConversationBlockId target state
            current =
                fromMaybe
                    (if delta < 0 then length blocks else -1)
                    (selected >>= \ident ->
                        findIndex ((== ident) . (.blockId)) blocks)
            nextIndex =
                max 0 (min (length blocks - 1) (current + delta))
        case drop nextIndex blocks of
            block : _ -> do
                selectConversationBlock target block.blockId
                makeVisible (ConversationBlock target block.blockId)
                queueConversationReflow
            [] -> pure ()
    toggle = do
        state <- get
        case (state.appAgentSelected, state.appHistorySelectedBlock) of
            (AgentRoot, Just blockId) ->
                modify' \current ->
                    current
                        { appHistoryWindow =
                            mapHistoryBlock
                                blockId
                                (\block ->
                                    block
                                        { blockExpanded =
                                            not block.blockExpanded
                                        })
                                current.appHistoryWindow
                        }
            _ -> applyActiveConversationUiEvent UiToggleSelected
        queueConversationReflow
    focusComposer =
        do
            modify' \state -> state { appHistorySelectedBlock = Nothing }
            applyLocalUiEvent (UiFocusChanged FocusComposer)
    leaveFollow =
        applyLocalUiEvent (UiSetFollow False)
    copySelected = do
        state <- get
        case selectedConversationBlock state.appAgentSelected state of
            Nothing -> pure ()
            Just block -> do
                copied <- liftIO $
                    state.appRuntime.runtimeCopy block.blockBody
                applyLocalUiEvent $
                    UiSetNotice $
                        Just $
                            if copied
                                then successNotice
                                    "Copied selected block."
                                else warningNotice
                                    "Terminal clipboard is unavailable."

resumeConversationFollow :: EventM Name AppState ()
resumeConversationFollow = do
    applyLocalUiEventWith (UiSetFollow True) \state ->
        state
            { appConversationAnchor =
                Scroll.followConversationTail
                    <$> state.appConversationAnchor
            }
    vScrollToEnd (viewportScroll ConversationViewport)
    queueConversationReflow

-- | Reassert the root viewport's retained follow policy after content
-- replacement or focus restoration. Brick does not clamp an old viewport top
-- when shorter content still exceeds the viewport, and scroll requests made
-- during a suppressed redraw do not survive later hidden-tab events.
resolveConversationFollow :: EventM Name AppState ()
resolveConversationFollow = do
    state <- get
    when (state.appAgentSelected == AgentRoot) $
        case Scroll.conversationFollowScroll state.appUi.uiFollow of
            Scroll.KeepConversationPosition -> pure ()
            Scroll.ScrollConversationToEnd ->
                vScrollToEnd (viewportScroll ConversationViewport)

applyActiveConversationUiEvent :: UiEvent -> EventM Name AppState ()
applyActiveConversationUiEvent uiEvent = do
    state <- get
    case state.appAgentSelected of
        AgentRoot ->
            applyLocalUiEvent uiEvent
        target@(AgentChild _) ->
            modify' (applyChildConversationUiEvent target uiEvent)
        target@(AgentNative _) ->
            modify' (applyChildConversationUiEvent target uiEvent)

scrollConversationPage :: Direction -> EventM Name AppState ()
scrollConversationPage direction = do
    height <- conversationViewportHeight
    scrollConversationBy $
        case direction of
            Up -> negate height
            Down -> height

scrollConversationHalfPage :: Direction -> EventM Name AppState ()
scrollConversationHalfPage direction = do
    height <- conversationViewportHeight
    let amount = max 1 (height `div` 2)
    scrollConversationBy $
        case direction of
            Up -> negate amount
            Down -> amount

conversationViewportHeight :: EventM Name AppState Int
conversationViewportHeight =
    lookupViewport ConversationViewport >>= \case
        Just (VP _ _ (_, height) _) -> pure (max 1 height)
        Nothing -> pure 1

scrollConversationBy :: Int -> EventM Name AppState ()
scrollConversationBy amount = do
    state <- get
    when (state.appAgentSelected == AgentRoot && amount /= 0) $
        requestHistoryNear
            (if amount < 0 then HistoryOlder else HistoryNewer)
            amount
    viewportBounds <-
        lookupViewport ConversationViewport >>= \case
            Just (VP _ top (_, height) (_, contentHeight)) ->
                pure (Just (top, height, contentHeight))
            Nothing ->
                pure Nothing
    case
        Scroll.conversationScrollGesture
            (state.appAgentSelected == AgentRoot
                && historyWindowOlderAvailable state.appHistoryWindow)
            amount
            viewportBounds of
        Scroll.IgnoreConversationScroll ->
            pure ()
        Scroll.PauseAndScrollConversation -> do
            setConversationFollow False
            vScrollBy scroll amount
            queueConversationReflow
        Scroll.ResumeConversationFollow -> do
            setConversationFollow True
            vScrollToEnd scroll
            queueConversationReflow
  where
    scroll = viewportScroll ConversationViewport

requestHistoryNear
    :: HistoryDirection
    -> Int
    -> EventM Name AppState ()
requestHistoryNear direction amount =
    lookupViewport ConversationViewport >>= \case
        Nothing -> requestHistoryPage direction
        Just (VP _ top (_, height) (_, contentHeight)) ->
            let nearEdge = case direction of
                    HistoryOlder ->
                        top + amount <= max 1 height
                    HistoryNewer ->
                        top + height + amount
                            >= contentHeight - max 1 height
            in when nearEdge (requestHistoryPage direction)

requestHistoryPage
    :: HistoryDirection
    -> EventM Name AppState ()
requestHistoryPage direction = do
    state <- get
    case
        if state.appAgentSelected == AgentRoot
            then historyWindowRequest direction state.appHistoryWindow
            else Nothing of
        Nothing -> pure ()
        Just request -> do
            modify' \current ->
                current
                    { appHistoryWindow =
                        markHistoryRequest
                            direction
                            current.appHistoryWindow
                    }
            liftIO $
                atomically $
                    writeTQueue
                        state.appRuntime.runtimeHistoryRequests
                        request

setConversationFollow :: Bool -> EventM Name AppState ()
setConversationFollow follow =
    applyLocalUiEvent (UiSetFollow follow)

conversationUnpaddedContentHeight :: EventM Name AppState Int
conversationUnpaddedContentHeight =
    lookupViewport ConversationViewport >>= \case
        Just (VP _ _ _ (_, contentHeight)) -> do
            renderedReserveRows <- conversationRenderedReserveRows
            pure $ max 0 (contentHeight - renderedReserveRows)
        Nothing -> pure 0

queueConversationReflow :: EventM Name AppState ()
queueConversationReflow = do
    state <- get
    unless state.appConversationReflowQueued do
        modify' \current ->
            current { appConversationReflowQueued = True }
        liftIO $
            enqueueAppEvent
                state.appRuntime
                AppConversationReflow

reflowConversation :: EventM Name AppState ()
reflowConversation = do
    state <- get
    case state.appConversationAnchor of
        Nothing -> pure ()
        Just anchor ->
            lookupViewport ConversationViewport >>= \case
                Nothing -> pure ()
                Just (VP _ top (_, height) (_, contentHeight)) -> do
                    renderedReserveRows <-
                        conversationRenderedReserveRows
                    renderedAnchorTop <-
                        conversationRenderedAnchorTop top anchor
                    let unpaddedContentHeight =
                            max 0
                                (contentHeight - renderedReserveRows)
                        alignedAnchor =
                            maybe
                                anchor
                                (`Scroll.realignConversationAnchor` anchor)
                                renderedAnchorTop
                        (next, scrollAction) =
                            Scroll.reflowConversationAnchor
                                state.appUi.uiFollow
                                top
                                height
                                unpaddedContentHeight
                                alignedAnchor
                    modify' \current ->
                        current { appConversationAnchor = Just next }
                    case scrollAction of
                        Scroll.KeepConversationPosition -> pure ()
                        Scroll.ScrollConversationToEnd ->
                            vScrollToEnd
                                (viewportScroll ConversationViewport)

syncSubmittedImagePlacements :: EventM Name AppState ()
syncSubmittedImagePlacements = do
    state <- get
    when state.appRuntime.runtimeNativeImagePreviews do
        let submitted =
                [ (blockId, index, preview)
                | (blockId, previews) <-
                    Map.toAscList state.appSubmittedImagePreviews
                , (index, preview) <- zip [0 ..] previews
                ]
        viewportExtent <- lookupExtent ConversationViewportExtent
        placements <-
            case viewportExtent of
                Nothing -> pure []
                Just viewportBounds ->
                    fmap concat $
                        sequence
                            [ placementFor
                                state
                                viewportBounds
                                ordinal
                                blockId
                                index
                                preview
                            | (ordinal, (blockId, index, preview)) <-
                                zip [0 ..] submitted
                            ]
        previous <-
            liftIO $
                readIORef
                    state.appRuntime.runtimeSubmittedImagePlacements
        when (not (sameNativePreviewLayout previous placements)) $
            liftIO do
                writeIORef
                    state.appRuntime.runtimeSubmittedImagePlacements
                    placements
                modifyIORef'
                    state.appRuntime.runtimeImagePreviewRevision
                    (+ 1)

placementFor
    :: AppState
    -> Extent Name
    -> Int
    -> BlockId
    -> Int
    -> TuiImagePreview
    -> EventM Name AppState [NativePreviewPlacement]
placementFor state viewportBounds ordinal blockId index preview =
    lookupExtent (ConversationImage blockId index) >>= \case
        Just imageBounds
            | extentInside viewportBounds imageBounds ->
                let Location (column, row) = imageBounds.extentUpperLeft
                    (columns, rows) = imageBounds.extentSize
                    imageId =
                        submittedImageId
                            state.appRuntime.runtimeImagePreviewIdBase
                            ordinal
                in pure
                    [ NativePreviewPlacement
                        { nativePreviewImageId = imageId
                        , nativePreviewRow = row
                        , nativePreviewColumn = column
                        , nativePreviewColumns = columns
                        , nativePreviewRows = rows
                        , nativePreviewAttachment =
                            preview.previewKittyAttachment
                        }
                    ]
        _ -> pure []

extentInside :: Extent Name -> Extent Name -> Bool
extentInside outer inner =
    innerColumn >= outerColumn
        && innerRow >= outerRow
        && innerColumn + innerWidth <= outerColumn + outerWidth
        && innerRow + innerHeight <= outerRow + outerHeight
        && innerWidth > 0
        && innerHeight > 0
  where
    Location (outerColumn, outerRow) = outer.extentUpperLeft
    (outerWidth, outerHeight) = outer.extentSize
    Location (innerColumn, innerRow) = inner.extentUpperLeft
    (innerWidth, innerHeight) = inner.extentSize

submittedImageId :: Int -> Int -> Int
submittedImageId imageIdBase ordinal =
    fromInteger $
        ((toInteger imageIdBase + 2 + toInteger ordinal)
            `mod` 4_294_967_295)
            + 1

clearSubmittedImagePlacements :: FullscreenRuntime -> EventM Name AppState ()
clearSubmittedImagePlacements runtime = do
    previous <-
        liftIO $ readIORef runtime.runtimeSubmittedImagePlacements
    when (not (null previous)) $
        liftIO do
            writeIORef runtime.runtimeSubmittedImagePlacements []
            modifyIORef' runtime.runtimeImagePreviewRevision (+ 1)

conversationRenderedReserveRows :: EventM Name AppState Int
conversationRenderedReserveRows =
    lookupExtent ConversationReserve >>= \case
        Just (Extent _ _ (_, reserveRows)) -> pure reserveRows
        Nothing -> pure 0

conversationRenderedAnchorTop
    :: Int
    -> Scroll.ConversationAnchor
    -> EventM Name AppState (Maybe Int)
conversationRenderedAnchorTop viewportTop anchor =
    lookupExtent ConversationViewportExtent >>= \case
        Nothing -> pure Nothing
        Just viewportBounds ->
            lookupExtent
                (ConversationBlock AgentRoot anchor.anchorBlockId)
                >>= \case
                    Just blockBounds
                        | extentTopRowInside viewportBounds blockBounds ->
                            let
                                Location (_, viewportRow) =
                                    viewportBounds.extentUpperLeft
                                Location (_, blockRow) =
                                    blockBounds.extentUpperLeft
                            in pure $
                                Just $
                                    max 0
                                        (viewportTop
                                            + blockRow
                                            - viewportRow)
                    _ -> pure Nothing

-- Brick translates child extents by the viewport offset. Only a block whose
-- top row is actually visible can safely repair the saved content position;
-- an extent clipped above the viewport must retain sticky-prompt behavior.
extentTopRowInside :: Extent Name -> Extent Name -> Bool
extentTopRowInside outer inner =
    innerRow >= outerRow
        && innerRow < outerRow + outerHeight
        && innerHeight > 0
  where
    Location (_, outerRow) = outer.extentUpperLeft
    (_, outerHeight) = outer.extentSize
    Location (_, innerRow) = inner.extentUpperLeft
    (_, innerHeight) = inner.extentSize

isSubmittedPrompt :: UiEvent -> Bool
isSubmittedPrompt = \case
    UiUserSubmitted _ -> True
    _ -> False

selectedBlock :: UiState -> BlockId -> Maybe UiBlock
selectedBlock state ident = lookupBlock ident state

conversationBlocks :: AgentTarget -> AppState -> [UiBlock]
conversationBlocks target state =
    case target of
        AgentRoot ->
            concatMap
                ( toList
                    . Transcript.coalesceInspectionBlocks
                    . (.historyTurnBlocks)
                )
                (toList state.appHistoryWindow.historyWindowTurns)
                <> toList
                    (Transcript.coalesceInspectionBlocks state.appUi.uiBlocks)
        AgentChild _ ->
            maybe
                []
                (toList . Transcript.coalesceInspectionBlocks . (.uiBlocks))
                (conversationUiForTarget target state)
        AgentNative _ ->
            maybe
                []
                (toList . Transcript.coalesceInspectionBlocks . (.uiBlocks))
                (conversationUiForTarget target state)

historyBlock :: HistoryWindow -> BlockId -> Maybe UiBlock
historyBlock window ident =
    historyWindowBlock ident window

selectedConversationBlockId
    :: AgentTarget
    -> AppState
    -> Maybe BlockId
selectedConversationBlockId target state =
    case target of
        AgentRoot ->
            state.appHistorySelectedBlock
                <|> state.appUi.uiSelectedBlock
        AgentChild _ ->
            conversationUiForTarget target state
                >>= (.uiSelectedBlock)
        AgentNative _ ->
            conversationUiForTarget target state
                >>= (.uiSelectedBlock)

selectedConversationBlock
    :: AgentTarget
    -> AppState
    -> Maybe UiBlock
selectedConversationBlock target state =
    selectedConversationBlockId target state >>= \ident ->
        find ((== ident) . (.blockId)) (conversationBlocks target state)

selectConversationBlock
    :: AgentTarget
    -> BlockId
    -> EventM Name AppState ()
selectConversationBlock target ident = do
    state <- get
    case target of
        AgentRoot ->
            case historyBlock state.appHistoryWindow ident of
                Just _ ->
                    modify' \current ->
                        current
                            { appHistorySelectedBlock = Just ident
                            , appUi =
                                (reduceUi
                                    (UiFocusChanged FocusScrollback)
                                    current.appUi)
                                    { uiSelectedBlock = Nothing
                                    , uiSelectedBlockIndex = Nothing
                                    }
                            }
                Nothing -> do
                    modify' \current ->
                        current { appHistorySelectedBlock = Nothing }
                    applyLocalUiEventWith
                        (UiSelectBlock ident)
                        (applyUiEvent
                            (UiFocusChanged FocusScrollback))
        AgentChild _ -> do
            modify' \current ->
                (applyChildConversationUiEvent
                    target
                    (UiSelectBlock ident)
                    current)
                    { appHistorySelectedBlock = Nothing
                    , appUi =
                        reduceUi
                            (UiFocusChanged FocusScrollback)
                            current.appUi
                    }
        AgentNative _ -> do
            modify' \current ->
                (applyChildConversationUiEvent
                    target
                    (UiSelectBlock ident)
                    current)
                    { appHistorySelectedBlock = Nothing
                    , appUi =
                        reduceUi
                            (UiFocusChanged FocusScrollback)
                            current.appUi
                    }

activateConversationBlock
    :: AgentTarget
    -> BlockId
    -> EventM Name AppState ()
activateConversationBlock target ident = do
    state <- get
    case target of
        AgentRoot ->
            case historyBlock state.appHistoryWindow ident of
                Just _ -> do
                    selectConversationBlock target ident
                    modify' \current ->
                        current
                            { appHistoryWindow =
                                mapHistoryBlock
                                    ident
                                    (\block ->
                                        block
                                            { blockExpanded =
                                                not block.blockExpanded
                                            })
                                    current.appHistoryWindow
                            }
                Nothing -> do
                    modify' \current ->
                        current { appHistorySelectedBlock = Nothing }
                    applyLocalUiEventWith
                        (UiActivateBlock ident)
                        (applyUiEvent
                            (UiFocusChanged FocusScrollback))
        AgentChild _ -> do
            modify' \current ->
                (applyChildConversationUiEvent
                    target
                    (UiActivateBlock ident)
                    current)
                    { appHistorySelectedBlock = Nothing
                    , appUi =
                        reduceUi
                            (UiFocusChanged FocusScrollback)
                            current.appUi
                    }
        AgentNative _ -> do
            modify' \current ->
                (applyChildConversationUiEvent
                    target
                    (UiActivateBlock ident)
                    current)
                    { appHistorySelectedBlock = Nothing
                    , appUi =
                        reduceUi
                            (UiFocusChanged FocusScrollback)
                            current.appUi
                    }
    queueConversationReflow

mapHistoryBlock
    :: BlockId
    -> (UiBlock -> UiBlock)
    -> HistoryWindow
    -> HistoryWindow
mapHistoryBlock ident update window =
    setHistoryWindowTurns
        (fmap
            (\turn ->
                turn
                    { historyTurnBlocks =
                        fmap
                            (\block ->
                                if block.blockId == ident
                                    then update block
                                    else block)
                            turn.historyTurnBlocks
                    })
            window.historyWindowTurns)
        window

handleCtrlC :: EventM Name AppState CtrlCDecision
handleCtrlC = do
    state <- get
    decision <- liftIO state.appRuntime.runtimeCtrlC
    case decision of
        SoftCancel -> applyLocalUiEvent $ UiSetNotice $
            Just $ warningNotice "Interrupted; press Ctrl-C again to exit."
        WarnExit -> applyLocalUiEvent $ UiSetNotice $
            Just $ warningNotice "Press Ctrl-C again to exit."
        ForceExit -> liftIO (throwIO UserInterrupt)
    pure decision
