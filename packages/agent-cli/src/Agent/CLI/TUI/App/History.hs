{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -O0 -Wno-unused-imports #-}
module Agent.CLI.TUI.App.History where

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

historyWindowTurnBudget :: Int
historyWindowTurnBudget = 200

historyWindowBlockBudget :: Int
historyWindowBlockBudget = 1200

historyWindowByteBudget :: Int
historyWindowByteBudget = 8 * 1024 * 1024

resetHistoryPage :: HistoryPage -> AppState -> AppState
resetHistoryPage page state =
    let
        empty =
            emptyHistoryWindow
                page.historyPageGeneration
                historyWindowTurnBudget
                historyWindowBlockBudget
                historyWindowByteBudget
        (nextBlockId, remapped) =
            remapHistoryPage state.appNextHistoryBlockId page
        window =
            either (const empty) id (applyHistoryPage remapped empty)
    in state
        { appUi = reduceUi UiConversationCleared state.appUi
        , appHistoryWindow = window
        , appHistorySelectedBlock = Nothing
        , appHistoryLiveStart = Nothing
        , appNextHistoryBlockId = nextBlockId
        , appCompletionFlashes = Map.empty
        , appConversationAnchor = Nothing
        }

setHistoryGeneration :: HistoryGeneration -> AppState -> AppState
setHistoryGeneration generation state =
    state
        { appHistoryWindow =
            state.appHistoryWindow
                { historyWindowGeneration = generation
                , historyWindowPending = Set.empty
                }
        }

applyLoadedHistoryPage :: HistoryPage -> AppState -> AppState
applyLoadedHistoryPage page state =
    if page.historyPageGeneration
        /= state.appHistoryWindow.historyWindowGeneration
        then state
        else
            let
                (nextBlockId, remapped) =
                    remapHistoryPage state.appNextHistoryBlockId page
                anchored =
                    historyWindowSetAnchors
                        (historyEdgeCursor
                            page.historyPageDirection
                            state.appHistoryWindow)
                        (state.appHistorySelectedBlock >>=
                            historyCursorForBlock
                                state.appHistoryWindow)
                        state.appHistoryWindow
                window =
                    either
                        (const anchored)
                        id
                        (applyHistoryPage remapped anchored)
                selected =
                    state.appHistorySelectedBlock >>= \blockId ->
                        if historyContainsBlock blockId window
                            then Just blockId
                            else Nothing
            in state
                { appHistoryWindow = window
                , appHistorySelectedBlock = selected
                , appNextHistoryBlockId = nextBlockId
                }

clearHistoryPending :: HistoryRequest -> AppState -> AppState
clearHistoryPending request state =
    state
        { appHistoryWindow =
            clearHistoryRequest request state.appHistoryWindow
        }

commitLiveHistoryTurn
    :: HistoryTurn
    -> HistoryCommit
    -> AppState
    -> AppState
commitLiveHistoryTurn durableTurn commit state =
    let
        start =
            case state.appHistoryLiveStart of
                Just index -> index
                Nothing
                    | commit == HistoryCommitReset -> 0
                    | otherwise ->
                        unarchivedLiveStart
                            state.appUi.uiBlocks
                            durableTurn.historyTurnBlocks
        (nextBlockId, remappedBlocks) =
            remapHistoryBlocks
                state.appNextHistoryBlockId
                durableTurn.historyTurnBlocks
        remappedTurn =
            durableTurn { historyTurnBlocks = remappedBlocks }
        baseWindow =
            case commit of
                HistoryCommitReset ->
                    emptyHistoryWindow
                        state.appHistoryWindow.historyWindowGeneration
                        historyWindowTurnBudget
                        historyWindowBlockBudget
                        historyWindowByteBudget
                _ ->
                    state.appHistoryWindow
        replacementPage =
            HistoryPage
                { historyPageGeneration =
                    state.appHistoryWindow.historyWindowGeneration
                , historyPageDirection = HistoryNewer
                , historyPageTurns = Seq.singleton remappedTurn
                , historyPageGenerationStart =
                    remappedTurn.historyTurnCursor
                , historyPageTotalTurns = 1
                , historyPageHasOlder = False
                , historyPageHasNewer = False
                }
        window =
            case commit of
                HistoryCommitReset ->
                    either
                        (const baseWindow)
                        id
                        (applyHistoryPage replacementPage baseWindow)
                _ ->
                    appendHistoryTurn remappedTurn baseWindow
        ui = truncateUiBlocks start state.appUi
    in state
        { appUi = ui
        , appHistoryWindow = window
        , appHistorySelectedBlock = Nothing
        , appHistoryLiveStart = Nothing
        , appNextHistoryBlockId = nextBlockId
        , appConversationAnchor = Nothing
        , appCompletionFlashes =
            Map.filterWithKey
                (\blockId _ -> any ((== blockId) . (.blockId)) (toList ui.uiBlocks))
                state.appCompletionFlashes
        }

truncateUiBlocks :: Int -> UiState -> UiState
truncateUiBlocks count ui =
    let
        blocks = Seq.take (max 0 count) ui.uiBlocks
        indices =
            Map.fromList
                [ (block.blockId, index)
                | (index, block) <- zip [0 ..] (toList blocks)
                ]
        selectedIndex =
            ui.uiSelectedBlock >>= (`Map.lookup` indices)
    in ui
        { uiBlocks = blocks
        , uiSelectedBlock =
            selectedIndex >>= \index ->
                (.blockId) <$> Seq.lookup index blocks
        , uiSelectedBlockIndex = selectedIndex
        , uiBlockIndices = indices
        , uiTurnStartBlock = min count ui.uiTurnStartBlock
        , uiAttemptStartBlock = min count ui.uiAttemptStartBlock
        , uiToolCalls =
            Map.filter
                (\(index, _) -> index < count)
                ui.uiToolCalls
        , uiRetryCountdown = Nothing
        }

remapHistoryPage :: Int -> HistoryPage -> (Int, HistoryPage)
remapHistoryPage nextId page =
    let
        (remaining, turns) =
            foldl'
                (\(current, accumulated) turn ->
                    let (next, blocks) =
                            remapHistoryBlocks
                                current
                                turn.historyTurnBlocks
                    in (next, accumulated |> turn
                        { historyTurnBlocks = blocks }))
                (nextId, Seq.empty)
                page.historyPageTurns
    in (remaining, page { historyPageTurns = turns })

remapHistoryBlocks :: Int -> Seq UiBlock -> (Int, Seq UiBlock)
remapHistoryBlocks nextId =
    foldl'
        (\(current, blocks) block ->
            ( current - 1
            , blocks |> block { blockId = BlockId current }
            ))
        (nextId, Seq.empty)

historyContainsBlock :: BlockId -> HistoryWindow -> Bool
historyContainsBlock blockId =
    any
        (any ((== blockId) . (.blockId))
            . toList
            . (.historyTurnBlocks))
        . toList
        . (.historyWindowTurns)

historyCursorForBlock
    :: HistoryWindow
    -> BlockId
    -> Maybe HistoryCursor
historyCursorForBlock window blockId =
    (.historyTurnCursor)
        <$> find
            (any ((== blockId) . (.blockId))
                . toList
                . (.historyTurnBlocks))
            (toList window.historyWindowTurns)

historyEdgeCursor
    :: HistoryDirection
    -> HistoryWindow
    -> Maybe HistoryCursor
historyEdgeCursor direction window =
    (.historyTurnCursor) <$> case direction of
        HistoryOlder -> window.historyWindowTurns Seq.!? 0
        HistoryNewer ->
            window.historyWindowTurns
                Seq.!? (Seq.length window.historyWindowTurns - 1)

historyPageAnchorBlock
    :: HistoryDirection
    -> HistoryWindow
    -> Maybe BlockId
historyPageAnchorBlock direction window =
    edgeTurn >>= edgeBlock
  where
    turns = window.historyWindowTurns
    edgeTurn = case direction of
        HistoryOlder -> turns Seq.!? 0
        HistoryNewer -> turns Seq.!? (Seq.length turns - 1)
    edgeBlock turn =
        let blocks = turn.historyTurnBlocks
        in case direction of
            HistoryOlder -> (.blockId) <$> blocks Seq.!? 0
            HistoryNewer ->
                (.blockId) <$> blocks Seq.!? (Seq.length blocks - 1)

wrapNativePreviewVty :: FullscreenRuntime -> V.Vty -> IO V.Vty
wrapNativePreviewVty runtime vty
    | not runtime.runtimeNativeImagePreviews = pure vty
    | otherwise = do
        rendered <- newIORef Nothing
        let output = V.outputIface vty
            deletePayload imageId =
                kittyDeleteImageSequence imageId
            placementPayload placement =
                let attachment = placement.nativePreviewAttachment
                    graphics =
                        kittyPlacedImageSequence
                            placement.nativePreviewImageId
                            placement.nativePreviewImageId
                            placement.nativePreviewColumns
                            placement.nativePreviewRows
                            attachment.imageMime
                            attachment.imageBytes
                in positionImagePayload
                    placement.nativePreviewRow
                    placement.nativePreviewColumn
                    graphics
            renderNative force = do
                revision <- readIORef runtime.runtimeImagePreviewRevision
                bounds@(terminalColumns, terminalRows) <-
                    V.displayBounds output
                previous <- readIORef rendered
                let unchanged = case previous of
                        Just (oldRevision, oldBounds, _) ->
                            oldRevision == revision && oldBounds == bounds
                        Nothing -> False
                when (force || not unchanged) do
                    visible <-
                        readIORef runtime.runtimeImagePreviewVisible
                    previews <-
                        if visible
                            then readIORef runtime.runtimeImagePreviews
                            else pure []
                    submitted <-
                        if visible
                            then
                                readIORef
                                    runtime.runtimeSubmittedImagePlacements
                            else pure []
                    let placements
                            | null previews = submitted
                            | otherwise =
                                nativePreviewPlacements
                                    runtime.runtimeImagePreviewIdBase
                                    terminalColumns
                                    terminalRows
                                    previews
                        oldImageIds = case previous of
                            Just (_, _, imageIds) -> imageIds
                            Nothing -> []
                        payload =
                            Text.concat
                                ( map deletePayload oldImageIds
                                    <> map placementPayload placements
                                )
                    when (not (Text.null payload)) $
                        V.outputByteBuffer output
                            (TextEncoding.encodeUtf8 payload)
                    writeIORef rendered $
                        Just
                            ( revision
                            , bounds
                            , map (.nativePreviewImageId) placements
                            )
            clearNative = do
                previous <- readIORef rendered
                let imageIds = case previous of
                        Just (_, _, ids) -> ids
                        Nothing -> []
                    payload = Text.concat (map deletePayload imageIds)
                when (not (Text.null payload)) $
                    V.outputByteBuffer output
                        (TextEncoding.encodeUtf8 payload)
                writeIORef rendered Nothing
        pure vty
            { V.update = \picture ->
                V.update vty picture >> renderNative False
            , V.refresh =
                V.refresh vty >> renderNative True
            , V.shutdown =
                clearNative `finally` V.shutdown vty
            }

