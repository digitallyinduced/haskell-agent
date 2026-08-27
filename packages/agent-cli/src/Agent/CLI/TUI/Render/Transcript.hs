-- | Internal fullscreen rendering helpers.
module Agent.CLI.TUI.Render.Transcript
    ( drawTranscript
    , historyRangeWidgets
    , stickyPromptLayers
    , drawEmptyConversation
    , drawConversationBlocks
    , quickStartVisible
    , quickStartRows
    ) where


import Agent.CLI.AgentViewport
    ( AgentEntry(..),
      AgentStep(..),
      AgentStepState(..),
      AgentTarget(..),
      agentDisplayName,
      agentEntryTreeLabelWithGlyphModel,
      agentStatusGlyph,
      lookupAgentEntry )
import Agent.CLI.Artifact ()
import Agent.CLI.Clipboard ( formatImageSize )
import Agent.CLI.Command ()
import Agent.CLI.Dictation ()
import Agent.CLI.ImagePreview ()
import Agent.CLI.Input ( terminalTextWidth, truncateDisplayText )
import Agent.CLI.Interrupt ()
import Agent.CLI.Permission ()
import Agent.CLI.Recap ()
import Agent.CLI.Render ( formatElapsed )
import Agent.CLI.Resume
    ( ResumeBrowser(resumeBrowserSource, resumeBrowserQuery,
                    resumeBrowserExpanded, resumeBrowserNow, resumeBrowserAppliedQuery,
                    resumeBrowserNotice, resumeBrowserDeletePending,
                    resumeBrowserSearching),
      ResumeEntry(resumeId, resumeTitle, resumeCwd, resumeModel,
                  resumeCreatedAt, resumeUpdatedAt, resumeProvider,
                  resumeMessageCount, resumeTurnCount, resumeToolCount, resumeRecap,
                  resumeLastTurnSummary, resumePrompt, resumeMatch),
      visibleResumeBrowser,
      selectedResumeBrowser,
      resumeSourceLabel,
      groupResumeEntries,
      resumeRelativeAge )
import Agent.CLI.Secret ()
import Agent.CLI.Status ( formatTokensPerSecond, formatUsageWithRate )
import Agent.CLI.Style ( motionGlyphSet )
import Agent.CLI.TUI.History
    ( HistoryWindow(historyWindowTurns, historyWindowTotalTurns,
                    historyWindowGenerationStart, historyWindowHasNewer,
                    historyWindowHasOlder, historyWindowPending),
      HistoryTurn(historyTurnCursor, historyTurnBlocks),
      HistoryDirection(..),
      HistoryCursor(HistoryCursor) )
import Agent.CLI.TUI.ImagePreview
    ( TuiImagePreview(previewBytes, previewMime, previewSourceWidth,
                      previewSourceHeight),
      previewCellSize,
      renderTuiImagePreview,
      previewCountForWidth )
import Agent.CLI.TUI.LambdaArt ( lambdaArtWidget )
import Agent.CLI.TUI.Motion
    ( userActionPending,
      hasBackgroundActivity,
      isBackgroundAgentActive,
      motionModeForTerminalFocus )
import Agent.TUI.Accent ( accentRail, waveHeader )
import Agent.CLI.TUI.Types
    ( TextInputMode(..),
      TextOverlay(textBody, textCursor, textInputMode, textDraft,
                  textTitle),
      ResumeOverlay(resumeOverlayBrowser),
      ChoiceOverlay(choicePresentation, choiceIndex, choiceRows,
                    choiceTitle, choiceBody),
      ChoicePresentation(ChoiceOnboarding, ChoiceDialog),
      AgentHover(agentHoverTarget, agentHoverPaneUpperLeft,
                 agentHoverPaneWidth, agentHoverUpperLeft),
      AppState(appRuntime, appHistorySelectedBlock, appSyntaxHighlighter,
               appImagePreviews, appSubmittedImagePreviews, appResume,
               appDictation, appTextPrompt, appChoice,
               appMotionElapsedMillis, appCompletionFlashes, appHoveredControl,
               appPressedControl, appAgentSelected, appConversationAnchor,
               appAgentEntries, appUi, appHistoryWindow, appAgentHover,
               appTerminalFocus),
      FullscreenRuntime(runtimeMotionMode, runtimeNativeImagePreviews,
                       runtimeColor, runtimeWaveTrough),
      Name(ChoiceRow, ConversationViewport, ConversationViewportExtent,
           ConversationImage, AgentRow, AgentPane,
           AgentPopover, ConversationChunkCache, ConversationReserve,
           QuickStartWorktree, QuickStartResume, QuickStartCommands,
           QuickStartModel, CodeBlockCache, ConversationBlock,
           ConversationBlockCache, ConversationBodyCache, CodeCopy, PermissionRow, ResumeViewport,
           ResumeSearchCursor, ResumeRow, OverlayViewport, MarkdownLink,
           OverlayCursor) )
import Agent.CLI.Terminal ()
import Agent.CLI.Timestamp ()
import Agent.Loop ()
import Agent.Syntax ( SyntaxHighlighter )
import Agent.TUI.Markdown
    ( codeWidgetWithSyntaxHighlighting,
      markdownWidgetWithLinks,
      markdownWidgetWithSyntaxHighlightingAndLinks )
import Agent.TUI.Model
    ( conversationIsEmpty,
      reduceUi,
      visibleTodoList,
      BlockId,
      BlockKind(BlockUser, BlockAssistant, BlockThinking, BlockTool,
                BlockTodo, BlockShell, BlockEdit, BlockSystem, BlockRecap,
                BlockError),
      BlockState(BlockComplete, BlockFailed, BlockCancelled, BlockDenied,
                 BlockRunning, BlockStreaming),
      Focus(FocusComposer, FocusPermission, FocusScrollback),
      NoticeKind(..),
      PermissionOverlay(permissionIndex, permissionSummary),
      PromptState(promptUsage),
      RetryCountdown(retryCountdownBlockId),
      UiBlock(blockId, blockTimestamp, blockTitle, blockKind, blockState,
              blockDetail, blockExpanded, blockBody),
      UiEvent,
      UiNotice(noticeKind, noticeText),
      UiState(uiBlocks, uiAwaitingInput, uiActivity,
              uiCompletionRemainingMillis, uiRunning, uiElapsedMillis, uiFocus,
              uiSelectedBlock, uiPermission, uiFollow, uiRetryCountdown,
              uiNotice, uiBranch, uiCwd, uiPrompt),
      uiTokensPerSecond )
import Agent.TUI.Motion
    ( backgroundIndicator,
      foregroundIndicator,
      nativeProgressAnimationEnabled,
      quietIndicator,
      waitingIndicator,
      MotionMode(MotionOff, MotionFull, MotionReduced) )
import Agent.TUI.Presentation
    ( TodoDisplayLine(todoLineText, todoLineStatus),
      liveTodoPanelLines,
      parseTodoList,
      todoStatusGlyph,
      TodoDisplayStatus(..) )
import Agent.TUI.TextWidth ( displayTerminalText )
import Agent.ToolDispatch ()
import Brick
    ( getContext,
      cached,
      clickable,
      emptyWidget,
      raw,
      fill,
      forceAttr,
      hBox,
      hLimit,
      hLimitPercent,
      overrideAttr,
      padAll,
      padBottom,
      padLeft,
      padLeftRight,
      padRight,
      padTop,
      padTopBottom,
      reportExtent,
      showCursor,
      translateBy,
      txt,
      txtWrap,
      vBox,
      vLimit,
      vLimitPercent,
      viewport,
      withAttr,
      withBorderStyle,
      withVScrollBarRenderer,
      withVScrollBars,
      AttrName,
      Location(Location),
      Context(availHeight, availWidth),
      CursorLocation(cursorLocation),
      Result(cursors, image),
      Size(Fixed, Greedy),
      VScrollBarOrientation(OnRight),
      VScrollbarRenderer(..),
      ViewportType(Vertical),
      Widget(render, Widget),
      Padding(Pad, Max) )
import Brick.BChan ()
import Brick.Widgets.Border ( borderWithLabel )
import Brick.Widgets.Border.Style ( unicodeRounded )
import Brick.Widgets.Center ( center, centerLayer, hCenter )
import Codec.Picture ()
import Control.Applicative ()
import Control.Concurrent ()
import Control.Concurrent.Async ()
import Control.Concurrent.STM ()
import Control.Exception ()
import Control.Exception.Safe ()
import Control.Monad ( (>=>) )
import Control.Monad.IO.Class ()
import Control.Monad.State.Strict ()
import Data.Char ()
import Data.Foldable ( toList )
import Data.IORef ()
import Data.List
    ( findIndex, intersperse, nub, sort, sortOn )
import Data.List.NonEmpty ()
import Data.Maybe ( fromMaybe, isJust, maybeToList )
import Data.Sequence ( Seq )
import Data.Text ( Text )
import Data.Time.Clock ( UTCTime )
import Data.Time.Clock.POSIX ()
import Data.Time.Format ( defaultTimeLocale, formatTime )
import Data.Word ()
import GHC.Clock ()
import System.Environment ()
import System.IO ()
import System.Info ()
import System.Posix.Process ()
import System.Process ()
import qualified Brick.Types as B
    ( lookupAttrName,
      getContext,
      Result(cursors, image),
      Size(Greedy),
      Widget(render, Widget) )
import qualified Brick.Widgets.Border as Border
    ( borderAttr, hBorder )
import qualified Agent.CLI.TUI.Bridge as Bridge ()
import qualified Agent.CLI.TUI.Composer as Composer
    ( draftCursorLocation,
      drawSlashMenu,
      drawQueuedInputs,
      drawComposer,
      controlAttr,
      controlInteractionAttr )
import qualified Data.Map.Strict as Map ( findWithDefault, member )
import qualified Agent.CLI.TUI.Scroll as Scroll
    ( ConversationAnchor(anchorText, anchorReserveRows),
      conversationAnchorSticky )
import qualified Data.Sequence as Seq ( (!?), length, null )
import qualified Data.Set as Set ( Set, member )
import qualified Data.Text as Text
    ( intercalate,
      isPrefixOf,
      justifyLeft,
      length,
      lines,
      null,
      replicate,
      strip,
      takeWhile,
      uncons,
      unlines,
      pack )
import qualified Data.Text.Encoding as TextEncoding ()
import qualified Agent.TUI.Theme as Theme
    ( assistantAttr,
      baseAttr,
      borderActiveAttr,
      borderAttr,
      completionFlashAttr,
      controlLinkAttr,
      controlLinkHoverAttr,
      dimAttr,
      errorAttr,
      footerAttr,
      headerAttr,
      headingAttr,
      mutedAttr,
      selectedAttr,
      strongAttr,
      successAttr,
      thinkingAttr,
      thinkingBodyAttr,
      todoCancelledAttr,
      todoCompletedAttr,
      todoInProgressAttr,
      todoPendingAttr,
      toolAttr,
      userAttr,
      userMutedAttr,
      waitingPulseAttr )
import qualified Agent.CLI.TUI.Transcript as Transcript
    ( transcriptChunks, transcriptChunkCacheKey )
import qualified Graphics.Vty as V
    ( Attr,
      Image,
      imageHeight,
      imageWidth,
      char,
      charFill,
      crop,
      horizCat,
      vertCat )
import qualified Graphics.Vty.CrossPlatform as Vty ()


import Agent.CLI.TUI.Render.Blocks (drawBlock, cacheableBlock)

terminalTxt :: Text -> Widget n
terminalTxt = txt . displayTerminalText

terminalTxtWrap :: Text -> Widget n
terminalTxtWrap = txtWrap . displayTerminalText

drawTranscript :: AppState -> Widget Name
drawTranscript state =
    vBox $
        [ vBox $
            olderGap
                <> map
                    (drawBlock state AgentRoot state.appUi)
                    historicalBlocks
                <> newerGap
                <> [drawConversationBlocks state AgentRoot state.appUi]
        ]
            <> conversationReserveWidgets anchor
  where
    historicalBlocks =
        concatMap
            (toList . (.historyTurnBlocks))
            (toList state.appHistoryWindow.historyWindowTurns)
    olderGap =
        historyGapWidget
            HistoryOlder
            state.appHistoryWindow.historyWindowHasOlder
            state.appHistoryWindow.historyWindowPending
    newerGap =
        historyGapWidget
            HistoryNewer
            state.appHistoryWindow.historyWindowHasNewer
            state.appHistoryWindow.historyWindowPending
    anchor = state.appConversationAnchor

-- | Cache completed transcript blocks in moderately sized groups.
--
-- A Brick viewport must still lay out its complete child to determine the
-- scroll range. Per-block caching avoids repeated Markdown parsing, but a
-- redraw still has to combine one image per retained block. Grouping stable
-- blocks means ordinary scrolling traverses roughly one cached image per 32
-- blocks instead. Only full chunks are cached: Brick retains cache entries
-- until explicit invalidation, so caching the growing final chunk under a new
-- last-block key for every append would retain all of those obsolete images.
-- The final partial/live/animated group remains uncached.
drawTranscriptChunk
    :: AppState
    -> AgentTarget
    -> UiState
    -> Seq UiBlock
    -> Widget Name
drawTranscriptChunk state target ui blocks =
    case
        Transcript.transcriptChunkCacheKey
            (cacheableBlock state target ui)
            dynamicBlockIds
            blocks of
        Just (firstBlockId, lastBlockId) ->
            cached
                (ConversationChunkCache
                    target
                    firstBlockId
                    lastBlockId)
                rendered
        Nothing -> rendered
  where
    rendered =
        vBox (map (drawBlock state target ui) (toList blocks))
    dynamicBlockIds =
        [ blockId
        | state.appUi.uiFocus == FocusScrollback
        , state.appAgentSelected == target
        , blockId <- maybeToList ui.uiSelectedBlock
        ]
            <> [ blockId
               | Just (CodeCopy hoveredTarget blockId _) <-
                    [state.appHoveredControl]
               , hoveredTarget == target
               ]

drawConversationBlocks
    :: AppState
    -> AgentTarget
    -> UiState
    -> Widget Name
drawConversationBlocks state target ui =
    vBox $
        map
            (drawTranscriptChunk state target ui)
            (Transcript.transcriptChunks ui.uiBlocks)

historyRangeWidgets :: AppState -> [Widget Name]
historyRangeWidgets state =
    case historyRangeText state.appHistoryWindow of
        Nothing -> []
        Just range ->
            [ padLeftRight 2 $
                withAttr Theme.mutedAttr $
                    hCenter (txt range)
            ]

historyRangeText :: HistoryWindow -> Maybe Text
historyRangeText window = do
    firstTurn <- window.historyWindowTurns Seq.!? 0
    lastTurn <-
        window.historyWindowTurns
            Seq.!? (Seq.length window.historyWindowTurns - 1)
    let HistoryCursor generationStart =
            window.historyWindowGenerationStart
        HistoryCursor firstCursor = firstTurn.historyTurnCursor
        HistoryCursor lastCursor = lastTurn.historyTurnCursor
        total = window.historyWindowTotalTurns
        firstPosition = max 1 (firstCursor - generationStart + 1)
        lastPosition = max firstPosition (lastCursor - generationStart + 1)
        virtualized =
            window.historyWindowHasOlder
                || window.historyWindowHasNewer
                || fromIntegral (Seq.length window.historyWindowTurns) < total
        available =
            [ label
            | (isAvailable, label) <-
                [ (window.historyWindowHasOlder, "older")
                , (window.historyWindowHasNewer, "newer")
                ]
            , isAvailable
            ]
        suffix =
            case available of
                [] -> ""
                labels ->
                    " · "
                        <> Text.intercalate "/" labels
                        <> " load on scroll"
    if total <= 0 || not virtualized
        then Nothing
        else
            Just $
                "Turns "
                    <> showText firstPosition
                    <> "–"
                    <> showText (min total lastPosition)
                    <> " of "
                    <> showText total
                    <> suffix

historyGapWidget
    :: HistoryDirection
    -> Bool
    -> Set.Set HistoryDirection
    -> [Widget Name]
historyGapWidget direction available pending
    | not available = []
    | otherwise =
        [ padTopBottom 1 $
            withAttr Theme.mutedAttr $
                hCenter $
                    txt $
                        if direction `Set.member` pending
                            then "… loading " <> directionLabel <> " turns …"
                            else
                                "… "
                                    <> directionLabel
                                    <> " persisted turns are unloaded …"
        ]
  where
    directionLabel = case direction of
        HistoryOlder -> "older"
        HistoryNewer -> "newer"

showText :: Show a => a -> Text
showText = Text.pack . show

stickyPromptLayers :: AppState -> [Widget Name]
stickyPromptLayers state =
    case (state.appAgentSelected, state.appConversationAnchor) of
        (AgentRoot, Just anchor)
            | Scroll.conversationAnchorSticky anchor ->
                [ translateBy (Location (0, 2)) $
                    hLimitPercent conversationWidth $
                        padLeftRight 2 $
                            withAttr Theme.userAttr $
                                vLimit 5 $
                                    padAll 1 $
                                        terminalTxtWrap
                                            (stickyPromptPreview
                                                anchor.anchorText)
                ]
        _ -> []
  where
    conversationWidth =
        if length state.appAgentEntries <= 1 then 100 else 68

stickyPromptPreview :: Text -> Text
stickyPromptPreview text =
    case splitAt 3 (Text.lines text) of
        (shown, []) -> Text.intercalate "\n" shown
        (shown, _ : _) ->
            Text.intercalate "\n" (take 2 shown <> ["…"])

conversationReserveWidgets
    :: Maybe Scroll.ConversationAnchor
    -> [Widget Name]
conversationReserveWidgets = \case
    Just anchor
        | anchor.anchorReserveRows > 0 ->
            [ reportExtent ConversationReserve $
                vLimit anchor.anchorReserveRows (fill ' ')
            ]
    _ -> []

drawEmptyConversation :: AppState -> Widget Name
drawEmptyConversation state =
    B.Widget B.Greedy B.Greedy do
        context <- B.getContext
        let width = context.availWidth
            height = context.availHeight
        B.render $
            if quickStartVisible width height
                then
                    center $
                        vBox
                            [ vLimit
                                (max 8 (height - quickStartReservedRows))
                                (hCenter (lambdaArtWidget colorEnabled frame))
                            , hCenter (drawQuickStartPanel state)
                            ]
                else center (lambdaArtWidget colorEnabled frame)
  where
    colorEnabled = state.appRuntime.runtimeColor
    frame
        | userActionPending state = 0
        | otherwise =
            case motionModeForTerminalFocus
                state.appTerminalFocus
                state.appRuntime.runtimeMotionMode of
                MotionFull -> state.appMotionElapsedMillis
                MotionReduced -> 0
                MotionOff -> 0

quickStartReservedRows :: Int
quickStartReservedRows = 9

quickStartVisible :: Int -> Int -> Bool
quickStartVisible width height =
    width >= 48 && height >= 20

quickStartRows :: [(Name, Text, Text)]
quickStartRows =
    [ (QuickStartWorktree, "New worktree", "/worktree")
    , (QuickStartResume, "Resume session", "/resume")
    , (QuickStartCommands, "Browse commands", "/")
    , (QuickStartModel, "Manage models", "/model")
    ]

drawQuickStartPanel :: AppState -> Widget Name
drawQuickStartPanel state =
    hLimit 44 $
        vBox
            [ withAttr Theme.headingAttr $
                hCenter (txt "What would you like to do?")
            , padTop (Pad 1) $
                vBox (map drawQuickStartRow quickStartRows)
            , padTop (Pad 1) $
                withAttr Theme.mutedAttr $
                    hCenter (txt "Tip: Type / to browse commands and skills.")
            ]
  where
    drawQuickStartRow (name, label, command) =
        clickable name $
            case Composer.controlInteractionAttr state name of
                Just attr -> forceAttr attr row
                Nothing -> row
      where
        row =
            hBox
                [ withAttr Theme.controlLinkAttr (txt ("  " <> label))
                , vLimit 1 (fill ' ')
                , withAttr Theme.mutedAttr (txt command)
                , txt "  "
                ]
