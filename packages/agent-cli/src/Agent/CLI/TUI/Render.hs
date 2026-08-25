-- | Rendering for the retained fullscreen terminal application.
module Agent.CLI.TUI.Render
    ( drawApp
    , agentEntryWindow
    , agentPaneVisible
    , agentPaneEntryLimit
    , conversationScrollbarRenderer
    , choiceRowColumns
    , quickStartVisible
    , quickStartRows
    , repositoryHeaderText
    , selectedAgentConversation
    , onboardingVisibleRowIndices
    , fullscreenBounds
    , fullscreenSurface
    , conversationUiForTarget
    , applyChildConversationUiEvent
    , normalizeTextOverlayInsertion
    , maskedSecretText
    , textOverlayDisplayText
    , resumeSearchCursorColumn
    ) where

import Agent.CLI.AgentViewport
    ( AgentEntry(..),
      AgentStep(..),
      AgentStepState(..),
      AgentTarget(..),
      agentDisplayName,
      agentEntryTreeLabelWithGlyphModel,
      agentStatusGlyph )
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
import Agent.CLI.Status ( formatTokenUsage )
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
      Name(ChoiceRow, ConversationViewport, AgentRow, AgentPane,
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
              uiNotice, uiBranch, uiCwd, uiPrompt) )
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
    ( find, findIndex, intersperse, nub, sort, sortOn )
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

drawApp :: AppState -> [Widget Name]
drawApp state =
    map fullscreenBounds $
        case state.appResume of
            Just resume ->
                drawResume state resume : dimmedMainLayers
            Nothing ->
                case (state.appTextPrompt, state.appChoice, state.appUi.uiPermission) of
                    (Just prompt, _, _) ->
                        drawTextPrompt state prompt : dimmedMainLayers
                    (Nothing, Just choice, _) ->
                        drawChoice state choice : dimmedMainLayers
                    (Nothing, Nothing, Just permission) ->
                        drawPermission state permission : dimmedMainLayers
                    (Nothing, Nothing, Nothing) -> interactiveLayers
  where
    mainLayers = stickyPromptLayers state <> [drawMain state]
    interactiveLayers =
        agentPopoverLayers state
            <> imagePreviewLayers
                state.appRuntime.runtimeNativeImagePreviews
                state.appImagePreviews
            <> mainLayers
    dimmedMainLayers = map (forceAttr Theme.dimAttr) mainLayers

terminalTxt :: Text -> Widget n
terminalTxt = txt . displayTerminalText

terminalTxtWrap :: Text -> Widget n
terminalTxtWrap = txtWrap . displayTerminalText

drawMain :: AppState -> Widget Name
drawMain state =
    fullscreenSurface $
        withAttr Theme.baseAttr $
            vBox
                [ drawHeader state
                , drawWorkspace state
                , drawNotice state
                , Composer.drawQueuedInputs state.appUi
                , Composer.drawSlashMenu state
                , drawFollowStatus state.appUi
                , drawLiveTodos (activeConversationUi state)
                , drawPromptActivity state
                , Composer.drawComposer state
                , drawFooter state
                ]

-- | Crop a top-level layer to the terminal without filling its unused cells.
-- Overlay layers must remain transparent, but custom widgets can otherwise
-- return images (and cursors) outside the render context and make Vty wrap
-- terminal rows.
fullscreenBounds :: Widget n -> Widget n
fullscreenBounds widget =
    B.Widget B.Greedy B.Greedy do
        context <- B.getContext
        result <- B.render widget
        let width = max 0 context.availWidth
            height = max 0 context.availHeight
            cursorInBounds cursor =
                let Location (column, row) = cursor.cursorLocation
                in column >= 0
                    && column < width
                    && row >= 0
                    && row < height
        pure
            result
                { B.image = V.crop width height result.image
                , B.cursors = filter cursorInBounds result.cursors
                }

-- | Bound the retained main layer to the terminal and explicitly paint every
-- cell. Some terminals can retain cells from an older, wider layout when a
-- later Brick image is narrower; an over-wide image can instead trigger
-- terminal-side wrapping and shift subsequent rows. Keeping the backing layer
-- exactly the render-context size prevents both failure modes.
fullscreenSurface :: Widget n -> Widget n
fullscreenSurface widget =
    B.Widget B.Greedy B.Greedy do
        context <- B.getContext
        base <- B.lookupAttrName Theme.baseAttr
        result <- B.render widget
        let width = max 0 context.availWidth
            height = max 0 context.availHeight
            cropped = V.crop width height result.image
            widthPadded =
                padImageRight
                    base
                    (width - V.imageWidth cropped)
                    cropped
            padded =
                padImageBottom
                    base
                    width
                    (height - V.imageHeight widthPadded)
                    widthPadded
        pure result { B.image = padded }

padImageRight :: V.Attr -> Int -> V.Image -> V.Image
padImageRight attr amount image
    | amount <= 0 || V.imageHeight image <= 0 = image
    | otherwise =
        V.horizCat
            [ image
            , V.charFill attr ' ' amount (V.imageHeight image)
            ]

padImageBottom :: V.Attr -> Int -> Int -> V.Image -> V.Image
padImageBottom attr width amount image
    | amount <= 0 || width <= 0 = image
    | otherwise =
        V.vertCat
            [ image
            , V.charFill attr ' ' width amount
            ]

imagePreviewLayers :: Bool -> [TuiImagePreview] -> [Widget Name]
imagePreviewLayers native previews =
    case takeLast 3 previews of
        [] -> []
        shown -> [centerLayer (drawImagePreviews native shown)]
  where
    takeLast count values =
        drop (max 0 (length values - count)) values

drawImagePreviews :: Bool -> [TuiImagePreview] -> Widget Name
drawImagePreviews native previews =
    Widget Fixed Fixed do
        context <- getContext
        let maxWidth = viewportPreviewSize context.availWidth
            maxHeight = viewportPreviewSize context.availHeight
            visible =
                drop
                    (max
                        0
                        (length previews - previewCountForWidth maxWidth))
                    previews
            gaps = max 0 (length visible - 1) * previewGap
            previewWidth =
                max 1 ((maxWidth - gaps) `div` max 1 (length visible))
            previewHeight = max 1 (maxHeight - 1)
            content =
                hBox $
                    intersperse
                        (hLimit previewGap (fill ' '))
                        (map (drawPreview previewWidth previewHeight) visible)
        render $
            hLimit maxWidth $
                vLimit maxHeight content
  where
    previewGap = 2

    drawPreview maxWidth maxHeight preview =
        hLimit maxWidth $
            vBox
                [ hCenter $
                    if native
                        then nativeImagePlaceholder maxWidth maxHeight preview
                        else renderTuiImagePreview maxWidth maxHeight preview
                , hCenter $
                    withAttr Theme.mutedAttr $
                        terminalTxt $
                            "🖼 "
                                <> preview.previewMime
                                <> " · "
                                <> Text.pack (show preview.previewSourceWidth)
                                <> "×"
                                <> Text.pack (show preview.previewSourceHeight)
                                <> " · "
                                <> formatImageSize preview.previewBytes
                ]

    nativeImagePlaceholder maxWidth maxHeight preview =
        let (width, height) =
                previewCellSize maxWidth maxHeight preview
        in hLimit width $
            vLimit height $
                fill ' '

viewportPreviewSize :: Int -> Int
viewportPreviewSize available =
    max 1 (available * 3 `div` 5)

drawWorkspace :: AppState -> Widget Name
drawWorkspace state =
    Widget Greedy Greedy do
        context <- getContext
        render $
            padTop (Pad 1) $
                hBox $
                    [drawConversationPane state]
                        <> if not
                            (agentPaneVisible
                                context.availWidth
                                context.availHeight
                                state.appAgentEntries)
                            then []
                            else
                                [ hLimitPercent 40 $
                                    hLimit agentPaneWidth $
                                        padLeft (Pad 1) $
                                            drawAgentPane
                                                state
                                                (agentPaneEntryLimit
                                                    context.availHeight)
                                                state.appAgentSelected
                                                ((.agentHoverTarget)
                                                    <$> state.appAgentHover)
                                                state.appAgentEntries
                                ]

drawConversationPane :: AppState -> Widget Name
drawConversationPane state =
    case selectedChildEntry state of
        Just entry ->
            withVScrollBarRenderer conversationScrollbarRenderer $
                withVScrollBars OnRight $
                    viewport ConversationViewport Vertical $
                        padLeftRight 2 (drawAgentConversation state entry)
        Nothing
            | conversationIsEmpty state.appUi
                && Seq.null
                    state.appHistoryWindow.historyWindowTurns ->
                padLeftRight 2 $
                    vBox
                        [ drawTranscript state
                        , drawEmptyConversation state
                        ]
            | otherwise ->
                vBox $
                    historyRangeWidgets state
                        <> [ withVScrollBarRenderer
                                conversationScrollbarRenderer $
                                withVScrollBars OnRight $
                                    viewport ConversationViewport Vertical $
                                        padLeftRight 2
                                            (drawTranscript state)
                           ]

selectedChildEntry :: AppState -> Maybe AgentEntry
selectedChildEntry state =
    selectedAgentConversation
        state.appAgentSelected
        state.appAgentEntries

selectedAgentConversation
    :: AgentTarget
    -> [AgentEntry]
    -> Maybe AgentEntry
selectedAgentConversation selected entries = case selected of
    AgentRoot -> Nothing
    target ->
        find ((== target) . (.agentTarget)) entries

conversationUiForTarget :: AgentTarget -> AppState -> Maybe UiState
conversationUiForTarget target state = case target of
    AgentRoot -> Just state.appUi
    AgentChild _ ->
        (.agentConversation)
            <$> find
                ((== target) . (.agentTarget))
                state.appAgentEntries

activeConversationUi :: AppState -> UiState
activeConversationUi state =
    fromMaybe state.appUi $
        conversationUiForTarget state.appAgentSelected state

applyChildConversationUiEvent
    :: AgentTarget
    -> UiEvent
    -> AppState
    -> AppState
applyChildConversationUiEvent target uiEvent state =
    state
        { appAgentEntries =
            map updateEntry state.appAgentEntries
        }
  where
    updateEntry entry
        | entry.agentTarget == target =
            entry
                { agentConversation =
                    reduceUi uiEvent entry.agentConversation
                }
        | otherwise = entry

drawAgentConversation :: AppState -> AgentEntry -> Widget Name
drawAgentConversation state entry =
    vBox
        [ withAttr Theme.headingAttr $
            terminalTxt ("Viewing " <> entry.agentPath)
        , withAttr Theme.mutedAttr $
            terminalTxt
                (entry.agentStatus
                    <> " · input is sent to /root")
        , padTop (Pad 1) $
            if Seq.null entry.agentConversation.uiBlocks
                then
                    withAttr Theme.mutedAttr $
                        terminalTxt "(no transcript yet)"
                else
                    drawConversationBlocks
                        state
                        entry.agentTarget
                        entry.agentConversation
        ]

-- Brick's default scrollbar uses a full block for the thumb and a blank
-- space for the trough. During rapid viewport reflow, some terminals can
-- leave the old full-block cells behind because the replacement trough is
-- visually identical to the surrounding background. A visible one-column
-- trough makes every old thumb position get explicitly repainted.
conversationScrollbarRenderer :: VScrollbarRenderer n
conversationScrollbarRenderer =
    VScrollbarRenderer
        { renderVScrollbar =
            withAttr Theme.mutedAttr (fill '┃')
        , renderVScrollbarTrough =
            withAttr Theme.borderAttr (fill '│')
        , renderVScrollbarHandleBefore = emptyWidget
        , renderVScrollbarHandleAfter = emptyWidget
        , scrollbarWidthAllocation = 1
        }

agentPaneWidth :: Int
agentPaneWidth = 42

agentPaneMinScreenWidth :: Int
agentPaneMinScreenWidth = 72

agentPaneMinAvailableHeight :: Int
agentPaneMinAvailableHeight = 10

agentPaneVisible :: Int -> Int -> [AgentEntry] -> Bool
agentPaneVisible width height entries =
    width >= agentPaneMinScreenWidth
        && height >= agentPaneMinAvailableHeight
        && length entries > 1

agentPaneEntryLimit :: Int -> Int
agentPaneEntryLimit availableHeight =
    -- Reserve the outer top pad, four pane chrome rows (border and padding),
    -- and two possible truncation indicators above and below.
    max 1 (min 12 (availableHeight - 7))

drawAgentPane
    :: AppState
    -> Int
    -> AgentTarget
    -> Maybe AgentTarget
    -> [AgentEntry]
    -> Widget Name
drawAgentPane state entryLimit selected hovered entries =
    clickable AgentPane $
        withAttr Theme.borderAttr $
            withBorderStyle unicodeRounded $
                borderWithLabel
                    (txt
                        (" Agents · "
                            <> Text.pack (show childCount)
                            <> " ")) $
                    padAll 1 $
                        vBox agentRows
  where
    ordered = sortOn (.agentPath) entries
    childCount =
        length
            [ ()
            | entry <- ordered
            , entry.agentTarget /= AgentRoot
            ]
    (hiddenBefore, shownEntries, hiddenAfter) =
        agentEntryWindow entryLimit selected ordered
    agentRows =
        [ drawHidden hiddenBefore "above"
        | hiddenBefore > 0
        ]
            <> map drawEntry shownEntries
            <> [ drawHidden hiddenAfter "below"
               | hiddenAfter > 0
               ]
    drawHidden count direction =
        withAttr Theme.mutedAttr $
            txt
                ("  … "
                    <> Text.pack (show count)
                    <> " "
                    <> direction)
    drawEntry entry =
        let
            index =
                maybe 0 id $
                    findIndex ((== entry.agentTarget) . (.agentTarget)) ordered
            marker =
                if entry.agentTarget == selected then "› " else "  "
            statusGlyph
                | isBackgroundAgentActive entry =
                    quietIndicator
                        motionGlyphSet
                        state.appRuntime.runtimeMotionMode
                        state.appMotionElapsedMillis
                | otherwise =
                    agentStatusGlyph entry.agentStatus
            row = hBox
                [ terminalTxt
                    (marker
                        <> agentEntryTreeLabelWithGlyphModel
                            statusGlyph
                            ordered
                            index
                            entry)
                , fill ' '
                ]
            styled =
                if hovered == Just entry.agentTarget
                    then withAttr Theme.controlLinkHoverAttr row
                    else if entry.agentTarget == selected
                        then withAttr Theme.successAttr row
                        else row
        in clickable (AgentRow entry.agentTarget) (vLimit 1 styled)

agentPopoverLayers :: AppState -> [Widget Name]
agentPopoverLayers state =
    case (length state.appAgentEntries > 1, state.appAgentHover) of
        (False, _) -> []
        (_, Nothing) -> []
        (True, Just hover) ->
            case find
                ((== hover.agentHoverTarget) . (.agentTarget))
                state.appAgentEntries of
                Nothing -> []
                Just entry -> [positionAgentPopover state hover entry]

agentPopoverPreferredWidth :: Int
agentPopoverPreferredWidth = 40

agentPopoverMinWidth :: Int
agentPopoverMinWidth = 24

agentPopoverHeight :: Int
agentPopoverHeight = 9

agentPopoverGap :: Int
agentPopoverGap = 1

positionAgentPopover :: AppState -> AgentHover -> AgentEntry -> Widget Name
positionAgentPopover state hover entry =
    Widget Fixed Fixed do
        context <- getContext
        let screenWidth = max 0 context.availWidth
            screenHeight = max 0 context.availHeight
            Location (_, anchorY) = hover.agentHoverUpperLeft
            Location (paneX, _) = hover.agentHoverPaneUpperLeft
            paneRight = paneX + max 1 hover.agentHoverPaneWidth
            leftAvailable = max 0 (paneX - agentPopoverGap)
            rightAvailable =
                max 0 (screenWidth - paneRight - agentPopoverGap)
            placeLeft =
                leftAvailable >= agentPopoverMinWidth
                    || leftAvailable >= rightAvailable
            available =
                if placeLeft then leftAvailable else rightAvailable
            width = min agentPopoverPreferredWidth available
            height = min agentPopoverHeight screenHeight
            x
                | placeLeft =
                    max 0 (paneX - agentPopoverGap - width)
                | otherwise =
                    min
                        (max 0 (screenWidth - width - agentPopoverGap))
                        paneRight
            y = max 0 (min anchorY (screenHeight - height))
        if screenWidth < agentPaneMinScreenWidth
            || width < agentPopoverMinWidth
            || height < 5
            then render emptyWidget
            else
                render $
                    translateBy (Location (x, y)) $
                        drawAgentPopover state placeLeft width height entry

drawAgentPopover :: AppState -> Bool -> Int -> Int -> AgentEntry -> Widget Name
drawAgentPopover state placeLeft width height entry =
    clickable (AgentPopover entry.agentTarget) surface
  where
    surface
        | placeLeft = hBox [popover, bridge]
        | otherwise = hBox [bridge, popover]
    bridge =
        hLimit agentPopoverGap $
            vLimit height (fill ' ')
    popover =
        hLimit width $
            vLimit height $
                withAttr Theme.baseAttr $
                    overrideAttr Border.borderAttr Theme.borderActiveAttr $
                        withBorderStyle unicodeRounded $
                            borderWithLabel
                                (withAttr Theme.headingAttr $
                                    terminalTxt
                                        (" "
                                            <> truncateDisplayText
                                                (max 1 (width - 6))
                                                (agentDisplayName entry.agentPath)
                                            <> " ")) $
                                padAll 1 $
                                    vBox
                                        ( intersperse
                                            (vLimit 1 (fill ' '))
                                            (map
                                                (drawAgentStep
                                                    state
                                                    (max 1 (width - 4)))
                                                steps)
                                            <> [fill ' ']
                                        )
    steps =
        case take 2 entry.agentSteps of
            [] ->
                [ AgentStep
                    { agentStepState = AgentStepInfo
                    , agentStepTitle = "No recent activity"
                    , agentStepDetail = Just entry.agentStatus
                    }
                ]
            recent -> recent

drawAgentStep :: AppState -> Int -> AgentStep -> Widget Name
drawAgentStep state width step =
    vLimit 2 $
        vBox
            [ hBox
                [ withAttr (agentStepAttr step.agentStepState) $
                    txt (agentStepGlyph state step.agentStepState)
                , txt " "
                , withAttr Theme.assistantAttr $
                    terminalTxt
                        (truncateDisplayText
                            (max 1 (width - 2))
                            step.agentStepTitle)
                ]
            , padLeft (Pad 2) $
                withAttr Theme.mutedAttr $
                    terminalTxt
                        (truncateDisplayText
                            (max 1 (width - 2))
                            (fromMaybe
                                (agentStepStateLabel step.agentStepState)
                                step.agentStepDetail))
            ]

agentStepGlyph :: AppState -> AgentStepState -> Text
agentStepGlyph state = \case
    AgentStepRunning ->
        quietIndicator
            motionGlyphSet
            state.appRuntime.runtimeMotionMode
            state.appMotionElapsedMillis
    AgentStepCompleted -> "✓"
    AgentStepFailed -> "✕"
    AgentStepInfo -> "◆"

agentStepStateLabel :: AgentStepState -> Text
agentStepStateLabel = \case
    AgentStepRunning -> "running"
    AgentStepCompleted -> "completed"
    AgentStepFailed -> "failed"
    AgentStepInfo -> "recent activity"

agentStepAttr :: AgentStepState -> AttrName
agentStepAttr = \case
    AgentStepRunning -> Theme.thinkingAttr
    AgentStepCompleted -> Theme.successAttr
    AgentStepFailed -> Theme.errorAttr
    AgentStepInfo -> Theme.toolAttr

agentEntryWindow
    :: Int
    -> AgentTarget
    -> [AgentEntry]
    -> (Int, [AgentEntry], Int)
agentEntryWindow count selected entries
    | count <= 0 = (0, [], length entries)
    | length entries <= count = (0, entries, 0)
    | otherwise =
        ( start
        , take count (drop start entries)
        , length entries - start - count
        )
  where
    selectedIndex =
        maybe 0 id $
            findIndex ((== selected) . (.agentTarget)) entries
    start =
        max 0 $
            min
                (selectedIndex - count `div` 2)
                (length entries - count)

drawHeader :: AppState -> Widget Name
drawHeader state =
    withAttr Theme.headerAttr $
        padLeftRight 2 $
            hBox
                [ hLimitPercent 68 (drawRepositoryHeader state.appUi)
                , vLimit 1 (fill ' ')
                , drawHeaderRight state
                ]

drawRepositoryHeader :: UiState -> Widget Name
drawRepositoryHeader state
    | Text.null state.uiBranch =
        withAttr Theme.mutedAttr (terminalTxt state.uiCwd)
    | otherwise =
        hBox
            [ txt "\xE0A0 "
            , withAttr Theme.mutedAttr $
                terminalTxt
                    (repositoryHeaderText state.uiBranch state.uiCwd)
            ]

repositoryHeaderText :: Text -> Text -> Text
repositoryHeaderText branch cwd =
    Text.intercalate "  " $
        filter (not . Text.null) [branch, cwd]

drawHeaderRight :: AppState -> Widget Name
drawHeaderRight state =
    withAttr Theme.mutedAttr $
        terminalTxt (formatTokenUsage state.appUi.uiPrompt.promptUsage)

drawLiveTodos :: UiState -> Widget Name
drawLiveTodos ui =
    case liveTodoPanelLines 8 (visibleTodoList ui) of
        [] -> emptyWidget
        lines_ ->
            padLeftRight 2 $
                vBox (map (vLimit 1 . drawLiveTodoLine) lines_)

drawLiveTodoLine :: Text -> Widget Name
drawLiveTodoLine line =
    Widget Fixed Fixed do
        context <- getContext
        let truncated = truncateDisplayText context.availWidth line
            painted
                | "… +" `Text.isPrefixOf` line =
                    withAttr Theme.mutedAttr (terminalTxt truncated)
                | otherwise =
                    withAttr
                        (todoStatusAttr (todoLineStatusFromText line))
                        (terminalTxt truncated)
        render painted

todoLineStatusFromText :: Text -> TodoDisplayStatus
todoLineStatusFromText line
    | "▶" `Text.isPrefixOf` line = TodoDisplayInProgress
    | "✓" `Text.isPrefixOf` line = TodoDisplayCompleted
    | "✗" `Text.isPrefixOf` line = TodoDisplayCancelled
    | otherwise = TodoDisplayPending

drawPromptActivity :: AppState -> Widget Name
drawPromptActivity state
    | not busy = emptyWidget
    | otherwise =
        padLeftRight 2 $
            hBox
                [ left
                , vLimit 1 (fill ' ')
                , withAttr Theme.mutedAttr (terminalTxt right)
                ]
  where
    ui = state.appUi
    waiting = userActionPending state
    background = hasBackgroundActivity state.appAgentEntries
    mode = state.appRuntime.runtimeMotionMode
    motionMillis = state.appMotionElapsedMillis
    colorEnabled = state.appRuntime.runtimeColor
    trough = state.appRuntime.runtimeWaveTrough
    effectiveMode =
        motionModeForTerminalFocus state.appTerminalFocus mode
    busy =
        ui.uiRunning
            || waiting
            || background
            || ui.uiCompletionRemainingMillis > 0
    diamond =
        raw
            ( V.char
                (Theme.waitingPulseAttr
                    colorEnabled
                    effectiveMode
                    trough
                    motionMillis)
                ( case Text.uncons
                    (waitingIndicator motionGlyphSet mode motionMillis) of
                    Just (character, _) -> character
                    Nothing -> '◆'
                )
            )
    left
        | waiting =
            hBox
                [ diamond
                , withAttr Theme.mutedAttr (txt " Waiting for you")
                ]
        | ui.uiRunning =
            hBox
                [ withAttr Theme.thinkingAttr $
                    txt
                        (foregroundIndicator
                            motionGlyphSet
                            mode
                            motionMillis)
                , withAttr Theme.mutedAttr
                    (txt (" " <> ui.uiActivity <> elapsed))
                ]
        | ui.uiCompletionRemainingMillis > 0 =
            withAttr Theme.successAttr (terminalTxt ui.uiActivity)
        | background =
            withAttr Theme.mutedAttr $
                terminalTxt
                    (backgroundIndicator motionGlyphSet mode motionMillis
                        <> " Background work")
        | otherwise = emptyWidget
    elapsed =
        " · "
            <> formatElapsed (fromIntegral ui.uiElapsedMillis / 1000)
    right =
        if ui.uiRunning
            then formatTokenUsage ui.uiPrompt.promptUsage
            else ""

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

drawBlock :: AppState -> AgentTarget -> UiState -> UiBlock -> Widget Name
drawBlock state target ui block =
    let selected =
            ui.uiSelectedBlock == Just block.blockId
                || (target == AgentRoot
                    && state.appHistorySelectedBlock == Just block.blockId)
        highlighted =
            selected
                && state.appUi.uiFocus == FocusScrollback
                && state.appAgentSelected == target
        waveElapsed = accentWaveElapsed state target block
        marker =
            txt (if highlighted then "❯ " else "  ")
        content = case block.blockKind of
            BlockUser ->
                withAttr Theme.userAttr $
                    padAll 1 $
                        hBox
                            [ withAttr
                                (if highlighted
                                    then Theme.borderActiveAttr
                                    else Theme.userMutedAttr)
                                marker
                            , timestampedMessage
                                Theme.userMutedAttr
                                block.blockTimestamp
                                (submittedUserMessage state target block)
                            ]
            BlockAssistant ->
                padLeft (Pad 3) $
                    padRight (Pad 1) $
                        timestampedMessage Theme.mutedAttr block.blockTimestamp $
                            withAttr Theme.assistantAttr
                                (markdownWidgetWithSyntaxHighlightingAndLinks
                                    state.appSyntaxHighlighter
                                    MarkdownLink
                                    (\codeIndex ->
                                        cached
                                            (CodeBlockCache
                                                target
                                                block.blockId
                                                codeIndex))
                                    (codeBlockHeader
                                        state
                                        target
                                        block.blockId)
                                    block.blockBody)
            BlockThinking ->
                accentMarkdownBlock
                    state
                    target
                    ui
                    block
                    waveElapsed
                    (thinkingBlockAttr state target block)
                    (blockStateGlyph state target block <> block.blockTitle)
                    (visibleBody block)
            BlockTool ->
                accentBlock
                    state
                    target
                    ui
                    block
                    waveElapsed
                    (statusAttr state target block)
                    (blockStateGlyph state target block
                        <> block.blockTitle
                        <> detailSuffix block)
                    (visibleBody block)
            BlockTodo ->
                accentBlockWithSections
                    state
                    target
                    ui
                    block
                    waveElapsed
                    (statusAttr state target block)
                    (blockStateGlyph state target block <> block.blockTitle)
                    (todoBodyWidgets block)
            BlockShell ->
                accentCodeBlock
                    state
                    target
                    ui
                    block
                    state.appSyntaxHighlighter
                    waveElapsed
                    (statusAttr state target block)
                    (blockStateGlyph state target block <> block.blockTitle)
                    block.blockDetail
                    (visibleShellBody block)
            BlockEdit ->
                accentBlock
                    state
                    target
                    ui
                    block
                    waveElapsed
                    (statusAttr state target block)
                    (blockStateGlyph state target block
                        <> block.blockTitle
                        <> detailSuffix block)
                    (visibleBody block)
            BlockSystem ->
                withAttr Theme.mutedAttr
                    (terminalTxtWrap block.blockBody)
            BlockRecap ->
                accentMarkdownBlock
                    state
                    target
                    ui
                    block
                    waveElapsed
                    (statusAttr state target block)
                    (blockStateGlyph state target block <> "Recap")
                    (visibleBody block)
            BlockError ->
                withAttr Theme.errorAttr
                    (terminalTxtWrap block.blockBody)
        framed =
            if highlighted
                then withAttr Theme.selectedAttr content
                else content
        rendered =
            clickable (ConversationBlock target block.blockId) $
                padBottom (Pad 1) $
                    case block.blockKind of
                        BlockUser -> framed
                        _ ->
                            hBox
                                [ withAttr
                                    (if highlighted
                                        then Theme.borderActiveAttr
                                        else Theme.mutedAttr)
                                    marker
                                , framed
                                ]
    in if cacheableBlock state target ui block
        then cached
            (ConversationBlockCache
                target
                block.blockId
                highlighted
                block.blockExpanded
                (codeCopyCacheState state target block.blockId))
            rendered
        else rendered

submittedUserMessage
    :: AppState
    -> AgentTarget
    -> UiBlock
    -> Widget Name
submittedUserMessage state target block =
    vBox $
        [terminalTxtWrap block.blockBody]
            <> case target of
                AgentChild _ -> []
                AgentRoot ->
                    map submittedImage $
                        Map.findWithDefault
                            []
                            block.blockId
                            state.appSubmittedImagePreviews
  where
    submittedImage preview =
        padTop (Pad 1) $
            vBox
                [ hLimit 36 (renderTuiImagePreview 36 12 preview)
                , withAttr Theme.userMutedAttr $
                    terminalTxt $
                        "🖼 "
                            <> preview.previewMime
                            <> " · "
                            <> Text.pack (show preview.previewSourceWidth)
                            <> "×"
                            <> Text.pack (show preview.previewSourceHeight)
                            <> " · "
                            <> formatImageSize preview.previewBytes
                ]

timestampedMessage :: AttrName -> Text -> Widget Name -> Widget Name
timestampedMessage timestampAttr timestamp body
    | Text.null timestamp = body
    | otherwise =
        hBox
            [ padRight Max body
            , withAttr timestampAttr
                (terminalTxt ("  " <> timestamp))
            ]

codeBlockHeader
    :: AppState
    -> AgentTarget
    -> BlockId
    -> Int
    -> Text
    -> Widget Name
codeBlockHeader state target blockId codeIndex language =
    hBox
        [ if Text.null language
            then emptyWidget
            else withAttr Theme.mutedAttr (terminalTxt language)
        , vLimit 1 (fill ' ')
        , clickable name $
            withAttr
                (Composer.controlAttr state name Theme.controlLinkAttr)
                (txt " Copy ")
        ]
  where
    name = CodeCopy target blockId codeIndex

codeCopyCacheState
    :: AppState
    -> AgentTarget
    -> BlockId
    -> Maybe (Int, Bool)
codeCopyCacheState state target blockId =
    case state.appHoveredControl of
        Just (CodeCopy hoveredTarget hoveredBlock codeIndex)
            | hoveredTarget == target
            , hoveredBlock == blockId ->
                Just
                    ( codeIndex
                    , state.appPressedControl
                        == Just (CodeCopy target blockId codeIndex)
                    )
        _ -> Nothing

cacheableBlock :: AppState -> AgentTarget -> UiState -> UiBlock -> Bool
cacheableBlock state target ui block =
    block.blockState
        `notElem` [BlockStreaming, BlockRunning]
        && maybe
            True
            ((/= block.blockId) . (.retryCountdownBlockId))
            ui.uiRetryCountdown
        && not (blockFlashing state target block)

blockStateGlyph :: AppState -> AgentTarget -> UiBlock -> Text
blockStateGlyph state target block = case block.blockState of
    BlockRunning -> liveGlyph
    BlockStreaming -> liveGlyph
    BlockComplete -> "✓ "
    BlockFailed -> "✗ "
    BlockDenied -> "⊘ "
    BlockCancelled -> "⊘ "
  where
    liveGlyph
        | target == AgentRoot
        , userActionPending state =
            waitingIndicator
                motionGlyphSet
                MotionOff
                state.appMotionElapsedMillis
                <> " "
        | otherwise =
            foregroundIndicator
                motionGlyphSet
                state.appRuntime.runtimeMotionMode
                state.appMotionElapsedMillis
                <> " "

accentBlock
    :: AppState
    -> AgentTarget
    -> UiState
    -> UiBlock
    -> Maybe Int
    -> AttrName
    -> Text
    -> Text
    -> Widget Name
accentBlock state target ui block waveElapsed accent title body =
    accentBlockWithSections state target ui block waveElapsed accent title $
        if Text.null (Text.strip body)
            then []
            else [terminalTxtWrap body]

accentMarkdownBlock
    :: AppState
    -> AgentTarget
    -> UiState
    -> UiBlock
    -> Maybe Int
    -> AttrName
    -> Text
    -> Text
    -> Widget Name
accentMarkdownBlock state target ui block waveElapsed accent title body =
    accentBlockWithSections state target ui block waveElapsed accent title $
        if Text.null (Text.strip body)
            then []
            else
                [ withAttr Theme.thinkingBodyAttr $
                    markdownWidgetWithLinks MarkdownLink body
                ]

accentCodeBlock
    :: AppState
    -> AgentTarget
    -> UiState
    -> UiBlock
    -> Maybe SyntaxHighlighter
    -> Maybe Int
    -> AttrName
    -> Text
    -> Text
    -> Text
    -> Widget Name
accentCodeBlock
    state
    target
    ui
    block
    syntaxHighlighter
    waveElapsed
    accent
    title
    code
    body =
    accentBlockWithSections state target ui block waveElapsed accent title $
        [ codeWidgetWithSyntaxHighlighting syntaxHighlighter "haskell" code
        | not (Text.null (Text.strip code))
        ]
            <> [ terminalTxtWrap body
               | not (Text.null (Text.strip body))
               ]

accentBlockWithSections
    :: AppState
    -> AgentTarget
    -> UiState
    -> UiBlock
    -> Maybe Int
    -> AttrName
    -> Text
    -> [Widget Name]
    -> Widget Name
accentBlockWithSections
    state
    target
    ui
    block
    waveElapsed
    accent
    title
    sections =
    accentRail
        motionGlyphSet
        accent
        state.appRuntime.runtimeColor
        state.appRuntime.runtimeWaveTrough
        waveElapsed $
        padLeft (Pad 2) $
            vBox (titleWidget : bodyWidgets)
  where
    titleWidget = case waveElapsed of
        Nothing ->
            withAttr accent (terminalTxtWrap title)
        Just elapsedMillis ->
            waveHeader
                accent
                state.appRuntime.runtimeColor
                state.appRuntime.runtimeWaveTrough
                elapsedMillis
                title
    paddedBody = map (padTop (Pad 1)) sections
    bodyWidgets
        | cacheableRunningBody state target ui block
        , not (null paddedBody) =
            [ cached
                (ConversationBodyCache
                    target
                    block.blockId
                    block.blockExpanded)
                (vBox paddedBody)
            ]
        | otherwise = paddedBody

-- Skip non-empty running bodies: live tool output changes without a new
-- block id, and Brick cache keys must stay stable.
cacheableRunningBody :: AppState -> AgentTarget -> UiState -> UiBlock -> Bool
cacheableRunningBody state target ui block =
    block.blockState == BlockRunning
        && Text.null (Text.strip block.blockBody)
        && maybe
            True
            ((/= block.blockId) . (.retryCountdownBlockId))
            ui.uiRetryCountdown
        && not (blockFlashing state target block)

accentWaveElapsed :: AppState -> AgentTarget -> UiBlock -> Maybe Int
accentWaveElapsed state target block
    | not (nativeProgressAnimationEnabled effectiveMode) = Nothing
    | target == AgentRoot && userActionPending state = Nothing
    | block.blockState `elem` [BlockRunning, BlockStreaming] =
        Just state.appMotionElapsedMillis
    | otherwise = Nothing
  where
    effectiveMode =
        motionModeForTerminalFocus
            state.appTerminalFocus
            state.appRuntime.runtimeMotionMode

visibleBody :: UiBlock -> Text
visibleBody block
    | block.blockExpanded = block.blockBody
    | otherwise = truncatedLines 3 block.blockBody

todoBodyWidgets :: UiBlock -> [Widget Name]
todoBodyWidgets block =
    let parsed = parseTodoList block.blockBody
        (shown, hidden)
            | block.blockExpanded = (parsed, 0)
            | otherwise =
                let visible = take 3 parsed
                in (visible, length parsed - length visible)
        rows = map todoLineWidget shown
        overflow
            | hidden > 0 =
                [ withAttr Theme.mutedAttr
                    (txt ("… +" <> Text.pack (show hidden) <> " lines"))
                ]
            | otherwise = []
    in case rows <> overflow of
        [] -> []
        widgets -> [vBox widgets]

todoLineWidget :: TodoDisplayLine -> Widget Name
todoLineWidget line =
    withAttr (todoStatusAttr line.todoLineStatus)
        (txtWrap (todoStatusGlyph line.todoLineStatus <> " " <> line.todoLineText))

todoStatusAttr :: TodoDisplayStatus -> AttrName
todoStatusAttr = \case
    TodoDisplayPending -> Theme.todoPendingAttr
    TodoDisplayInProgress -> Theme.todoInProgressAttr
    TodoDisplayCompleted -> Theme.todoCompletedAttr
    TodoDisplayCancelled -> Theme.todoCancelledAttr

truncatedLines :: Int -> Text -> Text
truncatedLines shownCount body =
    let rows = Text.lines body
        shown = take shownCount rows
        hidden = length rows - length shown
    in Text.unlines shown
        <> if hidden > 0
            then "… +" <> Text.pack (show hidden) <> " lines"
            else ""

visibleShellBody :: UiBlock -> Text
visibleShellBody block
    | block.blockExpanded = block.blockBody
    | otherwise =
        let rows = Text.lines block.blockBody
            shown
                | length rows <= 5 = rows
                | otherwise = take 2 rows <> ["…"] <> drop (length rows - 3) rows
        in Text.unlines shown

detailSuffix :: UiBlock -> Text
detailSuffix block
    | Text.null (Text.strip block.blockDetail) = ""
    | otherwise = "  " <> block.blockDetail

statusAttr :: AppState -> AgentTarget -> UiBlock -> AttrName
statusAttr state target block
    | blockFlashing state target block
    , block.blockState == BlockComplete =
        Theme.completionFlashAttr
    | otherwise = case block.blockState of
        BlockFailed -> Theme.errorAttr
        BlockCancelled -> Theme.mutedAttr
        BlockDenied -> Theme.errorAttr
        BlockComplete -> Theme.successAttr
        BlockRunning -> Theme.toolAttr
        BlockStreaming -> Theme.thinkingAttr

thinkingBlockAttr :: AppState -> AgentTarget -> UiBlock -> AttrName
thinkingBlockAttr state target block
    | blockFlashing state target block
    , block.blockState == BlockComplete =
        Theme.completionFlashAttr
    | otherwise =
        Theme.thinkingAttr

blockFlashing :: AppState -> AgentTarget -> UiBlock -> Bool
blockFlashing state target block =
    target == AgentRoot
        && Map.member block.blockId state.appCompletionFlashes

drawNotice :: AppState -> Widget Name
drawNotice state = case state.appUi.uiNotice of
    Nothing -> emptyWidget
    Just notice ->
        let (attr, prefix) = noticePresentation state notice.noticeKind
        in withAttr attr $
            padLeftRight 2
                (terminalTxtWrap (prefix <> notice.noticeText))

noticePresentation :: AppState -> NoticeKind -> (AttrName, Text)
noticePresentation state = \case
    NoticeInfo -> (Theme.footerAttr, "• ")
    NoticeSuccess -> (Theme.successAttr, "✓ ")
    NoticeWarning -> (Theme.thinkingAttr, "⚠ ")
    NoticeProgress ->
        ( Theme.thinkingAttr
        , foregroundIndicator
            motionGlyphSet
            (if userActionPending state
                then MotionOff
                else state.appRuntime.runtimeMotionMode)
            state.appMotionElapsedMillis
            <> " "
        )
    NoticeError -> (Theme.errorAttr, "✗ ")

drawFollowStatus :: UiState -> Widget Name
drawFollowStatus state
    | state.uiFollow = emptyWidget
    | otherwise =
        withAttr Theme.thinkingAttr $
            padLeftRight 2 $
                txt "↓ Live output paused · End to resume"

drawFooter :: AppState -> Widget Name
drawFooter state =
    withAttr Theme.footerAttr $
        padLeftRight 2 $
            txt footer
  where
    footer = case (state.appDictation, state.appTextPrompt, state.appChoice, state.appUi.uiFocus) of
        (Just _, _, _, _) ->
            "Enter stop  │  Esc cancel  │  Ctrl+R stop"
        (_, Just _, _, _) ->
            if state.appUi.uiRunning
                then "Enter submit  │  Shift+Enter newline  │  PgUp/PgDn scroll  │  Esc close  │  Ctrl+C cancel turn"
                else "Enter submit  │  Shift+Enter newline  │  PgUp/PgDn scroll  │  Esc cancel"
        (_, Nothing, Just _, _) ->
            if state.appUi.uiRunning
                then "↑↓ select  │  Enter choose  │  Esc close  │  Ctrl+C cancel turn"
                else "↑↓ select  │  Enter choose  │  Esc cancel"
        (_, Nothing, Nothing, focus) ->
                case focus of
                    FocusPermission ->
                        "↑↓ select  │  Enter choose  │  Esc deny"
                    FocusScrollback ->
                        "↑↓ blocks  │  Ctrl+J/K lines  │  PgUp/PgDn pages  │  wheel scroll  │  Tab/Space prompt"
                    FocusComposer
                        | not state.appUi.uiAwaitingInput ->
                            "Enter queue  │  Ctrl+R dictate  │  Ctrl+Enter/Ctrl+O send now  │  Esc/Ctrl+C cancel  │  Tab scrollback"
                        | otherwise ->
                            "Enter send  │  Ctrl+R dictate  │  Shift+Enter newline  │  PgUp/PgDn scroll  │  Tab scrollback"

drawPermission :: AppState -> PermissionOverlay -> Widget Name
drawPermission state permission =
    centerLayer $
        hLimitPercent 78 $
            overrideAttr Border.borderAttr Theme.borderActiveAttr $
                withBorderStyle unicodeRounded $
                    borderWithLabel
                        (waitingOverlayLabel state "Permission") $
                        padAll 1 $
                            vBox
                                [ terminalTxtWrap
                                    permission.permissionSummary
                                , padTop (Pad 1) $
                                    vBox $
                                        zipWith
                                            (permissionRow permission.permissionIndex)
                                            [0 ..]
                                            [ "Allow once"
                                            , "Always approve all tools for this project"
                                            , "Always allow this tool this session"
                                            , "Deny"
                                            ]
                                ]

permissionRow :: Int -> Int -> Text -> Widget Name
permissionRow selected index label =
    let prefix = if selected == index then "› " else "  "
        widget = terminalTxt (prefix <> label)
        styled =
            if selected == index
                then withAttr Theme.selectedAttr widget
                else widget
    in clickable (PermissionRow index) styled

drawResume :: AppState -> ResumeOverlay -> Widget Name
drawResume state overlay =
    centerLayer $
        hLimitPercent 82 $
            vLimitPercent 82 $
                overrideAttr Border.borderAttr Theme.borderActiveAttr $
                    withBorderStyle unicodeRounded $
                        borderWithLabel
                            (waitingOverlayLabel state "Resume session") $
                            vBox
                                [ padLeftRight 1 (resumeHeader browser)
                                , Border.hBorder
                                , withVScrollBarRenderer conversationScrollbarRenderer $
                                    withVScrollBars OnRight $
                                        viewport ResumeViewport Vertical $
                                            padLeftRight 1 (resumeList browser)
                                , Border.hBorder
                                , padLeftRight 1 (resumeFooter browser)
                                ]
  where
    browser = overlay.resumeOverlayBrowser

resumeHeader :: ResumeBrowser -> Widget Name
resumeHeader browser =
    hBox
        [ search
        , vLimit 1 (fill ' ')
        , withAttr Theme.mutedAttr $
            terminalTxt
                (resumeSourceLabel browser.resumeBrowserSource <> "  f")
        ]
  where
    prefix
        | browser.resumeBrowserSearching = "search: "
        | Text.null browser.resumeBrowserQuery = "/ to search"
        | otherwise = "search: "
    search
        | browser.resumeBrowserSearching =
            showCursor
                ResumeSearchCursor
                (Location
                    (resumeSearchCursorColumn
                        prefix
                        browser.resumeBrowserQuery, 0))
                (terminalTxt
                    (prefix <> browser.resumeBrowserQuery <> " "))
        | Text.null browser.resumeBrowserQuery =
            withAttr Theme.mutedAttr (terminalTxt prefix)
        | otherwise =
            terminalTxt (prefix <> browser.resumeBrowserQuery)

resumeSearchCursorColumn :: Text -> Text -> Int
resumeSearchCursorColumn prefix query =
    terminalTextWidth prefix + terminalTextWidth query

resumeList :: ResumeBrowser -> Widget Name
resumeList browser =
    case visibleResumeBrowser browser of
        [] ->
            padTop (Pad 1) $
                withAttr Theme.mutedAttr (txt "  No matches")
        _ : _ ->
            vBox $
                intersperse (txt "") $
                    map (resumeGroup browser selectedId) groups
  where
    entries = visibleResumeBrowser browser
    groups
        | isJust browser.resumeBrowserAppliedQuery =
            [("search results", entries)]
        | otherwise = groupResumeEntries entries
    selectedId = (.resumeId) <$> selectedResumeBrowser browser

resumeGroup
    :: ResumeBrowser
    -> Maybe Text
    -> (Text, [ResumeEntry])
    -> Widget Name
resumeGroup browser selectedId (project, entries) =
    vBox
        [ hBox
            [ withAttr Theme.mutedAttr $
                terminalTxt (" " <> project <> " ")
            , withAttr Theme.mutedAttr (vLimit 1 (fill '─'))
            ]
        , vBox (map (resumeRow browser selectedId) entries)
        ]

resumeRow :: ResumeBrowser -> Maybe Text -> ResumeEntry -> Widget Name
resumeRow browser selectedId entry =
    clickable (ResumeRow entry.resumeId) $
        if selected
            then forceAttr Theme.selectedAttr body
            else body
  where
    selected = selectedId == Just entry.resumeId
    expanded = browser.resumeBrowserExpanded == Just entry.resumeId
    marker
        | expanded = "◆ "
        | otherwise = "› "
    summary =
        hBox
            [ hLimitPercent 78
                (terminalTxt (marker <> entry.resumeTitle))
            , vLimit 1 (fill ' ')
            , withAttr Theme.mutedAttr $
                txt (resumeRelativeAge browser.resumeBrowserNow entry.resumeUpdatedAt)
            ]
    body
        | expanded =
            vBox
                [ summary
                , padLeft (Pad 4) $
                    vBox $
                        [ resumeDetail "ID" entry.resumeId
                        , resumeDetail "CWD" entry.resumeCwd
                        , resumeDetail "Model" entry.resumeModel
                        , resumeDetail
                            "Created"
                            (resumeAbsoluteTime entry.resumeCreatedAt)
                        , resumeDetail
                            "Updated"
                            (resumeAbsoluteTime entry.resumeUpdatedAt)
                        , resumeDetail
                            "Source"
                            ("local · " <> entry.resumeProvider)
                        , resumeDetail
                            "Messages"
                            (Text.pack (show entry.resumeMessageCount))
                        , resumeDetail
                            "Turns"
                            ( Text.pack (show entry.resumeTurnCount)
                                <> "    Tools  "
                                <> Text.pack (show entry.resumeToolCount)
                            )
                        ]
                            <> maybe
                                []
                                (\recap -> [resumeDetail "Recap" recap])
                                (nonEmptyResumeText entry.resumeRecap)
                            <> maybe
                                []
                                (\summaryLine ->
                                    [resumeDetail "Last turn" summaryLine])
                                (nonEmptyResumeText entry.resumeLastTurnSummary)
                            <>
                        [ resumeDetail
                            "Prompt"
                            (if Text.null entry.resumePrompt
                                then "(none)"
                                else entry.resumePrompt)
                        ]
                ]
        | otherwise =
            case entry.resumeMatch of
                Nothing -> summary
                Just match ->
                    vBox
                        [ summary
                        , padLeft (Pad 4) $
                            withAttr Theme.mutedAttr (txtWrap match)
                        ]

resumeDetail :: Text -> Text -> Widget Name
resumeDetail label value =
    hBox
        [ withAttr Theme.mutedAttr (txt (Text.justifyLeft 12 ' ' label))
        , terminalTxtWrap value
        ]

nonEmptyResumeText :: Maybe Text -> Maybe Text
nonEmptyResumeText =
    fmap Text.strip >=> \text ->
        if Text.null text then Nothing else Just text

resumeAbsoluteTime :: UTCTime -> Text
resumeAbsoluteTime =
    Text.pack . formatTime defaultTimeLocale "%b %e, %Y %H:%M UTC"

resumeFooter :: ResumeBrowser -> Widget Name
resumeFooter browser =
    withAttr attr (terminalTxt footer)
  where
    hasRows = not (null (visibleResumeBrowser browser))
    (attr, footer) =
        case browser.resumeBrowserNotice of
            Just notice -> (Theme.errorAttr, notice)
            Nothing
                | isJust browser.resumeBrowserDeletePending ->
                    (Theme.thinkingAttr, "y confirm delete  │  n cancel")
                | browser.resumeBrowserSearching ->
                    (Theme.footerAttr, "type to search  │  Enter run  │  Esc close  │  ↑↓ nav")
                | not hasRows ->
                    (Theme.footerAttr, "f filter  │  / search  │  Esc cancel")
                | otherwise ->
                    ( Theme.footerAttr
                    , "↑↓ nav  │  Enter resume  │  e expand  │  / search  │  f filter  │  d delete  │  Esc cancel"
                    )

drawChoice :: AppState -> ChoiceOverlay -> Widget Name
drawChoice appState choice = case choice.choicePresentation of
    ChoiceDialog -> drawDialogChoice appState choice
    ChoiceOnboarding -> drawOnboardingChoice appState choice

drawDialogChoice :: AppState -> ChoiceOverlay -> Widget Name
drawDialogChoice appState choice =
    centerLayer $
        hLimitPercent 82 $
            vLimitPercent 78 $
                overrideAttr Border.borderAttr Theme.borderActiveAttr $
                    withBorderStyle unicodeRounded $
                        borderWithLabel
                            (waitingOverlayLabel appState choice.choiceTitle) $
                            padAll 1 $
                                vBox
                                    [ if Text.null (Text.strip choice.choiceBody)
                                        then emptyWidget
                                        else padBottom (Pad 1) $
                                            vLimitPercent 65 $
                                                viewport OverlayViewport Vertical $
                                                    markdownWidgetWithLinks
                                                        MarkdownLink
                                                        choice.choiceBody
                                    , vBox $
                                        zipWith
                                            (choiceRow
                                                appState
                                                choice.choiceIndex)
                                            [start ..]
                                            rows
                                    ]
  where
    count = length choice.choiceRows
    start =
        max 0 (min choice.choiceIndex (max 0 (count - 14)))
    rows = take 14 (drop start choice.choiceRows)

drawOnboardingChoice :: AppState -> ChoiceOverlay -> Widget Name
drawOnboardingChoice appState choice =
    Widget Greedy Greedy do
        context <- getContext
        render $
            withAttr Theme.baseAttr $
                padRight Max $
                    padBottom Max $
                        padLeft (Pad 3) $
                            vBox
                                [ onboardingRow context.availWidth sourceIndex
                                | sourceIndex <-
                                    onboardingVisibleRowIndices
                                        context.availHeight
                                        choice.choiceIndex
                                        (length choice.choiceRows)
                                ]
  where
    choiceStart = 8
    choiceEnd = choiceStart + length choice.choiceRows
    onboardingRow width sourceIndex
        | sourceIndex >= choiceStart
        , sourceIndex < choiceEnd =
            case drop (sourceIndex - choiceStart) choice.choiceRows of
                row : _ ->
                    onboardingChoiceRow
                        appState
                        width
                        choice.choiceIndex
                        (sourceIndex - choiceStart)
                        row
                [] -> emptyWidget
        | otherwise =
            vLimit 1 $
                case sourceIndex of
                    0 -> withAttr Theme.headingAttr
                        (terminalTxt choice.choiceTitle)
                    2 -> terminalTxtWrap choice.choiceBody
                    3 ->
                        withAttr Theme.mutedAttr $
                            txt "Choose a sign-in option below, or add your own API key."
                    5 ->
                        withAttr Theme.mutedAttr $
                            txt "You can change this anytime with /login."
                    7 -> withAttr Theme.strongAttr (txt "Get started")
                    12 ->
                        withAttr Theme.mutedAttr $
                            txt "Credentials are stored locally on this computer."
                    15 ->
                        withAttr Theme.mutedAttr $
                            txt "Esc to exit · Explore all commands with /help"
                    _ -> txt " "

onboardingChoiceRow
    :: AppState
    -> Int
    -> Int
    -> Int
    -> (Text, Text)
    -> Widget Name
onboardingChoiceRow appState width selected index (label, detail) =
    clickable name interactive
  where
    prefix = if selected == index then "› " else "  "
    name = ChoiceRow index
    showDetail = width >= 72 && not (Text.null detail)
    row =
        vLimit 1 $
            if showDetail
                then hBox
                    [ hLimit 36 $
                        padRight Max (terminalTxt (prefix <> label))
                    , withAttr Theme.mutedAttr (terminalTxt detail)
                    ]
                else terminalTxt (prefix <> label)
    styled =
        if selected == index
            then withAttr Theme.selectedAttr row
            else row
    interactive = case Composer.controlInteractionAttr appState name of
        Nothing -> styled
        Just attr -> forceAttr attr row

-- | Project the 18-row onboarding surface into a short terminal while keeping
-- the selected action and all setup paths visible before explanatory copy.
onboardingVisibleRowIndices :: Int -> Int -> Int -> [Int]
onboardingVisibleRowIndices availableHeight selected choiceCount
    | availableHeight <= 0 = []
    | availableHeight >= onboardingRowCount =
        [0 .. onboardingRowCount - 1]
    | otherwise =
        sort $
            take availableHeight $
                nub $
                    [ choiceStart + clampedSelected
                    , choiceStart + max 0 (choiceCount - 1)
                    ]
                        <> [choiceStart .. choiceStart + choiceCount - 1]
                        <> [7, 12, 15, 5, 0, 2, 3, 1, 4, 6, 13, 14, 16, 17]
  where
    onboardingRowCount = 18
    choiceStart = 8
    clampedSelected =
        max 0 (min (max 0 (choiceCount - 1)) selected)

waitingOverlayLabel :: AppState -> Text -> Widget Name
waitingOverlayLabel state label =
    hBox
        [ txt " "
        , raw
            ( V.char
                (Theme.waitingPulseAttr
                    state.appRuntime.runtimeColor
                    (motionModeForTerminalFocus
                        state.appTerminalFocus
                        state.appRuntime.runtimeMotionMode)
                    state.appRuntime.runtimeWaveTrough
                    state.appMotionElapsedMillis)
                ( case Text.uncons
                    (waitingIndicator
                        motionGlyphSet
                        state.appRuntime.runtimeMotionMode
                        state.appMotionElapsedMillis) of
                    Just (character, _) -> character
                    Nothing -> '◆'
                )
            )
        , terminalTxt (" " <> label <> " ")
        ]

drawTextPrompt :: AppState -> TextOverlay -> Widget Name
drawTextPrompt state prompt =
    centerLayer $
        hLimitPercent 82 $
            vLimitPercent 78 $
                overrideAttr Border.borderAttr Theme.borderAttr $
                    withBorderStyle unicodeRounded $
                        borderWithLabel
                            (waitingOverlayLabel state prompt.textTitle) $
                            padAll 1 $
                                vBox
                                    [ if Text.null (Text.strip prompt.textBody)
                                        then emptyWidget
                                        else padBottom (Pad 1) $
                                            vLimitPercent 60 $
                                                viewport OverlayViewport Vertical $
                                                    markdownWidgetWithLinks
                                                        MarkdownLink
                                                        prompt.textBody
                                    , overrideAttr Border.borderAttr Theme.borderActiveAttr $
                                        withBorderStyle unicodeRounded $
                                            borderWithLabel (txt " Answer ") $
                                                padLeftRight 1 $
                                                    hBox
                                                        [ renderTextDraft prompt
                                                        , vLimit 1 (fill ' ')
                                                        ]
                                    ]

renderTextDraft :: TextOverlay -> Widget Name
renderTextDraft prompt =
    let displayDraft = textOverlayDisplayText prompt
        content =
            if Text.null displayDraft
                then withAttr Theme.mutedAttr (txt " ")
                else terminalTxt displayDraft
        (row, column) =
            Composer.draftCursorLocation displayDraft prompt.textCursor
    in showCursor OverlayCursor (Location (column, row)) content

-- | Replace every code point with one fixed-width masking glyph.
maskedSecretText :: Text -> Text
maskedSecretText value =
    Text.replicate (Text.length value) "•"

-- | Text that may be painted for an overlay draft.
textOverlayDisplayText :: TextOverlay -> Text
textOverlayDisplayText prompt = case prompt.textInputMode of
    TextInputPlain -> prompt.textDraft
    TextInputSecret -> maskedSecretText prompt.textDraft

-- | Secret prompts are deliberately single-line. Plain overlays preserve
-- multiline input, while secret pastes stop before the first line ending.
normalizeTextOverlayInsertion :: TextInputMode -> Text -> Text
normalizeTextOverlayInsertion = \case
    TextInputPlain -> id
    TextInputSecret -> Text.takeWhile \character ->
        character /= '\n' && character /= '\r'

choiceRow :: AppState -> Int -> Int -> (Text, Text) -> Widget Name
choiceRow appState selected index (label, detail) =
    let prefix = if selected == index then "› " else "  "
        name = ChoiceRow index
        row =
            Widget Greedy Fixed do
                context <- getContext
                let (shownLabel, shownDetail) =
                        choiceRowColumns
                            context.availWidth
                            (prefix <> label)
                            detail
                render $
                    hBox
                        [ terminalTxt shownLabel
                        , vLimit 1 (fill ' ')
                        , withAttr Theme.mutedAttr
                            (terminalTxt shownDetail)
                        ]
        styled =
            if selected == index
                then withAttr Theme.selectedAttr row
                else row
        interactive = case Composer.controlInteractionAttr appState name of
            Nothing -> styled
            Just attr -> forceAttr attr row
    in clickable name interactive

-- | Fit a choice label and its right-aligned detail into one terminal row.
-- When both do not fit, the label gets roughly two thirds of the available
-- cells and the detail gets the rest; short columns donate their unused space.
choiceRowColumns :: Int -> Text -> Text -> (Text, Text)
choiceRowColumns width label detail
    | width <= 0 = ("", "")
    | Text.null detail = (truncateDisplayText width label, "")
    | labelWidth + choiceRowGap + detailWidth <= width = (label, detail)
    | width <= choiceRowGap + 1 = (truncateDisplayText width label, "")
    | otherwise =
        ( truncateDisplayText labelBudget label
        , truncateDisplayText detailBudget detail
        )
  where
    choiceRowGap = 2
    labelWidth = terminalTextWidth label
    detailWidth = terminalTextWidth detail
    contentBudget = width - choiceRowGap
    preferredDetailBudget =
        min detailWidth (max 1 (contentBudget `div` 3))
    labelBudget =
        min labelWidth (contentBudget - preferredDetailBudget)
    detailBudget =
        min detailWidth (contentBudget - labelBudget)
