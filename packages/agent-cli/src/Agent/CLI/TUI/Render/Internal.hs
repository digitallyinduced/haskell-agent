-- | Rendering for the retained fullscreen terminal application.
module Agent.CLI.TUI.Render.Internal
    ( drawApp
    , agentEntryWindow
    , agentPaneVisible
    , agentPaneEntryLimit
    , backgroundActivityText
    , conversationScrollbarRenderer
    , choiceRowColumns
    , filterChoiceRowLimit
    , quickStartVisible
    , quickStartWideVisible
    , quickStartCardWidth
    , quickStartRows
    , drawQuickStartCard
    , startupCapabilityLines
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
    ( AgentEntry(agentSteps, agentPath), AgentStep(agentStepTitle, agentStepState),
      AgentStepState(AgentStepRunning), agentDisplayName )
import Agent.CLI.Artifact ()
import Agent.CLI.Clipboard ( formatImageSize )
import Agent.CLI.Command ()
import Agent.CLI.Dictation ()
import Agent.CLI.ImagePreview ()
import Agent.CLI.Input ( truncateDisplayText )
import Agent.CLI.Interrupt ()
import Agent.CLI.Permission ()
import Agent.CLI.Recap ()
import Agent.CLI.Render ( formatElapsed )
import Agent.CLI.Resume ()
import Agent.CLI.Secret ()
import Agent.CLI.Status
    ( formatEstimatedTokensPerSecond
    , formatContextUsage
    )
import Agent.CLI.Style ( motionGlyphSet )
import Agent.CLI.TUI.History ()
import Agent.CLI.TUI.ImagePreview
    ( TuiImagePreview(previewBytes, previewMime, previewSourceWidth,
                      previewSourceHeight),
      previewCellSize,
      renderTuiImagePreview,
      previewCountForWidth )
import Agent.CLI.TUI.LambdaArt ()
import Agent.CLI.TUI.Motion
    ( userActionPending,
      hasBackgroundActivity,
      isBackgroundAgentActive,
      motionModeForTerminalFocus )
import Agent.CLI.TUI.Types
    ( AppState(appMetaConsole, appMotionElapsedMillis, appTerminalFocus,
               appAgentEntries, appRuntime, appImagePreviews, appResume,
               appTextPrompt, appChoice, appUi),
      FullscreenRuntime(runtimeNativeImagePreviews, runtimeWaveTrough,
                        runtimeColor, runtimeMotionMode),
      Name(ComposerImageRemove) )
import Agent.CLI.Terminal ()
import Agent.CLI.Timestamp ()
import Agent.Loop ()
import Agent.Syntax ()
import Agent.TUI.Accent ()
import Agent.TUI.Markdown ()
import Agent.TUI.Model
    ( visibleTodoList,
      uiTokensPerSecond,
      uiTokensPerSecondEstimated,
      UiState(uiPermission, uiElapsedMillis, uiActivity, uiRunning,
              uiCompletionRemainingMillis, uiCwd,
              uiContextTokens, uiContextWindow) )
import Agent.TUI.Motion
    ( backgroundIndicator,
      foregroundIndicator,
      waitingIndicator )
import Agent.TUI.Presentation
    ( liveTodoPanelLines, TodoDisplayStatus(..) )
import Agent.TUI.TextWidth ( displayTerminalText )
import Agent.ToolDispatch ()
import Brick
    ( getContext,
      clickable,
      emptyWidget,
      raw,
      fill,
      forceAttr,
      hBox,
      hLimit,
      hLimitPercent,
      padLeftRight,
      padTop,
      txt,
      vBox,
      vLimit,
      withAttr,
      Location(Location),
      Context(availHeight, availWidth),
      CursorLocation(cursorLocation),
      Result(cursors, image),
      Size(Fixed),
      Widget(render, Widget),
      Padding(Pad) )
import Brick.BChan ()
import Brick.Widgets.Border ()
import Brick.Widgets.Border.Style ()
import Brick.Widgets.Center ( centerLayer, hCenter )
import Codec.Picture ()
import Control.Applicative ()
import Control.Concurrent ()
import Control.Concurrent.Async ()
import Control.Concurrent.STM ()
import Control.Exception ()
import Control.Exception.Safe ()
import Control.Monad ()
import Control.Monad.IO.Class ()
import Control.Monad.State.Strict ()
import Data.Char ()
import Data.Foldable ()
import Data.IORef ()
import Data.List ( intersperse )
import Data.List.NonEmpty ()
import Data.Maybe ( listToMaybe )
import Data.Sequence ()
import Data.Text ( Text )
import Data.Time.Clock ()
import Data.Time.Clock.POSIX ()
import Data.Time.Format ()
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
import qualified Brick.Widgets.Border as Border ()
import qualified Agent.CLI.TUI.Bridge as Bridge ()
import qualified Agent.CLI.TUI.Composer as Composer
    ( drawSlashMenu,
      drawQueuedInputs,
      drawComposer )
import qualified Data.Map.Strict as Map ()
import qualified Agent.CLI.TUI.Scroll as Scroll ()
import qualified Data.Sequence as Seq ()
import qualified Data.Set as Set ()
import qualified Data.Text as Text
    ( intercalate,
      isPrefixOf,
      null,
      uncons,
      pack )
import qualified Data.Text.Encoding as TextEncoding ()
import qualified Agent.TUI.Theme as Theme
    ( baseAttr,
      dimAttr,
      headerAttr,
      mutedAttr,
      successAttr,
      thinkingAttr,
      waitingPulseAttr )
import qualified Agent.CLI.TUI.Transcript as Transcript ()
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

import Agent.CLI.TUI.Render.Blocks (todoStatusAttr)
import Agent.CLI.TUI.Render.Overlays
    ( drawNotice, drawFollowStatus, drawFooter, drawPermission, drawResume
    , drawChoice, drawTextPrompt, drawMetaConsole, choiceRowColumns
    , filterChoiceRowLimit
    , onboardingVisibleRowIndices
    , normalizeTextOverlayInsertion, maskedSecretText, textOverlayDisplayText
    , resumeSearchCursorColumn )
import Agent.CLI.TUI.Render.Transcript
    ( stickyPromptLayers, quickStartVisible, quickStartWideVisible
    , quickStartCardWidth, quickStartRows, drawQuickStartCard
    , startupCapabilityLines )
import Agent.CLI.TUI.Render.Workspace
    ( drawWorkspace, agentPopoverLayers, agentEntryWindow, agentPaneVisible
    , agentPaneEntryLimit, conversationScrollbarRenderer
    , selectedAgentConversation, conversationUiForTarget
    , activeConversationUi
    , applyChildConversationUiEvent )

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
                    (Nothing, Nothing, Nothing) ->
                        case state.appMetaConsole of
                            Just overlay ->
                                drawMetaConsole state overlay : dimmedMainLayers
                            Nothing -> interactiveLayers
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
    case takeLast 3 (zip [0 ..] previews) of
        [] -> []
        shown -> [centerLayer (drawImagePreviews native shown)]
  where
    takeLast count values =
        drop (max 0 (length values - count)) values

drawImagePreviews :: Bool -> [(Int, TuiImagePreview)] -> Widget Name
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

    drawPreview maxWidth maxHeight (index, preview) =
        hLimit maxWidth $
            vBox
                [ hCenter $
                    if native
                        then nativeImagePlaceholder maxWidth maxHeight preview
                        else renderTuiImagePreview maxWidth maxHeight preview
                , clickable (ComposerImageRemove index) $
                    hCenter $
                        withAttr Theme.mutedAttr $
                            terminalTxt $
                                "[image] "
                                <> preview.previewMime
                                <> " · "
                                <> Text.pack (show preview.previewSourceWidth)
                                <> "×"
                                <> Text.pack (show preview.previewSourceHeight)
                                <> " · "
                                <> formatImageSize preview.previewBytes
                                <> "  [× remove]"
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

drawHeader :: AppState -> Widget Name
drawHeader state =
    padTop (Pad 1) $
        withAttr Theme.headerAttr $
            padLeftRight 2 $
                hBox
                    [ hLimitPercent 68 (drawRepositoryHeader state.appUi)
                    , vLimit 1 (fill ' ')
                    , drawHeaderRight state
                    ]

drawRepositoryHeader :: UiState -> Widget Name
drawRepositoryHeader state
    | Text.null state.uiCwd = txt "worktree"
    | otherwise =
        hBox
            [ txt "worktree"
            , txt "  "
            , withAttr Theme.mutedAttr (terminalTxt state.uiCwd)
            ]

repositoryHeaderText :: Text -> Text -> Text
repositoryHeaderText _branch cwd =
    Text.intercalate "  " $
        filter (not . Text.null) ["worktree", cwd]

drawHeaderRight :: AppState -> Widget Name
drawHeaderRight state =
    terminalTxt $
        formatContextUsage
            state.appUi.uiContextTokens
            state.appUi.uiContextWindow

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
                        <> " "
                        <> backgroundActivityText state.appAgentEntries)
        | otherwise = emptyWidget
    elapsed =
        " · "
            <> formatElapsed (fromIntegral ui.uiElapsedMillis / 1000)
    right
        | ui.uiRunning =
            formatUiGenerationRate ui
        | ui.uiCompletionRemainingMillis > 0 =
            formatUiGenerationRate ui
        | otherwise = ""

formatUiGenerationRate :: UiState -> Text
formatUiGenerationRate ui =
    maybe
        ""
        (formatEstimatedTokensPerSecond
            (uiTokensPerSecondEstimated ui))
        (uiTokensPerSecond ui)

-- | Describe the child agents which keep the session alive after the root
-- agent has finished. Prefer their current running step so the status line
-- shows observable progress instead of an unverifiable generic claim.
backgroundActivityText :: [AgentEntry] -> Text
backgroundActivityText entries =
    case activeEntries of
        [] -> "Background work"
        [entry] -> "Background · " <> describe entry
        _ ->
            Text.pack (show (length activeEntries))
                <> " agents · "
                <> Text.intercalate "; " (map describe (take 2 activeEntries))
                <> if length activeEntries > 2 then " …" else ""
  where
    activeEntries = filter isBackgroundAgentActive entries
    describe entry =
        agentDisplayName entry.agentPath
            <> maybe "" (" — " <>) (currentStep entry)
    currentStep entry =
        (.agentStepTitle)
            <$> listToMaybe
                ( filter
                    ((== AgentStepRunning) . (.agentStepState))
                    entry.agentSteps
                    <> entry.agentSteps
                )
