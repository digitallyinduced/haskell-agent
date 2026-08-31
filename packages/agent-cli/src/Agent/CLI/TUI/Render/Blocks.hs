-- | Transcript block rendering for the fullscreen UI.
module Agent.CLI.TUI.Render.Blocks
    ( drawBlock
    , cacheableBlock
    , todoStatusAttr
    ) where


import Agent.CLI.AgentViewport ( AgentTarget(..) )
import Agent.CLI.Artifact ()
import Agent.CLI.Clipboard ( formatImageSize )
import Agent.CLI.Command ()
import Agent.CLI.Dictation ()
import Agent.CLI.ImagePreview ()
import Agent.CLI.Input ()
import Agent.CLI.Interrupt ()
import Agent.CLI.Permission ()
import Agent.CLI.Recap ()
import Agent.CLI.Render ()
import Agent.CLI.Resume ()
import Agent.CLI.Secret ()
import Agent.CLI.Status ()
import Agent.CLI.Style ( motionGlyphSet )
import Agent.CLI.TUI.History ()
import Agent.CLI.TUI.ImagePreview
    ( TuiImagePreview(previewBytes, previewMime, previewSourceWidth,
                      previewSourceHeight),
      previewCellSize,
      renderTuiImagePreview )
import Agent.CLI.TUI.LambdaArt ()
import Agent.CLI.TUI.Motion
    ( motionModeForTerminalFocus, userActionPending )
import Agent.TUI.Accent ( accentRail, waveHeader )
import Agent.CLI.TUI.Types
    ( AppState(appSubmittedImagePreviews, appTerminalFocus,
               appHistorySelectedBlock, appUi, appAgentSelected,
               appSyntaxHighlighter, appCompletionFlashes, appMotionElapsedMillis,
               appHoveredControl, appPressedControl, appRuntime),
      FullscreenRuntime(runtimeWaveTrough, runtimeMotionMode,
                        runtimeNativeImagePreviews, runtimeColor),
      Name(ConversationBodyCache, CodeBlockCache, ConversationBlock,
           ConversationBlockCache, ConversationImage, CodeCopy,
           MarkdownLink) )
import Agent.CLI.Terminal ()
import Agent.CLI.Timestamp ()
import Agent.Loop ()
import Agent.Syntax ( SyntaxHighlighter )
import Agent.TUI.Markdown
    ( codeWidgetWithSyntaxHighlighting,
      markdownWidgetWithLinks,
      markdownWidgetWithSyntaxHighlightingAndLinks )
import Agent.TUI.Model
    ( blockCodeLanguage,
      BlockId,
      BlockKind(BlockUser, BlockAssistant, BlockThinking, BlockTool,
                BlockTodo, BlockShell, BlockEdit, BlockSystem, BlockRecap,
                BlockError),
      BlockState(BlockComplete, BlockFailed, BlockCancelled, BlockDenied,
                 BlockRunning, BlockStreaming),
      Focus(FocusScrollback),
      RetryCountdown(retryCountdownBlockId),
      UiBlock(blockId, blockTimestamp, blockTitle, blockKind, blockState,
              blockDetail, blockExpanded, blockBody),
      UiState(uiRetryCountdown, uiSelectedBlock, uiFocus) )
import Agent.TUI.Motion
    ( foregroundIndicator,
      nativeProgressAnimationEnabled,
      waitingIndicator,
      MotionMode(MotionOff) )
import Agent.TUI.Presentation
    ( TodoDisplayLine(todoLineText, todoLineStatus),
      parseTodoList,
      todoStatusGlyph,
      TodoDisplayStatus(..) )
import Agent.TUI.TextWidth ( displayTerminalText )
import Agent.ToolDispatch ()
import Brick
    ( getContext,
      cached,
      clickable,
      fill,
      hBox,
      hLimit,
      overrideAttr,
      padAll,
      padBottom,
      padLeft,
      padRight,
      padTop,
      reportExtent,
      txt,
      txtWrap,
      vBox,
      vLimit,
      withAttr,
      withBorderStyle,
      AttrName,
      Context(availWidth),
      Size(Fixed),
      Widget(render, Widget),
      Padding(Pad, Max) )
import Brick.BChan ()
import Brick.Widgets.Border ()
import Brick.Widgets.Border.Style ( unicodeRounded )
import Brick.Widgets.Center ()
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
import Data.List ()
import Data.List.NonEmpty ()
import Data.Maybe ( fromMaybe )
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
import qualified Brick.Types as B ()
import qualified Brick.Widgets.Border as Border
    ( border, borderAttr )
import qualified Agent.CLI.TUI.Bridge as Bridge ()
import qualified Agent.CLI.TUI.Composer as Composer
    ( controlAttr )
import qualified Data.Map.Strict as Map ( findWithDefault, member )
import qualified Agent.CLI.TUI.Scroll as Scroll ()
import qualified Data.Sequence as Seq ()
import qualified Data.Set as Set ()
import qualified Data.Text as Text
    ( lines,
      null,
      strip,
      unlines,
      pack )
import qualified Data.Text.Encoding as TextEncoding ()
import qualified Agent.TUI.Theme as Theme
    ( assistantAttr,
      borderActiveAttr,
      completionFlashAttr,
      controlLinkAttr,
      errorAttr,
      mutedAttr,
      successAttr,
      thinkingAttr,
      thinkingBodyAttr,
      todoCancelledAttr,
      todoCompletedAttr,
      todoInProgressAttr,
      todoPendingAttr,
      toolAttr,
      userAttr,
      userMutedAttr )
import qualified Agent.CLI.TUI.Transcript as Transcript ()
import qualified Graphics.Vty as V ()
import qualified Graphics.Vty.CrossPlatform as Vty ()


terminalTxt :: Text -> Widget n
terminalTxt = txt . displayTerminalText

terminalTxtWrap :: Text -> Widget n
terminalTxtWrap = txtWrap . displayTerminalText

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
                accentBlockWithSections
                    state
                    target
                    ui
                    block
                    waveElapsed
                    (statusAttr state target block)
                    (blockStateGlyph state target block
                        <> block.blockTitle
                        <> detailSuffix block)
                    (bodySections (visibleBody block)
                        <> toolImageSections state target block)
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
                    (toolImageSections state target block)
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
                then
                    withBorderStyle unicodeRounded $
                        overrideAttr Border.borderAttr Theme.borderActiveAttr $
                            Border.border content
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
                AgentNative _ -> []
                AgentRoot ->
                    zipWith submittedImage [0 ..] $
                        Map.findWithDefault
                            []
                            block.blockId
                            state.appSubmittedImagePreviews
  where
    submittedImage index preview =
        padTop (Pad 1) $
            vBox $
                nativePlaceholder index preview
                    <> [ withAttr Theme.userMutedAttr $
                            terminalTxt (imagePreviewSummary preview)
                       ]

    nativePlaceholder index preview
        | not state.appRuntime.runtimeNativeImagePreviews = []
        | otherwise =
            let (columns, rows) = previewCellSize 36 12 preview
            in [ reportExtent
                    (ConversationImage block.blockId index) $
                    hLimit columns $
                        vLimit rows (fill ' ')
               ]

imagePreviewSummary :: TuiImagePreview -> Text
imagePreviewSummary preview =
    "[image] "
        <> preview.previewMime
        <> " · "
        <> Text.pack (show preview.previewSourceWidth)
        <> "×"
        <> Text.pack (show preview.previewSourceHeight)
        <> " · "
        <> formatImageSize preview.previewBytes

-- | Images the agent displayed with @show_image@ while this tool call ran.
-- Only the root conversation carries previews; child viewports show the
-- textual tool result alone. Native terminals get a placeholder extent that
-- the Kitty placement sync fills after each reflow; other terminals draw the
-- sampled bitmap directly.
toolImageSections :: AppState -> AgentTarget -> UiBlock -> [Widget Name]
toolImageSections state target block =
    case target of
        AgentRoot ->
            zipWith toolImage [0 ..] $
                Map.findWithDefault
                    []
                    block.blockId
                    state.appSubmittedImagePreviews
        AgentChild _ -> []
        AgentNative _ -> []
  where
    toolImage index preview =
        vBox
            [ imageWidget index preview
            , withAttr Theme.mutedAttr $
                terminalTxt (imagePreviewSummary preview)
            ]

    imageWidget index preview =
        Widget Fixed Fixed do
            context <- getContext
            let maxColumns =
                    max 1 (min toolImageMaxColumns context.availWidth)
            render $
                if state.appRuntime.runtimeNativeImagePreviews
                    then
                        let (columns, rows) =
                                previewCellSize
                                    maxColumns
                                    toolImageMaxRows
                                    preview
                        in reportExtent
                            (ConversationImage block.blockId index) $
                            hLimit columns $
                                vLimit rows (fill ' ')
                    else
                        renderTuiImagePreview
                            maxColumns
                            toolImageMaxRows
                            preview

-- | Agent-displayed images are the point of the call, so they get a larger
-- canvas than the thumbnail attached to a submitted prompt.
toolImageMaxColumns :: Int
toolImageMaxColumns = 72

toolImageMaxRows :: Int
toolImageMaxRows = 24

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
codeBlockHeader state target blockId codeIndex _language =
    hBox
        [ vLimit 1 (fill ' ')
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
    accentBlockWithSections state target ui block waveElapsed accent title
        (bodySections body)

bodySections :: Text -> [Widget Name]
bodySections body
    | Text.null (Text.strip body) = []
    | otherwise = [terminalTxtWrap body]

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
    -> [Widget Name]
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
    body
    extraSections =
    accentBlockWithSections state target ui block waveElapsed accent title $
        [ codeWidgetWithSyntaxHighlighting
            syntaxHighlighter
            (fromMaybe "text" (blockCodeLanguage block))
            code
        | not (Text.null (Text.strip code))
        ]
            <> bodySections body
            <> extraSections

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
