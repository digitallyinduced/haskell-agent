-- | Notices and modal overlay rendering for the fullscreen UI.
module Agent.CLI.TUI.Render.Overlays
    ( drawNotice
    , drawFollowStatus
    , drawFooter
    , drawPermission
    , drawResume
    , drawChoice
    , drawTextPrompt
    , choiceRowColumns
    , onboardingVisibleRowIndices
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
import Agent.CLI.TUI.Render.Workspace (conversationScrollbarRenderer)
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
                    choiceTitle, choiceBody, choiceSearch, choiceQuery),
      choiceVisibleRows,
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


terminalTxt :: Text -> Widget n
terminalTxt = txt . displayTerminalText

terminalTxtWrap :: Text -> Widget n
terminalTxtWrap = txtWrap . displayTerminalText

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
        (_, Nothing, Just choice, _)
            | choice.choiceSearch ->
                "type to filter  │  ↑↓ navigate  │  Enter choose  │  Esc cancel"
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
                            "Enter steer  │  Ctrl+R dictate  │  Ctrl+Enter/Ctrl+O send now  │  Esc/Ctrl+C cancel  │  Tab scrollback"
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
drawChoice appState choice
    | choice.choiceSearch = drawFilterChoice appState choice
    | otherwise = case choice.choicePresentation of
        ChoiceDialog -> drawDialogChoice appState choice
        ChoiceOnboarding -> drawOnboardingChoice appState choice

drawFilterChoice :: AppState -> ChoiceOverlay -> Widget Name
drawFilterChoice appState choice =
    centerLayer $
        hLimitPercent 82 $
            vLimitPercent 78 $
                overrideAttr Border.borderAttr Theme.borderActiveAttr $
                    withBorderStyle unicodeRounded $
                        borderWithLabel
                            (waitingOverlayLabel appState choice.choiceTitle) $
                            padAll 1 $
                                vBox
                                    [ filterChoiceQuery choice
                                    , Border.hBorder
                                    , filterChoiceRows appState choice
                                    , Border.hBorder
                                    , withAttr Theme.footerAttr $
                                        terminalTxt
                                            "type to filter  │  ↑↓ navigate  │  Enter choose  │  Esc cancel"
                                    ]

filterChoiceQuery :: ChoiceOverlay -> Widget Name
filterChoiceQuery choice =
    let prefix = "search: "
        query = if Text.null choice.choiceQuery
            then withAttr Theme.mutedAttr (txt "(type to filter)")
            else terminalTxt choice.choiceQuery
        content = hBox
            [ terminalTxt prefix
            , query
            , terminalTxt " "
            ]
        cursorColumn =
            terminalTextWidth prefix
                + terminalTextWidth choice.choiceQuery
    in showCursor OverlayCursor (Location (cursorColumn, 0)) content

filterChoiceRows :: AppState -> ChoiceOverlay -> Widget Name
filterChoiceRows appState choice =
    case visible of
        [] ->
            padTop (Pad 1) $
                withAttr Theme.mutedAttr (txt "  No matches")
        _ ->
            vBox $
                [ choiceRow
                    appState
                    choice.choiceIndex
                    visibleIndex
                    originalIndex
                    row
                | (visibleIndex, (originalIndex, row)) <-
                    zip [start ..] rows
                ]
  where
    visible = choiceVisibleRows choice
    count = length visible
    start = max 0 (min choice.choiceIndex (max 0 (count - 14)))
    rows = take 14 (drop start visible)

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
                                        [ choiceRow
                                            appState
                                            choice.choiceIndex
                                            visibleIndex
                                            originalIndex
                                            row
                                        | (visibleIndex, (originalIndex, row)) <-
                                            zip [start ..] rows
                                        ]
                                    ]
  where
    visible = choiceVisibleRows choice
    count = length visible
    start =
        max 0 (min choice.choiceIndex (max 0 (count - 14)))
    rows = take 14 (drop start visible)

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

choiceRow
    :: AppState
    -> Int
    -> Int
    -> Int
    -> (Text, Text)
    -> Widget Name
choiceRow appState selected visibleIndex originalIndex (label, detail) =
    let prefix = if selected == visibleIndex then "› " else "  "
        name = ChoiceRow originalIndex
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
            if selected == visibleIndex
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
