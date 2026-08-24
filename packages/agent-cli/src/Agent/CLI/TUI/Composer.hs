-- | Fullscreen prompt composer rendering, editing, and input buffering.
module Agent.CLI.TUI.Composer
    ( ComposerEscapeAction(..)
    , activateSlashAt
    , appendFullscreenInput
    , applyComposerUiEvent
    , composerEscapeAction
    , controlAttr
    , controlInteractionAttr
    , decodePaste
    , draftCursorLocation
    , drawComposer
    , drawQueuedInputs
    , drawSlashMenu
    , handleComposerKey
    , handleControlMouseDown
    , handleControlMouseUp
    , handleEffortControlClick
    , handlePromptControlClick
    , newFullscreenInputBuffer
    , promoteFullscreenInput
    , queuedFullscreenInputDisplays
    , readFullscreenInputs
    , takeFullscreenInput
    , takeFullscreenInputOr
    , wrapDraft
    ) where

import Agent.CLI.Clipboard
    ( nonEmptyClipboardText
    , readClipboardText
    )
import Agent.CLI.Command
    ( SlashMenu(..)
    , SlashSuggestion(..)
    , slashMenuForWithSkillsAndModels
    )
import Agent.CLI.Input
    ( ReplLine(..)
    , appendReplHistory
    , displayEditorText
    , submissionPromptText
    )
import Agent.CLI.Interrupt (CtrlCDecision)
import Agent.CLI.Options (reasoningEfforts)
import qualified Agent.CLI.TUI.Bridge as Bridge
import Agent.CLI.TUI.Composer.Buffer
import Agent.CLI.TUI.Composer.Edit
import Agent.CLI.TUI.Types
import qualified Agent.TUI.Theme as Theme
import Agent.TUI.Model
import Brick
import qualified Brick.Widgets.Border as Border
import Brick.Widgets.Border.Style (unicodeRounded)
import qualified Brick.Widgets.Border.Style as BorderStyle
import Control.Concurrent.STM (atomically)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (modify')
import Data.List (elemIndex, intersperse)
import Data.Maybe (fromMaybe)
import Data.Sequence (ViewL(..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Vty as V

type ApplyLocalUiEvent =
    UiEvent
    -> (AppState -> AppState)
    -> EventM Name AppState ()

drawSlashMenu :: AppState -> Widget Name
drawSlashMenu state = case currentSlashMenu state of
    Nothing -> emptyWidget
    Just menu ->
        let allSuggestions = menu.slashMenuSuggestions
            count = length allSuggestions
            selected
                | count == 0 = 0
                | otherwise = state.appSlashIndex `mod` count
            start = max 0 (min selected (max 0 (count - 6)))
            suggestions = take 6 (drop start allSuggestions)
        in padLeftRight 2 $
            withAttr Theme.borderAttr $
                withBorderStyle unicodeRounded $
                    Border.borderWithLabel (txt (menuLabel menu)) $
                        vBox $
                            zipWith
                                (drawSlashRow selected)
                                [start ..]
                                suggestions
  where
    menuLabel menu =
        case menu.slashMenuSuggestions of
            suggestion : _
                | "$" `Text.isPrefixOf`
                    suggestion.slashSuggestionDisplay ->
                        " Skills "
            _ -> " Commands "

drawSlashRow :: Int -> Int -> SlashSuggestion -> Widget Name
drawSlashRow selected index suggestion =
    let prefix = if selected == index then "❯ " else "  "
        row =
            hBox
                [ txt (prefix <> suggestion.slashSuggestionDisplay)
                , vLimit 1 (fill ' ')
                , withAttr Theme.mutedAttr
                    (txt suggestion.slashSuggestionSummary)
                ]
        styled =
            if selected == index
                then withAttr Theme.selectedAttr row
                else row
    in clickable (SlashRow index) styled

drawQueuedInputs :: UiState -> Widget Name
drawQueuedInputs state =
    case Seq.viewl state.uiQueuedInputs of
        EmptyL -> emptyWidget
        next :< _ ->
            padLeftRight 2 $
                vLimit 1 $
                    hBox
                        [ withAttr Theme.toolAttr (txt "◇ ")
                        , withAttr Theme.footerAttr
                            (txt
                                ("queued "
                                    <> Text.pack
                                        (show (Seq.length state.uiQueuedInputs))
                                    <> " · "))
                        , withAttr Theme.mutedAttr (txt (queuePreview next))
                        ]

queuePreview :: Text -> Text
queuePreview text =
    let oneLine = Text.unwords (Text.words text)
    in if Text.length oneLine > 100
        then Text.take 99 oneLine <> "…"
        else oneLine

drawComposer :: AppState -> Widget Name
drawComposer appState =
    Widget Greedy Fixed do
        context <- getContext
        let focused = state.uiFocus == FocusComposer
            attr = if focused then Theme.borderActiveAttr else Theme.borderAttr
            leading =
                filter (not . Text.null)
                    [ if state.uiPrompt.promptAttachments > 0
                        then "image "
                            <> Text.pack
                                (show state.uiPrompt.promptAttachments)
                        else ""
                    , if Seq.null state.uiQueuedInputs
                        then
                            if not state.uiAwaitingInput
                                then "next message"
                                else ""
                        else "queued "
                            <> Text.pack
                                (show (Seq.length state.uiQueuedInputs))
                    ]
            label =
                if null leading
                    then Nothing
                    else Just $
                        hBox (intersperse (txt " · ") (map txt leading))
            draftWidth = max 1 (context.availWidth - composerDraftChromeWidth)
            (draftRows, _) =
                wrapDraft draftWidth state.uiDraft state.uiCursor
            bodyHeight = min maxComposerRows (length draftRows)
            editor =
                clickable ComposerArea $
                    padLeftRight 1 $
                        hBox
                            [ withAttr
                                (if focused
                                    then Theme.borderActiveAttr
                                    else Theme.mutedAttr)
                                (txt "❯ ")
                            , withAttr Theme.assistantAttr
                                (renderDraft focused bodyHeight state)
                            ]
            composer =
                withBorderStyle unicodeRounded $
                    composerBorder
                        bodyHeight
                        (withAttr Theme.footerAttr <$> label)
                        (drawComposerStatus appState)
                        editor
        render (overrideAttr Border.borderAttr attr composer)
  where
    state = appState.appUi

composerDraftChromeWidth :: Int
composerDraftChromeWidth = 6

maxComposerRows :: Int
maxComposerRows = 8

composerBorder
    :: Int
    -> Maybe (Widget Name)
    -> Widget Name
    -> Widget Name
    -> Widget Name
composerBorder bodyHeight topLabel bottomLabel body =
    vBox
        [ hBox
            [ Border.borderElem BorderStyle.bsCornerTL
            , topBorder
            , Border.borderElem BorderStyle.bsCornerTR
            ]
        , vLimit bodyHeight $
            hBox [Border.vBorder, body, Border.vBorder]
        , hBox
            [ Border.borderElem BorderStyle.bsCornerBL
            , bottomBorder
            , Border.borderElem BorderStyle.bsCornerBR
            ]
        ]
  where
    topBorder = case topLabel of
        Nothing -> Border.hBorder
        Just label ->
            hBox
                [ hLimit 1 Border.hBorder
                , txt " "
                , label
                , txt " "
                , Border.hBorder
                ]
    bottomBorder =
        hBox
            [ Border.hBorder
            , txt " "
            , bottomLabel
            , txt " "
            ]

controlAttr :: AppState -> Name -> AttrName -> AttrName
controlAttr state name fallback =
    case controlInteractionAttr state name of
        Just attr -> attr
        Nothing -> fallback

controlInteractionAttr :: AppState -> Name -> Maybe AttrName
controlInteractionAttr state name
    | state.appPressedControl == Just name
    , state.appHoveredControl == Just name =
        Just Theme.controlLinkActiveAttr
    | state.appHoveredControl == Just name =
        Just Theme.controlLinkHoverAttr
    | otherwise =
        Nothing

renderDraft :: Bool -> Int -> UiState -> Widget Name
renderDraft focused height state =
    Widget Greedy Fixed do
        context <- getContext
        let width = max 1 context.availWidth
            (rows, (row, column)) =
                wrapDraft width state.uiDraft state.uiCursor
            firstVisibleRow = max 0 (row - height + 1)
            visibleRows = take height (drop firstVisibleRow rows)
            visibleCursorRow = row - firstVisibleRow
            content
                | Text.null state.uiDraft =
                    withAttr Theme.mutedAttr $
                        txt
                            (if not state.uiAwaitingInput
                                then "Type a follow-up…"
                                else " ")
                | otherwise =
                    vBox (map renderRow visibleRows)
            cursorContent
                | focused =
                    showCursor
                        ComposerCursor
                        (Location (column, visibleCursorRow))
                        content
                | otherwise = content
        render (padRight Max cursorContent)
  where
    -- Empty visual rows still need one cell so Brick preserves their height
    -- and can place the insertion cursor on them.
    renderRow row
        | Text.null row = txt " "
        | otherwise = txt (displayEditorText row)

drawComposerStatus :: AppState -> Widget Name
drawComposerStatus state =
    hBox $
        intersperse (withAttr Theme.mutedAttr (txt " · ")) $
            modelAndEffort
                <> [ modeControl
                   | not (Text.null mode)
                   ]
                <> [ if prompt.promptAccountSelectable
                        then accountControl
                        else withAttr Theme.mutedAttr (txt account)
                   | not (Text.null account)
                   ]
  where
    prompt = state.appUi.uiPrompt
    mode = prompt.promptMode
    account = prompt.promptAccount
    modelControl =
        clickable ComposerModel $
            forceAttr
                (controlAttr state ComposerModel Theme.controlLinkAttr)
                (txt prompt.promptModel)
    effortControl =
        clickable ComposerEffort $
            forceAttr
                (controlAttr state ComposerEffort Theme.controlLinkAttr)
                (txt ("(" <> prompt.promptEffort <> ")"))
    modelAndEffort
        | Text.null prompt.promptModel = []
        | Text.null prompt.promptEffort = [modelControl]
        | otherwise = [hBox [modelControl, txt " ", effortControl]]
    modeControl =
        clickable ComposerMode $
            forceAttr
                (controlAttr state ComposerMode Theme.controlLinkAttr)
                (txt mode)
    accountControl =
        clickable ComposerAccount $
            forceAttr
                (controlAttr state ComposerAccount Theme.controlLinkAttr)
                (txt account)

handlePromptControlClick
    :: ApplyLocalUiEvent
    -> (Text -> ReplLine)
    -> EventM Name AppState ()
handlePromptControlClick applyUiEvent choice = do
    state <- get
    let ui = state.appUi
        overlayOpen =
            maybe False (const True) state.appTextPrompt
                || maybe False (const True) state.appChoice
                || maybe False (const True) ui.uiPermission
    if ui.uiAwaitingInput && not overlayOpen
        then do
            liftIO $ atomically $
                appendFullscreenInput state.appRuntime.runtimeInput FullscreenInput
                    { fullscreenInputLine = choice ui.uiDraft
                    , fullscreenInputQueued = False
                    , fullscreenInputDisplay = Nothing
                    }
            applyUiEvent (UiSetAwaitingInput False) id
        else
            applyUiEvent
                (UiSetNotice
                    (Just
                        (warningNotice
                            "Prompt settings can be changed when input is ready.")))
                id

handleEffortControlClick
    :: ApplyLocalUiEvent
    -> EventM Name AppState ()
handleEffortControlClick applyUiEvent = do
    state <- get
    let ui = state.appUi
        overlayOpen =
            maybe False (const True) state.appTextPrompt
                || maybe False (const True) state.appChoice
                || maybe False (const True) ui.uiPermission
    if ui.uiAwaitingInput
        then handlePromptControlClick applyUiEvent ReplChooseEffort
        else if ui.uiRunning && not overlayOpen
            then do
                let efforts = reasoningEfforts
                    current = ui.uiPrompt.promptEffort
                    initial = fromMaybe 0 (elemIndex current efforts)
                    choose = \case
                        Just index
                            | index >= 0
                            , index < length efforts -> do
                                let level = efforts !! index
                                when (level /= current) $
                                    state.appRuntime.runtimeRestartEffort level
                        _ -> pure ()
                modify' \currentState ->
                    currentState
                        { appChoice = Just ChoiceOverlay
                            { choicePresentation = ChoiceDialog
                            , choiceTitle = "Reasoning effort"
                            , choiceBody =
                                "Changing effort will restart the current turn."
                            , choiceIndex = initial
                            , choiceRows = [(effort, "") | effort <- efforts]
                            , choiceCloseOnTurnEnd = True
                            }
                        , appChoiceReply = Just choose
                        }
                vScrollToBeginning (viewportScroll OverlayViewport)
            else
                applyUiEvent
                    (UiSetNotice
                        (Just
                            (warningNotice
                                "Prompt settings cannot be changed right now.")))
                    id

handleControlMouseDown :: Name -> EventM Name AppState ()
handleControlMouseDown name =
    modify' \state ->
        state
            { appHoveredControl = Just name
            , appPressedControl = case state.appPressedControl of
                Nothing -> Just name
                pressed -> pressed
            }

handleControlMouseUp
    :: Name
    -> EventM Name AppState ()
    -> EventM Name AppState ()
handleControlMouseUp name action = do
    state <- get
    let activate = state.appPressedControl == Just name
    modify' \current ->
        current
            { appHoveredControl =
                if activate then Nothing else Just name
            , appPressedControl = Nothing
            }
    when activate action

activateSlashAt
    :: ApplyLocalUiEvent
    -> EventM Name AppState CtrlCDecision
    -> (Direction -> EventM Name AppState ())
    -> Int
    -> EventM Name AppState ()
activateSlashAt
    applyUiEvent
    handleCtrlC
    scrollConversationPage
    index = do
    state <- get
    case currentSlashMenu state of
        Just menu
            | index >= 0
            , index < length menu.slashMenuSuggestions -> do
                modify' \current ->
                    current { appSlashIndex = index }
                handleComposerKey
                    applyUiEvent
                    handleCtrlC
                    scrollConversationPage
                    (V.EvKey V.KEnter [])
        _ -> pure ()

-- | Handle one composer key. The host supplies Ctrl-C policy and conversation
-- page scrolling because those actions also affect non-composer UI state.
handleComposerKey
    :: ApplyLocalUiEvent
    -> EventM Name AppState CtrlCDecision
    -> (Direction -> EventM Name AppState ())
    -> V.Event
    -> EventM Name AppState ()
handleComposerKey
    applyUiEvent
    handleCtrlC
    scrollConversationPage
    event = do
    state <- get
    let ui = state.appUi
        slashMenu = currentSlashMenu state
    case event of
        _ | Bridge.isSendNowKey event ->
            sendNow
        V.EvKey (V.KChar 'q') [V.MCtrl] ->
            submitRaw ReplEof
        V.EvKey (V.KChar 'd') [V.MCtrl]
            | Text.null ui.uiDraft ->
                submitRaw ReplEof
        V.EvKey (V.KChar 'd') modifiers
            | V.MCtrl `elem` modifiers ->
                deleteAfter
        V.EvKey (V.KChar 'c') [V.MCtrl] ->
            void handleCtrlC
        V.EvKey V.KEsc [] ->
            case composerEscapeAction
                ui.uiAwaitingInput
                (maybe False (const True) slashMenu) of
                EscapeCancelTurn ->
                    cancelOrClear
                EscapeDismissSlashMenu ->
                    modify' \current ->
                        current { appSlashDismissed = True }
                EscapeClearDraft ->
                    cancelOrClear
        V.EvKey V.KBackTab []
            | ui.uiAwaitingInput ->
                submitRaw (ReplCycleMode ui.uiDraft)
        V.EvKey V.KUp []
            | Just menu <- slashMenu
            , not (null menu.slashMenuSuggestions) ->
                moveSlash (-1) (length menu.slashMenuSuggestions)
        V.EvKey V.KUp [] ->
            moveHistory 1
        V.EvKey V.KDown []
            | Just menu <- slashMenu
            , not (null menu.slashMenuSuggestions) ->
                moveSlash 1 (length menu.slashMenuSuggestions)
        V.EvKey V.KDown [] ->
            moveHistory (-1)
        V.EvKey (V.KChar '\t') [] ->
            case slashMenu of
                Just menu -> acceptSlash menu
                Nothing ->
                    when (not (Seq.null ui.uiBlocks)) $
                        modifyUi (UiFocusChanged FocusScrollback)
        V.EvKey V.KEnter [V.MShift] ->
            insertText "\n"
        V.EvKey V.KEnter [] ->
            case slashMenu of
                Just menu -> handleSlashEnter menu
                Nothing -> submitDraft
        V.EvKey V.KBS [] ->
            deleteBefore
        V.EvKey V.KBS modifiers
            | V.MMeta `elem` modifiers
                || V.MAlt `elem` modifiers ->
                deletePreviousWord
        V.EvKey (V.KChar 'w') modifiers
            | V.MCtrl `elem` modifiers ->
                deletePreviousWord
        V.EvKey (V.KChar 'u') modifiers
            | V.MCtrl `elem` modifiers ->
                deleteCurrentLine
        V.EvKey (V.KChar 'k') modifiers
            | V.MCtrl `elem` modifiers ->
                deleteLineEnd
        V.EvKey (V.KChar 'y') modifiers
            | V.MCtrl `elem` modifiers ->
                insertKillBuffer
        V.EvKey (V.KChar 'l') modifiers
            | V.MCtrl `elem` modifiers ->
                invalidateCache
        V.EvKey (V.KChar 'a') modifiers
            | V.MCtrl `elem` modifiers ->
                setCursor (lineStartCursor ui.uiDraft ui.uiCursor)
        V.EvKey (V.KChar 'e') modifiers
            | V.MCtrl `elem` modifiers ->
                setCursor (lineEndCursor ui.uiDraft ui.uiCursor)
        V.EvKey (V.KChar 'b') modifiers
            | V.MCtrl `elem` modifiers ->
                moveCursor (-1)
        V.EvKey (V.KChar 'f') modifiers
            | V.MCtrl `elem` modifiers ->
                moveCursor 1
        V.EvKey (V.KChar 'v') modifiers
            | V.MCtrl `elem` modifiers
                || V.MMeta `elem` modifiers -> do
                clipboardText <- liftIO readClipboardText
                case nonEmptyClipboardText clipboardText of
                    Just text -> insertPastedText text
                    Nothing ->
                        submitRaw (ReplClipboardPaste ui.uiDraft Nothing)
        V.EvKey V.KDel [] ->
            deleteAfter
        V.EvKey V.KLeft modifiers
            | V.MMeta `elem` modifiers
                || V.MAlt `elem` modifiers ->
                setCursor (moveWordLeft ui.uiDraft ui.uiCursor)
        V.EvKey V.KRight modifiers
            | V.MMeta `elem` modifiers
                || V.MAlt `elem` modifiers ->
                setCursor (moveWordRight ui.uiDraft ui.uiCursor)
        V.EvKey V.KLeft [] ->
            moveCursor (-1)
        V.EvKey V.KRight [] ->
            moveCursor 1
        V.EvKey V.KHome [] ->
            setCursor (lineStartCursor ui.uiDraft ui.uiCursor)
        V.EvKey V.KEnd [] ->
            setCursor (lineEndCursor ui.uiDraft ui.uiCursor)
        V.EvKey V.KPageUp [] ->
            scrollConversationPage Up
        V.EvKey V.KPageDown [] ->
            scrollConversationPage Down
        V.EvKey (V.KChar character) [] ->
            insertText (Text.singleton character)
        V.EvPaste bytes ->
            case decodePaste bytes of
                pasted
                    | Text.null pasted ->
                        submitRaw (ReplClipboardPaste ui.uiDraft Nothing)
                    | otherwise -> insertPastedText pasted
        _ -> pure ()
  where
    submitRaw replLine = do
        state <- get
        enqueueInput state replLine Nothing False

    submitDraft = do
        state <- get
        let draft = state.appUi.uiDraft
            attachmentCount =
                state.appUi.uiPrompt.promptAttachments
        case submissionPromptText attachmentCount draft of
            Nothing -> pure ()
            Just text -> submitText state text state.appPasted

    submitText state text pasted = do
        liftIO (appendReplHistory text)
        enqueueInput
            state
            (if pasted then ReplPasted text else ReplText text)
            (Just text)
            True
        modify' \current ->
            current
                { appPasted = False
                , appHistory = text : current.appHistory
                , appHistoryIndex = Nothing
                , appHistoryDraft = ""
                }
        vScrollToEnd (viewportScroll ConversationViewport)

    sendNow = do
        state <- get
        let ui = state.appUi
            draft = ui.uiDraft
        when ui.uiRunning $
            if Text.null (Text.strip draft)
                then
                    if Seq.null ui.uiQueuedInputs
                        then modifyUi
                            (UiSetNotice
                                (Just
                                    (warningNotice
                                        "There is no queued prompt to send now.")))
                        else do
                            modifyUi
                                (UiSetNotice
                                    (Just
                                        (warningNotice
                                            "Cancelling the current turn; sending the queued prompt next…")))
                            liftIO state.appRuntime.runtimeCancel
                else do
                    liftIO (appendReplHistory draft)
                    liftIO $ atomically $
                        promoteFullscreenInput
                            state.appRuntime.runtimeInput
                            FullscreenInput
                                { fullscreenInputLine =
                                    if state.appPasted
                                        then ReplPasted draft
                                        else ReplText draft
                                , fullscreenInputQueued = True
                                , fullscreenInputDisplay = Just draft
                                }
                    applyUiEvent
                        (UiInputPromoted draft)
                        \current ->
                            current
                                { appPasted = False
                                , appHistory = draft : current.appHistory
                                , appHistoryIndex = Nothing
                                , appHistoryDraft = ""
                                , appSlashIndex = 0
                                , appSlashDismissed = False
                                }
                    liftIO state.appRuntime.runtimeCancel
                    vScrollToEnd (viewportScroll ConversationViewport)

    enqueueInput state replLine display clearDraft = do
        let queued = not state.appUi.uiAwaitingInput
            event =
                if queued
                    then UiInputQueued <$> display
                    else Just
                        (if clearDraft
                            then UiDraftSubmitted
                            else UiSetAwaitingInput False)
            update current =
                current
                    { appSlashIndex = 0
                    , appSlashDismissed = False
                    }
        case event of
            Nothing -> modify' update
            Just uiEvent -> applyUiEvent uiEvent update
        liftIO $ atomically $
            appendFullscreenInput state.appRuntime.runtimeInput FullscreenInput
                { fullscreenInputLine = replLine
                , fullscreenInputQueued = queued
                , fullscreenInputDisplay = display
                }

    cancelOrClear = do
        state <- get
        if not state.appUi.uiAwaitingInput
            then do
                liftIO state.appRuntime.runtimeCancel
                modifyUi
                    (UiSetNotice (Just (progressNotice "Cancelling…")))
            else
                modifyUi (UiSetDraft "" 0)

    insertText inserted = do
        state <- get
        let ui = state.appUi
            before = Text.take ui.uiCursor ui.uiDraft
            after = Text.drop ui.uiCursor ui.uiDraft
        modifyUiResetSlash $
            UiSetDraft
                (before <> inserted <> after)
                (ui.uiCursor + Text.length inserted)

    insertPastedText inserted = do
        insertText inserted
        modify' \current -> current { appPasted = True }

    deleteBefore = do
        state <- get
        let ui = state.appUi
        when (ui.uiCursor > 0) do
            let before = Text.take (ui.uiCursor - 1) ui.uiDraft
                after = Text.drop ui.uiCursor ui.uiDraft
            modifyUiResetSlash
                (UiSetDraft (before <> after) (ui.uiCursor - 1))

    deleteAfter = do
        state <- get
        let ui = state.appUi
        when (ui.uiCursor < Text.length ui.uiDraft) do
            let before = Text.take ui.uiCursor ui.uiDraft
                after = Text.drop (ui.uiCursor + 1) ui.uiDraft
            modifyUiResetSlash
                (UiSetDraft (before <> after) ui.uiCursor)

    deletePreviousWord = do
        state <- get
        let old = state.appUi.uiDraft
            oldCursor = state.appUi.uiCursor
            (next, cursor) =
                deleteWordBefore state.appUi.uiDraft state.appUi.uiCursor
            killed =
                Text.take (oldCursor - cursor) (Text.drop cursor old)
        modifyUiWithKill killed (UiSetDraft next cursor)

    deleteLineEnd = do
        state <- get
        let old = state.appUi.uiDraft
            oldCursor = state.appUi.uiCursor
            (next, cursor) =
                deleteToLineEnd state.appUi.uiDraft state.appUi.uiCursor
            killedLength = Text.length old - Text.length next
            killed = Text.take killedLength (Text.drop oldCursor old)
        modifyUiWithKill killed (UiSetDraft next cursor)

    deleteCurrentLine = do
        state <- get
        let old = state.appUi.uiDraft
            oldCursor = state.appUi.uiCursor
            (next, cursor) =
                deleteToLineStart state.appUi.uiDraft state.appUi.uiCursor
            killed =
                Text.take (oldCursor - cursor) (Text.drop cursor old)
        modifyUiWithKill killed (UiSetDraft next cursor)

    insertKillBuffer = do
        state <- get
        when (not (Text.null state.appKillBuffer)) $
            insertText state.appKillBuffer

    moveCursor delta = do
        state <- get
        setCursor (state.appUi.uiCursor + delta)

    setCursor cursor =
        get >>= \current ->
            applyUiEvent
                (UiSetDraft current.appUi.uiDraft cursor)
                \state -> state { appSlashIndex = 0 }

    modifyUi uiEvent =
        applyUiEvent uiEvent id

    modifyUiResetSlash uiEvent =
        applyUiEvent uiEvent \state ->
            state
                { appSlashIndex = 0
                , appSlashDismissed = False
                , appHistoryIndex = Nothing
                , appHistoryDraft =
                    case uiEvent of
                        UiSetDraft text _ -> text
                        _ -> state.appHistoryDraft
                }

    modifyUiWithKill killed uiEvent =
        applyUiEvent uiEvent \state ->
            state
                { appSlashIndex = 0
                , appSlashDismissed = False
                , appKillBuffer = killed
                , appHistoryIndex = Nothing
                , appHistoryDraft =
                    case uiEvent of
                        UiSetDraft text _ -> text
                        _ -> state.appHistoryDraft
                }

    moveHistory delta = do
        state <- get
        let (text, index, draft) =
                Bridge.historyMove
                    delta
                    state.appHistory
                    state.appHistoryIndex
                    state.appUi.uiDraft
                    state.appHistoryDraft
        applyUiEvent
            (UiSetDraft text (Text.length text))
            \currentState ->
                currentState
                    { appHistoryIndex = index
                    , appHistoryDraft = draft
                    , appSlashIndex = 0
                    , appSlashDismissed = False
                    }

    moveSlash delta count =
        modify' \current ->
            current
                { appSlashIndex =
                    (current.appSlashIndex + delta) `mod` count
                }

    acceptSlash menu = do
        current <- get
        case selectedSlashSuggestion current menu of
            Nothing -> pure ()
            Just suggestion -> acceptSlashSuggestion menu suggestion

    handleSlashEnter menu = do
        current <- get
        case selectedSlashSuggestion current menu of
            Nothing -> submitDraft
            Just suggestion
                | Text.strip current.appUi.uiDraft
                    == suggestion.slashSuggestionDisplay ->
                        submitDraft
                | suggestion.slashSuggestionTakesArguments ->
                    acceptSlashSuggestion menu suggestion
                | otherwise -> do
                    let next = slashReplacement
                            current.appUi.uiDraft
                            menu
                            suggestion
                    submitText current next False

    acceptSlashSuggestion menu suggestion = do
        current <- get
        let next = slashReplacement
                current.appUi.uiDraft
                menu
                suggestion
            cursor =
                menu.slashMenuReplaceStart
                    + Text.length suggestion.slashSuggestionReplacement
        modifyUiResetSlash (UiSetDraft next cursor)

applyComposerUiEvent :: UiEvent -> AppState -> AppState
applyComposerUiEvent uiEvent state =
    state
        { appSlashDismissed = case uiEvent of
            UiSetDraft _ _ -> False
            _ -> state.appSlashDismissed
        , appPasted = case uiEvent of
            UiSetDraft _ _ -> False
            _ -> state.appPasted
        , appHistoryIndex = case uiEvent of
            UiSetDraft _ _ -> Nothing
            _ -> state.appHistoryIndex
        , appHistoryDraft = case uiEvent of
            UiSetDraft text _ -> text
            _ -> state.appHistoryDraft
        }

currentSlashMenu :: AppState -> Maybe SlashMenu
currentSlashMenu state
    | state.appSlashDismissed = Nothing
    | otherwise =
        slashMenuForWithSkillsAndModels
            state.appSkillCommands
            state.appModelIds
            state.appUi.uiDraft
            state.appUi.uiCursor

data ComposerEscapeAction
    = EscapeCancelTurn
    | EscapeDismissSlashMenu
    | EscapeClearDraft
    deriving (Eq, Show)

-- | During a running turn Esc keeps its advertised cancellation meaning,
-- even when the next-message draft happens to open the slash menu.
composerEscapeAction :: Bool -> Bool -> ComposerEscapeAction
composerEscapeAction awaitingInput hasSlashMenu
    | not awaitingInput = EscapeCancelTurn
    | hasSlashMenu = EscapeDismissSlashMenu
    | otherwise = EscapeClearDraft

selectedSlashSuggestion
    :: AppState
    -> SlashMenu
    -> Maybe SlashSuggestion
selectedSlashSuggestion state menu =
    case menu.slashMenuSuggestions of
        [] -> Nothing
        suggestions ->
            Just
                (suggestions
                    !! (state.appSlashIndex `mod` length suggestions))

slashReplacement
    :: Text
    -> SlashMenu
    -> SlashSuggestion
    -> Text
slashReplacement draft menu suggestion =
    let before = Text.take menu.slashMenuReplaceStart draft
        after = Text.drop menu.slashMenuReplaceEnd draft
    in before <> suggestion.slashSuggestionReplacement <> after
