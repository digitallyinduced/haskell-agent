-- | Fullscreen prompt composer rendering, editing, and input buffering.
module Agent.CLI.TUI.Composer
    ( ComposerEscapeAction(..)
    , KillDirection(..)
    , activateSlashAt
    , appendFullscreenInput
    , applyComposerUiEvent
    , combineKill
    , composerEscapeAction
    , composerScrollbackAvailable
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
    , immediateBtwQuestion
    , isKillKey
    , newFullscreenInputBuffer
    , prepareBracketedPaste
    , promoteFullscreenInput
    , queuedFullscreenInputDisplays
    , readFullscreenInputs
    , slashMenuWindowStart
    , takeFullscreenInput
    , takeFullscreenInputOr
    , verticalCursorMove
    , wrapDraft
    , wrapDraftWindow
    ) where

import Agent.CLI.Clipboard
    ( nonEmptyClipboardText
    , readClipboardText
    )
import Agent.CLI.Dictation (dictate)
import Agent.CLI.Command
    ( ReplAction(..)
    , SlashMenu(..)
    , SlashSuggestion(..)
    , parseReplLine
    , slashMenuForCatalog
    )
import Agent.CLI.Input
    ( ReplLine(..)
    , appendReplHistory
    , displayEditorText
    , submissionPromptText
    , truncateDisplayText
    )
import Agent.CLI.Interrupt (CtrlCDecision)
import Agent.CLI.Options (reasoningEfforts)
import qualified Agent.CLI.TUI.Bridge as Bridge
import Agent.CLI.TUI.Composer.Buffer
import Agent.CLI.TUI.Composer.Edit
import Agent.CLI.TUI.History (HistoryWindow, historyWindowHasBlocks)
import Agent.CLI.TUI.Types
import qualified Agent.TUI.Theme as Theme
import Agent.TUI.Model
import Agent.TUI.TextWidth
    ( nextGraphemeBoundary
    , previousGraphemeBoundary
    , terminalTextImage
    )
import Brick
import Brick.BChan (writeBChan)
import qualified Brick.Types as B
import qualified Brick.Widgets.Border as Border
import Brick.Widgets.Border.Style (unicodeRounded)
import qualified Brick.Widgets.Border.Style as BorderStyle
import Control.Concurrent.STM (atomically)
import Control.Exception.Safe (tryAny)
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

composerScrollbackAvailable :: UiState -> HistoryWindow -> Bool
composerScrollbackAvailable ui history =
    not (Seq.null ui.uiBlocks) || historyWindowHasBlocks history

drawSlashMenu :: AppState -> Widget Name
drawSlashMenu state = case currentSlashMenu state of
    Nothing -> emptyWidget
    Just menu ->
        let allSuggestions = menu.slashMenuSuggestions
            count = length allSuggestions
            selected
                | count == 0 = 0
                | otherwise = state.appSlashIndex `mod` count
            start = slashMenuWindowStart visibleSlashRows count selected
            suggestions = take visibleSlashRows (drop start allSuggestions)
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
            suggestions@(_ : _)
                | all
                    (("$" `Text.isPrefixOf`)
                        . (.slashSuggestionDisplay))
                    suggestions ->
                        " Skills "
            _ -> " Commands "

visibleSlashRows :: Int
visibleSlashRows = 6

-- | First visible row of the slash menu window. The selection stays roughly
-- centered once the menu scrolls, so moving through a long menu shifts the
-- window at the edges instead of pinning the highlight to the first row.
slashMenuWindowStart :: Int -> Int -> Int -> Int
slashMenuWindowStart visible count selected
    | count <= visible = 0
    | otherwise =
        max 0 (min (selected - (visible - 1) `div` 2) (count - visible))

drawSlashRow :: Int -> Int -> SlashSuggestion -> Widget Name
drawSlashRow selected index suggestion =
    let prefix = if selected == index then "❯ " else "  "
        row =
            hBox
                [ terminalTxt
                    (prefix <> suggestion.slashSuggestionDisplay)
                , vLimit 1 (fill ' ')
                , withAttr Theme.mutedAttr
                    (terminalTxt suggestion.slashSuggestionSummary)
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
                        , withAttr Theme.mutedAttr
                            (terminalTxt (queuePreview next))
                        ]

queuePreview :: Text -> Text
queuePreview text =
    truncateDisplayText 100 (Text.unwords (Text.words text))

drawComposer :: AppState -> Widget Name
drawComposer appState =
    Widget Greedy Fixed do
        context <- getContext
        let focused = state.uiFocus == FocusComposer
            attr = if focused then Theme.borderActiveAttr else Theme.borderAttr
            draftWidth = max 1 (context.availWidth - composerDraftChromeWidth)
            draftLayout@(draftRows, _) =
                wrapDraftWindow
                    maxComposerRows
                    draftWidth
                    state.uiDraft
                    state.uiCursor
            bodyHeight = min maxComposerRows (length draftRows)
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
                                (renderDraft
                                    focused
                                    bodyHeight
                                    state
                                    draftLayout)
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

-- | Columns consumed around the draft text: the two border columns, the
-- one-column padding on each side, and the two-cell @❯ @ prompt. The draft is
-- wrapped once in 'drawComposer' at the remaining width and the rows are
-- passed to 'renderDraft', so body height and cursor placement always agree;
-- drift between this constant and the actual chrome only changes the wrap
-- width.
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

-- | Render the precomputed bounded draft layout. Wrapping happens once in
-- 'drawComposer', so the visible height and cursor row agree without scanning
-- the complete draft.
renderDraft
    :: Bool
    -> Int
    -> UiState
    -> ([Text], (Int, Int))
    -> Widget Name
renderDraft focused height state (rows, (row, column)) =
    padRight Max cursorContent
  where
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

    -- Empty visual rows still need one cell so Brick preserves their height
    -- and can place the insertion cursor on them.
    renderRow row
        | Text.null row = txt " "
        | otherwise = terminalTxt (displayEditorText row)

terminalTxt :: Text -> Widget n
terminalTxt text =
    B.Widget B.Fixed B.Fixed do
        context <- B.getContext
        attr <- B.lookupAttrName (B.ctxAttrName context)
        pure B.emptyResult
            { B.image = terminalTextImage attr text
            }

drawComposerStatus :: AppState -> Widget Name
drawComposerStatus state =
    hBox $
        intersperse (withAttr Theme.mutedAttr (txt " · ")) $
            accountLimit
                <> modelAndEffort
                <> [ modeControl
                   | not (Text.null mode)
                   ]
                <> [ if prompt.promptAccountSelectable
                        then accountControl
                        else withAttr Theme.mutedAttr (terminalTxt account)
                   | not (Text.null account)
                   ]
  where
    prompt = state.appUi.uiPrompt
    mode = prompt.promptMode
    account = prompt.promptAccount
    accountLimit =
        [ withAttr
            Theme.controlLinkAttr
            (terminalTxt limitStatus.promptLimitText)
        | limitStatus <- maybeToList prompt.promptLimitStatus
        ]
    modelControl =
        clickable ComposerModel $
            forceAttr
                (controlAttr state ComposerModel Theme.controlLinkAttr)
                (terminalTxt prompt.promptModel)
    effortControl =
        clickable ComposerEffort $
            forceAttr
                (controlAttr state ComposerEffort Theme.controlLinkAttr)
                (terminalTxt ("(" <> prompt.promptEffort <> ")"))
    modelAndEffort
        | Text.null prompt.promptModel = []
        | Text.null prompt.promptEffort = [modelControl]
        | otherwise = [hBox [modelControl, txt " ", effortControl]]
    modeControl =
        clickable ComposerMode $
            forceAttr
                (controlAttr state ComposerMode Theme.controlLinkAttr)
                (terminalTxt mode)
    accountControl =
        clickable ComposerAccount $
            forceAttr
                (controlAttr state ComposerAccount Theme.controlLinkAttr)
                (terminalTxt account)

    maybeToList = \case
        Nothing -> []
        Just value -> [value]

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

-- | An idle composer can hand bracketed paste classification to the main REPL,
-- which is already waiting for input. During a running turn that consumer is
-- blocked, so insert terminal text locally rather than leaving the draft behind
-- a persistent "Reading clipboard…" notice.
prepareBracketedPaste
    :: Bool
    -> Text
    -> Int
    -> Text
    -> (Text, Int, Maybe ReplLine)
prepareBracketedPaste awaitingInput draft cursor pasted =
    let boundedCursor = max 0 (min (Text.length draft) cursor)
        before = Text.take boundedCursor draft
        after = Text.drop boundedCursor draft
        pastedDraft = before <> pasted <> after
        pastedCursor = boundedCursor + Text.length pasted
    in if Text.null pasted
        then
            ( draft
            , boundedCursor
            , Just (ReplClipboardPaste draft Nothing)
            )
        else if awaitingInput
        then
            ( draft
            , boundedCursor
            , Just (ReplClipboardPasteOrText draft pasted pastedDraft)
            )
        else (pastedDraft, pastedCursor, Nothing)

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
        V.EvKey (V.KChar 'q') modifiers
            | V.MCtrl `elem` modifiers ->
                submitRaw ReplEof
        V.EvKey (V.KChar 'd') modifiers
            | V.MCtrl `elem` modifiers
            , Text.null ui.uiDraft ->
                submitRaw ReplEof
        V.EvKey (V.KChar 'd') modifiers
            | V.MCtrl `elem` modifiers ->
                deleteAfter
        V.EvKey (V.KChar 'd') modifiers
            | V.MMeta `elem` modifiers
                || V.MAlt `elem` modifiers ->
                killWordAfter
        V.EvKey (V.KChar 'c') modifiers
            | V.MCtrl `elem` modifiers ->
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
            case verticalCursorMove (-1) ui.uiDraft ui.uiCursor of
                Just cursor -> setCursor cursor
                Nothing -> moveHistory 1
        V.EvKey V.KDown []
            | Just menu <- slashMenu
            , not (null menu.slashMenuSuggestions) ->
                moveSlash 1 (length menu.slashMenuSuggestions)
        V.EvKey V.KDown [] ->
            case verticalCursorMove 1 ui.uiDraft ui.uiCursor of
                Just cursor -> setCursor cursor
                Nothing -> moveHistory (-1)
        V.EvKey (V.KChar '\t') [] ->
            case slashMenu of
                Just menu -> acceptSlash menu
                Nothing ->
                    when
                        (composerScrollbackAvailable
                            ui
                            state.appHistoryWindow) $
                        modifyUi (UiFocusChanged FocusScrollback)
        V.EvKey V.KEnter modifiers
            | V.MShift `elem` modifiers ->
                insertText "\n"
        V.EvKey V.KEnter [] ->
            case slashMenu of
                Just menu -> handleSlashEnter menu
                Nothing -> submitDraft
        V.EvKey V.KBS [] ->
            deleteBefore
        V.EvKey V.KBS modifiers
            | any (`elem` modifiers) [V.MMeta, V.MAlt, V.MCtrl] ->
                killPreviousWord
        V.EvKey (V.KChar 'w') modifiers
            | V.MCtrl `elem` modifiers ->
                killPreviousWord
        V.EvKey (V.KChar 'u') modifiers
            | V.MCtrl `elem` modifiers ->
                killLineStart
        V.EvKey (V.KChar 'k') modifiers
            | V.MCtrl `elem` modifiers ->
                killLineEnd
        V.EvKey (V.KChar 'y') modifiers
            | V.MCtrl `elem` modifiers ->
                insertKillBuffer
        V.EvKey (V.KChar 'l') modifiers
            | V.MCtrl `elem` modifiers ->
                invalidateCache
        V.EvKey (V.KChar 'r') modifiers
            | V.MCtrl `elem` modifiers ->
                startDictation
        V.EvKey (V.KChar '\DC2') _ ->
            startDictation
        V.EvKey (V.KChar 'a') modifiers
            | V.MCtrl `elem` modifiers ->
                setCursor (lineStartCursor ui.uiDraft ui.uiCursor)
        V.EvKey (V.KChar 'e') modifiers
            | V.MCtrl `elem` modifiers ->
                setCursor (lineEndCursor ui.uiDraft ui.uiCursor)
        V.EvKey (V.KChar 'b') modifiers
            | V.MCtrl `elem` modifiers ->
                moveCursor (-1)
        V.EvKey (V.KChar 'b') modifiers
            | V.MMeta `elem` modifiers
                || V.MAlt `elem` modifiers ->
                setCursor (moveWordLeft ui.uiDraft ui.uiCursor)
        V.EvKey (V.KChar 'f') modifiers
            | V.MCtrl `elem` modifiers ->
                moveCursor 1
        V.EvKey (V.KChar 'f') modifiers
            | V.MMeta `elem` modifiers
                || V.MAlt `elem` modifiers ->
                setCursor (moveWordRight ui.uiDraft ui.uiCursor)
        V.EvKey (V.KChar '_') modifiers
            | V.MCtrl `elem` modifiers ->
                undoEdit
        V.EvKey (V.KChar '\US') _ ->
            undoEdit
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
        V.EvPaste bytes -> do
            let pasted = decodePaste bytes
                (pastedDraft, pastedCursor, clipboardInput) =
                    prepareBracketedPaste
                        ui.uiAwaitingInput
                        ui.uiDraft
                        ui.uiCursor
                        pasted
            case clipboardInput of
                Nothing -> do
                    modifyUiResetSlash
                        (UiSetDraft pastedDraft pastedCursor)
                    modify' \current -> current { appPasted = True }
                Just replLine -> do
                    when (not (Text.null pasted)) $
                        modifyUi
                            (UiSetNotice
                                (Just (progressNotice "Reading clipboard…")))
                    submitRaw replLine
        _ -> pure ()
    -- Only a kill directly followed by another kill accumulates into the
    -- kill buffer; any other key breaks the chain.
    modify' \current -> current { appKillChain = isKillKey event }
  where
    startDictation = do
        current <- get
        suspendAndResume do
            result <- tryAny dictate
            writeBChan
                current.appRuntime.runtimeEvents
                (AppDictationFinished
                    (case result of
                        Left err -> Left (Text.pack (show err))
                        Right transcript -> Right transcript))
            pure current

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
        let replLine = if pasted then ReplPasted text else ReplText text
        case immediateBtwQuestion state.appUi replLine of
            Just question -> do
                applyUiEvent UiDraftSubmitted \current ->
                    current
                        { appSlashIndex = 0
                        , appSlashDismissed = False
                        , appUndo = []
                        }
                liftIO (state.appRuntime.runtimeBtw question)
            Nothing ->
                enqueueInput state replLine (Just text) True
        modify' \current ->
            current
                { appPasted = False
                , appHistory = Bridge.pushHistory text current.appHistory
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
                                , appHistory =
                                    Bridge.pushHistory draft current.appHistory
                                , appHistoryIndex = Nothing
                                , appHistoryDraft = ""
                                , appSlashIndex = 0
                                , appSlashDismissed = False
                                , appUndo = []
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
                    , appUndo =
                        -- A submitted prompt leaves an empty composer; its
                        -- edit steps are no longer undoable.
                        if clearDraft || maybe False (const True) display
                            then []
                            else current.appUndo
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
            else do
                -- Esc must not destroy a typed draft irrecoverably: stash it
                -- in the kill buffer so Ctrl-Y (or Ctrl-_) restores it.
                let draft = state.appUi.uiDraft
                if Text.null draft
                    then modifyUi (UiSetDraft "" 0)
                    else modifyUiWithKill
                        KillBackward
                        draft
                        (UiSetDraft "" 0)

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
            let start =
                    previousGraphemeBoundary
                        ui.uiDraft
                        ui.uiCursor
                before = Text.take start ui.uiDraft
                after = Text.drop ui.uiCursor ui.uiDraft
            modifyUiResetSlash
                (UiSetDraft (before <> after) start)

    deleteAfter = do
        state <- get
        let ui = state.appUi
        when (ui.uiCursor < Text.length ui.uiDraft) do
            let before = Text.take ui.uiCursor ui.uiDraft
                after =
                    Text.drop
                        (nextGraphemeBoundary ui.uiDraft ui.uiCursor)
                        ui.uiDraft
            modifyUiResetSlash
                (UiSetDraft (before <> after) ui.uiCursor)

    killPreviousWord = do
        state <- get
        let old = state.appUi.uiDraft
            oldCursor = state.appUi.uiCursor
            (next, cursor) =
                deleteWordBefore state.appUi.uiDraft state.appUi.uiCursor
            killed =
                Text.take (oldCursor - cursor) (Text.drop cursor old)
        modifyUiWithKill KillBackward killed (UiSetDraft next cursor)

    killWordAfter = do
        state <- get
        let old = state.appUi.uiDraft
            oldCursor = state.appUi.uiCursor
            (next, cursor) =
                deleteWordAfter state.appUi.uiDraft state.appUi.uiCursor
            killedLength = Text.length old - Text.length next
            killed = Text.take killedLength (Text.drop oldCursor old)
        modifyUiWithKill KillForward killed (UiSetDraft next cursor)

    killLineEnd = do
        state <- get
        let old = state.appUi.uiDraft
            oldCursor = state.appUi.uiCursor
            (next, cursor) =
                deleteToLineEnd state.appUi.uiDraft state.appUi.uiCursor
            killedLength = Text.length old - Text.length next
            killed = Text.take killedLength (Text.drop oldCursor old)
        modifyUiWithKill KillForward killed (UiSetDraft next cursor)

    killLineStart = do
        state <- get
        let old = state.appUi.uiDraft
            oldCursor = state.appUi.uiCursor
            (next, cursor) =
                deleteToLineStart state.appUi.uiDraft state.appUi.uiCursor
            killed =
                Text.take (oldCursor - cursor) (Text.drop cursor old)
        modifyUiWithKill KillBackward killed (UiSetDraft next cursor)

    undoEdit = do
        state <- get
        case state.appUndo of
            [] -> pure ()
            (text, cursor) : rest ->
                applyUiEvent (UiSetDraft text cursor) \current ->
                    current
                        { appUndo = rest
                        , appSlashIndex = 0
                        , appSlashDismissed = False
                        , appHistoryIndex = Nothing
                        , appHistoryDraft = text
                        }

    insertKillBuffer = do
        state <- get
        when (not (Text.null state.appKillBuffer)) $
            insertText state.appKillBuffer

    moveCursor :: Int -> EventM Name AppState ()
    moveCursor delta = do
        state <- get
        let ui = state.appUi
            cursor
                | delta < 0 =
                    previousGraphemeBoundary ui.uiDraft ui.uiCursor
                | delta > 0 =
                    nextGraphemeBoundary ui.uiDraft ui.uiCursor
                | otherwise = ui.uiCursor
        setCursor cursor

    setCursor cursor =
        get >>= \current ->
            applyUiEvent
                (UiSetDraft current.appUi.uiDraft cursor)
                \state -> state { appSlashIndex = 0 }

    modifyUi uiEvent =
        applyUiEvent uiEvent id

    modifyUiResetSlash uiEvent = do
        old <- get
        applyUiEvent uiEvent \state ->
            (pushUndo old uiEvent state)
                { appSlashIndex = 0
                , appSlashDismissed = False
                , appHistoryIndex = Nothing
                , appHistoryDraft =
                    case uiEvent of
                        UiSetDraft text _ -> text
                        _ -> state.appHistoryDraft
                }

    modifyUiWithKill direction killed uiEvent = do
        old <- get
        applyUiEvent uiEvent \state ->
            (pushUndo old uiEvent state)
                { appSlashIndex = 0
                , appSlashDismissed = False
                , appKillBuffer =
                    if Text.null killed
                        then state.appKillBuffer
                        else if old.appKillChain
                            then combineKill
                                direction
                                killed
                                state.appKillBuffer
                            else killed
                , appHistoryIndex = Nothing
                , appHistoryDraft =
                    case uiEvent of
                        UiSetDraft text _ -> text
                        _ -> state.appHistoryDraft
                }

    -- Record the pre-edit draft for Ctrl-_ when the edit changes the text.
    pushUndo old uiEvent state =
        case uiEvent of
            UiSetDraft text _
                | text /= old.appUi.uiDraft ->
                    state
                        { appUndo =
                            take undoLimit
                                ((old.appUi.uiDraft, old.appUi.uiCursor)
                                    : state.appUndo)
                        }
            _ -> state

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

-- | Whether a kill removed text before or after the cursor.
data KillDirection = KillBackward | KillForward
    deriving (Eq, Show)

-- | Merge a new kill into the existing kill buffer readline-style: backward
-- kills prepend, forward kills append, so a chain of kills restores as one
-- contiguous block on Ctrl-Y.
combineKill :: KillDirection -> Text -> Text -> Text
combineKill KillBackward killed buffer = killed <> buffer
combineKill KillForward killed buffer = buffer <> killed

-- | Keys that store deleted text in the kill buffer. Plain Backspace and
-- Ctrl-D delete without killing, matching readline.
isKillKey :: V.Event -> Bool
isKillKey = \case
    V.EvKey V.KBS modifiers ->
        any (`elem` modifiers) [V.MMeta, V.MAlt, V.MCtrl]
    V.EvKey (V.KChar 'w') modifiers -> V.MCtrl `elem` modifiers
    V.EvKey (V.KChar 'u') modifiers -> V.MCtrl `elem` modifiers
    V.EvKey (V.KChar 'k') modifiers -> V.MCtrl `elem` modifiers
    V.EvKey (V.KChar 'd') modifiers ->
        (V.MMeta `elem` modifiers || V.MAlt `elem` modifiers)
            && V.MCtrl `notElem` modifiers
    _ -> False

undoLimit :: Int
undoLimit = 200

-- | Side questions are independent requests and should not wait behind the
-- active turn's ordinary follow-up queue.
immediateBtwQuestion :: UiState -> ReplLine -> Maybe Text
immediateBtwQuestion ui replLine
    | not ui.uiRunning = Nothing
    | otherwise =
        case replLine of
            ReplText text -> fromText text
            ReplPasted text -> fromText text
            _ -> Nothing
  where
    fromText text =
        case parseReplLine text of
            ReplBtw question -> Just question
            _ -> Nothing

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
        slashMenuForCatalog
            state.appSlashCatalog
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
