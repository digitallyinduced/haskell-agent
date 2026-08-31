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
    , DictationKeyAction(..)
    , dictationKeyAction
    , dictationProgressNotice
    , draftCursorLocation
    , drawComposer
    , drawQueuedInputs
    , drawSlashMenu
    , fullscreenInputByteLimit
    , fullscreenInputCountLimit
    , handleComposerKey
    , handleDictationKey
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
    , steeringPrompt
    , takeFullscreenInput
    , takeFullscreenInputOr
    , requestDictationStop
    , verticalCursorMove
    , wrapDraft
    , wrapDraftWindow
    ) where

import Agent.CLI.Clipboard
    ( nonEmptyClipboardText
    , readClipboardText
    )
import Agent.CLI.Command
    ( ReplAction(..)
    , SlashMenu(..)
    , SlashSuggestion(..)
    , parseReplLine
    )
import Agent.CLI.Input
    ( ReplLine(..)
    , appendReplHistory
    , submissionPromptText
    )
import Agent.CLI.Interrupt (CtrlCDecision)
import qualified Agent.CLI.TUI.Bridge as Bridge
import Agent.CLI.TUI.Composer.Buffer
import Agent.CLI.TUI.Composer.Edit
import Agent.CLI.TUI.Composer.Logic
import Agent.CLI.TUI.Composer.Render
import Agent.CLI.TUI.Types
import Agent.TUI.Model
import Agent.TUI.TextWidth
    ( nextGraphemeBoundary
    , previousGraphemeBoundary
    )
import Brick
import Control.Concurrent (newEmptyMVar, takeMVar, tryPutMVar)
import Control.Concurrent.STM (atomically, writeTQueue)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (modify')
import Data.IORef (newIORef, writeIORef)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Vty as V

type ApplyLocalUiEvent =
    UiEvent
    -> (AppState -> AppState)
    -> EventM Name AppState ()

data DictationKeyAction
    = DictationCommit
    | DictationAbort
    deriving (Eq, Show)

dictationKeyAction :: V.Event -> Maybe DictationKeyAction
dictationKeyAction = \case
    V.EvKey V.KEnter [] -> Just DictationCommit
    V.EvKey V.KEsc [] -> Just DictationAbort
    V.EvKey (V.KChar 'r') modifiers
        | V.MCtrl `elem` modifiers ->
            Just DictationCommit
    V.EvKey (V.KChar '\DC2') _ ->
        Just DictationCommit
    V.EvKey (V.KChar 'c') modifiers
        | V.MCtrl `elem` modifiers ->
            Just DictationAbort
    _ -> Nothing

dictationProgressNotice :: Text -> UiNotice
dictationProgressNotice transcript =
    progressNotice $
        case Text.strip transcript of
            "" -> "Listening… Enter to stop · Esc to cancel"
            text ->
                "Listening… "
                    <> Text.takeEnd 80 (Text.unwords (Text.lines text))

requestDictationStop :: DictationSession -> Bool -> IO ()
requestDictationStop session abort = do
    when abort $ writeIORef session.dictationAbort True
    void (tryPutMVar session.dictationStop ())

handleDictationKey
    :: EventM Name AppState CtrlCDecision
    -> DictationSession
    -> V.Event
    -> EventM Name AppState ()
handleDictationKey handleCtrlC session event =
    case dictationKeyAction event of
        Just DictationCommit ->
            liftIO (requestDictationStop session False)
        Just DictationAbort -> do
            liftIO (requestDictationStop session True)
            case event of
                V.EvKey (V.KChar 'c') modifiers
                    | V.MCtrl `elem` modifiers ->
                        void handleCtrlC
                _ ->
                    pure ()
        Nothing ->
            pure ()

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
                || maybe False (const True) state.appMetaConsole
                || maybe False (const True) ui.uiPermission
    if ui.uiAwaitingInput && not overlayOpen
        then do
            queued <- liftIO $ atomically $
                appendFullscreenInput state.appRuntime.runtimeInput FullscreenInput
                    { fullscreenInputLine = choice ui.uiDraft
                    , fullscreenInputQueued = False
                    , fullscreenInputDisplay = Nothing
                    }
            case queued of
                Left message ->
                    applyUiEvent
                        (UiSetNotice (Just (warningNotice message)))
                        id
                Right () ->
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
                || maybe False (const True) state.appMetaConsole
                || maybe False (const True) ui.uiPermission
    if ui.uiAwaitingInput
        then handlePromptControlClick applyUiEvent ReplChooseEffort
        else if ui.uiRunning && not overlayOpen
            then do
                let efforts = ui.uiPrompt.promptEffortOptions
                    current = ui.uiPrompt.promptEffort
                    initial = fromMaybe 0 (elemIndex current efforts)
                    choose = \case
                        Just selection
                            | let index = selection.choiceSelectionIndex
                            , index >= 0
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
                            , choiceSearch = False
                            , choiceQuery = ""
                            , choiceAdjustments = Nothing
                            , choiceAdjustmentIndices = []
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

steeringPrompt :: UiState -> Bool -> Text -> Maybe (Bool, Text)
steeringPrompt ui pasted text
    | not ui.uiRunning = Nothing
    | otherwise =
        case parseReplLine text of
            ReplPrompt prompt -> Just (pasted, prompt)
            ReplExpandedPrompt _ prompt -> Just (pasted, prompt)
            _ -> Nothing

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
                EscapePreserveDraft ->
                    pure ()
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
        case current.appDictation of
            Just session ->
                liftIO (requestDictationStop session False)
            Nothing -> do
                stop <- liftIO newEmptyMVar
                abort <- liftIO (newIORef False)
                let session =
                        DictationSession
                            { dictationStop = stop
                            , dictationAbort = abort
                            }
                applyUiEvent
                    (UiSetNotice (Just (dictationProgressNotice "")))
                    \state -> state { appDictation = Just session }
                liftIO $ atomically $
                    writeTQueue
                        current.appRuntime.runtimeDictationJobs
                        DictationJob
                            { dictationJobWaitForStop = takeMVar stop
                            }

    submitRaw replLine = do
        state <- get
        void (enqueueInput state replLine Nothing False)

    submitDraft = do
        state <- get
        let draft = state.appUi.uiDraft
            attachmentCount =
                state.appUi.uiPrompt.promptAttachments
        case submissionPromptText attachmentCount draft of
            Nothing -> pure ()
            Just text -> submitText state text state.appPasted

    submitText state text pasted = do
        let replLine = if pasted then ReplPasted text else ReplText text
        accepted <- case immediateBtwQuestion state.appUi replLine of
            Just question -> do
                applyUiEvent UiDraftSubmitted \current ->
                    current
                        { appSlashIndex = 0
                        , appSlashDismissed = False
                        , appUndo = []
                        }
                _ <- liftIO (state.appRuntime.runtimeBtw question)
                pure True
            Nothing ->
                case steeringPrompt state.appUi pasted text of
                    Just (steeringPasted, prompt) -> do
                        result <- liftIO
                            (state.appRuntime.runtimeSteer
                                steeringPasted
                                prompt)
                        case result of
                            Left message -> do
                                applyUiEvent
                                    (UiSetNotice
                                        (Just (warningNotice message)))
                                    id
                                pure False
                            Right () -> do
                                applyUiEvent UiDraftSubmitted \current ->
                                    current
                                        { appSlashIndex = 0
                                        , appSlashDismissed = False
                                        , appUndo = []
                                        }
                                pure True
                    Nothing ->
                        enqueueInput state replLine (Just text) True
        when accepted do
            liftIO (appendReplHistory text)
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
                    promoted <- liftIO $ atomically $
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
                    case promoted of
                        Left message ->
                            modifyUi
                                (UiSetNotice
                                    (Just (warningNotice message)))
                        Right () -> do
                            liftIO (appendReplHistory draft)
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
                            vScrollToEnd
                                (viewportScroll ConversationViewport)

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
        result <- liftIO $ atomically $
            appendFullscreenInput state.appRuntime.runtimeInput FullscreenInput
                { fullscreenInputLine = replLine
                , fullscreenInputQueued = queued
                , fullscreenInputDisplay = display
                }
        case result of
            Left message -> do
                applyUiEvent
                    (UiSetNotice (Just (warningNotice message)))
                    id
                pure False
            Right () -> do
                case event of
                    Nothing -> modify' update
                    Just uiEvent -> applyUiEvent uiEvent update
                pure True

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
