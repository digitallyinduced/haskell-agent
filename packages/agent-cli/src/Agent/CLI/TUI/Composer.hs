-- | Fullscreen prompt composer rendering, editing, and input buffering.
module Agent.CLI.TUI.Composer
    ( ComposerEscapeAction(..)
    , ComposerPasteResult(..)
    , KillDirection(..)
    , activateSlashAt
    , appendFullscreenInput
    , closeFullscreenInputBuffer
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
    , dictationStartingNotice
    , dictationSessionIsRecording
    , draftCursorLocation
    , draftWindowStart
    , drawComposer
    , drawBackgroundTaskStatus
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
    , handleImageRemoveClick
    , imagePlaceholder
    , immediateBtwQuestion
    , immediateReplCommand
    , insertImagePlaceholders
    , isKillKey
    , newFullscreenInputBuffer
    , prepareBracketedPaste
    , processComposerPaste
    , removeImagePlaceholderAt
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
    ( appendBoundedImageAttachments
    , loadImagesFromPastedText
    , nonEmptyClipboardText
    , readClipboardImagesImageFirst
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
import Agent.Loop (ImageAttachment)
import qualified Agent.CLI.TUI.Bridge as Bridge
import Agent.CLI.TUI.Composer.Buffer
import Agent.CLI.TUI.Composer.Edit
import Agent.CLI.TUI.Composer.Logic
import Agent.CLI.TUI.Composer.Render
import Agent.CLI.TUI.ImagePreview
    ( prepareNativeTuiImagePreview
    , prepareTuiImagePreview
    )
import Agent.CLI.TUI.Types
import Agent.TUI.Model
import Agent.TUI.TextWidth
    ( nextGraphemeBoundary
    , previousGraphemeBoundary
    )
import Brick
import Control.Concurrent (MVar, isEmptyMVar, newEmptyMVar, readMVar, tryPutMVar)
import Control.Concurrent.STM (atomically, writeTQueue)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (modify')
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
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

dictationStartingNotice :: UiNotice
dictationStartingNotice =
    progressNotice "Starting microphone… Enter to stop · Esc to cancel"

-- | A ready event can arrive after stop or after the session has finished.
-- Keep the stop signal filled so consuming it cannot revive the listening UI.
dictationSessionIsRecording :: MVar () -> Maybe DictationSession -> IO Bool
dictationSessionIsRecording stop = \case
    Just session | session.dictationStop == stop -> do
        notStopped <- isEmptyMVar stop
        aborted <- readIORef session.dictationAbort
        pure (notStopped && not aborted)
    _ -> pure False

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
    :: (UiEvent -> EventM Name AppState ())
    -> EventM Name AppState CtrlCDecision
    -> DictationSession
    -> V.Event
    -> EventM Name AppState ()
handleDictationKey applyUiEvent handleCtrlC session event =
    case dictationKeyAction event of
        Just DictationCommit -> do
            liftIO (requestDictationStop session False)
            applyUiEvent $
                UiSetNotice (Just (progressNotice "Transcribing…"))
        Just DictationAbort -> do
            liftIO (requestDictationStop session True)
            applyUiEvent $
                UiSetNotice (Just (progressNotice "Cancelling dictation…"))
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
                    , fullscreenInputFromInbox = False
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

-- | Edit locally owned attachments immediately, but apply the corresponding
-- session mutation in input order after any earlier queued messages.
handleImageRemoveClick
    :: ApplyLocalUiEvent
    -> Int
    -> EventM Name AppState ()
handleImageRemoveClick applyUiEvent index = do
    state <- get
    let ui = state.appUi
        overlayOpen =
            maybe False (const True) state.appTextPrompt
                || maybe False (const True) state.appChoice
                || maybe False (const True) state.appMetaConsole
                || maybe False (const True) ui.uiPermission
    if state.appComposerOwnsImagePreviews && not overlayOpen
        then do
            previous <- liftIO (readIORef state.appRuntime.runtimeImagePreviews)
            if index < 0
                then pure ()
                else case splitAt index previous of
                  (_, []) -> pure ()
                  (before, (image, _) : after) -> do
                    queued <- liftIO $ atomically $
                        appendFullscreenInput state.appRuntime.runtimeInput FullscreenInput
                            { fullscreenInputLine = ReplRemoveCapturedImage image
                            , fullscreenInputQueued = True
                            , fullscreenInputDisplay = Nothing
                            , fullscreenInputFromInbox = False
                            }
                    case queued of
                        Left message ->
                            applyUiEvent (UiSetNotice (Just (warningNotice message))) id
                        Right () -> do
                            let pending = before <> after
                                (draft, cursor) =
                                    removeImagePlaceholderAt
                                        ui.uiDraft
                                        ui.uiCursor
                                        (index + 1)
                                        (length previous)
                            liftIO do
                                writeIORef state.appRuntime.runtimeImagePreviews pending
                                modifyIORef' state.appRuntime.runtimeImagePreviewRevision (+ 1)
                            modify' \current -> current
                                { appImagePreviews = map snd pending }
                            applyUiEvent
                                (UiSetPrompt ui.uiPrompt { promptAttachments = length pending })
                                id
                            modifyUiResetSlash applyUiEvent (UiSetDraft draft cursor)
                            applyUiEvent
                                (UiSetNotice (Just (successNotice "attachment removed")))
                                id
        else handlePromptControlClick applyUiEvent \keptDraft ->
            let (draft, _) =
                    removeImagePlaceholderAt
                        keptDraft
                        ui.uiCursor
                        (index + 1)
                        ui.uiPrompt.promptAttachments
            in ReplRemovePendingImage draft index

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
                        { appChoice = Just $ PendingDialog choose ChoiceOverlay
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
                            , choiceDynamic = Nothing
                            , choiceReply = Nothing
                            }
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
            , appHoveredLine = Nothing
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
            , appHoveredLine = Nothing
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
    | ui.uiPrompt.promptAttachments > 0 = Nothing
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
            sendNow applyUiEvent
        V.EvKey (V.KChar 'q') modifiers
            | V.MCtrl `elem` modifiers ->
                submitRaw applyUiEvent ReplEof
        V.EvKey (V.KChar 'd') modifiers
            | V.MCtrl `elem` modifiers
            , Text.null ui.uiDraft ->
                submitRaw applyUiEvent ReplEof
        V.EvKey (V.KChar 'd') modifiers
            | V.MCtrl `elem` modifiers ->
                deleteAfter applyUiEvent
        V.EvKey (V.KChar 'd') modifiers
            | V.MMeta `elem` modifiers
                || V.MAlt `elem` modifiers ->
                killWordAfter applyUiEvent
        V.EvKey (V.KChar 'c') modifiers
            | V.MCtrl `elem` modifiers ->
                void handleCtrlC
        V.EvKey V.KEsc [] ->
            case composerEscapeAction
                ui.uiAwaitingInput
                (maybe False (const True) slashMenu) of
                EscapeCancelTurn ->
                    cancelOrClear applyUiEvent
                EscapeDismissSlashMenu ->
                    modify' \current ->
                        current { appSlashDismissed = True }
                EscapePreserveDraft ->
                    pure ()
        V.EvKey V.KBackTab []
            | ui.uiAwaitingInput ->
                submitRaw applyUiEvent (ReplCycleMode ui.uiDraft)
        V.EvKey V.KUp []
            | Just menu <- slashMenu
            , not (null menu.slashMenuSuggestions) ->
                moveSlash (-1) (length menu.slashMenuSuggestions)
        V.EvKey V.KUp [] ->
            case verticalCursorMove (-1) ui.uiDraft ui.uiCursor of
                Just cursor -> setCursor applyUiEvent cursor
                Nothing -> moveHistory applyUiEvent 1
        V.EvKey V.KDown []
            | Just menu <- slashMenu
            , not (null menu.slashMenuSuggestions) ->
                moveSlash 1 (length menu.slashMenuSuggestions)
        V.EvKey V.KDown [] ->
            case verticalCursorMove 1 ui.uiDraft ui.uiCursor of
                Just cursor -> setCursor applyUiEvent cursor
                Nothing -> moveHistory applyUiEvent (-1)
        V.EvKey (V.KChar '\t') [] ->
            case slashMenu of
                Just menu -> acceptSlash applyUiEvent menu
                Nothing ->
                    when
                        (composerScrollbackAvailable
                            ui
                            state.appHistoryWindow) $
                        modifyUi applyUiEvent (UiFocusChanged FocusScrollback)
        V.EvKey V.KEnter modifiers
            | V.MShift `elem` modifiers ->
                insertText applyUiEvent "\n"
        V.EvKey V.KEnter [] ->
            case slashMenu of
                Just menu -> handleSlashEnter applyUiEvent menu
                Nothing -> submitDraft applyUiEvent
        V.EvKey V.KBS [] ->
            deleteBefore applyUiEvent
        V.EvKey V.KBS modifiers
            | any (`elem` modifiers) [V.MMeta, V.MAlt, V.MCtrl] ->
                killPreviousWord applyUiEvent
        V.EvKey (V.KChar 'w') modifiers
            | V.MCtrl `elem` modifiers ->
                killPreviousWord applyUiEvent
        V.EvKey (V.KChar 'u') modifiers
            | V.MCtrl `elem` modifiers ->
                killLineStart applyUiEvent
        V.EvKey (V.KChar 'k') modifiers
            | V.MCtrl `elem` modifiers ->
                killLineEnd applyUiEvent
        V.EvKey (V.KChar 'y') modifiers
            | V.MCtrl `elem` modifiers ->
                insertKillBuffer applyUiEvent
        V.EvKey (V.KChar 'l') modifiers
            | V.MCtrl `elem` modifiers ->
                invalidateCache
        V.EvKey (V.KChar 'r') modifiers
            | V.MCtrl `elem` modifiers ->
                startDictation applyUiEvent
        V.EvKey (V.KChar '\DC2') _ ->
            startDictation applyUiEvent
        V.EvKey (V.KChar 'a') modifiers
            | V.MCtrl `elem` modifiers ->
                setCursor applyUiEvent (lineStartCursor ui.uiDraft ui.uiCursor)
        V.EvKey (V.KChar 'e') modifiers
            | V.MCtrl `elem` modifiers ->
                setCursor applyUiEvent (lineEndCursor ui.uiDraft ui.uiCursor)
        V.EvKey (V.KChar 'b') modifiers
            | V.MCtrl `elem` modifiers ->
                moveCursor applyUiEvent (-1)
        V.EvKey (V.KChar 'b') modifiers
            | V.MMeta `elem` modifiers
                || V.MAlt `elem` modifiers ->
                setCursor applyUiEvent (moveWordLeft ui.uiDraft ui.uiCursor)
        V.EvKey (V.KChar 'f') modifiers
            | V.MCtrl `elem` modifiers ->
                moveCursor applyUiEvent 1
        V.EvKey (V.KChar 'f') modifiers
            | V.MMeta `elem` modifiers
                || V.MAlt `elem` modifiers ->
                setCursor applyUiEvent (moveWordRight ui.uiDraft ui.uiCursor)
        V.EvKey (V.KChar '_') modifiers
            | V.MCtrl `elem` modifiers ->
                undoEdit applyUiEvent
        V.EvKey (V.KChar '\US') _ ->
            undoEdit applyUiEvent
        V.EvKey (V.KChar 'v') modifiers
            | V.MCtrl `elem` modifiers
                || V.MMeta `elem` modifiers -> do
                if ui.uiRunning
                    then handleActiveComposerPaste applyUiEvent Nothing
                    else do
                        clipboardText <- liftIO readClipboardText
                        case nonEmptyClipboardText clipboardText of
                            Just text -> insertPastedText applyUiEvent text
                            Nothing ->
                                handleActiveComposerPaste applyUiEvent Nothing
        V.EvKey V.KDel [] ->
            deleteAfter applyUiEvent
        V.EvKey V.KLeft modifiers
            | V.MMeta `elem` modifiers
                || V.MAlt `elem` modifiers ->
                setCursor applyUiEvent (moveWordLeft ui.uiDraft ui.uiCursor)
        V.EvKey V.KRight modifiers
            | V.MMeta `elem` modifiers
                || V.MAlt `elem` modifiers ->
                setCursor applyUiEvent (moveWordRight ui.uiDraft ui.uiCursor)
        V.EvKey V.KLeft [] ->
            moveCursor applyUiEvent (-1)
        V.EvKey V.KRight [] ->
            moveCursor applyUiEvent 1
        V.EvKey V.KHome [] ->
            setCursor applyUiEvent (lineStartCursor ui.uiDraft ui.uiCursor)
        V.EvKey V.KEnd [] ->
            setCursor applyUiEvent (lineEndCursor ui.uiDraft ui.uiCursor)
        V.EvKey V.KPageUp [] ->
            scrollConversationPage Up
        V.EvKey V.KPageDown [] ->
            scrollConversationPage Down
        V.EvKey (V.KChar character) [] ->
            insertText applyUiEvent (Text.singleton character)
        V.EvPaste bytes ->
            handleActiveComposerPaste applyUiEvent (Just (decodePaste bytes))
        _ -> pure ()
    -- Only a kill directly followed by another kill accumulates into the
    -- kill buffer; any other key breaks the chain.
    modify' \current -> current { appKillChain = isKillKey event }

-- | Resolve a paste independently of the REPL consumer, which cannot service
-- clipboard actions while a provider turn is running. Terminal-supplied text
-- is authoritative; only an explicit clipboard paste consults bitmap data.
data ComposerPasteResult
    = ComposerPasteText !Text
    | ComposerPasteAttached !Text
    | ComposerPasteFailed !Text
    deriving (Eq, Show)

processComposerPaste
    :: Monad m
    => m (Either Text [ImageAttachment])
    -> (Text -> m (Maybe [ImageAttachment]))
    -> m (Either Text Text)
    -> ([ImageAttachment] -> m (Either Text Text))
    -> Maybe Text
    -> m ComposerPasteResult
processComposerPaste readImages loadPaths readText attachImages terminalText =
    case terminalText of
        Just text | not (Text.null text) -> do
            loadPaths text >>= \case
                Just images@(_:_) -> attach images
                _ -> pure (ComposerPasteText text)
        _ -> do
            imagesResult <- readImages
            case imagesResult of
                Right images@(_:_) -> attach images
                _ -> do
                    textResult <- readText
                    pure $ case nonEmptyClipboardText textResult of
                        Just text -> ComposerPasteText text
                        Nothing -> ComposerPasteFailed $
                            either id
                                (const "no image found on the clipboard")
                                imagesResult
  where
    attach images =
        either ComposerPasteFailed ComposerPasteAttached
            <$> attachImages images

handleActiveComposerPaste
    :: ApplyLocalUiEvent
    -> Maybe Text
    -> EventM Name AppState ()
handleActiveComposerPaste applyUiEvent terminalText = do
    result <-
        processComposerPaste
            (liftIO readClipboardImagesImageFirst)
            (liftIO . loadImagesFromPastedText)
            (liftIO readClipboardText)
            (queueComposerImages applyUiEvent)
            terminalText
    case result of
        ComposerPasteText text -> insertPastedText applyUiEvent text
        ComposerPasteAttached message ->
            modifyUi applyUiEvent
                (UiSetNotice (Just (successNotice message)))
        ComposerPasteFailed message ->
            modifyUi applyUiEvent
                (UiSetNotice (Just (warningNotice message)))

queueComposerImages
    :: ApplyLocalUiEvent
    -> [ImageAttachment]
    -> EventM Name AppState (Either Text Text)
queueComposerImages applyUiEvent images = do
    state <- get
    previous <- liftIO (readIORef state.appRuntime.runtimeImagePreviews)
    let (_, added, _, rejected) =
            appendBoundedImageAttachments (map fst previous) images
        prepare =
            if state.appRuntime.runtimeNativeImagePreviews
                then prepareNativeTuiImagePreview
                else prepareTuiImagePreview
    if null added
        then pure $
            if rejected > 0
                then Left "image attachment limit reached"
                else Right "image already attached"
        else case traverse (\image -> (image,) <$> prepare image) added of
            Left message -> pure (Left message)
            Right prepared -> do
                queued <- liftIO $ atomically $
                    appendFullscreenInput state.appRuntime.runtimeInput FullscreenInput
                        { fullscreenInputLine = ReplClipboardPasteCaptured added
                        , fullscreenInputQueued = not state.appUi.uiAwaitingInput
                        , fullscreenInputDisplay = Nothing
                        , fullscreenInputFromInbox = False
                        }
                case queued of
                    Left message -> pure (Left message)
                    Right () -> do
                        let pending = previous <> prepared
                            prompt = state.appUi.uiPrompt
                            (draft, cursor) =
                                insertImagePlaceholders
                                    state.appUi.uiDraft
                                    state.appUi.uiCursor
                                    (length previous + 1)
                                    (length added)
                        liftIO do
                            writeIORef state.appRuntime.runtimeImagePreviews pending
                            modifyIORef'
                                state.appRuntime.runtimeImagePreviewRevision
                                (+ 1)
                        modify' \current -> current
                            { appImagePreviews = map snd pending
                            , appComposerOwnsImagePreviews = True
                            }
                        applyUiEvent
                            (UiSetPrompt prompt { promptAttachments = length pending })
                            id
                        modifyUiResetSlash applyUiEvent (UiSetDraft draft cursor)
                        pure $ Right $
                            if rejected > 0
                                then "image attached; some images exceeded the attachment limit"
                                else "image attached — send with next message"

startDictation :: ApplyLocalUiEvent -> EventM Name AppState ()
startDictation applyUiEvent = do
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
                (UiSetNotice (Just dictationStartingNotice))
                \state -> state { appDictation = Just session }
            liftIO $ atomically $
                writeTQueue
                    current.appRuntime.runtimeDictationJobs
                    DictationJob
                        { dictationJobWaitForStop = readMVar stop
                        , dictationJobRecordingSession = stop
                        }

submitRaw :: ApplyLocalUiEvent -> ReplLine -> EventM Name AppState ()
submitRaw applyUiEvent replLine = do
    state <- get
    void (enqueueInput applyUiEvent state replLine Nothing False)

submitDraft :: ApplyLocalUiEvent -> EventM Name AppState ()
submitDraft applyUiEvent = do
    state <- get
    let draft = state.appUi.uiDraft
        attachmentCount =
            state.appUi.uiPrompt.promptAttachments
    case submissionPromptText attachmentCount draft of
        Nothing -> pure ()
        Just text -> submitText applyUiEvent state text state.appPasted

submitText
    :: ApplyLocalUiEvent
    -> AppState
    -> Text
    -> Bool
    -> EventM Name AppState ()
submitText applyUiEvent state text pasted = do
    let replLine = if pasted then ReplPasted text else ReplText text
    accepted <- case immediateReplCommand state.appUi replLine of
        Just command -> do
            applyUiEvent UiDraftSubmitted \current ->
                current
                    { appSlashIndex = 0
                    , appSlashDismissed = False
                    , appUndo = []
                    }
            _ <- liftIO
                (state.appRuntime.runtimeImmediateCommand command)
            pure True
        Nothing ->
            case immediateBtwQuestion state.appUi replLine of
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
                            enqueueInput applyUiEvent state replLine (Just text) True
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

sendNow :: ApplyLocalUiEvent -> EventM Name AppState ()
sendNow applyUiEvent = do
    state <- get
    let ui = state.appUi
        draft = ui.uiDraft
    when ui.uiRunning $
        if Text.null (Text.strip draft)
            then
                if Seq.null ui.uiQueuedInputs
                    then modifyUi applyUiEvent
                        (UiSetNotice
                            (Just
                                (warningNotice
                                    "There is no queued prompt to send now.")))
                    else do
                        modifyUi applyUiEvent
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
                            , fullscreenInputFromInbox = False
                            }
                case promoted of
                    Left message ->
                        modifyUi applyUiEvent
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
                        when state.appComposerOwnsImagePreviews $
                            clearComposerImagePreviews applyUiEvent
                        liftIO state.appRuntime.runtimeCancel
                        vScrollToEnd
                            (viewportScroll ConversationViewport)

enqueueInput
    :: ApplyLocalUiEvent
    -> AppState
    -> ReplLine
    -> Maybe Text
    -> Bool
    -> EventM Name AppState Bool
enqueueInput applyUiEvent state replLine display clearDraft = do
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
            , fullscreenInputFromInbox = False
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
            when (clearDraft && state.appComposerOwnsImagePreviews) $
                clearComposerImagePreviews applyUiEvent
            pure True

clearComposerImagePreviews :: ApplyLocalUiEvent -> EventM Name AppState ()
clearComposerImagePreviews applyUiEvent = do
    state <- get
    liftIO do
        writeIORef state.appRuntime.runtimeImagePreviews []
        modifyIORef' state.appRuntime.runtimeImagePreviewRevision (+ 1)
    let prompt = state.appUi.uiPrompt
    modify' \current -> current { appImagePreviews = [] }
    applyUiEvent (UiSetPrompt prompt { promptAttachments = 0 }) id

cancelOrClear :: ApplyLocalUiEvent -> EventM Name AppState ()
cancelOrClear applyUiEvent = do
    state <- get
    if not state.appUi.uiAwaitingInput
        then do
            liftIO state.appRuntime.runtimeCancel
            modifyUi applyUiEvent
                (UiSetNotice (Just (progressNotice "Cancelling…")))
        else do
            -- Esc must not destroy a typed draft irrecoverably: stash it
            -- in the kill buffer so Ctrl-Y (or Ctrl-_) restores it.
            let draft = state.appUi.uiDraft
            if Text.null draft
                then modifyUi applyUiEvent (UiSetDraft "" 0)
                else modifyUiWithKill applyUiEvent
                    KillBackward
                    draft
                    (UiSetDraft "" 0)

insertText :: ApplyLocalUiEvent -> Text -> EventM Name AppState ()
insertText applyUiEvent inserted = do
    state <- get
    let ui = state.appUi
        before = Text.take ui.uiCursor ui.uiDraft
        after = Text.drop ui.uiCursor ui.uiDraft
    modifyUiResetSlash applyUiEvent $
        UiSetDraft
            (before <> inserted <> after)
            (ui.uiCursor + Text.length inserted)

insertPastedText :: ApplyLocalUiEvent -> Text -> EventM Name AppState ()
insertPastedText applyUiEvent inserted = do
    insertText applyUiEvent inserted
    modify' \current -> current { appPasted = True }

deleteBefore :: ApplyLocalUiEvent -> EventM Name AppState ()
deleteBefore applyUiEvent = do
    state <- get
    let ui = state.appUi
    when (ui.uiCursor > 0) do
        let start =
                previousGraphemeBoundary
                    ui.uiDraft
                    ui.uiCursor
            before = Text.take start ui.uiDraft
            after = Text.drop ui.uiCursor ui.uiDraft
        modifyUiResetSlash applyUiEvent
            (UiSetDraft (before <> after) start)

deleteAfter :: ApplyLocalUiEvent -> EventM Name AppState ()
deleteAfter applyUiEvent = do
    state <- get
    let ui = state.appUi
    when (ui.uiCursor < Text.length ui.uiDraft) do
        let before = Text.take ui.uiCursor ui.uiDraft
            after =
                Text.drop
                    (nextGraphemeBoundary ui.uiDraft ui.uiCursor)
                    ui.uiDraft
        modifyUiResetSlash applyUiEvent
            (UiSetDraft (before <> after) ui.uiCursor)

killPreviousWord :: ApplyLocalUiEvent -> EventM Name AppState ()
killPreviousWord applyUiEvent = do
    state <- get
    let old = state.appUi.uiDraft
        oldCursor = state.appUi.uiCursor
        (next, cursor) =
            deleteWordBefore state.appUi.uiDraft state.appUi.uiCursor
        killed =
            Text.take (oldCursor - cursor) (Text.drop cursor old)
    modifyUiWithKill applyUiEvent KillBackward killed (UiSetDraft next cursor)

killWordAfter :: ApplyLocalUiEvent -> EventM Name AppState ()
killWordAfter applyUiEvent = do
    state <- get
    let old = state.appUi.uiDraft
        oldCursor = state.appUi.uiCursor
        (next, cursor) =
            deleteWordAfter state.appUi.uiDraft state.appUi.uiCursor
        killedLength = Text.length old - Text.length next
        killed = Text.take killedLength (Text.drop oldCursor old)
    modifyUiWithKill applyUiEvent KillForward killed (UiSetDraft next cursor)

killLineEnd :: ApplyLocalUiEvent -> EventM Name AppState ()
killLineEnd applyUiEvent = do
    state <- get
    let old = state.appUi.uiDraft
        oldCursor = state.appUi.uiCursor
        (next, cursor) =
            deleteToLineEnd state.appUi.uiDraft state.appUi.uiCursor
        killedLength = Text.length old - Text.length next
        killed = Text.take killedLength (Text.drop oldCursor old)
    modifyUiWithKill applyUiEvent KillForward killed (UiSetDraft next cursor)

killLineStart :: ApplyLocalUiEvent -> EventM Name AppState ()
killLineStart applyUiEvent = do
    state <- get
    let old = state.appUi.uiDraft
        oldCursor = state.appUi.uiCursor
        (next, cursor) =
            deleteToLineStart state.appUi.uiDraft state.appUi.uiCursor
        killed =
            Text.take (oldCursor - cursor) (Text.drop cursor old)
    modifyUiWithKill applyUiEvent KillBackward killed (UiSetDraft next cursor)

undoEdit :: ApplyLocalUiEvent -> EventM Name AppState ()
undoEdit applyUiEvent = do
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

insertKillBuffer :: ApplyLocalUiEvent -> EventM Name AppState ()
insertKillBuffer applyUiEvent = do
    state <- get
    when (not (Text.null state.appKillBuffer)) $
        insertText applyUiEvent state.appKillBuffer

moveCursor :: ApplyLocalUiEvent -> Int -> EventM Name AppState ()
moveCursor applyUiEvent delta = do
    state <- get
    let ui = state.appUi
        cursor
            | delta < 0 =
                previousGraphemeBoundary ui.uiDraft ui.uiCursor
            | delta > 0 =
                nextGraphemeBoundary ui.uiDraft ui.uiCursor
            | otherwise = ui.uiCursor
    setCursor applyUiEvent cursor

setCursor :: ApplyLocalUiEvent -> Int -> EventM Name AppState ()
setCursor applyUiEvent cursor =
    get >>= \current ->
        applyUiEvent
            (UiSetDraft current.appUi.uiDraft cursor)
            \state -> state { appSlashIndex = 0 }

modifyUi :: ApplyLocalUiEvent -> UiEvent -> EventM Name AppState ()
modifyUi applyUiEvent uiEvent =
    applyUiEvent uiEvent id

modifyUiResetSlash :: ApplyLocalUiEvent -> UiEvent -> EventM Name AppState ()
modifyUiResetSlash applyUiEvent uiEvent = do
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

modifyUiWithKill
    :: ApplyLocalUiEvent
    -> KillDirection
    -> Text
    -> UiEvent
    -> EventM Name AppState ()
modifyUiWithKill applyUiEvent direction killed uiEvent = do
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
pushUndo :: AppState -> UiEvent -> AppState -> AppState
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

moveHistory :: ApplyLocalUiEvent -> Int -> EventM Name AppState ()
moveHistory applyUiEvent delta = do
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

moveSlash :: Int -> Int -> EventM Name AppState ()
moveSlash delta count =
    modify' \current ->
        current
            { appSlashIndex =
                (current.appSlashIndex + delta) `mod` count
            }

acceptSlash :: ApplyLocalUiEvent -> SlashMenu -> EventM Name AppState ()
acceptSlash applyUiEvent menu = do
    current <- get
    case selectedSlashSuggestion current menu of
        Nothing -> pure ()
        Just suggestion -> acceptSlashSuggestion applyUiEvent menu suggestion

handleSlashEnter :: ApplyLocalUiEvent -> SlashMenu -> EventM Name AppState ()
handleSlashEnter applyUiEvent menu = do
    current <- get
    case selectedSlashSuggestion current menu of
        Nothing -> submitDraft applyUiEvent
        Just suggestion
            | Text.strip current.appUi.uiDraft
                == suggestion.slashSuggestionDisplay ->
                    submitDraft applyUiEvent
            | suggestion.slashSuggestionTakesArguments ->
                acceptSlashSuggestion applyUiEvent menu suggestion
            | otherwise -> do
                let next = slashReplacement
                        current.appUi.uiDraft
                        menu
                        suggestion
                submitText applyUiEvent current next False

acceptSlashSuggestion
    :: ApplyLocalUiEvent
    -> SlashMenu
    -> SlashSuggestion
    -> EventM Name AppState ()
acceptSlashSuggestion applyUiEvent menu suggestion = do
    current <- get
    let next = slashReplacement
            current.appUi.uiDraft
            menu
            suggestion
        cursor =
            menu.slashMenuReplaceStart
                + Text.length suggestion.slashSuggestionReplacement
    modifyUiResetSlash applyUiEvent (UiSetDraft next cursor)
