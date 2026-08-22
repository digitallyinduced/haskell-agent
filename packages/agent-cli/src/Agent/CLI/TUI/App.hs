-- | Retained fullscreen terminal application and its session bridge.
module Agent.CLI.TUI.App
    ( FullscreenInputBuffer
    , FullscreenRuntime
    , agentEntryWindow
    , agentPaneEntryLimit
    , agentPaneVisible
    , conversationScrollbarRenderer
    , emitUiEvent
    , hasQueuedFullscreenInput
    , newFullscreenInputBuffer
    , newFullscreenRuntime
    , queuedFullscreenInputDisplays
    , readFullscreenLine
    , repositoryHeaderText
    , requestFullscreenPermission
    , requestFullscreenChoice
    , requestFullscreenChoiceWithBody
    , requestFullscreenText
    , runFullscreen
    , fullscreenVtyConfig
    , setFullscreenImagePreviews
    , setFullscreenWindowTitle
    , withFullscreenSuspended
    ) where

import Agent.CLI.Clipboard
    ( formatImageSize
    )
import Agent.CLI.Artifact (fencedCodeBlock)
import Agent.CLI.Input
    ( ReplLine(..)
    , readReplHistory
    , truncateDisplayText
    )
import Agent.CLI.AgentViewport
    ( AgentEntry(..)
    , AgentStep(..)
    , AgentStepState(..)
    , AgentTarget(..)
    , agentDisplayName
    , agentEntryTreeLabel
    )
import Agent.CLI.Interrupt (CtrlCDecision(..))
import Agent.CLI.Command
    ( SkillCommand
    )
import Agent.CLI.Permission (PermissionChoice(..))
import Agent.CLI.Render (formatElapsed, summarizeToolCall)
import Agent.CLI.Status (formatTokenUsage)
import Agent.CLI.Terminal (shiftEnterCsiBodies)
import qualified Agent.TUI.Theme as Theme
import qualified Agent.CLI.TUI.Bridge as Bridge
import qualified Agent.CLI.TUI.Composer as Composer
import Agent.CLI.TUI.ImagePreview
    ( TuiImagePreview(..)
    , prepareTuiImagePreview
    , renderTuiImagePreview
    )
import Agent.TUI.Markdown
    ( markdownWidget
    , markdownWidgetWithCodeControls
    )
import qualified Agent.CLI.TUI.Scroll as Scroll
import Agent.CLI.TUI.Types
import Agent.TUI.Model
import Agent.Loop (ImageAttachment, LoopEvent(..))
import Agent.ToolDispatch (ToolCall(..))
import Brick
import qualified Brick.Types as B
import Brick.BChan
    ( newBChan
    , writeBChan
    )
import Brick.Widgets.Border (borderWithLabel)
import qualified Brick.Widgets.Border as Border
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center (center, centerLayer, hCenter)
import Control.Concurrent.Async (wait, waitCatch, withAsync)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
    ( STM
    , atomically
    , newEmptyTMVarIO
    , newTVarIO
    , putTMVar
    , readTVar
    , readTMVar
    , retry
    , writeTVar
    )
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (modify')
import Control.Exception.Safe (finally, throwIO, tryAny)
import Control.Exception (AsyncException(UserInterrupt))
import Data.Foldable (toList)
import Data.IORef
    ( newIORef
    , readIORef
    , writeIORef
    )
import Data.List (find, findIndex, intersperse, sortOn)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Sequence (Seq, ViewL(..), ViewR(..), (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as Vty

newFullscreenInputBuffer :: IO FullscreenInputBuffer
newFullscreenInputBuffer = Composer.newFullscreenInputBuffer

newFullscreenRuntime
    :: FullscreenInputBuffer
    -> IO ()
    -> (Text -> IO ())
    -> IO CtrlCDecision
    -> (Text -> IO Bool)
    -> (Text -> IO ())
    -> (Bool -> IO ())
    -> IO (AgentTarget, [AgentEntry])
    -> (AgentTarget -> IO ())
    -> IO ()
    -> Bool
    -> UiState
    -> IO FullscreenRuntime
newFullscreenRuntime
    inputBuffer
    cancelAction
    restartEffortAction
    ctrlCAction
    copyAction
    setWindowTitle
    nativeProgress
    agentSnapshot
    agentSelect
    firstFrame
    color
    initial = do
        events <- newBChan 512
        mailbox <- AppEventMailbox <$> newTVarIO Seq.empty
        running <- newIORef (uiNeedsTick initial)
        pure FullscreenRuntime
            { runtimeEvents = events
            , runtimeMailbox = mailbox
            , runtimeInput = inputBuffer
            , runtimeCancel = cancelAction
            , runtimeRestartEffort = restartEffortAction
            , runtimeCtrlC = ctrlCAction
            , runtimeCopy = copyAction
            , runtimeSetWindowTitle = setWindowTitle
            , runtimeNativeProgress = nativeProgress
            , runtimeAgentSnapshot = agentSnapshot
            , runtimeAgentSelect = agentSelect
            , runtimeFirstFrame = firstFrame
            , runtimeRunning = running
            , runtimeColor = color
            , runtimeInitial = initial
            }

emitUiEvent :: FullscreenRuntime -> UiEvent -> IO ()
emitUiEvent runtime event =
    enqueueAppEvent runtime (AppUi event)

setFullscreenWindowTitle :: FullscreenRuntime -> Text -> IO ()
setFullscreenWindowTitle runtime =
    enqueueAppEvent runtime . AppSetWindowTitle

setFullscreenImagePreviews
    :: FullscreenRuntime
    -> [ImageAttachment]
    -> IO ()
setFullscreenImagePreviews runtime =
    enqueueAppEvent runtime . AppSetImagePreviews

hasQueuedFullscreenInput :: FullscreenRuntime -> IO Bool
hasQueuedFullscreenInput runtime =
    atomically do
        queued <- Composer.readFullscreenInputs runtime.runtimeInput
        pure (not (Seq.null queued))

queuedFullscreenInputDisplays
    :: FullscreenInputBuffer
    -> IO (Seq.Seq Text)
queuedFullscreenInputDisplays =
    Composer.queuedFullscreenInputDisplays

readFullscreenLine
    :: FullscreenRuntime
    -> [SkillCommand]
    -> PromptState
    -> Text
    -> IO ReplLine
readFullscreenLine runtime skills prompt initial = do
    enqueueAppEvent runtime (AppSetSkillCommands skills)
    emitUiEvent runtime (UiSetPrompt prompt)
    -- Keep anything the user started typing while the previous turn was
    -- running. Non-empty explicit drafts (for example after cycling mode or
    -- pasting an attachment) still take precedence.
    when (not (Text.null initial)) $
        emitUiEvent runtime (UiSetDraft initial (Text.length initial))
    emitUiEvent runtime (UiSetAwaitingInput True)
    input <- atomically (Composer.takeFullscreenInput runtime.runtimeInput)
    when input.fullscreenInputQueued $
        emitUiEvent runtime $
            case input.fullscreenInputDisplay of
                Just _ -> UiQueuedInputStarted
                Nothing -> UiSetAwaitingInput False
    pure input.fullscreenInputLine

-- | Fullscreen Vty configuration, including enhanced-keyboard encodings that
-- are not present in the default terminfo input table. Without these entries,
-- Vty emits the payload of Shift+Enter sequences as printable characters.
fullscreenVtyConfig :: V.VtyUserConfig
fullscreenVtyConfig =
    V.defaultConfig
        { V.configPreferredColorMode = Just V.FullColor
        , V.configInputMap =
            [ ( Nothing
              , "\ESC[" <> body
              , V.EvKey V.KEnter [V.MShift]
              )
            | body <- shiftEnterCsiBodies
            ]
        }

requestFullscreenPermission
    :: FullscreenRuntime
    -> ToolCall
    -> IO (Maybe PermissionChoice)
requestFullscreenPermission runtime call = do
    reply <- newEmptyTMVarIO
    let summary = summarizeToolCall call
    enqueueAppEvent runtime (AppAskPermission summary reply)
    atomically (readTMVar reply)

requestFullscreenChoice
    :: FullscreenRuntime
    -> Text
    -> Int
    -> [(Text, Text)]
    -> IO (Maybe Int)
requestFullscreenChoice runtime title initial rows = do
    requestFullscreenChoiceWithBody runtime title "" initial rows

requestFullscreenChoiceWithBody
    :: FullscreenRuntime
    -> Text
    -> Text
    -> Int
    -> [(Text, Text)]
    -> IO (Maybe Int)
requestFullscreenChoiceWithBody runtime title body initial rows = do
    reply <- newEmptyTMVarIO
    enqueueAppEvent runtime
        (AppAskChoice title body initial rows reply)
    atomically (readTMVar reply)

requestFullscreenText
    :: FullscreenRuntime
    -> Text
    -> Text
    -> Text
    -> IO (Maybe Text)
requestFullscreenText runtime title body initial = do
    reply <- newEmptyTMVarIO
    enqueueAppEvent runtime
        (AppAskText title body initial reply)
    atomically (readTMVar reply)

withFullscreenSuspended :: FullscreenRuntime -> IO a -> IO a
withFullscreenSuspended runtime action = do
    reply <- newEmptyTMVarIO
    enqueueAppEvent runtime (AppSuspend action reply)
    atomically (readTMVar reply) >>= either throwIO pure

runFullscreen :: FullscreenRuntime -> IO a -> IO a
runFullscreen runtime workerAction = do
    history <- readReplHistory
    (initialAgent, initialAgents) <- runtime.runtimeAgentSnapshot
    let buildVty = do
            vty <- Vty.mkVty fullscreenVtyConfig
            let output = V.outputIface vty
            -- Without this mode terminals paste image clipboard fallbacks
            -- (paths, URLs, or other text representations) as ordinary key
            -- events, so the composer renders them as text. Vty turns the
            -- bracketed sequence into one EvPaste that we can classify.
            when (V.supportsMode output V.BracketedPaste) $
                V.setMode output V.BracketedPaste True
            when (V.supportsMode output V.Mouse) $
                V.setMode output V.Mouse True
            pure vty
    initialVty <- buildVty
    let
        initialState = AppState
            { appUi = runtime.runtimeInitial
            , appPermissionReply = Nothing
            , appRuntime = runtime
            , appSlashIndex = 0
            , appChoice = Nothing
            , appChoiceReply = Nothing
            , appTextPrompt = Nothing
            , appTextReply = Nothing
            , appSlashDismissed = False
            , appPasted = False
            , appHistory = history
            , appHistoryIndex = Nothing
            , appHistoryDraft = ""
            , appKillBuffer = ""
            , appSkillCommands = []
            , appImagePreviews = []
            , appAgentSelected = initialAgent
            , appAgentEntries = initialAgents
            , appAgentHover = Nothing
            , appHoveredControl = Nothing
            , appPressedControl = Nothing
            , appWorkerStopped = False
            , appConversationAnchor = Nothing
            , appConversationReflowQueued = False
            }
    withAsync workerAction \worker ->
        withAsync uiTicker \_uiTicker ->
            withAsync (agentTicker (initialAgent, initialAgents)) \_agentTicker ->
                withAsync (eventPump runtime) \_eventPump ->
                    withAsync
                        (void (waitCatch worker)
                            >> enqueueAppEvent runtime AppStop)
                        \_notifier -> do
                            finalState <-
                                customMain
                                    initialVty
                                    buildVty
                                    (Just runtime.runtimeEvents)
                                    fullscreenApp
                                    initialState
                                `finally` runtime.runtimeNativeProgress False
                            when (not finalState.appWorkerStopped) $
                                atomically $
                                    Composer.appendFullscreenInput
                                        runtime.runtimeInput
                                        FullscreenInput
                                            { fullscreenInputLine = ReplEof
                                            , fullscreenInputQueued = False
                                            , fullscreenInputDisplay = Nothing
                                            }
                            wait worker
  where
    uiTicker = loop (0 :: Int)
      where
        loop tick = do
            threadDelay 50000
            needsTick <- readIORef runtime.runtimeRunning
            -- Model ticks remain at 10 Hz for elapsed time, completion
            -- settling, and notice expiry. The empty-state renderer samples
            -- every second frame for a calmer 5 FPS animation.
            when (needsTick && tick `mod` 2 == 0) $
                enqueueAppEvent runtime (AppUi UiTick)
            loop ((tick + 1) `mod` 20)

    agentTicker previous = do
        threadDelay 500000
        next <- tryAny runtime.runtimeAgentSnapshot
        previous' <- case next of
            Left _ -> pure previous
            Right snapshot
                | snapshot == previous -> pure previous
                | otherwise -> do
                    enqueueAppEvent runtime
                        (uncurry AppAgentSnapshot snapshot)
                    pure snapshot
        agentTicker previous'

-- | Move events from the producer-facing mailbox into Brick. UI updates are
-- collected for one frame so a fast token stream causes at most one redraw
-- every ~16 ms. Blocking on Brick's bounded channel only blocks this pump,
-- never the model/tool worker publishing into the mailbox.
eventPump :: FullscreenRuntime -> IO ()
eventPump runtime = loop
  where
    loop = do
        pending <- atomically (takePendingAppEvent runtime.runtimeMailbox)
        delivered <- case pending of
            PendingUi first -> do
                threadDelay uiFrameDelayMicros
                rest <- atomically $
                    takePendingUiEventPrefix
                        (uiFrameBatchLimit - 1)
                        runtime.runtimeMailbox
                pure (AppUiBatch
                    (pendingUiEvent first :| map pendingUiEvent rest))
            PendingEvent event ->
                pure event
        writeBChan runtime.runtimeEvents delivered
        loop

uiFrameDelayMicros :: Int
uiFrameDelayMicros = 16000

uiFrameBatchLimit :: Int
uiFrameBatchLimit = 256

enqueueAppEvent :: FullscreenRuntime -> AppEvent -> IO ()
enqueueAppEvent runtime event =
    atomically do
        let AppEventMailbox pendingRef = runtime.runtimeMailbox
        pending <- readTVar pendingRef
        writeTVar pendingRef (appendAppEvent event pending)

appendAppEvent :: AppEvent -> Seq PendingAppEvent -> Seq PendingAppEvent
appendAppEvent event pending = case event of
    AppUi (UiLoop (TextDelta delta)) ->
        case Seq.viewr pending of
            rest :> PendingUi (PendingTextDeltas deltas) ->
                rest |> PendingUi (PendingTextDeltas (deltas |> delta))
            _ ->
                pending |> PendingUi
                    (PendingTextDeltas (Seq.singleton delta))
    AppUi (UiLoop (ReasoningDelta delta)) ->
        case Seq.viewr pending of
            rest :> PendingUi (PendingReasoningDeltas deltas) ->
                rest |> PendingUi
                    (PendingReasoningDeltas (deltas |> delta))
            _ ->
                pending |> PendingUi
                    (PendingReasoningDeltas (Seq.singleton delta))
    AppUi uiEvent ->
        appendExactUiEvent uiEvent pending
    _ ->
        appendExactAppEvent event pending

appendExactUiEvent
    :: UiEvent
    -> Seq PendingAppEvent
    -> Seq PendingAppEvent
appendExactUiEvent event pending =
    case Seq.viewr pending of
        rest :> PendingUi (PendingExactUi previous)
            | Just merged <- Bridge.mergeUiEvents previous event ->
                rest |> PendingUi (PendingExactUi merged)
        _ ->
            pending |> PendingUi (PendingExactUi event)

appendExactAppEvent
    :: AppEvent
    -> Seq PendingAppEvent
    -> Seq PendingAppEvent
appendExactAppEvent event pending =
    case (Seq.viewr pending, event) of
        ( rest :> PendingEvent (AppAgentSnapshot _ _)
            , AppAgentSnapshot selected entries
            ) ->
                rest |> PendingEvent (AppAgentSnapshot selected entries)
        (rest :> PendingEvent (AppSetWindowTitle _), AppSetWindowTitle title) ->
            rest |> PendingEvent (AppSetWindowTitle title)
        _ ->
            pending |> PendingEvent event

takePendingAppEvent :: AppEventMailbox -> STM PendingAppEvent
takePendingAppEvent (AppEventMailbox pendingRef) = do
    pending <- readTVar pendingRef
    case Seq.viewl pending of
        EmptyL -> retry
        event :< rest -> do
            writeTVar pendingRef rest
            pure event

takePendingUiEventPrefix
    :: Int
    -> AppEventMailbox
    -> STM [PendingUiEvent]
takePendingUiEventPrefix limit (AppEventMailbox pendingRef) = do
    pending <- readTVar pendingRef
    let (events, rest) = go limit [] pending
    writeTVar pendingRef rest
    pure events
  where
    go remaining acc pending
        | remaining <= 0 = (reverse acc, pending)
        | otherwise =
            case Seq.viewl pending of
                PendingUi event :< rest ->
                    go (remaining - 1) (event : acc) rest
                _ ->
                    (reverse acc, pending)

pendingUiEvent :: PendingUiEvent -> UiEvent
pendingUiEvent = \case
    PendingExactUi event -> event
    PendingTextDeltas deltas ->
        UiLoop (TextDelta (Text.concat (toList deltas)))
    PendingReasoningDeltas deltas ->
        UiLoop (ReasoningDelta (Text.concat (toList deltas)))

handleChoiceKey :: V.Event -> EventM Name AppState ()
handleChoiceKey = \case
    V.EvKey V.KUp [] -> moveChoice (-1)
    V.EvKey V.KDown [] -> moveChoice 1
    V.EvKey V.KBackTab [] -> moveChoice (-1)
    V.EvKey (V.KChar '\t') [] -> moveChoice 1
    V.EvKey V.KPageUp [] ->
        vScrollPage (viewportScroll OverlayViewport) Up
    V.EvKey V.KPageDown [] ->
        vScrollPage (viewportScroll OverlayViewport) Down
    V.EvMouseDown _ _ V.BScrollUp _ ->
        vScrollBy (viewportScroll OverlayViewport) (-mouseScrollLines)
    V.EvMouseDown _ _ V.BScrollDown _ ->
        vScrollBy (viewportScroll OverlayViewport) mouseScrollLines
    V.EvKey V.KEnter [] -> resolveChoice True
    V.EvKey V.KEsc [] -> resolveChoice False
    V.EvKey (V.KChar 'q') [] -> resolveChoice False
    _ -> pure ()
  where
    moveChoice delta =
        modify' \state ->
            state
                { appChoice =
                    (\choice ->
                        let count = length choice.choiceRows
                        in if count == 0
                            then choice
                            else choice
                                { choiceIndex =
                                    (choice.choiceIndex + delta) `mod` count
                                })
                        <$> state.appChoice
                }

confirmChoiceAt :: Int -> EventM Name AppState ()
confirmChoiceAt index = do
    state <- get
    case state.appChoice of
        Just choice
            | index >= 0
            , index < length choice.choiceRows -> do
                modify' \current ->
                    current
                        { appChoice =
                            (\overlay -> overlay { choiceIndex = index })
                                <$> current.appChoice
                        }
                resolveChoice True
        _ -> pure ()

activateControl :: Name -> EventM Name AppState ()
activateControl = \case
    ComposerModel ->
        Composer.handlePromptControlClick ReplChooseModel
    ComposerEffort ->
        Composer.handleEffortControlClick
    ComposerMode ->
        Composer.handlePromptControlClick ReplCycleMode
    ChoiceRow index ->
        confirmChoiceAt index
    CodeCopy blockId codeIndex ->
        copyCodeBlock blockId codeIndex
    _ ->
        pure ()

isInteractiveControl :: Name -> Bool
isInteractiveControl = \case
    ComposerModel -> True
    ComposerEffort -> True
    ComposerMode -> True
    ChoiceRow _ -> True
    CodeCopy _ _ -> True
    _ -> False

copyCodeBlock :: BlockId -> Int -> EventM Name AppState ()
copyCodeBlock blockId codeIndex = do
    state <- get
    let code =
            selectedBlock state.appUi blockId
                >>= fencedCodeBlock codeIndex . (.blockBody)
    case code of
        Nothing ->
            modify' \current ->
                current
                    { appUi =
                        reduceUi
                            (UiSetNotice
                                (Just
                                    (warningNotice
                                        "Code block is no longer available.")))
                            current.appUi
                    }
        Just payload -> do
            copied <- liftIO (state.appRuntime.runtimeCopy payload)
            modify' \current ->
                current
                    { appUi =
                        reduceUi
                            (UiSetNotice
                                (Just
                                    (if copied
                                        then successNotice
                                            "Copied code block."
                                        else warningNotice
                                            "Terminal clipboard is unavailable.")))
                            current.appUi
                    }

resolveChoice :: Bool -> EventM Name AppState ()
resolveChoice confirmed = do
    state <- get
    case state.appChoiceReply of
        Nothing -> pure ()
        Just reply ->
            liftIO $ reply $
                if confirmed
                    then (.choiceIndex) <$> state.appChoice
                    else Nothing
    modify' \current ->
        current
            { appChoice = Nothing
            , appChoiceReply = Nothing
            }

handleTextPromptKey :: V.Event -> EventM Name AppState ()
handleTextPromptKey = \case
    V.EvKey V.KEsc [] -> resolveTextPrompt False
    V.EvKey V.KEnter [] -> resolveTextPrompt True
    V.EvKey V.KEnter [V.MShift] -> insert "\n"
    V.EvKey V.KPageUp [] ->
        vScrollPage (viewportScroll OverlayViewport) Up
    V.EvKey V.KPageDown [] ->
        vScrollPage (viewportScroll OverlayViewport) Down
    V.EvMouseDown _ _ V.BScrollUp _ ->
        vScrollBy (viewportScroll OverlayViewport) (-mouseScrollLines)
    V.EvMouseDown _ _ V.BScrollDown _ ->
        vScrollBy (viewportScroll OverlayViewport) mouseScrollLines
    V.EvKey V.KBS [] -> edit \draft cursor ->
        if cursor <= 0
            then (draft, cursor)
            else
                ( Text.take (cursor - 1) draft <> Text.drop cursor draft
                , cursor - 1
                )
    V.EvKey V.KDel [] -> edit \draft cursor ->
        if cursor >= Text.length draft
            then (draft, cursor)
            else
                ( Text.take cursor draft <> Text.drop (cursor + 1) draft
                , cursor
                )
    V.EvKey V.KLeft [] -> move (-1)
    V.EvKey V.KRight [] -> move 1
    V.EvKey V.KHome [] -> edit \draft cursor ->
        (draft, lineStartCursor draft cursor)
    V.EvKey V.KEnd [] -> edit \draft cursor ->
        (draft, lineEndCursor draft cursor)
    V.EvKey (V.KChar 'w') modifiers
        | V.MCtrl `elem` modifiers ->
            edit deleteWordBefore
    V.EvKey (V.KChar 'u') modifiers
        | V.MCtrl `elem` modifiers ->
            edit deleteToLineStart
    V.EvKey (V.KChar 'k') modifiers
        | V.MCtrl `elem` modifiers ->
            edit deleteToLineEnd
    V.EvKey (V.KChar character) [] ->
        insert (Text.singleton character)
    V.EvPaste bytes ->
        insert (Composer.decodePaste bytes)
    _ -> pure ()
  where
    edit change =
        modify' \state ->
            state
                { appTextPrompt =
                    (\prompt ->
                        let (draft, cursor) =
                                change prompt.textDraft prompt.textCursor
                        in prompt
                            { textDraft = draft
                            , textCursor =
                                max 0 (min (Text.length draft) cursor)
                            })
                        <$> state.appTextPrompt
                }
    insert inserted =
        edit \draft cursor ->
            ( Text.take cursor draft <> inserted <> Text.drop cursor draft
            , cursor + Text.length inserted
            )
    move delta =
        edit \draft cursor -> (draft, cursor + delta)

resolveTextPrompt :: Bool -> EventM Name AppState ()
resolveTextPrompt confirmed = do
    state <- get
    case state.appTextReply of
        Nothing -> pure ()
        Just reply ->
            liftIO $ atomically $
                putTMVar reply $
                    if confirmed
                        then (.textDraft) <$> state.appTextPrompt
                        else Nothing
    modify' \current ->
        current
            { appTextPrompt = Nothing
            , appTextReply = Nothing
            }

fullscreenApp :: App AppState AppEvent Name
fullscreenApp = App
    { appDraw = drawApp
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = do
        state <- get
        liftIO state.appRuntime.runtimeFirstFrame
        vScrollToEnd (viewportScroll ConversationViewport)
    , appAttrMap = \state ->
        if state.appRuntime.runtimeColor
            then Theme.solarizedDark
            else Theme.monochrome
    }

drawApp :: AppState -> [Widget Name]
drawApp state =
    let mainLayers = stickyPromptLayers state <> [drawMain state]
        interactiveLayers =
            agentPopoverLayers state
                <> imagePreviewLayers state.appImagePreviews
                <> mainLayers
        dimmedMainLayers = map (forceAttr Theme.dimAttr) mainLayers
    in
    case (state.appTextPrompt, state.appChoice, state.appUi.uiPermission) of
        (Just prompt, _, _) ->
            drawTextPrompt prompt : dimmedMainLayers
        (Nothing, Just choice, _) ->
            drawChoice state choice : dimmedMainLayers
        (Nothing, Nothing, Just permission) ->
            drawPermission permission : dimmedMainLayers
        (Nothing, Nothing, Nothing) -> interactiveLayers

drawMain :: AppState -> Widget Name
drawMain state =
    withAttr Theme.baseAttr $
        vBox
            [ drawHeader state.appUi
            , drawWorkspace state
            , drawNotice state.appUi
            , Composer.drawQueuedInputs state.appUi
            , Composer.drawSlashMenu state
            , drawFollowStatus state.appUi
            , Composer.drawComposer state
            , drawFooter state
            ]

imagePreviewLayers :: [TuiImagePreview] -> [Widget Name]
imagePreviewLayers previews =
    case takeLast 3 previews of
        [] -> []
        shown -> [centerLayer (drawImagePreviews shown)]
  where
    takeLast count values =
        drop (max 0 (length values - count)) values

drawImagePreviews :: [TuiImagePreview] -> Widget Name
drawImagePreviews previews =
    Widget Fixed Fixed do
        context <- getContext
        let maxWidth = viewportPreviewSize context.availWidth
            maxHeight = viewportPreviewSize context.availHeight
            gaps = max 0 (length previews - 1) * previewGap
            previewWidth =
                max 1 ((maxWidth - gaps) `div` max 1 (length previews))
            previewHeight = max 1 (maxHeight - 1)
            content =
                hBox $
                    intersperse
                        (hLimit previewGap (fill ' '))
                        (map (drawPreview previewWidth previewHeight) previews)
        render $
            hLimit maxWidth $
                vLimit maxHeight content
  where
    previewGap = 2

    drawPreview maxWidth maxHeight preview =
        hLimit maxWidth $
            vBox
                [ hCenter $
                    renderTuiImagePreview maxWidth maxHeight preview
                , hCenter $
                    withAttr Theme.mutedAttr $
                        txt $
                            "🖼 "
                                <> preview.previewMime
                                <> " · "
                                <> Text.pack (show preview.previewSourceWidth)
                                <> "×"
                                <> Text.pack (show preview.previewSourceHeight)
                                <> " · "
                                <> formatImageSize preview.previewBytes
                ]

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
                                                (agentPaneEntryLimit
                                                    context.availHeight)
                                                state.appAgentSelected
                                                ((.agentHoverTarget)
                                                    <$> state.appAgentHover)
                                                state.appAgentEntries
                                ]

drawConversationPane :: AppState -> Widget Name
drawConversationPane state
    | conversationIsEmpty state.appUi =
        padLeftRight 2 $
            vBox
                [ drawTranscript state
                , drawEmptyConversation (state.appUi.uiFrame `div` 2)
                ]
    | otherwise =
        withVScrollBarRenderer conversationScrollbarRenderer $
            withVScrollBars OnRight $
                viewport ConversationViewport Vertical $
                    padLeftRight 2 (drawTranscript state)

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
    -- Reserve the outer top pad, six pane chrome rows (border, padding,
    -- footer), and two possible truncation indicators above and below.
    max 1 (min 12 (availableHeight - 9))

drawAgentPane
    :: Int
    -> AgentTarget
    -> Maybe AgentTarget
    -> [AgentEntry]
    -> Widget Name
drawAgentPane entryLimit selected hovered entries =
    clickable AgentPane $
        withAttr Theme.borderAttr $
            withBorderStyle unicodeRounded $
                borderWithLabel
                    (txt
                        (" Agents · "
                            <> Text.pack (show childCount)
                            <> " ")) $
                    padAll 1 $
                        vBox
                            [ vBox agentRows
                            , padTop (Pad 1) $
                                withAttr Theme.footerAttr $
                                    txt "hover preview · /agents switch"
                            ]
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
            row = hBox
                [ txt (marker <> agentEntryTreeLabel ordered index entry)
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
                Just entry -> [positionAgentPopover hover entry]

agentPopoverPreferredWidth :: Int
agentPopoverPreferredWidth = 40

agentPopoverMinWidth :: Int
agentPopoverMinWidth = 24

agentPopoverHeight :: Int
agentPopoverHeight = 9

agentPopoverGap :: Int
agentPopoverGap = 1

positionAgentPopover :: AgentHover -> AgentEntry -> Widget Name
positionAgentPopover hover entry =
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
                        drawAgentPopover placeLeft width height entry

drawAgentPopover :: Bool -> Int -> Int -> AgentEntry -> Widget Name
drawAgentPopover placeLeft width height entry =
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
                                    txt
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

drawAgentStep :: Int -> AgentStep -> Widget Name
drawAgentStep width step =
    vLimit 2 $
        vBox
            [ hBox
                [ withAttr (agentStepAttr step.agentStepState) $
                    txt (agentStepGlyph step.agentStepState)
                , txt " "
                , withAttr Theme.assistantAttr $
                    txt
                        (truncateDisplayText
                            (max 1 (width - 2))
                            step.agentStepTitle)
                ]
            , padLeft (Pad 2) $
                withAttr Theme.mutedAttr $
                    txt
                        (truncateDisplayText
                            (max 1 (width - 2))
                            (fromMaybe
                                (agentStepStateLabel step.agentStepState)
                                step.agentStepDetail))
            ]

agentStepGlyph :: AgentStepState -> Text
agentStepGlyph = \case
    AgentStepRunning -> "●"
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

drawHeader :: UiState -> Widget Name
drawHeader state =
    withAttr Theme.headerAttr $
        padLeftRight 2 $
            hBox
                [ hLimitPercent 68 (drawRepositoryHeader state)
                , vLimit 1 (fill ' ')
                , drawHeaderRight state
                ]

drawRepositoryHeader :: UiState -> Widget Name
drawRepositoryHeader state
    | Text.null state.uiBranch =
        withAttr Theme.mutedAttr (txt state.uiCwd)
    | otherwise =
        hBox
            [ txt "\xE0A0 "
            , withAttr Theme.mutedAttr $
                txt (repositoryHeaderText state.uiBranch state.uiCwd)
            ]

repositoryHeaderText :: Text -> Text -> Text
repositoryHeaderText branch cwd =
    Text.intercalate "  " $
        filter (not . Text.null) [branch, cwd]

drawHeaderRight :: UiState -> Widget Name
drawHeaderRight state =
    hBox
        [ withAttr activityAttr (txt activity)
        , withAttr Theme.mutedAttr (txt elapsed)
        , withAttr Theme.mutedAttr (txt usage)
        ]
  where
    activityAttr
        | state.uiRunning = Theme.thinkingAttr
        | state.uiCompletionTicks > 0 = Theme.successAttr
        | otherwise = Theme.mutedAttr
    activity =
        (if state.uiRunning then spinnerFrame state.uiFrame <> " " else "")
            <> state.uiActivity
    elapsed =
        if state.uiRunning
            then " · "
                <> formatElapsed
                    (fromIntegral state.uiElapsedTenths / 10)
            else ""
    formattedUsage = formatTokenUsage state.uiPrompt.promptUsage
    usage =
        if Text.null formattedUsage then "" else " │ " <> formattedUsage

spinnerFrame :: Int -> Text
spinnerFrame frame =
    ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]
        !! (frame `mod` 10)

drawTranscript :: AppState -> Widget Name
drawTranscript state =
    vBox $
        [vBox (map (drawBlock state) blocks)]
            <> conversationReserveWidgets anchor
  where
    blocks = toList state.appUi.uiBlocks
    anchor = state.appConversationAnchor

stickyPromptLayers :: AppState -> [Widget Name]
stickyPromptLayers state =
    case state.appConversationAnchor of
        Just anchor
            | Scroll.conversationAnchorSticky anchor ->
                [ translateBy (Location (0, 2)) $
                    hLimitPercent conversationWidth $
                        padLeftRight 2 $
                            withAttr Theme.userAttr $
                                vLimit 5 $
                                    padAll 1 $
                                        txtWrap
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

drawEmptyConversation :: Int -> Widget Name
drawEmptyConversation frame =
    center (lambdaArtWidget frame)

data LambdaComposition = LambdaComposition
    { lambdaUpperHeight :: !Int
    , lambdaLowerHeight :: !Int
    , lambdaStrokeWidth :: !Int
    , lambdaMarginX :: !Int
    , lambdaMarginY :: !Int
    , lambdaHasOrbit :: !Bool
    }

data LambdaPalette = LambdaPalette
    { lambdaDim :: !V.Attr
    , lambdaTrail :: !V.Attr
    , lambdaGlow :: !V.Attr
    , lambdaSpark :: !V.Attr
    }

lambdaArtWidget :: Int -> Widget Name
lambdaArtWidget frame =
    B.Widget B.Fixed B.Fixed do
        context <- B.getContext
        dimAttr <- B.lookupAttrName Theme.lambdaDimAttr
        trailAttr <- B.lookupAttrName Theme.lambdaTrailAttr
        glowAttr <- B.lookupAttrName Theme.lambdaGlowAttr
        sparkAttr <- B.lookupAttrName Theme.lambdaSparkAttr
        let
            composition =
                lambdaComposition context.availWidth context.availHeight
            palette = LambdaPalette
                { lambdaDim = dimAttr
                , lambdaTrail = trailAttr
                , lambdaGlow = glowAttr
                , lambdaSpark = sparkAttr
                }
            rows =
                buildSolidLambdaRows
                    composition.lambdaUpperHeight
                    composition.lambdaLowerHeight
                    composition.lambdaStrokeWidth
            logoWidth = maximum (0 : map length rows)
            canvasWidth = logoWidth + 2 * composition.lambdaMarginX
            canvasHeight = length rows + 2 * composition.lambdaMarginY
            particles =
                lambdaOrbitParticles
                    palette
                    frame
                    canvasWidth
                    canvasHeight
                    composition.lambdaHasOrbit
            rendered =
                V.vertCat
                    [ V.horizCat
                        [ renderLambdaCell
                            palette
                            frame
                            composition
                            rows
                            particles
                            x
                            y
                        | x <- [0 .. canvasWidth - 1]
                        ]
                    | y <- [0 .. canvasHeight - 1]
                    ]
        pure B.emptyResult { B.image = rendered }

lambdaComposition :: Int -> Int -> LambdaComposition
lambdaComposition width height
    | width >= 42
    , height >= 21 =
        LambdaComposition
            { lambdaUpperHeight = 8
            , lambdaLowerHeight = 11
            , lambdaStrokeWidth = 3
            , lambdaMarginX = 8
            , lambdaMarginY = 1
            , lambdaHasOrbit = True
            }
    | width >= 24
    , height >= 14 =
        LambdaComposition
            { lambdaUpperHeight = 5
            , lambdaLowerHeight = 7
            , lambdaStrokeWidth = 2
            , lambdaMarginX = 4
            , lambdaMarginY = 1
            , lambdaHasOrbit = True
            }
    | otherwise =
        LambdaComposition
            { lambdaUpperHeight = 3
            , lambdaLowerHeight = 4
            , lambdaStrokeWidth = 2
            , lambdaMarginX = 0
            , lambdaMarginY = 0
            , lambdaHasOrbit = False
            }

buildSolidLambdaRows :: Int -> Int -> Int -> [String]
buildSolidLambdaRows upperHeight lowerHeight strokeWidth =
    map row [0 .. totalHeight - 1]
  where
    totalHeight = upperHeight + lowerHeight
    mainStart =
        max 0 (strokeWidth + lowerHeight - 1 - upperHeight)
    width = mainColumn (totalHeight - 1) + strokeWidth
    row rowIndex =
        map (cell rowIndex) [0 .. width - 1]
    cell rowIndex column
        | rowIndex >= upperHeight
        , column >= branchColumn rowIndex
        , column < branchColumn rowIndex + strokeWidth =
            '/'
        | column >= mainColumn rowIndex
        , column < mainColumn rowIndex + strokeWidth =
            '\\'
        | otherwise = ' '
      where
        branchColumn index =
            mainStart
                + upperHeight
                - strokeWidth
                - (index - upperHeight)
    mainColumn index =
        mainStart + index

renderLambdaCell
    :: LambdaPalette
    -> Int
    -> LambdaComposition
    -> [String]
    -> [((Int, Int), Char, V.Attr)]
    -> Int
    -> Int
    -> V.Image
renderLambdaCell palette frame composition rows particles x y =
    case lambdaLogoChar rows composition.lambdaMarginX
        composition.lambdaMarginY x y of
        ' ' ->
            case find
                (\(position, _, _) -> position == (x, y))
                particles of
                Just (_, character, attr) ->
                    V.char attr character
                Nothing ->
                    V.char palette.lambdaDim ' '
        character ->
            let
                localX = x - composition.lambdaMarginX
                localY = y - composition.lambdaMarginY
                (attr, animatedCharacter) =
                    animatedLambdaStroke
                        palette
                        frame
                        localX
                        localY
                        character
            in V.char attr animatedCharacter

lambdaLogoChar :: [String] -> Int -> Int -> Int -> Int -> Char
lambdaLogoChar rows marginX marginY x y
    | localX < 0 || localY < 0 = ' '
    | otherwise =
        case drop localY rows of
            row : _ ->
                case drop localX row of
                    character : _ -> character
                    [] -> ' '
            [] -> ' '
  where
    localX = x - marginX
    localY = y - marginY

animatedLambdaStroke
    :: LambdaPalette
    -> Int
    -> Int
    -> Int
    -> Char
    -> (V.Attr, Char)
animatedLambdaStroke palette frame x y character
    | distance == 0 =
        (palette.lambdaSpark, energizedStroke character)
    | distance <= 2 =
        (palette.lambdaGlow, character)
    | distance <= 5 =
        (palette.lambdaTrail, character)
    | otherwise =
        (palette.lambdaDim, character)
  where
    period = 36
    phase = frame `mod` period
    oppositePhase = (phase + period `div` 2) `mod` period
    cellPhase = (y * 5 + x * 3) `mod` period
    distance =
        min
            (circularDistance period phase cellPhase)
            (circularDistance period oppositePhase cellPhase)

energizedStroke :: Char -> Char
energizedStroke = \case
    '_' -> '='
    _ -> '*'

circularDistance :: Int -> Int -> Int -> Int
circularDistance period left right =
    let direct = abs (left - right)
    in min direct (period - direct)

lambdaOrbitParticles
    :: LambdaPalette
    -> Int
    -> Int
    -> Int
    -> Bool
    -> [((Int, Int), Char, V.Attr)]
lambdaOrbitParticles _ _ _ _ False = []
lambdaOrbitParticles palette frame width height True =
    [ (position, character, attr)
    | (offset, (character, attr)) <- zip offsets particleStyles
    , Just position <- [cyclicAt path (frame + offset)]
    ]
  where
    path = lambdaOrbitPath width height
    pathLength = length path
    offsets =
        [ 0
        , pathLength `div` 3
        , 2 * pathLength `div` 3
        ]
    particleStyles =
        [ ('*', palette.lambdaSpark)
        , ('+', palette.lambdaGlow)
        , ('.', palette.lambdaTrail)
        ]

lambdaOrbitPath :: Int -> Int -> [(Int, Int)]
lambdaOrbitPath width height =
    top <> right <> bottom <> left
  where
    horizontal = [2, 4 .. width - 3]
    vertical = [2, 4 .. height - 3]
    top = [(x, 0) | x <- horizontal]
    right = [(width - 1, y) | y <- vertical]
    bottom = [(x, height - 1) | x <- reverse horizontal]
    left = [(0, y) | y <- reverse vertical]

cyclicAt :: [a] -> Int -> Maybe a
cyclicAt [] _ = Nothing
cyclicAt values index =
    case drop (index `mod` length values) values of
        value : _ -> Just value
        [] -> Nothing

drawBlock :: AppState -> UiBlock -> Widget Name
drawBlock state block =
    let ui = state.appUi
        selected = ui.uiSelectedBlock == Just block.blockId
        highlighted = selected && ui.uiFocus == FocusScrollback
        content = case block.blockKind of
            BlockUser ->
                withAttr Theme.userAttr $
                    padAll 1 (txtWrap block.blockBody)
            BlockAssistant ->
                padLeft (Pad 3) $
                    withAttr Theme.assistantAttr
                        (markdownWidgetWithCodeControls
                            (codeBlockHeader state block.blockId)
                            block.blockBody)
            BlockThinking ->
                accentBlock Theme.thinkingAttr
                    (blockStateGlyph ui block <> block.blockTitle)
                    (visibleBody block)
            BlockTool ->
                accentBlock (statusAttr block.blockState)
                    (blockStateGlyph ui block <> block.blockTitle <> detailSuffix block)
                    (visibleBody block)
            BlockShell ->
                accentBlock (statusAttr block.blockState)
                    (blockStateGlyph ui block <> block.blockTitle <> detailSuffix block)
                    (visibleShellBody block)
            BlockEdit ->
                accentBlock (statusAttr block.blockState)
                    (blockStateGlyph ui block <> block.blockTitle <> detailSuffix block)
                    (visibleBody block)
            BlockSystem ->
                withAttr Theme.mutedAttr (txtWrap block.blockBody)
            BlockError ->
                withAttr Theme.errorAttr (txtWrap block.blockBody)
        framed =
            if highlighted
                then withAttr Theme.selectedAttr content
                else content
        rendered =
            clickable (ConversationBlock block.blockId) $
                padBottom (Pad 1) $
                    hBox
                        [ withAttr
                            (if highlighted
                                then Theme.borderActiveAttr
                                else Theme.mutedAttr)
                            (txt (if highlighted then "❯ " else "  "))
                        , framed
                        ]
    in if cacheableBlock block
        then cached
            (ConversationBlockCache
                block.blockId
                highlighted
                block.blockExpanded
                (codeCopyCacheState state block.blockId))
            rendered
        else rendered

codeBlockHeader :: AppState -> BlockId -> Int -> Text -> Widget Name
codeBlockHeader state blockId codeIndex language =
    hBox
        [ if Text.null language
            then emptyWidget
            else withAttr Theme.mutedAttr (txt language)
        , vLimit 1 (fill ' ')
        , clickable name $
            withAttr
                (Composer.controlAttr state name Theme.controlLinkAttr)
                (txt " Copy ")
        ]
  where
    name = CodeCopy blockId codeIndex

codeCopyCacheState :: AppState -> BlockId -> Maybe (Int, Bool)
codeCopyCacheState state blockId =
    case state.appHoveredControl of
        Just (CodeCopy hoveredBlock codeIndex)
            | hoveredBlock == blockId ->
                Just
                    ( codeIndex
                    , state.appPressedControl
                        == Just (CodeCopy blockId codeIndex)
                    )
        _ -> Nothing

cacheableBlock :: UiBlock -> Bool
cacheableBlock block =
    block.blockState
        `notElem` [BlockStreaming, BlockRunning]

blockStateGlyph :: UiState -> UiBlock -> Text
blockStateGlyph state block = case block.blockState of
    BlockRunning -> spinnerFrame state.uiFrame <> " "
    BlockStreaming -> spinnerFrame state.uiFrame <> " "
    BlockComplete -> "✓ "
    BlockFailed -> "✗ "
    BlockDenied -> "⊘ "
    BlockCancelled -> "⊘ "

accentBlock :: AttrName -> Text -> Text -> Widget Name
accentBlock accent title body =
    hBox
        [ withAttr accent (txt "❙")
        , padLeft (Pad 2) $
            vBox $
                [withAttr accent (txt title)]
                    <> if Text.null (Text.strip body)
                        then []
                        else [padTop (Pad 1) (txtWrap body)]
        ]

visibleBody :: UiBlock -> Text
visibleBody block
    | block.blockExpanded = block.blockBody
    | otherwise =
        let rows = Text.lines block.blockBody
            shown = take 3 rows
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

statusAttr :: BlockState -> AttrName
statusAttr state = case state of
    BlockFailed -> Theme.errorAttr
    BlockCancelled -> Theme.mutedAttr
    BlockDenied -> Theme.errorAttr
    BlockComplete -> Theme.successAttr
    BlockRunning -> Theme.toolAttr
    BlockStreaming -> Theme.thinkingAttr

drawNotice :: UiState -> Widget Name
drawNotice state = case state.uiNotice of
    Nothing -> emptyWidget
    Just notice ->
        let (attr, prefix) = noticePresentation state.uiFrame notice.noticeKind
        in withAttr attr $
            padLeftRight 2 (txtWrap (prefix <> notice.noticeText))

noticePresentation :: Int -> NoticeKind -> (AttrName, Text)
noticePresentation frame = \case
    NoticeInfo -> (Theme.footerAttr, "• ")
    NoticeSuccess -> (Theme.successAttr, "✓ ")
    NoticeWarning -> (Theme.thinkingAttr, "⚠ ")
    NoticeProgress -> (Theme.thinkingAttr, spinnerFrame frame <> " ")
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
    footer = case (state.appTextPrompt, state.appChoice, state.appUi.uiFocus) of
        (Just _, _, _) ->
            "Enter submit  │  Shift+Enter newline  │  PgUp/PgDn scroll  │  Esc cancel"
        (Nothing, Just _, _) ->
            "↑↓ select  │  Enter choose  │  Esc cancel"
        (Nothing, Nothing, focus) ->
                case focus of
                    FocusPermission ->
                        "↑↓ select  │  Enter choose  │  Esc deny"
                    FocusScrollback ->
                        "↑↓ blocks  │  Ctrl+J/K lines  │  PgUp/PgDn pages  │  wheel scroll  │  Tab/Space prompt"
                    FocusComposer
                        | not state.appUi.uiAwaitingInput ->
                            "Enter queue  │  Ctrl+Enter/Ctrl+O send now  │  Shift+Enter newline  │  Esc/Ctrl+C cancel  │  Tab scrollback"
                        | otherwise ->
                            "Enter send  │  Shift+Enter newline  │  PgUp/PgDn or wheel scroll  │  Tab scrollback"

drawPermission :: PermissionOverlay -> Widget Name
drawPermission permission =
    centerLayer $
        hLimitPercent 78 $
            overrideAttr Border.borderAttr Theme.borderActiveAttr $
                withBorderStyle unicodeRounded $
                    borderWithLabel (txt " Permission ") $
                        padAll 1 $
                            vBox
                                [ txtWrap ("Allow " <> permission.permissionSummary <> "?")
                                , padTop (Pad 1) $
                                    vBox $
                                        zipWith
                                            (permissionRow permission.permissionIndex)
                                            [0 ..]
                                            [ "Allow once"
                                            , "Always allow this tool this session"
                                            , "Deny"
                                            ]
                                ]

permissionRow :: Int -> Int -> Text -> Widget Name
permissionRow selected index label =
    let prefix = if selected == index then "› " else "  "
        widget = txt (prefix <> label)
        styled =
            if selected == index
                then withAttr Theme.selectedAttr widget
                else widget
    in clickable (PermissionRow index) styled

drawChoice :: AppState -> ChoiceOverlay -> Widget Name
drawChoice appState choice =
    centerLayer $
        hLimitPercent 82 $
            vLimitPercent 78 $
                overrideAttr Border.borderAttr Theme.borderActiveAttr $
                    withBorderStyle unicodeRounded $
                        borderWithLabel
                            (txt (" " <> choice.choiceTitle <> " ")) $
                            padAll 1 $
                                vBox
                                    [ if Text.null (Text.strip choice.choiceBody)
                                        then emptyWidget
                                        else padBottom (Pad 1) $
                                            vLimitPercent 65 $
                                                viewport OverlayViewport Vertical $
                                                    markdownWidget choice.choiceBody
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

drawTextPrompt :: TextOverlay -> Widget Name
drawTextPrompt prompt =
    centerLayer $
        hLimitPercent 82 $
            vLimitPercent 78 $
                overrideAttr Border.borderAttr Theme.borderAttr $
                    withBorderStyle unicodeRounded $
                        borderWithLabel
                            (txt (" " <> prompt.textTitle <> " ")) $
                            padAll 1 $
                                vBox
                                    [ if Text.null (Text.strip prompt.textBody)
                                        then emptyWidget
                                        else padBottom (Pad 1) $
                                            vLimitPercent 60 $
                                                viewport OverlayViewport Vertical $
                                                    markdownWidget prompt.textBody
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
    let content =
            if Text.null prompt.textDraft
                then withAttr Theme.mutedAttr (txt " ")
                else txt prompt.textDraft
        (row, column) =
            Composer.draftCursorLocation prompt.textDraft prompt.textCursor
    in showCursor OverlayCursor (Location (column, row)) content

choiceRow :: AppState -> Int -> Int -> (Text, Text) -> Widget Name
choiceRow appState selected index (label, detail) =
    let prefix = if selected == index then "› " else "  "
        name = ChoiceRow index
        row =
            hBox
                [ txt (prefix <> label)
                , vLimit 1 (fill ' ')
                , withAttr Theme.mutedAttr (txt detail)
                ]
        styled =
            if selected == index
                then withAttr Theme.selectedAttr row
                else row
        interactive = case Composer.controlInteractionAttr appState name of
            Nothing -> styled
            Just attr -> forceAttr attr row
    in clickable name interactive

handleUiEvents :: NonEmpty UiEvent -> EventM Name AppState ()
handleUiEvents uiEvents = do
    initial <- get
    renderedContentHeight <-
        if any isSubmittedPrompt uiEvents
            then conversationUnpaddedContentHeight
            else pure 0
    let
        (final, nativeProgress, shouldFollow, shouldInvalidate) =
            foldl'
                (applyOne renderedContentHeight)
                (initial, Nothing, False, False)
                uiEvents
    put final
    liftIO $
        writeIORef
            final.appRuntime.runtimeRunning
            (uiNeedsTick final.appUi)
    case nativeProgress of
        Nothing -> pure ()
        Just active ->
            liftIO (final.appRuntime.runtimeNativeProgress active)
    when shouldInvalidate invalidateCache
    when shouldFollow $
        case final.appConversationAnchor of
            Just _ -> do
                when (any isSubmittedPrompt uiEvents) $
                    vScrollToEnd (viewportScroll ConversationViewport)
                queueConversationReflow
            Nothing ->
                vScrollToEnd (viewportScroll ConversationViewport)
    when
        (all (== UiTick) uiEvents && final.appUi == initial.appUi)
        continueWithoutRedraw
  where
    applyOne
        renderedContentHeight
        (state, previousProgress, followed, invalidated)
        uiEvent =
            let
                next =
                    applyUiEvent uiEvent $
                        applyConversationUiEvent
                            renderedContentHeight
                            uiEvent
                            state
                progress =
                    case Bridge.nativeProgressSignal uiEvent next.appUi of
                        Nothing -> previousProgress
                        signal -> signal
                follows =
                    followed
                        || (Bridge.eventFollows uiEvent
                            && next.appUi.uiFollow)
                invalidates =
                    invalidated || uiEvent == UiConversationCleared
            in (next, progress, follows, invalidates)

applyConversationUiEvent :: Int -> UiEvent -> AppState -> AppState
applyConversationUiEvent renderedContentHeight uiEvent state =
    case uiEvent of
        UiUserSubmitted text ->
            state
                { appConversationAnchor =
                    Just $
                        Scroll.startConversationAnchor
                            (BlockId state.appUi.uiNextBlockId)
                            text
                            (if null state.appUi.uiBlocks
                                then 0
                                else renderedContentHeight)
                }
        UiConversationCleared ->
            state
                { appConversationAnchor = Nothing
                , appConversationReflowQueued = False
                }
        _ -> state

applyUiEvent :: UiEvent -> AppState -> AppState
applyUiEvent uiEvent state =
    Composer.applyComposerUiEvent uiEvent $
        state { appUi = reduceUi uiEvent state.appUi }

handleEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleEvent event = case event of
    AppEvent AppStop -> do
        modify' \state -> state { appWorkerStopped = True }
        halt
    AppEvent (AppSetSkillCommands skills) -> do
        state <- get
        if state.appSkillCommands == skills
            then continueWithoutRedraw
            else modify' \current -> current
                { appSkillCommands = skills
                , appSlashIndex = 0
                , appSlashDismissed = False
                }
    AppEvent (AppSetImagePreviews images) ->
        modify' \state ->
            state
                { appImagePreviews =
                    mapMaybe
                        (either (const Nothing) Just . prepareTuiImagePreview)
                        images
                }
    AppEvent (AppSetWindowTitle title) -> do
        state <- get
        liftIO (state.appRuntime.runtimeSetWindowTitle title)
    AppEvent (AppAgentSnapshot selected entries) -> do
        state <- get
        let normalized =
                Bridge.normalizeAgentSelection selected entries
        if state.appAgentSelected == normalized
            && state.appAgentEntries == entries
            then continueWithoutRedraw
            else do
                modify' \current ->
                    current
                        { appAgentSelected = normalized
                        , appAgentEntries = entries
                        , appAgentHover =
                            if normalized /= current.appAgentSelected
                                || length entries <= 1
                                || agentLayoutTargets entries
                                    /= agentLayoutTargets
                                        current.appAgentEntries
                                then Nothing
                                else
                                    current.appAgentHover >>= \hover ->
                                        if any
                                            ((== hover.agentHoverTarget)
                                                . (.agentTarget))
                                            entries
                                            then Just hover
                                            else Nothing
                        }
                when
                    ((length state.appAgentEntries > 1)
                        /= (length entries > 1))
                    do
                        invalidateCache
                        queueConversationReflow
    AppEvent (AppUi uiEvent) ->
        handleUiEvents (uiEvent :| [])
    AppEvent (AppUiBatch uiEvents) ->
        handleUiEvents uiEvents
    AppEvent AppConversationReflow -> do
        modify' \state ->
            state { appConversationReflowQueued = False }
        reflowConversation
    AppEvent (AppAskPermission summary reply) ->
        modify' \state ->
            state
                { appUi = reduceUi (UiPermissionShown summary) state.appUi
                , appPermissionReply = Just reply
                , appAgentHover = Nothing
                }
    AppEvent (AppAskChoice title body initial rows reply) -> do
        modify' \state ->
            state
                { appChoice = Just ChoiceOverlay
                    { choiceTitle = title
                    , choiceBody = body
                    , choiceIndex =
                        max 0 (min (max 0 (length rows - 1)) initial)
                    , choiceRows = rows
                    }
                , appChoiceReply = Just (atomically . putTMVar reply)
                , appAgentHover = Nothing
                }
        vScrollToBeginning (viewportScroll OverlayViewport)
    AppEvent (AppAskText title body initial reply) -> do
        modify' \state ->
            state
                { appTextPrompt = Just TextOverlay
                    { textTitle = title
                    , textBody = body
                    , textDraft = initial
                    , textCursor = Text.length initial
                    }
                , appTextReply = Just reply
                , appAgentHover = Nothing
                }
        vScrollToBeginning (viewportScroll OverlayViewport)
    AppEvent (AppSuspend action reply) -> do
        state <- get
        suspendAndResume do
            result <- tryAny action
            atomically (putTMVar reply result)
            pure state { appAgentHover = Nothing }
    MouseDown name button _ _ -> do
        unless (isAgentHoverSurface name) clearAgentHover
        state <- get
        case ( state.appTextPrompt
             , state.appChoice
             , state.appUi.uiPermission
             ) of
            (Just _, _, _) ->
                case button of
                    V.BScrollUp ->
                        vScrollBy
                            (viewportScroll OverlayViewport)
                            (-mouseScrollLines)
                    V.BScrollDown ->
                        vScrollBy
                            (viewportScroll OverlayViewport)
                            mouseScrollLines
                    _ -> pure ()
            (Nothing, Nothing, Nothing) ->
                case (name, button) of
                    (ComposerModel, V.BLeft) ->
                        Composer.handleControlMouseDown ComposerModel
                    (ComposerEffort, V.BLeft) ->
                        Composer.handleControlMouseDown ComposerEffort
                    (ComposerMode, V.BLeft) ->
                        Composer.handleControlMouseDown ComposerMode
                    (CodeCopy blockId codeIndex, V.BLeft) ->
                        Composer.handleControlMouseDown
                            (CodeCopy blockId codeIndex)
                    (SlashRow index, V.BLeft) ->
                        Composer.activateSlashAt
                            handleCtrlC
                            scrollConversationPage
                            index
                    (SlashRow _, V.BScrollUp) ->
                        Composer.handleComposerKey
                            handleCtrlC
                            scrollConversationPage
                            (V.EvKey V.KUp [])
                    (SlashRow _, V.BScrollDown) ->
                        Composer.handleComposerKey
                            handleCtrlC
                            scrollConversationPage
                            (V.EvKey V.KDown [])
                    (AgentRow target, V.BLeft) -> do
                        clearAgentHover
                        liftIO
                            (state.appRuntime.runtimeAgentSelect target)
                        modify' \current ->
                            current { appAgentSelected = target }
                    (AgentPopover target, V.BLeft) -> do
                        keepAgentHover target
                        liftIO
                            (state.appRuntime.runtimeAgentSelect target)
                        modify' \current ->
                            current { appAgentSelected = target }
                    _ -> handleMouseDown name button
            (Nothing, Just _, _) ->
                case (name, button) of
                    (ChoiceRow index, V.BLeft) ->
                        Composer.handleControlMouseDown (ChoiceRow index)
                    (ChoiceRow _, V.BScrollUp) ->
                        handleChoiceKey (V.EvKey V.KUp [])
                    (ChoiceRow _, V.BScrollDown) ->
                        handleChoiceKey (V.EvKey V.KDown [])
                    (_, V.BScrollUp) ->
                        vScrollBy
                            (viewportScroll OverlayViewport)
                            (-mouseScrollLines)
                    (_, V.BScrollDown) ->
                        vScrollBy
                            (viewportScroll OverlayViewport)
                            mouseScrollLines
                    _ -> pure ()
            (Nothing, Nothing, Just _) ->
                case (name, button) of
                    (PermissionRow index, V.BLeft) ->
                        resolvePermission (permissionChoiceAt index)
                    _ -> pure ()
    -- The patched vty-unix backend represents no-button pointer motion as
    -- MouseUp Nothing so Brick can route it through clickable extents.
    MouseUp (AgentRow target) Nothing _ ->
        rememberAgentHover target
    MouseUp (AgentPopover target) Nothing _ ->
        keepAgentHover target
    MouseUp AgentPane Nothing _ ->
        pure ()
    MouseUp name button _
        | isInteractiveControl name
        , button == Just V.BLeft || button == Nothing -> do
            clearAgentHover
            Composer.handleControlMouseUp name (activateControl name)
    MouseUp _ Nothing _ ->
        modify' \state ->
            state
                { appHoveredControl = Nothing
                , appAgentHover = Nothing
                }
    VtyEvent (V.EvMouseDown _ _ V.BLeft _) ->
        modify' \state ->
            state
                { appHoveredControl = Nothing
                , appAgentHover = Nothing
                }
    VtyEvent (V.EvMouseUp _ _ _) ->
        modify' \state ->
            state
                { appHoveredControl = Nothing
                , appPressedControl = Nothing
                , appAgentHover = Nothing
                }
    VtyEvent V.EvResize{} -> do
        clearAgentHover
        invalidateCache
        queueConversationReflow
    VtyEvent vtyEvent -> do
        clearAgentHover
        state <- get
        case (state.appTextPrompt, state.appChoice, state.appUi.uiPermission) of
            (Just _, _, _) -> handleTextPromptKey vtyEvent
            (Nothing, Just _, _) -> handleChoiceKey vtyEvent
            (Nothing, Nothing, Just _) -> handlePermissionKey vtyEvent
            (Nothing, Nothing, Nothing) -> handleNormalKey vtyEvent
    _ -> pure ()

handlePermissionKey :: V.Event -> EventM Name AppState ()
handlePermissionKey = \case
    V.EvKey V.KUp [] -> movePermission (-1)
    V.EvKey V.KDown [] -> movePermission 1
    V.EvKey V.KBackTab [] -> movePermission (-1)
    V.EvKey (V.KChar '\t') [] -> movePermission 1
    V.EvKey (V.KChar 'y') [] -> resolvePermission PermissionAllowOnce
    V.EvKey (V.KChar 'a') [] -> resolvePermission PermissionAllowTool
    V.EvKey (V.KChar 'n') [] -> resolvePermission PermissionDeny
    V.EvKey V.KEsc [] -> resolvePermission PermissionDeny
    V.EvKey (V.KChar 'c') modifiers
        | V.MCtrl `elem` modifiers -> do
            _ <- handleCtrlC
            resolvePermission PermissionDeny
    V.EvKey V.KEnter [] -> do
        state <- get
        let choice = case state.appUi.uiPermission of
                Just permission ->
                    permissionChoiceAt permission.permissionIndex
                Nothing -> PermissionDeny
        resolvePermission choice
    _ -> pure ()
  where
    movePermission delta =
        modify' \state ->
            state { appUi = reduceUi (UiPermissionMoved delta) state.appUi }

permissionChoiceAt :: Int -> PermissionChoice
permissionChoiceAt = \case
    0 -> PermissionAllowOnce
    1 -> PermissionAllowTool
    _ -> PermissionDeny

resolvePermission
    :: PermissionChoice
    -> EventM Name AppState ()
resolvePermission choice = do
    state <- get
    case state.appPermissionReply of
        Nothing -> pure ()
        Just reply ->
            liftIO $ atomically (putTMVar reply (Just choice))
    modify' \current ->
        current
            { appUi = reduceUi UiPermissionHidden current.appUi
            , appPermissionReply = Nothing
            }

handleCtrlC :: EventM Name AppState CtrlCDecision
handleCtrlC = do
    state <- get
    decision <- liftIO state.appRuntime.runtimeCtrlC
    case decision of
        SoftCancel ->
            modify' \current ->
                current
                    { appUi =
                        reduceUi
                            (UiSetNotice
                                (Just
                                    (warningNotice
                                        "Interrupted; press Ctrl-C again to exit.")))
                            current.appUi
                    }
        WarnExit ->
            modify' \current ->
                current
                    { appUi =
                        reduceUi
                            (UiSetNotice
                                (Just
                                    (warningNotice
                                        "Press Ctrl-C again to exit.")))
                            current.appUi
                    }
        ForceExit ->
            liftIO (throwIO UserInterrupt)
    pure decision

handleNormalKey :: V.Event -> EventM Name AppState ()
handleNormalKey event
    | Bridge.isSendNowKey event =
        Composer.handleComposerKey
            handleCtrlC
            scrollConversationPage
            event
    | otherwise = do
        case event of
            V.EvMouseDown _ _ V.BScrollUp _ ->
                scrollConversationBy (-mouseScrollLines)
            V.EvMouseDown _ _ V.BScrollDown _ ->
                scrollConversationBy mouseScrollLines
            _ -> do
                state <- get
                case state.appUi.uiFocus of
                    FocusScrollback -> handleScrollbackKey event
                    FocusComposer ->
                        Composer.handleComposerKey
                            handleCtrlC
                            scrollConversationPage
                            event
                    FocusPermission -> pure ()

rememberAgentHover :: AgentTarget -> EventM Name AppState ()
rememberAgentHover target = do
    rowExtent <- lookupExtent (AgentRow target)
    paneExtent <- lookupExtent AgentPane
    case rowExtent of
        Nothing -> pure ()
        Just extent ->
            let Location (rowX, rowY) = extent.extentUpperLeft
                fallbackPaneUpperLeft =
                    Location (max 0 (rowX - 2), max 0 (rowY - 2))
                fallbackPaneWidth = extentWidth extent + 4
            in
            modify' \state ->
                state
                    { appAgentHover =
                        Just AgentHover
                            { agentHoverTarget = target
                            , agentHoverUpperLeft = extent.extentUpperLeft
                            , agentHoverPaneUpperLeft =
                                maybe
                                    fallbackPaneUpperLeft
                                    (.extentUpperLeft)
                                    paneExtent
                            , agentHoverPaneWidth =
                                maybe
                                    fallbackPaneWidth
                                    extentWidth
                                    paneExtent
                            }
                    }
  where
    extentWidth extent = fst extent.extentSize

keepAgentHover :: AgentTarget -> EventM Name AppState ()
keepAgentHover target = do
    state <- get
    case state.appAgentHover of
        Just hover
            | hover.agentHoverTarget == target -> pure ()
        _ -> rememberAgentHover target

clearAgentHover :: EventM Name AppState ()
clearAgentHover =
    modify' \state ->
        state { appAgentHover = Nothing }

isAgentHoverSurface :: Name -> Bool
isAgentHoverSurface = \case
    AgentPane -> True
    AgentRow _ -> True
    AgentPopover _ -> True
    _ -> False

agentLayoutTargets :: [AgentEntry] -> [AgentTarget]
agentLayoutTargets =
    map (.agentTarget) . sortOn (.agentPath)

handleMouseDown :: Name -> V.Button -> EventM Name AppState ()
handleMouseDown name button =
    case button of
        V.BScrollUp -> scrollConversationBy (-mouseScrollLines)
        V.BScrollDown -> scrollConversationBy mouseScrollLines
        V.BLeft -> case name of
            ConversationBlock ident ->
                modify' \state ->
                    state
                        { appUi =
                            reduceUi
                                (UiFocusChanged FocusScrollback)
                                (reduceUi (UiSelectBlock ident) state.appUi)
                        }
            ComposerArea ->
                modify' \state ->
                    state
                        { appUi =
                            reduceUi
                                (UiFocusChanged FocusComposer)
                                state.appUi
                        }
            _ -> pure ()
        _ -> pure ()

mouseScrollLines :: Int
mouseScrollLines = 3

handleScrollbackKey :: V.Event -> EventM Name AppState ()
handleScrollbackKey = \case
    V.EvKey V.KUp [] -> moveBlock (-1)
    V.EvKey V.KDown [] -> moveBlock 1
    V.EvKey V.KPageUp [] -> scrollConversationPage Up
    V.EvKey V.KPageDown [] -> scrollConversationPage Down
    V.EvKey (V.KChar 'k') modifiers
        | V.MCtrl `elem` modifiers -> scrollConversationBy (-1)
    V.EvKey (V.KChar 'j') modifiers
        | V.MCtrl `elem` modifiers -> scrollConversationBy 1
    V.EvKey (V.KChar 'u') modifiers
        | V.MCtrl `elem` modifiers -> scrollConversationHalfPage Up
    V.EvKey (V.KChar 'd') modifiers
        | V.MCtrl `elem` modifiers -> scrollConversationHalfPage Down
    V.EvKey V.KHome [] -> do
        leaveFollow
        vScrollToBeginning scroll
        queueConversationReflow
    V.EvKey V.KEnd [] -> resumeFollow
    V.EvKey (V.KChar 'g') [] -> do
        leaveFollow
        vScrollToBeginning scroll
        queueConversationReflow
    V.EvKey (V.KChar 'G') [] -> resumeFollow
    V.EvKey V.KLeft [] -> toggle
    V.EvKey V.KRight [] -> toggle
    V.EvKey V.KEnter [] -> toggle
    V.EvKey (V.KChar 'y') [] -> copySelected
    V.EvKey (V.KChar ' ') [] -> focusComposer
    V.EvKey (V.KChar '\t') [] -> focusComposer
    V.EvKey V.KEsc [] -> focusComposer
    _ -> pure ()
  where
    scroll = viewportScroll ConversationViewport
    moveBlock delta = do
        modify' \state ->
            state { appUi = reduceUi (UiMoveSelection delta) state.appUi }
        state <- get
        case state.appUi.uiSelectedBlock of
            Just ident -> do
                makeVisible (ConversationBlock ident)
                queueConversationReflow
            Nothing -> pure ()
    toggle = do
        modify' \state ->
            state { appUi = reduceUi UiToggleSelected state.appUi }
        queueConversationReflow
    focusComposer =
        modify' \state ->
            state { appUi = reduceUi (UiFocusChanged FocusComposer) state.appUi }
    leaveFollow =
        modify' \state ->
            state { appUi = reduceUi (UiSetFollow False) state.appUi }
    resumeFollow = do
        modify' \state ->
            state
                { appUi = reduceUi (UiSetFollow True) state.appUi
                , appConversationAnchor =
                    Scroll.followConversationTail
                        <$> state.appConversationAnchor
                }
        vScrollToEnd scroll
        queueConversationReflow
    copySelected = do
        state <- get
        case state.appUi.uiSelectedBlock >>= selectedBlock state.appUi of
            Nothing -> pure ()
            Just block -> do
                copied <- liftIO $
                    state.appRuntime.runtimeCopy block.blockBody
                modify' \current ->
                    current
                        { appUi =
                            reduceUi
                                (UiSetNotice
                                    (Just
                                        (if copied
                                            then successNotice
                                                "Copied selected block."
                                            else warningNotice
                                                "Terminal clipboard is unavailable.")))
                                current.appUi
                        }

scrollConversationPage :: Direction -> EventM Name AppState ()
scrollConversationPage direction = do
    height <- conversationViewportHeight
    scrollConversationBy $
        case direction of
            Up -> negate height
            Down -> height

scrollConversationHalfPage :: Direction -> EventM Name AppState ()
scrollConversationHalfPage direction = do
    height <- conversationViewportHeight
    let amount = max 1 (height `div` 2)
    scrollConversationBy $
        case direction of
            Up -> negate amount
            Down -> amount

conversationViewportHeight :: EventM Name AppState Int
conversationViewportHeight =
    lookupViewport ConversationViewport >>= \case
        Just (VP _ _ (_, height) _) -> pure (max 1 height)
        Nothing -> pure 1

scrollConversationBy :: Int -> EventM Name AppState ()
scrollConversationBy amount
    | amount == 0 = pure ()
    | amount < 0 = do
        setConversationFollow False
        vScrollBy scroll amount
        queueConversationReflow
    | otherwise =
        lookupViewport ConversationViewport >>= \case
            Just (VP _ top (_, height) (_, contentHeight))
                | top + height + amount >= contentHeight -> do
                    setConversationFollow True
                    vScrollToEnd scroll
                    queueConversationReflow
            _ -> do
                setConversationFollow False
                vScrollBy scroll amount
                queueConversationReflow
  where
    scroll = viewportScroll ConversationViewport

setConversationFollow :: Bool -> EventM Name AppState ()
setConversationFollow follow =
    modify' \state ->
        state { appUi = reduceUi (UiSetFollow follow) state.appUi }

conversationUnpaddedContentHeight :: EventM Name AppState Int
conversationUnpaddedContentHeight =
    lookupViewport ConversationViewport >>= \case
        Just (VP _ _ _ (_, contentHeight)) -> do
            renderedReserveRows <- conversationRenderedReserveRows
            pure $ max 0 (contentHeight - renderedReserveRows)
        Nothing -> pure 0

queueConversationReflow :: EventM Name AppState ()
queueConversationReflow = do
    state <- get
    unless state.appConversationReflowQueued do
        modify' \current ->
            current { appConversationReflowQueued = True }
        liftIO $
            enqueueAppEvent
                state.appRuntime
                AppConversationReflow

reflowConversation :: EventM Name AppState ()
reflowConversation = do
    state <- get
    case state.appConversationAnchor of
        Nothing -> pure ()
        Just anchor ->
            lookupViewport ConversationViewport >>= \case
                Nothing -> pure ()
                Just (VP _ top (_, height) (_, contentHeight)) -> do
                    renderedReserveRows <-
                        conversationRenderedReserveRows
                    let unpaddedContentHeight =
                            max 0
                                (contentHeight - renderedReserveRows)
                        (next, scrollAction) =
                            Scroll.reflowConversationAnchor
                                state.appUi.uiFollow
                                top
                                height
                                unpaddedContentHeight
                                anchor
                    modify' \current ->
                        current { appConversationAnchor = Just next }
                    case scrollAction of
                        Scroll.KeepConversationPosition -> pure ()
                        Scroll.ScrollConversationToEnd ->
                            vScrollToEnd
                                (viewportScroll ConversationViewport)

conversationRenderedReserveRows :: EventM Name AppState Int
conversationRenderedReserveRows =
    lookupExtent ConversationReserve >>= \case
        Just (Extent _ _ (_, reserveRows)) -> pure reserveRows
        Nothing -> pure 0

isSubmittedPrompt :: UiEvent -> Bool
isSubmittedPrompt = \case
    UiUserSubmitted _ -> True
    _ -> False

selectedBlock :: UiState -> BlockId -> Maybe UiBlock
selectedBlock state ident =
    find ((== ident) . (.blockId)) (toList state.uiBlocks)
