-- | Retained fullscreen terminal application and its session bridge.
module Agent.CLI.TUI.App
    ( FullscreenRuntime
    , emitUiEvent
    , newFullscreenRuntime
    , readFullscreenLine
    , requestFullscreenPermission
    , requestFullscreenChoice
    , runFullscreen
    , withFullscreenSuspended
    ) where

import Agent.CLI.Input (ReplLine(..), terminalTextWidth)
import Agent.CLI.Interrupt (CtrlCDecision(..))
import Agent.CLI.Command
    ( ReplAction(..)
    , SlashMenu(..)
    , SlashSuggestion(..)
    , parseReplLine
    , slashMenuFor
    )
import Agent.CLI.Permission (PermissionChoice(..))
import Agent.CLI.ReplMode (replModeLabel)
import Agent.CLI.Render (summarizeToolCall)
import Agent.CLI.Status (formatTokenUsage)
import qualified Agent.CLI.TUI.Theme as Theme
import Agent.CLI.TUI.Markdown (markdownWidget)
import Agent.CLI.UI.Model
import Agent.Loop (LoopEvent(..))
import Agent.ToolDispatch (ToolCall(..))
import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center (centerLayer)
import Control.Concurrent.Async (wait, waitCatch, withAsync)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
    ( TMVar
    , TQueue
    , atomically
    , newEmptyTMVarIO
    , newTQueueIO
    , putTMVar
    , readTMVar
    , readTQueue
    , writeTQueue
    )
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (modify')
import Control.Exception.Safe (SomeException, finally, throwIO, tryAny)
import Control.Exception (AsyncException(UserInterrupt))
import Data.ByteString (ByteString)
import Data.Char (isControl)
import Data.Foldable (toList)
import Data.List (find, intersperse)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as Vty

data Name
    = ConversationViewport
    | ComposerCursor
    | ComposerModel
    | ComposerEffort
    deriving (Eq, Ord, Show)

data AppEvent
    = AppUi !UiEvent
    | AppAskPermission !Text !(TMVar (Maybe PermissionChoice))
    | AppAskChoice
        !Text
        !Int
        ![(Text, Text)]
        !(TMVar (Maybe Int))
    | forall a. AppSuspend !(IO a) !(TMVar (Either SomeException a))
    | AppStop

data FullscreenRuntime = FullscreenRuntime
    { runtimeEvents :: !(BChan AppEvent)
    , runtimeInput :: !(TQueue ReplLine)
    , runtimeCancel :: !(IO ())
    , runtimeCtrlC :: !(IO CtrlCDecision)
    , runtimeCopy :: !(Text -> IO Bool)
    , runtimeNativeProgress :: !(Bool -> IO ())
    , runtimeInitial :: !UiState
    }

data AppState = AppState
    { appUi :: !UiState
    , appPermissionReply :: !(Maybe (TMVar (Maybe PermissionChoice)))
    , appRuntime :: !FullscreenRuntime
    , appSlashIndex :: !Int
    , appChoice :: !(Maybe ChoiceOverlay)
    , appChoiceReply :: !(Maybe (TMVar (Maybe Int)))
    , appSlashDismissed :: !Bool
    , appPasted :: !Bool
    }

data ChoiceOverlay = ChoiceOverlay
    { choiceTitle :: !Text
    , choiceIndex :: !Int
    , choiceRows :: ![(Text, Text)]
    }

newFullscreenRuntime
    :: IO ()
    -> IO CtrlCDecision
    -> (Text -> IO Bool)
    -> (Bool -> IO ())
    -> UiState
    -> IO FullscreenRuntime
newFullscreenRuntime
    cancelAction
    ctrlCAction
    copyAction
    nativeProgress
    initial = FullscreenRuntime
    <$> newBChan 512
    <*> newTQueueIO
    <*> pure cancelAction
    <*> pure ctrlCAction
    <*> pure copyAction
    <*> pure nativeProgress
    <*> pure initial

emitUiEvent :: FullscreenRuntime -> UiEvent -> IO ()
emitUiEvent runtime = writeBChan runtime.runtimeEvents . AppUi

readFullscreenLine
    :: FullscreenRuntime
    -> PromptState
    -> Text
    -> IO ReplLine
readFullscreenLine runtime prompt initial = do
    emitUiEvent runtime (UiSetPrompt prompt)
    emitUiEvent runtime (UiSetDraft initial (Text.length initial))
    emitUiEvent runtime (UiSetAwaitingInput True)
    atomically (readTQueue runtime.runtimeInput)

requestFullscreenPermission
    :: FullscreenRuntime
    -> ToolCall
    -> IO (Maybe PermissionChoice)
requestFullscreenPermission runtime call = do
    reply <- newEmptyTMVarIO
    let summary = summarizeToolCall call
    writeBChan runtime.runtimeEvents (AppAskPermission summary reply)
    atomically (readTMVar reply)

requestFullscreenChoice
    :: FullscreenRuntime
    -> Text
    -> Int
    -> [(Text, Text)]
    -> IO (Maybe Int)
requestFullscreenChoice runtime title initial rows = do
    reply <- newEmptyTMVarIO
    writeBChan runtime.runtimeEvents
        (AppAskChoice title initial rows reply)
    atomically (readTMVar reply)

withFullscreenSuspended :: FullscreenRuntime -> IO a -> IO a
withFullscreenSuspended runtime action = do
    reply <- newEmptyTMVarIO
    writeBChan runtime.runtimeEvents (AppSuspend action reply)
    atomically (readTMVar reply) >>= either throwIO pure

runFullscreen :: FullscreenRuntime -> IO a -> IO a
runFullscreen runtime workerAction = do
    let vtyConfig =
            V.defaultConfig
                { V.configPreferredColorMode = Just V.FullColor
                }
        buildVty = do
            vty <- Vty.mkVty vtyConfig
            V.setMode (V.outputIface vty) V.Mouse True
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
            , appSlashDismissed = False
            , appPasted = False
            }
    withAsync workerAction \worker ->
        withAsync ticker \_ticker ->
            withAsync
                (void (waitCatch worker)
                    >> writeBChan runtime.runtimeEvents AppStop)
                \_notifier -> do
                    void
                        (customMain
                            initialVty
                            buildVty
                            (Just runtime.runtimeEvents)
                            fullscreenApp
                            initialState)
                        `finally` runtime.runtimeNativeProgress False
                    atomically $
                        writeTQueue runtime.runtimeInput ReplEof
                    wait worker
  where
    ticker = forever do
        threadDelay 100000
        writeBChan runtime.runtimeEvents (AppUi UiTick)

handleChoiceKey :: V.Event -> EventM Name AppState ()
handleChoiceKey = \case
    V.EvKey V.KUp [] -> moveChoice (-1)
    V.EvKey V.KDown [] -> moveChoice 1
    V.EvKey V.KBackTab [] -> moveChoice (-1)
    V.EvKey (V.KChar '\t') [] -> moveChoice 1
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

handleStatusClick
    :: (Text -> ReplLine)
    -> EventM Name AppState ()
handleStatusClick choice = do
    state <- get
    let ui = state.appUi
        overlayOpen =
            maybe False (const True) state.appChoice
                || maybe False (const True) ui.uiPermission
    if ui.uiAwaitingInput && not overlayOpen
        then do
            liftIO $ atomically $
                writeTQueue state.appRuntime.runtimeInput
                    (choice ui.uiDraft)
            modify' \current ->
                current
                    { appUi =
                        reduceUi
                            (UiSetAwaitingInput False)
                            current.appUi
                    }
        else
            modify' \current ->
                current
                    { appUi =
                        reduceUi
                            (UiSetNotice
                                (Just "Model and reasoning settings can be changed at the prompt."))
                            current.appUi
                    }

resolveChoice :: Bool -> EventM Name AppState ()
resolveChoice confirmed = do
    state <- get
    case state.appChoiceReply of
        Nothing -> pure ()
        Just reply ->
            liftIO $ atomically $
                putTMVar reply $
                    if confirmed
                        then (.choiceIndex) <$> state.appChoice
                        else Nothing
    modify' \current ->
        current
            { appChoice = Nothing
            , appChoiceReply = Nothing
            }

fullscreenApp :: App AppState AppEvent Name
fullscreenApp = App
    { appDraw = drawApp
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = vScrollToEnd (viewportScroll ConversationViewport)
    , appAttrMap = const Theme.solarizedDark
    }

drawApp :: AppState -> [Widget Name]
drawApp state =
    case (state.appChoice, state.appUi.uiPermission) of
        (Just choice, _) ->
            [ drawChoice choice
            , drawMain state
            ]
        (Nothing, Just permission) ->
            [ drawPermission permission
            , drawMain state
            ]
        (Nothing, Nothing) -> [drawMain state]

drawMain :: AppState -> Widget Name
drawMain state =
    withAttr Theme.baseAttr $
        vBox
            [ drawHeader state.appUi
            , padTop (Pad 1) $
                withVScrollBars OnRight $
                    viewport ConversationViewport Vertical $
                        padLeftRight 2 (drawTranscript state.appUi)
            , drawNotice state.appUi
            , drawSlashMenu state
            , drawComposer state.appUi
            , drawFooter state
            ]

drawHeader :: UiState -> Widget Name
drawHeader state =
    withAttr Theme.headerAttr $
        padLeftRight 2 $
            hBox
                [ hLimitPercent 68 (txt left)
                , fill ' '
                , txt right
                ]
  where
    left =
        (if Text.null state.uiBranch then "" else "git " <> state.uiBranch <> "  ")
            <> state.uiCwd
    usage = formatTokenUsage state.uiPrompt.promptUsage
    right =
        (if state.uiRunning then spinnerFrame state.uiFrame <> " " else "")
            <> state.uiActivity
            <> if Text.null usage then "" else " │ " <> usage

spinnerFrame :: Int -> Text
spinnerFrame frame =
    ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]
        !! (frame `mod` 10)

drawTranscript :: UiState -> Widget Name
drawTranscript state
    | null blocks =
        withAttr Theme.mutedAttr $
            padTop (Pad 2) $
                txt "Start by describing what you want to build or change."
    | otherwise =
        vBox (map (drawBlock state) blocks)
  where
    blocks = toList state.uiBlocks

drawBlock :: UiState -> UiBlock -> Widget Name
drawBlock state block =
    let selected = state.uiSelectedBlock == Just block.blockId
        content = case block.blockKind of
            BlockUser ->
                withAttr Theme.userAttr $
                    padAll 1 (txtWrap block.blockBody)
            BlockAssistant ->
                padLeft (Pad 3) $
                    withAttr Theme.assistantAttr
                        (markdownWidget block.blockBody)
            BlockThinking ->
                accentBlock Theme.thinkingAttr
                    ("◆ " <> block.blockTitle)
                    (visibleBody block)
            BlockTool ->
                accentBlock (statusAttr block.blockState)
                    ("◆ " <> block.blockTitle <> detailSuffix block)
                    (visibleBody block)
            BlockShell ->
                accentBlock (statusAttr block.blockState)
                    ("◆ " <> block.blockTitle <> detailSuffix block)
                    (visibleShellBody block)
            BlockEdit ->
                accentBlock (statusAttr block.blockState)
                    ("◆ " <> block.blockTitle <> detailSuffix block)
                    (visibleBody block)
            BlockSystem ->
                withAttr Theme.mutedAttr (txtWrap block.blockBody)
            BlockError ->
                withAttr Theme.errorAttr (txtWrap block.blockBody)
        framed =
            if selected && state.uiFocus == FocusScrollback
                then withAttr Theme.selectedAttr content
                else content
    in padBottom (Pad 1) framed

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
        withAttr Theme.footerAttr $
            padLeftRight 2 (txtWrap notice)

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
                    borderWithLabel (txt " Commands ") $
                        vBox $
                            zipWith
                                (drawSlashRow selected)
                                [start ..]
                                suggestions

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
    in if selected == index
        then withAttr Theme.selectedAttr row
        else row

drawComposer :: UiState -> Widget Name
drawComposer state =
    let focused = state.uiFocus == FocusComposer
        attr = if focused then Theme.borderActiveAttr else Theme.borderAttr
        mode = replModeLabel state.uiPrompt.promptMode
        usage = formatTokenUsage state.uiPrompt.promptUsage
        leading =
            filter (not . Text.null)
                [ if state.uiPrompt.promptAttachments > 0
                    then "📎 "
                        <> Text.pack
                            (show state.uiPrompt.promptAttachments)
                    else ""
                , usage
                ]
        modelControl
            | Text.null state.uiPrompt.promptModel = []
            | otherwise =
                [ clickable ComposerModel $
                    txt state.uiPrompt.promptModel
                ]
        effortControl
            | Text.null state.uiPrompt.promptEffort = []
            | otherwise =
                [ clickable ComposerEffort $
                    txt (" (" <> state.uiPrompt.promptEffort <> ")")
                ]
        modelAndEffort = case modelControl <> effortControl of
            [] -> []
            controls -> [hBox controls]
        labelWidgets =
            map txt leading
                <> modelAndEffort
                <> [txt mode | not (Text.null mode)]
        label =
            if null labelWidgets
                then txt " "
                else hBox
                    (txt " " : intersperse (txt " · ") labelWidgets <> [txt " "])
        editor =
            padLeftRight 1 $
                hBox
                    [ withAttr Theme.assistantAttr (txt "❯ ")
                    , renderDraft focused state
                    , vLimit 1 (fill ' ')
                    ]
    in withAttr attr $
        withBorderStyle unicodeRounded $
            borderWithLabel (withAttr Theme.footerAttr label) $
                editor

renderDraft :: Bool -> UiState -> Widget Name
renderDraft focused state =
    let content =
            if Text.null state.uiDraft
                then withAttr Theme.mutedAttr (txt " ")
                else txt state.uiDraft
        (row, column) = draftCursorLocation state.uiDraft state.uiCursor
    in if focused
        then showCursor ComposerCursor (Location (column, row)) content
        else content

draftCursorLocation :: Text -> Int -> (Int, Int)
draftCursorLocation text cursor =
    let before = Text.take cursor text
        rows = Text.splitOn "\n" before
    in case reverse rows of
        [] -> (0, 0)
        lastRow : rest -> (length rest, terminalTextWidth lastRow)

drawFooter :: AppState -> Widget Name
drawFooter state =
    withAttr Theme.footerAttr $
        padLeftRight 2 $
            txt footer
  where
    footer = case (state.appChoice, state.appUi.uiFocus) of
        (Just _, _) ->
            "↑↓ select  │  Enter choose  │  Esc cancel"
        (Nothing, focus) ->
                case focus of
                    FocusPermission ->
                        "↑↓ select  │  Enter choose  │  Esc deny"
                    FocusScrollback ->
                        "↑↓ blocks  │  ←→ fold  │  PgUp/PgDn scroll  │  Tab/Space prompt"
                    FocusComposer
                        | state.appUi.uiRunning ->
                            "Esc/Ctrl+C cancel  │  Tab scrollback"
                        | otherwise ->
                            "Enter send  │  Shift+Enter newline  │  Shift+Tab mode  │  Tab scrollback"

drawPermission :: PermissionOverlay -> Widget Name
drawPermission permission =
    centerLayer $
        hLimitPercent 78 $
            withAttr Theme.borderActiveAttr $
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
    in if selected == index
        then withAttr Theme.selectedAttr widget
        else widget

drawChoice :: ChoiceOverlay -> Widget Name
drawChoice choice =
    centerLayer $
        hLimitPercent 82 $
            vLimitPercent 78 $
                withAttr Theme.borderActiveAttr $
                    withBorderStyle unicodeRounded $
                        borderWithLabel
                            (txt (" " <> choice.choiceTitle <> " ")) $
                            padAll 1 $
                                vBox $
                                    zipWith
                                        (choiceRow choice.choiceIndex)
                                        [start ..]
                                        rows
  where
    count = length choice.choiceRows
    start =
        max 0 (min choice.choiceIndex (max 0 (count - 14)))
    rows = take 14 (drop start choice.choiceRows)

choiceRow :: Int -> Int -> (Text, Text) -> Widget Name
choiceRow selected index (label, detail) =
    let prefix = if selected == index then "› " else "  "
        row =
            hBox
                [ txt (prefix <> label)
                , vLimit 1 (fill ' ')
                , withAttr Theme.mutedAttr (txt detail)
                ]
    in if selected == index
        then withAttr Theme.selectedAttr row
        else row

handleEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleEvent event = case event of
    AppEvent AppStop ->
        halt
    AppEvent (AppUi uiEvent) -> do
        modify' \state -> state
            { appUi = reduceUi uiEvent state.appUi
            , appSlashDismissed = case uiEvent of
                UiSetDraft _ _ -> False
                _ -> state.appSlashDismissed
            , appPasted = case uiEvent of
                UiSetDraft _ _ -> False
                _ -> state.appPasted
            }
        state <- get
        case nativeProgressSignal uiEvent state.appUi of
            Nothing -> pure ()
            Just active ->
                liftIO (state.appRuntime.runtimeNativeProgress active)
        when (eventFollows uiEvent && state.appUi.uiFollow) $
            vScrollToEnd (viewportScroll ConversationViewport)
    AppEvent (AppAskPermission summary reply) ->
        modify' \state ->
            state
                { appUi = reduceUi (UiPermissionShown summary) state.appUi
                , appPermissionReply = Just reply
                }
    AppEvent (AppAskChoice title initial rows reply) ->
        modify' \state ->
            state
                { appChoice = Just ChoiceOverlay
                    { choiceTitle = title
                    , choiceIndex =
                        max 0 (min (max 0 (length rows - 1)) initial)
                    , choiceRows = rows
                    }
                , appChoiceReply = Just reply
                }
    AppEvent (AppSuspend action reply) -> do
        state <- get
        suspendAndResume do
            result <- tryAny action
            atomically (putTMVar reply result)
            pure state
    MouseDown ComposerModel V.BLeft _ _ ->
        handleStatusClick ReplChooseModel
    MouseDown ComposerEffort V.BLeft _ _ ->
        handleStatusClick ReplChooseEffort
    VtyEvent vtyEvent -> do
        state <- get
        case (state.appChoice, state.appUi.uiPermission) of
            (Just _, _) -> handleChoiceKey vtyEvent
            (Nothing, Just _) -> handlePermissionKey vtyEvent
            (Nothing, Nothing) -> handleNormalKey vtyEvent
    _ -> pure ()

eventFollows :: UiEvent -> Bool
eventFollows = \case
    UiLoop _ -> True
    UiUserSubmitted _ -> True
    UiConversationCleared -> True
    _ -> False

nativeProgressSignal :: UiEvent -> UiState -> Maybe Bool
nativeProgressSignal event state = case event of
    UiLoop TurnStarted -> Just True
    UiLoop (TurnFinished _) -> Just False
    UiTurnEnded _ -> Just False
    UiSetAwaitingInput True -> Just False
    UiTick
        | state.uiRunning
        , state.uiFrame == 0 ->
            Just True
    _ -> Nothing

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
                Just permission -> case permission.permissionIndex of
                    0 -> PermissionAllowOnce
                    1 -> PermissionAllowTool
                    _ -> PermissionDeny
                Nothing -> PermissionDeny
        resolvePermission choice
    _ -> pure ()
  where
    movePermission delta =
        modify' \state ->
            state { appUi = reduceUi (UiPermissionMoved delta) state.appUi }

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
                                (Just "Interrupted; press Ctrl-C again to exit."))
                            current.appUi
                    }
        WarnExit ->
            modify' \current ->
                current
                    { appUi =
                        reduceUi
                            (UiSetNotice
                                (Just "Press Ctrl-C again to exit."))
                            current.appUi
                    }
        ForceExit ->
            liftIO (throwIO UserInterrupt)
    pure decision

handleNormalKey :: V.Event -> EventM Name AppState ()
handleNormalKey event = do
    state <- get
    case state.appUi.uiFocus of
        FocusScrollback -> handleScrollbackKey event
        FocusComposer -> handleComposerKey event
        FocusPermission -> pure ()

handleScrollbackKey :: V.Event -> EventM Name AppState ()
handleScrollbackKey = \case
    V.EvKey V.KUp [] -> moveBlock (-1) >> vScrollBy scroll (-2)
    V.EvKey V.KDown [] -> moveBlock 1 >> vScrollBy scroll 2
    V.EvKey V.KPageUp [] -> leaveFollow >> vScrollPage scroll Up
    V.EvKey V.KPageDown [] -> leaveFollow >> vScrollPage scroll Down
    V.EvKey V.KHome [] -> leaveFollow >> vScrollToBeginning scroll
    V.EvKey V.KEnd [] -> resumeFollow
    V.EvKey (V.KChar 'g') [] -> leaveFollow >> vScrollToBeginning scroll
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
    moveBlock delta =
        modify' \state ->
            state { appUi = reduceUi (UiMoveSelection delta) state.appUi }
    toggle =
        modify' \state ->
            state { appUi = reduceUi UiToggleSelected state.appUi }
    focusComposer =
        modify' \state ->
            state { appUi = reduceUi (UiFocusChanged FocusComposer) state.appUi }
    leaveFollow =
        modify' \state ->
            state { appUi = reduceUi (UiSetFollow False) state.appUi }
    resumeFollow = do
        modify' \state ->
            state { appUi = reduceUi (UiSetFollow True) state.appUi }
        vScrollToEnd scroll
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
                                            then "Copied selected block."
                                            else "Terminal clipboard is unavailable.")))
                                current.appUi
                        }

handleComposerKey :: V.Event -> EventM Name AppState ()
handleComposerKey event = do
    state <- get
    let ui = state.appUi
        slashMenu = currentSlashMenu state
    if not ui.uiAwaitingInput
        && isDraftMutation event
        && not (isCtrlDEof event ui.uiDraft)
        then modifyUi
            (UiSetNotice
                (Just "Agent is busy; wait for the prompt or cancel the turn."))
        else case event of
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
            case slashMenu of
                Just _ ->
                    modify' \current ->
                        current { appSlashDismissed = True }
                Nothing -> cancelOrClear
        V.EvKey V.KBackTab []
            | ui.uiAwaitingInput ->
                submitRaw (ReplCycleMode ui.uiDraft)
        V.EvKey V.KUp []
            | Just menu <- slashMenu
            , not (null menu.slashMenuSuggestions) ->
                moveSlash (-1) (length menu.slashMenuSuggestions)
        V.EvKey V.KDown []
            | Just menu <- slashMenu
            , not (null menu.slashMenuSuggestions) ->
                moveSlash 1 (length menu.slashMenuSuggestions)
        V.EvKey (V.KChar '\t') [] ->
            case slashMenu of
                Just menu -> acceptSlash menu
                Nothing -> modifyUi (UiFocusChanged FocusScrollback)
        V.EvKey V.KEnter [V.MShift] ->
            insertText "\n"
        V.EvKey V.KEnter [] ->
            case slashMenu of
                Just menu -> handleSlashEnter menu
                Nothing
                    | ui.uiAwaitingInput -> submitDraft
                    | otherwise -> modifyUi
                        (UiSetNotice
                            (Just "Agent is running; press Esc or Ctrl+C to cancel."))
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
            | V.MCtrl `elem` modifiers ->
                submitRaw (ReplClipboardPaste ui.uiDraft)
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
            vScrollPage (viewportScroll ConversationViewport) Up
        V.EvKey V.KPageDown [] ->
            vScrollPage (viewportScroll ConversationViewport) Down
        V.EvKey (V.KChar character) [] ->
            insertText (Text.singleton character)
        V.EvPaste bytes -> do
            insertText (decodePaste bytes)
            modify' \current -> current { appPasted = True }
        _ -> pure ()
  where
    submitRaw replLine = do
        state <- get
        liftIO $ atomically $
            writeTQueue state.appRuntime.runtimeInput replLine
        modify' \current ->
            current
                { appUi =
                    reduceUi
                        (UiSetAwaitingInput False)
                        current.appUi
                }

    submitDraft = do
        state <- get
        let draft = state.appUi.uiDraft
        if Text.null (Text.strip draft)
            then pure ()
            else do
                liftIO $ atomically $
                    writeTQueue state.appRuntime.runtimeInput $
                        if state.appPasted
                            then ReplPasted draft
                            else ReplText draft
                modify' \current ->
                    current
                        { appUi = if isLocalCommand draft
                            then reduceUi
                                (UiSetDraft "" 0)
                                (reduceUi
                                    (UiSetAwaitingInput False)
                                    current.appUi)
                            else reduceUi
                                (UiUserSubmitted draft)
                                current.appUi
                        , appPasted = False
                        }
                vScrollToEnd (viewportScroll ConversationViewport)

    cancelOrClear = do
        state <- get
        if state.appUi.uiRunning
            then do
                liftIO state.appRuntime.runtimeCancel
                modifyUi (UiSetNotice (Just "Cancelling…"))
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
        let (next, cursor) =
                deleteWordBefore state.appUi.uiDraft state.appUi.uiCursor
        modifyUiResetSlash (UiSetDraft next cursor)

    deleteLineEnd = do
        state <- get
        let (next, cursor) =
                deleteToLineEnd state.appUi.uiDraft state.appUi.uiCursor
        modifyUiResetSlash (UiSetDraft next cursor)

    deleteCurrentLine = do
        state <- get
        let (next, cursor) =
                deleteToLineStart state.appUi.uiDraft state.appUi.uiCursor
        modifyUiResetSlash (UiSetDraft next cursor)

    moveCursor delta = do
        state <- get
        setCursor (state.appUi.uiCursor + delta)

    setCursor cursor =
        modify' \current ->
            current
                { appUi =
                    reduceUi
                        (UiSetDraft current.appUi.uiDraft cursor)
                        current.appUi
                , appSlashIndex = 0
                }

    modifyUi uiEvent =
        modify' \state ->
            state { appUi = reduceUi uiEvent state.appUi }

    modifyUiResetSlash uiEvent =
        modify' \state ->
            state
                { appUi = reduceUi uiEvent state.appUi
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
                    liftIO $ atomically $
                        writeTQueue
                            current.appRuntime.runtimeInput
                            (ReplText next)
                    modify' \state ->
                        state
                            { appUi =
                                reduceUi
                                    (UiSetDraft "" 0)
                                    (reduceUi
                                        (UiSetAwaitingInput False)
                                        state.appUi)
                            , appSlashIndex = 0
                            , appSlashDismissed = False
                            , appPasted = False
                            }

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

decodePaste :: ByteString -> Text
decodePaste =
    Text.filter
        (\character ->
            character == '\n'
                || character == '\t'
                || not (isControl character))
        . Text.decodeUtf8With lenientDecode

isDraftMutation :: V.Event -> Bool
isDraftMutation = \case
    V.EvPaste _ -> True
    V.EvKey V.KBS _ -> True
    V.EvKey V.KDel _ -> True
    V.EvKey V.KEnter modifiers -> V.MShift `elem` modifiers
    V.EvKey (V.KChar character) modifiers
        | V.MCtrl `elem` modifiers ->
            character `elem` ['d', 'k', 'u', 'w']
        | otherwise -> null modifiers
    _ -> False

isCtrlDEof :: V.Event -> Text -> Bool
isCtrlDEof event draft = case event of
    V.EvKey (V.KChar 'd') modifiers ->
        V.MCtrl `elem` modifiers && Text.null draft
    _ -> False

currentSlashMenu :: AppState -> Maybe SlashMenu
currentSlashMenu state
    | state.appSlashDismissed = Nothing
    | otherwise =
        slashMenuFor state.appUi.uiDraft state.appUi.uiCursor

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

isLocalCommand :: Text -> Bool
isLocalCommand draft = case parseReplLine draft of
    ReplPrompt _ -> False
    _ -> True

selectedBlock :: UiState -> BlockId -> Maybe UiBlock
selectedBlock state ident =
    find ((== ident) . (.blockId)) (toList state.uiBlocks)
