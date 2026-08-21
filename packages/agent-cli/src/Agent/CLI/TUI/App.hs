-- | Retained fullscreen terminal application and its session bridge.
module Agent.CLI.TUI.App
    ( FullscreenRuntime
    , emitUiEvent
    , newFullscreenRuntime
    , readFullscreenLine
    , requestFullscreenPermission
    , requestFullscreenChoice
    , requestFullscreenChoiceWithBody
    , requestFullscreenText
    , runFullscreen
    , withFullscreenSuspended
    ) where

import Agent.CLI.Clipboard
    ( nonEmptyClipboardImages
    , readClipboardImages
    )
import Agent.CLI.Input
    ( ReplLine(..)
    , appendReplHistory
    , readReplHistory
    , terminalTextWidth
    )
import Agent.CLI.AgentViewport
    ( AgentEntry(..)
    , AgentTarget(..)
    , agentEntryTreeLabel
    )
import Agent.CLI.Interrupt (CtrlCDecision(..))
import Agent.CLI.Command
    ( ReplAction(..)
    , SkillCommand
    , SlashMenu(..)
    , SlashSuggestion(..)
    , parseReplLineWithSkills
    , slashMenuForWithSkills
    )
import Agent.CLI.Permission (PermissionChoice(..))
import Agent.CLI.ReplMode (replModeLabel)
import Agent.CLI.Render (formatElapsed, summarizeToolCall)
import Agent.CLI.Status (formatTokenUsage)
import qualified Agent.CLI.TUI.Theme as Theme
import qualified Agent.CLI.TUI.Bridge as Bridge
import Agent.CLI.TUI.Markdown (markdownWidget)
import Agent.CLI.UI.Model
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
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (modify')
import Control.Exception.Safe (SomeException, finally, throwIO, tryAny)
import Control.Exception (AsyncException(UserInterrupt))
import Data.ByteString (ByteString)
import Data.Char (isControl)
import Data.Foldable (toList)
import Data.List (find, intersperse, sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as Vty

data Name
    = ConversationViewport
    | OverlayViewport
    | ConversationBlock !BlockId
    | ComposerArea
    | ComposerCursor
    | ComposerModel
    | ComposerEffort
    | ComposerMode
    | ChoiceRow !Int
    | PermissionRow !Int
    | SlashRow !Int
    | OverlayCursor
    | AgentRow !AgentTarget
    deriving (Eq, Ord, Show)

data AppEvent
    = AppUi !UiEvent
    | AppAskPermission !Text !(TMVar (Maybe PermissionChoice))
    | AppAskChoice
        !Text
        !Text
        !Int
        ![(Text, Text)]
        !(TMVar (Maybe Int))
    | AppAskText
        !Text
        !Text
        !Text
        !(TMVar (Maybe Text))
    | forall a. AppSuspend !(IO a) !(TMVar (Either SomeException a))
    | AppSetSkillCommands ![SkillCommand]
    | AppAgentSnapshot !AgentTarget ![AgentEntry]
    | AppStop

data FullscreenRuntime = FullscreenRuntime
    { runtimeEvents :: !(BChan AppEvent)
    , runtimeInput :: !(TQueue ReplLine)
    , runtimeCancel :: !(IO ())
    , runtimeCtrlC :: !(IO CtrlCDecision)
    , runtimeCopy :: !(Text -> IO Bool)
    , runtimeNativeProgress :: !(Bool -> IO ())
    , runtimeAgentSnapshot :: !(IO (AgentTarget, [AgentEntry]))
    , runtimeAgentSelect :: !(AgentTarget -> IO ())
    , runtimeColor :: !Bool
    , runtimeInitial :: !UiState
    }

data AppState = AppState
    { appUi :: !UiState
    , appPermissionReply :: !(Maybe (TMVar (Maybe PermissionChoice)))
    , appRuntime :: !FullscreenRuntime
    , appSlashIndex :: !Int
    , appChoice :: !(Maybe ChoiceOverlay)
    , appChoiceReply :: !(Maybe (TMVar (Maybe Int)))
    , appTextPrompt :: !(Maybe TextOverlay)
    , appTextReply :: !(Maybe (TMVar (Maybe Text)))
    , appSlashDismissed :: !Bool
    , appPasted :: !Bool
    , appHistory :: ![Text]
    , appHistoryIndex :: !(Maybe Int)
    , appHistoryDraft :: !Text
    , appKillBuffer :: !Text
    , appSkillCommands :: ![SkillCommand]
    , appAgentSelected :: !AgentTarget
    , appAgentEntries :: ![AgentEntry]
    }

data ChoiceOverlay = ChoiceOverlay
    { choiceTitle :: !Text
    , choiceBody :: !Text
    , choiceIndex :: !Int
    , choiceRows :: ![(Text, Text)]
    }

data TextOverlay = TextOverlay
    { textTitle :: !Text
    , textBody :: !Text
    , textDraft :: !Text
    , textCursor :: !Int
    }

newFullscreenRuntime
    :: IO ()
    -> IO CtrlCDecision
    -> (Text -> IO Bool)
    -> (Bool -> IO ())
    -> IO (AgentTarget, [AgentEntry])
    -> (AgentTarget -> IO ())
    -> Bool
    -> UiState
    -> IO FullscreenRuntime
newFullscreenRuntime
    cancelAction
    ctrlCAction
    copyAction
    nativeProgress
    agentSnapshot
    agentSelect
    color
    initial = FullscreenRuntime
    <$> newBChan 512
    <*> newTQueueIO
    <*> pure cancelAction
    <*> pure ctrlCAction
    <*> pure copyAction
    <*> pure nativeProgress
    <*> pure agentSnapshot
    <*> pure agentSelect
    <*> pure color
    <*> pure initial

emitUiEvent :: FullscreenRuntime -> UiEvent -> IO ()
emitUiEvent runtime = writeBChan runtime.runtimeEvents . AppUi

readFullscreenLine
    :: FullscreenRuntime
    -> [SkillCommand]
    -> PromptState
    -> Text
    -> IO ReplLine
readFullscreenLine runtime skills prompt initial = do
    writeBChan runtime.runtimeEvents (AppSetSkillCommands skills)
    emitUiEvent runtime (UiSetPrompt prompt)
    -- Keep anything the user started typing while the previous turn was
    -- running. Non-empty explicit drafts (for example after cycling mode or
    -- pasting an attachment) still take precedence.
    when (not (Text.null initial)) $
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
    writeBChan runtime.runtimeEvents
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
    writeBChan runtime.runtimeEvents
        (AppAskText title body initial reply)
    atomically (readTMVar reply)

withFullscreenSuspended :: FullscreenRuntime -> IO a -> IO a
withFullscreenSuspended runtime action = do
    reply <- newEmptyTMVarIO
    writeBChan runtime.runtimeEvents (AppSuspend action reply)
    atomically (readTMVar reply) >>= either throwIO pure

runFullscreen :: FullscreenRuntime -> IO a -> IO a
runFullscreen runtime workerAction = do
    history <- readReplHistory
    (initialAgent, initialAgents) <- runtime.runtimeAgentSnapshot
    let vtyConfig =
            V.defaultConfig
                { V.configPreferredColorMode = Just V.FullColor
                }
        buildVty = do
            vty <- Vty.mkVty vtyConfig
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
            , appAgentSelected = initialAgent
            , appAgentEntries = initialAgents
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
    ticker = loop (0 :: Int)
    loop tick = do
        threadDelay 100000
        writeBChan runtime.runtimeEvents (AppUi UiTick)
        when (tick `mod` 5 == 0) do
            tryAny runtime.runtimeAgentSnapshot >>= \case
                Left _ -> pure ()
                Right (selected, entries) ->
                    writeBChan runtime.runtimeEvents
                        (AppAgentSnapshot selected entries)
        loop ((tick + 1) `mod` 10)

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

handlePromptControlClick
    :: (Text -> ReplLine)
    -> EventM Name AppState ()
handlePromptControlClick choice = do
    state <- get
    let ui = state.appUi
        overlayOpen =
            maybe False (const True) state.appTextPrompt
                || maybe False (const True) state.appChoice
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
                                (Just "Prompt settings can be changed when input is ready."))
                            current.appUi
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
        insert (decodePaste bytes)
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
    , appStartEvent = vScrollToEnd (viewportScroll ConversationViewport)
    , appAttrMap = \state ->
        if state.appRuntime.runtimeColor
            then Theme.solarizedDark
            else Theme.monochrome
    }

drawApp :: AppState -> [Widget Name]
drawApp state =
    case (state.appTextPrompt, state.appChoice, state.appUi.uiPermission) of
        (Just prompt, _, _) ->
            [ drawTextPrompt prompt
            , drawMain state
            ]
        (Nothing, Just choice, _) ->
            [ drawChoice choice
            , drawMain state
            ]
        (Nothing, Nothing, Just permission) ->
            [ drawPermission permission
            , drawMain state
            ]
        (Nothing, Nothing, Nothing) -> [drawMain state]

drawMain :: AppState -> Widget Name
drawMain state =
    withAttr Theme.baseAttr $
        vBox
            [ drawHeader state.appUi
            , drawWorkspace state
            , drawNotice state.appUi
            , drawSlashMenu state
            , drawComposer state.appUi
            , drawFooter state
            ]

drawWorkspace :: AppState -> Widget Name
drawWorkspace state =
    padTop (Pad 1) $
        hBox $
            [ withVScrollBars OnRight $
                viewport ConversationViewport Vertical $
                    padLeftRight 2 (drawTranscript state.appUi)
            ]
                <> if length state.appAgentEntries <= 1
                    then []
                    else
                        [ hLimit 42 $
                            padLeft (Pad 1) $
                                drawAgentPane
                                    state.appAgentSelected
                                    state.appAgentEntries
                        ]

drawAgentPane :: AgentTarget -> [AgentEntry] -> Widget Name
drawAgentPane selected entries =
    withAttr Theme.borderAttr $
        withBorderStyle unicodeRounded $
            borderWithLabel (txt " Agents ") $
                padAll 1 $
                    vBox
                        [ vBox (map drawEntry (zip [0 ..] ordered))
                        , padTop (Pad 1) $
                            withAttr Theme.footerAttr $
                                txtWrap
                                    ("viewing "
                                        <> maybe "/root" (.agentPath)
                                            selectedEntry
                                        <> " · /agents to switch")
                        , padTop (Pad 1) $
                            withAttr Theme.assistantAttr $
                                vBox (map txtWrap transcript)
                        ]
  where
    ordered = sortOn (.agentPath) entries
    selectedEntry =
        find ((== selected) . (.agentTarget)) ordered
    drawEntry (index, entry) =
        let
            marker =
                if entry.agentTarget == selected then "› " else "  "
            row =
                txt
                    (marker
                        <> agentEntryTreeLabel ordered index entry)
            styled =
                if entry.agentTarget == selected
                    then withAttr Theme.successAttr row
                    else row
        in clickable (AgentRow entry.agentTarget) styled
    transcript = case selectedEntry of
        Nothing -> ["(agent unavailable)"]
        Just entry ->
            let rows =
                    filter (not . Text.null . Text.strip)
                        entry.agentTranscript
            in if null rows
                then ["(no transcript)"]
                else drop (max 0 (length rows - 12)) rows

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
            <> if state.uiRunning
                then " · "
                    <> formatElapsed
                        (fromIntegral state.uiElapsedTenths / 10)
                else ""
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
    in clickable (ConversationBlock block.blockId) $
        padBottom (Pad 1) framed

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
        styled =
            if selected == index
                then withAttr Theme.selectedAttr row
                else row
    in clickable (SlashRow index) styled

drawComposer :: UiState -> Widget Name
drawComposer state =
    let focused = state.uiFocus == FocusComposer
        attr = if focused then Theme.borderActiveAttr else Theme.borderAttr
        mode = replModeLabel state.uiPrompt.promptMode
        usage = formatTokenUsage state.uiPrompt.promptUsage
        leading =
            filter (not . Text.null)
                [ if state.uiPrompt.promptAttachments > 0
                    then "image "
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
                <> [clickable ComposerMode (txt mode) | not (Text.null mode)]
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
    in clickable ComposerArea $
        withAttr attr $
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
                            "Enter queue  │  Shift+Enter newline  │  Esc/Ctrl+C cancel  │  PgUp/PgDn or wheel scroll  │  Tab scrollback"
                        | otherwise ->
                            "Enter send  │  Shift+Enter newline  │  PgUp/PgDn or wheel scroll  │  Tab scrollback"

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
        styled =
            if selected == index
                then withAttr Theme.selectedAttr widget
                else widget
    in clickable (PermissionRow index) styled

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
                                vBox
                                    [ if Text.null (Text.strip choice.choiceBody)
                                        then emptyWidget
                                        else padBottom (Pad 1) $
                                            vLimitPercent 65 $
                                                viewport OverlayViewport Vertical $
                                                    markdownWidget choice.choiceBody
                                    , vBox $
                                        zipWith
                                            (choiceRow choice.choiceIndex)
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
                withAttr Theme.borderActiveAttr $
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
                                    , withAttr Theme.borderAttr $
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
            draftCursorLocation prompt.textDraft prompt.textCursor
    in showCursor OverlayCursor (Location (column, row)) content

choiceRow :: Int -> Int -> (Text, Text) -> Widget Name
choiceRow selected index (label, detail) =
    let prefix = if selected == index then "› " else "  "
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
    in clickable (ChoiceRow index) styled

handleEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleEvent event = case event of
    AppEvent AppStop ->
        halt
    AppEvent (AppSetSkillCommands skills) ->
        modify' \state -> state
            { appSkillCommands = skills
            , appSlashIndex = 0
            , appSlashDismissed = False
            }
    AppEvent (AppAgentSnapshot selected entries) ->
        modify' \state ->
            state
                { appAgentSelected =
                    Bridge.normalizeAgentSelection selected entries
                , appAgentEntries = entries
                }
    AppEvent (AppUi uiEvent) -> do
        modify' \state -> state
            { appUi = reduceUi uiEvent state.appUi
            , appSlashDismissed = case uiEvent of
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
        state <- get
        case Bridge.nativeProgressSignal uiEvent state.appUi of
            Nothing -> pure ()
            Just active ->
                liftIO (state.appRuntime.runtimeNativeProgress active)
        when (Bridge.eventFollows uiEvent && state.appUi.uiFollow) $
            vScrollToEnd (viewportScroll ConversationViewport)
    AppEvent (AppAskPermission summary reply) ->
        modify' \state ->
            state
                { appUi = reduceUi (UiPermissionShown summary) state.appUi
                , appPermissionReply = Just reply
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
                , appChoiceReply = Just reply
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
                }
        vScrollToBeginning (viewportScroll OverlayViewport)
    AppEvent (AppSuspend action reply) -> do
        state <- get
        suspendAndResume do
            result <- tryAny action
            atomically (putTMVar reply result)
            pure state
    MouseDown name button _ _ -> do
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
                        handlePromptControlClick ReplChooseModel
                    (ComposerEffort, V.BLeft) ->
                        handlePromptControlClick ReplChooseEffort
                    (ComposerMode, V.BLeft) ->
                        handlePromptControlClick ReplCycleMode
                    (SlashRow index, V.BLeft) ->
                        activateSlashAt index
                    (SlashRow _, V.BScrollUp) ->
                        handleComposerKey (V.EvKey V.KUp [])
                    (SlashRow _, V.BScrollDown) ->
                        handleComposerKey (V.EvKey V.KDown [])
                    (AgentRow target, V.BLeft) -> do
                        liftIO
                            (state.appRuntime.runtimeAgentSelect target)
                        modify' \current ->
                            current { appAgentSelected = target }
                    _ -> handleMouseDown name button
            (Nothing, Just _, _) ->
                case (name, button) of
                    (ChoiceRow index, V.BLeft) ->
                        confirmChoiceAt index
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
    VtyEvent vtyEvent -> do
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
    case event of
        V.EvMouseDown _ _ V.BScrollUp _ ->
            scrollConversationBy (-mouseScrollLines)
        V.EvMouseDown _ _ V.BScrollDown _ ->
            scrollConversationBy mouseScrollLines
        _ -> do
            state <- get
            case state.appUi.uiFocus of
                FocusScrollback -> handleScrollbackKey event
                FocusComposer -> handleComposerKey event
                FocusPermission -> pure ()

activateSlashAt :: Int -> EventM Name AppState ()
activateSlashAt index = do
    state <- get
    case currentSlashMenu state of
        Just menu
            | index >= 0
            , index < length menu.slashMenuSuggestions -> do
                modify' \current ->
                    current { appSlashIndex = index }
                handleComposerKey (V.EvKey V.KEnter [])
        _ -> pure ()

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
    moveBlock delta = do
        modify' \state ->
            state { appUi = reduceUi (UiMoveSelection delta) state.appUi }
        state <- get
        case state.appUi.uiSelectedBlock of
            Just ident -> makeVisible (ConversationBlock ident)
            Nothing -> pure ()
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
    case event of
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
                Nothing -> modifyUi (UiFocusChanged FocusScrollback)
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
                || V.MMeta `elem` modifiers ->
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
            images <- liftIO $
                nonEmptyClipboardImages <$> readClipboardImages
            case images of
                Just attached ->
                    submitRaw
                        (ReplClipboardPaste ui.uiDraft (Just attached))
                Nothing -> do
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
            queued = not state.appUi.uiAwaitingInput
        if Text.null (Text.strip draft)
            then pure ()
            else do
                liftIO (appendReplHistory draft)
                liftIO $ atomically $
                    writeTQueue state.appRuntime.runtimeInput $
                        if state.appPasted
                            then ReplPasted draft
                            else ReplText draft
                modify' \current ->
                    current
                        { appUi =
                            let submitted =
                                    if isLocalCommand
                                        state.appSkillCommands
                                        draft
                                        then reduceUi
                                            (UiSetDraft "" 0)
                                            (reduceUi
                                                (UiSetAwaitingInput False)
                                                current.appUi)
                                        else reduceUi
                                            (UiUserSubmitted draft)
                                            current.appUi
                            in if queued
                                then reduceUi
                                    (UiSetNotice
                                        (Just "Queued for the next turn."))
                                    submitted
                                else submitted
                        , appPasted = False
                        , appHistory = draft : current.appHistory
                        , appHistoryIndex = Nothing
                        , appHistoryDraft = ""
                        }
                vScrollToEnd (viewportScroll ConversationViewport)

    cancelOrClear = do
        state <- get
        if not state.appUi.uiAwaitingInput
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
                , appHistoryIndex = Nothing
                , appHistoryDraft =
                    case uiEvent of
                        UiSetDraft text _ -> text
                        _ -> state.appHistoryDraft
                }

    modifyUiWithKill killed uiEvent =
        modify' \state ->
            state
                { appUi = reduceUi uiEvent state.appUi
                , appSlashIndex = 0
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
        modify' \currentState ->
            currentState
                { appUi =
                    reduceUi
                        (UiSetDraft text (Text.length text))
                        currentState.appUi
                , appHistoryIndex = index
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
    | otherwise =
        lookupViewport ConversationViewport >>= \case
            Just (VP _ top (_, height) (_, contentHeight))
                | top + height + amount >= contentHeight -> do
                    setConversationFollow True
                    vScrollToEnd scroll
            _ -> do
                setConversationFollow False
                vScrollBy scroll amount
  where
    scroll = viewportScroll ConversationViewport

setConversationFollow :: Bool -> EventM Name AppState ()
setConversationFollow follow =
    modify' \state ->
        state { appUi = reduceUi (UiSetFollow follow) state.appUi }

decodePaste :: ByteString -> Text
decodePaste =
    Text.filter
        (\character ->
            character == '\n'
                || character == '\t'
                || not (isControl character))
        . Text.decodeUtf8With lenientDecode

currentSlashMenu :: AppState -> Maybe SlashMenu
currentSlashMenu state
    | state.appSlashDismissed = Nothing
    | otherwise =
        slashMenuForWithSkills
            state.appSkillCommands
            state.appUi.uiDraft
            state.appUi.uiCursor

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

isLocalCommand :: [SkillCommand] -> Text -> Bool
isLocalCommand skills draft = case parseReplLineWithSkills skills draft of
    ReplPrompt _ -> False
    _ -> True

selectedBlock :: UiState -> BlockId -> Maybe UiBlock
selectedBlock state ident =
    find ((== ident) . (.blockId)) (toList state.uiBlocks)
