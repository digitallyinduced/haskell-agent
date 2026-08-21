-- | Renderer-independent state for the retained terminal UI.
module Agent.TUI.Model
    ( BlockId(..)
    , BlockKind(..)
    , BlockState(..)
    , Focus(..)
    , NoticeKind(..)
    , PermissionOverlay(..)
    , PromptState(..)
    , UiBlock(..)
    , UiEvent(..)
    , UiNotice(..)
    , UiState(..)
    , errorNotice
    , infoNotice
    , initialUiState
    , deleteToLineStart
    , deleteToLineEnd
    , deleteWordBefore
    , lineEndCursor
    , lineStartCursor
    , moveWordLeft
    , moveWordRight
    , reduceUi
    , selectedBlockIndex
    , progressNotice
    , successNotice
    , uiNeedsTick
    , warningNotice
    ) where

import Agent.TUI.Presentation
    ( formatSearchReplaceDiff
    , formatToolOutput
    , summarizeToolCall
    )
import Agent.Loop
    ( LoopEvent(..)
    , TokenUsage
    , TurnOutput(..)
    , emptyTokenUsage
    )
import Agent.ToolDispatch (ToolCall(..), ToolCallResult(..))
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Char (isSpace)

newtype BlockId = BlockId Int
    deriving (Eq, Ord, Show)

data BlockKind
    = BlockUser
    | BlockAssistant
    | BlockThinking
    | BlockTool
    | BlockShell
    | BlockEdit
    | BlockSystem
    | BlockError
    deriving (Eq, Show)

data NoticeKind
    = NoticeInfo
    | NoticeSuccess
    | NoticeWarning
    | NoticeProgress
    | NoticeError
    deriving (Eq, Show)

data UiNotice = UiNotice
    { noticeKind :: !NoticeKind
    , noticeText :: !Text
    , noticeTransient :: !Bool
    }
    deriving (Eq, Show)

data BlockState
    = BlockStreaming
    | BlockRunning
    | BlockComplete
    | BlockFailed
    | BlockCancelled
    | BlockDenied
    deriving (Eq, Show)

data UiBlock = UiBlock
    { blockId :: !BlockId
    , blockKind :: !BlockKind
    , blockTitle :: !Text
    , blockBody :: !Text
    , blockDetail :: !Text
    , blockState :: !BlockState
    , blockExpanded :: !Bool
    , blockCallId :: !(Maybe Text)
    }
    deriving (Eq, Show)

data Focus
    = FocusComposer
    | FocusScrollback
    | FocusPermission
    deriving (Eq, Show)

data PermissionOverlay = PermissionOverlay
    { permissionSummary :: !Text
    , permissionIndex :: !Int
    }
    deriving (Eq, Show)

data PromptState = PromptState
    { promptModel :: !Text
    , promptEffort :: !Text
    , promptMode :: !Text
    , promptUsage :: !TokenUsage
    , promptAttachments :: !Int
    }
    deriving (Eq, Show)

data UiState = UiState
    { uiBlocks :: !(Seq UiBlock)
    , uiNextBlockId :: !Int
    , uiDraft :: !Text
    , uiCursor :: !Int
    , uiQueuedInputs :: !(Seq Text)
    , uiFocus :: !Focus
    , uiSelectedBlock :: !(Maybe BlockId)
    , uiFollow :: !Bool
    , uiRunning :: !Bool
    , uiAwaitingInput :: !Bool
    , uiActivity :: !Text
    , uiPrompt :: !PromptState
    , uiBranch :: !Text
    , uiCwd :: !Text
    , uiPermission :: !(Maybe PermissionOverlay)
    , uiNotice :: !(Maybe UiNotice)
    , uiNoticeTicks :: !Int
    , uiFrame :: !Int
    , uiElapsedTenths :: !Int
    , uiCompletionTicks :: !Int
    , uiTurnStartBlock :: !Int
    , uiToolCalls :: !(Map.Map Text ToolCall)
    }
    deriving (Eq, Show)

data UiEvent
    = UiLoop !LoopEvent
    | UiUserSubmitted !Text
    | UiDraftSubmitted
    | UiInputQueued !Text
    | UiInputPromoted !Text
    | UiQueuedInputStarted
    | UiSetDraft !Text !Int
    | UiSetPrompt !PromptState
    | UiSetPromptEffort !Text
    | UiSetAwaitingInput !Bool
    | UiSetRepository !Text !Text
    | UiSetNotice !(Maybe UiNotice)
    | UiMoveSelection !Int
    | UiSelectBlock !BlockId
    | UiToggleSelected
    | UiFocusChanged !Focus
    | UiPermissionShown !Text
    | UiPermissionMoved !Int
    | UiPermissionHidden
    | UiHistory !Text
    | UiAssistantHistory !Text
    | UiSystemMessage !Text
    | UiErrorMessage !Text
    | UiConversationCleared
    | UiSetFollow !Bool
    | UiTurnEnded !BlockState
    | UiTurnRestarted
    | UiTick
    deriving (Eq, Show)

initialUiState :: UiState
initialUiState = UiState
    { uiBlocks = Seq.empty
    , uiNextBlockId = 1
    , uiDraft = ""
    , uiCursor = 0
    , uiQueuedInputs = Seq.empty
    , uiFocus = FocusComposer
    , uiSelectedBlock = Nothing
    , uiFollow = True
    , uiRunning = False
    , uiAwaitingInput = False
    , uiActivity = "Ready"
    , uiPrompt = PromptState
        { promptModel = ""
        , promptEffort = ""
        , promptMode = "ask"
        , promptUsage = emptyTokenUsage
        , promptAttachments = 0
        }
    , uiBranch = ""
    , uiCwd = ""
    , uiPermission = Nothing
    , uiNotice = Nothing
    , uiNoticeTicks = 0
    , uiFrame = 0
    , uiElapsedTenths = 0
    , uiCompletionTicks = 0
    , uiTurnStartBlock = 0
    , uiToolCalls = Map.empty
    }

reduceUi :: UiEvent -> UiState -> UiState
reduceUi event state = case event of
    UiLoop loopEvent -> reduceLoop loopEvent state
    UiUserSubmitted text ->
        appendBlock BlockUser "You" text "" BlockComplete Nothing
            state
                { uiAwaitingInput = False
                , uiFollow = True
                , uiNotice = Nothing
                }
    UiDraftSubmitted ->
        state
            { uiDraft = ""
            , uiCursor = 0
            , uiAwaitingInput = False
            , uiNotice = Nothing
            }
    UiInputQueued text ->
        state
            { uiDraft = ""
            , uiCursor = 0
            , uiQueuedInputs = state.uiQueuedInputs Seq.|> text
            , uiNotice = Nothing
            }
    UiInputPromoted text ->
        state
            { uiDraft = ""
            , uiCursor = 0
            , uiQueuedInputs = text Seq.<| state.uiQueuedInputs
            , uiNotice =
                Just $
                    warningNotice
                        "Cancelling the current turn; sending this prompt next…"
            }
    UiQueuedInputStarted ->
        state
            { uiQueuedInputs = Seq.drop 1 state.uiQueuedInputs
            , uiAwaitingInput = False
            , uiNotice = Nothing
            }
    UiSetDraft text cursor ->
        state
            { uiDraft = text
            , uiCursor = max 0 (min (Text.length text) cursor)
            }
    UiSetPrompt prompt ->
        state { uiPrompt = prompt }
    UiSetPromptEffort effort ->
        state
            { uiPrompt = state.uiPrompt { promptEffort = effort }
            }
    UiSetAwaitingInput awaiting ->
        (if awaiting then finalizeStreams state else state)
            { uiAwaitingInput = awaiting
            , uiRunning = if awaiting then False else state.uiRunning
            , uiActivity =
                if awaiting && state.uiCompletionTicks == 0
                    then "Ready"
                    else state.uiActivity
            }
    UiSetRepository branch cwd ->
        state { uiBranch = branch, uiCwd = cwd }
    UiSetNotice notice ->
        state { uiNotice = notice, uiNoticeTicks = 0 }
    UiMoveSelection delta ->
        moveSelection delta state
    UiSelectBlock ident ->
        selectBlock ident state
    UiToggleSelected ->
        toggleSelected state
    UiFocusChanged focus ->
        state { uiFocus = focus }
    UiPermissionShown summary ->
        state
            { uiPermission = Just PermissionOverlay
                { permissionSummary = summary
                , permissionIndex = 0
                }
            , uiFocus = FocusPermission
            }
    UiPermissionMoved delta ->
        state
            { uiPermission =
                (\permission ->
                    permission
                        { permissionIndex =
                            (permission.permissionIndex + delta) `mod` 3
                        })
                    <$> state.uiPermission
            }
    UiPermissionHidden ->
        state { uiPermission = Nothing, uiFocus = FocusComposer }
    UiHistory history
        | Text.null (Text.strip history) -> state
        | otherwise ->
            appendBlock BlockSystem "Previous conversation" history ""
                BlockComplete Nothing state
    UiAssistantHistory message
        | Text.null (Text.strip message) -> state
        | otherwise ->
            appendBlock BlockAssistant "Assistant" message ""
                BlockComplete Nothing state
    UiSystemMessage message ->
        appendBlock BlockSystem "System" message "" BlockComplete Nothing state
    UiErrorMessage message ->
        appendBlock BlockError "Error" message "" BlockFailed Nothing state
    UiConversationCleared ->
        state
            { uiBlocks = Seq.empty
            , uiSelectedBlock = Nothing
            , uiNextBlockId = 1
            , uiTurnStartBlock = 0
            }
    UiSetFollow follow ->
        state
            { uiFollow = follow
            , uiSelectedBlock =
                if follow
                    then (.blockId) <$> Seq.lookup
                        (Seq.length state.uiBlocks - 1)
                        state.uiBlocks
                    else state.uiSelectedBlock
            }
    UiTurnEnded terminalState ->
        finalizeTurn terminalState state
    UiTurnRestarted ->
        state
            { uiBlocks = Seq.take state.uiTurnStartBlock state.uiBlocks
            , uiRunning = False
            , uiActivity = "Restarting…"
            , uiNotice =
                Just (progressNotice "Restarting current turn…")
            , uiNoticeTicks = 0
            , uiCompletionTicks = 0
            , uiToolCalls = Map.empty
            }
    UiTick ->
        if not (uiNeedsTick state)
            then state
            else
                let completionTicks =
                        max 0 (state.uiCompletionTicks - 1)
                    noticeTicks = state.uiNoticeTicks + 1
                    notice
                        | noticeTicks >= 30
                        , maybe False (.noticeTransient) state.uiNotice =
                            Nothing
                        | otherwise = state.uiNotice
                in state
                    { uiFrame = (state.uiFrame + 1) `mod` 10
                    , uiElapsedTenths =
                        if state.uiRunning
                            then state.uiElapsedTenths + 1
                            else state.uiElapsedTenths
                    , uiActivity =
                        if state.uiCompletionTicks == 1
                            then "Ready"
                            else state.uiActivity
                    , uiCompletionTicks = completionTicks
                    , uiNotice = notice
                    , uiNoticeTicks =
                        if notice == Nothing then 0 else noticeTicks
                    }

uiNeedsTick :: UiState -> Bool
uiNeedsTick state =
    state.uiRunning
        || state.uiCompletionTicks > 0
        || maybe False noticeNeedsTick state.uiNotice
  where
    noticeNeedsTick notice =
        notice.noticeTransient
            || notice.noticeKind == NoticeProgress

reduceLoop :: LoopEvent -> UiState -> UiState
reduceLoop event state = case event of
    TurnStarted ->
        state
            { uiRunning = True
            , uiAwaitingInput = False
            , uiActivity = "Thinking…"
            , uiNotice = Nothing
            , uiElapsedTenths = 0
            , uiCompletionTicks = 0
            , uiTurnStartBlock = Seq.length state.uiBlocks
            , uiToolCalls = Map.empty
            }
    ReasoningDelta delta ->
        appendOrExtend BlockThinking "Thought" delta BlockStreaming state
            { uiActivity = "Thinking…" }
    TextDelta delta ->
        appendOrExtend BlockAssistant "Assistant" delta BlockStreaming state
            { uiActivity = "Writing…" }
    ActivityUpdated activity ->
        state { uiActivity = activity }
    ToolStarted call ->
        let
            kind = toolBlockKind call.name
            body = case call.name of
                "search_replace" ->
                    formatSearchReplaceDiff call.arguments
                _ -> ""
        in appendBlock kind (summarizeToolCall call) body ""
            BlockRunning (Just call.callId)
            state
                { uiRunning = True
                , uiAwaitingInput = False
                , uiActivity = summarizeToolCall call
                , uiToolCalls = Map.insert call.callId call state.uiToolCalls
                }
    ToolOutputUpdated callId output ->
        updateToolOutput callId output state
    ToolFinished result ->
        let displayed = case Map.lookup result.callId state.uiToolCalls of
                Nothing -> result
                Just call ->
                    result
                        { output = formatToolOutput call result.output }
        in completeTool displayed state
            { uiRunning = True
            , uiAwaitingInput = False
            , uiActivity = "Thinking…"
            , uiToolCalls = Map.delete result.callId state.uiToolCalls
            }
    TurnFinished output ->
        let finalized = finalizeStreams state
            continuing = not (null output.toolCalls)
            withFallback = case output.assistantText of
                Just text
                    | not (Text.null (Text.strip text))
                    , not
                        (hasAssistantTextSince
                            finalized.uiTurnStartBlock
                            finalized) ->
                        appendBlock BlockAssistant "Assistant" text ""
                            BlockComplete Nothing finalized
                _ -> finalized
        in withFallback
            { uiRunning = continuing
            , uiActivity =
                if continuing
                    then "Running tools…"
                    else "Finished"
            , uiCompletionTicks =
                if continuing then 0 else 10
            }

appendOrExtend
    :: BlockKind
    -> Text
    -> Text
    -> BlockState
    -> UiState
    -> UiState
appendOrExtend kind title delta streamState state =
    case Seq.viewr state.uiBlocks of
        rest Seq.:> block
            | block.blockKind == kind
            , block.blockState == BlockStreaming ->
                state
                    { uiBlocks =
                        rest Seq.|> block
                            { blockBody = block.blockBody <> delta }
                    }
        _ ->
            appendBlock kind title delta "" streamState Nothing state

appendBlock
    :: BlockKind
    -> Text
    -> Text
    -> Text
    -> BlockState
    -> Maybe Text
    -> UiState
    -> UiState
appendBlock kind title body detail blockState callId state =
    let ident = BlockId state.uiNextBlockId
        block = UiBlock
            { blockId = ident
            , blockKind = kind
            , blockTitle = title
            , blockBody = body
            , blockDetail = detail
            , blockState
            , blockExpanded =
                kind `elem` [BlockUser, BlockAssistant, BlockSystem, BlockError]
            , blockCallId = callId
            }
    in state
        { uiBlocks = state.uiBlocks Seq.|> block
        , uiNextBlockId = state.uiNextBlockId + 1
        , uiSelectedBlock = Just ident
        }

completeTool :: ToolCallResult -> UiState -> UiState
completeTool result state =
    state
        { uiBlocks =
            fmap
                (\block ->
                    if block.blockCallId == Just result.callId
                        then block
                            { blockBody = result.output
                            , blockState = toolResultState result.output
                            }
                        else block)
                state.uiBlocks
        }

updateToolOutput :: Text -> Text -> UiState -> UiState
updateToolOutput callId output state =
    state
        { uiBlocks =
            fmap
                (\block ->
                    if block.blockCallId == Just callId
                        && block.blockState == BlockRunning
                        then block { blockBody = output }
                        else block)
                state.uiBlocks
        }

finalizeTurn :: BlockState -> UiState -> UiState
finalizeTurn terminalState state =
    state
        { uiBlocks =
            Seq.mapWithIndex
                (\index block ->
                    if index >= state.uiTurnStartBlock
                        && block.blockState
                            `elem` [BlockStreaming, BlockRunning]
                        then block { blockState = terminalState }
                        else block)
                state.uiBlocks
        , uiRunning = False
        , uiActivity =
            if terminalState == BlockComplete
                then "Finished"
                else "Ready"
        , uiCompletionTicks =
            if terminalState == BlockComplete then 10 else 0
        }

infoNotice, successNotice, warningNotice, progressNotice, errorNotice
    :: Text -> UiNotice
infoNotice = transientNotice NoticeInfo
successNotice = transientNotice NoticeSuccess
warningNotice = transientNotice NoticeWarning
errorNotice = transientNotice NoticeError
progressNotice text = UiNotice
    { noticeKind = NoticeProgress
    , noticeText = text
    , noticeTransient = False
    }

transientNotice :: NoticeKind -> Text -> UiNotice
transientNotice kind text = UiNotice
    { noticeKind = kind
    , noticeText = text
    , noticeTransient = True
    }

finalizeStreams :: UiState -> UiState
finalizeStreams state =
    state
        { uiBlocks =
            fmap
                (\block ->
                    if block.blockState == BlockStreaming
                        then block { blockState = BlockComplete }
                        else block)
                state.uiBlocks
        }

hasAssistantTextSince :: Int -> UiState -> Bool
hasAssistantTextSince start =
    any
        (\block ->
            block.blockKind == BlockAssistant
                && not (Text.null (Text.strip block.blockBody)))
        . toList
        . Seq.drop start
        . (.uiBlocks)

moveSelection :: Int -> UiState -> UiState
moveSelection delta state =
    case toList state.uiBlocks of
        [] -> state { uiSelectedBlock = Nothing }
        blocks ->
            let current = selectedBlockIndex state
                next = max 0 (min (length blocks - 1) (current + delta))
            in state
                { uiSelectedBlock = Just (blocks !! next).blockId
                , uiFollow = next == length blocks - 1
                }

selectBlock :: BlockId -> UiState -> UiState
selectBlock ident state =
    case Seq.findIndexL ((== ident) . (.blockId)) state.uiBlocks of
        Nothing -> state
        Just index ->
            state
                { uiSelectedBlock = Just ident
                , uiFollow = index == Seq.length state.uiBlocks - 1
                }

selectedBlockIndex :: UiState -> Int
selectedBlockIndex state =
    case state.uiSelectedBlock of
        Nothing -> max 0 (Seq.length state.uiBlocks - 1)
        Just ident ->
            case Seq.findIndexL ((== ident) . (.blockId)) state.uiBlocks of
                Nothing -> max 0 (Seq.length state.uiBlocks - 1)
                Just index -> index

-- | Delete whitespace and the previous non-whitespace word before the cursor.
deleteWordBefore :: Text -> Int -> (Text, Int)
deleteWordBefore text cursor =
    let cursor' = max 0 (min (Text.length text) cursor)
        before = Text.take cursor' text
        after = Text.drop cursor' text
        reversed = Text.reverse before
        withoutSpace = Text.dropWhile isSpace reversed
        remaining = Text.dropWhile (not . isSpace) withoutSpace
        kept = Text.reverse remaining
        after'
            | Text.isSuffixOf " " kept
            , Text.isPrefixOf " " after =
                Text.drop 1 after
            | otherwise = after
    in (kept <> after', Text.length kept)

-- | Delete from the cursor to the beginning of its logical line.
deleteToLineStart :: Text -> Int -> (Text, Int)
deleteToLineStart text cursor =
    let cursor' = max 0 (min (Text.length text) cursor)
        before = Text.take cursor' text
        after = Text.drop cursor' text
        kept = case Text.breakOnEnd "\n" before of
            ("", _) -> ""
            (prefix, _) -> prefix
    in (kept <> after, Text.length kept)

-- | Delete from the cursor to the end of its logical line.
deleteToLineEnd :: Text -> Int -> (Text, Int)
deleteToLineEnd text cursor =
    let cursor' = max 0 (min (Text.length text) cursor)
        before = Text.take cursor' text
        after = Text.drop cursor' text
        keptAfter = case Text.break (== '\n') after of
            (_, rest) -> rest
    in (before <> keptAfter, cursor')

lineStartCursor :: Text -> Int -> Int
lineStartCursor text cursor =
    let cursor' = max 0 (min (Text.length text) cursor)
        before = Text.take cursor' text
    in Text.length (fst (Text.breakOnEnd "\n" before))

lineEndCursor :: Text -> Int -> Int
lineEndCursor text cursor =
    let cursor' = max 0 (min (Text.length text) cursor)
        after = Text.drop cursor' text
        (line, _) = Text.break (== '\n') after
    in cursor' + Text.length line

moveWordLeft :: Text -> Int -> Int
moveWordLeft text cursor =
    let cursor' = max 0 (min (Text.length text) cursor)
        reversed = Text.reverse (Text.take cursor' text)
        withoutSpace = Text.dropWhile isSpace reversed
        remaining = Text.dropWhile (not . isSpace) withoutSpace
    in Text.length remaining

moveWordRight :: Text -> Int -> Int
moveWordRight text cursor =
    let cursor' = max 0 (min (Text.length text) cursor)
        after = Text.drop cursor' text
        withoutWord = Text.dropWhile (not . isSpace) after
        remaining = Text.dropWhile isSpace withoutWord
    in Text.length text - Text.length remaining

toggleSelected :: UiState -> UiState
toggleSelected state =
    case state.uiSelectedBlock of
        Nothing -> state
        Just ident ->
            state
                { uiBlocks =
                    fmap
                        (\block ->
                            if block.blockId == ident
                                then block
                                    { blockExpanded = not block.blockExpanded }
                                else block)
                        state.uiBlocks
                }

toolBlockKind :: Text -> BlockKind
toolBlockKind name
    | name `elem` ["run_terminal_cmd", "shell_command", "run_ghci"] =
        BlockShell
    | name `elem` ["search_replace", "apply_patch"] =
        BlockEdit
    | otherwise = BlockTool

outputLooksFailed :: Text -> Bool
outputLooksFailed output =
    let lowered = Text.toLower (Text.strip output)
    in "error:" `Text.isPrefixOf` lowered
        || "exit: 1" `Text.isPrefixOf` lowered
        || "exit: 2" `Text.isPrefixOf` lowered

toolResultState :: Text -> BlockState
toolResultState output
    | "tool call rejected by user" `Text.isInfixOf` lowered =
        BlockDenied
    | structuredCancellation lowered =
        BlockCancelled
    | outputLooksFailed output = BlockFailed
    | Just code <- exitCodeFrom lowered
    , code /= 0 = BlockFailed
    | otherwise = BlockComplete
  where
    lowered = Text.toLower (Text.strip output)

structuredCancellation :: Text -> Bool
structuredCancellation output =
    let header = Text.strip (headLine output)
    in header == "exit: cancelled"
        || header == "error: command cancelled"
        || header == "cancelled"
        || "cancelled (" `Text.isPrefixOf` header

headLine :: Text -> Text
headLine = Text.takeWhile (/= '\n')

exitCodeFrom :: Text -> Maybe Int
exitCodeFrom text = do
    rest <- Text.stripPrefix "exit:" text
    case reads (Text.unpack (Text.takeWhile (not . isSpace) (Text.strip rest))) of
        [(code, "")] -> Just code
        _ -> Nothing
