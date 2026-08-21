-- | Renderer-independent state for the retained terminal UI.
module Agent.CLI.UI.Model
    ( BlockId(..)
    , BlockKind(..)
    , BlockState(..)
    , Focus(..)
    , PermissionOverlay(..)
    , PromptState(..)
    , UiBlock(..)
    , UiEvent(..)
    , UiState(..)
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
    ) where

import Agent.CLI.ReplMode (ReplMode(..))
import Agent.CLI.Render (summarizeToolCall)
import Agent.Loop
    ( LoopEvent(..)
    , TokenUsage
    , TurnOutput(..)
    , emptyTokenUsage
    )
import Agent.ToolDispatch (ToolCall(..), ToolCallResult(..))
import Data.Foldable (toList)
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
    , promptMode :: !ReplMode
    , promptUsage :: !TokenUsage
    , promptAttachments :: !Int
    }
    deriving (Eq, Show)

data UiState = UiState
    { uiBlocks :: !(Seq UiBlock)
    , uiNextBlockId :: !Int
    , uiDraft :: !Text
    , uiCursor :: !Int
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
    , uiNotice :: !(Maybe Text)
    , uiFrame :: !Int
    , uiTurnStartBlock :: !Int
    }
    deriving (Eq, Show)

data UiEvent
    = UiLoop !LoopEvent
    | UiUserSubmitted !Text
    | UiSetDraft !Text !Int
    | UiSetPrompt !PromptState
    | UiSetAwaitingInput !Bool
    | UiSetRepository !Text !Text
    | UiSetNotice !(Maybe Text)
    | UiMoveSelection !Int
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
    | UiTick
    deriving (Eq, Show)

initialUiState :: UiState
initialUiState = UiState
    { uiBlocks = Seq.empty
    , uiNextBlockId = 1
    , uiDraft = ""
    , uiCursor = 0
    , uiFocus = FocusComposer
    , uiSelectedBlock = Nothing
    , uiFollow = True
    , uiRunning = False
    , uiAwaitingInput = False
    , uiActivity = "Ready"
    , uiPrompt = PromptState
        { promptModel = ""
        , promptEffort = ""
        , promptMode = ReplModeNormal
        , promptUsage = emptyTokenUsage
        , promptAttachments = 0
        }
    , uiBranch = ""
    , uiCwd = ""
    , uiPermission = Nothing
    , uiNotice = Nothing
    , uiFrame = 0
    , uiTurnStartBlock = 0
    }

reduceUi :: UiEvent -> UiState -> UiState
reduceUi event state = case event of
    UiLoop loopEvent -> reduceLoop loopEvent state
    UiUserSubmitted text ->
        appendBlock BlockUser "You" text "" BlockComplete Nothing
            state
                { uiDraft = ""
                , uiCursor = 0
                , uiAwaitingInput = False
                , uiFollow = True
                , uiNotice = Nothing
                }
    UiSetDraft text cursor ->
        state
            { uiDraft = text
            , uiCursor = max 0 (min (Text.length text) cursor)
            }
    UiSetPrompt prompt ->
        state { uiPrompt = prompt }
    UiSetAwaitingInput awaiting ->
        (if awaiting then finalizeStreams state else state)
            { uiAwaitingInput = awaiting
            , uiRunning = if awaiting then False else state.uiRunning
            , uiActivity = if awaiting then "Ready" else state.uiActivity
            }
    UiSetRepository branch cwd ->
        state { uiBranch = branch, uiCwd = cwd }
    UiSetNotice notice ->
        state { uiNotice = notice }
    UiMoveSelection delta ->
        moveSelection delta state
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
        state { uiFollow = follow }
    UiTurnEnded terminalState ->
        finalizeTurn terminalState state
    UiTick ->
        state { uiFrame = (state.uiFrame + 1) `mod` 10 }

reduceLoop :: LoopEvent -> UiState -> UiState
reduceLoop event state = case event of
    TurnStarted ->
        state
            { uiRunning = True
            , uiAwaitingInput = False
            , uiActivity = "Thinking…"
            , uiNotice = Nothing
            , uiTurnStartBlock = Seq.length state.uiBlocks
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
        let kind = toolBlockKind call.name
        in appendBlock kind (summarizeToolCall call) "" ""
            BlockRunning (Just call.callId)
            state { uiActivity = summarizeToolCall call }
    ToolFinished result ->
        completeTool result state
            { uiActivity = "Thinking…" }
    TurnFinished output ->
        let finalized = finalizeStreams state
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
            { uiRunning = False
            , uiActivity = "Ready"
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
        , uiActivity = "Ready"
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
