{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Project durable session turns into the bounded fullscreen history model.
module Agent.CLI.TUI.SessionHistory
    ( sessionHistoryPage
    , sessionHistoryTurn
    ) where

import Agent.CLI.Session
    ( SessionTurn(..)
    , SessionTurnPage(..)
    )
import Agent.CLI.Session.Types (TranscriptEffect(..))
import Agent.CLI.TurnState
    ( isDisplayAttemptBoundary
    , isTurnAbortedNote
    )
import Agent.Json (RawJson, rawJsonBytes)
import qualified Agent.Json.Decode as Hermes
import Agent.CLI.TUI.History
    ( HistoryCursor(..)
    , HistoryDirection
    , HistoryGeneration
    , HistoryPage(..)
    , HistoryTurn(..)
    )
import Agent.Loop
    ( LoopEvent(..)
    )
import Agent.OpenAI.Compaction (isCompactSessionTurn)
import Agent.Responses.LoopBackend (responseItemToToolCall)
import Agent.Responses.Types
    ( ComputerCallOutput(..)
    , CustomToolCall(..)
    , CustomToolCallOutput(..)
    , FunctionCall(..)
    , FunctionCallOutput(..)
    , ItemStatus(..)
    , MessageContent(..)
    , ResponseContentPart(..)
    , ResponseItem(..)
    , ResponseMessage(..)
    , ResponseRole(..)
    )
import Agent.ToolDispatch
    ( ToolCallKind(..)
    , ToolCallResult(..)
    , customToolCall
    , functionToolCall
    )
import Agent.TUI.Model
    ( BlockKind(..)
    , BlockState(..)
    , UiBlock(..)
    , UiEvent(..)
    , UiState(..)
    , initialUiState
    , reduceUi
    )
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

sessionHistoryPage
    :: HistoryGeneration
    -> HistoryDirection
    -> SessionTurnPage
    -> HistoryPage
sessionHistoryPage generation direction page =
    HistoryPage
        { historyPageGeneration = generation
        , historyPageDirection = direction
        , historyPageTurns =
            Seq.fromList
                [ sessionHistoryTurn cursor turn
                | (cursor, turn) <- page.pageTurns
                ]
        , historyPageGenerationStart =
            HistoryCursor page.pageGenerationStart
        , historyPageTotalTurns = page.pageTotalTurns
        , historyPageHasOlder = page.pageHasOlder
        , historyPageHasNewer = page.pageHasNewer
        }

sessionHistoryTurn :: Integral cursor => cursor -> SessionTurn -> HistoryTurn
sessionHistoryTurn cursor turn =
    HistoryTurn
        { historyTurnCursor =
            HistoryCursor (fromIntegral cursor)
        , historyTurnBlocks =
            (projectTurn turn).uiBlocks
        }

projectTurn :: SessionTurn -> UiState
projectTurn turn =
    case turn.turnEffect of
        TranscriptAppend -> addRegularTurn turn.turnItems initialUiState turn
        TranscriptReset -> addResetTurn initialUiState turn
        TranscriptReplace
            | isCompactSessionTurn turn.turnUserText ->
                addResetTurn initialUiState turn
            | otherwise ->
                addRegularTurn
                    (replacementDisplayItems turn)
                    initialUiState
                    turn

addResetTurn :: UiState -> SessionTurn -> UiState
addResetTurn state turn =
    case turn.turnAssistantText of
        Nothing -> state
        Just text -> reduceUi (UiHistory text) state

addRegularTurn :: [ResponseItem] -> UiState -> SessionTurn -> UiState
addRegularTurn items state turn =
    let withUser =
            if Text.null (Text.strip turn.turnUserText)
                then state
                else reduceUi
                    (UiUserSubmitted turn.turnUserText)
                    state
        -- Initial generated context, skill activations, and the prompt are all
        -- encoded as leading user messages. The prompt already has its own
        -- block above; later user messages are mid-turn steering and must stay
        -- visible after the live turn is replaced by durable history.
        withItems =
            completeProjectedStreams
                (foldl' projectItem withUser (dropWhile isUserMessage items))
        -- The failed provider attempt is intentionally separate from
        -- canonical context. Project it as live activity so the terminal turn
        -- state can mark unfinished text/tools failed while keeping completed
        -- tool results complete.
        withDisplayItems =
            foldl' projectDisplayItem withItems turn.turnDisplayItems
        -- A successful turn stores its final assistant text, which its items
        -- already project. An interrupted turn stores the text of the sample
        -- that never committed, which its retained items cannot contain; an
        -- incomplete response repeats its committed text instead, so skip
        -- text that an existing block already shows.
        withAssistant = case turn.turnAssistantText of
            Nothing -> withDisplayItems
            Just text
                | turn.turnError == Nothing
                    && hasAssistantBlock withDisplayItems ->
                    withDisplayItems
                | hasDisplayAssistant turn.turnDisplayItems ->
                    withDisplayItems
                | hasAssistantBlockText text withDisplayItems ->
                    withDisplayItems
                | otherwise ->
                    reduceUi (UiAssistantHistory text) withDisplayItems
        terminalState =
            if turn.turnError == Nothing
                then BlockComplete
                else BlockFailed
        finalized = reduceUi (UiTurnEnded terminalState) withAssistant
    in case turn.turnError of
        Nothing -> finalized
        Just err -> reduceUi (UiErrorMessage err) finalized

-- Automatic compaction persists a complete replacement transcript. The
-- history row should display only the current user turn after that checkpoint,
-- not rematerialise the entire compacted context into one virtualised page.
replacementDisplayItems :: SessionTurn -> [ResponseItem]
replacementDisplayItems turn =
    case lastMatchingUserIndex turn.turnUserText turn.turnItems of
        Nothing -> []
        Just index -> drop (index + 1) turn.turnItems

lastMatchingUserIndex :: Text.Text -> [ResponseItem] -> Maybe Int
lastMatchingUserIndex prompt =
    foldl'
        (\found (index, item) ->
            case item of
                MessageItem message
                    | message.role == RoleUser
                    , Text.strip (messageText message.content)
                        == Text.strip prompt ->
                        Just index
                _ -> found)
        Nothing
        . zip [0 ..]

projectItem :: UiState -> ResponseItem -> UiState
projectItem state = \case
    MessageItem message
        | message.role == RoleAssistant ->
            appendText (UiLoop . TextDelta) (messageText message.content) state
        | message.role == RoleUser
        , not (isGeneratedUserText (messageText message.content)) ->
            appendText UiInputSteered (messageText message.content) state
        | otherwise -> state
    -- Persisted reasoning summaries are model scratchpad, not conversation
    -- history. Replaying them after a tab switch makes old drafting notes look
    -- like newly surfaced user-visible output. Live turns still render streamed
    -- ReasoningDelta events; only durable history projection omits them.
    ReasoningItemValue _ -> state
    FunctionCallItem call ->
        reduceUi
            (UiLoop
                (ToolStarted
                    (functionToolCall
                        call.callId
                        call.name
                        call.arguments)))
            state
    ComputerCallOutputItem output ->
        reduceUi
            (UiLoop
                (ToolFinished
                    (ToolCallResult
                        output.computerOutputCallId
                        "Screenshot captured"
                        ComputerCallKind)))
            state
    ComputerCallItem call ->
        maybe state
            (\toolCall -> reduceUi (UiLoop (ToolStarted toolCall)) state)
            (responseItemToToolCall (ComputerCallItem call))
    CustomToolCallItem call ->
        reduceUi
            (UiLoop
                (ToolStarted
                    (customToolCall
                        call.callId
                        call.name
                        call.input)))
            state
    FunctionCallOutputItem output ->
        reduceUi
            (UiLoop
                (ToolFinished
                    (ToolCallResult
                        output.callId
                        (renderJsonValue output.output)
                        FunctionCallKind)))
            state
    CustomToolCallOutputItem output ->
        reduceUi
            (UiLoop
                (ToolFinished
                    (ToolCallResult
                        output.callId
                        (renderJsonValue output.output)
                        CustomCallKind)))
            state
    _ -> state

projectDisplayItem :: UiState -> ResponseItem -> UiState
projectDisplayItem state item
    | isDisplayAttemptBoundary item =
        reduceUi
            (UiLoop (ResponseRestarted "Retrying response…"))
            state
    | otherwise =
        case item of
            FunctionCallOutputItem output
                | output.status == Just ItemIncomplete ->
                    reduceUi
                        (UiLoop
                            (ToolOutputUpdated
                                output.callId
                                (renderJsonValue output.output)))
                        state
            CustomToolCallOutputItem output
                | output.status == Just ItemIncomplete ->
                    reduceUi
                        (UiLoop
                            (ToolOutputUpdated
                                output.callId
                                (renderJsonValue output.output)))
                        state
            _ -> projectItem state item

isUserMessage :: ResponseItem -> Bool
isUserMessage = \case
    MessageItem message -> message.role == RoleUser
    _ -> False

isGeneratedUserText :: Text.Text -> Bool
isGeneratedUserText text =
    Text.isPrefixOf "# Skill instructions: " (Text.stripStart text)
        || isTurnAbortedNote text

appendText :: (Text.Text -> UiEvent) -> Text.Text -> UiState -> UiState
appendText event text state
    | Text.null (Text.strip text) = state
    | otherwise = reduceUi (event text) state

hasAssistantBlock :: UiState -> Bool
hasAssistantBlock =
    any
        (\block ->
            block.blockKind == BlockAssistant
                && not (Text.null (Text.strip block.blockBody)))
        . toList
        . (.uiBlocks)

hasDisplayAssistant :: [ResponseItem] -> Bool
hasDisplayAssistant =
    any \case
        MessageItem message -> message.role == RoleAssistant
        _ -> False

-- | Items are committed history: an assistant message projected through text
-- deltas is complete whatever state the turn itself ended in, and must not be
-- marked failed next to the uncommitted text of an interrupted turn.
completeProjectedStreams :: UiState -> UiState
completeProjectedStreams state =
    state
        { uiBlocks =
            fmap
                (\block ->
                    if block.blockState == BlockStreaming
                        then block { blockState = BlockComplete }
                        else block)
                state.uiBlocks
        }

hasAssistantBlockText :: Text.Text -> UiState -> Bool
hasAssistantBlockText text =
    any
        (\block ->
            block.blockKind == BlockAssistant
                && Text.strip block.blockBody == Text.strip text)
        . toList
        . (.uiBlocks)

messageText :: MessageContent -> Text.Text
messageText = \case
    MessageContentText text -> text
    MessageContentParts parts ->
        Text.intercalate "\n" (concatMap responseContentText parts)

responseContentText :: ResponseContentPart -> [Text.Text]
responseContentText = \case
    OutputTextPart{text} -> [text]
    ReasoningTextPart{text} -> [text]
    SummaryTextPart{text} -> [text]
    RefusalPart{refusal} -> [refusal]
    _ -> []

renderJsonValue :: RawJson -> Text.Text
renderJsonValue value =
    case Hermes.decodeEither
            (Hermes.nullable Hermes.text)
            (rawJsonBytes value) of
        Right (Just text) -> text
        Right Nothing -> ""
        Left _ -> TextEncoding.decodeUtf8 (rawJsonBytes value)
