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
import Agent.Responses.Types
    ( CustomToolCall(..)
    , CustomToolCallOutput(..)
    , FunctionCall(..)
    , FunctionCallOutput(..)
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
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)

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
            foldl' projectItem withUser (dropWhile isUserMessage items)
        withAssistant =
            if hasAssistantBlock withItems
                then withItems
                else case turn.turnAssistantText of
                    Nothing -> withItems
                    Just text ->
                        reduceUi (UiAssistantHistory text) withItems
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

isUserMessage :: ResponseItem -> Bool
isUserMessage = \case
    MessageItem message -> message.role == RoleUser
    _ -> False

isGeneratedUserText :: Text.Text -> Bool
isGeneratedUserText =
    Text.isPrefixOf "# Skill instructions: " . Text.stripStart

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

renderJsonValue :: Aeson.Value -> Text.Text
renderJsonValue = \case
    Aeson.String text -> text
    value ->
        TextEncoding.decodeUtf8With lenientDecode
            (LazyByteString.toStrict (Aeson.encode value))
