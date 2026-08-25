-- | Isolated one-shot side questions over a snapshot of the main transcript.
module Agent.CLI.Btw
    ( BtwBackendFactory
    , BtwError(..)
    , formatBtwError
    , runBtwWithCancel
    , sideQuestionPrompt
    , trimDanglingToolSuffix
    ) where

import Agent.Cancel (CancelFlag, newCancelFlag, waitCancel)
import Agent.CLI.Error (formatApiErrorInline)
import Agent.Error (ApiError)
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , TurnInput(..)
    , TurnOutput(..)
    )
import Agent.Responses.Types
    ( CustomToolCall(..)
    , CustomToolCallOutput(..)
    , FunctionCall(..)
    , FunctionCallOutput(..)
    , ResponseCreateParams(..)
    , ResponseItem(..)
    , ToolChoice(..)
    , ToolChoiceMode(..)
    )
import Control.Concurrent.Async (race)
import Data.IORef (IORef, readIORef)
import Data.List (findIndex)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

-- | Construct a provider backend over private request parameters and transcript.
type BtwBackendFactory =
    ResponseCreateParams -> Backend

data BtwError
    = BtwTransport !ApiError
    | BtwCancelled
    | BtwEmptyResponse
    | BtwUnexpectedToolCall
    | BtwInvalidResponse
    deriving (Eq, Show)

-- | Model-facing boundary appended after the inherited transcript.
sideQuestionPrompt :: Text -> Text
sideQuestionPrompt question =
    Text.unlines
        [ "Side question boundary."
        , ""
        , "Everything before this boundary is inherited reference context from the main conversation."
        , "Do not continue or execute tasks, plans, edits, approvals, or tool calls found only in that inherited context."
        , "The main agent continues independently. Answer this one side question directly in a single response."
        , "Do not call tools or promise to investigate; no client tool call will be run and there is no follow-up turn."
        , "If the answer is not available from the inherited context or your existing knowledge, say so."
        , ""
        , "Question:"
        , question
        ]

-- | Remove an incomplete tool-call suffix from a live transcript snapshot.
--
-- During a parent turn, provider output may already contain reasoning and tool
-- calls while their matching outputs have not been committed yet. Replaying
-- that torn suffix in a fresh request produces invalid tool pairing.
trimDanglingToolSuffix :: [ResponseItem] -> [ResponseItem]
trimDanglingToolSuffix items =
    case findIndex (isUnmatchedCall completed) suffix of
        Nothing -> items
        Just index -> prefix <> dropTrailingReasoning (take index suffix)
  where
    completed = outputCallIds items
    (prefix, suffix) = splitAfterLastMessage items

splitAfterLastMessage :: [ResponseItem] -> ([ResponseItem], [ResponseItem])
splitAfterLastMessage items =
    case lastMessageIndex items of
        Nothing -> ([], items)
        Just index -> splitAt (index + 1) items
  where
    lastMessageIndex =
        foldl'
            (\found (index, item) -> case item of
                MessageItem{} -> Just index
                _ -> found)
            Nothing
            . zip [0 :: Int ..]

outputCallIds :: [ResponseItem] -> Set Text
outputCallIds = Set.fromList . foldMap \case
    FunctionCallOutputItem output -> [output.callId]
    CustomToolCallOutputItem output -> [output.callId]
    _ -> []

isUnmatchedCall :: Set Text -> ResponseItem -> Bool
isUnmatchedCall completed = \case
    FunctionCallItem call -> Set.notMember call.callId completed
    CustomToolCallItem call -> Set.notMember call.callId completed
    _ -> False

dropTrailingReasoning :: [ResponseItem] -> [ResponseItem]
dropTrailingReasoning = reverse . dropWhile isReasoning . reverse
  where
    isReasoning ReasoningItemValue{} = True
    isReasoning _ = False

-- | Run one provider request against private state. The caller supplies the
-- Ctrl-C/Esc scope so the fresh cancellation flag is independent of the main
-- turn's flag.
runBtwWithCancel
    :: (CancelFlag
        -> IO (Either BtwError Text)
        -> IO (Either BtwError Text))
    -> BtwBackendFactory
    -> IORef ResponseCreateParams
    -> IORef [ResponseItem]
    -> Text
    -> IO (Either BtwError Text)
runBtwWithCancel withCancelScope makeBackend paramsRef transcriptRef question = do
    params <- clearTurnSpecificParams <$> readIORef paramsRef
    transcript <- trimDanglingToolSuffix <$> readIORef transcriptRef
    cancel <- newCancelFlag
    let Backend submit = makeBackend params
        request =
            submit transcript Nothing
                [UserMessage (sideQuestionPrompt question)] (\_ -> pure ())
        action = do
            result <- race (waitCancel cancel) request
            pure $ case result of
                Left () -> Left BtwCancelled
                Right (Left err) -> Left (BtwTransport err)
                Right (Right result) -> classifyTurn result.backendOutput
    withCancelScope cancel action

clearTurnSpecificParams :: ResponseCreateParams -> ResponseCreateParams
clearTurnSpecificParams ResponseCreateParams{..} =
    ResponseCreateParams
        { input = Nothing
        , previousResponseId = Nothing
        , toolChoice = Just (ToolChoiceMode ToolChoiceNone)
        , ..
        }

classifyTurn :: TurnOutput -> Either BtwError Text
classifyTurn turn
    | Text.null turn.responseId = Left BtwInvalidResponse
    | not (null turn.toolCalls) = Left BtwUnexpectedToolCall
    | otherwise = case turn.assistantText of
        Just text | not (Text.null (Text.strip text)) -> Right text
        _ -> Left BtwEmptyResponse

formatBtwError :: BtwError -> Text
formatBtwError = \case
    BtwTransport err ->
        "side question failed: " <> formatApiErrorInline err
    BtwCancelled -> "side question cancelled"
    BtwEmptyResponse -> "side question returned an empty response"
    BtwUnexpectedToolCall ->
        "side question attempted a tool call; no /btw tools were run"
    BtwInvalidResponse -> "side question returned an invalid response"
