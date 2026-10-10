-- | Isolated one-shot side questions over a snapshot of the main transcript.
module Agent.CLI.Btw
    ( BtwBackendFactory
    , BtwError(..)
    , SideCallSnapshot
    , formatBtwError
    , runBtwWithCancel
    , sideCallSnapshotParams
    , sideCallSnapshot
    , sideCallSnapshotTranscript
    , sideQuestionPrompt
    ) where

import Agent.Cancel (CancelFlag, newCancelFlag, waitCancel)
import Agent.Runtime.Error (formatApiErrorInline)
import Agent.Error (ApiError)
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , TurnInput(..)
    , TurnOutput(..)
    , initialBackendSnapshot
    )
import Agent.Responses.Types
    ( ResponseCreateParams(..)
    , ResponseItem
    , ToolChoice(..)
    , ToolChoiceMode(..)
    )
import Agent.Subagents (trimDanglingToolSuffix)
import Control.Concurrent.Async (race)
import Data.Text (Text)
import qualified Data.Text as Text

-- | Construct a provider backend over private request parameters and transcript.
type BtwBackendFactory =
    ResponseCreateParams -> Backend

-- | Immutable provider parameters and transcript used by a one-shot side call.
--
-- Constructing the snapshot also removes request fields that belong to the
-- parent turn and trims any incomplete live tool-call suffix.
data SideCallSnapshot = SideCallSnapshot
    { sideCallParams :: !ResponseCreateParams
    , sideCallTranscript :: ![ResponseItem]
    }

sideCallSnapshot
    :: ResponseCreateParams
    -> [ResponseItem]
    -> SideCallSnapshot
sideCallSnapshot params transcript =
    SideCallSnapshot
        { sideCallParams = clearTurnSpecificParams params
        , sideCallTranscript = trimDanglingToolSuffix transcript
        }

sideCallSnapshotParams :: SideCallSnapshot -> ResponseCreateParams
sideCallSnapshotParams SideCallSnapshot{sideCallParams = params} = params

sideCallSnapshotTranscript :: SideCallSnapshot -> [ResponseItem]
sideCallSnapshotTranscript
        SideCallSnapshot{sideCallTranscript = transcript} =
    transcript

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

-- | Run one provider request against private state. The caller supplies the
-- Ctrl-C/Esc scope so the fresh cancellation flag is independent of the main
-- turn's flag.
runBtwWithCancel
    :: (CancelFlag
        -> IO (Either BtwError Text)
        -> IO (Either BtwError Text))
    -> BtwBackendFactory
    -> SideCallSnapshot
    -> Text
    -> IO (Either BtwError Text)
runBtwWithCancel
        withCancelScope
        makeBackend
        SideCallSnapshot
            { sideCallParams = params
            , sideCallTranscript = transcript
            }
        question = do
    cancel <- newCancelFlag
    let Backend submit = makeBackend params
        request =
            submit (initialBackendSnapshot transcript) Nothing
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
