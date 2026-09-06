-- | Completed response output and the provider-neutral live event protocol.
module Agent.Loop.Output
    ( TurnOutput(..)
    , TurnCompletion(..)
    , emptyTurnOutput
    , LoopEvent(..)
    , NativeAgentStatus(..)
    , visibleResponseActivity
    ) where

import Agent.Loop.TokenUsage (TokenUsage, emptyTokenUsage)
import Agent.Telemetry (TurnTelemetry)
import Agent.ToolDispatch (ToolCall, ToolCallResult)
import Data.Text (Text)

data TurnOutput = TurnOutput
    { responseId :: !Text
    , toolCalls :: ![ToolCall]
    , assistantText :: !(Maybe Text)
    , tokenUsage :: !TokenUsage
    , providerTelemetry :: !(Maybe TurnTelemetry)
    , completion :: !TurnCompletion
    } deriving (Eq, Show)

data TurnCompletion
    = TurnCompleted
    | TurnIncomplete
        { incompleteReason :: !Text
        , incompleteReasoningTokens :: !(Maybe Int)
        }
    deriving (Eq, Show)

emptyTurnOutput :: Text -> [ToolCall] -> Maybe Text -> TurnOutput
emptyTurnOutput responseId toolCalls assistantText = TurnOutput
    { responseId
    , toolCalls
    , assistantText
    , tokenUsage = emptyTokenUsage
    , providerTelemetry = Nothing
    , completion = TurnCompleted
    }

data LoopEvent
    = TextDelta Text
    | ReasoningDelta Text
    -- | Ephemeral transport/tool activity for the live CLI status line.
    | ActivityUpdated Text
    -- | Latest provider-reported limit status for retained prompt chrome.
    | ProviderLimitUpdated
        { providerLimitText :: !Text
        , providerLimitWarning :: !Bool
        }
    -- | A persistent user-visible warning that must not replace live activity.
    | WarningRaised Text
    -- | A streamed response was interrupted and its provider submission is
    -- being retried. Renderers must close the partial stream before displaying
    -- output from the new attempt.
    | ResponseRestarted Text
    | TurnStarted
    | TurnFinished TurnOutput
    | ToolStarted ToolCall
    -- | Replace the metadata for an already-visible in-flight tool call.
    -- Providers may learn canonical arguments after an early live start.
    | ToolUpdated ToolCall
    -- | Replace the live UI preview for an in-flight tool call while its
    -- arguments are still streaming. Append-only renderers may ignore this;
    -- retained renderers can repaint the existing tool block.
    | ToolArgumentsUpdated ToolCall
    -- | Latest accumulated output snapshot for an in-flight tool call.
    | ToolOutputUpdated Text Text
    | ToolFinished ToolCallResult
    -- | Remove a provider-retracted tool call from the current attempt.
    | ToolRetracted Text
    -- | Discard all UI activity emitted by the current response attempt.
    -- This is distinct from ending the whole turn: a retry may follow.
    | ResponseAttemptDiscarded
    -- | The composed backend (including recovery/fallback wrappers) has
    -- definitively failed after emitting visible output. Renderers must keep
    -- that output and close any streaming/running blocks as failed.
    | ResponseAttemptFailed
    -- | Lifecycle/activity from a provider-managed child agent. These agents
    -- are display-only unless the provider exposes targeted controls.
    | NativeAgentStarted Text (Maybe Text) Text (Maybe Text)
    | NativeAgentOutput Text Text
    | NativeAgentFinished Text NativeAgentStatus
    deriving (Eq, Show)

data NativeAgentStatus
    = NativeAgentRunning
    | NativeAgentCompleted
    | NativeAgentFailed
    | NativeAgentCancelled
    deriving (Eq, Show)

visibleResponseActivity :: LoopEvent -> Bool
visibleResponseActivity = \case
    TextDelta _ -> True
    ReasoningDelta _ -> True
    ToolStarted _ -> True
    ToolUpdated _ -> True
    ToolArgumentsUpdated _ -> True
    ToolOutputUpdated _ _ -> True
    ToolFinished _ -> True
    NativeAgentStarted{} -> True
    NativeAgentOutput{} -> True
    NativeAgentFinished{} -> True
    _ -> False
