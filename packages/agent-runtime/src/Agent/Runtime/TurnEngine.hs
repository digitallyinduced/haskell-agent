-- | Frontend-independent finalization of a provider-loop execution.
--
-- This is the lifecycle kernel of the session engine, not a new scheduler.
-- The host still owns resources and commits the returned patch at its existing
-- persistence boundary. All frontends share the choice of outcome, compaction
-- rebase, interrupted-tool recovery, and model/display projections.
module Agent.Runtime.TurnEngine
    ( TurnPolicy(..)
    , TurnDisposition(..)
    , FinalizedTurn
        ( finalizedDisposition
        , finalizedPrepared
        , finalizedPatch
        , finalizedModelItems
        , finalizedDisplayItems
        )
    , ModelItems
    , DisplayItems
    , modelItems
    , displayItems
    , finalizeTurn
    ) where

import Agent.Error (ApiError)
import Agent.Loop
    ( LoopError(..)
    , LoopExecution(..)
    , LoopProgress(..)
    , LoopResult(..)
    , TurnCompletion(TurnIncomplete)
    , TurnOutput(..)
    )
import Agent.Loop qualified as Loop
import Agent.Responses.Types (ResponseItem)
import Agent.Runtime.Compaction (AutomaticCompactionBoundary)
import Agent.Runtime.TurnState
    ( ConversationOutcome(..)
    , ConversationPatch
    , PreparedTurn(..)
    , TurnAbort(..)
    , finishConversation
    , interruptedTurnItems
    , rebasePreparedTurn
    , turnNewItems
    , uncommittedDisplayItems
    )
import Data.Text (Text)

-- | Host policy, independent of presentation. Unavailability classification
-- does not authorize fallback: routing/gateway policy remains authoritative.
data TurnPolicy = TurnPolicy
    { providerUnavailable :: ApiError -> Bool
    , normalizeAssistant :: Text -> Text
    }

data TurnDisposition
    = TurnRestarted !Text
    | TurnCancelled !LoopError
    | TurnProviderUnavailable !ApiError
    | TurnFailed !LoopError
    | TurnCompleted !LoopResult

-- | Canonical items retained by this turn. Constructors are intentionally
-- hidden so display-only output cannot be substituted for a model projection.
newtype ModelItems = ModelItems [ResponseItem]
    deriving (Eq, Show)

-- | Uncommitted activity for history rendering only.
newtype DisplayItems = DisplayItems [ResponseItem]
    deriving (Eq, Show)

-- | Explicit conversions at legacy storage/renderer boundaries. There is no
-- conversion from 'DisplayItems' to 'ModelItems'.
modelItems :: ModelItems -> [ResponseItem]
modelItems (ModelItems items) = items

displayItems :: DisplayItems -> [ResponseItem]
displayItems (DisplayItems items) = items

data FinalizedTurn = FinalizedTurn
    { finalizedDisposition :: TurnDisposition
    , finalizedPrepared :: PreparedTurn
    , finalizedPatch :: ConversationPatch
    , finalizedModelItems :: ModelItems
    -- Keep projections lazy: successful turns need not normalize a failure
    -- journal, and presentation must not be forced on the model-state path.
    , finalizedDisplayItems :: DisplayItems
    }

finalizeTurn
    :: TurnPolicy
    -> Maybe Text
    -> Maybe AutomaticCompactionBoundary
    -> PreparedTurn
    -> LoopExecution
    -> FinalizedTurn
finalizeTurn policy restart boundary original execution =
    case restart of
        Just effort ->
            finish (TurnRestarted effort) ConversationRestarted [] []
        Nothing -> case execution.executionResult of
            Left err@(LoopCancelled _) ->
                let retained = interruptedTurnItems prepared execution TurnAbortedByUser
                in finish (TurnCancelled err) (ConversationCancelled retained)
                    retained failedDisplay
            Left (LoopTransport apiError)
                | execution.executionProgress == NoResponseCommitted
                , policy.providerUnavailable apiError ->
                    finish (TurnProviderUnavailable apiError)
                        ConversationProviderUnavailable [] []
            Left err ->
                let retained = interruptedTurnItems prepared execution
                        (TurnAbortedByFailure (loopErrorAbortReason err))
                in finish (TurnFailed err) (ConversationFailed retained)
                    retained failedDisplay
            Right result ->
                finish (TurnCompleted result)
                    (ConversationCompleted result.finalResponseId result.tokenUsage
                        (policy.normalizeAssistant <$> result.finalText))
                    (turnNewItems prepared.preparedBeforeItems execution.executionState)
                    []
  where
    prepared = rebasePreparedTurn boundary original
    failedDisplay = uncommittedDisplayItems execution
    finish disposition outcome retained display =
        FinalizedTurn
            { finalizedDisposition = disposition
            , finalizedPrepared = prepared
            , finalizedPatch = finishConversation prepared outcome
            , finalizedModelItems = ModelItems retained
            , finalizedDisplayItems = DisplayItems display
            }

-- | Stable, non-secret reasons persisted on synthetic tool outputs.
loopErrorAbortReason :: LoopError -> Text
loopErrorAbortReason = \case
    LoopIncomplete turn -> case turn.completion of
        TurnIncomplete reason _ ->
            "the response was cut off (" <> reason <> ")"
        Loop.TurnCompleted -> "the response was cut off"
    LoopMaxTurns _ -> "the turn reached its maximum number of model steps"
    LoopTransport _ -> "the provider request failed"
    LoopTransportAfterOutput _ -> "the provider connection was interrupted"
    LoopNoResponseId -> "the provider returned no response id"
    LoopUnexpected _ -> "the agent hit an unexpected error"
    LoopCancelled _ -> "the turn was cancelled"
