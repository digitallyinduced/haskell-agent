-- | Provider-independent state carried while rebuilding a backend.
module Agent.CLI.ProviderTransition
    ( PendingTurn(..)
    , ProviderTransition(..)
    , TransitionCause(..)
    , TurnResult(..)
    , applyProviderTransition
    , providerTransitionDraft
    , setPendingExitAfter
    ) where

import Agent.CLI.Models (ModelOption(..))
import Agent.CLI.Options (CliOptions(..))
import Agent.Error (ApiError)
import Agent.Loop (TurnInput)
import Agent.Provider (BillingMode, Provider)
import Agent.Tools.PlanMode (PlanModeState)
import Control.Applicative ((<|>))
import Data.Text (Text)

data PendingTurn = PendingTurn
    { pendingPromptText :: !Text
    , pendingInputs :: ![TurnInput]
    , pendingExitAfter :: !Bool
    , pendingPlanState :: !PlanModeState
    }

data TransitionCause
    = ManualTransition
    | AutomaticFallback
    deriving (Eq, Show)

data ProviderTransition = ProviderTransition
    { transitionTarget :: !ModelOption
    , transitionSessionId :: !(Maybe Text)
    , transitionPendingTurn :: !(Maybe PendingTurn)
    , transitionDraft :: !Text
    , transitionUnavailableProviders :: ![Provider]
    , transitionCause :: !TransitionCause
    -- | Billing class of the session that initiated an automatic fallback.
    -- Preserved across failed replacement providers so the whole chain obeys
    -- the original billing boundary. Manual transitions use 'Nothing'.
    , transitionAutomaticBilling :: !(Maybe BillingMode)
    }

data TurnResult
    = TurnSucceeded
    | TurnFailed
    | TurnRestartRequested !Text !PendingTurn
    | TurnProviderUnavailable !ApiError !PendingTurn

-- | Rebuild provider-specific state without changing how the invocation was
-- launched. In particular, one-shot prompt flags remain set so approval policy
-- and autonomous prompting do not silently become interactive after fallback.
applyProviderTransition :: CliOptions -> ProviderTransition -> CliOptions
applyProviderTransition options transition =
    options
        { optProvider = Just transition.transitionTarget.modelProvider
        , optModel = Just transition.transitionTarget.modelId
        , optCwd = Nothing
        , optWorktree = False
        , optEffort = Nothing
        , optResume = transition.transitionSessionId <|> options.optResume
        }

-- | An idle composer draft survives a manual provider rebuild. Automatic
-- fallback resumes a pending turn instead, so its transition draft is empty.
providerTransitionDraft :: Maybe ProviderTransition -> Text
providerTransitionDraft = maybe "" (.transitionDraft)

setPendingExitAfter :: Bool -> PendingTurn -> PendingTurn
setPendingExitAfter exitAfter pending =
    pending { pendingExitAfter = exitAfter }
