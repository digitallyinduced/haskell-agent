-- | Provider-independent state carried while rebuilding a backend.
module Agent.CLI.ProviderTransition
    ( PendingTurn(..)
    , ProviderTransition(..)
    , TransitionCause(..)
    , TurnResult(..)
    , applyProviderTransition
    , setPendingExitAfter
    , transitionCommitsImmediately
    ) where

import Agent.CLI.Models (ModelTarget(..))
import Agent.CLI.Options (CliOptions(..))
import Agent.Error (ApiError)
import Agent.Loop (TurnInput)
import Agent.Provider (BillingMode, Provider)
import Agent.Tools.PlanMode (PlanModeState)
import Agent.Tools.PlanMode.Tracker (ApprovedPlanContinuation)
import Control.Applicative ((<|>))
import Data.Set (Set)
import Data.Text (Text)

data PendingTurn = PendingTurn
    { pendingPromptText :: !Text
    , pendingInputs :: ![TurnInput]
    -- | The exact continuation input is already durable in the transcript;
    -- retry without regenerating plan/startup/provider framing.
    , pendingCheckpointed :: !Bool
    -- | Exact approved-plan continuation represented by this attempt.  A
    -- checkpoint retry does not regenerate turn context, so it must carry
    -- this identity explicitly in order to consume it after a durable retry.
    , pendingPlanContinuation :: !(Maybe ApprovedPlanContinuation)
    , pendingExitAfter :: !Bool
    , pendingPlanState :: !PlanModeState
    }

data TransitionCause
    = ManualTransition
    | AutomaticFallback
    deriving (Eq, Show)

data ProviderTransition = ProviderTransition
    { transitionTarget :: !ModelTarget
    -- | Stable credential-source key to select after rebuilding the provider.
    , transitionAccountSelectionId :: !(Maybe Text)
    -- | Provider account id used by transports whose live pool selects by id.
    , transitionAccountId :: !(Maybe Text)
    , transitionSessionId :: !(Maybe Text)
    , transitionPendingTurn :: !(Maybe PendingTurn)
    , transitionUnavailableProviders :: !(Set Provider)
    , transitionCause :: !TransitionCause
    -- | Billing class of the session that initiated an automatic fallback.
    -- Preserved across failed replacement providers so the whole chain obeys
    -- the original billing boundary. Manual transitions use 'Nothing'.
    , transitionAutomaticBilling :: !(Maybe BillingMode)
    }

data TurnResult
    = TurnSucceeded
    | TurnCancelled
    | TurnFailed !PendingTurn
    | TurnRestartRequested !Text !PendingTurn
    | TurnProviderUnavailable !ApiError !PendingTurn

-- | Rebuild provider-specific state without changing how the invocation was
-- launched. In particular, one-shot prompt flags remain set so approval policy
-- and autonomous prompting do not silently become interactive after fallback.
applyProviderTransition :: CliOptions -> ProviderTransition -> CliOptions
applyProviderTransition options transition =
    options
        { optProvider = Just transition.transitionTarget.targetProvider
        , optModel = Just transition.transitionTarget.targetModelId
        , optCwd = Nothing
        , optWorktree = False
        , optEffort = Nothing
        , optResume = transition.transitionSessionId <|> options.optResume
        }

setPendingExitAfter :: Bool -> PendingTurn -> PendingTurn
setPendingExitAfter exitAfter pending =
    pending { pendingExitAfter = exitAfter }

-- | Manual selections become the project/session target immediately.
-- Automatic fallbacks remain provisional until their replacement backend
-- completes a request successfully, including fallbacks chosen at startup.
transitionCommitsImmediately :: ProviderTransition -> Bool
transitionCommitsImmediately transition =
    transition.transitionCause == ManualTransition
