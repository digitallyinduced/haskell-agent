-- | Provider-independent state carried while rebuilding a backend.
module Agent.CLI.ProviderTransition
    ( PendingTurn(..)
    , ProviderTransition(..)
    , TransitionCause(..)
    , TurnResult(..)
    , applyProviderTransition
    , resumePendingTurnIfPresent
    , setPendingExitAfter
    , transitionCommitsImmediately
    , withOptimisticPromptTarget
    ) where

import Agent.CLI.Models (ModelTarget(..))
import Agent.CLI.Options (CliOptions(..))
import Agent.Error (ApiError)
import Agent.Loop (TurnInput)
import Agent.Provider (BillingMode, Provider)
import Agent.Tools.PlanMode (PlanModeState)
import Control.Applicative ((<|>))
import Control.Exception.Safe (mask, onException)
import Data.IORef (IORef, atomicModifyIORef')
import Data.Set (Set)
import Data.Text (Text)

data PendingTurn = PendingTurn
    { pendingPromptText :: !Text
    , pendingInputs :: ![TurnInput]
    -- | The exact continuation input is already durable in the transcript;
    -- retry without regenerating plan/startup/provider framing.
    , pendingCheckpointed :: !Bool
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

-- | Atomically claim a pending turn before resuming it. This keeps one
-- explicit recovery action from racing another, such as /retry and account
-- refresh.
resumePendingTurnIfPresent
    :: IORef (Maybe PendingTurn)
    -> (PendingTurn -> IO result)
    -> IO result
    -> IO result
resumePendingTurnIfPresent pendingTurnRef resume noPendingTurn =
    atomicModifyIORef'
        pendingTurnRef
        (\pending -> (Nothing, pending))
        >>= maybe noPendingTurn resume

-- | Manual selections become the project/session target immediately.
-- Automatic fallbacks remain provisional until their replacement backend
-- completes a request successfully, including fallbacks chosen at startup.
transitionCommitsImmediately :: ProviderTransition -> Bool
transitionCommitsImmediately transition =
    transition.transitionCause == ManualTransition

-- | Publish a provisional prompt target before a slow validation action,
-- restoring the prior target if that action rejects, throws, or is cancelled.
withOptimisticPromptTarget
    :: IO ()
    -> IO ()
    -> IO (Either err value)
    -> IO (Either err value)
withOptimisticPromptTarget publish rollback action =
    mask \restoreMask -> do
        result <-
            (publish >> restoreMask action)
                `onException` rollback
        case result of
            Left err -> rollback >> pure (Left err)
            Right value -> pure (Right value)
