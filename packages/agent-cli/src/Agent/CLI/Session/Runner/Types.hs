-- | Small state records used while constructing a CLI session runner.
module Agent.CLI.Session.Runner.Types
    ( AgentStepCache(..)
    , SessionRunnerContinuation(..)
    ) where

import Agent.CLI.AgentViewport.Runtime (AgentStepCache(..))
import Agent.CLI.ProviderTransition (TurnResult)
import Agent.CLI.Recap (RecapKind)
import Agent.CLI.Runtime.Types (PendingTurnPresentation, RunResult)
import Agent.CLI.SessionEnv (SessionEnv)
import Agent.CLI.Session.Runtime.Types (StartupRuntime)
import Agent.CLI.ProviderTransition (PendingTurn)
import Agent.Loop (TurnInput)
import Data.Text (Text)

data SessionRunnerContinuation = SessionRunnerContinuation
    { runnerFinishStartup :: StartupRuntime -> IO ()
    , runnerRepl :: SessionEnv -> IO RunResult
    , runnerReplWithDraft :: SessionEnv -> Text -> IO RunResult
    , runnerRunPendingTurn
        :: PendingTurnPresentation -> SessionEnv -> PendingTurn -> IO RunResult
    , runnerFinishTurn :: SessionEnv -> Bool -> TurnResult -> IO RunResult
    , runnerPreparePromptSkillInputs
        :: SessionEnv
        -> Bool
        -> Text
        -> [TurnInput]
        -> IO (Either Text [TurnInput])
    , runnerRunSessionRecap :: Bool -> SessionEnv -> RecapKind -> IO ()
    , runnerRunSessionTurnSummary :: SessionEnv -> IO ()
    }
