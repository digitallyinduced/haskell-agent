-- | Small state records used while constructing a CLI session runner.
module Agent.CLI.Session.Runner.Types
    ( AgentStepCache(..)
    , SessionRunnerContinuation(..)
    ) where

import Agent.CLI.AgentViewport (AgentStep)
import Agent.CLI.ProviderTransition (TurnResult)
import Agent.CLI.Recap (RecapKind)
import Agent.CLI.Runtime.Types (PendingTurnPresentation, RunResult)
import Agent.CLI.SessionEnv (SessionEnv)
import Agent.CLI.Session.Runtime.Types (StartupRuntime)
import Agent.CLI.ProviderTransition (PendingTurn)
import Agent.Loop (TurnInput)
import Agent.Responses.Types (ResponseItem)
import Agent.Subagents (SubagentStatus)
import Data.Text (Text)
import System.Mem.StableName (StableName)

data AgentStepCache = AgentStepCache
    { cachedTranscript :: !(StableName [ResponseItem])
    , cachedVariant :: !(Maybe SubagentStatus)
    , cachedSteps :: ![AgentStep]
    }

data SessionRunnerContinuation = SessionRunnerContinuation
    { runnerFinishStartup :: StartupRuntime -> IO ()
    , runnerRepl :: SessionEnv -> IO RunResult
    , runnerReplWithDraft :: SessionEnv -> Text -> IO RunResult
    , runnerRunPendingTurn
        :: PendingTurnPresentation -> SessionEnv -> PendingTurn -> IO RunResult
    , runnerFinishTurn :: SessionEnv -> Bool -> TurnResult -> IO RunResult
    , runnerPreparePromptSkillInputs
        :: SessionEnv -> Text -> [TurnInput] -> IO (Either Text [TurnInput])
    , runnerRunSessionRecap :: Bool -> SessionEnv -> RecapKind -> IO ()
    , runnerRunSessionTurnSummary :: SessionEnv -> IO ()
    }
