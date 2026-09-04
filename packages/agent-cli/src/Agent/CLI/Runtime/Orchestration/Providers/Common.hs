module Agent.CLI.Runtime.Orchestration.Providers.Common
    ( decorateAutomaticCompact
    , decorateManualCompact
    , runSession
    , startupFailure
    ) where

import Agent.CLI.Compaction
    ( CompactOutcome
    , decorateCompactOutcomeWithTaskPlanWithin
    )
import Agent.CLI.Error (formatApiErrorAt)
import Agent.CLI.Runtime.Orchestration.Providers.Types
    ( AgentProviderRequest(..)
    )
import Agent.CLI.Runtime.Orchestration.Startup (finishStartup)
import Agent.CLI.Runtime.Recap
    ( runSessionRecap
    , runSessionTurnSummary
    )
import Agent.CLI.Runtime.Repl
    ( finishTurn
    , preparePromptSkillInputsWithPaste
    , repl
    , replWithDraft
    , runPendingTurn
    )
import Agent.CLI.Runtime.Types (RunResult)
import Agent.CLI.Session.Runtime.Types
    ( SessionBackend
    , SessionRequest
    )
import Agent.Error
    ( ApiError(ProviderError)
    , ErrorType(InvalidRequestError)
    )
import Agent.Responses.Types (ResponseCreateParams)
import Data.IORef (readIORef)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import qualified Agent.CLI.Session.Runner as SessionRunner
import qualified Agent.CLI.Startup.Auth as Startup
import qualified Data.Text as Text

decorateManualCompact
    :: AgentProviderRequest
    -> (ResponseCreateParams -> Int)
    -> Either Text CompactOutcome
    -> IO (Either Text CompactOutcome)
decorateManualCompact
    request
    contextWindowForResult = \case
        Left err -> pure (Left err)
        Right outcome -> do
            currentParams <- readIORef request.paramsRef
            decorateCompactOutcomeWithTaskPlanWithin
                (contextWindowForResult currentParams)
                currentParams
                request.taskPlan
                outcome

decorateAutomaticCompact
    :: AgentProviderRequest
    -> (ResponseCreateParams -> Int)
    -> Either ApiError CompactOutcome
    -> IO (Either ApiError CompactOutcome)
decorateAutomaticCompact
    request
    contextWindowForResult = \case
        Left err -> pure (Left err)
        Right outcome ->
            decorateManualCompact
                request
                contextWindowForResult
                (Right outcome) >>= pure . \case
                    Left message ->
                        Left
                            (ProviderError
                                InvalidRequestError
                                message
                                Nothing)
                    Right decorated -> Right decorated

startupFailure
    :: AgentProviderRequest
    -> ApiError
    -> IO RunResult
startupFailure AgentProviderRequest{startup} err = do
    now <- getCurrentTime
    Startup.startupDie startup
        (Text.unpack (formatApiErrorAt now err))

sessionRunnerContinuation :: SessionRunner.SessionRunnerContinuation
sessionRunnerContinuation =
    SessionRunner.SessionRunnerContinuation
        { runnerRepl = repl
        , runnerReplWithDraft = replWithDraft
        , runnerRunPendingTurn = runPendingTurn
        , runnerFinishTurn = finishTurn
        , runnerFinishStartup = finishStartup
        , runnerPreparePromptSkillInputs = preparePromptSkillInputsWithPaste
        , runnerRunSessionRecap = runSessionRecap
        , runnerRunSessionTurnSummary = runSessionTurnSummary
        }

runSession
    :: SessionRequest
    -> SessionBackend
    -> IO RunResult
runSession = SessionRunner.runSession sessionRunnerContinuation
