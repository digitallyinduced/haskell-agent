module Agent.CLI.Runtime.Orchestration.Providers.Common
    ( HttpProviderSession(..)
    , runHttpProvider
    , decorateAutomaticCompact
    , decorateManualCompact
    , runSession
    , startupFailure
    ) where

import Agent.CLI.Auth.Types (LoadedAuth(..), isGatewayLoadedAuth)
import Agent.CLI.Compaction
    ( CompactOutcome
    , OccupancySnapshot
    , boundCompletedToolContinuations
    , installLiveCompactOutcome
    , decorateCompactOutcomeWithTaskPlanWithin
    , runResponsesCompactWithContextWindow
    )
import Agent.CLI.Error (formatApiErrorAt)
import Agent.CLI.PendingInputs (withPendingInputs)
import Agent.CLI.Provider.Switch (prepareTransitionBackend)
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
import Agent.CLI.Session.History (readLiveTranscript)
import Agent.CLI.Session.Runtime.Types
    ( SessionBackend(..)
    , SessionRequest
    , StartupRuntime(..)
    )
import Agent.CLI.Subagents.Runtime (runHttpSubagent)
import Agent.Connectivity (withConnectionRecoveryOn)
import Agent.Error
    ( ApiError(ProviderError)
    , ErrorType(InvalidRequestError)
    )
import Agent.Loop (Backend)
import Agent.Subagents (setSubagentRunner)
import Agent.Tools.MultiAgents (MultiAgentContext(..))
import Agent.Responses.Types (ResponseCreateParams, Response)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import qualified Agent.CLI.Session.Runner as SessionRunner
import qualified Agent.CLI.Startup.Auth as Startup

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
        (formatApiErrorAt now err)

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

-- Transport and occupancy choices remain owned by each provider. This runner
-- shares the HTTP session lifecycle used by Gemini and OpenRouter.
data HttpProviderSession = HttpProviderSession
    { httpMakeBackend :: IO ResponseCreateParams -> Backend
    , httpSendCompact :: ResponseCreateParams -> IO (Either ApiError Response)
    , httpTransportModel :: Text -> Text
    , httpOccupancy :: IORef (Maybe OccupancySnapshot)
    }

runHttpProvider
    :: AgentProviderRequest
    -> HttpProviderSession
    -> IO RunResult
runHttpProvider request@AgentProviderRequest{..} HttpProviderSession{..} = do
    let contextWindowFor = contextWindowForParams httpTransportModel 1_048_576
        protectOverflow getParams =
            boundCompletedToolContinuations
                contextWindowFor getParams httpOccupancy
    case multiCtx of
        Just ctx ->
            setSubagentRunner ctx.multiRegistry $
                runHttpSubagent
                    subagentRuntime
                    dialect
                    provider
                    ctx.multiSendToRoot
                    (\childParams ->
                        protectOverflow
                            (pure childParams)
                            (httpMakeBackend (pure childParams)))
        Nothing -> pure ()
    let backend =
            withPendingInputs pendingNotices $
                withConnectionRecoveryOn startup.startupNetworkRecovery $
                    protectOverflow
                        (readIORef paramsRef)
                        (httpMakeBackend (readIORef paramsRef))
        compactRunner focus = do
            contextWindow <- currentModelContextWindow httpTransportModel
            historyRef <- newIORef =<< readLiveTranscript conversationRef
            installLiveCompactOutcome conversationRef Nothing
                (\requestedFocus ->
                    runResponsesCompactWithContextWindow
                        contextWindow
                        httpSendCompact
                        recordCompactionUsage
                        paramsRef
                        historyRef
                        requestedFocus
                        >>= decorateManualCompact request contextWindowFor)
                focus
    activeBackend <-
        prepareTransitionBackend
            modelSwitchScope home projectRoot transition persist backend
    runSession
        (sessionRequest
            startupUnavailable
            (Just tokenProvider)
            loaded.loadedOpenAiPool
            (if isGatewayLoadedAuth loaded then Nothing else Just selectHttpAccount)
            (Just . contextWindowFor <$> readIORef paramsRef)
            compactRunner)
        SessionBackend
            { backend = activeBackend
            , btwBackend = httpMakeBackend . pure
            , interruptBackend = pure ()
            , resetBackendState = pure ()
            }
