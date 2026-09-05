module Agent.CLI.Runtime.Orchestration.Providers.Common
    ( HttpProviderTransport(..)
    , withHttpProvider
    , decorateAutomaticCompact
    , decorateManualCompact
    ) where

import Agent.CLI.Compaction
    ( CompactOutcome
    , OccupancySnapshot
    , boundCompletedToolContinuations
    , installLiveCompactOutcome
    , runResponsesCompactWithContextWindow
    , decorateCompactOutcomeWithTaskPlanWithin
    )
import Agent.CLI.Runtime.Orchestration.Providers.Types
    ( ProviderHost(..), ProviderCompaction(..), ProviderRuntime(..)
    , ProviderAccountSelection(..), ProviderSubagents(..)
    )
import Agent.CLI.Session.History (readLiveTranscript)
import Agent.CLI.Session.Runtime.Types (SessionBackend(..))
import Agent.Connectivity (withConnectionRecoveryOn)
import Agent.Loop (Backend)
import Data.IORef (IORef, newIORef, readIORef)
import Agent.Error
    ( ApiError(ProviderError)
    , ErrorType(InvalidRequestError)
    )
import Agent.Responses.Types (ResponseCreateParams, Response)
import Agent.Tools.TaskPlan (TaskPlanEnv)
import Data.Text (Text)

decorateManualCompact
    :: IO ResponseCreateParams
    -> Maybe TaskPlanEnv
    -> (ResponseCreateParams -> Int)
    -> Either Text CompactOutcome
    -> IO (Either Text CompactOutcome)
decorateManualCompact
    getParams
    taskPlan
    contextWindowForResult = \case
        Left err -> pure (Left err)
        Right outcome -> do
            currentParams <- getParams
            decorateCompactOutcomeWithTaskPlanWithin
                (contextWindowForResult currentParams)
                currentParams
                taskPlan
                outcome

decorateAutomaticCompact
    :: IO ResponseCreateParams
    -> Maybe TaskPlanEnv
    -> (ResponseCreateParams -> Int)
    -> Either ApiError CompactOutcome
    -> IO (Either ApiError CompactOutcome)
decorateAutomaticCompact
    getParams
    taskPlan
    contextWindowForResult = \case
        Left err -> pure (Left err)
        Right outcome ->
            decorateManualCompact
                getParams
                taskPlan
                contextWindowForResult
                (Right outcome) >>= pure . \case
                    Left message ->
                        Left
                            (ProviderError
                                InvalidRequestError
                                message
                                Nothing)
                    Right decorated -> Right decorated

-- | Gemini and OpenRouter share HTTP backend and manual-compaction assembly.
-- The transport chooses its sender, model mapping, and occupancy state; the
-- consumer owns session startup, persistence, notices, and subagent registration.
data HttpProviderTransport = HttpProviderTransport
    { httpMakeBackend :: IO ResponseCreateParams -> Backend
    , httpSendCompact :: ResponseCreateParams -> IO (Either ApiError Response)
    , httpTransportModel :: Text -> Text
    , httpOccupancy :: IORef (Maybe OccupancySnapshot)
    }

withHttpProvider
    :: ProviderHost
    -> HttpProviderTransport
    -> (ProviderRuntime -> IO a)
    -> IO a
withHttpProvider
        ProviderHost{compaction = ProviderCompaction{..}, networkRecovery}
        HttpProviderTransport{..} use = do
    let contextWindowFor = contextWindowForParams httpTransportModel 1_048_576
        protectOverflow getParams =
            boundCompletedToolContinuations
                contextWindowFor getParams httpOccupancy
        backend =
            withConnectionRecoveryOn networkRecovery $
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
                        >>= decorateManualCompact
                            (readIORef paramsRef) taskPlan contextWindowFor)
                focus
    use ProviderRuntime
        { sessionBackend = SessionBackend
            { backend
            , btwBackend = httpMakeBackend . pure
            , interruptBackend = pure ()
            , resetBackendState = pure ()
            }
        , currentContextWindow = Just . contextWindowFor <$> readIORef paramsRef
        , compactRunner
        , accountSelection = HttpAccountSelection
        , subagents = HttpSubagents \childParams ->
            protectOverflow
                (pure childParams)
                (httpMakeBackend (pure childParams))
        }
