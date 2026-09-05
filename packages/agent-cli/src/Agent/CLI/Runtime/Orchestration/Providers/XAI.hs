module Agent.CLI.Runtime.Orchestration.Providers.XAI
    ( withXaiProvider
    ) where

import Agent.CLI.Compaction
    ( autoCompactBackendWith
    , boundCompletedToolContinuations
    , installLiveCompactOutcome
    , runXaiBackendCompactHistoryWithContextWindow
    , runXaiResponsesCompactWithContextWindow
    )
import Agent.Connectivity (withConnectionRecoveryOn)
import Agent.CLI.Runtime.Orchestration.Providers.Common
    ( decorateAutomaticCompact
    , decorateManualCompact
    )
import Agent.CLI.Runtime.Orchestration.Providers.Types
    ( ProviderHost(..), ProviderCompaction(..), ProviderRuntime(..)
    , ProviderAccountSelection(..), ProviderSubagents(..)
    )
import Agent.CLI.Session.History (readLiveTranscript)
import Agent.CLI.Session.Runtime.Types
    ( SessionBackend(..)
    )
import Agent.Provider (TokenProvider, runWithTokenProvider)
import Agent.Responses.Types (ResponseCreateParams(model))
import Agent.XAI.LoopBackend (xaiBackendWithClientOptions)
import Data.IORef (newIORef, readIORef)
import Data.Maybe (fromMaybe)
import qualified Agent.XAI.Client as XAIClient
import qualified Agent.XAI.Options as XAI
import qualified Agent.XAI.Request as XAIRequest

withXaiProvider
    :: TokenProvider
    -> Bool
    -> ProviderHost
    -> (ProviderRuntime -> IO a)
    -> IO a
withXaiProvider tokenProvider hostedTools
        ProviderHost{compaction = ProviderCompaction{..}, networkRecovery} use = do
    xaiOptions0 <- XAI.clientOptionsFromEnv
    let xaiOptions =
            xaiOptions0
                { XAI.hostedXSearchEnabled =
                    hostedTools
                }
    let xaiContextWindow =
            contextWindowForParams
                (XAIRequest.mapModel xaiOptions)
                XAI.grokDefaultContextWindow
        xaiWireModel params =
            maybe xaiOptions.defaultModel
                (XAIRequest.mapModel xaiOptions)
                params.model
        xaiCompactThresholdFor currentParams =
            let contextWindow =
                    xaiContextWindow currentParams
            in max 1 $
                min contextWindow $
                    fromMaybe
                        (XAI.grokAutoCompactTokenLimit
                            (xaiWireModel currentParams)
                            contextWindow)
                        compactThreshold
        xaiOptionsFor currentParams =
            xaiOptions
                { XAI.autoCompactTokenLimit =
                    Just
                        (xaiCompactThresholdFor
                            currentParams)
                }
        xaiCompactThreshold =
            xaiCompactThresholdFor
                <$> readIORef paramsRef
        protectXaiOverflow occupancy getParams backend =
            boundCompletedToolContinuations
                xaiContextWindow
                getParams
                occupancy
                backend
    let btwBackend privateParams =
            xaiBackendWithClientOptions
                xaiOptionsFor
                tokenProvider
                (pure privateParams)
        compactHistory history _inputs = do
            currentParams <- readIORef paramsRef
            runXaiBackendCompactHistoryWithContextWindow
                (xaiContextWindow currentParams)
                btwBackend
                recordCompactionUsage
                currentParams
                history
                Nothing
                >>= decorateAutomaticCompact (readIORef paramsRef) taskPlan
                    xaiContextWindow
        -- Reconnection wraps only the continuation. Keeping automatic
        -- compaction outside it prevents a failed continuation from
        -- rerunning the summary.
        requestBackend =
            withConnectionRecoveryOn
                networkRecovery $
                protectXaiOverflow
                    contextTokensRef
                    (readIORef paramsRef)
                    (xaiBackendWithClientOptions
                        xaiOptionsFor
                        tokenProvider
                        (readIORef paramsRef))
        compactingBackend =
            autoCompactBackendWith
                xaiCompactThreshold
                compactHistory
                installAutomaticCompact
                (readIORef paramsRef)
                contextTokensRef
                requestBackend
        compactRunner focus = do
            contextWindow <-
                currentModelContextWindow
                    (XAIRequest.mapModel xaiOptions)
            historyRef <-
                newIORef =<< readLiveTranscript conversationRef
            installLiveCompactOutcome
                conversationRef
                (Just contextTokensRef)
                (\requestedFocus ->
                    runXaiResponsesCompactWithContextWindow
                        contextWindow
                        (\compactRequest ->
                            runWithTokenProvider tokenProvider
                                \credential ->
                                    XAIClient.createResponseWith
                                        (xaiOptionsFor compactRequest)
                                        credential
                                        compactRequest)
                        recordCompactionUsage
                        paramsRef
                        historyRef
                        requestedFocus
                        >>= decorateManualCompact (readIORef paramsRef) taskPlan
                            xaiContextWindow)
                focus
    use ProviderRuntime
        { sessionBackend = SessionBackend
            { backend = compactingBackend
            , btwBackend
            , interruptBackend = pure ()
            , resetBackendState = pure ()
            }
        , currentContextWindow = Just . xaiContextWindow <$> readIORef paramsRef
        , compactRunner
        , accountSelection = HttpAccountSelection
        , subagents = XaiSubagents xaiContextWindow xaiCompactThresholdFor
            (\childParams ->
                xaiBackendWithClientOptions xaiOptionsFor tokenProvider (pure childParams))
        }
