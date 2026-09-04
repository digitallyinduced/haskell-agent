module Agent.CLI.Runtime.Orchestration.Providers.XAI
    ( runXaiProvider
    ) where

import Agent.CLI.Auth.Types
    ( LoadedAuth(..)
    , isGatewayLoadedAuth
    )
import Agent.CLI.Compaction
    ( autoCompactBackendWith
    , boundCompletedToolContinuations
    , installLiveCompactOutcome
    , runXaiBackendCompactHistoryWithContextWindow
    , runXaiResponsesCompactWithContextWindow
    )
import Agent.Connectivity (withConnectionRecoveryOn)
import Agent.CLI.Options (CliOptions(..))
import Agent.CLI.PendingInputs (withPendingInputs)
import Agent.CLI.Provider.Switch (prepareTransitionBackend)
import Agent.CLI.Runtime.Orchestration.Providers.Common
    ( decorateAutomaticCompact
    , decorateManualCompact
    , runSession
    )
import Agent.CLI.Runtime.Orchestration.Providers.Types
    ( AgentProviderRequest(..)
    )
import Agent.CLI.Runtime.Orchestration.Types
    ( NativeRunCapabilities(..)
    )
import Agent.CLI.Runtime.Types (RunResult)
import Agent.CLI.Session.History (readLiveTranscript)
import Agent.CLI.Session.Runtime.Types
    ( SessionBackend(..)
    , StartupRuntime(..)
    )
import Agent.CLI.Subagents.Runtime (runXaiParentSubagent)
import Agent.Provider (runWithTokenProvider)
import Agent.Responses.Types (ResponseCreateParams(model))
import Agent.Subagents (setSubagentRunner)
import Agent.Tools.MultiAgents (MultiAgentContext(..))
import Agent.XAI.LoopBackend (xaiBackendWithClientOptions)
import Data.IORef (newIORef, readIORef)
import Data.Maybe (fromMaybe, isJust)
import qualified Agent.XAI.Client as XAIClient
import qualified Agent.XAI.Options as XAI
import qualified Agent.XAI.Request as XAIRequest

runXaiProvider
    :: AgentProviderRequest
    -> NativeRunCapabilities
    -> IO RunResult
runXaiProvider request@AgentProviderRequest{..} nativeCapabilities = do
    xaiOptions0 <- XAI.clientOptionsFromEnv
    let xaiOptions =
            xaiOptions0
                { XAI.hostedXSearchEnabled =
                    nativeCapabilities.nativeProviderHostedTools
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
                        options.optCompactThreshold
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
    case multiCtx of
        Just ctx ->
            setSubagentRunner ctx.multiRegistry $
                runXaiParentSubagent
                    subagentRuntime
                    dialect
                    ctx.multiSendToRoot
                    xaiContextWindow
                    xaiCompactThresholdFor
                    (\childParams ->
                        xaiBackendWithClientOptions
                            xaiOptionsFor
                            tokenProvider
                            (pure childParams))
        Nothing -> pure ()
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
                >>= decorateAutomaticCompact request
                    xaiContextWindow
        -- Reconnection wraps only the continuation. Keeping automatic
        -- compaction outside it prevents a failed continuation from
        -- rerunning the summary.
        requestBackend =
            withConnectionRecoveryOn
                startup.startupNetworkRecovery $
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
                (\outcome inputs ->
                    readIORef automaticCompactionHookRef
                        >>= \hook ->
                            hook outcome inputs)
                (readIORef paramsRef)
                contextTokensRef
                requestBackend
        backend =
            withPendingInputs pendingNotices
                compactingBackend
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
                        >>= decorateManualCompact request
                            xaiContextWindow)
                focus
    activeBackend <-
        prepareTransitionBackend
            modelSwitchScope home projectRoot
            transition persist backend
    runSession
        (sessionRequest
            startupUnavailable
            (Just tokenProvider)
            loaded.loadedOpenAiPool
            (if isGatewayLoadedAuth loaded
                || isJust customGenericOptions
                then Nothing
                else Just selectHttpAccount)
            (Just . xaiContextWindow
                <$> readIORef paramsRef)
            compactRunner)
        SessionBackend
            { backend = activeBackend
            , btwBackend
            , interruptBackend = pure ()
            , resetBackendState = pure ()
            }
