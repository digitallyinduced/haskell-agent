module Agent.CLI.Runtime.Orchestration.Providers.OpenRouter
    ( runOpenRouterProvider
    ) where

import Agent.CLI.Auth.Types
    ( LoadedAuth(..)
    , isGatewayLoadedAuth
    )
import Agent.CLI.Compaction
    ( boundCompletedToolContinuations
    , installLiveCompactOutcome
    , runResponsesCompactWithContextWindow
    )
import Agent.Connectivity (withConnectionRecoveryOn)
import Agent.CLI.PendingInputs (withPendingInputs)
import Agent.CLI.Provider.Switch (prepareTransitionBackend)
import Agent.CLI.Runtime.Orchestration.Providers.Common
    ( decorateManualCompact
    , runSession
    )
import Agent.CLI.Runtime.Orchestration.Providers.Types
    ( AgentProviderRequest(..)
    )
import Agent.CLI.Runtime.Types (RunResult)
import Agent.CLI.Session.History (readLiveTranscript)
import Agent.CLI.Session.Runtime.Types
    ( SessionBackend(..)
    , StartupRuntime(..)
    )
import Agent.CLI.Subagents.Runtime (runHttpSubagent)
import Agent.OpenRouter.LoopBackend (openRouterBackend)
import Agent.Provider
    ( Provider(OpenRouterProvider)
    , runWithTokenProvider
    )
import Agent.Responses.GenericBackend (genericResponsesBackendWith)
import Agent.Responses.Types (ResponseCreateParams(model))
import Agent.Subagents (setSubagentRunner)
import Agent.Tools.MultiAgents (MultiAgentContext(..))
import Data.IORef (newIORef, readIORef)
import Data.Maybe (fromMaybe)
import qualified Agent.OpenRouter as OpenRouter
import qualified Agent.Responses.GenericClient as GenericResponses

runOpenRouterProvider
    :: AgentProviderRequest
    -> IO RunResult
runOpenRouterProvider request@AgentProviderRequest{..} = do
    let openRouterContextWindow =
            contextWindowForParams transportModel 1_048_576
        makeBackend params =
            case customGenericOptions of
                Just genericOptions ->
                    genericResponsesBackendWith
                        (\responseRequest onEvent ->
                            GenericResponses.createResponseWithEvents
                                genericOptions
                                    { GenericResponses.model =
                                        transportModel
                                            (fromMaybe
                                                model
                                                responseRequest.model)
                                    }
                                responseRequest
                                onEvent)
                        params
                Nothing ->
                    openRouterBackend openRouterOptions
                        tokenProvider params
        protectOverflow occupancy getParams backend =
            boundCompletedToolContinuations
                openRouterContextWindow
                getParams
                occupancy
                backend
    case multiCtx of
        Just ctx ->
            setSubagentRunner ctx.multiRegistry $
                runHttpSubagent
                    subagentRuntime
                    dialect
                    OpenRouterProvider
                    ctx.multiSendToRoot
                    (\childParams ->
                        protectOverflow
                            contextTokensRef
                            (pure childParams)
                            (makeBackend
                                (pure childParams)))
        Nothing -> pure ()
    let backend =
            withPendingInputs pendingNotices $
                withConnectionRecoveryOn
                    startup.startupNetworkRecovery $
                    protectOverflow
                        contextTokensRef
                        (readIORef paramsRef)
                        (makeBackend
                            (readIORef paramsRef))
        btwBackend privateParams =
            makeBackend
                (pure privateParams)
        compactRunner focus = do
            contextWindow <-
                currentModelContextWindow transportModel
            historyRef <-
                newIORef =<< readLiveTranscript conversationRef
            installLiveCompactOutcome conversationRef Nothing
                (\requestedFocus ->
                    (case customGenericOptions of
                        Just genericOptions ->
                            runResponsesCompactWithContextWindow
                                contextWindow
                                (\responseRequest ->
                                    GenericResponses.createResponseWith
                                        genericOptions
                                            { GenericResponses.model =
                                                transportModel
                                                    (fromMaybe
                                                        model
                                                        responseRequest.model)
                                            }
                                        responseRequest)
                                recordCompactionUsage
                                paramsRef
                                historyRef
                        Nothing ->
                            runResponsesCompactWithContextWindow
                                contextWindow
                                (\responseRequest ->
                                    runWithTokenProvider
                                        tokenProvider
                                        \credential ->
                                            OpenRouter.createResponseWith
                                                openRouterOptions
                                                credential
                                                responseRequest)
                                recordCompactionUsage
                                paramsRef
                                historyRef)
                        requestedFocus
                        >>= decorateManualCompact request
                            openRouterContextWindow)
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
                then Nothing
                else Just selectHttpAccount)
            (Just . openRouterContextWindow
                <$> readIORef paramsRef)
            compactRunner)
        SessionBackend
            { backend = activeBackend
            , btwBackend
            , interruptBackend = pure ()
            , resetBackendState = pure ()
            }
