module Agent.CLI.Runtime.Orchestration.Providers.Gemini
    ( runGeminiProvider
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
import Agent.Gemini.LoopBackend (tokenProviderStatelessGeminiBackend)
import Agent.Provider
    ( Provider(GeminiProvider)
    , runWithTokenProvider
    )
import Agent.Subagents (setSubagentRunner)
import Agent.Tools.MultiAgents (MultiAgentContext(..))
import Data.IORef (newIORef, readIORef)
import qualified Agent.Gemini.Client as GeminiClient
import qualified Agent.Gemini.Options as Gemini

runGeminiProvider
    :: AgentProviderRequest
    -> IO RunResult
runGeminiProvider request@AgentProviderRequest{..} = do
    geminiOptions <- Gemini.clientOptionsFromEnv
    geminiOccupancy <- newIORef Nothing
    let geminiContextWindow =
            contextWindowForParams id 1_048_576
        makeBackend getParams =
            tokenProviderStatelessGeminiBackend
                tokenProvider
                (GeminiClient.createResponseWithEvents
                    geminiOptions)
                getParams
        protectGeminiOverflow occupancy getParams backend =
            boundCompletedToolContinuations
                geminiContextWindow
                getParams
                occupancy
                backend
    case multiCtx of
        Just ctx ->
            setSubagentRunner ctx.multiRegistry $
                runHttpSubagent
                    subagentRuntime
                    dialect
                    GeminiProvider
                    ctx.multiSendToRoot
                    (\childParams ->
                        protectGeminiOverflow
                            geminiOccupancy
                            (pure childParams)
                            (makeBackend
                                (pure childParams)))
        Nothing -> pure ()
    let backend =
            withPendingInputs pendingNotices $
                withConnectionRecoveryOn
                    startup.startupNetworkRecovery $
                    protectGeminiOverflow
                        geminiOccupancy
                        (readIORef paramsRef)
                        (makeBackend
                            (readIORef paramsRef))
        btwBackend privateParams =
            makeBackend (pure privateParams)
        compactRunner focus = do
            contextWindow <-
                currentModelContextWindow id
            historyRef <-
                newIORef =<< readLiveTranscript conversationRef
            installLiveCompactOutcome conversationRef Nothing
                (\requestedFocus ->
                    runResponsesCompactWithContextWindow
                        contextWindow
                        (\compactRequest ->
                            runWithTokenProvider tokenProvider
                                \credential ->
                                    GeminiClient.createResponseWith
                                        geminiOptions
                                        credential
                                        compactRequest)
                        recordCompactionUsage
                        paramsRef
                        historyRef
                        requestedFocus
                        >>= decorateManualCompact request
                            geminiContextWindow)
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
            (Just . geminiContextWindow
                <$> readIORef paramsRef)
            compactRunner)
        SessionBackend
            { backend = activeBackend
            , btwBackend
            , interruptBackend = pure ()
            , resetBackendState = pure ()
            }
