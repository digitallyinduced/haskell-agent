module Agent.CLI.Runtime.Orchestration.Providers.Claude
    ( withClaudeProvider
    ) where

import Agent.CLI.Session.Request
    ( readSessionRequestParams
    )
import Agent.CLI.Compaction
    ( autoCompactBackendWith
    , claudeAutoCompactTokenLimit
    , claudeCompactionInputLimit
    , installLiveCompactOutcome
    , runBackendCompactHistoryWithLimits
    , runBackendCompactWithLimits
    )
import Agent.CLI.Runtime.Orchestration.Providers.Common
    ( decorateAutomaticCompact
    , decorateManualCompact
    )
import Agent.CLI.Runtime.Orchestration.Providers.Types
    ( ClaudeConfig(..), ProviderHost(..), ProviderCompaction(..)
    , ProviderRuntime(..), ProviderAccountSelection(..), ProviderSubagents(..)
    )
import Agent.CLI.Session.History (readLiveTranscript)
import Agent.CLI.Session.Runtime.Types
    ( SessionBackend(..)
    )
import Agent.Claude
    ( ClaudeCodeAuth(..)
    , ClaudeCodeBackendHandle(..)
    , ClaudeCodeOptions(..)
    , ClaudeCodePermission(..)
    , claudeCodeOneShotBackend
    , defaultClaudeCodeOptions
    , withClaudeCodeBackendWithHost
    )
import Agent.Loop
    ( Backend(Backend, submitTurn)
    , BackendSnapshot(..)
    )
import Agent.OsPath (unsafeToFilePath)
import Data.IORef (newIORef, writeIORef)
import Data.Maybe (fromMaybe)

withClaudeProvider
    :: ClaudeConfig
    -> ProviderHost
    -> (ProviderRuntime -> IO a)
    -> IO a
withClaudeProvider ClaudeConfig{..}
        ProviderHost{compaction = ProviderCompaction{..}} use =
    withAuth \claudeAuth -> do
        let permission =
                ClaudeCodeManual
            claudeOptions =
                (defaultClaudeCodeOptions
                    claudeAuth.executable
                    (unsafeToFilePath cwd))
                    { permission
                    , safeMode = True
                    , transport = claudeAuth.transport
                    }
            claudeContextWindow = do
                currentParams <- readSessionRequestParams paramsRef
                pure $
                    contextWindowForParams
                        transportModel
                        200_000
                        currentParams
            claudeCompactThreshold = do
                contextWindow <- claudeContextWindow
                let hardLimit =
                        claudeCompactionInputLimit contextWindow
                pure $
                    max 1 $
                        min hardLimit $
                            fromMaybe
                                (claudeAutoCompactTokenLimit
                                    contextWindow)
                                compactThreshold
            claudeSummaryInputLimit =
                claudeCompactionInputLimit
                    <$> claudeContextWindow
            btwBackend privateParams =
                Backend \state previous inputs onEvent -> do
                    privateTranscript <-
                        newIORef state.backendItems
                    let privateBackend =
                            claudeCodeOneShotBackend
                                claudeOptions
                                    { permission =
                                        ClaudeCodeDontAsk
                                    }
                                (pure privateParams)
                                privateTranscript
                    privateBackend.submitTurn
                        state
                        previous
                        inputs
                        onEvent
            compactRunner focus = do
                contextWindow <- claudeContextWindow
                inputLimit <- claudeSummaryInputLimit
                historyRef <-
                    newIORef =<< readLiveTranscript
                        conversationRef
                installLiveCompactOutcome
                    conversationRef
                    (Just contextTokensRef)
                    (\requestedFocus ->
                        runBackendCompactWithLimits
                            contextWindow
                            inputLimit
                            btwBackend
                            recordCompactionUsage
                            paramsRef
                            historyRef
                            requestedFocus
                            >>= decorateManualCompact (readSessionRequestParams paramsRef) taskPlan
                                (const contextWindow))
                    focus
        onConnected claudeAuth.accountLabel
        claudeTranscriptRef <-
            newIORef =<< readLiveTranscript conversationRef
        withClaudeCodeBackendWithHost
            claudeOptions
            hostHandlers
            initialPrevious
            (readSessionRequestParams paramsRef)
            claudeTranscriptRef
            \handle -> do
                let compactHistory history _inputs = do
                        contextWindow <- claudeContextWindow
                        inputLimit <- claudeSummaryInputLimit
                        currentParams <- readSessionRequestParams paramsRef
                        runBackendCompactHistoryWithLimits
                            contextWindow
                            inputLimit
                            btwBackend
                            recordCompactionUsage
                            currentParams
                            history
                            Nothing
                            >>= decorateAutomaticCompact (readSessionRequestParams paramsRef) taskPlan
                                (const contextWindow)
                    compactingBackend =
                        autoCompactBackendWith
                            claudeCompactThreshold
                            compactHistory
                            installAutomaticCompact
                            (readSessionRequestParams paramsRef)
                            contextTokensRef
                            handle.loopBackend
                use ProviderRuntime
                    { sessionBackend = SessionBackend
                        { backend = compactingBackend
                        , btwBackend
                        , interruptBackend = handle.interruptActiveTurn
                        , resetBackendState = writeIORef claudeTranscriptRef []
                        }
                    , currentContextWindow = Just <$> claudeContextWindow
                    , compactRunner
                    , accountSelection = NoAccountSelection
                    , subagents = NoProviderSubagents
                    }
