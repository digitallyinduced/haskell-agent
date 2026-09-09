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
    , rememberReportedContextWindow
    , runClaudeBackendCompactHistoryWithLimits
    , runClaudeBackendCompactWithLimits
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
    ( Backend(submitTurnWithCallbacks)
    , BackendSnapshot(..)
    , backendWithCallbacks
    )
import Agent.OsPath (unsafeToFilePath)
import Agent.Telemetry (preferReportedContextWindow)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)

withClaudeProvider
    :: ClaudeConfig
    -> ProviderHost
    -> (ProviderRuntime -> IO a)
    -> IO a
withClaudeProvider ClaudeConfig{..}
        ProviderHost{compaction = ProviderCompaction{..}} use =
    withAuth \claudeAuth -> do
        reportedWindowRef <- newIORef Nothing
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
                reported <- readIORef reportedWindowRef
                pure $
                    preferReportedContextWindow reported $
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
                backendWithCallbacks \state previous inputs callbacks -> do
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
                    privateBackend.submitTurnWithCallbacks
                        state
                        previous
                        inputs
                        callbacks
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
                        runClaudeBackendCompactWithLimits
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
                        runClaudeBackendCompactHistoryWithLimits
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
                            (rememberReportedContextWindow
                                reportedWindowRef
                                handle.loopBackend)
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
