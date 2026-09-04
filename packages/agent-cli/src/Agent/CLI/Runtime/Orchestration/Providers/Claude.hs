module Agent.CLI.Runtime.Orchestration.Providers.Claude
    ( runClaudeProvider
    ) where

import Agent.CLI.Auth.Types
    ( LoadedAuth
    , isGatewayLoadedAuth
    )
import Agent.CLI.Claude
    ( approveClaudeRegisteredTool
    , handleClaudePermissionRequest
    )
import Agent.CLI.ClaudeGatewayProxy (withClaudeGatewayProxy)
import Agent.CLI.Compaction
    ( autoCompactBackendWith
    , claudeAutoCompactTokenLimit
    , claudeCompactionInputLimit
    , installLiveCompactOutcome
    , runBackendCompactHistoryWithLimits
    , runBackendCompactWithLimits
    )
import Agent.CLI.GatewayClient (GatewayCredential)
import Agent.CLI.Options (CliOptions(..))
import Agent.CLI.Provider.Switch (prepareTransitionBackend)
import Agent.CLI.Render (putTextLn)
import Agent.CLI.Runtime.Orchestration.Providers.Common
    ( decorateAutomaticCompact
    , decorateManualCompact
    , runSession
    )
import Agent.CLI.Runtime.Orchestration.Providers.Types
    ( AgentProviderRequest(..)
    )
import Agent.CLI.Runtime.Orchestration.Types (NativeRunCapabilities(..))
import Agent.CLI.Runtime.Types (RunResult)
import Agent.CLI.Session.History (readLiveTranscript)
import Agent.CLI.Session.Runtime.Types
    ( SessionBackend(..)
    , SessionRequest(..)
    )
import Agent.CLI.Startup.Auth (startupDie)
import Agent.CLI.Style (glyphWarn, roleWarn)
import Agent.CLI.TUI.App (emitUiEvent)
import Agent.CLI.Terminal (resolveColor)
import Agent.Claude
    ( ClaudeCodeAuth(..)
    , ClaudeCodeBackendHandle(..)
    , ClaudeCodeOptions(..)
    , ClaudeCodePermission(..)
    , claudeCodeOneShotBackend
    , defaultClaudeCodeOptions
    , loadClaudeCodeAuth
    , loadClaudeCodeGatewayAuth
    , withClaudeCodeBackendWithHost
    )
import Agent.Claude.Control
    ( ClaudeCodeHostHandlers(..)
    , ClaudeCodeMcpRequest(..)
    , defaultClaudeCodeHostHandlers
    )
import Agent.Loop
    ( Backend(Backend, submitTurn)
    , BackendSnapshot(..)
    , defaultLoopDispatch
    )
import Agent.OsPath (unsafeToFilePath)
import Agent.ToolDispatch (ToolDispatchConfig(..))
import Agent.Tools.OutputArtifact (finalizeToolOutput)
import Agent.TUI.Model (UiEvent(UiSystemMessage))
import Control.Monad (when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Agent.MCP as MCP
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

runClaudeProvider
    :: AgentProviderRequest
    -> NativeRunCapabilities
    -> IO RunResult
runClaudeProvider request@AgentProviderRequest{..} nativeCapabilities =
    withSelectedClaudeAuth
        connectedGateway
        loaded
        (startupDie startup . Text.unpack)
        \claudeAuth -> do
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
                    currentParams <- readIORef paramsRef
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
                                    options.optCompactThreshold
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
                                >>= decorateManualCompact request
                                    (const contextWindow))
                        focus
                claudeRequest =
                    sessionRequest
                        startupUnavailable
                        Nothing
                        Nothing
                        Nothing
                        (Just <$> claudeContextWindow)
                        compactRunner
            claudeMcpServer <-
                case MCP.createInProcessMcpServer
                    "haskell-agent"
                    "0.1.0"
                    (defaultLoopDispatch
                        { toolDispatchFinalizeOutput =
                            \call output ->
                                finalizeToolOutput
                                    claudeRequest.toolEnv
                                    call
                                    output
                        })
                    (approveClaudeRegisteredTool
                        claudeRequest.claudeRuntimeSlot)
                    claudeRequest.claudeBridgeTools of
                    Left err -> startupDie startup (Text.unpack err)
                    Right server -> pure server
            let hostHandlers =
                    defaultClaudeCodeHostHandlers
                        { canUseTool =
                            Just
                                (handleClaudePermissionRequest
                                    claudeRequest.claudeRuntimeSlot)
                        , handleMcpMessage =
                            Just \mcpRequest ->
                                if mcpRequest.serverName
                                        /= "haskell-agent"
                                    then
                                        pure Aeson.Null
                                    else
                                        MCP.handleInProcessMcpMessage
                                            claudeMcpServer
                                            mcpRequest.message
                                            >>= pure . fromMaybe
                                                (Aeson.object [])
                        , mcpToolNames =
                            MCP.inProcessMcpToolNames
                                claudeMcpServer
                        , nativeToolsEnabled =
                            nativeCapabilities.nativeProviderNativeTools
                        }
            when claudeBypassEnabled $
                case fullscreen of
                    Just runtime ->
                        emitUiEvent runtime
                            (UiSystemMessage
                                "Claude Code --yolo is active; host catastrophic-command and Plan Mode denies remain enforced.")
                    Nothing -> do
                        color <- resolveColor stderrHandle
                        putTextLn stderrHandle $
                            roleWarn color $
                                glyphWarn
                                    <> "Claude Code --yolo is active; host catastrophic-command and Plan Mode denies remain enforced."
            writeIORef activeAccountRef claudeAuth.accountLabel
            claudeTranscriptRef <-
                newIORef =<< readLiveTranscript conversationRef
            withClaudeCodeBackendWithHost
                claudeOptions
                hostHandlers
                initialPrevious
                (readIORef paramsRef)
                claudeTranscriptRef
                \handle -> do
                    let compactHistory history _inputs = do
                            contextWindow <- claudeContextWindow
                            inputLimit <- claudeSummaryInputLimit
                            currentParams <- readIORef paramsRef
                            runBackendCompactHistoryWithLimits
                                contextWindow
                                inputLimit
                                btwBackend
                                recordCompactionUsage
                                currentParams
                                history
                                Nothing
                                >>= decorateAutomaticCompact request
                                    (const contextWindow)
                        compactingBackend =
                            autoCompactBackendWith
                                claudeCompactThreshold
                                compactHistory
                                (\outcome inputs ->
                                    readIORef
                                        automaticCompactionHookRef
                                        >>= \hook ->
                                            hook outcome inputs)
                                (readIORef paramsRef)
                                contextTokensRef
                                handle.loopBackend
                    activeBackend <-
                        prepareTransitionBackend
                            modelSwitchScope home projectRoot
                            transition persist compactingBackend
                    runSession
                        claudeRequest
                        SessionBackend
                            { backend = activeBackend
                            , btwBackend
                            , interruptBackend =
                                handle.interruptActiveTurn
                            , resetBackendState =
                                writeIORef claudeTranscriptRef []
                            }

withSelectedClaudeAuth
    :: Maybe GatewayCredential
    -> LoadedAuth
    -> (Text -> IO value)
    -> (ClaudeCodeAuth -> IO value)
    -> IO value
withSelectedClaudeAuth connectedGateway loaded onError action
    -- Use the same immutable credential snapshot that selected the catalog,
    -- auth, and session boundary. Reloading here could cross organizations.
    | not (isGatewayLoadedAuth loaded) =
        loadClaudeCodeAuth >>= either onError action
    | otherwise = case connectedGateway of
        Nothing ->
            onError "No organization gateway credential is connected."
        Just credential -> do
            result <- withClaudeGatewayProxy credential \transport ->
                loadClaudeCodeGatewayAuth transport
                    >>= either onError action
            either onError pure result
