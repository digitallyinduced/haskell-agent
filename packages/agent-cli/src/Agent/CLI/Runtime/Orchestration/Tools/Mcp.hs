-- | MCP fleet acquisition and progressive startup notifications.
module Agent.CLI.Runtime.Orchestration.Tools.Mcp
    ( McpRuntime(..)
    , acquireMcpRuntime
    ) where

import Agent.Runtime.Mcp.Startup
import Agent.CLI.FileUri (fileUri)
import Agent.CLI.IntegrationGateway (integrationEndpointServers)
import Agent.CLI.McpElicitation (cliMcpElicitation)
import Agent.CLI.McpStatus
    ( formatMcpInstructionsNotice, formatMcpModelNoticeFor
    , formatMcpProgress, summarizeMcpStatuses )
import Agent.CLI.Options (isOneShot)
import Agent.CLI.PendingInputs (PendingNoticeKind(..), enqueuePendingNotice)
import Agent.CLI.Prompt (mcpInstructionsForRequest)
import Agent.CLI.Runtime.Orchestration.Startup (reportStartupWarning)
import Agent.CLI.Runtime.Orchestration.Tools.Collaboration
import Agent.CLI.Runtime.Orchestration.Tools.Model
import Agent.CLI.Runtime.Orchestration.Tools.Request
import Agent.CLI.Runtime.Orchestration.Tools.Scratch
import Agent.CLI.Runtime.Orchestration.Types (AgentProcessRuntime(..), NativeRunCapabilities(..))
import Agent.CLI.Session.Runtime.Types (StartupRuntime(..))
import Agent.CLI.Startup.Auth (setStartupNotice, startupDie)
import Agent.CLI.TUI.App (emitUiEvent)
import Agent.Integration.API
    (IntegrationRuntime(..))
import Agent.Loop (TurnInput(..))
import qualified Agent.MCP as MCP
import Agent.OsPath (unsafeToFilePath)
import Agent.TUI.Model (UiEvent(..))
import Agent.Tools.Types (withToolHumanInputWait)
import Control.Exception.Safe (catch)
import Control.Monad (forM_, unless, when)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text

data McpRuntime = McpRuntime
    { runtimeMcpServerConfigs :: [MCP.McpServerConfig]
    , runtimeProgressiveMcp :: Bool
    , runtimeMcpFleet :: MCP.McpFleet
    , runtimeMcpInstructions :: [(Text, Text)]
    , runtimeCloseMcp :: IO ()
    }

mcpConfiguration
    :: AgentToolsRequest windowTitleResult
    -> ToolStartup
    -> ([MCP.McpServerConfig], Bool)
mcpConfiguration AgentToolsRequest
    { cwd
    , home
    , options
    } ToolStartup
    { toolNativeCapabilities = nativeCapabilities
    , toolHarnessConfig = harnessConfig
    } = resolveMcpConfiguration McpConfigurationRequest
        { mcpConfigCwd = cwd
        , mcpConfigHome = home
        , mcpConfigHarness = harnessConfig
        , mcpConfigToolsEnabled = nativeCapabilities.nativeMcpTools
        , mcpConfigHostExtensions = nativeCapabilities.nativeHostExtensions
        , mcpConfigOneShot = isOneShot options
        }

acquireMcpRuntime
    :: AgentToolsRequest windowTitleResult
    -> ToolStartup
    -> ToolModelRuntime
    -> CollaborationRuntime
    -> ScratchRuntime
    -> Maybe IntegrationRuntime
    -> IO McpRuntime
acquireMcpRuntime request@AgentToolsRequest
    { processRuntime
    , startup
    , options
    , isTty
    , stdinControl
    , uiRuntimeRef
    , baseToolEnv
    , mcpSupervisor
    } toolStartup ToolModelRuntime
    { toolDialectId = dialectId
    } CollaborationRuntime
    { collaborationPendingNotices = pendingNotices
    } ScratchRuntime
    { scratchSessionTmp = sessionTmp
    } integrationRuntime = do
    let (configuredServers, configuredProgressive) =
            mcpConfiguration request toolStartup
        (remoteServers, localServers) = maybe ([], [])
            (integrationEndpointServers (map (.mcpServerName) configuredServers)
                . integrationRuntimeEndpoint)
            integrationRuntime
        inMemoryServers =
            [(integrationsMcpConfig name, server) | (name, server) <- localServers]
        -- Include the in-memory name in the reported configuration too: callers
        -- use this list to decide whether MCP tools exist at all.
        mcpRequest = McpStartupRequest
            { mcpStartupTransportServers = configuredServers <> remoteServers
            , mcpStartupInMemoryServers = inMemoryServers
            , mcpStartupProgressive = configuredProgressive
            }
        runtimeMcpServerConfigs = startupServerConfigs mcpRequest
        runtimeProgressiveMcp = configuredProgressive
    startStaleResourceCleanup request sessionTmp
    mcpStatusPhaseRef <- newIORef (Nothing :: Maybe Bool)
    mcpFleetRef <- newIORef (Nothing :: Maybe MCP.McpFleet)
    let installMcpHostHooks = do
            writeIORef processRuntime.processMcpElicitation
                (if isOneShot options || not isTty
                    then Nothing
                    else Just \elicitation ->
                        withToolHumanInputWait baseToolEnv $
                            cliMcpElicitation stdinControl uiRuntimeRef elicitation)
            writeIORef processRuntime.processMcpRoots $
                Just \_serverName ->
                    pure
                        [ MCP.McpRoot
                            { MCP.rootUri = fileUri (unsafeToFilePath request.cwd)
                            , MCP.rootName = Nothing
                            }
                        ]
            writeIORef processRuntime.processMcpSampling $
                if any
                    (\MCP.McpServerConfig
                        { MCP.mcpServerSamplingEnabled = samplingEnabled
                        } -> samplingEnabled)
                    runtimeMcpServerConfigs
                    then Just \_ ->
                        pure (Left "MCP sampling is unavailable until the model session is ready")
                    else Nothing
    let clearMcpHostHooks = do
            writeIORef processRuntime.processMcpElicitation Nothing
            writeIORef processRuntime.processMcpRoots Nothing
            writeIORef processRuntime.processMcpSampling Nothing
    let enqueueMcpSnapshot statuses =
            unless (null statuses) do
                instructions <-
                    readIORef mcpFleetRef
                        >>= maybe (pure []) MCP.mcpFleetInstructions
                enqueuePendingNotice pendingNotices PendingMcpNotice
                    (UserMessage
                        (formatMcpModelNoticeFor dialectId statuses
                            <> formatMcpInstructionsNotice instructions))
                    >>= either (reportStartupWarning startup) pure
        reportProgressiveMcp statuses = do
            finished <- readIORef startup.startupFinished
            unless finished do
                setStartupNotice startup.startupFullscreen
                    (formatMcpProgress statuses)
                -- A callback can race with finishStartup between the read and
                -- the UI update. Clear a late notice if startup won the race.
                readIORef startup.startupFinished >>= \nowFinished ->
                    when nowFinished $
                        forM_ startup.startupFullscreen \runtime ->
                            emitUiEvent runtime (UiSetNotice Nothing)
            let (connecting, _, _) = summarizeMcpStatuses statuses
                isConnecting = connecting > 0
            settled <-
                atomicModifyIORef' mcpStatusPhaseRef \previous ->
                    (Just isConnecting, previous == Just True && not isConnecting)
            when settled (enqueueMcpSnapshot statuses)
        finishRuntime runtimeMcpFleet runtimeCloseMcp = do
            writeIORef mcpFleetRef (Just runtimeMcpFleet)
            when runtimeProgressiveMcp $
                MCP.mcpFleetStatuses runtimeMcpFleet >>= enqueueMcpSnapshot
            currentMcpInstructions <- MCP.mcpFleetInstructions runtimeMcpFleet
            let runtimeMcpInstructions =
                    mcpInstructionsForRequest
                        runtimeProgressiveMcp
                        currentMcpInstructions
            mapM_
                (reportStartupWarning startup)
                runtimeMcpFleet.mcpFleetWarnings
            setStartupNotice startup.startupFullscreen "Loading built-in tools…"
            pure McpRuntime{..}
        reportBlockingMcp names =
            setStartupNotice startup.startupFullscreen
                (if null names
                    then "Loading built-in tools…"
                    else
                        "Loading tools: "
                            <> Text.intercalate ", " names
                            <> "…")
    acquireMcpStartup mcpSupervisor mcpRequest McpStartupHooks
        { mcpInstallHostHooks = installMcpHostHooks
        , mcpClearHostHooks = clearMcpHostHooks
        , mcpReportBlocking = reportBlockingMcp
        , mcpReportProgressive = reportProgressiveMcp
        } finishRuntime
        `catch` \(McpStartupError exception) ->
            startupDie startup
                ("Failed to initialize MCP tools: " <> Text.pack (show exception))
