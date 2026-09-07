-- | MCP fleet acquisition and progressive startup notifications.
module Agent.CLI.Runtime.Orchestration.Tools.Mcp
    ( McpRuntime(..)
    , acquireMcpRuntime
    ) where

import Agent.CLI.Config
    ( HarnessConfig(..), McpServerConfig(..)
    , mcpServersForRuntime, useProgressiveMcp )
import Agent.CLI.FileUri (fileUri)
import Agent.CLI.McpElicitation (cliMcpElicitation)
import Agent.CLI.McpOAuthStore (mcpOAuthStorePath)
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
    (IntegrationRuntime(..), IntegrationEndpoint(..), integrationSupervisorArtifactDirectory)
import Agent.Loop (TurnInput(..))
import qualified Agent.MCP as MCP
import Agent.OsPath (unsafeToFilePath)
import Agent.TUI.Model (UiEvent(..))
import Agent.Tools.Types (withToolHumanInputWait)
import Control.Exception.Safe
    ( SomeException, bracketOnError, finally, onException, try )
import Control.Monad (forM_, unless, when)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
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
    } =
    ( serverConfigs
    , useProgressiveMcp
        harnessConfig.configMcpInitStrategy
        (isOneShot options)
    )
  where
    serverConfigs =
        [ MCP.McpServerConfig
            { MCP.mcpServerName = label
            , MCP.mcpServerUrl = config.mcpUrl
            , MCP.mcpServerCommand = Text.unpack config.mcpCommand
            , MCP.mcpServerArgs = map Text.unpack config.mcpArgs
            , MCP.mcpServerCwd =
                Just $
                    maybe (unsafeToFilePath cwd) Text.unpack config.mcpCwd
            , MCP.mcpServerEnv =
                [ (Text.unpack name, Text.unpack value)
                | (name, value) <- Map.toAscList config.mcpEnv
                ] <> case config.mcpUrl of
                    Just url
                        | Map.notMember
                            "MCP_OAUTH_TOKEN_FILE"
                            config.mcpEnv ->
                            [ ( "MCP_OAUTH_TOKEN_FILE"
                              , unsafeToFilePath
                                    (mcpOAuthStorePath home url)
                              )
                            ]
                    _ -> []
            , MCP.mcpServerStartupTimeoutSeconds =
                config.mcpStartupTimeoutSeconds
            , MCP.mcpServerRequestTimeoutSeconds =
                config.mcpRequestTimeoutSeconds
            , MCP.mcpServerProtocol = config.mcpProtocol
            , MCP.mcpServerRootsEnabled = config.mcpRoots
            , MCP.mcpServerSamplingEnabled = config.mcpSampling
            , MCP.mcpServerLogLevel = config.mcpLogLevel
            }
        | (label, config) <-
            mcpServersForRuntime
                nativeCapabilities.nativeMcpTools
                nativeCapabilities.nativeHostExtensions
                harnessConfig
        ]

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
        remoteServers =
            [config | runtime <- maybeToList integrationRuntime
            , RemoteIntegrationEndpoint config <- [integrationRuntimeEndpoint runtime]]
        inMemoryServers =
            [(integrationsMcpConfig, server)
            | runtime <- maybeToList integrationRuntime
            , LocalIntegrationEndpoint server <- [integrationRuntimeEndpoint runtime]]
        -- Include the in-memory name in the reported configuration too: callers
        -- use this list to decide whether MCP tools exist at all.
        runtimeMcpServerConfigs = configuredServers <> remoteServers
            <> map fst inMemoryServers
        transportServers = configuredServers <> remoteServers
        runtimeProgressiveMcp = configuredProgressive && null inMemoryServers
    startStaleResourceCleanup request sessionTmp
    mcpStatusPhaseRef <- newIORef (Nothing :: Maybe Bool)
    mcpFleetRef <- newIORef (Nothing :: Maybe MCP.McpFleet)
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
    if null inMemoryServers
        then do
            let acquireMcpLease =
                    try @_ @SomeException
                        (if runtimeProgressiveMcp
                            then
                                MCP.acquireMcpFleetProgressive
                                    mcpSupervisor
                                    reportProgressiveMcp
                                    runtimeMcpServerConfigs
                            else
                                MCP.acquireMcpFleetWithProgress
                                    mcpSupervisor
                                    (\names ->
                                        setStartupNotice startup.startupFullscreen
                                            (if null names
                                                then "Loading built-in tools…"
                                                else
                                                    "Loading tools: "
                                                        <> Text.intercalate ", " names
                                                        <> "…"))
                                    runtimeMcpServerConfigs)
                        >>= \case
                            Left exception ->
                                startupDie startup
                                    ("Failed to initialize MCP tools: "
                                        <> Text.pack (show exception))
                            Right lease -> pure lease
            bracketOnError
                (acquireMcpLease `onException` clearMcpHostHooks)
                (\lease ->
                    MCP.releaseMcpFleetLease lease
                        `finally` clearMcpHostHooks)
                \runtimeMcpLease -> finishRuntime runtimeMcpLease.mcpLeaseFleet
                    (MCP.releaseMcpFleetLease runtimeMcpLease
                        `finally` clearMcpHostHooks)
        else do
            fleet <-
                ( try @_ @SomeException
                    (MCP.startMcpFleetWithInMemory
                        MCP.defaultMcpHostHooks
                            { MCP.mcpHostElicit =
                                readIORef processRuntime.processMcpElicitation
                            , MCP.mcpHostRoots =
                                readIORef processRuntime.processMcpRoots
                            , MCP.mcpHostSample =
                                readIORef processRuntime.processMcpSampling
                            , MCP.mcpHostArtifactDirectory = Just
                                (integrationSupervisorArtifactDirectory
                                    processRuntime.processIntegrationSupervisor)
                            }
                        (\names ->
                            setStartupNotice startup.startupFullscreen
                                (if null names
                                    then "Loading built-in tools…"
                                    else
                                        "Loading tools: "
                                            <> Text.intercalate ", " names
                                            <> "…"))
                        transportServers
                        inMemoryServers)
                    >>= \case
                        Left exception ->
                            startupDie startup
                                ("Failed to initialize MCP tools: "
                                    <> Text.pack (show exception))
                        Right value -> pure value
                ) `onException` clearMcpHostHooks
            finishRuntime fleet
                (MCP.closeMcpFleet fleet `finally` clearMcpHostHooks)

integrationsMcpConfig :: MCP.McpServerConfig
integrationsMcpConfig = MCP.McpServerConfig
    { MCP.mcpServerName = "integrations"
    , MCP.mcpServerUrl = Nothing
    , MCP.mcpServerCommand = ""
    , MCP.mcpServerArgs = []
    , MCP.mcpServerCwd = Nothing
    , MCP.mcpServerEnv = []
    , MCP.mcpServerStartupTimeoutSeconds = 10
    , MCP.mcpServerRequestTimeoutSeconds = 60
    , MCP.mcpServerProtocol = MCP.McpProtocolModern
    , MCP.mcpServerRootsEnabled = False
    , MCP.mcpServerSamplingEnabled = False
    , MCP.mcpServerLogLevel = Nothing
    }
