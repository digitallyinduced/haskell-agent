-- | MCP fleet acquisition and progressive startup notifications.
module Agent.CLI.Runtime.Orchestration.Tools.Mcp
    ( McpRuntime(..)
    , acquireMcpRuntime
    ) where

import Agent.CLI.Config (HarnessConfig(..), McpServerConfig(..), useProgressiveMcp)
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
import Agent.Loop (TurnInput(..))
import qualified Agent.MCP as MCP
import Agent.OsPath (unsafeToFilePath)
import Agent.TUI.Model (UiEvent(..))
import Agent.Tools.Types (withToolHumanInputWait)
import Control.Exception.Safe (SomeException, bracketOnError, try)
import Control.Monad (forM_, unless, when)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
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
            }
        | (label, config) <-
            Map.toAscList harnessConfig.configMcpServers
        , config.mcpEnabled
        , nativeCapabilities.nativeHostExtensions
        ]

acquireMcpRuntime
    :: AgentToolsRequest windowTitleResult
    -> ToolStartup
    -> ToolModelRuntime
    -> CollaborationRuntime
    -> ScratchRuntime
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
    } = do
    let (runtimeMcpServerConfigs, runtimeProgressiveMcp) =
            mcpConfiguration request toolStartup
    startStaleResourceCleanup request sessionTmp
    mcpStatusPhaseRef <- newIORef (Nothing :: Maybe Bool)
    mcpFleetRef <- newIORef (Nothing :: Maybe MCP.McpFleet)
    writeIORef processRuntime.processMcpElicitation
        (if isOneShot options || not isTty
            then Nothing
            else Just \elicitation ->
                withToolHumanInputWait baseToolEnv $
                    cliMcpElicitation stdinControl uiRuntimeRef elicitation)
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
        acquireMcpLease
        MCP.releaseMcpFleetLease
        \runtimeMcpLease -> do
            let runtimeMcpFleet = runtimeMcpLease.mcpLeaseFleet
                runtimeCloseMcp =
                    MCP.releaseMcpFleetLease runtimeMcpLease
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
