module Agent.CLI.NativeRuntime
    ( NativeProcessRuntime
    , NativeInteractionMode(..)
    , NativeDiscoveryContext(..)
    , NativeWorkspaceDiscovery(..)
    , NativeRunCapabilities(..)
    , NativeShellMode(..)
    , NativeRunHooks(..)
    , fullNativeRunCapabilities
    , nativeLoadsHostWorkspaceContext
    , nativePreparedDiscovery
    , NativeSessionTarget(..)
    , NativeTurnRequest(..)
    , NativeMessageClock(..)
    , StartupFailure(..)
    , exitFailedTurn
    , closeNativeProcessRuntime
    , newNativeProcessRuntime
    , newNativeProcessRuntimeWithIntegrations
    , newNativeProcessRuntimeWithOrganizationIntegrations
    , nativeProcessIntegrationSupervisor
    , acquireNativeLocalIntegrationRuntime
    , nativeTurnOptions
    , applyNativeStartupPolicy
    , applyNativeInteractionMode
    , restartNativeMcpRuntime
    , runNativeAgent
    , runNativeTurn
    ) where

import qualified Agent.Runtime.NativeProcess as NativeProcess
import Agent.CLI.Session.Lifecycle (exitFailedTurn)
import Agent.CLI.Session.Runner.Execution (applyNativeInteractionMode)
import Agent.Integration.API
    ( IntegrationSupervisor
    , IntegrationRuntime(..)
    , IntegrationAuthority(..)
    , acquireIntegrationRuntime
    , newIntegrationSupervisor
    , closeIntegrationSupervisor
    , newIntegrationSupervisorWithOrganizationProvider
    , resetIntegrationSupervisor
    , IntegrationProvider
    , OrganizationIntegrationProvider
    , emptyIntegrationProvider
    )
import Agent.Runtime.StartupPolicy
    ( NativeStartupPolicy(..)
    , NativeContextSources(..)
    , NativeExecutionFacilities(..)
    )
import Agent.Loop
    ( TurnAttachment(ImageAttachmentItem)
    , userMessageWithAttachments
    )
import Agent.CLI.Options
    ( Command(..)
    , CliOptions(..)
    , CodeModeOption(..)
    , ScreenMode(..)
    , defaultCliOptions
    , parseArgs
    )
import Agent.Connectivity.NetworkPath (networkRecovery)
import Agent.CLI.Runtime.Orchestration (runAgentWithRuntime)
import Agent.CLI.Runtime.Orchestration.Types
    ( AgentProcessRuntime(..)
    , NativeInteractionMode(..)
    , NativeDiscoveryContext(..)
    , NativeWorkspaceDiscovery(..)
    , NativeRunCapabilities(..)
    , NativeShellMode(..)
    , NativeRunHooks(..)
    , fullNativeRunCapabilities
    , nativeLoadsHostWorkspaceContext
    , nativePreparedDiscovery
    , nativeRunMode
    )
import Agent.CLI.Runtime.Types (DevResult(..), StartupFailure(..))
import Agent.Runtime.Request
    ( NativeMessageClock(..)
    , NativeSessionTarget(..)
    , NativeTurnRequest(..)
    , validateNativeTurnRequest
    )
import Agent.CLI.Timestamp (MessageClock, parseMessageClock)
import Agent.TUI.Motion (MotionMode(..))
import Agent.Tools.Types (defaultToolEnv)
import qualified Agent.MCP as MCP
import Agent.Runtime.McpConnectionRuntime (mcpConnectionCredentials, registerMcpConnectionRuntime, observeMcpConnectionInfo)
import Control.Exception.Safe (finally, mask, onException)
import Data.Text (Text)
import qualified Data.Text as Text
import System.IO (Handle)
import System.OsPath (OsPath)

-- | Native process resources, including the shared in-memory integrations
-- host. Shared process resources live in @agent-runtime@; frontend lifecycle
-- wiring remains here.
data NativeProcessRuntime = NativeProcessRuntime
    { nativeProcessCore :: !NativeProcess.NativeProcessRuntime
    , nativeIntegrationSupervisor :: !IntegrationSupervisor
    , nativeLocalIntegrationSupervisor :: !IntegrationSupervisor
    , nativeUnregisterMcpConnections :: !(IO ())
    }

newNativeProcessRuntime :: OsPath -> IO NativeProcessRuntime
newNativeProcessRuntime = newNativeProcessRuntimeWithIntegrations emptyIntegrationProvider

newNativeProcessRuntimeWithIntegrations
    :: IntegrationProvider -> OsPath -> IO NativeProcessRuntime
newNativeProcessRuntimeWithIntegrations provider =
    newNativeProcessRuntimeWithOrganizationIntegrations provider Nothing

-- | Organization-local execution is a separate distribution opt-in; supplying
-- an ordinary local provider alone never enables it for gateway users.
newNativeProcessRuntimeWithOrganizationIntegrations
    :: IntegrationProvider -> Maybe OrganizationIntegrationProvider
    -> OsPath -> IO NativeProcessRuntime
newNativeProcessRuntimeWithOrganizationIntegrations provider organizationProvider root = mask \restore -> do
    integrationToolEnv <- restore (defaultToolEnv root)
    localIntegrations <- restore (newIntegrationSupervisor provider integrationToolEnv)
    -- Direct turns borrow the same local owner used by native account settings.
    -- Changing gateway identity must retire banking, not an ongoing mail OAuth flow.
    let borrowedLocalProvider _ =
            fmap (fmap (\runtime -> runtime { closeIntegrationRuntime = pure () }))
                (acquireIntegrationRuntime localIntegrations LocalIntegrationAuthority)
    integrations <-
        restore (newIntegrationSupervisorWithOrganizationProvider
            borrowedLocalProvider organizationProvider integrationToolEnv)
            `onException` closeIntegrationSupervisor localIntegrations
    core <- restore (NativeProcess.newNativeProcessRuntimeWithMcpHooks
        MCP.defaultMcpHostHooks
            { MCP.mcpHostCredentials = mcpConnectionCredentials
            , MCP.mcpHostServerInfo = observeMcpConnectionInfo
            }
        root) `onException` (closeIntegrationSupervisor integrations
            `finally` closeIntegrationSupervisor localIntegrations)
    unregister <- registerMcpConnectionRuntime (NativeProcess.restartNativeMcpRuntime core)
        `onException` (NativeProcess.closeNativeProcessRuntime core
            `finally` closeIntegrationSupervisor integrations
            `finally` closeIntegrationSupervisor localIntegrations)
    pure NativeProcessRuntime
        { nativeProcessCore = core
        , nativeIntegrationSupervisor = integrations
        , nativeLocalIntegrationSupervisor = localIntegrations
        , nativeUnregisterMcpConnections = unregister
        }

closeNativeProcessRuntime :: NativeProcessRuntime -> IO ()
closeNativeProcessRuntime runtime =
    runtime.nativeUnregisterMcpConnections
        `finally` NativeProcess.closeNativeProcessRuntime runtime.nativeProcessCore
        `finally`
            closeIntegrationSupervisor runtime.nativeIntegrationSupervisor
                `finally` closeIntegrationSupervisor runtime.nativeLocalIntegrationSupervisor

restartNativeMcpRuntime :: NativeProcessRuntime -> IO ()
restartNativeMcpRuntime runtime =
    NativeProcess.restartNativeMcpRuntime runtime.nativeProcessCore
        `finally` resetIntegrationSupervisor runtime.nativeIntegrationSupervisor

nativeProcessIntegrationSupervisor
    :: NativeProcessRuntime
    -> IntegrationSupervisor
nativeProcessIntegrationSupervisor = (.nativeIntegrationSupervisor)

-- | Native account administration is always device-local. It neither changes
-- the turn's authority nor acquires/exposes local tools to organization turns.
acquireNativeLocalIntegrationRuntime
    :: NativeProcessRuntime -> IO (Either Text IntegrationRuntime)
acquireNativeLocalIntegrationRuntime runtime =
    acquireIntegrationRuntime runtime.nativeLocalIntegrationSupervisor
        LocalIntegrationAuthority

-- | Execute one typed native turn without reconstructing command-line
-- arguments.
--
-- Approval behavior is explicit on the typed request and is carried through
-- native hooks rather than inferred from CLI flags.
runNativeTurn
    :: NativeProcessRuntime
    -> Handle
    -> NativeRunHooks
    -> NativeTurnRequest
    -> IO (Either Text ())
runNativeTurn runtime output hooks request =
    case nativeTurnOptions request of
        Left err -> pure (Left err)
        Right options ->
            runNativeOptions
                runtime
                output
                request.nativeTurnCwd
                hooks
                    { nativeInteractionMode =
                        request.nativeTurnInteractionMode
                    , nativeShellMode = request.nativeTurnShellMode
                    , nativeInitialTurnInputs =
                        Just
                            [ userMessageWithAttachments
                                initialPrompt
                                (map ImageAttachmentItem request.nativeTurnImages)
                            ]
                    }
                options
  where
    initialPrompt
        | Text.null (Text.strip request.nativeTurnPrompt)
        , not (null request.nativeTurnImages) = "Image attached."
        | otherwise = request.nativeTurnPrompt

-- | Lower a typed native request into the existing orchestration options.
--
-- This compatibility adapter never enables capabilities excluded from native
-- turns. Transport adapters can validate without CLI options using
-- 'validateNativeTurnRequest'.
nativeTurnOptions :: NativeTurnRequest -> Either Text CliOptions
nativeTurnOptions request = do
    validateNativeTurnRequest request
    clock <- traverse parseNativeMessageClock request.nativeTurnMessageClock
    pure defaultCliOptions
            { optProvider = request.nativeTurnProvider
            , optModel = request.nativeTurnModel
            , optCwd = Just request.nativeTurnCwd
            , optWorktree = False
            , optYolo = False
            , optNoYolo = True
            , optEffort = request.nativeTurnEffort
            , optPrompt = Just request.nativeTurnPrompt
            , optPromptFile = Nothing
            , optManagedTurnFile = Nothing
            , optResume = case request.nativeTurnSession of
                NativeNewSession -> Nothing
                NativeResumeSession sessionId -> Just sessionId
            , optSaveSession = True
            , optGhci = nativeGhciEnabled request.nativeTurnShellMode
            , optBash = nativeBashEnabled request.nativeTurnShellMode
            , optComputerUse = False
            , optScreenMode = ScreenMinimal
            , optMotionMode = MotionOff
            , optMessageClock = clock
            }

parseNativeMessageClock :: NativeMessageClock -> Either Text MessageClock
parseNativeMessageClock clock =
    parseMessageClock
        clock.nativeHourCycle
        clock.nativeTimeZoneName
        clock.nativeTimeZoneOffsetMinutes

runNativeAgent
    :: NativeProcessRuntime
    -> Handle
    -> OsPath
    -> NativeRunHooks
    -> [String]
    -> IO (Either Text ())
runNativeAgent runtime output cwd hooks args =
    case parseArgs args of
        Left err -> pure (Left (Text.pack err))
        Right (RunAgent options) ->
            runNativeOptions runtime output cwd hooks options
        Right _ -> pure (Left
            "native turn arguments did not select an agent")

runNativeOptions
    :: NativeProcessRuntime
    -> Handle
    -> OsPath
    -> NativeRunHooks
    -> CliOptions
    -> IO (Either Text ())
runNativeOptions runtime output cwd hooks options =
    runAgentWithRuntime
        AgentProcessRuntime
            { processMcpSupervisor =
                runtime.nativeProcessCore.nativeMcpSupervisor
            , processIntegrationSupervisor =
                runtime.nativeIntegrationSupervisor
            , processSessionThreads =
                runtime.nativeProcessCore.nativeSessionThreads
            , processToolResourceArbiter =
                runtime.nativeProcessCore.nativeToolResourceArbiter
            , processStartCleanup =
                runtime.nativeProcessCore.nativeStartCleanup
            , processMcpElicitation =
                runtime.nativeProcessCore.nativeMcpElicitation
            , processMcpRoots =
                runtime.nativeProcessCore.nativeMcpRoots
            , processMcpSampling =
                runtime.nativeProcessCore.nativeMcpSampling
            , processNetworkRecovery =
                networkRecovery runtime.nativeProcessCore.nativeNetworkRecovery
            }
        (nativeRunMode output cwd hooks)
        (applyNativeStartupPolicy hooks.nativeStartupPolicy cwd
            (shellOptions options)) >>= \case
            DevQuit -> pure (Right ())
            DevReload _ ->
                pure (Left
                    "native turn unexpectedly requested a reload")
  where
    shellOptions prepared =
        prepared
            { optGhci = nativeGhciEnabled hooks.nativeShellMode
            , optBash = nativeBashEnabled hooks.nativeShellMode
            }

-- | The only legacy-options translation of native startup permissions.
-- Apply after request/argument preparation so conflicting options cannot
-- relax the embedding's restrictions. Typed-turn invariants are enforced
-- independently by 'nativeTurnOptions'.
applyNativeStartupPolicy :: NativeStartupPolicy -> OsPath -> CliOptions -> CliOptions
applyNativeStartupPolicy policy cwd = restrictContext . restrictFacilities
  where
    restrictContext options = case policy.nativeContextSources of
        WorkspaceContextAllowed -> options
        SuppliedContextOnly -> options
            { optAgentsMd = False
            , optSkills = False
            }
    restrictFacilities options = case policy.nativeExecutionFacilities of
        HostStartupFacilities -> options
        TurnScopedFacilities -> options
            { optCwd = Just cwd
            , optWorktree = False
            , optYolo = False
            , optNoYolo = True
            , optPromptFile = Nothing
            , optManagedTurnFile = Nothing
            , optComputerUse = False
            , optCodeMode = CodeModeDisabled
            }

nativeGhciEnabled :: NativeShellMode -> Bool
nativeGhciEnabled = \case
    NativeShellGhci -> True
    NativeShellBoth -> True
    NativeShellNone -> False
    NativeShellBash -> False

nativeBashEnabled :: NativeShellMode -> Bool
nativeBashEnabled = \case
    NativeShellBash -> True
    NativeShellBoth -> True
    NativeShellNone -> False
    NativeShellGhci -> False
