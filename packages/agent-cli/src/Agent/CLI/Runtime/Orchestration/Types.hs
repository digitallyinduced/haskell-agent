module Agent.CLI.Runtime.Orchestration.Types
    ( ActiveHttpAuth(..)
    , AgentProcessRuntime(..)
    , AgentRunMode(..)
    , NativeInteractionMode(..)
    , NativeDiscoveryContext(..)
    , NativeWorkspaceDiscovery(..)
    , NativeRunCapabilities(..)
    , NativeShellMode(..)
    , NativeRunHooks(..)
    , fullNativeRunCapabilities
    , nativeLoadsHostWorkspaceContext
    , nativePreparedDiscovery
    , foregroundRunMode
    , backgroundRunMode
    , nativeRunMode
    ) where

import Agent.CLI.AgentSessions ( SessionThreadManager )
import Agent.CLI.AgentViewport ( AgentEntry )
import Agent.CLI.Options ( CliOptions )
import Agent.CLI.Project ( ProjectSettings )
import Agent.Connectivity.NetworkPath ( NetworkRecovery )
import Agent.CLI.Permission ( PermissionChoice )
import Agent.Loop ( LoopEvent )
import Agent.Provider ( Credential, TokenProvider )
import Agent.Store.Postgres ( Store )
import Agent.ToolDispatch ( ToolCall )
import Agent.Tools.PlanMode ( PlanModeHooks )
import Agent.Tools.Types ( AppTool, AppToolGroup )
import Data.IORef ( IORef )
import Data.Text ( Text )
import System.IO ( Handle, stderr, stdout )
import System.OsPath ( OsPath )

import qualified Agent.MCP as MCP

data ActiveHttpAuth = ActiveHttpAuth
    { activeHttpGeneration :: !Int
    , activeHttpProvider :: !TokenProvider
    , activeHttpResolveLabel :: !(Credential -> IO Text)
    , activeHttpAccountId :: !Text
    }

data AgentProcessRuntime = AgentProcessRuntime
    { processMcpSupervisor :: !MCP.McpSupervisor
    , processSessionThreads :: !SessionThreadManager
    -- | Starts the process-scoped stale-resource cleanup exactly once. Its
    -- owner joins it when the process runtime closes.
    , processStartCleanup :: !(IO () -> IO ())
    , processMcpElicitation
        :: !(IORef (Maybe (MCP.McpElicitRequest -> IO MCP.McpElicitResult)))
    -- ^ Interactive elicitation UI installed by the active session, shared by
    -- every MCP fleet the supervisor starts.
    , processNetworkRecovery :: !(Maybe NetworkRecovery)
    }

data NativeInteractionMode
    = NativeAsk
    -- ^ Prompt before mutating tools.
    | NativePlan
    -- ^ Begin this turn with plan mode active.
    | NativeYolo
    -- ^ Auto-approve mutating tools.
    deriving (Eq, Show)

data NativeShellMode
    = NativeShellNone
    | NativeShellBash
    | NativeShellGhci
    | NativeShellBoth
    deriving (Eq, Show)

-- | Host-side discovery results supplied by an embedding that does not want
-- generic orchestration to inspect the turn workspace.
data NativeDiscoveryContext = NativeDiscoveryContext
    { nativeDiscoveryHome :: !OsPath
    , nativeDiscoveryProjectRoot :: !OsPath
    , nativeDiscoveryCatalogRoot :: !OsPath
    , nativeDiscoveryProjectSettings :: !ProjectSettings
    , nativeDiscoveryGitBranch :: !Text
    , nativeDiscoveryOperatingSystem :: !Text
    , nativeDiscoveryShell :: !Text
    }

-- | Choose whether orchestration performs normal host workspace discovery or
-- consumes a context prepared by the embedding. The sum type makes the
-- no-host-discovery case total: it cannot be requested without supplying the
-- replacement context.
data NativeWorkspaceDiscovery
    = DiscoverHostWorkspace
    | UsePreparedWorkspace !NativeDiscoveryContext

nativePreparedDiscovery
    :: NativeWorkspaceDiscovery
    -> Maybe NativeDiscoveryContext
nativePreparedDiscovery = \case
    DiscoverHostWorkspace -> Nothing
    UsePreparedWorkspace context -> Just context

nativeLoadsHostWorkspaceContext :: NativeWorkspaceDiscovery -> Bool
nativeLoadsHostWorkspaceContext = \case
    DiscoverHostWorkspace -> True
    UsePreparedWorkspace _ -> False

-- | Independent optional facilities available to a native run. These are
-- capabilities rather than an execution-mode identity: orchestration asks
-- only whether a concrete facility was supplied.
data NativeRunCapabilities = NativeRunCapabilities
    { nativeProviderFallback :: !Bool
    , nativeProviderHostedTools :: !Bool
    , nativeHostExtensions :: !Bool
    , nativeCollaboration :: !Bool
    , nativeProviderNativeTools :: !Bool
    }
    deriving (Eq, Show)

fullNativeRunCapabilities :: NativeRunCapabilities
fullNativeRunCapabilities = NativeRunCapabilities
    { nativeProviderFallback = True
    , nativeProviderHostedTools = True
    , nativeHostExtensions = True
    , nativeCollaboration = True
    , nativeProviderNativeTools = True
    }

data NativeRunHooks = NativeRunHooks
    { nativeOnLoopEvent :: !(LoopEvent -> IO ())
    , nativeOnSessionId :: !(Text -> IO ())
    , nativeRegisterCancel :: !(IO () -> IO ())
    , nativeRegisterAgentSnapshot :: !(IO [AgentEntry] -> IO ())
    , nativeRequestApproval :: !(ToolCall -> IO (Maybe PermissionChoice))
    , nativeRequestRootAccess :: !(OsPath -> IO Bool)
    , nativeToolGroups :: ![AppToolGroup]
    -- | Compose the final model-visible tool surface once, before the generic
    -- registry is built. An embedding may replace execution groups here.
    , nativeComposeTools :: !([AppToolGroup] -> [AppTool])
    , nativePlanHooks :: !PlanModeHooks
    , nativeInteractionMode :: !NativeInteractionMode
    , nativeShellMode :: !NativeShellMode
    -- | Optional home override while performing ordinary host discovery.
    -- Prepared discovery carries its required home in its context instead.
    , nativeHome :: !(Maybe OsPath)
    -- | A host-owned store borrowed for the duration of the turn. Native
    -- orchestration never closes a borrowed store.
    , nativeDatabaseStore :: !(Maybe Store)
    -- | Stable, non-secret namespace mixed into every derived custom-data
    -- scope. Multi-tenant callers use the tenant id here because PostgreSQL
    -- roles are cluster-global even when each tenant has its own database.
    , nativeDatabaseScopeNamespace :: !(Maybe Text)
    , nativeWorkspaceDiscovery :: !NativeWorkspaceDiscovery
    , nativeCapabilities :: !NativeRunCapabilities
    -- | Last-mile option policy owned by the embedding.
    , nativePrepareOptions :: !(CliOptions -> Either Text CliOptions)
    }

data AgentRunMode = AgentRunMode
    { runStdout :: !Handle
    , runStderr :: !Handle
    , runInBackground :: !Bool
    , runCwdHint :: !(Maybe OsPath)
    , runNativeHooks :: !(Maybe NativeRunHooks)
    }

foregroundRunMode :: AgentRunMode
foregroundRunMode = AgentRunMode
    { runStdout = stdout
    , runStderr = stderr
    , runInBackground = False
    , runCwdHint = Nothing
    , runNativeHooks = Nothing
    }

backgroundRunMode :: Handle -> OsPath -> AgentRunMode
backgroundRunMode output cwd = AgentRunMode
    { runStdout = output
    , runStderr = output
    , runInBackground = True
    , runCwdHint = Just cwd
    , runNativeHooks = Nothing
    }

nativeRunMode :: Handle -> OsPath -> NativeRunHooks -> AgentRunMode
nativeRunMode output cwd hooks = AgentRunMode
    { runStdout = output
    , runStderr = output
    , runInBackground = True
    , runCwdHint = Just cwd
    , runNativeHooks = Just hooks
    }
