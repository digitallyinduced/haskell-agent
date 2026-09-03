module Agent.CLI.Runtime.Orchestration.Types
    ( ActiveHttpAuth(..)
    , AccountSwitchRequest(..)
    , AgentProcessRuntime(..)
    , AgentRunMode(..)
    , NativeInteractionMode(..)
    , NativeIsolationMode(..)
    , NativeShellMode(..)
    , NativeRunHooks(..)
    , foregroundRunMode
    , backgroundRunMode
    , nativeRunMode
    ) where

import Agent.CLI.AgentSessions ( SessionThreadManager )
import Agent.CLI.AgentViewport ( AgentEntry )
import Agent.Connectivity.NetworkPath ( NetworkRecovery )
import Agent.CLI.Permission ( PermissionChoice )
import Agent.Error ( ApiError )
import Agent.Loop ( LoopEvent )
import Agent.Provider ( Credential, TokenProvider )
import Agent.Store.Postgres ( Store )
import Agent.ToolDispatch ( ToolCall )
import Agent.Tools.PlanMode ( PlanModeHooks )
import Agent.Tools.Types ( AppTool )
import Control.Concurrent.MVar ( MVar )
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

data AccountSwitchRequest
    = AccountSwitchRequest !Credential !(MVar (Either ApiError Text))

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

-- | Whether a native embedding shares the host execution boundary.
--
-- Sandboxed embeddings must route every model-callable tool explicitly. The
-- CLI remains unrestricted so existing local behavior is unchanged.
data NativeIsolationMode
    = NativeUnrestricted
    | NativeSandboxed
    deriving (Eq, Show)

data NativeRunHooks = NativeRunHooks
    { nativeOnLoopEvent :: !(LoopEvent -> IO ())
    , nativeOnSessionId :: !(Text -> IO ())
    , nativeRegisterCancel :: !(IO () -> IO ())
    , nativeRegisterAgentSnapshot :: !(IO [AgentEntry] -> IO ())
    , nativeRequestApproval :: !(ToolCall -> IO (Maybe PermissionChoice))
    , nativeRequestRootAccess :: !(OsPath -> IO Bool)
    , nativeTools :: ![AppTool]
    , nativePlanHooks :: !PlanModeHooks
    , nativeInteractionMode :: !NativeInteractionMode
    , nativeShellMode :: !NativeShellMode
    , nativeHome :: !(Maybe OsPath)
    -- | A host-owned store borrowed for the duration of the turn. Native
    -- orchestration never closes a borrowed store.
    , nativeDatabaseStore :: !(Maybe Store)
    -- | Stable, non-secret namespace mixed into every derived custom-data
    -- scope. Multi-tenant callers use the tenant id here because PostgreSQL
    -- roles are cluster-global even when each tenant has its own database.
    , nativeDatabaseScopeNamespace :: !(Maybe Text)
    , nativeIsolationMode :: !NativeIsolationMode
    -- | Route one fully classified tool to its execution boundary. Sandboxed
    -- embeddings reject unclassified tools and replace sandbox handlers with
    -- broker-backed handlers.
    , nativeRouteTool :: !(AppTool -> Either Text AppTool)
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
