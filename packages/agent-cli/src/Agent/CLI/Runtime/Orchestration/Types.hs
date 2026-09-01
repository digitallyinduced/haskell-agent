module Agent.CLI.Runtime.Orchestration.Types
    ( ActiveHttpAuth(..)
    , AccountSwitchRequest(..)
    , AgentProcessRuntime(..)
    , AgentRunMode(..)
    , NativeInteractionMode(..)
    , NativeShellMode(..)
    , NativeRunHooks(..)
    , foregroundRunMode
    , backgroundRunMode
    , nativeRunMode
    ) where

import Agent.CLI.AgentSessions ( SessionThreadManager )
import Agent.CLI.AgentViewport ( AgentEntry )
import Agent.CLI.NetworkPath ( NetworkRecovery )
import Agent.CLI.Permission ( PermissionChoice )
import Agent.Error ( ApiError )
import Agent.Loop ( LoopEvent )
import Agent.Provider ( Credential, TokenProvider )
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
