module Agent.CLI.Runtime.Orchestration.Types
    ( ActiveHttpAuth(..)
    , AccountSwitchRequest(..)
    , AgentProcessRuntime(..)
    , AgentRunMode(..)
    , NativeRunHooks(..)
    , foregroundRunMode
    , backgroundRunMode
    , nativeRunMode
    ) where

import Agent.CLI.AgentSessions ( SessionThreadManager )
import Agent.CLI.AgentViewport ( AgentEntry )
import Agent.CLI.Permission ( PermissionChoice )
import Agent.CLI.RuntimeModel ( RuntimeResponsesModel )
import Agent.Error ( ApiError )
import Agent.Loop ( LoopEvent )
import Agent.Provider ( Credential, TokenProvider )
import Agent.ToolDispatch ( ToolCall )
import Agent.Tools.Types ( AppTool )
import Control.Concurrent.MVar ( MVar )
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
    -- | An ephemeral model endpoint supplied by a trusted native host. It is
    -- never read from or written to the user model catalog.
    , processRuntimeResponsesModel :: !(Maybe RuntimeResponsesModel)
    }

data NativeRunHooks = NativeRunHooks
    { nativeOnLoopEvent :: !(LoopEvent -> IO ())
    , nativeOnSessionId :: !(Text -> IO ())
    , nativeRegisterCancel :: !(IO () -> IO ())
    , nativeRegisterAgentSnapshot :: !(IO [AgentEntry] -> IO ())
    , nativeRequestApproval :: !(ToolCall -> IO (Maybe PermissionChoice))
    , nativeTools :: ![AppTool]
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
