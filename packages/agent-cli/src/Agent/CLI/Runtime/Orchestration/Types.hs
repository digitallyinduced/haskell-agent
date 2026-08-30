module Agent.CLI.Runtime.Orchestration.Types
    ( ActiveHttpAuth(..)
    , AccountSwitchRequest(..)
    , AgentProcessRuntime(..)
    , AgentRunMode(..)
    , foregroundRunMode
    , backgroundRunMode
    ) where

import Agent.CLI.AgentSessions ( SessionThreadManager )
import Agent.Error ( ApiError )
import Agent.Provider ( Credential, TokenProvider )
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
    }

data AgentRunMode = AgentRunMode
    { runStdout :: !Handle
    , runStderr :: !Handle
    , runInBackground :: !Bool
    , runCwdHint :: !(Maybe OsPath)
    }

foregroundRunMode :: AgentRunMode
foregroundRunMode = AgentRunMode
    { runStdout = stdout
    , runStderr = stderr
    , runInBackground = False
    , runCwdHint = Nothing
    }

backgroundRunMode :: Handle -> OsPath -> AgentRunMode
backgroundRunMode output cwd = AgentRunMode
    { runStdout = output
    , runStderr = output
    , runInBackground = True
    , runCwdHint = Just cwd
    }
