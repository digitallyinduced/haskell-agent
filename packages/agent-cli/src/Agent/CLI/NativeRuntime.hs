module Agent.CLI.NativeRuntime
    ( NativeProcessRuntime
    , NativeRunHooks(..)
    , StartupFailure(..)
    , closeNativeProcessRuntime
    , newNativeProcessRuntime
    , restartNativeMcpRuntime
    , runNativeAgent
    ) where

import Agent.CLI.AgentSessions
    ( SessionThreadManager
    , closeSessionThreadManager
    , newSessionThreadManager
    )
import Agent.CLI.Options
    ( Command(..)
    , parseArgs
    )
import Agent.CLI.Runtime.Orchestration (runAgentWithRuntime)
import Agent.CLI.Runtime.Orchestration.Types
    ( AgentProcessRuntime(..)
    , NativeRunHooks(..)
    , nativeRunMode
    )
import Agent.CLI.Runtime.Types (DevResult(..), StartupFailure(..))
import qualified Agent.MCP as MCP
import Control.Exception.Safe (finally, onException)
import Data.Text (Text)
import qualified Data.Text as Text
import System.IO (Handle)
import System.OsPath (OsPath)

data NativeProcessRuntime = NativeProcessRuntime
    { nativeMcpSupervisor :: !MCP.McpSupervisor
    , nativeSessionThreads :: !SessionThreadManager
    }

newNativeProcessRuntime :: OsPath -> IO NativeProcessRuntime
newNativeProcessRuntime root = do
    mcpSupervisor <- MCP.newMcpSupervisor
    sessionThreads <-
        newSessionThreadManager root
            `onException` MCP.closeMcpSupervisor mcpSupervisor
    pure NativeProcessRuntime
        { nativeMcpSupervisor = mcpSupervisor
        , nativeSessionThreads = sessionThreads
        }

closeNativeProcessRuntime :: NativeProcessRuntime -> IO ()
closeNativeProcessRuntime runtime =
    closeSessionThreadManager runtime.nativeSessionThreads
        `finally` MCP.closeMcpSupervisor runtime.nativeMcpSupervisor

restartNativeMcpRuntime :: NativeProcessRuntime -> IO ()
restartNativeMcpRuntime runtime =
    MCP.restartMcpSupervisor runtime.nativeMcpSupervisor

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
            runAgentWithRuntime
                AgentProcessRuntime
                    { processMcpSupervisor = runtime.nativeMcpSupervisor
                    , processSessionThreads = runtime.nativeSessionThreads
                    }
                (nativeRunMode output cwd hooks)
                options >>= \case
                    DevQuit -> pure (Right ())
                    DevReload _ ->
                        pure (Left
                            "native turn unexpectedly requested a reload")
        Right _ -> pure (Left
            "native turn arguments did not select an agent")
