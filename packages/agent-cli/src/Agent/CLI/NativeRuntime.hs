module Agent.CLI.NativeRuntime
    ( NativeProcessRuntime
    , NativeInteractionMode(..)
    , NativeShellMode(..)
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
    , CliOptions(..)
    , parseArgs
    )
import Agent.CLI.NetworkPath
    ( NetworkRecoveryMonitor
    , closeNetworkRecoveryMonitor
    , networkRecovery
    , newNetworkRecoveryMonitor
    )
import Agent.CLI.Runtime.Orchestration (runAgentWithRuntime)
import Agent.CLI.Runtime.Orchestration.Types
    ( AgentProcessRuntime(..)
    , NativeInteractionMode(..)
    , NativeShellMode(..)
    , NativeRunHooks(..)
    , nativeRunMode
    )
import Agent.CLI.Runtime.Types (DevResult(..), StartupFailure(..))
import qualified Agent.MCP as MCP
import Control.Concurrent.Async
    ( Async
    , async
    , cancel
    , waitCatch
    )
import Control.Concurrent.MVar
    ( newEmptyMVar
    , putMVar
    , takeMVar
    )
import Control.Exception.Safe (finally, mask_, onException)
import Control.Monad (void)
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    )
import Data.Text (Text)
import qualified Data.Text as Text
import System.IO (Handle)
import System.OsPath (OsPath)

data NativeProcessRuntime = NativeProcessRuntime
    { nativeMcpSupervisor :: !MCP.McpSupervisor
    , nativeSessionThreads :: !SessionThreadManager
    , nativeNetworkRecovery :: !NetworkRecoveryMonitor
    , nativeStartCleanup :: !(IO () -> IO ())
    , nativeMcpElicitation
        :: !(IORef (Maybe
            (MCP.McpElicitRequest -> IO MCP.McpElicitResult)))
    , nativeCleanupWorker :: !(Async ())
    }

newNativeProcessRuntime :: OsPath -> IO NativeProcessRuntime
newNativeProcessRuntime root = do
    elicitationRef <- newIORef Nothing
    cleanupStarted <- newIORef False
    cleanupRequest <- newEmptyMVar
    cleanupWorker <- async (takeMVar cleanupRequest >>= id)
    let closeCleanupWorker = do
            cancel cleanupWorker
            void (waitCatch cleanupWorker)
        startCleanup action = mask_ do
            shouldStart <- atomicModifyIORef'
                cleanupStarted
                (\started -> (True, not started))
            if shouldStart
                then putMVar cleanupRequest action
                else pure ()
    networkMonitor <-
        newNetworkRecoveryMonitor
            `onException` closeCleanupWorker
    mcpSupervisor <-
        MCP.newMcpSupervisorWith
            MCP.defaultMcpHostHooks
                { MCP.mcpHostElicit = readIORef elicitationRef }
            `onException`
                (closeNetworkRecoveryMonitor networkMonitor
                    `finally` closeCleanupWorker)
    sessionThreads <-
        newSessionThreadManager root
            `onException`
                (MCP.closeMcpSupervisor mcpSupervisor
                    `finally`
                        (closeNetworkRecoveryMonitor networkMonitor
                            `finally` closeCleanupWorker))
    pure NativeProcessRuntime
        { nativeMcpSupervisor = mcpSupervisor
        , nativeSessionThreads = sessionThreads
        , nativeNetworkRecovery = networkMonitor
        , nativeStartCleanup = startCleanup
        , nativeMcpElicitation = elicitationRef
        , nativeCleanupWorker = cleanupWorker
        }

closeNativeProcessRuntime :: NativeProcessRuntime -> IO ()
closeNativeProcessRuntime runtime =
    closeSessionThreadManager runtime.nativeSessionThreads
        `finally`
            (MCP.closeMcpSupervisor runtime.nativeMcpSupervisor
                `finally`
                    (closeNetworkRecoveryMonitor
                        runtime.nativeNetworkRecovery
                        `finally` do
                            cancel runtime.nativeCleanupWorker
                            void (waitCatch runtime.nativeCleanupWorker)))

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
                    , processStartCleanup = runtime.nativeStartCleanup
                    , processMcpElicitation = runtime.nativeMcpElicitation
                    , processNetworkRecovery =
                        networkRecovery runtime.nativeNetworkRecovery
                    }
                (nativeRunMode output cwd hooks)
                options
                    { optGhci = nativeGhciEnabled hooks.nativeShellMode
                    , optBash = nativeBashEnabled hooks.nativeShellMode
                    } >>= \case
                    DevQuit -> pure (Right ())
                    DevReload _ ->
                        pure (Left
                            "native turn unexpectedly requested a reload")
        Right _ -> pure (Left
            "native turn arguments did not select an agent")

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
