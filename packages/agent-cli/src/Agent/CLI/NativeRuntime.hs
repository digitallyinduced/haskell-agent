module Agent.CLI.NativeRuntime
    ( NativeProcessRuntime
    , NativeInteractionMode(..)
    , NativeIsolationMode(..)
    , NativeShellMode(..)
    , NativeRunHooks(..)
    , NativeSessionTarget(..)
    , NativeTurnRequest(..)
    , StartupFailure(..)
    , closeNativeProcessRuntime
    , newNativeProcessRuntime
    , nativeTurnOptions
    , restartNativeMcpRuntime
    , runNativeAgent
    , runNativeTurn
    ) where

import Agent.CLI.AgentSessions
    ( SessionThreadManager
    , closeSessionThreadManager
    , newSessionThreadManager
    )
import Agent.CLI.Options
    ( Command(..)
    , CliOptions(..)
    , ScreenMode(..)
    , defaultCliOptions
    , parseArgs
    )
import Agent.Connectivity.NetworkPath
    ( NetworkRecoveryMonitor
    , closeNetworkRecoveryMonitor
    , networkRecovery
    , newNetworkRecoveryMonitor
    )
import Agent.CLI.Runtime.Orchestration (runAgentWithRuntime)
import Agent.CLI.Runtime.Orchestration.Types
    ( AgentProcessRuntime(..)
    , NativeInteractionMode(..)
    , NativeIsolationMode(..)
    , NativeShellMode(..)
    , NativeRunHooks(..)
    , nativeRunMode
    )
import Agent.CLI.Runtime.Types (DevResult(..), StartupFailure(..))
import Agent.Provider (Provider)
import Agent.ReasoningEffort (ReasoningEffort)
import Agent.TUI.Motion (MotionMode(..))
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

-- | Whether a native turn creates a durable session or resumes one.
--
-- Keeping this as a sum type prevents transport adapters from constructing
-- ambiguous combinations of @save@ and @resume@ flags.
data NativeSessionTarget
    = NativeNewSession
    | NativeResumeSession !Text
    deriving (Eq, Show)

-- | Typed, transport-neutral inputs for one native agent turn.
--
-- Native turns intentionally exclude CLI-only capabilities such as worktree
-- creation, computer use, prompt files, and implicit non-interactive
-- auto-approval. The supplied 'NativeRunHooks' remain responsible for all
-- interactive approval and plan-mode callbacks.
data NativeTurnRequest = NativeTurnRequest
    { nativeTurnPrompt :: !Text
    , nativeTurnSession :: !NativeSessionTarget
    , nativeTurnProvider :: !(Maybe Provider)
    , nativeTurnModel :: !(Maybe Text)
    , nativeTurnCwd :: !OsPath
    , nativeTurnEffort :: !(Maybe ReasoningEffort)
    , nativeTurnInteractionMode :: !NativeInteractionMode
    , nativeTurnShellMode :: !NativeShellMode
    }
    deriving (Eq, Show)

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

-- | Execute one typed native turn without reconstructing command-line
-- arguments.
--
-- Auto-approval is deliberately unavailable through this entry point. Native
-- HTTP and embedding transports must surface approval requests through hooks
-- instead of silently inheriting the CLI's non-interactive yolo behavior.
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
                    }
                options

-- | Lower a typed native request into the existing orchestration options.
--
-- This function is public so transport adapters can validate requests before
-- queue admission. It never enables capabilities excluded from native turns.
nativeTurnOptions :: NativeTurnRequest -> Either Text CliOptions
nativeTurnOptions request
    | request.nativeTurnInteractionMode == NativeYolo =
        Left "typed native turns do not support auto-approval"
    | NativeResumeSession sessionId <- request.nativeTurnSession
    , Text.null (Text.strip sessionId) =
        Left "native resume session id must not be empty"
    | otherwise =
        Right defaultCliOptions
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
            }

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
    case (hooks.nativeIsolationMode, hooks.nativeHome) of
        (NativeSandboxed, Nothing) ->
            pure (Left
                "sandboxed native turns require a host-controlled home")
        _ ->
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
                isolatedOptions >>= \case
                    DevQuit -> pure (Right ())
                    DevReload _ ->
                        pure (Left
                            "native turn unexpectedly requested a reload")
  where
    isolatedOptions =
        (case hooks.nativeIsolationMode of
            NativeUnrestricted -> options
            NativeSandboxed ->
                options
                    { optCwd = Just cwd
                    , optWorktree = False
                    , optYolo = False
                    , optNoYolo = True
                    , optPromptFile = Nothing
                    , optManagedTurnFile = Nothing
                    , optAgentsMd = False
                    , optSkills = False
                    , optComputerUse = False
                    , optCodeMode = False
                    })
            { optGhci = nativeGhciEnabled hooks.nativeShellMode
            , optBash = nativeBashEnabled hooks.nativeShellMode
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
