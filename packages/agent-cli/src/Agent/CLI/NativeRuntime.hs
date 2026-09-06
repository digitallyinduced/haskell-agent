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
    , StartupFailure(..)
    , closeNativeProcessRuntime
    , newNativeProcessRuntime
    , nativeTurnOptions
    , restartNativeMcpRuntime
    , runNativeAgent
    , runNativeTurn
    ) where

import Agent.CLI.NativeProcess
    ( NativeProcessRuntime
        ( nativeMcpSupervisor
        , nativeSessionThreads
        , nativeNetworkRecovery
        , nativeStartCleanup
        , nativeMcpElicitation
        )
    , closeNativeProcessRuntime
    , newNativeProcessRuntime
    , restartNativeMcpRuntime
    )
import Agent.Loop
    ( TurnAttachment(ImageAttachmentItem)
    , userMessageWithAttachments
    )
import Agent.CLI.Options
    ( Command(..)
    , CliOptions(..)
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
    ( NativeSessionTarget(..)
    , NativeTurnRequest(..)
    , validateNativeTurnRequest
    )
import Agent.TUI.Motion (MotionMode(..))
import Data.Text (Text)
import qualified Data.Text as Text
import System.IO (Handle)
import System.OsPath (OsPath)

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
    case hooks.nativePrepareOptions options of
        Left err -> pure (Left err)
        Right preparedOptions ->
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
                (shellOptions preparedOptions) >>= \case
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
