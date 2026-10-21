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
    , newNativeProcessRuntimeWithCredentialRuntime
    , nativeProcessIntegrationSupervisor
    , acquireNativeLocalIntegrationRuntime
    , nativeTurnOptions
    , applyNativeStartupPolicy
    , applyNativeInteractionMode
    , restartNativeMcpRuntime
    , runNativeAgent
    , runNativeTurn
    ) where

import Agent.Runtime.Process
    ( NativeProcessRuntime
    , newNativeProcessRuntime
    , newNativeProcessRuntimeWithIntegrations
    , newNativeProcessRuntimeWithOrganizationIntegrations
    , newNativeProcessRuntimeWithCredentialRuntime
    , closeNativeProcessRuntime
    , restartNativeMcpRuntime
    , nativeProcessIntegrationSupervisor
    , acquireNativeLocalIntegrationRuntime
    , nativeAgentProcessRuntime
    )
import Agent.CLI.Session.Lifecycle (exitFailedTurn)
import Agent.CLI.Session.Runner.Execution (applyNativeInteractionMode)
import Agent.Runtime.StartupPolicy
    ( NativeStartupPolicy(..)
    , NativeContextSources(..)
    , NativeExecutionFacilities(..)
    )
import Agent.Loop
    ( TurnAttachment(FileAttachmentItem, ImageAttachmentItem)
    , userMessageWithAttachments
    )
import Agent.CLI.Options
    ( Command(..)
    , CliOptions(..)
    , Override(..)
    , ResumeTarget(..)
    , CodeModeOption(..)
    , ScreenMode(..)
    , defaultCliOptions
    , parseArgs
    )
import Agent.CLI.Runtime.Orchestration (runAgentWithRuntime)
import Agent.CLI.Runtime.Orchestration.Types
    ( NativeInteractionMode(..)
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
import Data.Text (Text)
import qualified Data.Text as Text
import System.IO (Handle)
import System.OsPath (OsPath)

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
                                (map ImageAttachmentItem request.nativeTurnImages
                                    <> map FileAttachmentItem request.nativeTurnFiles)
                            ]
                    }
                options
  where
    initialPrompt
        | Text.null (Text.strip request.nativeTurnPrompt)
        , not (null request.nativeTurnImages) = "Image attached."
        | Text.null (Text.strip request.nativeTurnPrompt)
        , not (null request.nativeTurnFiles) = "File attached."
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
            , optYolo = Explicit False
            , optEffort = request.nativeTurnEffort
            , optPrompt = Just request.nativeTurnPrompt
            , optPromptFile = Nothing
            , optManagedTurnFile = Nothing
            , optResume = case request.nativeTurnSession of
                NativeNewSession -> Nothing
                NativeResumeSession sessionId -> Just (ResumeSession sessionId)
            , optSaveSession = True
            , optGhci = nativeGhciEnabled request.nativeTurnShellMode
            , optBash = nativeBashEnabled request.nativeTurnShellMode
            , optComputerUse = Explicit False
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
        (nativeAgentProcessRuntime runtime)
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
            , optYolo = Explicit False
            , optPromptFile = Nothing
            , optManagedTurnFile = Nothing
            , optComputerUse = Explicit False
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
