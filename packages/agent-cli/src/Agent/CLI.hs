-- | Command-line entry point: one-shot @-p@ or an interactive REPL.
module Agent.CLI
    ( DevResult(..)
    , afterDev
    , applyReplMode
    , cycleReplInteraction
    , devArgs
    , devMain
    , devMainResume
    , formatReplStatusLine
    , formatRepositoryPath
    , formatStartupTimings
    , formatTokenUsage
    , run
    , withRestoredCurrentDirectory
    ) where

import Agent.CLI.Artifact (fencedCodeBlock, lastDiffBlock)
import Agent.CLI.Auth
    ( LoadedAuth(..)
    , authErrorNeedsOnboarding
    , loadAuth
    , loadAuthForAccount
    , preferredOpenAiTokenProvider
    , probeLoadedAuthCredential
    )
import Agent.CLI.AgentViewport
    ( AgentEntry(..)
    , AgentStep
    , AgentTarget(..)
    , AgentViewportEnv(..)
    , agentStepsForStatus
    , formatAgentStatus
    , pickAgentViewport
    , renderAgentViewportPanelFor
    , responseItemPreviewLines
    , responseItemStepPreviews
    )
import Agent.CLI.SessionTitle
    ( SessionTitleResult(..)
    , invalidateSessionTitles
    , requestSessionTitle
    , waitForSessionTitleResults
    , withSessionTitleManager
    )
import Agent.CLI.AgentSessions
    ( AgentSessionToolsEnv(..)
    , acquireSessionLock
    , agentSessionTools
    , closeSessionProcessManager
    , launchSessionTurn
    , newSessionProcessManager
    , releaseSessionLock
    , sessionProcessStatus
    )
import Agent.CLI.Approval
    ( approveToolDecision
    , approveToolDecisionWith
    , toggleAlwaysApprove
    )
import Agent.CLI.Btw
    ( BtwBackendFactory
    , formatBtwError
    , runBtwWithCancel
    )
import Agent.CLI.CancelWatch (withEscCancel, withStdinPaused)
import Agent.CLI.Clipboard
    ( ClipboardContent(..)
    , formatImageSize
    , loadImagesFromPastedText
    , readClipboard
    , readClipboardImages
    )
import Agent.CLI.Command
import Agent.CLI.Compaction
    ( CompactOutcome(..)
    , OpenAiCompactionSender
    , autoCompactOpenAiBackendWithSender
    , installCompactOutcome
    , runProviderCompactWith
    )
import Agent.CLI.Connectivity (withConnectionRecovery)
import Agent.CLI.Error
    ( formatApiErrorAt
    , formatApiErrorInlineAt
    )
import Agent.CLI.ImagePreview
    ( detectImagePreviewProtocol
    , previewColumnsFor
    , previewRowsFor
    , renderImagePreview
    )
import Agent.CLI.Input
    ( ReplLine(..)
    , formatPasteChip
    , readReplLineWithSkills
    , submissionPromptText
    )
import Agent.CLI.ReplMode
    ( ReplMode(..)
    , replModeLabel
    , replModeFromState
    )
import Agent.CLI.Interrupt
    ( CtrlCDecision(..)
    , InterruptState
    , isWrappedUserInterrupt
    , newInterruptState
    , noteFullscreenCtrlC
    , withCtrlCHandler
    , withTurnCancel
    )
import Agent.CLI.Login
    ( AccountBilling(..)
    , AccountUsage(..)
    , LoginAccount(..)
    , UsageState(..)
    , UsageWindow(..)
    , connectProviderAccount
    , discoverSelectableLoginAccounts
    , loginAccountSelectionId
    , refreshLoginAccount
    , runLoginManager
    )
import Agent.CLI.ModelPicker (pickModel)
import Agent.CLI.Models
    ( ModelOption(..)
    , PickerState(..)
    , initialPickerState
    )
import Agent.CLI.Notification
    ( AttentionRequest(InputRequested)
    , notifyAttention
    )
import Agent.CLI.Options
import Agent.CLI.PendingInputs (withPendingInputs)
import Agent.CLI.Resume
    ( ResumeEntry(..)
    , initialResumeBrowser
    , loadResumeEntry
    , pickResumeSession
    , resumeEntryFromMeta
    )
import Agent.CLI.Plan (cliPlanHooks)
import Agent.CLI.Progress
    ( osc9ProgressIndeterminate
    , osc9ProgressRemove
    , wrapOscForTmux
    )
import Agent.CLI.Project
    ( ProjectSettings(..)
    , loadProjectSettings
    , projectModelFor
    , projectModelProvider
    , resolveProjectRoot
    , saveProjectModel
    )
import Agent.CLI.Prompt (defaultModelFor, systemPrompt)
import Agent.CLI.Request (requestParams)
import Agent.CLI.ProviderFallback
    ( allowsAutomaticBillingFallback
    , automaticCooldownRetryDelay
    , fallbackCandidates
    , isProviderUnavailable
    )
import Agent.CLI.ProviderAvailability (probeLoadedAvailability)
import Agent.CLI.ProviderTransition
    ( PendingTurn(..)
    , ProviderTransition(..)
    , TransitionCause(..)
    , TurnResult(..)
    , applyProviderTransition
    , providerTransitionDraft
    , setPendingExitAfter
    )
import Agent.CLI.Render
    ( RenderConfig(..)
    , clearThinking
    , emptyMarkdownStreamState
    , putTextLn
    , renderAssistantText
    , renderEvent
    )
import Agent.CLI.Session
import Agent.CLI.SessionEnv (SessionEnv(..))
import Agent.CLI.Skills
    ( formatSkillsListing
    , installSkillCatalog
    , loadSkillsCatalog
    , reservedSlashNames
    , skillInvocationCommand
    )
import Agent.CLI.Status
    ( applyReplMode
    , cycleReplInteraction
    , formatReplStatusLine
    , formatTokenUsage
    )
import Agent.CLI.Subagents.Runtime
    ( SubagentRuntime(..)
    , SubagentSession(..)
    , SubagentStoreRoot
    , flushAllSubagentSnapshots
    , freshOpenAiBackend
    , persistSubagentSnapshotWithStatus
    , prepareCollaborationSpawn
    , restoreAgentFromDisk
    , runCodexSubagent
    , runHttpSubagent
    )
import Agent.CLI.Style
    ( beginBackground
    , cliWindowTitle
    , endBackground
    , glyphOk
    , glyphSession
    , glyphWarn
    , roleError
    , roleMuted
    , rolePrompt
    , roleSuccess
    , roleWarn
    , setCliWindowTitle
    , userBackground
    )
import Agent.CLI.Terminal
    ( TerminalCapabilities(..)
    , copyTerminalClipboard
    , detectTerminalCapabilities
    , emitTerminalSequence
    , formatTerminalCapabilities
    , osc133PromptEnd
    , osc133PromptStart
    , reportTerminalCwd
    , resolveColor
    , withSynchronizedOutput
    )
import Agent.CLI.Tools (requireToolRegistry, schemasFromAppTools)
import Agent.CLI.TUI.App
    ( FullscreenInputBuffer
    , FullscreenRuntime
    , emitUiEvent
    , hasQueuedFullscreenInput
    , newFullscreenInputBuffer
    , newFullscreenRuntime
    , queuedFullscreenInputDisplays
    , readFullscreenLine
    , readFullscreenLineOr
    , requestFullscreenPermission
    , requestFullscreenChoice
    , requestFullscreenChoiceWithBody
    , requestFullscreenOnboarding
    , requestFullscreenResume
    , requestFullscreenText
    , runFullscreen
    , setFullscreenSessionActions
    , setFullscreenImagePreviews
    , setFullscreenWindowTitle
    , withFullscreenSuspended
    )
import qualified Agent.CLI.TUI.Bridge as TuiBridge
import Agent.TUI.Model
    ( PromptState(..)
    , UiEvent(..)
    , UiState(..)
    , infoNotice
    , progressNotice
    , successNotice
    , warningNotice
    , initialUiState
    , reduceUi
    )
import Agent.TUI.Motion (nativeProgressAnimationEnabled)
import Agent.CLI.Turn (applyPendingSessionTitles, runOneTurn)
import Agent.CLI.Usage
    ( AccountUsageLine(..)
    , formatDuration
    , formatUsageReport
    , formatUsageSummary
    )
import Agent.CLI.Worktree
    ( createWorktree
    , isUnderWorktreeRoot
    , removeWorktree
    , worktreeRoot
    )
import Agent.Cancel (requestCancel, resetCancel, waitCancel)
import Agent.Loop
import Agent.Error (ApiError(..))
import Agent.ProjectInstructions
    ( DiscoverOptions(..)
    , defaultDiscoverOptions
    , discoverProjectInstructions
    , formatAgentsMdForProvider
    , globalAgentsHomeDir
    , loadedInstructionFiles
    )
import Agent.Skills
    ( Skill(..)
    , SkillCatalog(..)
    , SkillInvocation(..)
    , SkillWarning(..)
    , formatSkillActivation
    , resolveSkillInvocation
    , resolveSkillMentions
    )
import Agent.OpenAI.Compaction
    ( clearSessionUserText
    , compactSessionUserText
    , hasCompactionCheckpoint
    , isTranscriptResetTurn
    , newSessionUserText
    )
import qualified Agent.OpenAI.Auth as OpenAI
import Agent.OpenAI.LoopBackend
    ( openAiAuxiliaryResponseSenderReconnecting
    , openAiBackendWith
    , openAiResponseSenderReconnecting
    )
import Agent.Responses.Types
import Agent.OpenAI.Usage (fetchUsage)
import Agent.OpenAI.WebSocketClient
    ( CodexAuthFailed(..)
    , CodexConn
    , closeCodexConn
    , withCodexWsCredential
    , withCodexWsWithProvider
    )
import Agent.Provider
    ( AccountFailure(..)
    , BillingMode(..)
    , Credential(..)
    , FailedCredential(..)
    , Provider(..)
    , TokenProvider
    , getNextToken
    , providerSlug
    , tokenProvider
    , tokenProviderBillingMode
    )
import qualified Agent.Provider as Provider
import Agent.Subagents
    ( RootTurnId
    , SubagentId(..)
    , SubagentStatus(..)
    , abortRootTurn
    , beginRootTurn
    , closeSubagentRegistry
    , resetSubagentRegistry
    , defaultSubagentConfig
    , formatCompletionNotice
    , interruptActiveSubagents
    , listAgents
    , newSubagentRegistry
    , setSubagentOnComplete
    , setSubagentRunner
    )
import Agent.Tools (CodingTools(..), codingToolsForWithTypes)
import Agent.Subagents.TaskPath (taskPathRoot, taskPathText)
import Agent.Tools.MultiAgents
    ( MultiAgentContext(..)
    , SubagentWorktree(..)
    )
import Agent.Tools.PlanMode
    ( PlanDecision(..)
    , PlanModeEnv(..)
    , PlanModeHooks(..)
    , PlanModeState(..)
    , activatePlanMode
    , deactivatePlanMode
    , planFilePath
    )
import Agent.Tools.Types
    ( AppTool
    , ToolEnv(..)
    , defaultToolEnv
    )
import Agent.OpenRouter.LoopBackend (openRouterBackend)
import qualified Agent.OpenRouter.Options as OpenRouter
import Agent.OsPath (fromText, toText, unsafeToFilePath)
import Agent.XAI.LoopBackend (xaiBackend)
import qualified Agent.XAI.Options as XAI
import Control.Concurrent.Async (link, mapConcurrently, waitSTM, withAsync)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar_
    , newEmptyMVar
    , newMVar
    , putMVar
    , readMVar
    , takeMVar
    , tryPutMVar
    , withMVar
    )
import Control.Concurrent.STM (STM, retry)
import Control.Exception (AsyncException(UserInterrupt))
import Control.Exception.Safe
    ( Exception
    , SomeException
    , catchAny
    , catchAsync
    , finally
    , mask_
    , throwIO
    , try
    )
import Control.Monad (forM_, when)
import qualified Data.ByteString as BS
import Data.IORef
import Data.List (elemIndex, findIndex, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Set as Set
import Text.Printf (printf)
import Data.Time.Clock
    ( NominalDiffTime
    , UTCTime
    , diffUTCTime
    , getCurrentTime
    , utctDay
    )
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory.OsPath
    ( getCurrentDirectory
    , getHomeDirectory
    , makeAbsolute
    , setCurrentDirectory
    )
import System.Environment (getArgs, getProgName, lookupEnv)
import System.OsPath (OsPath, decodeFS, unsafeEncodeUtf, (</>), takeDirectory, takeFileName)
import System.Console.ANSI (getTerminalSize)
import System.Console.ANSI.Codes (clearFromCursorToLineEndCode)
import System.Exit (ExitCode(..), die, exitFailure)
import System.IO (Handle, hFlush, hIsTerminalDevice, stderr, stdin, stdout)
import System.Mem.StableName (StableName, makeStableName)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)

-- | How the GHCi-driven agent REPL finished.
data DevResult
    = DevQuit
    | DevReload Text
    deriving (Eq, Show)

data RunResult
    = RunQuit
    | RunReload Text
    | RunSwitchProvider ProviderTransition
    | RunProviderStartFailed ApiError
    | RunResumeSession Text
      -- ^ Persisted session id. Consumed after the current provider-specific
      -- backend shuts down before starting the selected session.

data PreparedAgent = PreparedAgent
    { preparedFullscreen :: !(Maybe FullscreenRuntime)
    , preparedRun :: !(IO RunResult)
    }

data PendingTurnPresentation
    = SubmitPendingTurn
    | RestartPendingTurn
    | ContinuePendingTurn

data AccountPickerOption
    = AccountPickerAccount !Text !Text !Text !Text
    | AccountPickerConnect

data ActiveHttpAuth = ActiveHttpAuth
    { activeHttpGeneration :: !Int
    , activeHttpProvider :: !TokenProvider
    , activeHttpResolveLabel :: !(Credential -> IO Text)
    , activeHttpAccountId :: !Text
    }

data OpenAiPersistentConnection
    = OpenAiPersistentConnection !Credential !(IORef Bool) !CodexConn

data AccountSwitchRequest
    = AccountSwitchRequest !Credential !(MVar (Either ApiError Text))

data AgentStepCache = AgentStepCache
    { cachedTranscript :: !(StableName [ResponseItem])
    , cachedVariant :: !(Maybe SubagentStatus)
    , cachedSteps :: ![AgentStep]
    }

data StartupRuntime = StartupRuntime
    { startupToolEnv :: !ToolEnv
    , startupInterrupt :: !InterruptState
    , startupEscPaused :: !(IORef Bool)
    , startupUiRuntimeRef :: !(IORef (Maybe FullscreenRuntime))
    , startupFullscreen :: !(Maybe FullscreenRuntime)
    , startupTerminal :: !TerminalCapabilities
    , startupUseColor :: !Bool
    , startupStderrTty :: !Bool
    , startupStdinTty :: !Bool
    , startupStdoutTty :: !Bool
    , startupFullscreenReused :: !Bool
    , startupAgentSnapshot :: !(IORef (IO (AgentTarget, [AgentEntry])))
    , startupAgentSelect :: !(IORef (AgentTarget -> IO ()))
    , startupRestartEffort :: !(IORef (Text -> IO ()))
    , startupStartedAt :: !UTCTime
    , startupTimings :: !(IORef [(Text, NominalDiffTime)])
    , startupSyntaxLoadDuration :: !(IORef (Maybe NominalDiffTime))
    }

newtype StartupFailure = StartupFailure String
    deriving (Show)

instance Exception StartupFailure

data StartupCancelled = StartupCancelled
    deriving (Show)

instance Exception StartupCancelled

-- | GHCi @:cmd@ helper: on 'DevReload', reload modules and resume that exact
-- session. Keeping the id in the generated GHCi command avoids a shared
-- cross-process resume pointer when several development REPLs are open.
afterDev :: DevResult -> IO String
afterDev = \case
    DevQuit -> pure ""
    DevReload sessionId -> pure $ unlines
        [ ":reload"
        , ":module +Agent.CLI"
        , ":cmd afterDev =<< devMainResume (Just "
            <> show (Text.unpack sessionId)
            <> ")"
        ]

-- | Arguments used by the development @repl@ launcher.
--
-- Fresh sessions use OpenAI's frontier model in yolo mode. Reloaded sessions
-- keep their persisted provider and model while reapplying the yolo default.
devArgs :: Maybe Text -> Bool -> [String]
devArgs resumeId underWorktree = case resumeId of
    Just sessionId ->
        [ "--yolo"
        , "--resume", Text.unpack sessionId
        ]
    Nothing ->
        [ "--provider", "openai"
        , "--model", "gpt-5.6-sol"
        , "--yolo"
        ]
            <> ["--worktree" | not underWorktree]

-- | Start a fresh agent from GHCi (@repl@).
devMain :: IO DevResult
devMain = devMainResume Nothing

-- | Start or resume the GHCi-driven agent. 'afterDev' embeds the session id in
-- the next @:cmd@ invocation, so concurrent REPLs cannot consume each other's
-- reload state.
devMainResume :: Maybe Text -> IO DevResult
devMainResume resumeId = do
    home <- getHomeDirectory
    underWorktree <- case resumeId of
        Just _ -> pure True
        Nothing -> do
            cwd <- makeAbsolute =<< getCurrentDirectory
            root <- makeAbsolute (worktreeRoot home)
            pure (isUnderWorktreeRoot root cwd)
    let args = devArgs resumeId underWorktree
    case parseArgs args of
        Left err -> die err
        Right ShowHelp -> putStr usage >> pure DevQuit
        Right ShowVersion -> putStrLn "agent-cli 0.1.0.0" >> pure DevQuit
        Right Login -> do
            color <- resolveColor stderr
            runLoginManager color
            pure DevQuit
        Right ListSessions -> runListSessions >> pure DevQuit
        Right (ShowSession sessionId) -> runShowSession sessionId >> pure DevQuit
        Right (RunAgent options) -> do
            result <- runAgentWithRestarts options
            case result of
                DevQuit -> pure DevQuit
                DevReload sessionId -> pure (DevReload sessionId)

run :: IO ()
run = do
    args <- getArgs
    case parseArgs args of
        Left err -> die err
        Right ShowHelp -> putStr usage
        Right ShowVersion -> putStrLn "agent-cli 0.1.0.0"
        Right Login -> do
            color <- resolveColor stderr
            runLoginManager color
        Right ListSessions -> runListSessions
        Right (ShowSession sessionId) -> runShowSession sessionId
        Right (RunAgent options) -> do
            result <- runAgentWithRestarts options
            case result of
                DevQuit -> pure ()
                DevReload _ ->
                    die ":reload is only available under `repl` (nix develop)"

-- | Tear down and rebuild provider-specific auth, tools, prompt, and transport.
-- Automatic transitions carry the exact failed turn in memory and commit
-- persisted provider metadata only after the replacement backend succeeds.
runAgentWithRestarts :: CliOptions -> IO DevResult
runAgentWithRestarts options = do
    fullscreenInputs <- newFullscreenInputBuffer
    withRestoredCurrentDirectory (go fullscreenInputs options Nothing)
  where
    go fullscreenInputs current transition =
        runAgent fullscreenInputs current transition >>= \case
            RunResumeSession sessionId ->
                go fullscreenInputs
                    current
                        { optProvider = Nothing
                        , optModel = Nothing
                        , optCwd = Nothing
                        , optWorktree = False
                        , optEffort = Nothing
                        , optPrompt = Nothing
                        , optPromptFile = Nothing
                        , optResume = Just sessionId
                        }
                    Nothing
            RunSwitchProvider next ->
                go fullscreenInputs
                    (applyProviderTransition current next)
                    (Just next)
            RunProviderStartFailed apiError ->
                case transition of
                    Just failed
                        | failed.transitionCause == AutomaticFallback ->
                            continueAutomaticFallback
                                Nothing failed apiError >>= \case
                                Just next ->
                                    go fullscreenInputs
                                        (applyProviderTransition current next)
                                        (Just next)
                                Nothing -> do
                                    reportProviderUnavailable Nothing apiError
                                    pure DevQuit
                    _ -> pure DevQuit
            RunQuit -> pure DevQuit
            RunReload sessionId -> pure (DevReload sessionId)

-- | Restore the process cwd after an action succeeds or throws. Cabal gives
-- GHCi relative source paths, so returning from an agent session in its cwd
-- would make the following @:reload@ lose local modules.
withRestoredCurrentDirectory :: IO a -> IO a
withRestoredCurrentDirectory action = do
    originalCwd <- getCurrentDirectory
    action `finally` setCurrentDirectory originalCwd

runListSessions :: IO ()
runListSessions = do
    home <- getHomeDirectory
    sessions <- listSessions (sessionsRoot home)
    if null sessions
        then putStrLn "No sessions in ~/.haskell-agent/sessions"
        else mapM_ printSessionSummary sessions

runShowSession :: Text -> IO ()
runShowSession sessionId = do
    home <- getHomeDirectory
    loadSession (sessionsRoot home) sessionId >>= \case
        Left err -> die (Text.unpack err)
        Right (meta, turns) -> do
            printSessionSummary meta
            putStrLn ""
            if null turns
                then putStrLn "(empty transcript)"
                else mapM_ printTurn turns

printSessionSummary :: SessionMeta -> IO ()
printSessionSummary meta =
    putStrLn $ Text.unpack $ Text.intercalate "  "
        [ meta.metaId
        , Text.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M" meta.metaUpdatedAt)
        , providerSlug meta.metaProvider
        , meta.metaModel
        , meta.metaTitle
        ]

printTurn :: SessionTurn -> IO ()
printTurn turn = do
    Text.putStrLn ("user> " <> turn.turnUserText)
    case turn.turnAssistantText of
        Just text | not (Text.null (Text.strip text)) ->
            Text.putStrLn ("assistant> " <> text)
        _ -> pure ()
    case turn.turnError of
        Just err | not (Text.null (Text.strip err)) ->
            Text.putStrLn ("error> " <> err)
        _ -> pure ()
    putStrLn ""

setStartupNotice :: Maybe FullscreenRuntime -> Text -> IO ()
setStartupNotice fullscreen message =
    case fullscreen of
        Nothing -> pure ()
        Just runtime ->
            emitUiEvent runtime
                (UiSetNotice (Just (progressNotice message)))

recordStartupTiming
    :: UTCTime
    -> IORef [(Text, NominalDiffTime)]
    -> Text
    -> IO ()
recordStartupTiming startedAt timingsRef label = do
    elapsed <- (`diffUTCTime` startedAt) <$> getCurrentTime
    atomicModifyIORef' timingsRef \timings ->
        (timings <> [(label, elapsed)], ())

markStartupStage :: StartupRuntime -> Text -> IO ()
markStartupStage startup label = do
    recordStartupTiming startup.startupStartedAt startup.startupTimings label
    setStartupNotice startup.startupFullscreen label

finishStartup :: StartupRuntime -> IO ()
finishStartup startup = do
    recordStartupTiming startup.startupStartedAt startup.startupTimings "ready"
    case startup.startupFullscreen of
        Nothing -> pure ()
        Just runtime ->
            emitUiEvent runtime (UiSetNotice Nothing)
    lookupEnv "HASKELL_AGENT_STARTUP_TIMING" >>= \case
        Just "1" -> do
            timings <- readIORef startup.startupTimings
            syntaxLoadDuration <-
                readIORef startup.startupSyntaxLoadDuration
            let message =
                    formatStartupTimings timings
                        <> maybe
                            ""
                            (\duration ->
                                " · syntax highlighting "
                                    <> formatStartupDuration duration)
                            syntaxLoadDuration
            case startup.startupFullscreen of
                Nothing -> putTextLn stderr message
                Just runtime -> emitUiEvent runtime (UiSystemMessage message)
        _ -> pure ()

startupDie :: StartupRuntime -> String -> IO a
startupDie startup message =
    case startup.startupFullscreen of
        Nothing -> die message
        Just _ -> throwIO (StartupFailure message)

formatStartupTimings :: [(Text, NominalDiffTime)] -> Text
formatStartupTimings timings =
    "startup: "
        <> Text.intercalate " · "
            [ label <> " " <> formatStartupDuration elapsed
            | (label, elapsed) <- sortOn snd timings
            ]

formatStartupDuration :: NominalDiffTime -> Text
formatStartupDuration elapsed
    | elapsed < 1 =
        Text.pack (show (round (elapsed * 1000) :: Int)) <> "ms"
    | otherwise =
        Text.pack (printf "%.2fs" (realToFrac elapsed :: Double))

setStartupRepository
    :: Maybe FullscreenRuntime
    -> OsPath
    -> Text
    -> OsPath
    -> IO ()
setStartupRepository fullscreen home branch cwd =
    case fullscreen of
        Nothing -> pure ()
        Just runtime ->
            emitUiEvent runtime $
                UiSetRepository
                    branch
                    (formatRepositoryPath home cwd)

formatRepositoryPath :: OsPath -> OsPath -> Text
formatRepositoryPath home cwd
    | cwdText == homeText = "~"
    | homePrefix `Text.isPrefixOf` cwdText =
        "~/" <> Text.drop (Text.length homePrefix) cwdText
    | otherwise = cwdText
  where
    homeText = Text.dropWhileEnd (== '/') (toText home)
    homePrefix = homeText <> "/"
    cwdText = toText cwd

runAgent
    :: FullscreenInputBuffer
    -> CliOptions
    -> Maybe ProviderTransition
    -> IO RunResult
runAgent fullscreenInputs options transition = do
    prepared <-
        prepareAgentIteration fullscreenInputs Nothing options transition
    let runPrepared = case prepared.preparedFullscreen of
            Nothing -> prepared.preparedRun
            Just runtime ->
                runFullscreen runtime $
                    runFullscreenRestartLoop
                        fullscreenInputs
                        runtime
                        options
                        transition
                        prepared.preparedRun
    outcome <- try @_ @StartupCancelled (try @_ @StartupFailure runPrepared)
    result <- case outcome of
        Left StartupCancelled -> pure RunQuit
        Right startupOutcome ->
            either (\(StartupFailure message) -> die message) pure startupOutcome
    case (prepared.preparedFullscreen, result) of
        -- The retained screen has been restored before this persistent final
        -- diagnostic is printed.
        (Just _, RunProviderStartFailed apiError) -> do
            reportProviderUnavailable Nothing apiError
            pure RunQuit
        _ -> pure result

-- | Prepare one provider-specific backend. The outer Brick worker loops over
-- these prepared actions while reusing @activeFullscreen@, so Vty stays in the
-- alternate screen until the whole provider-restart chain finishes. Session
-- resumes still return to 'runAgentWithRestarts' and start a fresh UI.
prepareAgentIteration
    :: FullscreenInputBuffer
    -> Maybe FullscreenRuntime
    -> CliOptions
    -> Maybe ProviderTransition
    -> IO PreparedAgent
prepareAgentIteration fullscreenInputs activeFullscreen options transition = do
    forM_ activeFullscreen resetFullscreenSessionActions
    let failPreparation message =
            case activeFullscreen of
                Nothing -> die message
                Just _ -> throwIO (StartupFailure message)
    startedAt <- getCurrentTime
    startupTimingsRef <- newIORef []
    syntaxLoadDurationRef <- newIORef Nothing
    home <- getHomeDirectory
    let root = sessionsRoot home
    resumed <- case options.optResume of
        Nothing -> pure Nothing
        Just sessionId ->
            loadSession root sessionId >>= \case
                Left err -> failPreparation (Text.unpack err)
                Right loaded -> pure (Just loaded)

    source <- maybe getCurrentDirectory makeAbsolute options.optCwd
    cwd <- case resumed of
        Just (meta, _)
            | isJustCwd options -> pure source
            | otherwise -> makeAbsolute meta.metaCwd
        Nothing
            | options.optWorktree -> do
                createWorktree source (worktreeRoot home)
                    >>= either (failPreparation . Text.unpack) \path -> do
                    color <- resolveColor stderr
                    putTextLn stderr (roleMuted color (glyphSession <> "worktree: " <> toText path))
                    pure path
            | otherwise -> pure source
    setCurrentDirectory cwd

    toolEnv <- defaultToolEnv cwd
    interrupt <- newInterruptState \msg -> do
        -- Drop an in-place "Thinking…" status so the hint is its own line.
        Text.hPutStr stderr "\r\ESC[K"
        clearNativeProgress stderr
        color <- resolveColor stderr
        putTextLn stderr (roleMuted color msg)
    -- Shared with Esc cancel and plan prompts so arrow-key pickers own stdin.
    escPaused <- newIORef False
    uiRuntimeRef <- newIORef Nothing
    stderrTty <- hIsTerminalDevice stderr
    stdinTty <- hIsTerminalDevice stdin
    stdoutTty <- hIsTerminalDevice stdout
    terminal <- detectTerminalCapabilities stdout
    terminalCwd <- decodeFS cwd
    reportTerminalCwd terminal stdout terminalCwd
    useColor <- resolveColor stdout
    agentSnapshotRef <- newIORef (pure (AgentRoot, []))
    agentSelectRef <- newIORef (\_ -> pure ())
    restartEffortActionRef <- newIORef (\_ -> pure ())
    queuedInputDisplays <- queuedFullscreenInputDisplays fullscreenInputs
    let initialTurns = maybe [] snd resumed
        fullscreenEnabled =
            stdinTty
                && stdoutTty
                && not (isOneShot options)
                && options.optScreenMode /= ScreenMinimal
        initialFullscreenState =
            (reduceUi
                (UiSetNotice
                    (Just (progressNotice "Loading project…")))
                (reduceUi
                    (UiSetRepository
                        ""
                        (toText (takeFileName (takeDirectory cwd))
                            <> "/"
                            <> toText (takeFileName cwd)))
                    (hydrateUiHistory initialTurns)))
                        { uiQueuedInputs = queuedInputDisplays }
    fullscreen <- case activeFullscreen of
        Just runtime -> pure (Just runtime)
        Nothing
            | fullscreenEnabled ->
                Just <$> newFullscreenRuntime
                    fullscreenInputs
                    (requestCancel toolEnv.toolCancel)
                    (\level ->
                        readIORef restartEffortActionRef >>= ($ level))
                    (noteFullscreenCtrlC interrupt)
                    (copyTerminalClipboard terminal stdout)
                    (setCliWindowTitle stdoutTty stdout)
                    (\active ->
                        when
                            (terminal.terminalNativeProgress
                                && nativeProgressAnimationEnabled
                                    options.optMotionMode) $
                            setNativeProgress stderr active)
                    (readIORef agentSnapshotRef >>= id)
                    (\target -> readIORef agentSelectRef >>= ($ target))
                    (recordStartupTiming
                        startedAt startupTimingsRef "first frame")
                    (writeIORef syntaxLoadDurationRef . Just)
                    options.optMotionMode
                    useColor
                    initialFullscreenState
            | otherwise -> pure Nothing
    forM_ fullscreen \runtime ->
        setFullscreenSessionActions
            runtime
            (requestCancel toolEnv.toolCancel)
            (\level -> readIORef restartEffortActionRef >>= ($ level))
            (noteFullscreenCtrlC interrupt)
            (readIORef agentSnapshotRef >>= id)
            (\target -> readIORef agentSelectRef >>= ($ target))
    writeIORef uiRuntimeRef fullscreen
    let startup = StartupRuntime
            { startupToolEnv = toolEnv
            , startupInterrupt = interrupt
            , startupEscPaused = escPaused
            , startupUiRuntimeRef = uiRuntimeRef
            , startupFullscreen = fullscreen
            , startupTerminal = terminal
            , startupUseColor = useColor
            , startupStderrTty = stderrTty
            , startupStdinTty = stdinTty
            , startupStdoutTty = stdoutTty
            , startupFullscreenReused = isJust activeFullscreen
            , startupAgentSnapshot = agentSnapshotRef
            , startupAgentSelect = agentSelectRef
            , startupRestartEffort = restartEffortActionRef
            , startupStartedAt = startedAt
            , startupTimings = startupTimingsRef
            , startupSyntaxLoadDuration = syntaxLoadDurationRef
            }
        action =
            runAgentInitialized
                options transition home root resumed cwd startup
        cleanup = do
            writeIORef uiRuntimeRef Nothing
            forM_ fullscreen resetFullscreenSessionActions
    pure PreparedAgent
        { preparedFullscreen = fullscreen
        , preparedRun = action `finally` cleanup
        }

resetFullscreenSessionActions :: FullscreenRuntime -> IO ()
resetFullscreenSessionActions runtime =
    setFullscreenSessionActions
        runtime
        (pure ())
        (const (pure ()))
        -- No session-local interrupt state is alive between providers. A
        -- transition must remain escapable even if auth probing blocks.
        (pure ForceExit)
        (pure (AgentRoot, []))
        (const (pure ()))

runFullscreenRestartLoop
    :: FullscreenInputBuffer
    -> FullscreenRuntime
    -> CliOptions
    -> Maybe ProviderTransition
    -> IO RunResult
    -> IO RunResult
runFullscreenRestartLoop
    fullscreenInputs
    runtime =
        loop
  where
    loop options transition action =
        -- The notifier in 'runFullscreen' watches this whole tail-recursive
        -- chain, rather than stopping Brick after the first provider exits.
        action >>= \case
            RunSwitchProvider next -> do
                let nextOptions = applyProviderTransition options next
                prepared <- prepareAgentIteration
                    fullscreenInputs
                    (Just runtime)
                    nextOptions
                    (Just next)
                loop nextOptions (Just next) prepared.preparedRun
            RunProviderStartFailed apiError ->
                case transition of
                    Just failed
                        | failed.transitionCause == AutomaticFallback ->
                            continueAutomaticFallback
                                (Just runtime) failed apiError >>= \case
                                Just next -> do
                                    let nextOptions =
                                            applyProviderTransition options next
                                    prepared <- prepareAgentIteration
                                        fullscreenInputs
                                        (Just runtime)
                                        nextOptions
                                        (Just next)
                                    loop
                                        nextOptions
                                        (Just next)
                                        prepared.preparedRun
                                Nothing ->
                                    pure (RunProviderStartFailed apiError)
                    _ -> pure (RunProviderStartFailed apiError)
            result -> pure result

runAgentInitialized
    :: CliOptions
    -> Maybe ProviderTransition
    -> OsPath
    -> OsPath
    -> Maybe (SessionMeta, [SessionTurn])
    -> OsPath
    -> StartupRuntime
    -> IO RunResult
runAgentInitialized options transition home root resumed cwd startup = do
    let toolEnv = startup.startupToolEnv
        interrupt = startup.startupInterrupt
        escPaused = startup.startupEscPaused
        uiRuntimeRef = startup.startupUiRuntimeRef
        fullscreen = startup.startupFullscreen
        isTty = startup.startupStdinTty
        stdoutTty = startup.startupStdoutTty
        setWindowTitle title =
            case fullscreen of
                Just runtime -> setFullscreenWindowTitle runtime title
                Nothing -> setCliWindowTitle stdoutTty stdout title
    projectRoot <- resolveProjectRoot cwd
    projectSettings <- loadProjectSettings projectRoot
    branch <- detectGitBranch cwd
    setStartupRepository fullscreen home branch cwd
    markStartupStage startup "Loading credentials…"
    let transitionTarget = (.transitionTarget) <$> transition
        pendingTurn = transition >>= (.transitionPendingTurn)
        transitionDraft = providerTransitionDraft transition
        unavailableProviders =
            maybe [] (.transitionUnavailableProviders) transition
        requestedProvider = case transitionTarget of
            Just target -> Just target.modelProvider
            Nothing -> case resumed of
                Just (meta, _) -> Just meta.metaProvider
                Nothing -> case options.optProvider of
                    Just requested -> Just requested
                    Nothing
                        | isNothing options.optModel ->
                            projectModelProvider projectSettings
                        | otherwise -> Nothing
    loaded <- loadStartupAuth startup transition requestedProvider
    case (transitionTarget, resumed) of
        (Just target, _)
            | loaded.loadedProvider /= target.modelProvider ->
                startupDie startup $ "provider transition requested "
                    <> Text.unpack (providerSlug target.modelProvider)
                    <> " but auth resolved "
                    <> Text.unpack (providerSlug loaded.loadedProvider)
        (Nothing, Just (meta, _))
            | loaded.loadedProvider /= meta.metaProvider ->
                startupDie startup $ "session provider is "
                    <> Text.unpack (providerSlug meta.metaProvider)
                    <> " but auth resolved "
                    <> Text.unpack (providerSlug loaded.loadedProvider)
        _ -> pure ()
    case transition >>= (.transitionAutomaticBilling) of
        Just sourceBilling
            | not
                (allowsAutomaticBillingFallback
                    sourceBilling
                    (tokenProviderBillingMode loaded.loadedTokenProvider)) ->
                startupDie startup
                    "automatic provider fallback would cross from subscription \
                    \billing to API-credit billing"
        _ -> pure ()
    activeAccountRef <- newIORef ""
    activeAccountIdRef <- newIORef ""
    activeSelectionRef <- newIORef ""
    preferredOpenAiAccountRef <- newIORef Nothing
    let selectableTokenProvider =
            case loaded.loadedOpenAiPool of
                Just pool ->
                    preferredOpenAiTokenProvider
                        preferredOpenAiAccountRef
                        pool
                        loaded.loadedTokenProvider
                Nothing ->
                    loaded.loadedTokenProvider
    initialHttp <- case loaded.loadedProvider of
        OpenAIProvider ->
            pure
                ( selectableTokenProvider
                , loaded.loadedAccountLabel
                , ""
                )
        _ ->
            probeLoadedAuthCredential loaded >>= \case
                Right (credential, usable) -> do
                    label <- usable.loadedAccountLabel credential
                    writeIORef activeAccountRef label
                    writeIORef activeAccountIdRef credential.accountId
                    let selectionId =
                            fromMaybe
                                credential.accountId
                                usable.loadedSelectionId
                    writeIORef activeSelectionRef selectionId
                    pure
                        ( usable.loadedTokenProvider
                        , usable.loadedAccountLabel
                        , credential.accountId
                        )
                Left _ -> do
                    let fallback = case loaded.loadedProvider of
                            XAIProvider -> "Grok"
                            OpenRouterProvider -> "OpenRouter"
                        selectionId = fromMaybe "" loaded.loadedSelectionId
                    writeIORef activeAccountRef fallback
                    writeIORef activeSelectionRef selectionId
                    pure
                        ( selectableTokenProvider
                        , loaded.loadedAccountLabel
                        , ""
                        )
    let
        ( initialHttpProvider
            , initialHttpResolver
            , initialHttpAccountId
            ) = initialHttp
    activeHttpAuth <- newMVar ActiveHttpAuth
        { activeHttpGeneration = 0
        , activeHttpProvider = initialHttpProvider
        , activeHttpResolveLabel = initialHttpResolver
        , activeHttpAccountId = initialHttpAccountId
        }
    let switchableTokenProvider =
            Provider.tokenProvider
                (tokenProviderBillingMode selectableTokenProvider)
                \failed -> do
                    snapshot <- readMVar activeHttpAuth
                    let routedFailure = case failed of
                            Just reported
                                | reported.credential.accountId
                                    == snapshot.activeHttpAccountId ->
                                    Just reported
                            _ -> Nothing
                    getNextToken
                        snapshot.activeHttpProvider
                        routedFailure
                        >>= \case
                            Left err -> pure (Left err)
                            Right credential -> do
                                label <-
                                    snapshot.activeHttpResolveLabel credential
                                modifyMVar_ activeHttpAuth \current ->
                                    if current.activeHttpGeneration
                                        == snapshot.activeHttpGeneration
                                        then do
                                            writeIORef
                                                activeAccountIdRef
                                                credential.accountId
                                            writeIORef activeAccountRef label
                                            pure current
                                                { activeHttpAccountId =
                                                    credential.accountId
                                                }
                                        else pure current
                                pure (Right credential)
        resolveActiveAccountLabel credential =
            case loaded.loadedProvider of
                OpenAIProvider ->
                    loaded.loadedAccountLabel credential
                _ -> do
                    active <- readMVar activeHttpAuth
                    active.activeHttpResolveLabel credential
        tokenProvider =
            case loaded.loadedProvider of
                OpenAIProvider ->
                    trackCredentialAccount
                        activeAccountRef
                        activeAccountIdRef
                        activeSelectionRef
                        resolveActiveAccountLabel
                        selectableTokenProvider
                _ -> switchableTokenProvider
        selectHttpAccount selectedSelectionId =
            loadAuthForAccount loaded.loadedProvider selectedSelectionId
                >>= \case
                    Left err ->
                        pure (Left (CredentialError err))
                    Right selected
                        | tokenProviderBillingMode
                            selected.loadedTokenProvider
                            /= tokenProviderBillingMode
                                selectableTokenProvider ->
                            pure $ Left $ CredentialError
                                "selected account uses a different billing mode"
                        | otherwise ->
                            probeLoadedAuthCredential selected >>= \case
                                Left err -> pure (Left err)
                                Right (credential, usable) -> do
                                    label <-
                                        usable.loadedAccountLabel credential
                                    let selectionId =
                                            fromMaybe
                                                selectedSelectionId
                                                usable.loadedSelectionId
                                    modifyMVar_ activeHttpAuth \current -> do
                                        writeIORef
                                            activeAccountIdRef
                                            credential.accountId
                                        writeIORef
                                            activeSelectionRef
                                            selectionId
                                        writeIORef activeAccountRef label
                                        pure ActiveHttpAuth
                                            { activeHttpGeneration =
                                                current.activeHttpGeneration + 1
                                            , activeHttpProvider =
                                                usable.loadedTokenProvider
                                            , activeHttpResolveLabel =
                                                usable.loadedAccountLabel
                                            , activeHttpAccountId =
                                                credential.accountId
                                            }
                                    pure (Right label)

    markStartupStage startup "Loading tools…"
    let basePlanHooks =
            cliPlanHooks interrupt escPaused (resolveColor stderr)
        planHooks = fullscreenAwarePlanHooks uiRuntimeRef basePlanHooks
        provider = loaded.loadedProvider
        model = fromMaybe
            (case transitionTarget of
                Just target -> target.modelId
                Nothing ->
                    case fst <$> resumed of
                        Just meta -> meta.metaModel
                        Nothing ->
                            fromMaybe
                                (defaultModelFor provider)
                                (projectModelFor provider projectSettings))
            options.optModel
        effort = fromMaybe
            (maybe (defaultEffortFor provider) (.metaEffort) (fst <$> resumed))
            options.optEffort
        policy = resolveApprovalPolicy options isTty
            projectSettings.settingsAutoApprove
    -- Provider transitions commit their selection separately: manual switches
    -- immediately, automatic fallbacks only after the replacement succeeds.
    when (isNothing transition) $
        saveProjectModel projectRoot provider model
    sessionProcessManager <- newSessionProcessManager root
    managedAgentSession <- (== Just "1") <$> lookupEnv "HASKELL_AGENT_MANAGED_SESSION"
    activeSessionLock <- newIORef (Nothing :: Maybe FilePath)
    persistSlotRef <- newIORef PersistenceDisabled
    -- Per-subagent transcripts / previous ids, shared across send_input / task.
    subagentSessions <- newIORef Map.empty
    subagentStoreRoot <- newIORef Nothing
    subagentForkSource <- newIORef (Nothing :: Maybe (IORef [ResponseItem]))
    pendingNotices <- newIORef ([] :: [TurnInput])
    registry <- newSubagentRegistry defaultSubagentConfig cwd
        (\_ _ _ _ -> pure $ Left LoopNoResponseId)
        (\_ _ -> pure ())
    rootTurnRef <- newIORef (Nothing :: Maybe RootTurnId)
    agentTypesRef <- newIORef Map.empty
    let sendToRoot message = do
            atomicModifyIORef' pendingNotices \xs ->
                (xs <> [AgentMessage message], ())
            pure (Right "queued")
        multiCtx = Just MultiAgentContext
            { multiRegistry = registry
            , multiSelfId = Nothing
            , multiDepth = 0
            , multiTaskPath = taskPathRoot
            , multiRootTurnId = readIORef rootTurnRef
            , multiResumeFromDisk = Just
                (restoreAgentFromDisk subagentStoreRoot registry subagentSessions agentTypesRef)
            , multiCreateWorktree = Just \source ->
                createWorktree source (worktreeRoot home) >>= \case
                    Left err -> pure (Left err)
                    Right path -> pure $ Right SubagentWorktree
                        { subagentWorktreePath = path
                        , subagentWorktreeCleanup =
                            removeWorktree source path >>= \case
                                Left err -> pure (Left err)
                                Right () -> pure (Right ())
                        }
            , multiPrepareSpawn = Just
                (prepareCollaborationSpawn
                    subagentSessions subagentStoreRoot agentTypesRef
                    subagentForkSource)
            , multiSendToRoot = Just sendToRoot
            }
    coding <- codingToolsForWithTypes provider toolEnv (Just planHooks) multiCtx agentTypesRef
    case multiCtx of
        Just ctx ->
            setSubagentOnComplete ctx.multiRegistry \agentId status -> do
                atomicModifyIORef' pendingNotices \xs ->
                    (xs <> [UserMessage (formatCompletionNotice agentId status)], ())
                sessions <- readIORef subagentSessions
                case Map.lookup agentId sessions of
                    Just session ->
                        persistSubagentSnapshotWithStatus
                            subagentStoreRoot ctx.multiRegistry agentTypesRef
                            agentId status session.subSessionTranscript
                    Nothing -> pure ()
        Nothing -> pure ()
    let claimCurrentSession handle
            | managedAgentSession = pure ()
            | otherwise = do
                let desired =
                        unsafeToFilePath
                            (handle.sessionDir </> unsafeEncodeUtf ".agent-running")
                readIORef activeSessionLock >>= \case
                    Just current | current == desired -> pure ()
                    previous ->
                        acquireSessionLock handle >>= \case
                            Left err -> throwIO (userError (Text.unpack err))
                            Right lockPath -> do
                                writeIORef activeSessionLock (Just lockPath)
                                mapM_ releaseSessionLock previous
        sessionToolsEnv = AgentSessionToolsEnv
            { toolsRoot = root
            , toolsProvider = provider
            , toolsModel = model
            , toolsCwd = cwd
            , toolsEffort = effort
            , toolsCurrentSessionId =
                readIORef persistSlotRef >>= currentSessionId
            , toolsLaunchTurn =
                launchSessionTurn sessionProcessManager
                    (not (isOneShot options)) policy
            , toolsSessionStatus =
                sessionProcessStatus sessionProcessManager
            }
        tools = coding.codingAppTools ++ agentSessionTools sessionToolsEnv
        planMode = coding.codingPlanMode
        -- Keep planSessionDir and subagent store root in sync.
        noteSessionDir dir = do
            writeIORef planMode.planSessionDir (Just dir)
            writeIORef subagentStoreRoot (Just dir)
        closeAll = do
            case multiCtx of
                Just ctx -> do
                    interruptActiveSubagents ctx.multiRegistry
                    flushAllSubagentSnapshots subagentStoreRoot ctx.multiRegistry
                        subagentSessions agentTypesRef
                    closeSubagentRegistry ctx.multiRegistry
                Nothing -> pure ()
            closeSessionProcessManager sessionProcessManager
            readIORef activeSessionLock >>= mapM_ releaseSessionLock
            coding.codingClose
    flip finally closeAll do
        today <- utctDay <$> getCurrentTime
        let instructions = systemPrompt provider cwd today (isOneShot options)
            params = requestParams model instructions
                (schemasFromAppTools provider tools) effort
            initialItems = maybe [] (foldSessionItems . snd) resumed
            initialTurns = maybe [] snd resumed
            initialPrevious = case transition of
                Just _ -> Nothing
                Nothing -> resumed >>= \(meta, _) -> meta.metaLastResponseId
        paramsRef <- newIORef params
        let subagentRuntime = SubagentRuntime
                { subagentOptions = options
                , subagentPolicy = policy
                , subagentPlanHooks = planHooks
                , subagentParams = paramsRef
                , subagentRegistry = registry
                , subagentSessions = subagentSessions
                , subagentStoreRoot = subagentStoreRoot
                , subagentTypes = agentTypesRef
                }
        transcriptRef <- newIORef initialItems
        contextTokensRef <- newIORef Nothing
        previousRef <- newIORef initialPrevious
        writeIORef subagentForkSource (Just transcriptRef)
        prompt <- loadPrompt options
        let titleHint = case resumed of
                Just (meta, _) -> Just meta.metaTitle
                Nothing -> sessionTitleFromPrompt <$> prompt
        setWindowTitle (cliWindowTitle cwd titleHint)
        markStartupStage startup "Loading instructions…"
        startupContext <-
            loadAgentsContext
                fullscreen options provider home cwd initialItems initialPrevious
        -- Fullscreen sessions load skills after Brick has taken over the
        -- terminal, so filesystem discovery cannot delay the first frame.
        -- Minimal and one-shot sessions still initialize them synchronously
        -- before their first prompt/turn below.
        skillsRef <- newIORef (SkillCatalog [] [])
        skillInvocationsRef <- newIORef []

        persist <-
            preparePersistence
                fullscreen options root provider model cwd effort prompt resumed
        writeIORef persistSlotRef persist
        usageRef <- newIORef $ case resumed of
            Just (meta, turns) -> sessionUsageFromTurns meta turns
            Nothing -> emptyTokenUsage
        let recordCompactionUsage usage =
                when (usage /= emptyTokenUsage) $
                    mask_ do
                        case persist of
                            PersistenceDisabled -> pure ()
                            PersistenceEnabled slotRef -> do
                                handle <- ensureSession slotRef
                                claimCurrentSession handle
                                updated <- addSessionUsage usage handle
                                writeIORef slotRef (PersistenceActive updated)
                        modifyIORef' usageRef (`addTokenUsage` usage)
        case persist of
            PersistenceEnabled slotRef -> do
                slot <- readIORef slotRef
                case slot of
                    PersistenceActive handle -> do
                        claimCurrentSession handle
                        noteSessionDir handle.sessionDir
                    PersistencePending _ -> pure ()
            PersistenceDisabled -> pure ()
        progName <- getProgName
        markStartupStage startup "Connecting to provider…"
        withCtrlCHandler interrupt $
            withInterruptResume progName persist RunQuit do
                let shouldProbeAtStartup =
                        isJust fullscreen
                            && isNothing transition
                            && isNothing resumed
                            && isNothing options.optProvider
                            && isNothing options.optModel
                            && isNothing prompt
                    withStartupAvailability action
                        | shouldProbeAtStartup =
                            withAsync
                                (probeLoadedAvailability
                                    loaded
                                        { loadedTokenProvider =
                                            tokenProvider
                                        })
                                \availability -> do
                                    let startupUnavailable =
                                            waitSTM availability >>= \case
                                                Left err
                                                    | isProviderUnavailable err ->
                                                        pure err
                                                _ -> retry
                                    action (Just startupUnavailable)
                        | otherwise = action Nothing
                withStartupAvailability \startupUnavailable ->
                    case provider of
                    OpenAIProvider ->
                        try @_ @CodexAuthFailed
                            (withCodexWsWithProvider tokenProvider \conn credential -> do
                                wsLock <- newMVar ()
                                initialWsHealthy <- newIORef True
                                activeConnectionRef <- newIORef $
                                    OpenAiPersistentConnection
                                        credential
                                        initialWsHealthy
                                        conn
                                switchRequests <-
                                    newChan :: IO (Chan AccountSwitchRequest)
                                let selectAccount = case loaded.loadedOpenAiPool of
                                        Nothing -> Nothing
                                        Just pool ->
                                            Just \selectedAccountId -> do
                                                    _ <- OpenAI.discoverAccounts pool
                                                    OpenAI.getAccessTokenForAccount
                                                        pool
                                                        selectedAccountId
                                                        >>= \case
                                                            Left err ->
                                                                pure (Left err)
                                                            Right
                                                                ( accessToken
                                                                , accountId
                                                                ) -> do
                                                                reply <- newEmptyMVar
                                                                writeChan
                                                                    switchRequests
                                                                    (AccountSwitchRequest
                                                                        Credential
                                                                            { accessToken
                                                                            , accountId
                                                                            , leaseId = Nothing
                                                                            , provider =
                                                                                OpenAIProvider
                                                                            }
                                                                        reply)
                                                                takeMVar reply
                                    switchLoop = case loaded.loadedOpenAiPool of
                                        Nothing -> pure ()
                                        Just pool ->
                                            readChan switchRequests
                                                >>= switchTo pool
                                    switchTo pool request =
                                        runSwitch pool request >>= \case
                                            Nothing -> switchLoop
                                            Just next -> switchTo pool next
                                    runSwitch
                                        pool
                                        (AccountSwitchRequest
                                            selectedCredential
                                            reply) = do
                                                takeMVar wsLock
                                                lockHeld <- newIORef True
                                                let releaseLock = do
                                                        held <-
                                                            atomicModifyIORef'
                                                                lockHeld
                                                                (\held ->
                                                                    (False, held))
                                                        when held $
                                                            putMVar wsLock ()
                                                    failSwitch err = do
                                                        releaseLock
                                                        _ <- tryPutMVar
                                                            reply
                                                            (Left err)
                                                        pure Nothing
                                                    installConnection
                                                        newCredential
                                                        newConn = do
                                                            newHealthy <-
                                                                newIORef True
                                                            label <-
                                                                resolveActiveAccountLabel
                                                                    newCredential
                                                            writeIORef
                                                                activeConnectionRef $
                                                                OpenAiPersistentConnection
                                                                    newCredential
                                                                    newHealthy
                                                                    newConn
                                                            writeIORef
                                                                activeAccountIdRef
                                                                newCredential.accountId
                                                            writeIORef
                                                                activeSelectionRef
                                                                newCredential.accountId
                                                            writeIORef
                                                                activeAccountRef
                                                                label
                                                            pure (newHealthy, label)
                                                    awaitNext newHealthy =
                                                        readChan switchRequests
                                                            `finally`
                                                                writeIORef
                                                                    newHealthy
                                                                    False
                                                oldConnection <-
                                                    readIORef activeConnectionRef
                                                previousAccountId <-
                                                    readIORef activeAccountIdRef
                                                let OpenAiPersistentConnection
                                                        _
                                                        oldHealthy
                                                        oldConn =
                                                            oldConnection
                                                writeIORef oldHealthy False
                                                closeCodexConn oldConn
                                                writeIORef
                                                    preferredOpenAiAccountRef
                                                    (Just
                                                        selectedCredential.accountId)
                                                let connectSelected =
                                                        withCodexWsCredential
                                                            selectedCredential
                                                            \newConn
                                                                newCredential -> do
                                                                    (newHealthy, label) <-
                                                                        installConnection
                                                                            newCredential
                                                                            newConn
                                                                    releaseLock
                                                                    _ <- tryPutMVar
                                                                        reply
                                                                        (Right label)
                                                                    awaitNext
                                                                        newHealthy
                                                    restorePrevious
                                                        selectedError
                                                        | Text.null
                                                            previousAccountId =
                                                            failSwitch
                                                                selectedError
                                                        | otherwise = do
                                                            writeIORef
                                                                preferredOpenAiAccountRef
                                                                (Just
                                                                    previousAccountId)
                                                            OpenAI.getAccessTokenForAccount
                                                                pool
                                                                previousAccountId
                                                                >>= \case
                                                                    Left _ ->
                                                                        failSwitch
                                                                            selectedError
                                                                    Right
                                                                        ( previousToken
                                                                        , restoredId
                                                                        ) -> do
                                                                            let restoredCredential =
                                                                                    Credential
                                                                                        { accessToken =
                                                                                            previousToken
                                                                                        , accountId =
                                                                                            restoredId
                                                                                        , leaseId =
                                                                                            Nothing
                                                                                        , provider =
                                                                                            OpenAIProvider
                                                                                        }
                                                                            (withCodexWsCredential
                                                                                restoredCredential
                                                                                \newConn
                                                                                    newCredential -> do
                                                                                        (newHealthy, _) <-
                                                                                            installConnection
                                                                                                newCredential
                                                                                                newConn
                                                                                        releaseLock
                                                                                        _ <- tryPutMVar
                                                                                            reply
                                                                                            (Left
                                                                                                selectedError)
                                                                                        awaitNext
                                                                                            newHealthy)
                                                                                >>= \case
                                                                                    Left _ ->
                                                                                        failSwitch
                                                                                            selectedError
                                                                                    Right next ->
                                                                                        pure
                                                                                            (Just
                                                                                                next)
                                                (connectSelected >>= \case
                                                    Left selectedError ->
                                                        restorePrevious
                                                            selectedError
                                                    Right next ->
                                                        pure (Just next))
                                                    `catchAny` \_ ->
                                                        failSwitch $
                                                            ConnectionError
                                                                "account switch failed"
                                case multiCtx of
                                    Just ctx ->
                                        setSubagentRunner ctx.multiRegistry $
                                            runCodexSubagent
                                                subagentRuntime
                                                selectableTokenProvider
                                                ctx.multiSendToRoot
                                    Nothing -> pure ()
                                let (compactSender, lockedBackend) =
                                        lockedOpenAiSession
                                            options.optCompactThreshold
                                            wsLock
                                            tokenProvider
                                            activeConnectionRef
                                            (readIORef paramsRef)
                                            transcriptRef
                                            contextTokensRef
                                            recordCompactionUsage
                                    noticingBackend =
                                        withPendingInputs pendingNotices
                                            lockedBackend
                                    btwBackend privateParams privateTranscript =
                                        freshOpenAiBackend
                                            tokenProvider
                                            (readIORef privateParams)
                                            privateTranscript
                                    compactRunner focus =
                                        withMVar wsLock \_ ->
                                            installCompactOutcome
                                                previousRef
                                                transcriptRef
                                                (Just contextTokensRef)
                                                (runProviderCompactWith
                                                    (Just compactSender)
                                                    recordCompactionUsage
                                                    provider
                                                    (Just tokenProvider)
                                                    paramsRef
                                                    transcriptRef)
                                                focus
                                activeBackend <-
                                    prepareTransitionBackend
                                        projectRoot transition persist noticingBackend
                                withAsync switchLoop \switchWorker -> do
                                    link switchWorker
                                    runSession options provider policy tools toolEnv planMode startup prompt pendingTurn transitionDraft unavailableProviders startupUnavailable paramsRef transcriptRef initialTurns
                                        previousRef persist projectRoot home cwd (Just tokenProvider) loaded.loadedOpenAiPool startupContext skillsRef skillInvocationsRef escPaused interrupt
                                        multiCtx rootTurnRef subagentSessions pendingNotices subagentStoreRoot usageRef activeAccountRef activeAccountIdRef activeSelectionRef resolveActiveAccountLabel selectAccount claimCurrentSession compactRunner activeBackend btwBackend)
                            >>= \case
                                Left (CodexAuthFailed err) ->
                                    case transition of
                                        Just active
                                            | active.transitionCause == AutomaticFallback ->
                                                pure (RunProviderStartFailed err)
                                        _
                                            | shouldProbeAtStartup
                                            , isProviderUnavailable err ->
                                                chooseStartupProviderTransition
                                                    fullscreen
                                                    (tokenProviderBillingMode
                                                        tokenProvider)
                                                    provider
                                                    unavailableProviders
                                                    Nothing
                                                    err >>= \case
                                                        Just next ->
                                                            pure
                                                                (RunSwitchProvider
                                                                    next)
                                                        Nothing ->
                                                            startupFailure err
                                        _ -> do
                                            startupFailure err
                                Right result -> pure result
                    XAIProvider -> do
                        xaiOptions <- XAI.clientOptionsFromEnv
                        case multiCtx of
                            Just ctx ->
                                setSubagentRunner ctx.multiRegistry $
                                    runHttpSubagent
                                        subagentRuntime
                                        XAIProvider
                                        (\childParamsRef childTranscript ->
                                            xaiBackend xaiOptions tokenProvider
                                                (readIORef childParamsRef) childTranscript)
                            Nothing -> pure ()
                        let backend =
                                withPendingInputs pendingNotices $
                                    withConnectionRecovery $
                                        xaiBackend xaiOptions tokenProvider
                                            (readIORef paramsRef) transcriptRef
                            btwBackend privateParams privateTranscript =
                                xaiBackend xaiOptions tokenProvider
                                    (readIORef privateParams) privateTranscript
                            compactRunner =
                                installCompactOutcome previousRef transcriptRef Nothing $
                                    runProviderCompactWith
                                        Nothing
                                        recordCompactionUsage
                                        provider
                                        (Just tokenProvider)
                                        paramsRef
                                        transcriptRef
                        activeBackend <-
                            prepareTransitionBackend
                                projectRoot transition persist backend
                        runSession options provider policy tools toolEnv planMode startup prompt pendingTurn transitionDraft unavailableProviders startupUnavailable paramsRef transcriptRef initialTurns
                            previousRef persist projectRoot home cwd (Just tokenProvider) loaded.loadedOpenAiPool startupContext skillsRef skillInvocationsRef escPaused interrupt
                            multiCtx rootTurnRef subagentSessions pendingNotices subagentStoreRoot usageRef activeAccountRef activeAccountIdRef activeSelectionRef resolveActiveAccountLabel (Just selectHttpAccount) claimCurrentSession compactRunner activeBackend btwBackend
                    OpenRouterProvider -> do
                        openRouterOptions <- OpenRouter.clientOptionsFromEnv
                        case multiCtx of
                            Just ctx ->
                                setSubagentRunner ctx.multiRegistry $
                                    runHttpSubagent
                                        subagentRuntime
                                        OpenRouterProvider
                                        (\childParamsRef childTranscript ->
                                            openRouterBackend openRouterOptions
                                                tokenProvider
                                                (readIORef childParamsRef) childTranscript)
                            Nothing -> pure ()
                        let backend =
                                withPendingInputs pendingNotices $
                                    withConnectionRecovery $
                                        openRouterBackend openRouterOptions tokenProvider
                                            (readIORef paramsRef) transcriptRef
                            btwBackend privateParams privateTranscript =
                                openRouterBackend openRouterOptions tokenProvider
                                    (readIORef privateParams) privateTranscript
                            compactRunner =
                                installCompactOutcome previousRef transcriptRef Nothing $
                                    runProviderCompactWith
                                        Nothing
                                        recordCompactionUsage
                                        provider
                                        (Just tokenProvider)
                                        paramsRef
                                        transcriptRef
                        activeBackend <-
                            prepareTransitionBackend
                                projectRoot transition persist backend
                        runSession options provider policy tools toolEnv planMode startup prompt pendingTurn transitionDraft unavailableProviders startupUnavailable paramsRef transcriptRef initialTurns
                            previousRef persist projectRoot home cwd (Just tokenProvider) loaded.loadedOpenAiPool startupContext skillsRef skillInvocationsRef escPaused interrupt
                            multiCtx rootTurnRef subagentSessions pendingNotices subagentStoreRoot usageRef activeAccountRef activeAccountIdRef activeSelectionRef resolveActiveAccountLabel (Just selectHttpAccount) claimCurrentSession compactRunner activeBackend btwBackend
          where
            startupFailure err = do
                now <- getCurrentTime
                startupDie startup
                    (Text.unpack (formatApiErrorAt now err))

trackCredentialAccount
    :: IORef Text
    -> IORef Text
    -> IORef Text
    -> (Credential -> IO Text)
    -> TokenProvider
    -> TokenProvider
trackCredentialAccount accountRef accountIdRef selectionRef resolveLabel provider =
    tokenProvider (tokenProviderBillingMode provider) \failed ->
        getNextToken provider failed >>= \case
            Left err -> pure (Left err)
            Right credential -> do
                writeIORef accountIdRef credential.accountId
                writeIORef selectionRef credential.accountId
                resolveLabel credential >>= writeIORef accountRef
                pure (Right credential)

preparePersistence
    :: Maybe FullscreenRuntime
    -> CliOptions
    -> OsPath
    -> Provider
    -> Text
    -> OsPath
    -> Text
    -> Maybe Text
    -> Maybe (SessionMeta, [SessionTurn])
    -> IO Persistence
preparePersistence fullscreen options root provider model cwd effort prompt resumed =
    case resumed of
        Just (meta, _) -> do
            let handle = SessionHandle
                    { sessionDir = root </> fromText meta.metaId
                    , sessionMetaPath =
                        root </> fromText meta.metaId </> unsafeEncodeUtf "meta.json"
                    , sessionTranscriptPath =
                        root </> fromText meta.metaId </> unsafeEncodeUtf "transcript.jsonl"
                    , sessionMeta = meta
                    }
            let message = "session: " <> meta.metaId <> " (resumed)"
            case fullscreen of
                Nothing -> do
                    color <- resolveColor stderr
                    putTextLn stderr
                        (roleMuted color (glyphSession <> message))
                Just runtime ->
                    emitUiEvent runtime (UiSystemMessage message)
            newActivePersistence handle
        Nothing
            | shouldPersist options ->
                -- Defer directory creation until the first successful turn so
                -- an abandoned REPL does not leave empty session folders.
                newPendingPersistence SessionCreate
                    { createRoot = root
                    , createProvider = provider
                    , createModel = model
                    , createCwd = cwd
                    , createEffort = effort
                    , createTitleHint = sessionTitleFromPrompt <$> prompt
                    , createTitleIsManual = False
                    }
            | otherwise -> pure PersistenceDisabled

-- | On Ctrl-C, print a copy-pasteable --resume line when a session exists.
withInterruptResume
    :: String
    -> Persistence
    -> a
    -> IO a
    -> IO a
withInterruptResume progName persist interrupted action =
    (action `catchAny` handleSyncException) `catchAsync` handleInterrupt
  where
    -- UserInterrupt can arrive asynchronously from the installed SIGINT
    -- handler or wrapped as a synchronous exception by safe-exceptions when
    -- the inline editor's double-Ctrl-C path calls throwIO.
    handleInterrupt (e :: AsyncException) =
        case e of
            UserInterrupt -> finishInterrupt
            _ -> throwIO e
    handleSyncException (e :: SomeException)
        | isWrappedUserInterrupt e = finishInterrupt
        | otherwise = throwIO e
    finishInterrupt = do
        printResumeHint progName persist
        -- The interrupt is the requested, graceful end of the CLI session.
        -- Returning lets the surrounding brackets restore the SIGINT handler
        -- and close tools without GHC's top-level exception handler printing
        -- "user interrupt" and a backtrace.
        pure interrupted

printResumeHint
    :: String
    -> Persistence
    -> IO ()
printResumeHint progName = \case
    PersistenceDisabled -> pure ()
    PersistenceEnabled slotRef -> do
        slot <- readIORef slotRef
        case slot of
            PersistencePending _ -> pure ()
            PersistenceActive handle -> do
                -- Drop an in-place "Thinking…" status so the hint is its own line.
                Text.hPutStr stderr "\r\ESC[K"
                clearNativeProgress stderr
                color <- resolveColor stderr
                putTextLn stderr
                    (roleMuted color (resumeHint progName handle.sessionMeta.metaId))

shouldPersist :: CliOptions -> Bool
shouldPersist options = not (isOneShot options) || options.optSaveSession

isJustCwd :: CliOptions -> Bool
isJustCwd options = case options.optCwd of
    Just _ -> True
    Nothing -> False


runSession
    :: CliOptions
    -> Provider
    -> ApprovalPolicy
    -> [AppTool]
    -> ToolEnv
    -> PlanModeEnv
    -> StartupRuntime
    -> Maybe Text
    -> Maybe PendingTurn
    -> Text
    -> [Provider]
    -> Maybe (STM ApiError)
    -> IORef ResponseCreateParams
    -> IORef [ResponseItem]
    -> [SessionTurn]
    -> IORef (Maybe Text)
    -> Persistence
    -> OsPath
    -> OsPath
    -> OsPath
    -> Maybe TokenProvider
    -> Maybe OpenAI.Pool
    -> IORef (Maybe Text)
    -> IORef SkillCatalog
    -> IORef [SkillInvocation]
    -> IORef Bool
    -> InterruptState
    -> Maybe MultiAgentContext
    -> IORef (Maybe RootTurnId)
    -> IORef (Map SubagentId SubagentSession)
    -> IORef [TurnInput]
    -> SubagentStoreRoot
    -> IORef TokenUsage
    -> IORef Text
    -> IORef Text
    -> IORef Text
    -> (Credential -> IO Text)
    -> Maybe (Text -> IO (Either ApiError Text))
    -> (SessionHandle -> IO ())
    -> (Maybe Text -> IO (Either Text CompactOutcome))
    -> Backend
    -> BtwBackendFactory
    -> IO RunResult
runSession options provider policy tools toolEnv planMode startup prompt pendingTurn initialDraft unavailableProviders startupUnavailable paramsRef transcriptRef initialTurns previous persist projectRoot home cwd tokenProvider openAiPool startupContext skillsRef skillInvocationsRef escPaused interrupt multiCtx rootTurnRef subagentSessions pendingNotices storeRoot usageRef accountRef accountIdRef selectionRef accountLabel selectAccount onPersisted compactRunner backend btwBackend = do
  initialPrevious <- readIORef previous
  ioLock <- newMVar ()
  let fullscreen = startup.startupFullscreen
      terminal = startup.startupTerminal
      useColor = startup.startupUseColor
      stderrTty = startup.startupStderrTty
      stdoutTty = startup.startupStdoutTty
      setWindowTitle title =
          case fullscreen of
              Just runtime -> setFullscreenWindowTitle runtime title
              Nothing -> setCliWindowTitle stdoutTty stdout title
      showGeneratedTitle SessionTitleResult{..} =
          case persist of
              PersistenceDisabled -> pure ()
              PersistenceEnabled slotRef ->
                  readIORef slotRef >>= \case
                      PersistenceActive handle
                          | handle.sessionMeta.metaId == resultSessionId
                          , not handle.sessionMeta.metaTitleIsManual ->
                              withMVar ioLock \_ ->
                                  setWindowTitle
                                      (cliWindowTitle handle.sessionMeta.metaCwd
                                          (Just resultTitle))
                      _ -> pure ()
  withSessionTitleManager btwBackend paramsRef showGeneratedTitle \titleManager -> do
    toolRegistry <- requireToolRegistry tools
    printed <- newIORef False
    attachmentsRef <- newIORef []
    previewIdRef <- newIORef (1 :: Int)
    textBuffer <- newIORef ""
    markdownState <- newIORef emptyMarkdownStreamState
    liveActive <- newIORef False
    thinkingVisible <- newIORef False
    spinnerRef <- newIORef Nothing
    reasoningBuffer <- newIORef ""
    activityRef <- newIORef "Thinking…"
    startedAtRef <- newIORef Nothing
    toolCallsRef <- newIORef Map.empty
    allowedToolsRef <- newIORef Set.empty
    lastAssistantRef <- newIORef Nothing
    modelRef <- newIORef =<< (currentModel <$> readIORef paramsRef)
    unavailableProvidersRef <- newIORef unavailableProviders
    startupUnavailableRef <- newIORef startupUnavailable
    restartEffortRef <- newIORef Nothing
    titleTurnCount <- newIORef =<< sessionTitleTurnCountFromSlot persist
    selectedAgent <- newIORef AgentRoot
    agentStepCache <- newIORef (Map.empty :: Map AgentTarget AgentStepCache)
    let cachedAgentSteps target variant items build = do
            transcriptName <- makeStableName items
            cache <- readIORef agentStepCache
            case Map.lookup target cache of
                Just cached
                    | cached.cachedTranscript == transcriptName
                    , cached.cachedVariant == variant ->
                        pure cached.cachedSteps
                _ -> do
                    let steps = build items
                    atomicModifyIORef' agentStepCache \current ->
                        ( Map.insert target (AgentStepCache
                            { cachedTranscript = transcriptName
                            , cachedVariant = variant
                            , cachedSteps = steps
                            })
                            current
                        , ()
                        )
                    pure steps
        loadAgentSnapshot includeSummaries = do
            rootItems <- readIORef transcriptRef
            agents <- case multiCtx of
                Nothing -> pure []
                Just ctx -> listAgents ctx.multiRegistry Nothing
            let availableTargets =
                    AgentRoot
                        : [ AgentChild agentId
                          | (_, agentId, _) <- agents
                          ]
            selected <-
                atomicModifyIORef' selectedAgent \current ->
                    let reconciled =
                            TuiBridge.reconcileAgentSelection
                                availableTargets
                                current
                    in (reconciled, reconciled)
            sessions <- readIORef subagentSessions
            let previewCount target =
                    if null agents
                        then Nothing
                        else if target == selected
                            then Just 12
                            else if includeSummaries
                                then Just 0
                                else Nothing
            rootSteps <-
                if null agents
                    then pure []
                    else cachedAgentSteps
                        AgentRoot
                        Nothing
                        rootItems
                        (responseItemStepPreviews 2)
            let rootEntry = AgentEntry
                    { agentTarget = AgentRoot
                    , agentPath = "/root"
                    , agentStatus = "active"
                    , agentSteps = rootSteps
                    , agentTranscript = case previewCount AgentRoot of
                        Nothing -> []
                        Just count ->
                            responseItemPreviewLines count rootItems
                    }
            children <- mapM
                (materializeChild previewCount sessions)
                agents
            pure (selected, rootEntry : children)
          where
            materializeChild previewCount sessions (path, agentId, status) = do
                let target = AgentChild agentId
                items <- case Map.lookup agentId sessions of
                    Nothing -> pure []
                    Just session -> readIORef session.subSessionTranscript
                steps <- cachedAgentSteps
                    target
                    (Just status)
                    items
                    (agentStepsForStatus 2 status)
                let transcript = case previewCount target of
                        Nothing -> []
                        Just count ->
                            compactAgentPreview count $
                                (if null items
                                    then ["(" <> formatAgentStatus status <> ")"]
                                    else responseItemPreviewLines count items)
                                    <> case status of
                                        Completed (Just result)
                                            | not (Text.null (Text.strip result)) ->
                                                ["assistant: " <> Text.strip result]
                                        Errored message ->
                                            ["error: " <> Text.strip message]
                                        _ -> []
                pure AgentEntry
                    { agentTarget = target
                    , agentPath = taskPathText path
                    , agentStatus = formatAgentStatus status
                    , agentSteps = steps
                    , agentTranscript = transcript
                    }
            compactAgentPreview count rows
                | length rows <= count = rows
                | otherwise = case rows of
                    [] -> []
                    firstLine : _ ->
                        firstLine
                            : drop (max 0 (length rows - count)) rows
        agentViewport = AgentViewportEnv
            { viewportSelected = selectedAgent
            , viewportEntries = snd <$> loadAgentSnapshot True
            }
    writeIORef startup.startupAgentSnapshot
        (loadAgentSnapshot False)
    writeIORef startup.startupAgentSelect (writeIORef selectedAgent)
    let sessionReset = do
            resetLiveConversation previous transcriptRef attachmentsRef planMode
            writeIORef usageRef emptyTokenUsage
            writeIORef lastAssistantRef Nothing
            writeIORef pendingNotices []
            writeIORef subagentSessions Map.empty
            writeIORef selectedAgent AgentRoot
            writeIORef agentStepCache Map.empty
            case multiCtx of
                Just ctx -> resetSubagentRegistry ctx.multiRegistry
                Nothing -> pure ()
            freshAgents <-
                loadAgentsContext fullscreen options provider home cwd [] Nothing
            freshSkills <- loadSkillsCatalog options home projectRoot cwd True
            installSkillCatalog
                reservedSlashNames True freshAgents
                skillsRef skillInvocationsRef freshSkills
            fresh <- readIORef freshAgents
            writeIORef startupContext fresh
        refreshSkills queueContext = do
            refreshed <- loadSkillsCatalog
                options home projectRoot cwd queueContext
            installSkillCatalog
                reservedSlashNames queueContext startupContext
                skillsRef skillInvocationsRef refreshed
    policyRef <- newIORef policy
    -- Mirror plan session dir into the subagent store root for this session.
    let syncStore = do
            sessionDir <- readIORef planMode.planSessionDir
            case sessionDir of
                Just dir -> writeIORef storeRoot (Just dir)
                Nothing -> pure ()
    syncStore
    let render = RenderConfig
            { renderShowThinking = stderrTty
            , renderThinkingVisible = thinkingVisible
            , renderThinkingSpinner = spinnerRef
            , renderReasoningBuffer = reasoningBuffer
            , renderColor = useColor
            , renderPrintedText = printed
            , renderTextBuffer = textBuffer
            , renderMarkdownState = markdownState
            , renderLiveActive = liveActive
            , renderLock = ioLock
            , renderStdout = stdout
            , renderStderr = stderr
            , renderModelRef = modelRef
            , renderActivityRef = activityRef
            , renderStartedAt = startedAtRef
            , renderToolCalls = toolCallsRef
            -- OSC 9;4 is ignored by terminals that do not implement it.
            -- Gate on the same TTY check as the in-pane spinner so pipes
            -- and redirected stderr stay clean.
            , renderNativeProgress =
                stderrTty
                    && terminal.terminalNativeProgress
                    && nativeProgressAnimationEnabled
                        options.optMotionMode
            , renderMotionMode = options.optMotionMode
            }
        emitLoop event = case fullscreen of
            Nothing -> renderEvent render event
            Just runtime -> do
                case event of
                    TurnStarted -> do
                        now <- getCurrentTime
                        writeIORef startedAtRef (Just now)
                        writeIORef activityRef "Thinking…"
                    TextDelta _ -> writeIORef printed True
                    ToolStarted _ ->
                        writeIORef activityRef "Running tool…"
                    _ -> pure ()
                emitUiEvent runtime (UiLoop event)
        config = LoopConfig
            { loopBackend = backend
            , loopTools = toolRegistry
            , loopDispatch = defaultLoopDispatch
            , loopMaxTurns = options.optMaxTurns
            , loopOnEvent = emitLoop
            , loopApprove = \call ->
                withMVar ioLock \_ ->
                    case fullscreen of
                        Nothing ->
                            withStdinPaused escPaused $
                                approveToolDecision
                                    policyRef allowedToolsRef toolRegistry planMode call
                        Just runtime ->
                            approveToolDecisionWith
                                (requestFullscreenPermission runtime)
                                policyRef
                                allowedToolsRef
                                toolRegistry
                                planMode
                                call
            , loopCancel = toolEnv.toolCancel
            }
        beginSubagentTurn = do
            case multiCtx of
                Nothing -> pure Nothing
                Just ctx -> do
                    rootTurnId <- beginRootTurn ctx.multiRegistry
                    writeIORef rootTurnRef (Just rootTurnId)
                    pure (Just rootTurnId)
        finishSubagentTurn rootTurnId =
            atomicModifyIORef' rootTurnRef \current ->
                (if current == rootTurnId then Nothing else current, ())
        abortSubagentTurn rootTurnId = do
            case rootTurnId of
                Just owned -> case multiCtx of
                    Just ctx -> abortRootTurn ctx.multiRegistry owned
                    Nothing -> pure ()
                Nothing -> pure ()
            finishSubagentTurn rootTurnId
        env = SessionEnv
            { sessionLoop = config
            , sessionBtwBackend = btwBackend
            , sessionCompact = compactRunner
            , sessionRender = render
            , sessionProvider = provider
            , sessionUnavailableProviders = unavailableProvidersRef
            , sessionStartupUnavailable = startupUnavailableRef
            , sessionPrevious = previous
            , sessionPrinted = printed
            , sessionParams = paramsRef
            , sessionPolicy = policyRef
            , sessionTranscript = transcriptRef
            , sessionPersist = persist
            , sessionTitleManager = titleManager
            , sessionTitleTurnCount = titleTurnCount
            , sessionPlanMode = planMode
            , sessionProjectRoot = projectRoot
            , sessionCwd = cwd
            , sessionHome = home
            , sessionTokenProvider = tokenProvider
            , sessionOpenAiPool = openAiPool
            , sessionStartupContext = startupContext
            , sessionSkills = skillsRef
            , sessionSkillInvocations = skillInvocationsRef
            , sessionRefreshSkills = refreshSkills
            , sessionEscPaused = escPaused
            , sessionAttachments = attachmentsRef
            , sessionPreviewId = previewIdRef
            , sessionInterrupt = interrupt
            , sessionRestartEffort = restartEffortRef
            , sessionStoreRoot = storeRoot
            , sessionUsage = usageRef
            , sessionAccount = accountRef
            , sessionAccountId = accountIdRef
            , sessionAccountSelectionId = selectionRef
            , sessionAccountLabel = accountLabel
            , sessionSelectAccount = selectAccount
            , sessionLastAssistant = lastAssistantRef
            , sessionTerminal = terminal
            , sessionFullscreen = fullscreen
            , sessionSetWindowTitle = setWindowTitle
            , sessionAgentViewport = Just agentViewport
            , sessionBeginSubagentTurn = beginSubagentTurn
            , sessionFinishSubagentTurn = finishSubagentTurn
            , sessionAbortSubagentTurn = abortSubagentTurn
            , sessionOnPersisted = onPersisted
            , sessionReset = sessionReset
            }
    writeIORef startup.startupRestartEffort \level -> do
        setSessionEffort env level
        writeIORef restartEffortRef (Just level)
        requestCancel toolEnv.toolCancel
    let formatSkillWarning warning =
            "skill ignored: "
                <> toText warning.skillWarningPath
                <> ": "
                <> warning.skillWarningMessage
        initializeSkills = do
            markStartupStage startup "Loading skills…"
            skills <- loadSkillsCatalog
                options home projectRoot cwd (isNothing fullscreen)
            installSkillCatalog
                reservedSlashNames
                (null initialTurns && not (isJust initialPrevious))
                startupContext skillsRef skillInvocationsRef skills
            case fullscreen of
                Nothing -> pure ()
                Just runtime -> do
                    mapM_
                        (emitUiEvent runtime . UiSystemMessage . formatSkillWarning)
                        skills.catalogWarnings
            finishStartup startup
        sessionAction = do
            initializeSkills
            case pendingTurn of
                Just pending ->
                    runPendingTurn
                        (if startup.startupFullscreenReused
                            then ContinuePendingTurn
                            else SubmitPendingTurn)
                        env
                        pending
                Nothing -> case prompt of
                    Just text -> do
                        inputs <- preparePromptSkillInputs env text [UserMessage text]
                            >>= either (die . Text.unpack) pure
                        result <- runOneTurn env text inputs
                        finishTurn env True result
                    Nothing ->
                        replWithDraft env initialDraft
    result <- sessionAction
    _ <- waitForSessionTitleResults 5000000 titleManager
    applyPendingSessionTitles env
    pure result

loadStartupAuth
    :: StartupRuntime
    -> Maybe ProviderTransition
    -> Maybe Provider
    -> IO LoadedAuth
loadStartupAuth startup transition requestedProvider =
    loadAuth requestedProvider >>= \case
        Right loaded -> pure loaded
        Left err
            | isNothing transition
            , authErrorNeedsOnboarding err
            , Just runtime <- startup.startupFullscreen ->
                runCredentialOnboarding startup runtime >>= \provider ->
                    loadAuth (Just provider)
                        >>= either (startupDie startup . Text.unpack) pure
            | otherwise ->
                startupDie startup (Text.unpack err)

runCredentialOnboarding
    :: StartupRuntime
    -> FullscreenRuntime
    -> IO Provider
runCredentialOnboarding startup runtime = do
    markStartupStage startup "Choose how to connect…"
    loop
  where
    choices =
        [ ( OpenAIProvider
          , ("Sign in with ChatGPT", "Use an OpenAI subscription")
          )
        , ( XAIProvider
          , ("Sign in with Grok", "Use an xAI subscription")
          )
        , ( OpenRouterProvider
          , ("Add an OpenRouter API key", "Use API credits")
          )
        ]
    loop =
        requestFullscreenOnboarding
            runtime
            "Welcome to haskell-agent"
            "haskell-agent can access AI models with a subscription or API key."
            (map snd choices)
            >>= \case
                Nothing -> throwIO StartupCancelled
                Just index ->
                    case atMay index choices of
                        Nothing -> loop
                        Just (provider, _) -> do
                            connected <-
                                withFullscreenSuspended runtime $
                                    resolveColor stderr >>= \color ->
                                        connectProviderAccount color provider
                            case connected of
                                Nothing -> loop
                                Just _ -> pure provider

runPendingTurn
    :: PendingTurnPresentation
    -> SessionEnv
    -> PendingTurn
    -> IO RunResult
runPendingTurn presentation =
    runPendingTurnWithCooldownRetry True presentation

runPendingTurnWithCooldownRetry
    :: Bool
    -> PendingTurnPresentation
    -> SessionEnv
    -> PendingTurn
    -> IO RunResult
runPendingTurnWithCooldownRetry
    allowCooldownRetry presentation env pending = do
    writeIORef env.sessionPlanMode.planStateRef pending.pendingPlanState
    case env.sessionFullscreen of
        Nothing -> pure ()
        Just runtime -> case presentation of
            SubmitPendingTurn ->
                emitUiEvent runtime
                    (UiUserSubmitted pending.pendingPromptText)
            RestartPendingTurn ->
                emitUiEvent runtime UiTurnRestarted
            ContinuePendingTurn ->
                pure ()
    result <- runOneTurn env pending.pendingPromptText pending.pendingInputs
    finishTurnWithCooldownRetry
        allowCooldownRetry env pending.pendingExitAfter result

finishTurn
    :: SessionEnv
    -> Bool
    -> TurnResult
    -> IO RunResult
finishTurn = finishTurnWithCooldownRetry True

finishTurnWithCooldownRetry
    :: Bool
    -> SessionEnv
    -> Bool
    -> TurnResult
    -> IO RunResult
finishTurnWithCooldownRetry allowCooldownRetry env exitAfter = \case
    TurnSucceeded -> do
        writeIORef env.sessionUnavailableProviders []
        case env.sessionFullscreen of
            Nothing -> putTrailingNewline env.sessionPrinted
            Just _ -> pure ()
        if exitAfter
            then pure RunQuit
            else continueAfterTurn env
    TurnFailed ->
        if exitAfter
            then exitFailure
            else do
                case env.sessionFullscreen of
                    Nothing -> putTrailingNewline env.sessionPrinted
                    Just _ -> pure ()
                continueAfterTurn env
    TurnRestartRequested level pending -> do
        setSessionEffort env level
        writeIORef env.sessionPlanMode.planStateRef pending.pendingPlanState
        case env.sessionFullscreen of
            Just runtime ->
                emitUiEvent runtime
                    (UiSystemMessage
                        ("restarting current turn with " <> level <> " effort"))
            Nothing -> pure ()
        result <- runOneTurn env pending.pendingPromptText pending.pendingInputs
        finishTurnWithCooldownRetry allowCooldownRetry env exitAfter result
    TurnProviderUnavailable apiError pending ->
        let pending' = setPendingExitAfter exitAfter pending
        in requestAutomaticProviderFallback env apiError pending' >>= \case
            Just providerTransition ->
                pure (RunSwitchProvider providerTransition)
            Nothing -> do
                now <- getCurrentTime
                case automaticCooldownRetryDelay now apiError of
                    Just delay | allowCooldownRetry ->
                        waitAndRetryPendingTurn env delay pending'
                    _ -> do
                        reportProviderUnavailable
                            env.sessionFullscreen apiError
                        if exitAfter
                            then exitFailure
                            else do
                                notifyAttention stderr InputRequested
                                replWithDraft env pending.pendingPromptText

continueAfterTurn :: SessionEnv -> IO RunResult
continueAfterTurn env = do
    queued <- case env.sessionFullscreen of
        Nothing -> pure False
        Just runtime -> hasQueuedFullscreenInput runtime
    when (not queued) $
        notifyAttention stderr InputRequested
    repl env

waitAndRetryPendingTurn
    :: SessionEnv
    -> NominalDiffTime
    -> PendingTurn
    -> IO RunResult
waitAndRetryPendingTurn env delay pending = do
    let waitMessage =
            "provider credentials temporarily unavailable; retrying this turn in "
                <> formatDuration delay
                <> " (Esc to cancel)"
    case env.sessionFullscreen of
        Just runtime ->
            emitUiEvent runtime
                (UiSetNotice (Just (warningNotice waitMessage)))
        Nothing -> do
            color <- resolveColor stderr
            putTextLn stderr $
                roleWarn color (glyphWarn <> waitMessage)
    let cancel = env.sessionLoop.loopCancel
        -- Give the provider reset boundary a small margin so the automatic
        -- retry does not race a rounded server timestamp.
        waitMicros = max 1 (ceiling ((realToFrac delay + 0.25) * 1_000_000 :: Double))
        waitForCancel =
            isJust <$> timeout waitMicros (waitCancel cancel)
        waitAction = case env.sessionFullscreen of
            Just _ -> waitForCancel
            Nothing ->
                withEscCancel cancel env.sessionEscPaused waitForCancel
    resetCancel cancel
    cancelled <-
        (withTurnCancel env.sessionInterrupt cancel waitAction)
            `finally` resetCancel cancel
    if cancelled
        then do
            case env.sessionFullscreen of
                Just runtime ->
                    emitUiEvent runtime
                        (UiSetNotice
                            (Just
                                (infoNotice
                                    "automatic retry cancelled")))
                Nothing -> do
                    color <- resolveColor stderr
                    putTextLn stderr
                        (roleMuted color "automatic retry cancelled")
            if pending.pendingExitAfter
                then pure RunQuit
                else replWithDraft env pending.pendingPromptText
        else do
            case env.sessionFullscreen of
                Just runtime ->
                    emitUiEvent runtime
                        (UiSetNotice
                            (Just (successNotice "retrying turn")))
                Nothing -> do
                    color <- resolveColor stderr
                    putTextLn stderr
                        (roleMuted color (glyphOk <> "retrying turn"))
            runPendingTurnWithCooldownRetry
                False RestartPendingTurn env pending

reportProviderUnavailable
    :: Maybe FullscreenRuntime
    -> ApiError
    -> IO ()
reportProviderUnavailable fullscreen apiError = do
    now <- getCurrentTime
    let message =
            "No usable fallback provider account is available.\n"
            <> formatApiErrorAt now apiError
    case fullscreen of
        Nothing -> do
            color <- resolveColor stderr
            putTextLn stderr (roleError color message)
        Just runtime ->
            emitUiEvent runtime (UiErrorMessage message)

setSessionEffort :: SessionEnv -> Text -> IO ()
setSessionEffort env level = do
    modifyIORef' env.sessionParams (setReasoningEffort level)
    case env.sessionFullscreen of
        Just runtime ->
            emitUiEvent runtime (UiSetPromptEffort level)
        Nothing -> pure ()
    case env.sessionPersist of
        PersistenceDisabled -> pure ()
        PersistenceEnabled slotRef -> do
            slot <- readIORef slotRef
            case slot of
                PersistencePending pending ->
                    writeIORef slotRef
                        (PersistencePending pending { createEffort = level })
                PersistenceActive handle -> do
                    let meta = handle.sessionMeta { metaEffort = level }
                    writeSessionMeta handle.sessionMetaPath meta
                    writeIORef slotRef
                        (PersistenceActive handle { sessionMeta = meta })

repl :: SessionEnv -> IO RunResult
repl env = replWithDraft env ""

replWithDraft :: SessionEnv -> Text -> IO RunResult
replWithDraft env@SessionEnv
    { sessionBtwBackend = btwBackend
    , sessionCompact = compactRunner
    , sessionRender = render
    , sessionProvider = provider
    , sessionStartupUnavailable = startupUnavailableRef
    , sessionPrevious = previous
    , sessionPrinted = printed
    , sessionParams = paramsRef
    , sessionPolicy = policyRef
    , sessionTranscript = transcriptRef
    , sessionPersist = persist
    , sessionPlanMode = planMode
    , sessionProjectRoot = projectRoot
    , sessionCwd = cwd
    , sessionTokenProvider = tokenProvider
    , sessionOpenAiPool = openAiPool
    , sessionSkills = skillsRef
    , sessionSkillInvocations = skillInvocationsRef
    , sessionRefreshSkills = refreshSkills
    , sessionAttachments = attachmentsRef
    , sessionPreviewId = previewIdRef
    , sessionInterrupt = interrupt
    , sessionEscPaused = escPaused
    , sessionStoreRoot = storeRoot
    , sessionUsage = usageRef
    , sessionAccount = accountRef
    , sessionAccountSelectionId = selectionRef
    , sessionAccountLabel = accountLabel
    , sessionSelectAccount = selectAccount
    , sessionLastAssistant = lastAssistantRef
    , sessionTerminal = terminal
    , sessionFullscreen = fullscreen
    , sessionSetWindowTitle = setWindowTitle
    , sessionAgentViewport = agentViewport
    , sessionReset = sessionReset
    } draft = do
    refreshSkills False
    skillInvocations <- readIORef skillInvocationsRef
    let skillCommands =
            map skillInvocationCommand
                (filter (.invocationSkill.skillUserInvocable) skillInvocations)
    stdoutColor <- resolveColor stdout
    planState <- readIORef planMode.planStateRef
    let planActive = planState == PlanActive
        planPending = planState == PlanPending
    params <- readIORef paramsRef
    policy <- readIORef policyRef
    pendingAttachments <- readIORef attachmentsRef
    let idleMode = replModeFromState planState policy
    usage <- readIORef usageRef
    account <- readIORef accountRef
    mlineResult <- case fullscreen of
        Just runtime -> do
            setFullscreenImagePreviews runtime pendingAttachments
            let promptState =
                    PromptState
                    { promptModel = currentModel params
                    , promptEffort = currentEffort params
                    , promptMode = replModeLabel idleMode
                    , promptAccount = account
                    , promptAccountSelectable = isJust selectAccount
                    , promptUsage = usage
                    , promptAttachments = length pendingAttachments
                    }
            readIORef startupUnavailableRef >>= \case
                Nothing ->
                    Right
                        <$> readFullscreenLine
                            runtime skillCommands promptState draft
                Just unavailable ->
                    readFullscreenLineOr
                        runtime skillCommands promptState draft unavailable
        Nothing -> Right <$> withMVar render.renderLock \_ -> do
            -- The inline editor redraws its ANSI frame with several writes.
            -- Keep the renderer out for the complete prompt lifetime so a
            -- late tool event cannot be spliced into the composer row.
            when terminal.terminalSemanticPrompts $
                emitTerminalSequence terminal stdout osc133PromptStart
            termCols <- fmap snd <$> getTerminalSize
            case agentViewport of
                Nothing -> pure ()
                Just viewport -> do
                    entries <- viewport.viewportEntries
                    selected <- readIORef viewport.viewportSelected
                    let panel =
                            renderAgentViewportPanelFor
                                stdoutColor
                                (fromMaybe 100 termCols)
                                selected
                                entries
                    when (not (Text.null panel)) (Text.putStrLn panel)
            -- Status sits on the line above λ in minimal mode.
            withSynchronizedOutput terminal stdout do
                Text.putStrLn $ formatReplStatusLine stdoutColor termCols
                    (currentModel params)
                    (currentEffort params)
                    idleMode
                    account
                    usage
                hFlush stdout
            let modeTag
                    | planActive = roleWarn stdoutColor "[plan] "
                    | planPending = roleMuted stdoutColor "[plan…] "
                    | idleMode == ReplModeAlwaysApprove =
                        roleSuccess stdoutColor "[yolo] "
                    | otherwise = ""
                chromePrompt =
                    beginBackground stdoutColor userBackground
                        <> modeTag
                        <> if null pendingAttachments
                            then ""
                            else roleMuted stdoutColor
                                ("[📎 " <> Text.pack (show (length pendingAttachments)) <> "] ")
                        <> rolePrompt stdoutColor "λ "
                        <> if stdoutColor
                            then Text.pack clearFromCursorToLineEndCode
                            else mempty
            result <- readReplLineWithSkills
                skillCommands interrupt chromePrompt draft
            when terminal.terminalSemanticPrompts $
                emitTerminalSequence terminal stdout osc133PromptEnd
            Text.putStr (endBackground stdoutColor)
            hFlush stdout
            pure result
    case mlineResult of
        Left apiError -> do
            -- The startup check is one-shot. If no fallback account is usable,
            -- leave request-time error handling in charge of later submits.
            writeIORef startupUnavailableRef Nothing
            requestStartupProviderFallback env apiError >>= \case
                Just providerTransition ->
                    pure (RunSwitchProvider providerTransition)
                Nothing -> do
                    reportProviderUnavailable fullscreen apiError
                    replWithDraft env ""
        Right mline -> do
            -- Any user action wins the startup race. In particular, a prompt
            -- already submitted while the preflight was running proceeds on
            -- the selected provider and leaves request-time fallback in charge.
            writeIORef startupUnavailableRef Nothing
            handleReplLine
                skillCommands
                skillInvocations
                stdoutColor
                planState
                policy
                mline
  where
    handleReplLine skillCommands skillInvocations stdoutColor planState policy = \case
        ReplEof -> do
            putStrLn ""
            pure RunQuit
        ReplQuitInterrupt ->
            -- Confirmed double Ctrl-C: rethrow so withInterruptResume prints
            -- the --resume hint and the process exits.
            throwIO UserInterrupt
        ReplCycleMode keptDraft -> do
            let next = cycleReplInteraction planState policy
            applyReplMode planMode policyRef projectRoot next
            case fullscreen of
                Just runtime ->
                    emitUiEvent runtime $
                        UiSetNotice $
                            Just $
                                infoNotice
                                    ("Switched to "
                                        <> replModeLabel next
                                        <> " mode.")
                Nothing -> do
                    -- Minimal editor advanced a line; replace its old chrome.
                    putStr "\ESC[2A\r\ESC[J"
                    hFlush stdout
            continueWith keptDraft
        ReplClipboardPaste keptDraft clipboardPasteImages -> do
            case clipboardPasteImages of
                Just images@(_:_) -> do
                    message <- queueAttachedImages
                        attachmentsRef
                        previewIdRef
                        stdoutColor
                        (isNothing fullscreen)
                        images
                    syncFullscreenImagePreviews
                    displayInfo message $
                        Text.putStrLn
                            (roleMuted stdoutColor
                                (glyphOk <> message))
                _ ->
                    queueClipboardImages
                        attachmentsRef
                        previewIdRef
                        stdoutColor
                        (isNothing fullscreen)
                        >>= \case
                            Left err ->
                                displayError err do
                                    errColor <- resolveColor stderr
                                    Text.hPutStrLn stderr (roleError errColor err)
                            Right message -> do
                                syncFullscreenImagePreviews
                                displayInfo message $
                                    Text.putStrLn
                                        (roleMuted stdoutColor
                                            (glyphOk <> message))
            continueWith keptDraft
        ReplChooseModel keptDraft ->
            chooseModel keptDraft (continueWith keptDraft)
        ReplChooseEffort keptDraft ->
            chooseEffort (continueWith keptDraft)
        ReplChooseAccount keptDraft ->
            chooseAccount keptDraft (continueWith keptDraft)
        ReplPasted pasted ->
            submitLine skillCommands skillInvocations
                continue stdoutColor True pasted
        ReplText line ->
            submitLine skillCommands skillInvocations
                continue stdoutColor False line
    submitLine skillCommands skillInvocations continue color pasted line = do
        attachmentCount <- length <$> readIORef attachmentsRef
        case submissionPromptText attachmentCount line of
            Nothing -> continue
            Just promptLine -> do
                let stripped = Text.strip promptLine
                when pasted do
                    let chip = formatPasteChip stripped
                    when (chip /= stripped && isNothing fullscreen) do
                        Text.putStrLn (roleMuted color chip)
                case parseReplLineWithSkills skillCommands promptLine of
                    ReplQuit -> pure RunQuit
                    ReplReload -> requestReload persist
                    ReplPrompt text -> do
                        -- Native Cmd+V of a Finder image often pastes a path
                        -- rather than bitmap bytes. Treat a prompt that is
                        -- only image path(s) as an attach + in-terminal preview,
                        -- matching Grok Build's paste chip.
                        pastedImages <- loadImagesFromPastedText text
                        case pastedImages of
                            Just images@(_:_) -> do
                                message <- queueAttachedImages
                                    attachmentsRef
                                    previewIdRef
                                    color
                                    (isNothing fullscreen)
                                    images
                                syncFullscreenImagePreviews
                                displayInfo message $
                                    Text.putStrLn
                                        (roleMuted color
                                            (glyphOk <> message))
                                continue
                            _ -> do
                                pendingImages <- atomicModifyIORef' attachmentsRef \imgs -> ([], imgs)
                                forM_ fullscreen \runtime ->
                                    setFullscreenImagePreviews runtime []
                                writeIORef printed False
                                let turnInputs =
                                        if null pendingImages
                                            then [UserMessage text]
                                            else
                                                [ UserMultimodal
                                                    { userText = text
                                                    , userImages = pendingImages
                                                    }
                                                ]
                                preparePromptSkillInputs env text turnInputs >>= \case
                                    Left err -> do
                                        displayError err $
                                            Text.hPutStrLn stderr
                                                (roleError color err)
                                        continue
                                    Right skillInputs -> do
                                        fullscreenEvent (UiUserSubmitted text)
                                        result <- runOneTurn env text skillInputs
                                        finishTurn env False result
                    ReplInvokeSkill invocationName arguments ->
                        case resolveSkillInvocation
                            skillInvocations invocationName of
                            Left err -> do
                                displayError err $
                                    Text.hPutStrLn stderr
                                        (roleError color err)
                                continue
                            Right invocation -> do
                                pendingImages <- atomicModifyIORef'
                                    attachmentsRef \imgs -> ([], imgs)
                                forM_ fullscreen \runtime ->
                                    setFullscreenImagePreviews runtime []
                                let userText =
                                        if Text.null arguments
                                            then "Use the "
                                                <> invocation.invocationSkill.skillName
                                                <> " skill."
                                            else arguments
                                    userInput =
                                        if null pendingImages
                                            then UserMessage userText
                                            else UserMultimodal
                                                { userText = userText
                                                , userImages = pendingImages
                                                }
                                    skillInputs =
                                        [ UserMessage
                                            (formatSkillActivation
                                                invocation arguments)
                                        , userInput
                                        ]
                                writeIORef printed False
                                fullscreenEvent (UiUserSubmitted line)
                                result <- runOneTurn env line skillInputs
                                finishTurn env False result
                    ReplSkills reloadFirst -> do
                        when reloadFirst (refreshSkills True)
                        current <- readIORef skillsRef
                        invocations <- readIORef skillInvocationsRef
                        let listing =
                                formatSkillsListing color current invocations
                        displayInfo (formatSkillsListing False current invocations) $
                            Text.putStrLn listing
                        continue
                    ReplPaste pasteImmediate pasteCaption -> do
                        color <- resolveColor stdout
                        errColor <- resolveColor stderr
                        imagesResult <- readClipboardImages
                        case imagesResult of
                            Left err -> do
                                -- Fall back to a richer clipboard sniff for better errors.
                                content <- readClipboard
                                let
                                    message = case content of
                                        ClipboardText _ ->
                                            "clipboard has text, not an image \
                                            \(paste text normally into the prompt)"
                                        ClipboardPaths paths ->
                                            "clipboard has file path(s), \
                                            \but no loadable image: "
                                                <> Text.intercalate
                                                    ", " (map Text.pack paths)
                                        ClipboardEmpty ->
                                            err
                                        ClipboardImage image ->
                                            -- Shouldn't happen if readClipboardImages failed, but be safe.
                                            "attached "
                                                <> image.imageMime
                                case content of
                                    ClipboardImage image -> do
                                        attachMessage <- queueAttachedImages
                                            attachmentsRef
                                            previewIdRef
                                            color
                                            (isNothing fullscreen)
                                            [image]
                                        syncFullscreenImagePreviews
                                        displayInfo attachMessage $
                                            Text.putStrLn
                                                (roleMuted color
                                                    (glyphOk <> attachMessage))
                                    _ ->
                                        displayError message $
                                            Text.hPutStrLn stderr
                                                (roleError errColor message)
                                continue
                            Right [] -> do
                                displayError "no image found on the clipboard" $
                                    Text.hPutStrLn stderr
                                        (roleError errColor
                                            "no image found on the clipboard")
                                continue
                            Right images -> do
                                let sizes =
                                        Text.intercalate ", "
                                            [ img.imageMime <> " (" <> formatImageSize (BS.length img.imageBytes) <> ")"
                                            | img <- images
                                            ]
                                if pasteImmediate
                                    then do
                                        let promptText =
                                                if Text.null pasteCaption
                                                    then "See attached image."
                                                    else pasteCaption
                                        when (isNothing fullscreen) $
                                            putImagePreview previewIdRef color images
                                        displayInfo ("pasted " <> sizes) $
                                            Text.putStrLn
                                                (roleMuted color
                                                    (glyphOk <> "pasted " <> sizes))
                                        writeIORef printed False
                                        fullscreenEvent
                                            (UiUserSubmitted promptText)
                                        let turnInputs =
                                                [ UserMultimodal
                                                    { userText = promptText
                                                    , userImages = images
                                                    }
                                                ]
                                        result <- runOneTurn env promptText turnInputs
                                        finishTurn env False result
                                    else do
                                        message <- queueAttachedImages
                                            attachmentsRef
                                            previewIdRef
                                            color
                                            (isNothing fullscreen)
                                            images
                                        syncFullscreenImagePreviews
                                        displayInfo message $
                                            Text.putStrLn
                                                (roleMuted color
                                                    (glyphOk <> message))
                                        continue
                    ReplShowAttachments -> do
                        pending <- readIORef attachmentsRef
                        color <- resolveColor stdout
                        let message =
                                if null pending
                                    then "attachments: (none)"
                                    else "attachments: "
                                        <> Text.intercalate ", "
                                            [ img.imageMime
                                                <> " ("
                                                <> formatImageSize
                                                    (BS.length img.imageBytes)
                                                <> ")"
                                            | img <- pending
                                            ]
                        displayInfo message $
                            Text.putStrLn
                                (roleMuted color (glyphSession <> message))
                        continue
                    ReplClearAttachments -> do
                        writeIORef attachmentsRef []
                        forM_ fullscreen \runtime ->
                            setFullscreenImagePreviews runtime []
                        color <- resolveColor stdout
                        displayInfo "attachments cleared" $
                            Text.putStrLn
                                (roleMuted color
                                    (glyphOk <> "attachments cleared"))
                        continue
                    ReplAgents -> do
                        case agentViewport of
                            Nothing -> continue
                            Just viewport -> do
                                entries <- viewport.viewportEntries
                                selected <- readIORef viewport.viewportSelected
                                color <- resolveColor stderr
                                pickAgentChoice
                                    fullscreen color selected entries >>= \case
                                    Nothing -> pure ()
                                    Just target ->
                                        writeIORef viewport.viewportSelected target
                                continue

                    ReplCopyLast -> do
                        answer <- readIORef lastAssistantRef
                        copyCommand
                            "last response"
                            "no assistant response to copy"
                            answer
                        continue
                    ReplCopyCode index -> do
                        answer <- readIORef lastAssistantRef
                        let label =
                                "code block " <> Text.pack (show index)
                        copyCommand
                            label
                            (label <> " was not found")
                            (answer >>= fencedCodeBlock index)
                        continue
                    ReplCopyDiff -> do
                        answer <- readIORef lastAssistantRef
                        copyCommand
                            "diff block"
                            "no diff block was found"
                            (answer >>= lastDiffBlock)
                        continue
                    ReplCopyPath -> do
                        copyCommand
                            "worktree path"
                            "worktree path is unavailable"
                            (Just (toText cwd))
                        continue
                    ReplCopySession -> do
                        sessionId <- currentSessionId persist
                        copyCommand
                            "session id"
                            "this session has no persisted id yet"
                            sessionId
                        continue
                    ReplShowTerminal -> do
                        let message = formatTerminalCapabilities terminal
                        displayInfo message $
                            Text.putStrLn (roleMuted color message)
                        continue
                    ReplShowEffort -> do
                        color <- resolveColor stdout
                        params <- readIORef paramsRef
                        let message = "effort: " <> currentEffort params
                        displayInfo message $
                            Text.putStrLn
                                (roleMuted color (glyphSession <> message))
                        continue
                    ReplSetEffort level -> do
                        setEffort level
                        continue
                    ReplShowModel -> do
                        chooseModel "" continue
                    ReplSetModel name -> do
                        color <- resolveColor stdout
                        message <- applyModelChange
                            projectRoot provider name paramsRef render previous persist
                        displayInfo message $
                            Text.putStrLn
                                (roleMuted color (glyphOk <> message))
                        continue
                    ReplToggleAlwaysApprove -> do
                        message <- toggleAlwaysApprove policyRef projectRoot
                        color <- resolveColor stderr
                        displayInfo message $
                            putTextLn stderr (roleMuted color message)
                        continue
                    ReplCompact focus -> do
                        color <- resolveColor stderr
                        result <-
                            withReplActivity "Compacting context…" $
                                compactRunner focus
                        case result of
                            Left err -> do
                                displayError err $
                                    Text.hPutStrLn stderr (roleError color err)
                                continue
                            Right outcome -> do
                                fullscreenEvent UiConversationCleared
                                fullscreenEvent
                                    (UiSystemMessage outcome.compactSummary)
                                let message =
                                        "compacted "
                                            <> Text.pack
                                                (show outcome.compactBeforeTokens)
                                            <> " → "
                                            <> Text.pack
                                                (show outcome.compactAfterTokens)
                                            <> " tokens ("
                                            <> Text.pack
                                                (show (length outcome.compactHistory))
                                            <> " items)"
                                displayInfo message $
                                    Text.hPutStrLn stderr
                                        (roleMuted color
                                            (glyphSession <> message))
                                case persist of
                                    PersistenceDisabled -> pure ()
                                    PersistenceEnabled slotRef -> do
                                        now <- getCurrentTime
                                        handle <- ensureSession slotRef
                                        let turn = SessionTurn
                                                { turnAt = now
                                                , turnUserText = compactSessionUserText focus
                                                , turnAssistantText = Just outcome.compactSummary
                                                , turnError = Nothing
                                                , turnResponseId = Nothing
                                                , turnItems = outcome.compactHistory
                                                -- Compaction response usage is
                                                -- recorded immediately by
                                                -- compactRunner, including
                                                -- response-level failures.
                                                , turnUsage = Nothing
                                                }
                                        handle' <-
                                            appendTurnWithMetaUpdate handle turn
                                                \meta -> meta
                                                    { metaLastResponseId = Nothing
                                                    }
                                        writeIORef slotRef
                                            (PersistenceActive handle')
                                continue
                    ReplPlan maybeDescription ->
                        enterPlanFromSlash env maybeDescription >>= \case
                            Just providerSwitch ->
                                pure (RunSwitchProvider providerSwitch)
                            Nothing -> continue
                    ReplBtw question -> do
                        color <- resolveColor stdout
                        fullscreenEvent
                            (UiSetNotice
                                (Just
                                    (progressNotice
                                        "btw · asking…")))
                        result <-
                            runBtwWithCancel
                                (\cancel action ->
                                    withTurnCancel interrupt cancel $
                                        case fullscreen of
                                            Nothing ->
                                                withEscCancel
                                                    cancel escPaused action
                                            Just _ -> action)
                                btwBackend
                                paramsRef
                                transcriptRef
                                question
                        case result of
                            Left err -> do
                                errorColor <- resolveColor stderr
                                let message = formatBtwError err
                                displayError message $
                                    putTextLn stderr
                                        (roleError errorColor message)
                            Right answer ->
                                case fullscreen of
                                    Just runtime ->
                                        emitUiEvent runtime
                                            (UiAssistantHistory answer)
                                    Nothing ->
                                        putTextLn stdout
                                            (renderAssistantText color answer)
                        continue
                    ReplResume maybeId -> do
                        handleResume fullscreen maybeId persist >>= \case
                            Nothing -> continue
                            Just result -> pure result
                    ReplClear -> do
                        sessionReset
                        fullscreenEvent UiConversationCleared
                        color <- resolveColor stderr
                        message <- case persist of
                            PersistenceDisabled ->
                                pure "conversation cleared"
                            PersistenceEnabled slotRef -> do
                                now <- getCurrentTime
                                slot <- readIORef slotRef
                                case slot of
                                    PersistencePending _ ->
                                        pure "conversation cleared"
                                    PersistenceActive handle -> do
                                        let turn = SessionTurn
                                                { turnAt = now
                                                , turnUserText = clearSessionUserText
                                                , turnAssistantText =
                                                    Just "Conversation cleared."
                                                , turnError = Nothing
                                                , turnResponseId = Nothing
                                                , turnItems = []
                                                , turnUsage = Nothing
                                                }
                                        handle' <- appendTurnKeepTitle handle turn
                                        let meta = handle'.sessionMeta
                                                { metaLastResponseId = Nothing
                                                , metaUpdatedAt = now
                                                , metaInputTokens = 0
                                                , metaOutputTokens = 0
                                                , metaCachedTokens = 0
                                                }
                                        writeSessionMeta handle'.sessionMetaPath meta
                                        writeIORef slotRef
                                            (PersistenceActive handle'{sessionMeta = meta})
                                        pure
                                            ("conversation cleared (session "
                                                <> meta.metaId
                                                <> ")")
                        displayInfo message $
                            Text.hPutStrLn stderr
                                (roleMuted color (glyphOk <> message))
                        continue
                    ReplNew -> do
                        sessionReset
                        fullscreenEvent UiConversationCleared
                        color <- resolveColor stderr
                        case persist of
                            PersistenceDisabled -> do
                                displayInfo "started a fresh conversation" $
                                    Text.hPutStrLn stderr
                                        (roleMuted color
                                            (glyphOk
                                                <> "started a fresh conversation"))
                                continue
                            PersistenceEnabled slotRef -> do
                                now <- getCurrentTime
                                params <- readIORef paramsRef
                                slot <- readIORef slotRef
                                let model = currentModel params
                                    effort = currentEffort params
                                    create = case slot of
                                        PersistencePending pending ->
                                            pending
                                                { createModel = model
                                                , createEffort = effort
                                                , createTitleHint = Nothing
                                                , createTitleIsManual = False
                                                }
                                        PersistenceActive handle ->
                                            SessionCreate
                                                { createRoot =
                                                    takeDirectory handle.sessionDir
                                                , createProvider = provider
                                                , createModel = model
                                                , createCwd =
                                                    handle.sessionMeta.metaCwd
                                                , createEffort = effort
                                                , createTitleHint = Nothing
                                                , createTitleIsManual = False
                                                }
                                handle <- createSession create
                                let turn = SessionTurn
                                        { turnAt = now
                                        , turnUserText = newSessionUserText
                                        , turnAssistantText =
                                            Just "Started a new session."
                                        , turnError = Nothing
                                        , turnResponseId = Nothing
                                        , turnItems = []
                                        , turnUsage = Nothing
                                        }
                                handle' <- appendTurnKeepTitle handle turn
                                let meta = handle'.sessionMeta
                                        { metaLastResponseId = Nothing
                                        , metaUpdatedAt = now
                                        }
                                writeSessionMeta handle'.sessionMetaPath meta
                                env.sessionOnPersisted handle'
                                writeIORef slotRef
                                    (PersistenceActive handle'{sessionMeta = meta})
                                writeIORef env.sessionTitleTurnCount 0
                                writeIORef planMode.planSessionDir
                                    (Just handle'.sessionDir)
                                writeIORef storeRoot (Just handle'.sessionDir)
                                setWindowTitle
                                    (cliWindowTitle meta.metaCwd
                                        (Just meta.metaTitle))
                                let message = "new session: " <> meta.metaId
                                displayInfo message $
                                    Text.hPutStrLn stderr
                                        (roleMuted color
                                            (glyphOk <> message))
                                continue
                    ReplShowSession -> do
                        color <- resolveColor stdout
                        case persist of
                            PersistenceDisabled ->
                                displayInfo "session: (not persisted)" $
                                    Text.putStrLn
                                        (roleMuted color
                                            "session: (not persisted)")
                            PersistenceEnabled slotRef -> do
                                slot <- readIORef slotRef
                                case slot of
                                    PersistencePending _ ->
                                        displayInfo
                                            "session: (pending until first turn)" $
                                            Text.putStrLn
                                                (roleMuted color
                                                    "session: (pending until first turn)")
                                    PersistenceActive handle ->
                                        let message =
                                                "session: "
                                                    <> handle.sessionMeta.metaId
                                        in displayInfo message $
                                            Text.putStrLn
                                                (roleMuted color
                                                    (glyphSession <> message))
                        continue
                    ReplRename title -> do
                        color <- resolveColor stderr
                        case persist of
                            PersistenceDisabled ->
                                displayError
                                    "cannot rename a session that is not persisted" $
                                    putTextLn stderr
                                        (roleError color
                                            "cannot rename a session that is not persisted")
                            PersistenceEnabled slotRef ->
                                readIORef slotRef >>= \case
                                    PersistencePending pending -> do
                                        writeIORef slotRef (PersistencePending pending
                                            { createTitleHint = Just title
                                            , createTitleIsManual = True
                                            })
                                        setWindowTitle
                                            (cliWindowTitle pending.createCwd
                                                (Just title))
                                        let message = "session title: " <> title
                                        displayInfo message $
                                            putTextLn stderr
                                                (roleMuted color
                                                    (glyphOk <> message))
                                    PersistenceActive handle -> do
                                        invalidateSessionTitles
                                            env.sessionTitleManager
                                            handle.sessionMeta.metaId
                                        updated <- setManualSessionTitle title handle
                                        writeIORef slotRef (PersistenceActive updated)
                                        setWindowTitle
                                            (cliWindowTitle updated.sessionMeta.metaCwd
                                                (Just updated.sessionMeta.metaTitle))
                                        let message =
                                                "session title: "
                                                    <> updated.sessionMeta.metaTitle
                                        displayInfo message $
                                            putTextLn stderr
                                                (roleMuted color
                                                    (glyphOk <> message))
                        continue
                    ReplRenameAuto -> do
                        color <- resolveColor stderr
                        case persist of
                            PersistenceDisabled ->
                                displayError
                                    "cannot rename a session that is not persisted" $
                                    putTextLn stderr
                                        (roleError color
                                            "cannot rename a session that is not persisted")
                            PersistenceEnabled slotRef ->
                                readIORef slotRef >>= \case
                                    PersistencePending pending -> do
                                        writeIORef slotRef (PersistencePending pending
                                            { createTitleHint = Nothing
                                            , createTitleIsManual = False
                                            })
                                        setWindowTitle
                                            (cliWindowTitle pending.createCwd Nothing)
                                        displayInfo
                                            "automatic session titles enabled" $
                                            putTextLn stderr
                                                (roleMuted color
                                                    (glyphOk
                                                        <> "automatic session titles enabled"))
                                    PersistenceActive handle -> do
                                        invalidateSessionTitles
                                            env.sessionTitleManager
                                            handle.sessionMeta.metaId
                                        updated <- resetSessionTitleToAuto handle
                                        writeIORef slotRef (PersistenceActive updated)
                                        loadSession
                                            (takeDirectory updated.sessionDir)
                                            updated.sessionMeta.metaId
                                            >>= \case
                                                Left _ -> pure ()
                                                Right (_, turns) -> do
                                                    let source =
                                                            sessionConversationText turns
                                                    requestSessionTitle
                                                        env.sessionTitleManager
                                                        updated.sessionMeta.metaId
                                                        1
                                                        source
                                        displayInfo
                                            "automatic session titles enabled" $
                                            putTextLn stderr
                                                (roleMuted color
                                                    (glyphOk
                                                        <> "automatic session titles enabled"))
                        continue
                    ReplLogin -> do
                        color <- resolveColor stderr
                        legacy (runLoginManager color)
                        continue
                    ReplUsage -> do
                        case fullscreen of
                            Nothing ->
                                showAccountUsage
                                    provider tokenProvider openAiPool
                            Just runtime ->
                                accountUsageText
                                    False provider tokenProvider openAiPool
                                    >>= emitUiEvent runtime . UiSystemMessage
                        continue
                    ReplReloadAuth -> do
                        reloadResult <- reloadAuth provider tokenProvider
                        color <- resolveColor stderr
                        case reloadResult of
                            Left err ->
                                displayError err $
                                    putTextLn stderr (roleError color err)
                            Right message ->
                                displayInfo message $
                                    putTextLn stderr (roleMuted color message)
                        continue
                    ReplHelp maybeName -> do
                        color <- resolveColor stdout
                        displayInfo
                            (formatSlashHelpWithSkills
                                False skillCommands maybeName) $
                            Text.putStrLn
                                (formatSlashHelpWithSkills
                                    color skillCommands maybeName)
                        continue
                    ReplCommandError err -> do
                        color <- resolveColor stderr
                        displayError err $
                            Text.hPutStrLn stderr (roleError color err)
                        continue
    continue = continueWith ""
    continueWith keptDraft =
        replWithDraft env keptDraft
    legacy action = case fullscreen of
        Nothing -> action
        Just runtime -> withFullscreenSuspended runtime action
    fullscreenEvent event = case fullscreen of
        Nothing -> pure ()
        Just runtime -> emitUiEvent runtime event
    syncFullscreenImagePreviews =
        forM_ fullscreen \runtime ->
            readIORef attachmentsRef
                >>= setFullscreenImagePreviews runtime
    displayInfo message minimalAction = case fullscreen of
        Nothing -> minimalAction
        Just runtime -> emitUiEvent runtime (UiSystemMessage message)
    displayError message minimalAction = case fullscreen of
        Nothing -> minimalAction
        Just runtime -> emitUiEvent runtime (UiErrorMessage message)
    withReplActivity message action = do
        case fullscreen of
            Nothing ->
                renderEvent render (ActivityUpdated message)
            Just runtime ->
                emitUiEvent runtime
                    (UiSetNotice (Just (progressNotice message)))
        action `finally`
            case fullscreen of
                Nothing -> clearThinking render
                Just runtime -> emitUiEvent runtime (UiSetNotice Nothing)
    setEffort level = do
        color <- resolveColor stdout
        setSessionEffort env level
        displayInfo ("effort set to " <> level) $
            Text.putStrLn
                (roleMuted color
                    (glyphOk <> "effort set to " <> level))
    chooseEffort next = do
        params <- readIORef paramsRef
        effortChoice fullscreen (currentEffort params) >>= \case
            Nothing -> next
            Just level -> setEffort level >> next
    chooseModel keptDraft next = do
        color <- resolveColor stderr
        params <- readIORef paramsRef
        let current = currentModel params
        modelChoice fullscreen color provider current >>= \case
            Nothing -> next
            Just choice
                | choice.modelProvider == provider
                , choice.modelId == current -> do
                    let message =
                            "model: "
                                <> providerSlug provider
                                <> "/"
                                <> choice.modelId
                    displayInfo message $
                        Text.putStrLn
                            (roleMuted color
                                (glyphSession <> message))
                    next
                | choice.modelProvider == provider -> do
                    message <- applyModelChange
                        projectRoot provider choice.modelId paramsRef render previous persist
                    displayInfo message $
                        Text.putStrLn
                            (roleMuted color
                                (glyphOk <> message))
                    next
                | otherwise ->
                    requestModelProviderSwitch
                        fullscreen choice keptDraft persist >>= \case
                        Left err -> do
                            displayError err $
                                Text.hPutStrLn stderr
                                    (roleError color err)
                            next
                        Right result -> pure result
    chooseAccount _keptDraft next =
        case (fullscreen, selectAccount) of
            (Just runtime, Just select) -> do
                currentSelectionId <- readIORef selectionRef
                options <- withReplActivity
                    "Loading account usage…"
                    (case openAiPool of
                        Just pool ->
                            loadAccountPickerOptions accountLabel pool
                        Nothing -> case tokenProvider of
                            Just active ->
                                loadProviderAccountPickerOptions
                                    provider
                                    (tokenProviderBillingMode active)
                                    currentSelectionId
                            Nothing ->
                                pure [AccountPickerConnect])
                let initial =
                        fromMaybe 0 $
                            findIndex
                                (accountPickerMatches currentSelectionId)
                                options
                requestFullscreenChoiceWithBody
                    runtime
                    "Account"
                    ("Choose the "
                        <> providerSlug provider
                        <> " account for future requests.")
                    initial
                    (map (accountPickerRow currentSelectionId) options)
                    >>= \case
                        Just index
                            | Just option <- atMay index options ->
                                case option of
                                    AccountPickerAccount
                                        selectedSelectionId
                                        _
                                        selectedLabel
                                        _
                                            | selectedSelectionId
                                                == currentSelectionId ->
                                                displayInfo
                                                    ("account: " <> selectedLabel)
                                                    (pure ())
                                                    >> next
                                            | otherwise ->
                                                chooseSelectedAccount
                                                    selectedSelectionId
                                    AccountPickerConnect -> do
                                        color <- resolveColor stderr
                                        connected <-
                                            withFullscreenSuspended runtime $
                                                connectProviderAccount
                                                    color
                                                    provider
                                        case connected of
                                            Nothing -> next
                                            Just selectedAccountId -> do
                                                refreshed <-
                                                    loadProviderAccountPickerOptions
                                                        provider
                                                        (maybe
                                                            SubscriptionBilled
                                                            tokenProviderBillingMode
                                                            tokenProvider)
                                                        currentSelectionId
                                                let connectedSelection =
                                                        [ selectionId
                                                        | AccountPickerAccount
                                                            selectionId
                                                            accountId
                                                            _
                                                            _ <- refreshed
                                                        , accountId
                                                            == selectedAccountId
                                                        ]
                                                chooseSelectedAccount $
                                                    fromMaybe
                                                        selectedAccountId
                                                        (listToMaybe
                                                            connectedSelection)
                        _ -> next
              where
                chooseSelectedAccount selectedId =
                    select selectedId >>= \case
                        Left err -> do
                            now <- getCurrentTime
                            let message =
                                    "could not select account: "
                                        <> formatApiErrorInlineAt
                                            now
                                            err
                            displayError message (pure ())
                            next
                        Right label -> do
                            displayInfo
                                ("account switched to " <> label)
                                (pure ())
                            next
            _ -> do
                displayError
                    "Account switching is unavailable for this session."
                    (pure ())
                next
    copyCommand label missing payload = case payload of
        Nothing ->
            displayError missing do
                color <- resolveColor stderr
                Text.hPutStrLn stderr (roleError color missing)
        Just value -> do
            copied <- copyTerminalClipboard terminal stdout value
            if copied
                then
                    let message = "copied " <> label
                    in displayInfo message do
                        color <- resolveColor stderr
                        Text.hPutStrLn stderr
                            (roleSuccess color (glyphOk <> message))
                else
                    displayError "terminal clipboard is unavailable" do
                        color <- resolveColor stderr
                        Text.hPutStrLn stderr
                            (roleError color
                                "terminal clipboard is unavailable")

modelChoice
    :: Maybe FullscreenRuntime
    -> Bool
    -> Provider
    -> Text
    -> IO (Maybe ModelOption)
modelChoice fullscreen color provider current = case fullscreen of
    Nothing -> pickModel color provider current
    Just runtime -> do
        let picker = initialPickerState provider current
            options = picker.pickerAll
            rows =
                [ ( providerSlug option.modelProvider
                        <> " · "
                        <> option.modelId
                  , fromMaybe "" option.modelLabel
                  )
                | option <- options
                ]
        requestFullscreenChoice
            runtime
            "Model"
            picker.pickerIndex
            rows
            >>= \case
                Just index
                    | index >= 0
                    , index < length options ->
                        pure (Just (options !! index))
                _ -> pure Nothing

effortChoice
    :: Maybe FullscreenRuntime
    -> Text
    -> IO (Maybe Text)
effortChoice fullscreen current = case fullscreen of
    Nothing -> pure Nothing
    Just runtime -> do
        let efforts = reasoningEfforts
            initial = fromMaybe 0 (elemIndex current efforts)
        requestFullscreenChoice
            runtime
            "Reasoning effort"
            initial
            [(effort, "") | effort <- efforts]
            >>= \case
                Just index
                    | index >= 0
                    , index < length efforts ->
                        pure (Just (efforts !! index))
                _ -> pure Nothing

loadAccountPickerOptions
    :: (Credential -> IO Text)
    -> OpenAI.Pool
    -> IO [AccountPickerOption]
loadAccountPickerOptions resolveLabel pool = do
    _ <- OpenAI.discoverAccounts pool
    now <- getCurrentTime
    snapshots <- OpenAI.snapshotAccounts pool
    accounts <- mapConcurrently (loadAccount now) snapshots
    pure (accounts <> [AccountPickerConnect])
  where
    loadAccount now snapshot = do
        let auth = snapshot.snapshotAuth
            credential = Credential
                { accessToken = auth.accessToken
                , accountId = auth.accountId
                , leaseId = Nothing
                , provider = OpenAIProvider
                }
        label <- resolveLabel credential
        usage <- fetchUsage auth.accessToken auth.accountId
        pure $
            AccountPickerAccount
                auth.accountId
                auth.accountId
                label
                (formatUsageSummary
                    now
                    snapshot.snapshotCooldownUntil
                    usage)

loadProviderAccountPickerOptions
    :: Provider
    -> BillingMode
    -> Text
    -> IO [AccountPickerOption]
loadProviderAccountPickerOptions provider billing _currentSelectionId = do
    discovered <- discoverSelectableLoginAccounts
    refreshed <-
        mapConcurrently refreshLoginAccount
            [ account
            | account <- discovered
            , account.loginProvider == provider
            , accountBillingMode provider account.loginBilling == billing
            ]
    now <- getCurrentTime
    pure $
        [ AccountPickerAccount
            (loginAccountSelectionId account)
            account.loginAccountId
            account.loginLabel
            (formatLoginUsageSummary provider now account)
        | account <- refreshed
        ]
            <> [AccountPickerConnect]

accountBillingMode :: Provider -> AccountBilling -> BillingMode
accountBillingMode provider = case provider of
    OpenRouterProvider -> const ApiBilled
    _ -> \case
        SubscriptionBilling _ -> SubscriptionBilled
        ApiCreditsBilling -> ApiBilled

formatLoginUsageSummary :: Provider -> UTCTime -> LoginAccount -> Text
formatLoginUsageSummary provider now account =
    Text.intercalate " · " $
        billing <> case account.loginUsage of
            UsageNotChecked -> []
            UsageUnavailable _ -> ["usage unavailable"]
            UsageAvailable usage ->
                maybeToList usage.usagePlan
                    <> map summarizeWindow usage.usageWindows
                    <> maybeToList
                        (("credits " <>) <$> usage.creditsRemaining)
                    <> maybeToList
                        (("used " <>) <$> usage.creditsUsed)
  where
    billing = case accountBillingMode provider account.loginBilling of
        ApiBilled -> ["API credits"]
        SubscriptionBilled -> case account.loginBilling of
            SubscriptionBilling plan ->
                maybe ["subscription"] (\value -> ["subscription", value]) plan
            ApiCreditsBilling -> ["subscription"]
    summarizeWindow window =
        window.windowName
            <> " "
            <> Text.pack (show (max 0 (100 - window.usedPercent)))
            <> "% left"
            <> if window.resetsAt > now
                then " · resets "
                    <> formatDuration (diffUTCTime window.resetsAt now)
                else ""
    maybeToList = maybe [] pure

accountPickerMatches :: Text -> AccountPickerOption -> Bool
accountPickerMatches currentSelectionId = \case
    AccountPickerAccount selectionId _ _ _ ->
        selectionId == currentSelectionId
    AccountPickerConnect -> False

accountPickerRow
    :: Text
    -> AccountPickerOption
    -> (Text, Text)
accountPickerRow currentSelectionId = \case
    AccountPickerAccount
        selectionId
        _
        accountPickerLabel
        accountPickerUsage ->
            ( (if selectionId == currentSelectionId then "✓ " else "")
                <> accountPickerLabel
            , accountPickerUsage
            )
    AccountPickerConnect ->
        ("＋ Connect new account", "")

fullscreenAwarePlanHooks
    :: IORef (Maybe FullscreenRuntime)
    -> PlanModeHooks
    -> PlanModeHooks
fullscreenAwarePlanHooks runtimeRef hooks = PlanModeHooks
    { planConfirmEnter = \reason ->
        withCurrentFullscreen runtimeRef
            (hooks.planConfirmEnter reason)
            \runtime ->
                requestFullscreenChoiceWithBody
                    runtime
                    "Enter plan mode?"
                    reason
                    0
                    [ ("Enter plan mode", "Explore and design before implementing")
                    , ("Stay in normal mode", "Continue without entering plan mode")
                    ]
                    >>= pure . (== Just 0)
    , planDecideExit = \planBody ->
        withCurrentFullscreen runtimeRef
            (hooks.planDecideExit planBody)
            \runtime ->
                requestFullscreenChoiceWithBody
                    runtime
                    "Ready to implement this plan?"
                    planBody
                    0
                    [ ("Approve and implement", "Leave plan mode and start implementation")
                    , ("Request changes", "Send feedback and keep planning")
                    , ("Cancel plan", "Leave plan mode without implementing")
                    ]
                    >>= \case
                        Just 0 -> pure PlanApprove
                        Just 1 ->
                            requestFullscreenText
                                runtime
                                "Request changes"
                                "What should be changed in the plan?"
                                ""
                                >>= pure
                                    . PlanRequestChanges
                                    . normalizePlanNotes
                        _ -> pure PlanCancel
    , planAskQuestion = \question options ->
        withCurrentFullscreen runtimeRef
            (hooks.planAskQuestion question options)
            \runtime -> case options of
                [] ->
                    requestFullscreenText
                        runtime
                        "Planning question"
                        question
                        ""
                        >>= pure . nonBlank
                choices ->
                    requestFullscreenChoiceWithBody
                        runtime
                        "Planning question"
                        question
                        0
                        [(choice, "") | choice <- choices]
                        >>= pure . (>>= (`atMay` choices))
    }

withCurrentFullscreen
    :: IORef (Maybe FullscreenRuntime)
    -> IO a
    -> (FullscreenRuntime -> IO a)
    -> IO a
withCurrentFullscreen runtimeRef fallback fullscreenAction = do
    runtime <- readIORef runtimeRef
    case runtime of
        Nothing -> fallback
        Just active -> fullscreenAction active

normalizePlanNotes :: Maybe Text -> Text
normalizePlanNotes =
    fromMaybe "(no notes)" . nonBlank

nonBlank :: Maybe Text -> Maybe Text
nonBlank =
    (>>= \text ->
        let stripped = Text.strip text
        in if Text.null stripped then Nothing else Just stripped)

atMay :: Int -> [a] -> Maybe a
atMay index values
    | index < 0 = Nothing
    | otherwise = case drop index values of
        value : _ -> Just value
        [] -> Nothing

showAccountUsage
    :: Provider
    -> Maybe TokenProvider
    -> Maybe OpenAI.Pool
    -> IO ()
showAccountUsage provider tokenProvider openAiPool = do
    color <- resolveColor stdout
    accountUsageText color provider tokenProvider openAiPool
        >>= Text.putStrLn

accountUsageText
    :: Bool
    -> Provider
    -> Maybe TokenProvider
    -> Maybe OpenAI.Pool
    -> IO Text
accountUsageText color provider tokenProvider openAiPool = do
    now <- getCurrentTime
    case provider of
        OpenAIProvider ->
            case openAiPool of
                Just pool -> do
                    snapshots <- OpenAI.snapshotAccounts pool
                    lines_ <- mapM fetchSnapshot snapshots
                    pure (formatUsageReport color now lines_)
                Nothing ->
                    case tokenProvider of
                        Just provider_ ->
                            getNextToken provider_ Nothing >>= \case
                                Left err ->
                                    pure $
                                        roleError color
                                            ("usage: "
                                                <> formatApiErrorInlineAt now err)
                                Right credential -> do
                                    result <- fetchUsage
                                        credential.accessToken credential.accountId
                                    pure $
                                        formatUsageReport color now
                                            [ AccountUsageLine
                                                { usageAccountId = credential.accountId
                                                , usageCooldownUntil = Nothing
                                                , usageResult = result
                                                }
                                            ]
                        Nothing ->
                            pure $
                                roleMuted color
                                    "usage: no OpenAI credentials loaded"
        _ ->
            pure $
                roleMuted color
                    "usage: ChatGPT Codex windows only (xAI/OpenRouter have no account usage API here)"

fetchSnapshot :: OpenAI.AccountSnapshot -> IO AccountUsageLine
fetchSnapshot snapshot = do
    result <- fetchUsage
        snapshot.snapshotAuth.accessToken
        snapshot.snapshotAuth.accountId
    pure AccountUsageLine
        { usageAccountId = snapshot.snapshotAuth.accountId
        , usageCooldownUntil = snapshot.snapshotCooldownUntil
        , usageResult = result
        }
applyModelChange
    :: OsPath
    -> Provider
    -> Text
    -> IORef ResponseCreateParams
    -> RenderConfig
    -> IORef (Maybe Text)
    -> Persistence
    -> IO Text
applyModelChange projectRoot provider name paramsRef render previous persist = do
    modifyIORef' paramsRef (setModel name)
    writeIORef render.renderModelRef name
    saveProjectModel projectRoot provider name
    clearedChain <- case provider of
        OpenAIProvider ->
            atomicModifyIORef' previous \prev ->
                (Nothing, isJust prev)
        _ -> pure False
    case persist of
        PersistenceDisabled -> pure ()
        PersistenceEnabled slotRef -> do
            slot <- readIORef slotRef
            case slot of
                PersistencePending pending ->
                    writeIORef slotRef
                        (PersistencePending pending { createModel = name })
                PersistenceActive handle -> do
                    let meta = handle.sessionMeta { metaModel = name }
                    writeSessionMeta handle.sessionMetaPath meta
                    writeIORef slotRef
                        (PersistenceActive handle { sessionMeta = meta })
    pure $
        "model set to "
            <> name
            <> if clearedChain
                then " (conversation continued locally)"
                else ""

requestModelProviderSwitch
    :: Maybe FullscreenRuntime
    -> ModelOption
    -> Text
    -> Persistence
    -> IO (Either Text RunResult)
requestModelProviderSwitch fullscreen choice draft persist =
    prepareProviderTransition
        ManualTransition [] Nothing draft choice persist >>= \case
            Left err -> pure (Left err)
            Right transition -> do
                color <- resolveColor stdout
                let message =
                        "switching to "
                            <> providerSlug choice.modelProvider
                            <> "/"
                            <> choice.modelId
                            <> " (conversation continued locally)"
                case fullscreen of
                    Nothing ->
                        Text.putStrLn
                            (roleMuted color (glyphOk <> message))
                    Just runtime ->
                        emitUiEvent runtime (UiSystemMessage message)
                pure (Right (RunSwitchProvider transition))

requestAutomaticProviderFallback
    :: SessionEnv
    -> ApiError
    -> PendingTurn
    -> IO (Maybe ProviderTransition)
requestAutomaticProviderFallback env apiError pending = do
    forM_ env.sessionFullscreen \runtime ->
        emitUiEvent runtime UiTurnRestarted
    sessionId <- ensureTransitionSessionId env.sessionPersist
    unavailable <- readIORef env.sessionUnavailableProviders
    case env.sessionTokenProvider of
        Nothing -> pure Nothing
        Just tokenProvider ->
            chooseAutomaticProviderTransition
                env.sessionFullscreen
                (tokenProviderBillingMode tokenProvider)
                env.sessionProvider
                unavailable
                sessionId
                pending
                apiError

requestStartupProviderFallback
    :: SessionEnv
    -> ApiError
    -> IO (Maybe ProviderTransition)
requestStartupProviderFallback env apiError = do
    unavailable <- readIORef env.sessionUnavailableProviders
    case env.sessionTokenProvider of
        Nothing -> pure Nothing
        Just tokenProvider ->
            chooseStartupProviderTransition
                env.sessionFullscreen
                (tokenProviderBillingMode tokenProvider)
                env.sessionProvider
                unavailable
                Nothing
                apiError

continueAutomaticFallback
    :: Maybe FullscreenRuntime
    -> ProviderTransition
    -> ApiError
    -> IO (Maybe ProviderTransition)
continueAutomaticFallback fullscreen failed apiError =
    case ( failed.transitionAutomaticBilling
         , failed.transitionPendingTurn
         ) of
        (Just billing, Just pending) ->
            chooseAutomaticProviderTransition
                fullscreen
                billing
                failed.transitionTarget.modelProvider
                failed.transitionUnavailableProviders
                failed.transitionSessionId
                pending
                apiError
        _ -> pure Nothing

chooseAutomaticProviderTransition
    :: Maybe FullscreenRuntime
    -> BillingMode
    -> Provider
    -> [Provider]
    -> Maybe Text
    -> PendingTurn
    -> ApiError
    -> IO (Maybe ProviderTransition)
chooseAutomaticProviderTransition
    fullscreen sourceBilling current unavailable0 sessionId pending apiError =
    tryCandidates unavailable candidates
  where
    unavailable = markUnavailable current unavailable0
    candidates = fallbackCandidates unavailable0 current apiError

    tryCandidates unavailable = \case
        [] -> pure Nothing
        choice : rest ->
            validateAutomaticProviderTarget sourceBilling choice >>= \case
                Left err -> do
                    let message =
                            "skipping "
                            <> providerSlug choice.modelProvider
                            <> ": "
                            <> err
                    case fullscreen of
                        Nothing -> do
                            color <- resolveColor stderr
                            putTextLn stderr (roleMuted color message)
                        Just runtime ->
                            emitUiEvent runtime (UiSystemMessage message)
                    tryCandidates
                        (markUnavailable choice.modelProvider unavailable)
                        rest
                Right () -> do
                    let message =
                            providerSlug current
                            <> " unavailable; trying this turn with "
                            <> providerSlug choice.modelProvider
                            <> "/"
                            <> choice.modelId
                    case fullscreen of
                        Nothing -> do
                            color <- resolveColor stderr
                            putTextLn stderr
                                (roleWarn color (glyphWarn <> message))
                        Just runtime ->
                            emitUiEvent runtime (UiSystemMessage message)
                    pure $ Just ProviderTransition
                        { transitionTarget = choice
                        , transitionSessionId = sessionId
                        , transitionPendingTurn = Just pending
                        , transitionDraft = ""
                        , transitionUnavailableProviders = unavailable
                        , transitionCause = AutomaticFallback
                        , transitionAutomaticBilling = Just sourceBilling
                        }

chooseStartupProviderTransition
    :: Maybe FullscreenRuntime
    -> BillingMode
    -> Provider
    -> [Provider]
    -> Maybe Text
    -> ApiError
    -> IO (Maybe ProviderTransition)
chooseStartupProviderTransition
    fullscreen sourceBilling current unavailable0 sessionId apiError =
    tryCandidates unavailable candidates
  where
    unavailable = markUnavailable current unavailable0
    candidates = fallbackCandidates unavailable0 current apiError

    tryCandidates unavailable = \case
        [] -> pure Nothing
        choice : rest ->
            validateAutomaticProviderTarget sourceBilling choice >>= \case
                Left err -> do
                    let message =
                            "skipping "
                            <> providerSlug choice.modelProvider
                            <> ": "
                            <> err
                    forM_ fullscreen \runtime ->
                        emitUiEvent runtime (UiSystemMessage message)
                    tryCandidates
                        (markUnavailable choice.modelProvider unavailable)
                        rest
                Right () -> do
                    let message =
                            providerSlug current
                            <> " account unavailable; switched to "
                            <> providerSlug choice.modelProvider
                            <> "/"
                            <> choice.modelId
                    forM_ fullscreen \runtime ->
                        emitUiEvent runtime (UiSystemMessage message)
                    pure $ Just ProviderTransition
                        { transitionTarget = choice
                        , transitionSessionId = sessionId
                        , transitionPendingTurn = Nothing
                        , transitionDraft = ""
                        , transitionUnavailableProviders = unavailable
                        , transitionCause = AutomaticFallback
                        , transitionAutomaticBilling = Just sourceBilling
                        }

prepareProviderTransition
    :: TransitionCause
    -> [Provider]
    -> Maybe PendingTurn
    -> Text
    -> ModelOption
    -> Persistence
    -> IO (Either Text ProviderTransition)
prepareProviderTransition cause unavailable pending draft choice persist =
    validateProviderTarget choice >>= \case
        Left err -> pure (Left err)
        Right () -> do
            sessionId <- ensureTransitionSessionId persist
            pure $ Right ProviderTransition
                { transitionTarget = choice
                , transitionSessionId = sessionId
                , transitionPendingTurn = pending
                , transitionDraft = draft
                , transitionUnavailableProviders = unavailable
                , transitionCause = cause
                , transitionAutomaticBilling = Nothing
                }

validateProviderTarget :: ModelOption -> IO (Either Text ())
validateProviderTarget choice =
    fmap (() <$) (loadValidatedProviderTarget choice)

validateAutomaticProviderTarget
    :: BillingMode
    -> ModelOption
    -> IO (Either Text ())
validateAutomaticProviderTarget sourceBilling choice =
    loadValidatedProviderTarget choice >>= \case
        Left err -> pure (Left err)
        Right loaded
            | allowsAutomaticBillingFallback
                sourceBilling
                (tokenProviderBillingMode loaded.loadedTokenProvider) ->
                    pure (Right ())
            | otherwise ->
                pure $ Left
                    "automatic fallback from subscription billing to API \
                    \credits is disabled"

loadValidatedProviderTarget :: ModelOption -> IO (Either Text LoadedAuth)
loadValidatedProviderTarget choice =
    loadAuth (Just choice.modelProvider) >>= \case
        Left err -> pure $ Left $
            "cannot switch to "
                <> providerSlug choice.modelProvider
                <> ": "
                <> err
        Right loaded ->
            probeLoadedAvailability loaded >>= \case
                Left err -> do
                    now <- getCurrentTime
                    pure $ Left $
                        "cannot switch to "
                            <> providerSlug choice.modelProvider
                            <> ": "
                            <> formatApiErrorInlineAt now err
                Right usable
                    | usable.loadedProvider /= choice.modelProvider ->
                        pure $ Left $
                            "cannot switch to "
                                <> providerSlug choice.modelProvider
                                <> ": auth resolved "
                                <> providerSlug usable.loadedProvider
                    | otherwise -> pure (Right usable)

ensureTransitionSessionId
    :: Persistence
    -> IO (Maybe Text)
ensureTransitionSessionId PersistenceDisabled = pure Nothing
ensureTransitionSessionId (PersistenceEnabled slotRef) = do
    handle <- ensureSession slotRef
    pure (Just handle.sessionMeta.metaId)

commitProviderTransition
    :: OsPath
    -> Maybe ProviderTransition
    -> Persistence
    -> IO ()
commitProviderTransition _ Nothing _ = pure ()
commitProviderTransition projectRoot (Just transition) persist = do
    saveProjectModel
        projectRoot
        transition.transitionTarget.modelProvider
        transition.transitionTarget.modelId
    case persist of
        PersistenceDisabled -> pure ()
        PersistenceEnabled slotRef -> do
            slot <- readIORef slotRef
            case slot of
                PersistencePending pending ->
                    writeIORef slotRef $ PersistencePending pending
                        { createProvider = transition.transitionTarget.modelProvider
                        , createModel = transition.transitionTarget.modelId
                        }
                PersistenceActive handle -> do
                    now <- getCurrentTime
                    let meta = handle.sessionMeta
                            { metaProvider = transition.transitionTarget.modelProvider
                            , metaModel = transition.transitionTarget.modelId
                            , metaLastResponseId = Nothing
                            , metaUpdatedAt = now
                            }
                    writeSessionMeta handle.sessionMetaPath meta
                    writeIORef slotRef
                        (PersistenceActive handle { sessionMeta = meta })

prepareTransitionBackend
    :: OsPath
    -> Maybe ProviderTransition
    -> Persistence
    -> Backend
    -> IO Backend
prepareTransitionBackend _ Nothing _ backend = pure backend
prepareTransitionBackend projectRoot (Just transition) persist backend
    | transition.transitionCause == ManualTransition
        || isNothing transition.transitionPendingTurn = do
        commitProviderTransition projectRoot (Just transition) persist
        pure backend
    | otherwise = do
        committed <- newIORef False
        pure $
            commitBackendOnSuccess
                projectRoot committed transition persist backend

commitBackendOnSuccess
    :: OsPath
    -> IORef Bool
    -> ProviderTransition
    -> Persistence
    -> Backend
    -> Backend
commitBackendOnSuccess projectRoot committed transition persist (Backend submit) =
    Backend \previous inputs onEvent -> do
        result <- submit previous inputs onEvent
        case result of
            Right _ -> do
                shouldCommit <- atomicModifyIORef' committed \done ->
                    (True, not done)
                when shouldCommit $
                    commitProviderTransition projectRoot (Just transition) persist
            Left _ -> pure ()
        pure result

markUnavailable :: Provider -> [Provider] -> [Provider]
markUnavailable provider unavailable
    | provider `elem` unavailable = unavailable
    | otherwise = unavailable <> [provider]

reloadAuth :: Provider -> Maybe TokenProvider -> IO (Either Text Text)
reloadAuth provider = \case
    Nothing ->
        pure $ Right $
            "reload-auth: OpenAI WebSocket auth is fixed for this process; "
                <> "restart after refreshing ~/.codex/auth.json "
                <> "(OAuth pools already rotate on handshake failure)"
    Just tokenProvider ->
        -- Force a disk/env re-read by rejecting the credential that is
        -- actually active. Switchable providers intentionally ignore failures
        -- from older accounts, so a fabricated empty account id is insufficient.
        getNextToken tokenProvider Nothing >>= \case
            Left err -> do
                now <- getCurrentTime
                pure $ Left $
                    "reload-auth failed: " <> formatApiErrorInlineAt now err
            Right current ->
                getNextToken tokenProvider (Just FailedCredential
                    { credential = current
                    , failure = AccountAuthenticationRejected
                    }) >>= \case
                    Left err -> do
                        now <- getCurrentTime
                        pure $ Left $
                            "reload-auth failed: "
                                <> formatApiErrorInlineAt now err
                    Right credential ->
                        pure $ Right $
                            "auth reloaded ("
                                <> providerSlug provider
                                <> " account "
                                <> credential.accountId
                                <> ")"


requestReload
    :: Persistence
    -> IO RunResult
requestReload persist = do
    color <- resolveColor stderr
    case persist of
        PersistenceDisabled -> do
            putTextLn stderr
                (roleError color ":reload needs a persisted REPL session")
            pure RunQuit
        PersistenceEnabled slotRef -> do
            handle <- ensureSession slotRef
            putTextLn stderr
                (roleMuted color
                    (glyphSession <> "reloading; session " <> handle.sessionMeta.metaId))
            pure (RunReload handle.sessionMeta.metaId)

enterPlanFromSlash :: SessionEnv -> Maybe Text -> IO (Maybe ProviderTransition)
enterPlanFromSlash env@SessionEnv
    { sessionPlanMode = planMode
    , sessionPersist = persist
    , sessionPrinted = printed
    , sessionFullscreen = fullscreen
    } maybeDescription = do
    discardStore <- newIORef Nothing
    color <- resolveColor stderr
    let report message minimal = case fullscreen of
            Nothing -> putTextLn stderr (roleMuted color minimal)
            Just runtime -> emitUiEvent runtime (UiSystemMessage message)
    case persist of
        PersistenceEnabled slotRef -> do
            handle <- ensureSession slotRef
            writeIORef planMode.planSessionDir (Just handle.sessionDir)
            report
                ("session: " <> handle.sessionMeta.metaId)
                (glyphSession <> "session: " <> handle.sessionMeta.metaId)
        PersistenceDisabled -> pure ()
    case maybeDescription of
        Nothing -> do
            writeIORef planMode.planStateRef PlanPending
            let message =
                    "plan mode armed; send a prompt to activate \
                    \(or /plan <description>)"
            report message (glyphSession <> message)
            pure Nothing
        Just description -> do
            activatePlanMode planMode
            path <- planFilePath planMode
            let message = "plan mode on (" <> toText path <> ")"
            report message (glyphSession <> message)
            writeIORef printed False
            case fullscreen of
                Nothing -> pure ()
                Just runtime ->
                    emitUiEvent runtime (UiUserSubmitted description)
            let planEnv = env { sessionStoreRoot = discardStore }
                inputs = [UserMessage description]
            result <- runOneTurn planEnv description inputs
            case result of
                TurnProviderUnavailable apiError pending ->
                    requestAutomaticProviderFallback
                        planEnv apiError pending >>= \case
                            Nothing -> do
                                reportProviderUnavailable fullscreen apiError
                                pure Nothing
                            Just providerTransition ->
                                pure (Just providerTransition)
                _ -> do
                    putTrailingNewline printed
                    pure Nothing

-- | Discover AGENTS.md once for a fresh session. Resumed transcripts keep
-- whatever instructions were already in history.
loadAgentsContext
    :: Maybe FullscreenRuntime
    -> CliOptions
    -> Provider
    -> OsPath
    -> OsPath
    -> [ResponseItem]
    -> Maybe Text
    -> IO (IORef (Maybe Text))
loadAgentsContext fullscreen options provider home cwd initialItems initialPrevious
    | not options.optAgentsMd = newIORef Nothing
    | not (null initialItems) || isJust initialPrevious = newIORef Nothing
    | otherwise = do
        let discoverOptions = DiscoverOptions
                { discoverMaxBytes = defaultDiscoverOptions.discoverMaxBytes
                , discoverGlobalDir = Just (globalAgentsHomeDir provider home)
                , discoverRootMarkers = defaultDiscoverOptions.discoverRootMarkers
                }
        loaded <- discoverProjectInstructions discoverOptions cwd
        let files = loadedInstructionFiles loaded
        case formatAgentsMdForProvider provider cwd loaded of
            Nothing -> newIORef Nothing
            Just text -> do
                let message =
                        "agents.md: loaded "
                            <> Text.pack (show (length files))
                            <> if length files == 1 then " file" else " files"
                case fullscreen of
                    Nothing -> do
                        color <- resolveColor stderr
                        putTextLn stderr
                            (roleMuted color (glyphSession <> message))
                    Just runtime ->
                        emitUiEvent runtime (UiSystemMessage message)
                newIORef (Just text)

preparePromptSkillInputs
    :: SessionEnv
    -> Text
    -> [TurnInput]
    -> IO (Either Text [TurnInput])
preparePromptSkillInputs env prompt inputs = do
    invocations <- readIORef env.sessionSkillInvocations
    pure do
        selected <- resolveSkillMentions invocations prompt
        let activations =
                [ UserMessage (formatSkillActivation invocation prompt)
                | invocation <- selected
                ]
        pure (activations <> inputs)

-- | Drop Ghostty / Windows Terminal native progress (OSC 9;4) on stderr.
-- Safe when the bar was never shown; unknown terminals ignore the sequence.
clearNativeProgress :: Handle -> IO ()
clearNativeProgress handle =
    setNativeProgress handle False

setNativeProgress :: Handle -> Bool -> IO ()
setNativeProgress handle active = do
    tty <- hIsTerminalDevice handle
    when tty do
        inTmux <- isJust <$> lookupEnv "TMUX"
        let sequence_
                | active = osc9ProgressIndeterminate
                | otherwise = osc9ProgressRemove
        Text.hPutStr handle (wrapOscForTmux inTmux sequence_)
        hFlush handle

-- | Queue clipboard / Finder-paste images and optionally draw an in-terminal
-- thumbnail. The caller reports the returned message through the active UI.
queueAttachedImages
    :: IORef [ImageAttachment]
    -> IORef Int
    -> Bool
    -> Bool
    -> [ImageAttachment]
    -> IO Text
queueAttachedImages attachmentsRef previewIdRef color showPreview images = do
    modifyIORef' attachmentsRef (<> images)
    pending <- readIORef attachmentsRef
    let sizes =
            Text.intercalate ", "
                [ img.imageMime <> " (" <> formatImageSize (BS.length img.imageBytes) <> ")"
                | img <- images
                ]
    when showPreview (putImagePreview previewIdRef color images)
    pure $
        "attached "
            <> sizes
            <> " — send with next message ("
            <> Text.pack (show (length pending))
            <> " queued)"

queueClipboardImages
    :: IORef [ImageAttachment]
    -> IORef Int
    -> Bool
    -> Bool
    -> IO (Either Text Text)
queueClipboardImages
    attachmentsRef
    previewIdRef
    color
    showPreview = do
    imagesResult <- readClipboardImages
    case imagesResult of
        Right images@(_:_) ->
            Right <$> queueAttachedImages
                attachmentsRef previewIdRef color showPreview images
        Right [] ->
            pure (Left "no image found on the clipboard")
        Left err -> reportClipboardImageError err
  where
    reportClipboardImageError err =
        readClipboard >>= \case
            ClipboardText _ ->
                pure $ Left
                    "clipboard has text, not an image \
                    \(paste text normally into the prompt)"
            ClipboardPaths paths ->
                pure $ Left
                    ("clipboard has file path(s), but no loadable image: "
                        <> Text.intercalate ", " (map Text.pack paths))
            ClipboardEmpty ->
                pure (Left err)
            ClipboardImage image ->
                Right <$> queueAttachedImages
                    attachmentsRef previewIdRef color showPreview [image]

putImagePreview :: IORef Int -> Bool -> [ImageAttachment] -> IO ()
putImagePreview previewIdRef color images = do
    protocol <- detectImagePreviewProtocol stdout
    inTmux <- isJust <$> lookupEnv "TMUX"
    size <- getTerminalSize
    let (termRows, termCols) = fromMaybe (24, 80) size
        columns = previewColumnsFor termCols
        rows = previewRowsFor termRows
    startId <- atomicModifyIORef' previewIdRef \n ->
        (n + max 1 (length images), n)
    case renderImagePreview protocol inTmux (roleMuted color) columns rows startId images of
        Nothing -> pure ()
        Just block -> do
            -- Graphics sequences must not go through the Solarized wash.
            Text.putStrLn block
            hFlush stdout

putTrailingNewline :: IORef Bool -> IO ()
putTrailingNewline printed = do
    didPrint <- readIORef printed
    if didPrint then putStrLn "" else pure ()

loadPrompt :: CliOptions -> IO (Maybe Text)
loadPrompt options = case (options.optPrompt, options.optPromptFile) of
    (Just text, _) -> pure (Just text)
    (_, Just path) -> Just . Text.strip <$> Text.readFile (unsafeToFilePath path)
    _ -> pure Nothing

handleResume
    :: Maybe FullscreenRuntime
    -> Maybe Text
    -> Persistence
    -> IO (Maybe RunResult)
handleResume fullscreen maybeId persist = do
    color <- resolveColor stderr
    home <- getHomeDirectory
    let root = sessionsRoot home
        resume sessionId = do
            currentId <- currentSessionId persist
            if Just sessionId == currentId
                then do
                    Text.hPutStrLn stderr
                        (roleMuted color
                            (glyphSession <> "already on session " <> sessionId))
                    pure Nothing
                else
                    loadSession root sessionId >>= \case
                        Left err -> do
                            Text.hPutStrLn stderr (roleError color err)
                            pure Nothing
                        Right _ -> pure (Just (RunResumeSession sessionId))
    case maybeId of
        Just sessionId -> resume sessionId
        Nothing -> do
            sessions <- listSessions root
            currentId <- currentSessionId persist
            pickResumeChoice fullscreen color root currentId sessions >>= \case
                Nothing -> pure Nothing
                Just sessionId -> resume sessionId

pickResumeChoice
    :: Maybe FullscreenRuntime
    -> Bool
    -> OsPath
    -> Maybe Text
    -> [SessionMeta]
    -> IO (Maybe Text)
pickResumeChoice fullscreen color root currentId sessions = case fullscreen of
    Nothing -> pickResumeSession color root sessions
    Just runtime -> do
        now <- getCurrentTime
        let browser =
                initialResumeBrowser now (map resumeEntryFromMeta sessions)
            deleteEntry sessionId
                | currentId == Just sessionId =
                    pure (Left "cannot delete the current session")
                | otherwise =
                    deleteSession root sessionId
        fmap (.resumeId)
            <$> requestFullscreenResume
                runtime
                browser
                (loadResumeEntry root)
                deleteEntry

pickAgentChoice
    :: Maybe FullscreenRuntime
    -> Bool
    -> AgentTarget
    -> [AgentEntry]
    -> IO (Maybe AgentTarget)
pickAgentChoice fullscreen color selected entries = case fullscreen of
    Nothing -> pickAgentViewport color selected entries
    Just runtime ->
        requestFullscreenChoice
            runtime
            "Agents"
            (fromMaybe 0
                (findIndex ((== selected) . (.agentTarget)) entries))
            [ ( entry.agentPath
              , entry.agentStatus
                    <> case entry.agentTranscript of
                        first : _
                            | not (Text.null (Text.strip first)) ->
                                " · " <> Text.take 80 (Text.strip first)
                        _ -> ""
              )
            | entry <- entries
            ]
            >>= pure . (>>= (`atMay` map (.agentTarget) entries))

currentSessionId
    :: Persistence
    -> IO (Maybe Text)
currentSessionId = \case
    PersistenceDisabled -> pure Nothing
    PersistenceEnabled slotRef -> do
        slot <- readIORef slotRef
        pure $ case slot of
            PersistencePending _ -> Nothing
            PersistenceActive handle -> Just handle.sessionMeta.metaId

-- | Build a root OpenAI backend plus an unlocked sender for manual compaction.
-- Callers hold the same lock around the entire manual compact/install
-- transition. A normal logical turn holds it across automatic compaction and
-- its continuation because the active WebSocket is not multiplexed and
-- transcript mutation must not interleave between those two requests.
lockedOpenAiSession
    :: Maybe Int
    -> MVar ()
    -> TokenProvider
    -> IORef OpenAiPersistentConnection
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> IORef (Maybe (Int, Int))
    -> (TokenUsage -> IO ())
    -> (OpenAiCompactionSender, Backend)
lockedOpenAiSession compactThreshold wsLock provider activeConnection
        getParams transcript contextTokens
        recordCompactionUsage =
    let sendResponse request previousResponseId onEvent = do
            OpenAiPersistentConnection
                credential
                connectionHealthy
                conn <-
                    readIORef activeConnection
            openAiResponseSenderReconnecting
                provider
                credential
                connectionHealthy
                conn
                request
                previousResponseId
                onEvent
        sendAuxiliary request previousResponseId onEvent = do
            OpenAiPersistentConnection
                credential
                connectionHealthy
                conn <-
                    readIORef activeConnection
            openAiAuxiliaryResponseSenderReconnecting
                provider
                credential
                connectionHealthy
                conn
                request
                previousResponseId
                onEvent
        baseBackend =
            withConnectionRecovery $
                openAiBackendWith sendResponse getParams transcript
        compactSender request =
            sendAuxiliary request Nothing (const (pure ()))
        compactingBackend =
            autoCompactOpenAiBackendWithSender
                compactThreshold
                compactSender
                recordCompactionUsage
                getParams
                transcript
                contextTokens
                baseBackend
        serializedBackend = Backend \previous inputs onEvent ->
            withMVar wsLock \_ ->
                compactingBackend.submitTurn previous inputs onEvent
    in (compactSender, serializedBackend)

-- | Drop live conversation state without touching persisted session files.
resetLiveConversation
    :: IORef (Maybe Text)
    -> IORef [ResponseItem]
    -> IORef [ImageAttachment]
    -> PlanModeEnv
    -> IO ()
resetLiveConversation previous transcriptRef attachmentsRef planMode = do
    writeIORef previous Nothing
    writeIORef transcriptRef []
    writeIORef attachmentsRef []
    deactivatePlanMode planMode

detectGitBranch :: OsPath -> IO Text
detectGitBranch cwd = do
    result <-
        (try $
            readProcessWithExitCode
                "git"
                ["-C", unsafeToFilePath cwd, "rev-parse", "--abbrev-ref", "HEAD"]
                "")
            :: IO (Either SomeException (ExitCode, String, String))
    pure $ case result of
        Right (ExitSuccess, output, _) ->
            let branch = Text.strip (Text.pack output)
            in if Text.null branch
                then ""
                else if branch == "HEAD" then "detached" else branch
        _ -> ""

-- | Apply compact turns as full transcript replacements when resuming.
foldSessionItems :: [SessionTurn] -> [ResponseItem]
foldSessionItems =
    concat . reverse . foldl' addTurn []
  where
    addTurn chunks turn
        | isTranscriptResetTurn turn.turnUserText =
            -- /clear and /new store an empty snapshot; /compact stores the
            -- rebuilt history. Either way, turnItems replaces prior history.
            [turn.turnItems]
        | hasCompactionCheckpoint turn.turnItems =
            [turn.turnItems]
        | otherwise =
            turn.turnItems : chunks

hydrateUiHistory :: [SessionTurn] -> UiState
hydrateUiHistory = foldl' addTurn initialUiState
  where
    addTurn state turn
        | isTranscriptResetTurn turn.turnUserText =
            addResetTurn state turn
        | otherwise =
            addRegularTurn state turn

    addResetTurn state turn =
        let cleared = reduceUi UiConversationCleared state
        in case turn.turnAssistantText of
            Nothing -> cleared
            Just text -> reduceUi (UiHistory text) cleared

    addRegularTurn state turn =
        let withUser =
                if Text.null (Text.strip turn.turnUserText)
                    then state
                    else reduceUi
                        (UiUserSubmitted turn.turnUserText)
                        state
            withAssistant = case turn.turnAssistantText of
                Nothing -> withUser
                Just text ->
                    reduceUi (UiAssistantHistory text) withUser
        in case turn.turnError of
            Nothing -> withAssistant
            Just err -> reduceUi (UiErrorMessage err) withAssistant
