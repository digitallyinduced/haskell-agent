-- | Command-line entry point: one-shot @-p@ or an interactive REPL.
module Agent.CLI
    ( DevResult(..)
    , afterDev
    , accountSwitchTarget
    , applyReplMode
    , buildPromptState
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
    , staticCredentialProvider
    )
import Agent.CLI.Secret
    ( promptSecretLine
    , sanitizeSecretPromptText
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
    , responseItemLines
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
    , agentSessionTools
    , closeSessionProcessManager
    , launchSessionTurn
    , newSessionProcessManager
    , signalManagedSessionReady
    , sessionProcessStatus
    )
import Agent.CLI.Approval
    ( ApprovalNotice(..)
    , approveToolDecision
    , approveToolDecisionWithReporter
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
    , nonEmptyClipboardImages
    , readClipboard
    , readClipboardImages
    , readClipboardImagesImageFirst
    )
import Agent.CLI.Command
import Agent.CLI.Config
    ( HarnessConfig(..)
    , McpServerConfig(..)
    , loadHarnessConfig
    )
import Agent.CLI.Compaction
    ( CompactOutcome(..)
    , OpenAiCompactionSender
    , autoCompactOpenAiBackendWithSender
    , installCompactOutcome
    , runProviderCompactWith
    , runResponsesCompactWith
    )
import Agent.CLI.Connectivity (withConnectionRecovery)
import Agent.CLI.Database (databaseTools)
import Agent.CLI.Database.Store
    ( databaseToolsEnvForStore
    , deriveDatabaseScopes
    )
import Agent.CLI.Database.Storage
    ( postgresStorageCommandEnv
    , runStorageCommand
    )
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
    , readReplLineWithSkillsAndModels
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
import Agent.CLI.ModelConfig
    ( ConnectionKind(..)
    , ModelCatalog
    , ModelConnection(..)
    , ResponsesConnection(..)
    , builtinConnectionId
    , catalogConnection
    , loadModelCatalog
    )
import Agent.CLI.Models
    ( ModelOption(..)
    , ModelTarget(..)
    , PickerState(..)
    , catalogModelIds
    , defaultModelFor
    , defaultModelOptionFor
    , initialPickerStateResolved
    , modelTargetRequiresRebuild
    , rawModelOption
    , resolveConfiguredModel
    , resolveModelOptionDialect
    , resolvePersistedDialect
    )
import Agent.CLI.Notification
    ( AttentionRequest(InputRequested)
    , notifyAttention
    )
import Agent.CLI.Options
import Agent.Store.Postgres
    ( ManagedPostgresConfig
    , Store
    , closeStore
    , managedPostgresConfigFromEnv
    , openStore
    , trustedPool
    , withStore
    )
import Agent.Store.Types (renderStoreError)
import Agent.Store.Postgres.Connection (StorePool)
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
    ( ProjectModel(..)
    , ProjectSettings(..)
    , loadProjectSettings
    , projectModelProvider
    , resolveProjectRoot
    , saveProjectModel
    )
import Agent.CLI.Prompt
    ( secretInputGuidance
    , subscriptionSubagentModelGuidance
    , systemPromptForTools
    )
import Agent.CLI.Request (requestParams, setRequestInstructions)
import Agent.CLI.ProviderFallback
    ( allowsAutomaticBillingFallback
    , automaticCooldownRetryDelay
    , automaticRetryCountdownText
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
import Agent.CLI.SessionLock
    ( SessionLock
    , acquireSessionLock
    , releaseSessionLock
    , sessionLockFilePath
    , sessionLockPath
    )
import Agent.CLI.Skills
    ( formatSkillsListing
    , installSkillCatalogWithOmissions
    , loadSkillsCatalogQuiet
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
    , lookupOrCreateSubagentSession
    , persistAndEvictSubagentSessionWithStatus
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
import Agent.CLI.Dialects
    ( CodingTools(..)
    , codingToolsForWithTypes
    , filterBashTools
    , filterGhciTools
    , formatAgentsMdForDialect
    , globalAgentsHomeDir
    )
import Agent.CLI.TUI.App
    ( FullscreenInputBuffer
    , FullscreenRuntime
    , emitUiEvent
    , hasQueuedFullscreenInput
    , newFullscreenInputBuffer
    , newFullscreenRuntime
    , queuedFullscreenInputDisplays
    , readFullscreenLineOrWithModels
    , readFullscreenLineWithModels
    , requestFullscreenPermission
    , requestFullscreenChoice
    , requestFullscreenChoiceWithBody
    , requestFullscreenOnboarding
    , requestFullscreenResume
    , requestFullscreenSecret
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
    )
import Agent.CLI.Worktree
    ( createWorktree
    , isUnderWorktreeRoot
    , removeWorktree
    , worktreeRoot
    )
import Agent.Cancel (requestCancel, resetCancel, waitCancel)
import Agent.Loop
import qualified Agent.MCP as MCP
import Agent.Error (ApiError(..))
import Agent.Dialect
    ( Dialect
    , DialectId
    , dialectForId
    , dialectId
    , dialectSlug
    , providerSupportsDialect
    )
import Agent.ProjectInstructions
    ( DiscoverOptions(..)
    , defaultDiscoverOptions
    , discoverProjectInstructions
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
import Agent.Responses.GenericBackend (genericResponsesBackendWith)
import Agent.Responses.GenericClient (GenericClientOptions(..))
import qualified Agent.Responses.GenericClient as GenericResponses
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
    , getStatus
    , interruptActiveSubagents
    , listAgents
    , newSubagentRegistry
    , setSubagentOnComplete
    , setSubagentOnSettled
    , setSubagentRunner
    )
import Agent.GrokBuild.Dialect.Task (GrokSubagentSpecs)
import Agent.Subagents.TaskPath (taskPathRoot, taskPathText)
import Agent.TextBuffer (emptyTextBuffer)
import Agent.ToolDispatch (canonicalToolName)
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
import Agent.Tools.Secret
    ( SecretPrompt(..)
    , SecretPromptHooks(..)
    )
import Agent.Tools.Types
    ( AppTool(..)
    , ToolEnv(..)
    , defaultToolEnv
    , setToolSessionTmp
    )
import Agent.OpenRouter.LoopBackend (openRouterBackend)
import qualified Agent.OpenRouter as OpenRouter
import Agent.OsPath (fromText, toText, unsafeToFilePath)
import Agent.XAI.LoopBackend (xaiBackend)
import qualified Agent.XAI.Options as XAI
import Control.Applicative ((<|>))
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
    , onException
    , throwIO
    , try
    )
import Control.Monad (forM_, void, when)
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
    , addUTCTime
    , diffUTCTime
    , getCurrentTime
    , utctDay
    )
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory.OsPath
    ( doesDirectoryExist
    , getCurrentDirectory
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
    | RunSwitchWorktree OsPath Provider Text Text
      -- ^ Fresh worktree path. Starts a new session after the current backend
      -- and fullscreen UI have shut down, retaining provider, model, and effort.

data PreparedAgent = PreparedAgent
    { preparedFullscreen :: !(Maybe FullscreenRuntime)
    , preparedRun :: !(IO RunResult)
    }

data PendingTurnPresentation
    = SubmitPendingTurn
    | RestartPendingTurn
    | ContinuePendingTurn

data AccountPickerOption
    = AccountPickerAccount
        !Provider
        !BillingMode
        !Text
        !Text
        !Text
        !Text
    | AccountPickerConnect !Provider

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
    , startupDatabaseStore :: !Store
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
        Right (Storage command) ->
            runStorageAdmin command >> pure DevQuit
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
        Right (Storage command) -> runStorageAdmin command
        Right (RunAgent options) -> do
            result <- runAgentWithRestarts options
            case result of
                DevQuit -> pure ()
                DevReload _ ->
                    die ":reload is only available under `repl` (nix develop)"

runStorageAdmin :: StorageCommand -> IO ()
runStorageAdmin command = do
    home <- getHomeDirectory
    config <- managedPostgresConfigForHome home
    runStorageCommand (postgresStorageCommandEnv config) command >>= \case
        Left err -> die (Text.unpack err)
        Right message -> Text.putStrLn message

managedPostgresConfigForHome :: OsPath -> IO ManagedPostgresConfig
managedPostgresConfigForHome home = do
    stateDirectory <-
        decodeFS (home </> unsafeEncodeUtf ".haskell-agent")
    managedPostgresConfigFromEnv stateDirectory

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
            RunSwitchWorktree path provider model effort ->
                go fullscreenInputs
                    current
                        { optProvider = Just provider
                        , optModel = Just model
                        , optCwd = Just path
                        , optWorktree = False
                        , optEffort = Just effort
                        , optPrompt = Nothing
                        , optPromptFile = Nothing
                        , optResume = Nothing
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
    withStoreForHome home \store -> do
        sessions <- listSessions (trustedPool store) (sessionsRoot home)
        if null sessions
            then putStrLn "No sessions in ~/.haskell-agent/sessions"
            else mapM_ printSessionSummary sessions

runShowSession :: Text -> IO ()
runShowSession sessionId = do
    home <- getHomeDirectory
    withStoreForHome home \store ->
        loadSession (trustedPool store) (sessionsRoot home) sessionId >>= \case
            Left err -> die (Text.unpack err)
            Right (meta, turns) -> do
                printSessionSummary meta
                putStrLn ""
                if null turns
                    then putStrLn "(empty transcript)"
                    else mapM_ printTurn turns

withStoreForHome :: OsPath -> (Store -> IO a) -> IO a
withStoreForHome home action = do
    config <- managedPostgresConfigForHome home
    withStore config action >>= \case
        Left err -> die (Text.unpack (renderStoreError err))
        Right value -> pure value

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

reportStartupWarning :: StartupRuntime -> Text -> IO ()
reportStartupWarning startup message =
    case startup.startupFullscreen of
        Nothing -> putTextLn stderr ("warning: " <> message)
        Just runtime ->
            emitUiEvent runtime (UiSystemMessage ("warning: " <> message))

mcpToolCollision :: [AppTool] -> [MCP.McpToolRegistration] -> Maybe Text
mcpToolCollision existingTools = go
  where
    existing =
        Map.fromList $
            ("web_search", "built-in web search")
                : [ (canonicalToolName tool.appToolName, "built-in tool")
                  | tool <- existingTools
                  ]

    go [] = Nothing
    go (registration : rest) =
        let tool = registration.mcpRegistrationTool
            name = canonicalToolName tool.appToolName
        in case Map.lookup name existing of
            Nothing -> go rest
            Just source ->
                Just $
                    "MCP tool "
                        <> tool.appToolName
                        <> " from server "
                        <> registration.mcpRegistrationServer
                        <> " conflicts with "
                        <> source

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
    resumeLockRef <- newIORef (Nothing :: Maybe SessionLock)
    databaseStoreRef <- newIORef (Nothing :: Maybe Store)
    let failPreparation message =
            readIORef resumeLockRef >>= mapM_ releaseSessionLock >>
            readIORef databaseStoreRef >>= mapM_ closeStore >>
                case activeFullscreen of
                    Nothing -> die message
                    Just _ -> throwIO (StartupFailure message)
    startedAt <- getCurrentTime
    startupTimingsRef <- newIORef []
    syntaxLoadDurationRef <- newIORef Nothing
    home <- getHomeDirectory
    let root = sessionsRoot home
    databaseConfig <- managedPostgresConfigForHome home
    databaseStore <- openStore databaseConfig >>= \case
        Left err -> failPreparation (Text.unpack (renderStoreError err))
        Right store -> writeIORef databaseStoreRef (Just store) >> pure store
    let sessionPool = trustedPool databaseStore
    resumed <- case options.optResume of
        Nothing -> pure Nothing
        Just sessionId -> do
            dir <- either
                (\err -> do
                    signalManagedSessionReady (Left err)
                    failPreparation (Text.unpack err))
                pure
                (sessionDirForId root sessionId)
            exists <- doesDirectoryExist dir
            when (not exists) do
                let err = "session not found: " <> sessionId
                signalManagedSessionReady (Left err)
                failPreparation (Text.unpack err)
            acquireSessionLock dir sessionId >>= \case
                Left err -> do
                    signalManagedSessionReady (Left err)
                    failPreparation (Text.unpack err)
                Right lock -> do
                    writeIORef resumeLockRef (Just lock)
                    loadSession sessionPool root sessionId >>= \case
                        Left err -> do
                            signalManagedSessionReady (Left err)
                            failPreparation (Text.unpack err)
                        Right loaded -> do
                            signalManagedSessionReady (Right ())
                            pure (Just loaded)

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
    uiRuntimeRef <- newIORef Nothing
    interrupt <- newInterruptState \msg -> do
        readIORef uiRuntimeRef >>= \case
            Just runtime ->
                emitUiEvent runtime
                    (UiSetNotice (Just (warningNotice msg)))
            Nothing -> do
                -- Drop an in-place "Thinking…" status so the hint is its own line.
                Text.hPutStr stderr "\r\ESC[K"
                clearNativeProgress stderr
                color <- resolveColor stderr
                putTextLn stderr (roleMuted color msg)
    -- Shared with Esc cancel and plan prompts so arrow-key pickers own stdin.
    escPaused <- newIORef False
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
            , startupDatabaseStore = databaseStore
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
    resumeLock <- readIORef resumeLockRef
    let action =
            runAgentInitialized
                options transition home root resumed resumeLock cwd startup
        cleanup = do
            writeIORef uiRuntimeRef Nothing
            forM_ fullscreen resetFullscreenSessionActions
            closeStore databaseStore
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
    -> Maybe SessionLock
    -> OsPath
    -> StartupRuntime
    -> IO RunResult
runAgentInitialized options transition home root resumed resumeLock cwd startup =
    runAgentInitializedWithLock
        options transition home root resumed resumeLock cwd startup
        `onException` mapM_ releaseSessionLock resumeLock

runAgentInitializedWithLock
    :: CliOptions
    -> Maybe ProviderTransition
    -> OsPath
    -> OsPath
    -> Maybe (SessionMeta, [SessionTurn])
    -> Maybe SessionLock
    -> OsPath
    -> StartupRuntime
    -> IO RunResult
runAgentInitializedWithLock
        options transition home root resumed resumeLock cwd startup = do
    let baseToolEnv = startup.startupToolEnv
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
    stateDirectory <- decodeFS (home </> unsafeEncodeUtf ".haskell-agent")
    projectRootPath <- decodeFS projectRoot
    databaseScopes <-
        deriveDatabaseScopes stateDirectory projectRootPath >>= \case
            Left err -> startupDie startup (Text.unpack err)
            Right scopes -> pure scopes
    projectSettings <- loadProjectSettings projectRoot
    catalog <-
        loadModelCatalog home >>= either
            (startupDie startup . Text.unpack)
            pure
    branch <- detectGitBranch cwd
    setStartupRepository fullscreen home branch cwd
    markStartupStage startup "Loading credentials…"
    let transitionTarget = (.transitionTarget) <$> transition
        pendingTurn = transition >>= (.transitionPendingTurn)
        transitionDraft = providerTransitionDraft transition
        unavailableProviders =
            maybe [] (.transitionUnavailableProviders) transition
        configuredOptionTarget =
            (.modelTarget)
                <$> (options.optModel >>= resolveConfiguredModel catalog)
        savedTarget provider connection model transport dialect =
            case resolveConfiguredModel catalog model of
                Just option
                    | option.modelTarget.targetConnectionId == connection ->
                        Right option.modelTarget
                _
                    | connection == builtinConnectionId provider ->
                        Right ModelTarget
                            { targetProvider = provider
                            , targetConnectionId = connection
                            , targetModelId = model
                            , targetWireModelId = fromMaybe model transport
                            , targetDialect = dialect
                            }
                    | otherwise ->
                        Left $
                            "saved model "
                                <> connection <> "/" <> model
                                <> " is not present in ~/.haskell-agent/models.json"
        resumedTargetResult
            | isJust transitionTarget || isJust options.optModel =
                Right Nothing
            | otherwise = case fst <$> resumed of
            Nothing -> Right Nothing
            Just meta ->
                Just <$> savedTarget
                    meta.metaProvider
                    meta.metaConnection
                    meta.metaModel
                    meta.metaTransportModel
                    meta.metaDialect
        projectTargetResult
            | isJust transitionTarget
                || isJust options.optModel
                || isJust resumed =
                    Right Nothing
            | otherwise = case projectSettings.settingsLastModel of
            Nothing -> Right Nothing
            Just remembered ->
                let target = remembered.projectModelTarget
                in
                Just <$> savedTarget
                    target.targetProvider
                    target.targetConnectionId
                    target.targetModelId
                    (Just target.targetWireModelId)
                    target.targetDialect
    resumedTarget <-
        either (startupDie startup . Text.unpack) pure resumedTargetResult
    projectTarget <-
        either (startupDie startup . Text.unpack) pure projectTargetResult
    let targetHint =
            transitionTarget
                <|> configuredOptionTarget
                <|> resumedTarget
                <|> if isNothing options.optModel
                    then projectTarget
                    else Nothing
        requestedProvider =
            (.targetProvider) <$> targetHint
                <|> options.optProvider
                <|> if isNothing options.optModel
                    then projectModelProvider projectSettings
                    else Nothing
        targetConnection =
            targetHint >>= catalogConnection catalog . (.targetConnectionId)
        customResponses = targetConnection >>= \connection ->
            case connection.connectionKind of
                CustomResponsesConnection responses -> Just
                    (connection.connectionId, responses)
                BuiltinConnection _ -> Nothing
    (loaded, customBearerToken) <- case customResponses of
        Nothing -> do
            builtinLoaded <-
                loadStartupAuth startup transition requestedProvider
            pure (builtinLoaded, Nothing)
        Just (connectionId, responses) -> do
            token <- case responses.responsesApiKeyEnv of
                Nothing
                    | responses.responsesApiKeyOptional -> pure ""
                    | otherwise ->
                        startupDie startup $
                            "custom connection "
                                <> Text.unpack connectionId
                                <> " requires api_key_env or api_key_optional=true"
                Just envName ->
                    lookupEnv (Text.unpack envName) >>= \case
                        Just value | not (null value) -> pure (Text.pack value)
                        _
                            | responses.responsesApiKeyOptional -> pure ""
                            | otherwise ->
                                startupDie startup $
                                    "custom connection "
                                        <> Text.unpack connectionId
                                        <> " requires environment variable "
                                        <> Text.unpack envName
            let credential = Credential
                    { accessToken = token
                    , accountId = connectionId
                    , leaseId = Nothing
                    , provider = OpenRouterProvider
                    }
            pure
                ( LoadedAuth
                    { loadedProvider = OpenRouterProvider
                    , loadedTokenProvider =
                        staticCredentialProvider ApiBilled credential
                    , loadedAccountLabel = const (pure connectionId)
                    , loadedSelectionId = Nothing
                    , loadedOpenAiPool = Nothing
                    }
                , if Text.null token then Nothing else Just token
                )
    case (transitionTarget, resumed) of
        (Just target, _)
            | loaded.loadedProvider /= target.targetProvider ->
                startupDie startup $ "provider transition requested "
                    <> Text.unpack (providerSlug target.targetProvider)
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
    initialHttp <- case customResponses of
        Just (connectionId, _) -> do
            writeIORef activeAccountRef connectionId
            pure
                ( selectableTokenProvider
                , const (pure connectionId)
                , connectionId
                )
        Nothing -> case loaded.loadedProvider of
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

    openRouterOptions <- OpenRouter.clientOptionsFromEnv
    markStartupStage startup "Loading tools…"
    harnessConfig <-
        loadHarnessConfig home >>= \case
            Left err -> startupDie startup (Text.unpack err)
            Right config -> pure config
    let basePlanHooks =
            cliPlanHooks interrupt escPaused (resolveColor stderr)
        planHooks = fullscreenAwarePlanHooks uiRuntimeRef basePlanHooks
        baseSecretHooks = SecretPromptHooks \request ->
            Right <$> promptSecretLine
                escPaused
                request.secretPromptMessage
                request.secretPromptPurpose
        secretHooks
            | isOneShot options || not isTty = Nothing
            | otherwise =
                Just (fullscreenAwareSecretHooks uiRuntimeRef baseSecretHooks)
        provider = loaded.loadedProvider
        fallbackModel =
            fromMaybe
                (error "validated default model is missing")
                (defaultModelFor catalog provider)
        model = fromMaybe
            (maybe fallbackModel (.targetModelId) targetHint)
            options.optModel
        rawTarget = (rawModelOption provider model).modelTarget
        inferredTarget0 =
            fromMaybe rawTarget $
                transitionTarget
                    <|> configuredOptionTarget
                    <|> resumedTarget
                    <|> if isNothing options.optModel
                        then projectTarget
                        else Nothing
        transportModel = case customResponses of
            Just _ ->
                \name ->
                    case resolveConfiguredModel catalog name of
                        Just option
                            | option.modelTarget.targetConnectionId
                                == inferredTarget0.targetConnectionId ->
                                option.modelTarget.targetWireModelId
                        _
                            | name == model ->
                                inferredTarget0.targetWireModelId
                            | otherwise -> name
            _ -> case provider of
                OpenRouterProvider -> OpenRouter.mapModel openRouterOptions
                _ -> id
        inferredTarget =
            inferredTarget0
                { targetWireModelId =
                    if inferredTarget0.targetConnectionId
                        == builtinConnectionId OpenRouterProvider
                        && inferredTarget0.targetWireModelId
                            == inferredTarget0.targetModelId
                        then transportModel model
                        else inferredTarget0.targetWireModelId
                }
        customGenericOptions = do
            (_, responses) <- customResponses
            pure GenericClientOptions
                { baseUrl = Text.unpack responses.responsesBaseUrl
                , model = inferredTarget.targetWireModelId
                , bearerToken = customBearerToken
                , requestTimeoutSeconds =
                    responses.responsesRequestTimeoutSeconds
                }
        persistedTarget = case fst <$> resumed of
            Just meta ->
                Just
                    ( meta.metaDialect
                    , meta.metaTransportModel
                    )
            Nothing -> do
                remembered <- projectSettings.settingsLastModel
                let target = remembered.projectModelTarget
                if target.targetProvider == provider
                    then Just
                        ( target.targetDialect
                        , Just target.targetWireModelId
                        )
                    else Nothing
        resolvedPersistedTarget =
            (\(storedDialect, storedTransportModel) ->
                resolvePersistedDialect
                    storedDialect
                    storedTransportModel
                    inferredTarget)
                <$> persistedTarget
        mappedTargetChanged =
            maybe False snd resolvedPersistedTarget
        dialectId = case transitionTarget of
            Just target -> target.targetDialect
            Nothing -> case options.optModel of
                Just _ -> inferredTarget.targetDialect
                Nothing
                    | mappedTargetChanged -> inferredTarget.targetDialect
                    | otherwise ->
                        maybe
                            inferredTarget.targetDialect
                            fst
                            resolvedPersistedTarget
        dialect = dialectForId dialectId
        resumeTargetChanged = case fst <$> resumed of
            Just meta ->
                provider /= meta.metaProvider
                    || inferredTarget.targetConnectionId /= meta.metaConnection
                    || model /= meta.metaModel
                    || mappedTargetChanged
                    || dialectId /= meta.metaDialect
            Nothing -> False
        refreshDialectContext = case fst <$> resumed of
            Just meta -> dialectId /= meta.metaDialect
            Nothing -> False
        legacySubagentTarget =
            sessionLegacySubagentTarget . fst <$> resumed
        effort = fromMaybe
            (maybe (defaultEffortFor provider) (.metaEffort) (fst <$> resumed))
            options.optEffort
        policy = resolveApprovalPolicy options isTty
            projectSettings.settingsAutoApprove
    -- Provider transitions commit their selection separately: manual switches
    -- immediately, automatic fallbacks only after the replacement succeeds.
    when (isNothing transition) $
        saveProjectModel projectRoot
            inferredTarget { targetDialect = dialectId }
    sessionProcessManager <- newSessionProcessManager root
    activeSessionLock <- newIORef resumeLock
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
                (restoreAgentFromDisk
                    provider
                    inferredTarget.targetConnectionId
                    transportModel
                    inferredTarget.targetWireModelId
                    dialectId
                    legacySubagentTarget
                    subagentStoreRoot
                    registry
                    subagentSessions
                    agentTypesRef)
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
                    provider
                    inferredTarget.targetConnectionId
                    transportModel
                    inferredTarget.targetWireModelId
                    dialectId
                    legacySubagentTarget
                    subagentSessions subagentStoreRoot agentTypesRef
                    subagentForkSource)
            , multiSendToRoot = Just sendToRoot
            , multiSpawnModelGuidance =
                subscriptionSubagentModelGuidance
                    provider
                    (tokenProviderBillingMode tokenProvider)
            }
    prompt <- loadPrompt options
    persist <-
        preparePersistence
            (trustedPool startup.startupDatabaseStore)
            fullscreen options root
                inferredTarget { targetDialect = dialectId }
                (isNothing transition) cwd effort prompt resumed
    writeIORef persistSlotRef persist
    (sessionTmp, ephemeralSessionId) <-
        persistenceTempDir persist >>= \case
            Just tempDir -> pure (tempDir, Nothing)
            Nothing -> do
                (sessionId, tempDir) <- allocateSessionTemp root
                pure (tempDir, Just sessionId)
    setToolSessionTmp baseToolEnv (Just sessionTmp)
    let cleanupScratch = do
            cleanupPendingPersistence persist
            forM_ ephemeralSessionId \sessionId -> do
                _ <- removeSessionTemp root sessionId
                pure ()
        toolEnv = baseToolEnv
        mcpServerConfigs =
            [ MCP.McpServerConfig
                { MCP.mcpServerName = label
                , MCP.mcpServerCommand = Text.unpack config.mcpCommand
                , MCP.mcpServerArgs = map Text.unpack config.mcpArgs
                , MCP.mcpServerCwd = Text.unpack <$> config.mcpCwd
                , MCP.mcpServerEnv =
                    [ (Text.unpack name, Text.unpack value)
                    | (name, value) <- Map.toAscList config.mcpEnv
                    ]
                , MCP.mcpServerStartupTimeoutSeconds =
                    config.mcpStartupTimeoutSeconds
                , MCP.mcpServerRequestTimeoutSeconds =
                    config.mcpRequestTimeoutSeconds
                }
            | (label, config) <-
                Map.toAscList harnessConfig.configMcpServers
            , config.mcpEnabled
            ]
    mcpFleet <-
        try @_ @SomeException (MCP.startMcpFleet mcpServerConfigs) >>= \case
            Left exception ->
                startupDie startup
                    ("Failed to initialize MCP tools: " <> show exception)
            Right fleet -> pure fleet
    mapM_ (reportStartupWarning startup) mcpFleet.mcpFleetWarnings
    coding <-
        codingToolsForWithTypes
            dialect
            toolEnv
            (Just planHooks)
            secretHooks
            multiCtx
            agentTypesRef
            `onException` (MCP.closeMcpFleet mcpFleet >> cleanupScratch)
    case multiCtx of
        Just ctx -> do
            setSubagentOnComplete ctx.multiRegistry \agentId status -> do
                atomicModifyIORef' pendingNotices \xs ->
                    (xs <> [UserMessage (formatCompletionNotice agentId status)], ())
            setSubagentOnSettled ctx.multiRegistry \agentId status -> do
                sessions <- readIORef subagentSessions
                case Map.lookup agentId sessions of
                    Just session -> do
                        _ <-
                            persistAndEvictSubagentSessionWithStatus
                                subagentStoreRoot ctx.multiRegistry agentTypesRef
                                agentId status session
                        pure ()
                    Nothing -> pure ()
        Nothing -> pure ()
    let claimCurrentSession handle = do
            let desired = sessionLockPath handle.sessionDir
            readIORef activeSessionLock >>= \case
                Just current
                    | sessionLockFilePath current == desired -> pure ()
                previous ->
                    acquireSessionLock
                        handle.sessionDir
                        handle.sessionMeta.metaId >>= \case
                            Left err -> throwIO (userError (Text.unpack err))
                            Right lock -> do
                                writeIORef activeSessionLock (Just lock)
                                mapM_ releaseSessionLock previous
        sessionToolsEnv = AgentSessionToolsEnv
            { toolsPool = trustedPool startup.startupDatabaseStore
            , toolsRoot = root
            , toolsProvider = provider
            , toolsConnection = inferredTarget.targetConnectionId
            , toolsModel = model
            , toolsTransportModel = inferredTarget.targetWireModelId
            , toolsDialect = dialectId
            , toolsCwd = cwd
            , toolsEffort = effort
            , toolsCurrentSessionId =
                readIORef persistSlotRef >>= currentSessionId
            , toolsLaunchTurn =
                launchSessionTurn sessionProcessManager
                    (not (isOneShot options)) policy
                    options.optGhci options.optBash
            , toolsSessionStatus =
                sessionProcessStatus sessionProcessManager
            }
        mcpTools = MCP.mcpFleetTools mcpFleet
        databaseToolsEnv =
            databaseToolsEnvForStore
                startup.startupDatabaseStore
                databaseScopes
                (readIORef persistSlotRef >>= currentSessionId)
        tools =
            filterGhciTools options.optGhci
                (filterBashTools options.optBash coding.codingAppTools)
                ++ mcpTools
                ++ agentSessionTools sessionToolsEnv
                ++ databaseTools databaseToolsEnv
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
            MCP.closeMcpFleet mcpFleet
            coding.codingClose
            cleanupScratch
    flip finally closeAll do
        case
                mcpToolCollision
                    (coding.codingAppTools ++ agentSessionTools sessionToolsEnv)
                    mcpFleet.mcpFleetRegistrations
            of
                Just err ->
                    startupDie startup
                        ("Failed to initialize MCP tools: " <> Text.unpack err)
                Nothing -> pure ()
        today <- utctDay <$> getCurrentTime
        let instructions =
                systemPromptForTools
                    dialect
                    (map (.appToolName) tools)
                    cwd
                    (Just sessionTmp)
                    today
                    (isOneShot options)
            params = requestParams model instructions
                (schemasFromAppTools dialect tools) effort
            initialItems = maybe [] (foldSessionItems . snd) resumed
            initialTurns = maybe [] snd resumed
            initialPrevious = case transition of
                Just _ -> Nothing
                Nothing
                    | resumeTargetChanged -> Nothing
                    | otherwise ->
                        resumed >>= \(meta, _) -> meta.metaLastResponseId
        paramsRef <- newIORef params
        let subagentRuntime = SubagentRuntime
                { subagentOptions = options
                , subagentPolicy = policy
                , subagentPlanHooks = planHooks
                , subagentParams = paramsRef
                , subagentMcpTools = mcpTools
                , subagentRegistry = registry
                , subagentSessions = subagentSessions
                , subagentStoreRoot = subagentStoreRoot
                , subagentTypes = agentTypesRef
                , subagentLegacyTarget = legacySubagentTarget
                , subagentConnection = inferredTarget.targetConnectionId
                , subagentMapModel = transportModel
                , subagentSessionTmp = toolEnv.toolSessionTmp
                , subagentSpawnModelGuidance =
                    subscriptionSubagentModelGuidance
                        provider
                        (tokenProviderBillingMode tokenProvider)
                }
        transcriptRef <- newIORef initialItems
        contextTokensRef <- newIORef Nothing
        previousRef <- newIORef initialPrevious
        writeIORef subagentForkSource (Just transcriptRef)
        let titleHint = case resumed of
                Just (meta, _) -> Just meta.metaTitle
                Nothing -> sessionTitleFromPrompt <$> prompt
        setWindowTitle (cliWindowTitle cwd titleHint)
        markStartupStage startup "Loading instructions…"
        startupContext <-
            loadAgentsContext
                fullscreen
                options
                dialect
                home
                cwd
                (if refreshDialectContext then [] else initialItems)
                (if refreshDialectContext then Nothing else initialPrevious)
        -- Fullscreen sessions load skills after Brick has taken over the
        -- terminal, so filesystem discovery cannot delay the first frame.
        -- Minimal and one-shot sessions still initialize them synchronously
        -- before their first prompt/turn below.
        skillsRef <- newIORef (SkillCatalog [] [])
        skillInvocationsRef <- newIORef []

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
                    PersistencePending _ _ _ -> pure ()
            PersistenceDisabled -> pure ()
        progName <- getProgName
        markStartupStage startup "Connecting to provider…"
        withCtrlCHandler interrupt $
            withInterruptResume fullscreen progName persist RunQuit do
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
                                            contextTokensRef
                                            recordCompactionUsage
                                    noticingBackend =
                                        withPendingInputs pendingNotices
                                            lockedBackend
                                    btwBackend privateParams =
                                        freshOpenAiBackend
                                            tokenProvider
                                            (readIORef privateParams)
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
                                    runSession catalog inferredTarget.targetConnectionId options provider dialect policy tools toolEnv planMode startup prompt pendingTurn transitionDraft unavailableProviders startupUnavailable paramsRef transcriptRef initialTurns
                                        previousRef persist projectRoot home cwd (Just tokenProvider) loaded.loadedOpenAiPool startupContext skillsRef skillInvocationsRef escPaused interrupt
                                        multiCtx rootTurnRef subagentSessions pendingNotices subagentStoreRoot agentTypesRef legacySubagentTarget usageRef activeAccountRef activeAccountIdRef activeSelectionRef resolveActiveAccountLabel selectAccount claimCurrentSession compactRunner activeBackend btwBackend)
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
                                                    catalog
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
                                        dialect
                                        XAIProvider
                                        ctx.multiSendToRoot
                                        (\childParamsRef ->
                                            xaiBackend xaiOptions tokenProvider
                                                (readIORef childParamsRef))
                            Nothing -> pure ()
                        let backend =
                                withPendingInputs pendingNotices $
                                    withConnectionRecovery $
                                        xaiBackend xaiOptions tokenProvider
                                            (readIORef paramsRef)
                            btwBackend privateParams =
                                xaiBackend xaiOptions tokenProvider
                                    (readIORef privateParams)
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
                        runSession catalog inferredTarget.targetConnectionId options provider dialect policy tools toolEnv planMode startup prompt pendingTurn transitionDraft unavailableProviders startupUnavailable paramsRef transcriptRef initialTurns
                            previousRef persist projectRoot home cwd (Just tokenProvider) loaded.loadedOpenAiPool startupContext skillsRef skillInvocationsRef escPaused interrupt
                            multiCtx rootTurnRef subagentSessions pendingNotices subagentStoreRoot agentTypesRef legacySubagentTarget usageRef activeAccountRef activeAccountIdRef activeSelectionRef resolveActiveAccountLabel (if isJust customGenericOptions then Nothing else Just selectHttpAccount) claimCurrentSession compactRunner activeBackend btwBackend
                    OpenRouterProvider -> do
                        let makeBackend params =
                                case customGenericOptions of
                                    Just genericOptions ->
                                        genericResponsesBackendWith
                                            (\request onEvent ->
                                                GenericResponses.createResponseWithEvents
                                                    genericOptions
                                                        { GenericResponses.model =
                                                            transportModel
                                                                (fromMaybe
                                                                    model
                                                                    request.model)
                                                        }
                                                    request
                                                    onEvent)
                                            params
                                    Nothing ->
                                        openRouterBackend openRouterOptions
                                            tokenProvider params
                        case multiCtx of
                            Just ctx ->
                                setSubagentRunner ctx.multiRegistry $
                                    runHttpSubagent
                                        subagentRuntime
                                        dialect
                                        OpenRouterProvider
                                        ctx.multiSendToRoot
                                        (\childParamsRef ->
                                            makeBackend
                                                (readIORef childParamsRef))
                            Nothing -> pure ()
                        let backend =
                                withPendingInputs pendingNotices $
                                    withConnectionRecovery $
                                        makeBackend
                                            (readIORef paramsRef)
                            btwBackend privateParams =
                                makeBackend
                                    (readIORef privateParams)
                            compactRunner =
                                installCompactOutcome previousRef transcriptRef Nothing $
                                    case customGenericOptions of
                                        Just genericOptions ->
                                            runResponsesCompactWith
                                                (\request ->
                                                    GenericResponses.createResponseWith
                                                        genericOptions
                                                            { GenericResponses.model =
                                                                transportModel
                                                                    (fromMaybe
                                                                        model
                                                                        request.model)
                                                            }
                                                        request)
                                                recordCompactionUsage
                                                paramsRef
                                                transcriptRef
                                        Nothing ->
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
                        runSession catalog inferredTarget.targetConnectionId options provider dialect policy tools toolEnv planMode startup prompt pendingTurn transitionDraft unavailableProviders startupUnavailable paramsRef transcriptRef initialTurns
                            previousRef persist projectRoot home cwd (Just tokenProvider) loaded.loadedOpenAiPool startupContext skillsRef skillInvocationsRef escPaused interrupt
                            multiCtx rootTurnRef subagentSessions pendingNotices subagentStoreRoot agentTypesRef legacySubagentTarget usageRef activeAccountRef activeAccountIdRef activeSelectionRef resolveActiveAccountLabel (Just selectHttpAccount) claimCurrentSession compactRunner activeBackend btwBackend
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
    :: StorePool
    -> Maybe FullscreenRuntime
    -> CliOptions
    -> OsPath
    -> ModelTarget
    -> Bool
    -> OsPath
    -> Text
    -> Maybe Text
    -> Maybe (SessionMeta, [SessionTurn])
    -> IO Persistence
preparePersistence
        sessionPool fullscreen options root target
        retargetResumed cwd effort prompt resumed =
    case resumed of
        Just (meta, _) -> do
            now <- getCurrentTime
            let targetChanged =
                    retargetResumed
                        && ( target.targetProvider /= meta.metaProvider
                            || target.targetConnectionId /= meta.metaConnection
                            || target.targetModelId /= meta.metaModel
                            || maybe
                                False
                                (/= target.targetWireModelId)
                                meta.metaTransportModel
                            || target.targetDialect /= meta.metaDialect
                           )
                metadataChanged =
                    retargetResumed
                        && ( targetChanged
                            || meta.metaTransportModel
                                /= Just target.targetWireModelId
                            || isNothing meta.metaLegacySubagentTarget
                           )
                activeMeta
                    | metadataChanged =
                        meta
                            { metaProvider = target.targetProvider
                            , metaConnection = target.targetConnectionId
                            , metaModel = target.targetModelId
                            , metaTransportModel =
                                Just target.targetWireModelId
                            , metaDialect = target.targetDialect
                            , metaLegacySubagentTarget =
                                Just (sessionLegacySubagentTarget meta)
                            , metaLastResponseId =
                                if targetChanged
                                    then Nothing
                                    else meta.metaLastResponseId
                            , metaUpdatedAt = now
                            }
                    | otherwise = meta
            let handle = SessionHandle
                    { sessionPool = sessionPool
                    , sessionDir = root </> fromText activeMeta.metaId
                    , sessionTempDir =
                        either
                            (error . Text.unpack)
                            id
                            (sessionTempDirForId root activeMeta.metaId)
                    , sessionMetaPath =
                        root
                            </> fromText activeMeta.metaId
                            </> unsafeEncodeUtf "meta.json"
                    , sessionTranscriptPath =
                        root
                            </> fromText activeMeta.metaId
                            </> unsafeEncodeUtf "transcript.jsonl"
                    , sessionMeta = activeMeta
                    }
            when metadataChanged $
                writeSessionMeta
                    handle.sessionPool
                    handle.sessionMetaPath
                    activeMeta
            let message = "session: " <> activeMeta.metaId <> " (resumed)"
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
                    { createPool = sessionPool
                    , createRoot = root
                    , createTarget = target
                    , createCwd = cwd
                    , createEffort = effort
                    , createTitleHint = sessionTitleFromPrompt <$> prompt
                    , createTitleIsManual = False
                    }
            | otherwise -> pure PersistenceDisabled

-- | On Ctrl-C, print a copy-pasteable --resume line when a session exists.
withInterruptResume
    :: Maybe FullscreenRuntime
    -> String
    -> Persistence
    -> a
    -> IO a
    -> IO a
withInterruptResume fullscreen progName persist interrupted action =
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
        case fullscreen of
            Nothing -> printResumeHint progName persist
            Just runtime ->
                withFullscreenSuspended runtime
                    (printResumeHint progName persist)
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
            PersistencePending _ _ _ -> pure ()
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
    :: ModelCatalog
    -> Text
    -> CliOptions
    -> Provider
    -> Dialect
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
    -> GrokSubagentSpecs
    -> Maybe LegacySubagentTarget
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
runSession catalog connectionId options provider dialect policy tools toolEnv planMode startup prompt pendingTurn initialDraft unavailableProviders startupUnavailable paramsRef transcriptRef initialTurns previous persist projectRoot home cwd tokenProvider openAiPool startupContext skillsRef skillInvocationsRef escPaused interrupt multiCtx rootTurnRef subagentSessions pendingNotices storeRoot agentTypes legacyTarget usageRef accountRef accountIdRef selectionRef accountLabel selectAccount onPersisted compactRunner backend btwBackend = do
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
    markdownState <- newIORef emptyMarkdownStreamState
    liveActive <- newIORef False
    thinkingVisible <- newIORef False
    spinnerRef <- newIORef Nothing
    reasoningBuffer <- newIORef emptyTextBuffer
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
            let transcriptLines target items
                    | null agents = []
                    | target == selected = case target of
                        AgentRoot ->
                            responseItemPreviewLines 12 items
                        AgentChild _
                            | includeSummaries ->
                                responseItemPreviewLines 12 items
                            | otherwise ->
                                responseItemLines items
                    | includeSummaries =
                        responseItemPreviewLines 0 items
                    | otherwise = []
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
                    , agentTranscript =
                        transcriptLines AgentRoot rootItems
                    }
            children <- mapM
                (materializeChild transcriptLines sessions)
                agents
            pure (selected, rootEntry : children)
          where
            materializeChild transcriptLines sessions (path, agentId, status) = do
                let target = AgentChild agentId
                items <- case Map.lookup agentId sessions of
                    Nothing -> pure []
                    Just session -> readIORef session.subSessionTranscript
                steps <- cachedAgentSteps
                    target
                    (Just status)
                    items
                    (agentStepsForStatus 2 status)
                let transcript =
                        transcriptLines target items
                            <> case status of
                                Completed (Just result)
                                    | null items
                                    , not (Text.null (Text.strip result)) ->
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
        hydrateSelectedAgent agentId = do
            effectiveModel <- readIORef modelRef
            lookupOrCreateSubagentSession
                subagentSessions
                storeRoot
                agentTypes
                provider
                connectionId
                legacyTarget
                effectiveModel
                (dialectId dialect)
                agentId
        selectAgent target = do
            previous <- readIORef selectedAgent
            when (previous /= target) $
                releaseSelectedAgent previous
            case target of
                AgentRoot -> pure ()
                AgentChild agentId -> do
                    session <-
                        (Just <$>
                            hydrateSelectedAgent agentId)
                            `catchAny` \_ -> pure Nothing
                    forM_ session \selectedSession -> do
                        withMVar selectedSession.subSessionHydrated \_ ->
                            writeIORef selectedSession.subSessionPinned True
                        -- If settlement won the race before the pin was set,
                        -- refill the same stable object now that it is pinned.
                        void
                            (hydrateSelectedAgent agentId)
                            `catchAny` \_ -> pure ()
            writeIORef selectedAgent target
        releaseSelectedAgent = \case
            AgentRoot -> pure ()
            AgentChild agentId -> do
                sessions <- readIORef subagentSessions
                forM_ (Map.lookup agentId sessions) \session -> do
                    withMVar session.subSessionHydrated \_ ->
                        writeIORef session.subSessionPinned False
                    case multiCtx of
                        Nothing -> pure ()
                        Just ctx -> do
                            status <- getStatus ctx.multiRegistry agentId
                            void $
                                persistAndEvictSubagentSessionWithStatus
                                    storeRoot ctx.multiRegistry agentTypes
                                    agentId status session
        agentViewport = AgentViewportEnv
            { viewportSelected = selectedAgent
            , viewportSelect = selectAgent
            , viewportEntries = snd <$> loadAgentSnapshot True
            }
    writeIORef startup.startupAgentSnapshot
        (loadAgentSnapshot False)
    writeIORef startup.startupAgentSelect selectAgent
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
                loadAgentsContext fullscreen options dialect home cwd [] Nothing
            freshSkills <- loadSkillsCatalogQuiet options home projectRoot cwd
            omitted <- installSkillCatalogWithOmissions
                reservedSlashNames True freshAgents
                skillsRef skillInvocationsRef freshSkills
            reportSkillCatalog True freshSkills omitted
            fresh <- readIORef freshAgents
            writeIORef startupContext fresh
        refreshSkills queueContext = do
            refreshed <- loadSkillsCatalogQuiet
                options home projectRoot cwd
            omitted <- installSkillCatalogWithOmissions
                reservedSlashNames queueContext startupContext
                skillsRef skillInvocationsRef refreshed
            when queueContext $
                reportSkillCatalog True refreshed omitted
        formatSkillWarning warning =
            "skill ignored: "
                <> toText warning.skillWarningPath
                <> ": "
                <> warning.skillWarningMessage
        formatSkillOmission omitted =
            "skills: "
                <> Text.pack (show omitted)
                <> " omitted from model context due to the catalog budget"
        reportSkillCatalog includeSummary catalog omitted =
            case fullscreen of
                Nothing -> do
                    color <- resolveColor stderr
                    when includeSummary do
                        let count = length catalog.catalogSkills
                        putTextLn stderr $
                            roleMuted color
                                (glyphSession
                                    <> "skills: loaded "
                                    <> Text.pack (show count)
                                    <> if count == 1
                                        then " skill"
                                        else " skills")
                    mapM_
                        (putTextLn stderr
                            . roleWarn color
                            . (glyphWarn <>)
                            . formatSkillWarning)
                        catalog.catalogWarnings
                    when (omitted > 0) $
                        putTextLn stderr $
                            roleWarn color
                                (glyphWarn <> formatSkillOmission omitted)
                Just runtime -> do
                    when includeSummary do
                        let count = length catalog.catalogSkills
                        emitUiEvent runtime $
                            UiSystemMessage
                                ("skills: loaded "
                                    <> Text.pack (show count)
                                    <> if count == 1
                                        then " skill"
                                        else " skills")
                    mapM_
                        (emitUiEvent runtime
                            . UiSystemMessage
                            . formatSkillWarning)
                        catalog.catalogWarnings
                    when (omitted > 0) $
                        emitUiEvent runtime
                            (UiSystemMessage (formatSkillOmission omitted))
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
            , loopBackendState = BackendStateStore
                { readBackendState = readIORef transcriptRef
                , commitBackendState = writeIORef transcriptRef
                }
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
                            approveToolDecisionWithReporter
                                (requestFullscreenPermission runtime)
                                (\case
                                    ApprovalWarning _ -> pure ()
                                    ApprovalSuccess message ->
                                        emitUiEvent runtime
                                            (UiSetNotice
                                                (Just
                                                    (successNotice message))))
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
        setSessionTempDir tempDir = do
            setToolSessionTmp toolEnv (Just tempDir)
            today <- utctDay <$> getCurrentTime
            modifyIORef' paramsRef $
                setRequestInstructions
                    (systemPromptForTools
                        dialect
                        (map (.appToolName) tools)
                        cwd
                        (Just tempDir)
                        today
                        (isOneShot options))
        env = SessionEnv
            { sessionLoop = config
            , sessionBtwBackend = btwBackend
            , sessionCompact = compactRunner
            , sessionRender = render
            , sessionProvider = provider
            , sessionConnection = connectionId
            , sessionModelCatalog = catalog
            , sessionDialect = dialect
            , sessionUnavailableProviders = unavailableProvidersRef
            , sessionStartupUnavailable = startupUnavailableRef
            , sessionPrevious = previous
            , sessionPrinted = printed
            , sessionParams = paramsRef
            , sessionPolicy = policyRef
            , sessionTranscript = transcriptRef
            , sessionPersist = persist
            , sessionDatabasePool =
                trustedPool startup.startupDatabaseStore
            , sessionTitleManager = titleManager
            , sessionTitleTurnCount = titleTurnCount
            , sessionPlanMode = planMode
            , sessionProjectRoot = projectRoot
            , sessionCwd = cwd
            , sessionHome = home
            , sessionSetTempDir = setSessionTempDir
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
    let initializeSkills = do
            markStartupStage startup "Loading skills…"
            skills <- loadSkillsCatalogQuiet
                options home projectRoot cwd
            omitted <- installSkillCatalogWithOmissions
                reservedSlashNames
                (null initialTurns && not (isJust initialPrevious))
                startupContext skillsRef skillInvocationsRef skills
            reportSkillCatalog (isNothing fullscreen) skills omitted
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
    loadTransitionAuth transition requestedProvider >>= \case
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

loadTransitionAuth
    :: Maybe ProviderTransition
    -> Maybe Provider
    -> IO (Either Text LoadedAuth)
loadTransitionAuth transition requestedProvider =
    case transition of
        Just active
            | Just selectionId <- active.transitionAccountSelectionId ->
                loadSelectedAccountAuth
                    active.transitionTarget.targetProvider
                    selectionId
                    (fromMaybe selectionId active.transitionAccountId)
        _ -> loadAuth requestedProvider

loadSelectedAccountAuth
    :: Provider
    -> Text
    -> Text
    -> IO (Either Text LoadedAuth)
loadSelectedAccountAuth provider selectionId accountId =
    case provider of
        OpenAIProvider ->
            loadAuth (Just OpenAIProvider) >>= \case
                Left err -> pure (Left err)
                Right loaded -> case loaded.loadedOpenAiPool of
                    Nothing ->
                        pure (Left
                            "OpenAI account selection requires a live account pool")
                    Just pool -> do
                        preferred <- newIORef (Just accountId)
                        pure $ Right loaded
                            { loadedTokenProvider =
                                preferredOpenAiTokenProvider
                                    preferred
                                    pool
                                    loaded.loadedTokenProvider
                            , loadedSelectionId = Just accountId
                            }
        _ -> loadAuthForAccount provider selectionId

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
    syncFullscreenPrompt env
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

-- | A retained fullscreen runtime survives provider rebuilds. Publish the new
-- session's prompt metadata before replaying a pending turn so the composer
-- does not keep showing the exhausted provider while its replacement runs.
syncFullscreenPrompt :: SessionEnv -> IO ()
syncFullscreenPrompt env =
    forM_ env.sessionFullscreen \runtime -> do
        planState <- readIORef env.sessionPlanMode.planStateRef
        params <- readIORef env.sessionParams
        policy <- readIORef env.sessionPolicy
        account <- readIORef env.sessionAccount
        usage <- readIORef env.sessionUsage
        attachments <- readIORef env.sessionAttachments
        emitUiEvent runtime $ UiSetPrompt $
            buildPromptState
                params
                planState
                policy
                account
                (isJust env.sessionSelectAccount)
                usage
                (length attachments)

buildPromptState
    :: ResponseCreateParams
    -> PlanModeState
    -> ApprovalPolicy
    -> Text
    -> Bool
    -> TokenUsage
    -> Int
    -> PromptState
buildPromptState params planState policy account accountSelectable usage attachments =
    PromptState
        { promptModel = currentModel params
        , promptEffort = currentEffort params
        , promptMode =
            replModeLabel (replModeFromState planState policy)
        , promptAccount = account
        , promptAccountSelectable = accountSelectable
        , promptUsage = usage
        , promptAttachments = attachments
        }

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
    TurnCancelled -> do
        case env.sessionFullscreen of
            Nothing -> putTrailingNewline env.sessionPrinted
            Just _ -> pure ()
        if exitAfter
            then pure RunQuit
            else repl env
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
    let cancel = env.sessionLoop.loopCancel
        renderCountdown seconds =
            let message = automaticRetryCountdownText seconds
            in case env.sessionFullscreen of
                Just runtime ->
                    emitUiEvent runtime
                        (UiSetNotice (Just (progressNotice message)))
                Nothing ->
                    renderEvent env.sessionRender (ActivityUpdated message)
        waitForCancel = do
            startedAt <- getCurrentTime
            let retryAt = addUTCTime (max 0 delay) startedAt
                poll lastShown = do
                    now <- getCurrentTime
                    let remaining = max 0 (diffUTCTime retryAt now)
                        seconds = max 0 (ceiling remaining)
                    when (lastShown /= Just seconds) (renderCountdown seconds)
                    if remaining <= 0
                        then do
                            -- Give the provider reset boundary a small margin
                            -- so the retry does not race a rounded timestamp.
                            isJust <$> timeout 250000 (waitCancel cancel)
                        else do
                            let waitMicros =
                                    max 1 $
                                        min 1000000
                                            (ceiling
                                                (realToFrac remaining
                                                    * 1_000_000
                                                    :: Double))
                            cancelled <-
                                isJust <$> timeout waitMicros (waitCancel cancel)
                            if cancelled
                                then pure True
                                else poll (Just seconds)
            poll Nothing
        waitAction = case env.sessionFullscreen of
            Just _ -> waitForCancel
            Nothing ->
                withEscCancel cancel env.sessionEscPaused waitForCancel
    resetCancel cancel
    case env.sessionFullscreen of
        Just _ -> pure ()
        Nothing -> renderEvent env.sessionRender TurnStarted
    cancelled <-
        (withTurnCancel env.sessionInterrupt cancel waitAction)
            `finally` do
                resetCancel cancel
                case env.sessionFullscreen of
                    Just _ -> pure ()
                    Nothing -> clearThinking env.sessionRender
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
                PersistencePending pending sessionId tempDir ->
                    writeIORef slotRef
                        (PersistencePending
                            pending { createEffort = level }
                            sessionId
                            tempDir)
                PersistenceActive handle -> do
                    let meta = handle.sessionMeta { metaEffort = level }
                    writeSessionMeta
                        handle.sessionPool
                        handle.sessionMetaPath
                        meta
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
    , sessionConnection = connectionId
    , sessionModelCatalog = catalog
    , sessionDialect = dialect
    , sessionStartupUnavailable = startupUnavailableRef
    , sessionPrevious = previous
    , sessionPrinted = printed
    , sessionParams = paramsRef
    , sessionPolicy = policyRef
    , sessionTranscript = transcriptRef
    , sessionPersist = persist
    , sessionDatabasePool = databasePool
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
    , sessionAccountId = accountIdRef
    , sessionAccountSelectionId = selectionRef
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
                    buildPromptState
                        params
                        planState
                        policy
                        account
                        (isJust selectAccount)
                        usage
                        (length pendingAttachments)
            readIORef startupUnavailableRef >>= \case
                Nothing ->
                    Right
                        <$> readFullscreenLineWithModels
                            runtime skillCommands (catalogModelIds catalog)
                            promptState draft
                Just unavailable ->
                    readFullscreenLineOrWithModels
                        runtime skillCommands (catalogModelIds catalog)
                        promptState draft unavailable
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
            result <- readReplLineWithSkillsAndModels
                skillCommands (catalogModelIds catalog)
                interrupt chromePrompt draft
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
            when (isNothing fullscreen) $
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
                    fullscreenEvent (UiSetNotice Nothing)
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
        ReplClipboardPasteOrText keptDraft pastedDraft -> do
            imagesResult <- readClipboardImagesImageFirst
            case nonEmptyClipboardImages imagesResult of
                Just images -> do
                    message <- queueAttachedImages
                        attachmentsRef
                        previewIdRef
                        stdoutColor
                        (isNothing fullscreen)
                        images
                    syncFullscreenImagePreviews
                    fullscreenEvent (UiSetNotice Nothing)
                    displayInfo message $
                        Text.putStrLn
                            (roleMuted stdoutColor
                                (glyphOk <> message))
                    continueWith keptDraft
                Nothing -> do
                    fullscreenEvent (UiSetNotice Nothing)
                    continueWith pastedDraft
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
                    ReplReload -> requestReload fullscreen persist
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
                                        viewport.viewportSelect target
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
                        let rawChoice = rawModelOption provider name
                        choice <-
                            resolveModelOptionDialect $
                                fromMaybe
                                    (rawChoice
                                        { modelTarget =
                                            rawChoice.modelTarget
                                                { targetConnectionId = connectionId
                                                , targetDialect = dialectId dialect
                                                }
                                        })
                                    (resolveConfiguredModel catalog name)
                        if modelTargetRequiresRebuild
                                connectionId provider (dialectId dialect) choice
                            then
                                requestModelTargetSwitch
                                    fullscreen choice "" persist >>= \case
                                    Left err -> do
                                        displayError err $
                                            Text.hPutStrLn stderr
                                                (roleError color err)
                                        continue
                                    Right result -> pure result
                            else do
                                message <- applyModelChange
                                    projectRoot provider connectionId name
                                    choice.modelTarget.targetWireModelId
                                    choice.modelTarget.targetDialect
                                    paramsRef render previous persist
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
                        handleResume databasePool fullscreen maybeId persist >>= \case
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
                                    PersistencePending _ _ _ ->
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
                                        writeSessionMeta
                                            handle'.sessionPool
                                            handle'.sessionMetaPath
                                            meta
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
                                        PersistencePending pending _ _ ->
                                            pending
                                                { createTarget =
                                                    pending.createTarget
                                                        { targetModelId = model }
                                                , createEffort = effort
                                                , createTitleHint = Nothing
                                                , createTitleIsManual = False
                                                }
                                        PersistenceActive handle ->
                                            SessionCreate
                                                { createPool = handle.sessionPool
                                                , createRoot =
                                                    takeDirectory handle.sessionDir
                                                , createTarget = ModelTarget
                                                    { targetProvider = provider
                                                    , targetConnectionId =
                                                        connectionId
                                                    , targetModelId = model
                                                    , targetWireModelId =
                                                        fromMaybe
                                                            model
                                                            handle.sessionMeta.metaTransportModel
                                                    , targetDialect =
                                                        dialectId dialect
                                                    }
                                                , createCwd =
                                                    handle.sessionMeta.metaCwd
                                                , createEffort = effort
                                                , createTitleHint = Nothing
                                                , createTitleIsManual = False
                                                }
                                handle <- createSession create
                                case slot of
                                    PersistencePending pending sessionId _ -> do
                                        _ <- removeSessionTemp
                                            pending.createRoot
                                            sessionId
                                        pure ()
                                    PersistenceActive _ -> pure ()
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
                                writeSessionMeta
                                    handle'.sessionPool
                                    handle'.sessionMetaPath
                                    meta
                                env.sessionOnPersisted handle'
                                env.sessionSetTempDir handle'.sessionTempDir
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
                                    PersistencePending _ _ _ ->
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
                    ReplWorktree -> do
                        result <- withReplActivity "Creating worktree…" $
                            createWorktree cwd (worktreeRoot env.sessionHome)
                        case result of
                            Left err -> do
                                color <- resolveColor stderr
                                displayError err $
                                    putTextLn stderr (roleError color err)
                                continue
                            Right path -> do
                                color <- resolveColor stderr
                                params <- readIORef paramsRef
                                let message = "worktree: " <> toText path
                                displayInfo message $
                                    putTextLn stderr
                                        (roleMuted color
                                            (glyphSession <> message))
                                pure
                                    (RunSwitchWorktree
                                        path
                                        provider
                                        (currentModel params)
                                        (currentEffort params))
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
                                    PersistencePending pending sessionId tempDir -> do
                                        writeIORef slotRef
                                            (PersistencePending
                                                pending
                                                    { createTitleHint = Just title
                                                    , createTitleIsManual = True
                                                    }
                                                sessionId
                                                tempDir)
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
                                    PersistencePending pending sessionId tempDir -> do
                                        writeIORef slotRef
                                            (PersistencePending
                                                pending
                                                    { createTitleHint = Nothing
                                                    , createTitleIsManual = False
                                                    }
                                                sessionId
                                                tempDir)
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
                                            updated.sessionPool
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
        modelChoice
            catalog fullscreen color connectionId provider current
                (dialectId dialect) >>= \case
            Nothing -> next
            Just rawChoice -> do
                choice <- resolveModelOptionDialect rawChoice
                if choice.modelTarget.targetProvider == provider
                    && choice.modelTarget.targetConnectionId == connectionId
                    && choice.modelTarget.targetModelId == current
                    && choice.modelTarget.targetDialect == dialectId dialect
                  then do
                    let message =
                            "model: "
                                <> connectionId
                                <> "/"
                                <> choice.modelTarget.targetModelId
                    displayInfo message $
                        Text.putStrLn
                            (roleMuted color
                                (glyphSession <> message))
                    next
                  else if not
                        (modelTargetRequiresRebuild
                            connectionId provider (dialectId dialect) choice)
                  then do
                    message <- applyModelChange
                        projectRoot provider connectionId
                        choice.modelTarget.targetModelId
                        choice.modelTarget.targetWireModelId
                        choice.modelTarget.targetDialect
                        paramsRef render previous persist
                    displayInfo message $
                        Text.putStrLn
                            (roleMuted color
                                (glyphOk <> message))
                    next
                  else
                    requestModelTargetSwitch
                        fullscreen choice keptDraft persist >>= \case
                        Left err -> do
                            displayError err $
                                Text.hPutStrLn stderr
                                    (roleError color err)
                            next
                        Right result -> pure result
    chooseAccount keptDraft next =
        case fullscreen of
            Just runtime -> do
                currentSelectionId <- readIORef selectionRef
                currentAccountId <- readIORef accountIdRef
                options <- withReplActivity
                    "Loading account usage…"
                    (loadAllAccountPickerOptions provider)
                let initial =
                        fromMaybe 0 $
                            findIndex
                                (accountPickerMatches
                                    provider
                                    currentSelectionId
                                    currentAccountId)
                                options
                requestFullscreenChoiceWithBody
                    runtime
                    "Accounts"
                    "Choose any account. Switching provider also switches to its default model."
                    initial
                    (map
                        (accountPickerRow
                            provider
                            currentSelectionId
                            currentAccountId)
                        options)
                    >>= \case
                        Just index
                            | Just option <- atMay index options ->
                                case option of
                                    AccountPickerAccount
                                        selectedProvider
                                        selectedBilling
                                        selectedSelectionId
                                        selectedAccountId
                                        selectedLabel
                                        _
                                            | selectedProvider == provider
                                            , selectedAccountId
                                                == currentAccountId ->
                                                displayInfo
                                                    ("account: " <> selectedLabel)
                                                    (pure ())
                                                    >> next
                                            | otherwise ->
                                                chooseSelectedAccount
                                                    selectedProvider
                                                    selectedBilling
                                                    selectedSelectionId
                                                    selectedAccountId
                                                    selectedLabel
                                    AccountPickerConnect selectedProvider -> do
                                        color <- resolveColor stderr
                                        connected <-
                                            withFullscreenSuspended runtime $
                                                connectProviderAccount
                                                    color
                                                    selectedProvider
                                        case connected of
                                            Nothing -> next
                                            Just selectedAccountId -> do
                                                refreshed <-
                                                    loadAllAccountPickerOptions
                                                        provider
                                                case listToMaybe
                                                        [ account
                                                        | account@(AccountPickerAccount
                                                            accountProvider
                                                            _
                                                            _
                                                            accountId
                                                            _
                                                            _) <- refreshed
                                                        , accountProvider
                                                            == selectedProvider
                                                        , accountId
                                                            == selectedAccountId
                                                        ] of
                                                    Just
                                                        (AccountPickerAccount
                                                            accountProvider
                                                            billing
                                                            selectionId
                                                            accountId
                                                            label
                                                            _) ->
                                                        chooseSelectedAccount
                                                            accountProvider
                                                            billing
                                                            selectionId
                                                            accountId
                                                            label
                                                    _ -> do
                                                        displayError
                                                            "Connected account could not be loaded."
                                                            (pure ())
                                                        next
                        _ -> next
              where
                currentBilling =
                    tokenProviderBillingMode
                        <$> tokenProvider
                chooseSelectedAccount
                    selectedProvider
                    selectedBilling
                    selectedSelectionId
                    selectedAccountId
                    selectedLabel
                        | selectedProvider == provider
                        , Just selectedBilling == currentBilling
                        , Just select <- selectAccount =
                            let liveSelectionId =
                                    case selectedProvider of
                                        OpenAIProvider -> selectedAccountId
                                        _ -> selectedSelectionId
                            in select liveSelectionId >>= \case
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
                        | otherwise =
                            readIORef paramsRef >>= \params ->
                                requestAccountProviderSwitch
                                    catalog
                                    fullscreen
                                    provider
                                    connectionId
                                    (currentModel params)
                                    (dialectId dialect)
                                    selectedProvider
                                    selectedSelectionId
                                    selectedAccountId
                                    keptDraft
                                    persist >>= \case
                                        Left err -> do
                                            displayError err (pure ())
                                            next
                                        Right result -> do
                                            displayInfo
                                                ("switching to "
                                                    <> selectedLabel
                                                    <> " ("
                                                    <> providerSlug
                                                        selectedProvider
                                                    <> ")")
                                                (pure ())
                                            pure result
            Nothing -> do
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
    :: ModelCatalog
    -> Maybe FullscreenRuntime
    -> Bool
    -> Text
    -> Provider
    -> Text
    -> DialectId
    -> IO (Maybe ModelOption)
modelChoice
        catalog fullscreen color connectionId provider current currentDialect =
    case fullscreen of
    Nothing ->
        pickModel
            catalog color connectionId provider current currentDialect
    Just runtime -> do
        picker <-
            initialPickerStateResolved
                catalog connectionId provider current currentDialect
        let options = picker.pickerAll
            rows =
                [ ( option.modelTarget.targetConnectionId
                        <> " · "
                        <> option.modelTarget.targetModelId
                        <> " · "
                        <> dialectSlug option.modelTarget.targetDialect
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

loadAllAccountPickerOptions :: Provider -> IO [AccountPickerOption]
loadAllAccountPickerOptions currentProvider = do
    discovered <- discoverSelectableLoginAccounts
    refreshed <- mapConcurrently refreshLoginAccount discovered
    now <- getCurrentTime
    let ordered =
            sortOn
                (\account ->
                    ( account.loginProvider /= currentProvider
                    , providerOrder account.loginProvider
                    , Text.toLower account.loginLabel
                    ))
                (deduplicateAccounts refreshed)
    pure $
        [ AccountPickerAccount
            account.loginProvider
            (accountBillingMode account.loginProvider account.loginBilling)
            (loginAccountSelectionId account)
            account.loginAccountId
            account.loginLabel
            (formatLoginUsageSummary account.loginProvider now account)
        | account <- ordered
        ]
            <> map AccountPickerConnect
                [OpenAIProvider, XAIProvider, OpenRouterProvider]
  where
    deduplicateAccounts = foldr addUnique []
    addUnique account accounts
        | any (samePickerAccount account) accounts = accounts
        | otherwise = account : accounts
    samePickerAccount left right
        | left.loginProvider /= right.loginProvider = False
        | left.loginProvider == OpenAIProvider =
            left.loginAccountId == right.loginAccountId
        | otherwise =
            loginAccountSelectionId left == loginAccountSelectionId right
    providerOrder = \case
        OpenAIProvider -> 0 :: Int
        XAIProvider -> 1
        OpenRouterProvider -> 2

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

accountPickerMatches
    :: Provider
    -> Text
    -> Text
    -> AccountPickerOption
    -> Bool
accountPickerMatches currentProvider currentSelectionId currentAccountId = \case
    AccountPickerAccount optionProvider _ selectionId accountId _ _ ->
        optionProvider == currentProvider
            && ( selectionId == currentSelectionId
                || accountId == currentAccountId
               )
    AccountPickerConnect _ -> False

accountPickerRow
    :: Provider
    -> Text
    -> Text
    -> AccountPickerOption
    -> (Text, Text)
accountPickerRow currentProvider currentSelectionId currentAccountId = \case
    AccountPickerAccount
        optionProvider
        _
        selectionId
        accountId
        accountPickerLabel
        accountPickerUsage ->
            ( (if optionProvider == currentProvider
                    && ( selectionId == currentSelectionId
                        || accountId == currentAccountId
                       )
                    then "✓ "
                    else "")
                <> providerSlug optionProvider
                <> " · "
                <> accountPickerLabel
            , accountPickerUsage
            )
    AccountPickerConnect optionProvider ->
        ("＋ Connect " <> providerSlug optionProvider <> " account", "")

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

fullscreenAwareSecretHooks
    :: IORef (Maybe FullscreenRuntime)
    -> SecretPromptHooks
    -> SecretPromptHooks
fullscreenAwareSecretHooks runtimeRef hooks =
    SecretPromptHooks \request ->
        withCurrentFullscreen runtimeRef
            (hooks.promptSecret request)
            \runtime ->
                Right <$> requestFullscreenSecret
                    runtime
                    "Secret requested by agent"
                    (secretRequestBody request)

secretRequestBody :: SecretPrompt -> Text
secretRequestBody request =
    Text.intercalate "\n\n" $
        filter (not . Text.null)
            [ maybe ""
                (\purpose ->
                    "Purpose: "
                        <> sanitizeSecretPromptText (Text.strip purpose))
                request.secretPromptPurpose
            , sanitizeSecretPromptText
                (Text.strip request.secretPromptMessage)
            , "Input is hidden and is not added to conversation history."
            ]

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
    -> Text
    -> Text
    -> DialectId
    -> IORef ResponseCreateParams
    -> RenderConfig
    -> IORef (Maybe Text)
    -> Persistence
    -> IO Text
applyModelChange
        projectRoot provider connection name transportModel dialectId
        paramsRef render previous persist = do
    modifyIORef' paramsRef (setModel name)
    writeIORef render.renderModelRef name
    saveProjectModel projectRoot ModelTarget
        { targetProvider = provider
        , targetConnectionId = connection
        , targetModelId = name
        , targetWireModelId = transportModel
        , targetDialect = dialectId
        }
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
                PersistencePending pending sessionId tempDir ->
                    writeIORef slotRef
                        (PersistencePending
                            pending
                                { createTarget = ModelTarget
                                    { targetProvider = provider
                                    , targetConnectionId = connection
                                    , targetModelId = name
                                    , targetWireModelId = transportModel
                                    , targetDialect = dialectId
                                    }
                                }
                            sessionId
                            tempDir)
                PersistenceActive handle -> do
                    let meta = handle.sessionMeta
                            { metaConnection = connection
                            , metaModel = name
                            , metaTransportModel = Just transportModel
                            , metaDialect = dialectId
                            }
                    writeSessionMeta
                        handle.sessionPool
                        handle.sessionMetaPath
                        meta
                    writeIORef slotRef
                        (PersistenceActive handle { sessionMeta = meta })
    pure $
        "model set to "
            <> name
            <> if clearedChain
                then " (conversation continued locally)"
                else ""

requestModelTargetSwitch
    :: Maybe FullscreenRuntime
    -> ModelOption
    -> Text
    -> Persistence
    -> IO (Either Text RunResult)
requestModelTargetSwitch fullscreen choice draft persist =
    prepareProviderTransition
        ManualTransition [] Nothing draft choice persist >>= \case
            Left err -> pure (Left err)
            Right transition -> do
                color <- resolveColor stdout
                let message =
                        "switching to "
                            <> providerSlug choice.modelTarget.targetProvider
                            <> "/"
                            <> choice.modelTarget.targetModelId
                            <> " (conversation continued locally)"
                case fullscreen of
                    Nothing ->
                        Text.putStrLn
                            (roleMuted color (glyphOk <> message))
                    Just runtime ->
                        emitUiEvent runtime (UiSystemMessage message)
                pure (Right (RunSwitchProvider transition))

requestAccountProviderSwitch
    :: ModelCatalog
    -> Maybe FullscreenRuntime
    -> Provider
    -> Text
    -> Text
    -> DialectId
    -> Provider
    -> Text
    -> Text
    -> Text
    -> Persistence
    -> IO (Either Text RunResult)
requestAccountProviderSwitch
    catalog
    fullscreen
    currentProvider
    currentConnection
    currentModelId
    currentDialect
    selectedProvider
    selectionId
    accountId
    draft
    persist = do
        currentTransportModel <-
            persistenceTransportModel currentModelId persist
        let rawChoice =
                accountSwitchTarget
                    catalog
                    currentProvider
                    currentConnection
                    currentModelId
                    currentTransportModel
                    currentDialect
                    selectedProvider
        choice <-
            if currentProvider == selectedProvider
                then pure rawChoice
                else resolveModelOptionDialect rawChoice
        validateSelectedAccountTarget
            selectedProvider
            selectionId
            accountId >>= \case
                Left err -> pure (Left err)
                Right () -> do
                    sessionId <- ensureTransitionSessionId persist
                    let transition = ProviderTransition
                            { transitionTarget = choice.modelTarget
                            , transitionAccountSelectionId =
                                Just selectionId
                            , transitionAccountId = Just accountId
                            , transitionSessionId = sessionId
                            , transitionPendingTurn = Nothing
                            , transitionDraft = draft
                            , transitionUnavailableProviders = []
                            , transitionCause = ManualTransition
                            , transitionAutomaticBilling = Nothing
                            }
                        modelMessage
                            | currentProvider == selectedProvider =
                                providerSlug selectedProvider
                                    <> "/"
                                    <> choice.modelTarget.targetModelId
                            | otherwise =
                                providerSlug selectedProvider
                                    <> "/"
                                    <> choice.modelTarget.targetModelId
                                    <> " (provider changed)"
                    color <- resolveColor stdout
                    case fullscreen of
                        Nothing ->
                            Text.putStrLn
                                (roleMuted color
                                    (glyphOk
                                        <> "switching to "
                                        <> modelMessage))
                        Just runtime ->
                            emitUiEvent runtime $
                                UiSystemMessage
                                    ("switching to " <> modelMessage)
                    pure (Right (RunSwitchProvider transition))

accountSwitchTarget
    :: ModelCatalog
    -> Provider
    -> Text
    -> Text
    -> Text
    -> DialectId
    -> Provider
    -> ModelOption
accountSwitchTarget
        catalog currentProvider currentConnection currentModelId
        currentTransportModel currentDialect
        selectedProvider =
    if currentProvider == selectedProvider
        then
            let current = rawModelOption selectedProvider currentModelId
            in current
                { modelTarget = current.modelTarget
                    { targetConnectionId = currentConnection
                    , targetWireModelId = currentTransportModel
                    , targetDialect = currentDialect
                    }
                }
        else
            fromMaybe
                (error "validated default model is missing")
                (defaultModelOptionFor catalog selectedProvider)

persistenceTransportModel :: Text -> Persistence -> IO Text
persistenceTransportModel fallback = \case
    PersistenceDisabled -> pure fallback
    PersistenceEnabled slotRef ->
        readIORef slotRef >>= \case
            PersistencePending pending _ _ ->
                pure pending.createTarget.targetWireModelId
            PersistenceActive handle ->
                pure $
                    fromMaybe
                        fallback
                        handle.sessionMeta.metaTransportModel

validateSelectedAccountTarget
    :: Provider
    -> Text
    -> Text
    -> IO (Either Text ())
validateSelectedAccountTarget provider selectionId accountId =
    loadSelectedAccountAuth provider selectionId accountId >>= \case
        Left err ->
            pure $ Left $
                "cannot switch to "
                    <> providerSlug provider
                    <> " account: "
                    <> err
        Right loaded ->
            probeLoadedAvailability loaded >>= \case
                Left err -> do
                    now <- getCurrentTime
                    pure $ Left $
                        "cannot switch to "
                            <> providerSlug provider
                            <> " account: "
                            <> formatApiErrorInlineAt now err
                Right usable
                    | usable.loadedProvider /= provider ->
                        pure $ Left $
                            "cannot switch to "
                                <> providerSlug provider
                                <> " account: auth resolved "
                                <> providerSlug usable.loadedProvider
                    | otherwise -> pure (Right ())

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
                env.sessionModelCatalog
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
                env.sessionModelCatalog
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
        (Just billing, Just pending) -> do
            home <- getHomeDirectory
            loadModelCatalog home >>= \case
                Left _ -> pure Nothing
                Right catalog ->
                    chooseAutomaticProviderTransition
                        catalog
                        fullscreen
                        billing
                        failed.transitionTarget.targetProvider
                        failed.transitionUnavailableProviders
                        failed.transitionSessionId
                        pending
                        apiError
        _ -> pure Nothing

chooseAutomaticProviderTransition
    :: ModelCatalog
    -> Maybe FullscreenRuntime
    -> BillingMode
    -> Provider
    -> [Provider]
    -> Maybe Text
    -> PendingTurn
    -> ApiError
    -> IO (Maybe ProviderTransition)
chooseAutomaticProviderTransition
    catalog fullscreen sourceBilling current unavailable0 sessionId pending apiError =
    tryCandidates unavailable candidates
  where
    unavailable = markUnavailable current unavailable0
    candidates = fallbackCandidates catalog unavailable0 current apiError

    tryCandidates unavailable = \case
        [] -> pure Nothing
        rawChoice : rest -> do
            choice <- resolveModelOptionDialect rawChoice
            validateAutomaticProviderTarget sourceBilling choice >>= \case
                Left err -> do
                    let message =
                            "skipping "
                            <> providerSlug choice.modelTarget.targetProvider
                            <> ": "
                            <> err
                    case fullscreen of
                        Nothing -> do
                            color <- resolveColor stderr
                            putTextLn stderr (roleMuted color message)
                        Just runtime ->
                            emitUiEvent runtime (UiSystemMessage message)
                    tryCandidates
                        (markUnavailable choice.modelTarget.targetProvider unavailable)
                        rest
                Right () -> do
                    let message =
                            providerSlug current
                            <> " unavailable; trying this turn with "
                            <> providerSlug choice.modelTarget.targetProvider
                            <> "/"
                            <> choice.modelTarget.targetModelId
                    case fullscreen of
                        Nothing -> do
                            color <- resolveColor stderr
                            putTextLn stderr
                                (roleWarn color (glyphWarn <> message))
                        Just runtime ->
                            emitUiEvent runtime (UiSystemMessage message)
                    pure $ Just ProviderTransition
                        { transitionTarget = choice.modelTarget
                        , transitionAccountSelectionId = Nothing
                        , transitionAccountId = Nothing
                        , transitionSessionId = sessionId
                        , transitionPendingTurn = Just pending
                        , transitionDraft = ""
                        , transitionUnavailableProviders = unavailable
                        , transitionCause = AutomaticFallback
                        , transitionAutomaticBilling = Just sourceBilling
                        }

chooseStartupProviderTransition
    :: ModelCatalog
    -> Maybe FullscreenRuntime
    -> BillingMode
    -> Provider
    -> [Provider]
    -> Maybe Text
    -> ApiError
    -> IO (Maybe ProviderTransition)
chooseStartupProviderTransition
    catalog fullscreen sourceBilling current unavailable0 sessionId apiError =
    tryCandidates unavailable candidates
  where
    unavailable = markUnavailable current unavailable0
    candidates = fallbackCandidates catalog unavailable0 current apiError

    tryCandidates unavailable = \case
        [] -> pure Nothing
        rawChoice : rest -> do
            choice <- resolveModelOptionDialect rawChoice
            validateAutomaticProviderTarget sourceBilling choice >>= \case
                Left err -> do
                    let message =
                            "skipping "
                            <> providerSlug choice.modelTarget.targetProvider
                            <> ": "
                            <> err
                    forM_ fullscreen \runtime ->
                        emitUiEvent runtime (UiSystemMessage message)
                    tryCandidates
                        (markUnavailable choice.modelTarget.targetProvider unavailable)
                        rest
                Right () -> do
                    let message =
                            providerSlug current
                            <> " account unavailable; switched to "
                            <> providerSlug choice.modelTarget.targetProvider
                            <> "/"
                            <> choice.modelTarget.targetModelId
                    forM_ fullscreen \runtime ->
                        emitUiEvent runtime (UiSystemMessage message)
                    pure $ Just ProviderTransition
                        { transitionTarget = choice.modelTarget
                        , transitionAccountSelectionId = Nothing
                        , transitionAccountId = Nothing
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
prepareProviderTransition cause unavailable pending draft rawChoice persist = do
    choice <- resolveModelOptionDialect rawChoice
    validateProviderTarget choice >>= \case
        Left err -> pure (Left err)
        Right () -> do
            sessionId <- ensureTransitionSessionId persist
            pure $ Right ProviderTransition
                { transitionTarget = choice.modelTarget
                , transitionAccountSelectionId = Nothing
                , transitionAccountId = Nothing
                , transitionSessionId = sessionId
                , transitionPendingTurn = pending
                , transitionDraft = draft
                , transitionUnavailableProviders = unavailable
                , transitionCause = cause
                , transitionAutomaticBilling = Nothing
                }

validateProviderTarget :: ModelOption -> IO (Either Text ())
validateProviderTarget choice =
    if choice.modelTarget.targetConnectionId
        `notElem` map builtinConnectionId
            [OpenAIProvider, XAIProvider, OpenRouterProvider]
    then pure (Right ())
    else fmap (() <$) (loadValidatedProviderTarget choice)

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
    if not
        (providerSupportsDialect
            choice.modelTarget.targetProvider
            choice.modelTarget.targetDialect)
    then
        pure $ Left $
            "dialect "
                <> dialectSlug choice.modelTarget.targetDialect
                <> " is incompatible with provider "
                <> providerSlug choice.modelTarget.targetProvider
    else loadAuth (Just choice.modelTarget.targetProvider) >>= \case
        Left err -> pure $ Left $
            "cannot switch to "
                <> providerSlug choice.modelTarget.targetProvider
                <> ": "
                <> err
        Right loaded ->
            probeLoadedAvailability loaded >>= \case
                Left err -> do
                    now <- getCurrentTime
                    pure $ Left $
                        "cannot switch to "
                            <> providerSlug choice.modelTarget.targetProvider
                            <> ": "
                            <> formatApiErrorInlineAt now err
                Right usable
                    | usable.loadedProvider /= choice.modelTarget.targetProvider ->
                        pure $ Left $
                            "cannot switch to "
                                <> providerSlug choice.modelTarget.targetProvider
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
    saveProjectModel projectRoot transition.transitionTarget
    case persist of
        PersistenceDisabled -> pure ()
        PersistenceEnabled slotRef -> do
            slot <- readIORef slotRef
            case slot of
                PersistencePending pending sessionId tempDir ->
                    writeIORef slotRef $ PersistencePending
                        pending
                            { createTarget = transition.transitionTarget }
                        sessionId
                        tempDir
                PersistenceActive handle -> do
                    now <- getCurrentTime
                    let previousMeta = handle.sessionMeta
                        meta = previousMeta
                            { metaProvider = transition.transitionTarget.targetProvider
                            , metaConnection =
                                transition.transitionTarget.targetConnectionId
                            , metaModel = transition.transitionTarget.targetModelId
                            , metaTransportModel =
                                Just transition.transitionTarget.targetWireModelId
                            , metaDialect = transition.transitionTarget.targetDialect
                            , metaLegacySubagentTarget =
                                Just
                                    (sessionLegacySubagentTarget previousMeta)
                            , metaLastResponseId = Nothing
                            , metaUpdatedAt = now
                            }
                    writeSessionMeta
                        handle.sessionPool
                        handle.sessionMetaPath
                        meta
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
    Backend \state previous inputs onEvent -> do
        result <- submit state previous inputs onEvent
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
    :: Maybe FullscreenRuntime
    -> Persistence
    -> IO RunResult
requestReload fullscreen persist = do
    color <- resolveColor stderr
    let reportInfo message =
            case fullscreen of
                Nothing ->
                    putTextLn stderr
                        (roleMuted color (glyphSession <> message))
                Just runtime ->
                    emitUiEvent runtime (UiSystemMessage message)
        reportError message =
            case fullscreen of
                Nothing ->
                    putTextLn stderr (roleError color message)
                Just runtime ->
                    emitUiEvent runtime (UiErrorMessage message)
    case persist of
        PersistenceDisabled -> do
            reportError ":reload needs a persisted REPL session"
            pure RunQuit
        PersistenceEnabled slotRef -> do
            handle <- ensureSession slotRef
            reportInfo ("reloading; session " <> handle.sessionMeta.metaId)
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
                    when (isNothing fullscreen) $
                        putTrailingNewline printed
                    pure Nothing

-- | Discover AGENTS.md once for a fresh session. Resumed transcripts keep
-- whatever instructions were already in history.
loadAgentsContext
    :: Maybe FullscreenRuntime
    -> CliOptions
    -> Dialect
    -> OsPath
    -> OsPath
    -> [ResponseItem]
    -> Maybe Text
    -> IO (IORef (Maybe Text))
loadAgentsContext fullscreen options dialect home cwd initialItems initialPrevious
    | not options.optAgentsMd = newIORef Nothing
    | not (null initialItems) || isJust initialPrevious = newIORef Nothing
    | otherwise = do
        let discoverOptions = DiscoverOptions
                { discoverMaxBytes = defaultDiscoverOptions.discoverMaxBytes
                , discoverGlobalDir = Just (globalAgentsHomeDir dialect home)
                , discoverRootMarkers = defaultDiscoverOptions.discoverRootMarkers
                }
        loaded <- discoverProjectInstructions discoverOptions cwd
        let files = loadedInstructionFiles loaded
        case formatAgentsMdForDialect dialect cwd loaded of
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
    :: StorePool
    -> Maybe FullscreenRuntime
    -> Maybe Text
    -> Persistence
    -> IO (Maybe RunResult)
handleResume databasePool fullscreen maybeId persist = do
    color <- resolveColor stderr
    home <- getHomeDirectory
    let reportInfo message =
            case fullscreen of
                Nothing ->
                    Text.hPutStrLn stderr
                        (roleMuted color (glyphSession <> message))
                Just runtime ->
                    emitUiEvent runtime (UiSystemMessage message)
        reportError message =
            case fullscreen of
                Nothing ->
                    Text.hPutStrLn stderr (roleError color message)
                Just runtime ->
                    emitUiEvent runtime (UiErrorMessage message)
        root = sessionsRoot home
        resume sessionId = do
            currentId <- currentSessionId persist
            if Just sessionId == currentId
                then do
                    reportInfo ("already on session " <> sessionId)
                    pure Nothing
                else
                    loadSession databasePool root sessionId >>= \case
                        Left err -> do
                            reportError err
                            pure Nothing
                        Right _ -> pure (Just (RunResumeSession sessionId))
    case maybeId of
        Just sessionId -> resume sessionId
        Nothing -> do
            sessions <- listSessions databasePool root
            currentId <- currentSessionId persist
            pickResumeChoice
                databasePool fullscreen color root currentId sessions >>= \case
                Nothing -> pure Nothing
                Just sessionId -> resume sessionId

pickResumeChoice
    :: StorePool
    -> Maybe FullscreenRuntime
    -> Bool
    -> OsPath
    -> Maybe Text
    -> [SessionMeta]
    -> IO (Maybe Text)
pickResumeChoice databasePool fullscreen color root currentId sessions =
  case fullscreen of
    Nothing -> pickResumeSession databasePool color root sessions
    Just runtime -> do
        now <- getCurrentTime
        let browser =
                initialResumeBrowser now (map resumeEntryFromMeta sessions)
            deleteEntry sessionId
                | currentId == Just sessionId =
                    pure (Left "cannot delete the current session")
                | otherwise =
                    deleteSession databasePool root sessionId
        fmap (.resumeId)
            <$> requestFullscreenResume
                runtime
                browser
                (loadResumeEntry databasePool root)
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
            PersistencePending _ _ _ -> Nothing
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
    -> IORef (Maybe (Int, Int))
    -> (TokenUsage -> IO ())
    -> (OpenAiCompactionSender, Backend)
lockedOpenAiSession compactThreshold wsLock provider activeConnection
        getParams contextTokens
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
                openAiBackendWith sendResponse getParams
        compactSender request =
            sendAuxiliary request Nothing (const (pure ()))
        compactingBackend =
            autoCompactOpenAiBackendWithSender
                compactThreshold
                compactSender
                recordCompactionUsage
                getParams
                contextTokens
                baseBackend
        serializedBackend = Backend \state previous inputs onEvent ->
            withMVar wsLock \_ ->
                compactingBackend.submitTurn state previous inputs onEvent
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
