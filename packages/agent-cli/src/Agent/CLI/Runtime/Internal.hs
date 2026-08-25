-- | Command-line entry point: one-shot @-p@ or an interactive REPL.
module Agent.CLI.Runtime.Internal
    ( DevResult(..)
    , afterDev
    , accountSwitchTarget
    , applyReplMode
    , buildPromptState
    , cycleReplInteraction
    , devArgs
    , devMain
    , devMainResume
    , formatMcpModelNotice
    , formatMcpModelNoticeFor
    , formatMcpProgress
    , formatReplStatusLine
    , formatRepositoryPath
    , formatStartupTimings
    , formatTokenUsage
    , learnAboutUserOnboardingPrompt
    , run
    , withRestoredCurrentDirectory
    ) where

import Agent.CLI.Artifact (fencedCodeBlock, lastDiffBlock)
import Agent.CLI.Afk
    ( AfkTarget(..)
    , handoffLocal
    , handoffRemote
    , parseAfkTarget
    )
import Agent.CLI.AccountSelection
    ( SelectedAccount(..)
    , providerSupportsUsageAccountSelection
    , selectProviderAccount
    )
import Agent.CLI.Auth
    ( LoadedAuth(..)
    , loadAuthForAccount
    , preferredOpenAiTokenProvider
    , probeLoadedAuthCredential
    , staticCredentialProvider
    )
import Agent.CLI.Secret (promptSecretLine)
import Agent.CLI.AgentViewport
    ( AgentTarget(..)
    , AgentViewportEnv(..)
    , renderAgentViewportPanelFor
    )
import Agent.CLI.SessionTitle
    ( invalidateSessionTitles
    , requestSessionTitle
    )
import Agent.CLI.AgentSessions
    ( AgentSessionToolsEnv(..)
    , SessionThreadManager
    , agentSessionTools
    , closeSessionThreadManager
    , launchSessionThread
    , newSessionThreadManager
    , signalManagedSessionReady
    , sessionThreadStatus
    )
import Agent.CLI.ManagedTurn (ManagedTurnRequest(..))
import Agent.CLI.GatewayBridge (managedGatewayTools)
import Agent.CLI.Approval (toggleAlwaysApprove)
import Agent.CLI.AccountPicker
    ( AccountPickerOption(..)
    , accountPickerMatches
    , accountPickerRow
    , loadAllAccountPickerOptions
    )
import Agent.CLI.Recap
    ( RecapKind(..)
    , RecapRequest(..)
    )
import Agent.CLI.Clipboard
    ( formatImageSize
    , loadImagesFromPastedText
    , nonEmptyClipboardImages
    , readClipboardImagesForPaste
    , readClipboardImagesImageFirst
    )
import Agent.CLI.Command
import Agent.CLI.Config
    ( HarnessConfig(..)
    , McpServerConfig(..)
    , loadHarnessConfig
    , useProgressiveMcp
    )
import Agent.CLI.Compaction
    ( CompactOutcome(..)
    , installLiveCompactOutcome
    , runProviderCompactWith
    , runResponsesCompactWith
    )
import Agent.CLI.Connectivity (withConnectionRecovery)
import Agent.CLI.Database (databaseTools)
import Agent.CLI.Database.Store
    ( databaseToolsEnvForStore
    , deriveDatabaseScopes
    )
import Agent.CLI.LearnedSkills (learnedSkillTools)
import Agent.CLI.LearnedSkills.Store (learnedSkillToolsEnvForStore)
import Agent.CLI.Error
    ( formatException
    , formatApiErrorAt
    , formatApiErrorInlineAt
    )
import Agent.CLI.Input
    ( ReplLine(..)
    , formatPasteChip
    , readReplLineWithCatalog
    , submissionPromptText
    )
import Agent.CLI.ReplMode
    ( ReplMode(..)
    , replModeLabel
    , replModeFromState
    )
import Agent.CLI.PromptHooks
    ( fullscreenAwarePlanHooks
    , fullscreenAwareSecretHooks
    )
import Agent.CLI.Runtime.Types
    ( DevResult(..)
    , PendingTurnPresentation(..)
    , PreparedAgent(..)
    , RunResult(..)
    )
import Agent.CLI.Runtime.Persistence (preparePersistence)
import Agent.CLI.Runtime.Recap
    ( runSessionRecap
    , runSessionTurnSummary
    )
import Agent.CLI.Session.Runtime.Types
    ( SessionBackend(..)
    , SessionRequest(..)
    , StartupCancelled(..)
    , StartupFailure(..)
    , StartupRuntime(..)
    )
import Agent.CLI.Interrupt
    ( CtrlCDecision(..)
    , catchUserInterrupt
    , newInterruptState
    , noteFullscreenCtrlC
    , withCtrlCHandler
    )
import Agent.CLI.Login
    ( connectProviderAccount
    , runLoginManager
    )
import Agent.CLI.Lsp
    ( LspStartup(..)
    , closeLspRuntime
    , lspRuntimeTool
    , newLspRuntime
    )
import Agent.CLI.McpManager (runMcpManager)
import Agent.CLI.McpStatus
    ( formatMcpModelNotice
    , formatMcpModelNoticeFor
    , formatMcpProgress
    , summarizeMcpStatuses
    )
import Agent.CLI.ModelConfig
    ( ConnectionKind(..)
    , ModelConnection(..)
    , ResponsesConnection(..)
    , builtinConnectionId
    , catalogConnection
    , loadModelCatalogAt
    )
import Agent.CLI.Models
    ( ModelOption(..)
    , ModelTarget(..)
    , catalogModelIds
    , defaultModelFor
    , modelTargetRequiresRebuild
    , rawModelOption
    , resolveConfiguredModel
    , resolveModelOptionDialect
    , resolvePersistedDialect
    )
import Agent.CLI.Options
import Agent.Store.Postgres
    ( Store
    , closeStore
    , openStore
    , trustedPool
    )
import Agent.Store.Types (renderStoreError)
import Agent.CLI.PendingInputs (withPendingInputs)
import Agent.CLI.Plan (cliPlanHooks)
import Agent.CLI.Progress
    ( osc9ProgressIndeterminate
    , osc9ProgressRemove
    , wrapOscForTmux
    )
import Agent.CLI.Project
    ( ProjectAccount(..)
    , ProjectModel(..)
    , ProjectSettings(..)
    , loadProjectSettings
    , projectAccountFor
    , projectModelProvider
    , resolveProjectRoot
    , saveProjectModel
    )
import Agent.CLI.Prompt
    ( subscriptionSubagentModelGuidance
    , systemPromptForTools
    )
import Agent.CLI.Request (requestParams)
import Agent.CLI.ProviderFallback
    ( allowsAutomaticBillingFallback
    , isProviderUnavailable
    )
import Agent.CLI.Provider.OpenAI
    ( OpenAiPersistentConnection(..)
    , lockedOpenAiSession
    )
import Agent.CLI.ProviderAvailability (probeLoadedAvailability)
import Agent.CLI.Provider.Switch
    ( accountSwitchTarget
    , applyModelChange
    , chooseStartupProviderTransition
    , continueAutomaticFallback
    , loadSelectedAccountAuth
    , prepareTransitionBackend
    , reloadAuth
    , reportProviderUnavailable
    , requestAccountProviderSwitch
    , requestAutomaticProviderFallback
    , requestModelTargetSwitch
    , requestStartupProviderFallback
    )
import Agent.CLI.ProviderTransition
    ( PendingTurn(..)
    , ProviderTransition(..)
    , TransitionCause(..)
    , TurnResult(..)
    , applyProviderTransition
    )
import Agent.CLI.SessionState (SessionState(..), newSessionState)
import Agent.CLI.Render
    ( RenderConfig(..)
    , clearThinking
    , putTextLn
    , renderEvent
    , renderPrintedText
    , resetRenderPrintedText
    )
import Agent.CLI.Session
import Agent.CLI.Session.Attachments
    ( putImagePreview
    , queueAttachedImages
    , queueClipboardImages
    )
import Agent.CLI.Session.Choices
    ( accountUsageText
    , atMay
    , effortChoice
    , modelChoice
    , showAccountUsage
    )
import Agent.CLI.Session.History
    ( detectGitBranch
    , foldSessionItems
    , hydrateUiHistory
    , LiveConversation(..)
    , readLiveAttachments
    , readLiveTranscript
    , modifyLiveAttachments
    , writeLiveTranscript
    )
import Agent.CLI.Session.Interaction
    ( buildPromptState
    , runBtwQuestion
    , setSessionEffort
    )
import Agent.CLI.Session.Lifecycle (SessionContinuation(..))
import qualified Agent.CLI.Session.Lifecycle as SessionLifecycle
import qualified Agent.CLI.Session.Runner as SessionRunner
import Agent.CLI.Session.Selection
    ( currentSessionId
    , handleConversationSearch
    , handleResume
    , loadPrompt
    , pickAgentChoice
    , reservedSessionId
    )
import Agent.CLI.SessionAdmin
    ( managedPostgresConfigForHome
    , runImportSession
    , runListSessions
    , runShowSession
    , runStorageAdmin
    , runWaitSession
    )
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
    , skillInvocationCommand
    )
import Agent.CLI.Status
    ( applyReplMode
    , cycleReplInteraction
    , formatReplStatusLine
    , formatTokenUsage
    )
import Agent.CLI.StartupContext (loadAgentsContext)
import Agent.CLI.Startup.Format
    ( formatRepositoryPath
    , formatStartupDuration
    , formatStartupTimings
    )
import Agent.CLI.Startup.Auth
    ( learnAboutUserOnboardingPrompt
    , loadStartupAuth
    , markStartupStage
    , recordStartupTiming
    , setStartupNotice
    , startupDie
    )
import Agent.CLI.Subagents.Runtime
    ( SubagentRuntime(..)
    , flushAllSubagentSnapshots
    , freshOpenAiBackend
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
import Agent.CLI.Tools (schemasFromAppTools)
import Agent.CLI.Dialects
    ( CodingTools(..)
    , codingToolsForWithTypes
    , filterBashTools
    , filterGhciTools
    )
import Agent.CLI.TUI.App
    ( FullscreenInputBuffer
    , FullscreenRuntime
    , emitUiEvent
    , newFullscreenInputBuffer
    , newFullscreenRuntime
    , queuedFullscreenInputDisplays
    , readFullscreenLineOrWithCatalog
    , readFullscreenLineWithCatalog
    , requestFullscreenChoiceWithBody
    , runFullscreen
    , setFullscreenSessionActions
    , setFullscreenImagePreviews
    , setFullscreenWindowTitle
    , withFullscreenSuspended
    )
import Agent.TUI.Model
    ( UiEvent(..)
    , UiState(..)
    , infoNotice
    , progressNotice
    , warningNotice
    , reduceUi
    )
import Agent.TUI.Motion (nativeProgressAnimationEnabled)
import Agent.CLI.Turn (runOneTurn)
import Agent.CLI.Usage
    ( formatGrokLimitStatus
    , formatOpenAiLimitStatus
    , formatOpenRouterLimitStatus
    )
import Agent.CLI.WebFetch
    ( closeWebFetchRuntime
    , newWebFetchRuntime
    , webFetchRuntimeTool
    )
import Agent.CLI.Worktree
    ( createWorktree
    , isUnderWorktreeRoot
    , removeWorktree
    , worktreeRoot
    )
import Agent.Cancel (requestCancel)
import Agent.Claude
    ( ClaudeCodeAuth(..)
    , ClaudeCodeOptions(..)
    , ClaudeCodePermission(..)
    , claudeCodeOneShotBackend
    , defaultClaudeCodeOptions
    , loadClaudeCodeAuth
    , withClaudeCodeBackend
    )
import Agent.Loop
import qualified Agent.MCP as MCP
import Agent.Error
    ( ApiError(..) )
import Agent.Dialect
    ( DialectId(..)
    , dialectForId
    , dialectId
    , dialectSlug
    )
import Agent.Skills
    ( Skill(..)
    , SkillCatalog(..)
    , SkillInvocation(..)
    , formatSkillActivation
    , resolveSkillInvocation
    , resolveSkillMentions
    )
import Agent.OpenAI.Compaction
    ( clearSessionUserText
    , compactSessionUserText
    , newSessionUserText
    )
import qualified Agent.OpenAI.Auth as OpenAI
import Agent.Responses.Types
import Agent.Responses.GenericBackend (genericResponsesBackendWith)
import Agent.Responses.GenericClient (GenericClientOptions(..))
import qualified Agent.Responses.GenericClient as GenericResponses
import Agent.OpenAI.Usage (fetchUsage)
import Agent.OpenAI.WebSocketClient
    ( CodexAuthFailed(..)
    , closeCodexConn
    , codexConnTurnState
    , resetCodexTurnState
    , withCodexWsCredential
    , withCodexWsWithProvider
    )
import Agent.Provider
    ( BillingMode(..)
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
    , SubagentConfig(..)
    , closeSubagentRegistry
    , defaultMaxConcurrent
    , defaultSubagentConfig
    , formatCompletionNotice
    , interruptActiveSubagents
    , newSubagentRegistry
    , setSubagentOnComplete
    , setSubagentOnSettled
    , setSubagentRunner
    )
import Agent.GrokBuild.Dialect.Goal
    ( activateGoal
    , clearGoal
    , formatGoalSnapshot
    , pauseGoal
    , readGoal
    , resumeGoal
    )
import Agent.GrokBuild.Dialect.Runtime (GrokRuntimeControl(..))
import Agent.GrokBuild.Dialect.Workflow
    ( formatWorkflowRuns
    , workflowRunSnapshots
    )
import Agent.Subagents.TaskPath (taskPathRoot)
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
import qualified Agent.OpenRouter.Usage as OpenRouterUsage
import Agent.OsPath (toText, unsafeToFilePath)
import Agent.XAI.LoopBackend (xaiBackend)
import qualified Agent.XAI.Options as XAI
import qualified Agent.XAI.Usage as XAIUsage
import Control.Applicative ((<|>))
import Control.Concurrent.Async
    ( cancel
    , concurrently
    , concurrently_
    , link
    , waitCatch
    , waitEitherCatch
    , waitSTM
    , withAsync
    )
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
import Control.Concurrent.STM (retry)
import Control.Exception (AsyncException(UserInterrupt))
import Control.Exception.Safe
    ( SomeException
    , catchAny
    , finally
    , mask
    , mask_
    , onException
    , throwIO
    , try
    )
import Control.Monad (forM_, unless, void, when)
import qualified Data.ByteString as BS
import Data.IORef
import Data.List (findIndex)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Set as Set
import Data.Time.Clock
    ( getCurrentTime
    , utctDay
    )
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
import System.Exit (die)
import System.IO
    ( Handle
    , IOMode(AppendMode)
    , hFlush
    , hIsTerminalDevice
    , stderr
    , stdin
    , stdout
    , withFile
    )
import System.Posix.Files (setFileMode)

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
        Right (WaitSession sessionId) -> runWaitSession sessionId >> pure DevQuit
        Right (ImportSession cwd) -> runImportSession cwd >> pure DevQuit
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
        Right (WaitSession sessionId) -> runWaitSession sessionId
        Right (ImportSession cwd) -> runImportSession cwd
        Right (Storage command) -> runStorageAdmin command
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
runAgentWithRestarts options =
    catchUserInterrupt
        (do
            home <- getHomeDirectory
            let root = sessionsRoot home
            mcpSupervisor <- MCP.newMcpSupervisor
            sessionThreads <-
                newSessionThreadManager root
                    `onException` MCP.closeMcpSupervisor mcpSupervisor
            let processRuntime = AgentProcessRuntime
                    { processMcpSupervisor = mcpSupervisor
                    , processSessionThreads = sessionThreads
                    }
            withRestoredCurrentDirectory
                (runAgentWithRuntime processRuntime foregroundRunMode options)
                `finally`
                    (closeSessionThreadManager sessionThreads
                        `finally` MCP.closeMcpSupervisor mcpSupervisor))
        (pure DevQuit)

runAgentWithRuntime
    :: AgentProcessRuntime
    -> AgentRunMode
    -> CliOptions
    -> IO DevResult
runAgentWithRuntime processRuntime runMode options = do
    fullscreenInputs <- newFullscreenInputBuffer
    sessionState <- newSessionState
    go fullscreenInputs sessionState options Nothing
  where
    go fullscreenInputs sessionState current transition =
        runAgent
            processRuntime
            runMode
            fullscreenInputs
            sessionState
            current
            transition >>= \case
            RunResumeSession sessionId ->
                newSessionState >>= \nextState ->
                    go fullscreenInputs nextState
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
                newSessionState >>= \nextState ->
                    go fullscreenInputs nextState
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
                go fullscreenInputs sessionState
                    (applyProviderTransition current next)
                    (Just next)
            RunRestart sessionId ->
                go fullscreenInputs sessionState
                    (restartSessionOptions current sessionId)
                    Nothing
            RunProviderStartFailed apiError ->
                case transition of
                    Just failed
                        | failed.transitionCause == AutomaticFallback ->
                            continueAutomaticFallback
                                runMode.runCwdHint
                                runMode.runStderr
                                Nothing
                                failed
                                apiError >>= \case
                                Just next ->
                                    go fullscreenInputs sessionState
                                        (applyProviderTransition current next)
                                        (Just next)
                                Nothing
                                    | runMode.runInBackground -> do
                                        now <- getCurrentTime
                                        throwIO $
                                            StartupFailure
                                                (Text.unpack
                                                    (formatApiErrorAt
                                                        now
                                                        apiError))
                                    | otherwise -> do
                                        reportProviderUnavailable Nothing apiError
                                        pure DevQuit
                    _
                        | runMode.runInBackground -> do
                            now <- getCurrentTime
                            throwIO $
                                StartupFailure
                                    (Text.unpack
                                        (formatApiErrorAt now apiError))
                        | otherwise -> pure DevQuit
            RunQuit -> pure DevQuit
            RunReload sessionId -> pure (DevReload sessionId)

runInProcessSessionTurn
    :: AgentProcessRuntime
    -> CliOptions
    -> ApprovalPolicy
    -> Bool
    -> Bool
    -> SessionHandle
    -> Text
    -> IO (Either Text ())
runInProcessSessionTurn
        processRuntime parentOptions policy ghciEnabled bashEnabled handle message =
    withFile logPath AppendMode \logHandle -> do
        setFileMode logPath 0o600
        runAgentWithRuntime
            processRuntime
            (backgroundRunMode logHandle handle.sessionMeta.metaCwd)
            childOptions >>= \case
                DevQuit -> pure (Right ())
                DevReload _ ->
                    pure (Left "background agent session requested a reload")
  where
    logPath =
        unsafeToFilePath
            (handle.sessionDir </> unsafeEncodeUtf "agent.log")
    childOptions =
        applyBackgroundApproval policy $
            parentOptions
                { optProvider = Nothing
                , optModel = Nothing
                , optCwd = Nothing
                , optWorktree = False
                , optEffort = Nothing
                , optPrompt = Just message
                , optPromptFile = Nothing
                , optManagedTurnFile = Nothing
                , optResume = Just handle.sessionMeta.metaId
                , optSaveSession = True
                , optGhci = ghciEnabled
                , optBash = bashEnabled
                , optScreenMode = ScreenMinimal
                }

applyBackgroundApproval :: ApprovalPolicy -> CliOptions -> CliOptions
applyBackgroundApproval policy options =
    case policy of
        ApproveAll ->
            options
                { optYolo = True
                , optNoYolo = False
                , optManagedDenyMutations = False
                }
        DenyMutating ->
            options
                { optYolo = False
                , optNoYolo = True
                , optManagedDenyMutations = True
                }
        PromptMutating ->
            -- Background sessions cannot safely borrow the caller's stdin.
            -- Keep the prompt policy marker; non-TTY one-shot resolution
            -- conservatively denies mutating calls.
            options
                { optYolo = False
                , optNoYolo = True
                , optManagedDenyMutations = False
                }

-- | Restore the process cwd after an action succeeds or throws. Cabal gives
-- GHCi relative source paths, so returning from an agent session in its cwd
-- would make the following @:reload@ lose local modules.
withRestoredCurrentDirectory :: IO a -> IO a
withRestoredCurrentDirectory action = do
    originalCwd <- getCurrentDirectory
    action `finally` setCurrentDirectory originalCwd

finishStartup :: StartupRuntime -> IO ()
finishStartup startup = do
    writeIORef startup.startupFinished True
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
                Nothing -> putTextLn startup.startupStderr message
                Just runtime -> emitUiEvent runtime (UiSystemMessage message)
        _ -> pure ()

reportStartupWarning :: StartupRuntime -> Text -> IO ()
reportStartupWarning startup message =
    case startup.startupFullscreen of
        Nothing -> putTextLn startup.startupStderr ("warning: " <> message)
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

runAgent
    :: AgentProcessRuntime
    -> AgentRunMode
    -> FullscreenInputBuffer
    -> SessionState
    -> CliOptions
    -> Maybe ProviderTransition
    -> IO RunResult
runAgent
        processRuntime runMode fullscreenInputs sessionState options transition = do
    prepared <-
        prepareAgentIteration
            processRuntime
            runMode
            fullscreenInputs
            sessionState
            Nothing
            options
            transition
    let runPrepared = case prepared.preparedFullscreen of
            Nothing -> prepared.preparedRun
            Just runtime ->
                runFullscreen runtime $
                    runFullscreenRestartLoop
                        processRuntime
                        runMode
                        fullscreenInputs
                        sessionState
                        runtime
                        options
                        transition
                        prepared.preparedRun
    outcome <- try @_ @StartupCancelled (try @_ @StartupFailure runPrepared)
    result <- case outcome of
        Left StartupCancelled -> pure RunQuit
        Right startupOutcome ->
            either
                (\failure@(StartupFailure message) ->
                    if runMode.runInBackground
                        then throwIO failure
                        else die message)
                pure
                startupOutcome
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
    :: AgentProcessRuntime
    -> AgentRunMode
    -> FullscreenInputBuffer
    -> SessionState
    -> Maybe FullscreenRuntime
    -> CliOptions
    -> Maybe ProviderTransition
    -> IO PreparedAgent
prepareAgentIteration
        processRuntime runMode
        fullscreenInputs sessionState activeFullscreen options transition = do
    resumeLockRef <- newIORef (Nothing :: Maybe SessionLock)
    databaseStoreRef <- newIORef (Nothing :: Maybe Store)
    prepareAgentIterationTracked
        resumeLockRef
        databaseStoreRef
        processRuntime
        runMode
        fullscreenInputs
        sessionState
        activeFullscreen
        options
        transition
        `onException`
            releasePreparationResources resumeLockRef databaseStoreRef

prepareAgentIterationTracked
    :: IORef (Maybe SessionLock)
    -> IORef (Maybe Store)
    -> AgentProcessRuntime
    -> AgentRunMode
    -> FullscreenInputBuffer
    -> SessionState
    -> Maybe FullscreenRuntime
    -> CliOptions
    -> Maybe ProviderTransition
    -> IO PreparedAgent
prepareAgentIterationTracked
        resumeLockRef databaseStoreRef
        processRuntime runMode
        fullscreenInputs sessionState activeFullscreen options transition = do
    forM_ activeFullscreen resetFullscreenSessionActions
    let stdoutHandle = runMode.runStdout
        stderrHandle = runMode.runStderr
        background = runMode.runInBackground
        signalReady result =
            unless background (signalManagedSessionReady result)
        failPreparation message =
            releasePreparationResources resumeLockRef databaseStoreRef >>
                case activeFullscreen of
                    Nothing
                        | background -> throwIO (StartupFailure message)
                        | otherwise -> die message
                    Just _ -> throwIO (StartupFailure message)
    startedAt <- getCurrentTime
    startupTimingsRef <- newIORef []
    syntaxLoadDurationRef <- newIORef Nothing
    startupFinishedRef <- newIORef False
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
                    signalReady (Left err)
                    failPreparation (Text.unpack err))
                pure
                (sessionDirForId root sessionId)
            exists <- doesDirectoryExist dir
            when (not exists) do
                let err = "session not found: " <> sessionId
                signalReady (Left err)
                failPreparation (Text.unpack err)
            acquireSessionLock dir sessionId >>= \case
                Left err -> do
                    signalReady (Left err)
                    failPreparation (Text.unpack err)
                Right lock -> do
                    writeIORef resumeLockRef (Just lock)
                    loadSession sessionPool root sessionId >>= \case
                        Left err -> do
                            signalReady (Left err)
                            failPreparation (Text.unpack err)
                        Right loaded -> do
                            signalReady (Right ())
                            pure (Just loaded)

    source <- case options.optCwd of
        Just requestedCwd -> makeAbsolute requestedCwd
        Nothing -> case resumed of
            Just (meta, _) -> makeAbsolute meta.metaCwd
            Nothing ->
                maybe getCurrentDirectory makeAbsolute runMode.runCwdHint
    let initialCwd = source
    uiRuntimeRef <- newIORef Nothing
    cancelToolRef <- newIORef (pure ())
    interrupt <- newInterruptState \msg -> do
        readIORef uiRuntimeRef >>= \case
            Just runtime ->
                emitUiEvent runtime
                    (UiSetNotice (Just (warningNotice msg)))
            Nothing -> do
                -- Drop an in-place "Thinking…" status so the hint is its own line.
                Text.hPutStr stderrHandle "\r\ESC[K"
                clearNativeProgress stderrHandle
                color <- resolveColor stderrHandle
                putTextLn stderrHandle (roleMuted color msg)
    -- Shared with Esc cancel and plan prompts so arrow-key pickers own stdin.
    escPaused <- newIORef False
    stderrTty <-
        if background then pure False else hIsTerminalDevice stderrHandle
    stdinTty <- if background then pure False else hIsTerminalDevice stdin
    stdoutTty <-
        if background then pure False else hIsTerminalDevice stdoutHandle
    terminal <- detectTerminalCapabilities stdoutHandle
    useColor <- if background then pure False else resolveColor stdoutHandle
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
                    (Just (progressNotice
                        (if options.optWorktree
                            then "Creating worktree…"
                            else "Loading project…"))))
                (reduceUi
                    (UiSetRepository
                        ""
                        (toText (takeFileName (takeDirectory initialCwd))
                            <> "/"
                            <> toText (takeFileName initialCwd)))
                    (hydrateUiHistory initialTurns)))
                        { uiQueuedInputs = queuedInputDisplays }
    firstFrameReady <-
        if isJust activeFullscreen || not fullscreenEnabled
            then newMVar ()
            else newEmptyMVar
    fullscreen <- case activeFullscreen of
        Just runtime -> pure (Just runtime)
        Nothing
            | fullscreenEnabled ->
                Just <$> newFullscreenRuntime
                    fullscreenInputs
                    (readIORef cancelToolRef >>= id)
                    (\level ->
                        readIORef restartEffortActionRef >>= ($ level))
                    (noteFullscreenCtrlC interrupt)
                    (copyTerminalClipboard terminal stdoutHandle)
                    (setCliWindowTitle stdoutTty stdoutHandle)
                    (\active ->
                        when
                            (terminal.terminalNativeProgress
                                && nativeProgressAnimationEnabled
                                    options.optMotionMode) $
                            setNativeProgress stderrHandle active)
                    (readIORef agentSnapshotRef >>= id)
                    (\target -> readIORef agentSelectRef >>= ($ target))
                    (do
                        recordStartupTiming
                            startedAt startupTimingsRef "first frame"
                        void (tryPutMVar firstFrameReady ()))
                    (writeIORef syntaxLoadDurationRef . Just)
                    options.optMotionMode
                    useColor
                    initialFullscreenState
            | otherwise -> pure Nothing
    writeIORef uiRuntimeRef fullscreen
    resumeLock <- readIORef resumeLockRef
    let action =
            do
                cwd <- case resumed of
                    Just _ -> pure initialCwd
                    Nothing
                        | options.optWorktree -> do
                            readMVar firstFrameReady
                            case fullscreen of
                                Just _ -> pure ()
                                Nothing ->
                                    putTextLn stderrHandle "Creating worktree…"
                            createWorktree source (worktreeRoot home)
                                >>= either
                                    (\err -> do
                                        mapM_ releaseSessionLock resumeLock
                                        case fullscreen of
                                            Nothing -> die (Text.unpack err)
                                            Just _ ->
                                                throwIO
                                                    (StartupFailure
                                                        (Text.unpack err)))
                                    (\path -> do
                                        color <- resolveColor stderrHandle
                                        case fullscreen of
                                            Nothing ->
                                                putTextLn stderrHandle
                                                    (roleMuted color
                                                        (glyphSession
                                                            <> "worktree: "
                                                            <> toText path))
                                            Just runtime ->
                                                emitUiEvent runtime
                                                    (UiSystemMessage
                                                        (glyphSession
                                                            <> "worktree: "
                                                            <> toText path))
                                        setStartupNotice fullscreen
                                            "Loading project…"
                                        pure path)
                        | otherwise -> pure initialCwd
                unless background (setCurrentDirectory cwd)
                terminalCwd <- decodeFS cwd
                reportTerminalCwd terminal stdoutHandle terminalCwd
                toolEnv <- defaultToolEnv cwd
                writeIORef cancelToolRef (requestCancel toolEnv.toolCancel)
                forM_ fullscreen \runtime ->
                    setFullscreenSessionActions
                        runtime
                        (requestCancel toolEnv.toolCancel)
                        (const (pure ()))
                        (pure ())
                        (\level ->
                            readIORef restartEffortActionRef >>= ($ level))
                        (noteFullscreenCtrlC interrupt)
                        (readIORef agentSnapshotRef >>= id)
                        (\target -> readIORef agentSelectRef >>= ($ target))
                let startup = StartupRuntime
                        { startupToolEnv = toolEnv
                        , startupDatabaseStore = databaseStore
                        , startupInterrupt = interrupt
                        , startupEscPaused = escPaused
                        , startupUiRuntimeRef = uiRuntimeRef
                        , startupFullscreen = fullscreen
                        , startupTerminal = terminal
                        , startupStdout = stdoutHandle
                        , startupStderr = stderrHandle
                        , startupBackground = background
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
                        , startupFinished = startupFinishedRef
                        , startupSessionState = sessionState
                        }
                runAgentInitialized
                    processRuntime
                    options
                    transition
                    home
                    root
                    resumed
                    resumeLock
                    cwd
                    startup
        cleanup = do
            writeIORef uiRuntimeRef Nothing
            writeIORef cancelToolRef (pure ())
            forM_ fullscreen resetFullscreenSessionActions
            closeStore databaseStore
    pure PreparedAgent
        { preparedFullscreen = fullscreen
        , preparedRun = action `finally` cleanup
        }

releasePreparationResources
    :: IORef (Maybe SessionLock)
    -> IORef (Maybe Store)
    -> IO ()
releasePreparationResources resumeLockRef databaseStoreRef = do
    atomicModifyIORef' resumeLockRef (\current -> (Nothing, current))
        >>= mapM_ releaseSessionLock
    atomicModifyIORef' databaseStoreRef (\current -> (Nothing, current))
        >>= mapM_ closeStore

resetFullscreenSessionActions :: FullscreenRuntime -> IO ()
resetFullscreenSessionActions runtime =
    setFullscreenSessionActions
        runtime
        (pure ())
        (const (pure ()))
        (pure ())
        (const (pure ()))
        -- No session-local interrupt state is alive between providers. A
        -- transition must remain escapable even if auth probing blocks.
        (pure ForceExit)
        (pure (AgentRoot, []))
        (const (pure ()))

runFullscreenRestartLoop
    :: AgentProcessRuntime
    -> AgentRunMode
    -> FullscreenInputBuffer
    -> SessionState
    -> FullscreenRuntime
    -> CliOptions
    -> Maybe ProviderTransition
    -> IO RunResult
    -> IO RunResult
runFullscreenRestartLoop
    processRuntime
    runMode
    fullscreenInputs
    sessionState
    runtime =
        loop
  where
    loop options transition action =
        -- The notifier in 'runFullscreen' watches this whole tail-recursive
        -- chain, rather than stopping Brick after the first provider exits.
        try @_ @StartupFailure action >>= \case
            Left (StartupFailure message) ->
                recoverStartup options transition (Text.pack message)
            Right result -> case result of
                RunRestart sessionId -> do
                    let nextOptions = restartSessionOptions options sessionId
                    retryStartup nextOptions Nothing
                RunSwitchProvider next -> do
                    let nextOptions = applyProviderTransition options next
                    retryStartup nextOptions (Just next)
                RunProviderStartFailed apiError ->
                    case transition of
                        Just failed
                            | failed.transitionCause == AutomaticFallback ->
                                continueAutomaticFallback
                                    runMode.runCwdHint
                                    runMode.runStderr
                                    (Just runtime)
                                    failed
                                    apiError >>= \case
                                    Just next -> do
                                        let nextOptions =
                                                applyProviderTransition
                                                    options next
                                        retryStartup nextOptions (Just next)
                                    Nothing ->
                                        recoverProviderStart
                                            options transition apiError
                        _ ->
                            recoverProviderStart options transition apiError
                other -> pure other

    recoverProviderStart options transition apiError = do
        now <- getCurrentTime
        recoverStartup
            options
            transition
            (formatApiErrorAt now apiError)

    retryStartup options transition = do
        emitUiEvent runtime $
            UiSetNotice (Just (progressNotice "Retrying startup…"))
        try @_ @StartupFailure
            (prepareAgentIteration
                processRuntime
                runMode
                fullscreenInputs
                sessionState
                (Just runtime)
                options
                transition) >>= \case
                    Left (StartupFailure message) ->
                        recoverStartup
                            options transition (Text.pack message)
                    Right prepared ->
                        loop options transition prepared.preparedRun

    recoverStartup options transition message = do
        emitUiEvent runtime (UiSetNotice Nothing)
        requestFullscreenChoiceWithBody
            runtime
            "Couldn’t start the agent"
            message
            0
            [ ( "Retry"
              , "Try loading credentials and account usage again"
              )
            , ( "Manage"
              , "Connect, refresh, enable, or remove provider accounts"
              )
            , ("Exit", "Close the agent")
            ] >>= \case
                Just 0 ->
                    retryStartup options transition
                Just 1 -> do
                    color <- resolveColor stderr
                    withFullscreenSuspended runtime (runLoginManager color)
                    retryStartup options transition
                _ -> pure RunQuit

runAgentInitialized
    :: AgentProcessRuntime
    -> CliOptions
    -> Maybe ProviderTransition
    -> OsPath
    -> OsPath
    -> Maybe (SessionMeta, [SessionTurn])
    -> Maybe SessionLock
    -> OsPath
    -> StartupRuntime
    -> IO RunResult
runAgentInitialized
        processRuntime options transition home root resumed resumeLock cwd startup =
    runAgentInitializedWithLock
        processRuntime options transition home root resumed resumeLock cwd startup
        `onException` mapM_ releaseSessionLock resumeLock

runAgentInitializedWithLock
    :: AgentProcessRuntime
    -> CliOptions
    -> Maybe ProviderTransition
    -> OsPath
    -> OsPath
    -> Maybe (SessionMeta, [SessionTurn])
    -> Maybe SessionLock
    -> OsPath
    -> StartupRuntime
    -> IO RunResult
runAgentInitializedWithLock
        processRuntime
        options transition home root resumed resumeLock cwd startup = do
    let baseToolEnv = startup.startupToolEnv
        mcpSupervisor = processRuntime.processMcpSupervisor
        interrupt = startup.startupInterrupt
        escPaused = startup.startupEscPaused
        uiRuntimeRef = startup.startupUiRuntimeRef
        fullscreen = startup.startupFullscreen
        isTty = startup.startupStdinTty
        stdoutTty = startup.startupStdoutTty
        stdoutHandle = startup.startupStdout
        stderrHandle = startup.startupStderr
        setWindowTitle title =
            case fullscreen of
                Just runtime -> setFullscreenWindowTitle runtime title
                Nothing -> setCliWindowTitle stdoutTty stdoutHandle title
    projectRoot <- resolveProjectRoot cwd
    stateDirectory <- decodeFS (home </> unsafeEncodeUtf ".haskell-agent")
    projectRootPath <- decodeFS projectRoot
    databaseScopes <-
        deriveDatabaseScopes stateDirectory projectRootPath >>= \case
            Left err -> startupDie startup (Text.unpack err)
            Right scopes -> pure scopes
    (projectSettings, (catalogResult, branch)) <-
        concurrently
            (loadProjectSettings projectRoot)
            (concurrently
                (loadModelCatalogAt home cwd)
                (detectGitBranch cwd))
    catalog <- either
        (startupDie startup . Text.unpack)
        pure
        catalogResult
    setStartupRepository fullscreen home branch cwd
    markStartupStage startup "Loading credentials…"
    let transitionTarget = (.transitionTarget) <$> transition
        pendingTurn = transition >>= (.transitionPendingTurn)
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
    ((initialLoaded, learnAboutUserRequested), customBearerToken) <-
        case customResponses of
            Nothing -> do
                startupAuth <-
                    loadStartupAuth startup transition requestedProvider
                pure (startupAuth, Nothing)
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
                            Just value
                                | not (null value) -> pure (Text.pack value)
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
                    ( ( LoadedAuth
                            { loadedProvider = OpenRouterProvider
                            , loadedTokenProvider =
                                staticCredentialProvider ApiBilled credential
                            , loadedAccountLabel = const (pure connectionId)
                            , loadedSelectionId = Nothing
                            , loadedOpenAiPool = Nothing
                            }
                      , False
                      )
                    , if Text.null token then Nothing else Just token
                    )
    (loaded, startupAccountIds) <- case customResponses of
        Just _ -> pure (initialLoaded, Nothing)
        Nothing
            | Just active <- transition
            , Just selectionId <- active.transitionAccountSelectionId ->
                pure
                    ( initialLoaded
                    , Just
                        ( selectionId
                        , fromMaybe selectionId active.transitionAccountId
                        )
                    )
            | not
                (providerSupportsUsageAccountSelection
                    initialLoaded.loadedProvider) ->
                        pure (initialLoaded, Nothing)
            | otherwise -> do
                let provider = initialLoaded.loadedProvider
                    rememberedIds = fmap
                        (\account ->
                            ( account.projectAccountSelectionId
                            , account.projectAccountId
                            ))
                        (projectAccountFor provider projectSettings)
                selectProviderAccount
                    provider
                    Nothing
                    rememberedIds >>= \case
                        Left err ->
                            startupDie startup (Text.unpack err)
                        Right selected ->
                            loadSelectedAccountAuth
                                provider
                                selected.selectedSelectionId
                                selected.selectedAccountId
                                >>= either
                                    (startupDie startup . Text.unpack)
                                    (\selectedLoaded ->
                                        pure
                                            ( selectedLoaded
                                            , Just
                                                ( selected.selectedSelectionId
                                                , selected.selectedAccountId
                                                )
                                            ))
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
    activeAccountIdRef <-
        newIORef (maybe "" snd startupAccountIds)
    activeSelectionRef <-
        newIORef $
            maybe
                (fromMaybe "" loaded.loadedSelectionId)
                fst
                startupAccountIds
    preferredOpenAiAccountRef <-
        newIORef $
            case (loaded.loadedProvider, startupAccountIds) of
                (OpenAIProvider, Just (_, accountId))
                    | not (Text.null accountId) -> Just accountId
                _ -> Nothing
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
                                ClaudeCodeProvider -> "Claude Code"
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
    let basePlanHooks
            | startup.startupBackground =
                PlanModeHooks
                    { planConfirmEnter = \_ -> pure False
                    , planDecideExit = \_ -> pure PlanCancel
                    , planAskQuestion = \_ _ -> pure Nothing
                    }
            | otherwise =
                cliPlanHooks interrupt escPaused (resolveColor stderrHandle)
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
        claudeBypassEnabled =
            not options.optNoYolo
                && (options.optYolo || projectSettings.settingsAutoApprove)
    -- Provider transitions commit their selection separately: manual switches
    -- immediately, automatic fallbacks only after the replacement succeeds.
    when (isNothing transition) $
        saveProjectModel projectRoot
            inferredTarget { targetDialect = dialectId }
    activeSessionLock <- newIORef resumeLock
    persistSlotRef <- newIORef PersistenceDisabled
    -- Per-subagent transcripts / previous ids, shared across send_input / task.
    subagentSessions <- newIORef Map.empty
    subagentStoreRoot <- newIORef Nothing
    subagentForkSource <- newIORef (Nothing :: Maybe (IO [ResponseItem]))
    pendingNotices <- newIORef ([] :: [TurnInput])
    let maxConcurrentAgents =
            fromMaybe defaultMaxConcurrent $
                options.optMaxConcurrentAgents
                    <|> projectSettings.settingsMaxConcurrentAgents
                    <|> harnessConfig.configMaxConcurrentAgents
    registry <- newSubagentRegistry
        defaultSubagentConfig { maxConcurrent = maxConcurrentAgents }
        cwd
        (\_ _ _ _ -> pure $ Left LoopNoResponseId)
        (\_ _ -> pure ())
    rootTurnRef <- newIORef (Nothing :: Maybe RootTurnId)
    agentTypesRef <- newIORef Map.empty
    let sendToRoot message = do
            atomicModifyIORef' pendingNotices \xs ->
                (xs <> [AgentMessage message], ())
            pure (Right "queued")
        createSubagentWorktree source =
            createWorktree source (worktreeRoot home) >>= \case
                Left err -> pure (Left err)
                Right path -> pure $ Right SubagentWorktree
                    { subagentWorktreePath = path
                    , subagentWorktreeCleanup =
                        removeWorktree source path >>= \case
                            Left err -> pure (Left err)
                            Right () -> pure (Right ())
                    }
        multiCtx = Just MultiAgentContext
            { multiRegistry = registry
            , multiCwd = cwd
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
            , multiCreateWorktree = Just createSubagentWorktree
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
    promptRequest <- loadPrompt options
    let promptText = fmap (\request -> request.managedTurnText) promptRequest
    persist <-
        preparePersistence
            (trustedPool startup.startupDatabaseStore)
            startup options root
                inferredTarget { targetDialect = dialectId }
                (isNothing transition) cwd effort promptText resumed
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
                , MCP.mcpServerCwd =
                    Just $
                        maybe (unsafeToFilePath cwd) Text.unpack config.mcpCwd
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
        progressiveMcp =
            useProgressiveMcp
                harnessConfig.configMcpInitStrategy
                (isOneShot options)
    mcpStatusPhaseRef <- newIORef (Nothing :: Maybe Bool)
    let reportProgressiveMcp statuses = do
            finished <- readIORef startup.startupFinished
            unless finished do
                setStartupNotice startup.startupFullscreen
                    (formatMcpProgress statuses)
                -- A callback can race with finishStartup between the read and
                -- the UI update. Clear a late notice if startup won the race.
                readIORef startup.startupFinished >>= \nowFinished ->
                    when nowFinished $
                        forM_ startup.startupFullscreen \runtime ->
                            emitUiEvent runtime (UiSetNotice Nothing)
            let (connecting, _, _) = summarizeMcpStatuses statuses
                isConnecting = connecting > 0
            settled <-
                atomicModifyIORef' mcpStatusPhaseRef \previous ->
                    (Just isConnecting, previous == Just True && not isConnecting)
            when (settled && not (null statuses)) $
                atomicModifyIORef' pendingNotices \notices ->
                    ( notices
                        <> [ UserMessage
                                (formatMcpModelNoticeFor dialectId statuses)
                           ]
                    , ()
                    )
    mcpLease <-
        try @_ @SomeException
            (if progressiveMcp
                then
                    MCP.acquireMcpFleetProgressive
                        mcpSupervisor
                        reportProgressiveMcp
                        mcpServerConfigs
                else
                    MCP.acquireMcpFleetWithProgress
                        mcpSupervisor
                        (\names ->
                            setStartupNotice startup.startupFullscreen
                                (if null names
                                    then "Loading built-in tools…"
                                    else
                                        "Loading tools: "
                                            <> Text.intercalate ", " names
                                            <> "…"))
                        mcpServerConfigs)
            >>= \case
            Left exception ->
                startupDie startup
                    ("Failed to initialize MCP tools: " <> show exception)
            Right lease -> pure lease
    let mcpFleet = mcpLease.mcpLeaseFleet
    mapM_ (reportStartupWarning startup) mcpFleet.mcpFleetWarnings
    setStartupNotice startup.startupFullscreen "Loading built-in tools…"
    coding <-
        codingToolsForWithTypes
            dialect
            toolEnv
            (Just planHooks)
            secretHooks
            multiCtx
            agentTypesRef
            `onException`
                (MCP.releaseMcpFleetLease mcpLease >> cleanupScratch)
    let closeBeforeSession =
            coding.codingClose
                `finally`
                    (MCP.releaseMcpFleetLease mcpLease
                        `finally` cleanupScratch)
        acquireGrokExtras
            | dialectId /= GrokBuildDialect =
                pure
                    ( Nothing
                    , LspStartup
                        { lspStartupRuntime = Nothing
                        , lspStartupWarnings = []
                        }
                    )
            | otherwise =
                concurrentlyAcquire
                    (newWebFetchRuntime
                        harnessConfig.configWebFetch
                        toolEnv >>= \case
                            Left err ->
                                startupDie startup
                                    ("Failed to initialize web_fetch: "
                                        <> Text.unpack err)
                            Right runtime -> pure runtime)
                    (mapM_ closeWebFetchRuntime)
                    (newLspRuntime harnessConfig.configLsp toolEnv)
                    (mapM_ closeLspRuntime . (.lspStartupRuntime))
    (webFetchRuntime, lspStartup) <-
        acquireGrokExtras `onException` closeBeforeSession
    mapM_ (reportStartupWarning startup) lspStartup.lspStartupWarnings
    let lspRuntime = lspStartup.lspStartupRuntime
        extraTools =
            maybe [] (pure . webFetchRuntimeTool) webFetchRuntime
                <> maybe [] (pure . lspRuntimeTool) lspRuntime
        closeExtraTools =
            concurrently_
                (mapM_ closeLspRuntime lspRuntime)
                (mapM_ closeWebFetchRuntime webFetchRuntime)
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
    ghciEnabledRef <- newIORef options.optGhci
    bashEnabledRef <- newIORef options.optBash
    skillsRef <- newIORef (SkillCatalog [] [])
    skillInvocationsRef <- newIORef []
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
            , toolsLaunchTurn = \handle message -> do
                ghciEnabled <- readIORef ghciEnabledRef
                bashEnabled <- readIORef bashEnabledRef
                let action =
                        runInProcessSessionTurn
                            processRuntime
                            options
                            policy
                            ghciEnabled
                            bashEnabled
                            handle
                            message
                if isOneShot options
                    then
                        try @_ @SomeException action >>= \case
                            Left err -> pure (Left (formatException err))
                            Right (Left err) -> pure (Left err)
                            Right (Right ()) ->
                                pure
                                    (Right
                                        ("completed session "
                                            <> handle.sessionMeta.metaId))
                    else
                        launchSessionThread
                            processRuntime.processSessionThreads
                            handle.sessionMeta.metaId
                            action
            , toolsSessionStatus =
                sessionThreadStatus processRuntime.processSessionThreads
            }
        mcpTools =
            if null mcpServerConfigs
                then []
                else if dialectId == GrokBuildDialect
                    then MCP.mcpFleetGrokMetaTools mcpFleet
                    else if progressiveMcp
                        then MCP.mcpFleetMetaTools mcpFleet
                        else MCP.mcpFleetTools mcpFleet
        databaseToolsEnv =
            databaseToolsEnvForStore
                startup.startupDatabaseStore
                databaseScopes
                (readIORef persistSlotRef >>= currentSessionId)
        learnedSkillToolsEnv =
            learnedSkillToolsEnvForStore
                startup.startupDatabaseStore
                databaseScopes
                (readIORef persistSlotRef >>= reservedSessionId)
        sessionTools = agentSessionTools sessionToolsEnv
        gatewayTools = maybe [] managedGatewayTools promptRequest
        databaseAppTools = databaseTools databaseToolsEnv
        learnedSkillAppTools =
            learnedSkillTools skillInvocationsRef learnedSkillToolsEnv
        allTools =
            coding.codingAppTools
                ++ extraTools
                ++ mcpTools
                ++ sessionTools
                ++ gatewayTools
                ++ databaseAppTools
                ++ learnedSkillAppTools
        tools =
            filterGhciTools options.optGhci
                (filterBashTools options.optBash coding.codingAppTools)
                ++ extraTools
                ++ mcpTools
                ++ sessionTools
                ++ gatewayTools
                ++ databaseAppTools
                ++ learnedSkillAppTools
        planMode = coding.codingPlanMode
        -- Keep planSessionDir and subagent store root in sync.
        noteSessionDir dir = do
            writeIORef planMode.planSessionDir (Just dir)
            writeIORef subagentStoreRoot (Just dir)
        closeAgents =
            case multiCtx of
                Just ctx -> do
                    interruptActiveSubagents ctx.multiRegistry
                    flushAllSubagentSnapshots subagentStoreRoot ctx.multiRegistry
                        subagentSessions agentTypesRef
                    closeSubagentRegistry ctx.multiRegistry
                Nothing -> pure ()
        closeAll =
            closeAgents
                `finally`
                    ((readIORef activeSessionLock
                        >>= mapM_ releaseSessionLock)
                        `finally`
                            (closeExtraTools
                                `finally`
                                    (MCP.releaseMcpFleetLease mcpLease
                                        `finally`
                                            (coding.codingClose
                                                `finally`
                                                    cleanupScratch))))
    flip finally closeAll do
        case
                mcpToolCollision
                    ( coding.codingAppTools
                        ++ extraTools
                        ++ sessionTools
                        ++ gatewayTools
                        ++ databaseAppTools
                        ++ learnedSkillAppTools
                    )
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
            params = requestParams provider model instructions
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
                , subagentGhciEnabled = ghciEnabledRef
                , subagentBashEnabled = bashEnabledRef
                , subagentPolicy = policy
                , subagentPlanHooks = planHooks
                , subagentSkillRoots = toolEnv.toolSkillRoots
                , subagentParams = paramsRef
                , subagentMcpTools = mcpTools
                , subagentRegistry = registry
                , subagentSessions = subagentSessions
                , subagentStoreRoot = subagentStoreRoot
                , subagentTypes = agentTypesRef
                , subagentLegacyTarget = legacySubagentTarget
                , subagentConnection = inferredTarget.targetConnectionId
                , subagentMapModel = transportModel
                , subagentCreateWorktree = Just createSubagentWorktree
                , subagentSessionTmp = toolEnv.toolSessionTmp
                , subagentSpawnModelGuidance =
                    subscriptionSubagentModelGuidance
                        provider
                        (tokenProviderBillingMode tokenProvider)
                }
        let conversationRef = startup.startupSessionState.sessionConversation
        atomicModifyIORef' conversationRef \state ->
            ( state
                { livePreviousResponseId = initialPrevious
                , liveTranscript = initialItems
                }
            , ()
            )
        contextTokensRef <- newIORef Nothing
        writeIORef subagentForkSource (Just (readLiveTranscript conversationRef))
        let titleHint = case resumed of
                Just (meta, _) -> Just meta.metaTitle
                Nothing ->
                    fmap (\request -> sessionTitleFromPrompt request.managedTurnText)
                        promptRequest
        setWindowTitle (cliWindowTitle cwd titleHint)
        markStartupStage startup "Loading instructions…"
        startupContext <-
            loadAgentsContext
                stderrHandle
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
        let runWithInterruptHandling action
                | startup.startupBackground = action
                | otherwise =
                    withCtrlCHandler interrupt $
                        withInterruptResume
                            fullscreen progName persist RunQuit action
        runWithInterruptHandling do
                let shouldProbeAtStartup =
                        isJust fullscreen
                            && isNothing transition
                            && isNothing resumed
                            && isNothing options.optProvider
                            && isNothing options.optModel
                            && isNothing promptRequest
                    sessionRequest
                        startupUnavailable
                        sessionTokenProvider
                        sessionOpenAiPool
                        sessionSelectAccount
                        sessionCompactRunner =
                            SessionRequest
                                { catalog
                                , connectionId =
                                    inferredTarget.targetConnectionId
                                , options
                                , provider
                                , dialect
                                , policy
                                , allTools
                                , suspendGhci = coding.codingSuspendGhci
                                , grokRuntime = coding.codingGrokRuntime
                                , mcpRegistrations =
                                    mcpFleet.mcpFleetRegistrations
                                , mcpWarnings = mcpFleet.mcpFleetWarnings
                                , ghciEnabledRef
                                , bashEnabledRef
                                , toolEnv
                                , planMode
                                , startup
                                , learnAboutUserRequested
                                , databaseScopes
                                , promptRequest
                                , pendingTurn
                                , unavailableProviders
                                , startupUnavailable
                                , paramsRef
                                , conversationRef
                                , initialTurns
                                , persist
                                , projectRoot
                                , home
                                , cwd
                                , tokenProvider = sessionTokenProvider
                                , openAiPool = sessionOpenAiPool
                                , startupContext
                                , skillsRef
                                , skillInvocationsRef
                                , escPaused
                                , interrupt
                                , multiCtx
                                , rootTurnRef
                                , subagentSessions
                                , pendingNotices
                                , storeRoot = subagentStoreRoot
                                , agentTypes = agentTypesRef
                                , legacyTarget = legacySubagentTarget
                                , usageRef
                                , accountRef = activeAccountRef
                                , accountIdRef = activeAccountIdRef
                                , selectionRef = activeSelectionRef
                                , accountLabel = resolveActiveAccountLabel
                                , selectAccount = sessionSelectAccount
                                , onPersisted = claimCurrentSession
                                , compactRunner = sessionCompactRunner
                                }
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
                                httpFallbackActive <- newIORef False
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
                                            options.optShowRawReasoning
                                            wsLock
                                            httpFallbackActive
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
                                            options.optShowRawReasoning
                                            tokenProvider
                                            (pure privateParams)
                                    compactRunner focus =
                                        withMVar wsLock \_ -> do
                                            OpenAiPersistentConnection
                                                _credential
                                                _connectionHealthy
                                                activeConn <-
                                                    readIORef activeConnectionRef
                                            historyRef <-
                                                newIORef =<< readLiveTranscript
                                                    conversationRef
                                            let turnState =
                                                    codexConnTurnState activeConn
                                                runCompact =
                                                    installLiveCompactOutcome
                                                        conversationRef
                                                        (Just contextTokensRef)
                                                        (runProviderCompactWith
                                                            (Just compactSender)
                                                            recordCompactionUsage
                                                            provider
                                                            (Just tokenProvider)
                                                            paramsRef
                                                            historyRef)
                                                        focus
                                            resetCodexTurnState turnState
                                            runCompact `finally`
                                                resetCodexTurnState turnState
                                activeBackend <-
                                    prepareTransitionBackend
                                        projectRoot transition persist noticingBackend
                                withAsync switchLoop \switchWorker -> do
                                    link switchWorker
                                    runSession
                                        (sessionRequest
                                            startupUnavailable
                                            (Just tokenProvider)
                                            loaded.loadedOpenAiPool
                                            selectAccount
                                            compactRunner)
                                        SessionBackend
                                            { backend = activeBackend
                                            , btwBackend
                                            , resetBackendState = do
                                                OpenAiPersistentConnection
                                                    _credential
                                                    _connectionHealthy
                                                    activeConn <-
                                                        readIORef activeConnectionRef
                                                resetCodexTurnState
                                                    (codexConnTurnState activeConn)
                                            })
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
                                                    cwd
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
                                        (\childParams ->
                                            xaiBackend xaiOptions tokenProvider
                                                (pure childParams))
                            Nothing -> pure ()
                        let backend =
                                withPendingInputs pendingNotices $
                                    withConnectionRecovery $
                                        xaiBackend xaiOptions tokenProvider
                                            (readIORef paramsRef)
                            btwBackend privateParams =
                                xaiBackend xaiOptions tokenProvider
                                    (pure privateParams)
                            compactRunner focus = do
                                historyRef <-
                                    newIORef =<< readLiveTranscript conversationRef
                                installLiveCompactOutcome conversationRef Nothing
                                    (runProviderCompactWith
                                        Nothing
                                        recordCompactionUsage
                                        provider
                                        (Just tokenProvider)
                                        paramsRef
                                        historyRef)
                                    focus
                        activeBackend <-
                            prepareTransitionBackend
                                projectRoot transition persist backend
                        runSession
                            (sessionRequest
                                startupUnavailable
                                (Just tokenProvider)
                                loaded.loadedOpenAiPool
                                (if isJust customGenericOptions
                                    then Nothing
                                    else Just selectHttpAccount)
                                compactRunner)
                            SessionBackend
                                { backend = activeBackend
                                , btwBackend
                                , resetBackendState = pure ()
                                }
                    ClaudeCodeProvider -> do
                        claudeAuth <-
                            loadClaudeCodeAuth
                                >>= either (startupDie startup . Text.unpack) pure
                        let permission =
                                if claudeBypassEnabled
                                    then ClaudeCodeBypass
                                    else ClaudeCodeDontAsk
                            claudeOptions =
                                (defaultClaudeCodeOptions
                                    claudeAuth.executable
                                    (unsafeToFilePath cwd))
                                    { permission
                                    , safeMode = True
                                    }
                            compactRunner _ =
                                pure $ Left
                                    "Claude Code manages its own context; /compact is unavailable."
                            btwBackend privateParams =
                                Backend \state previous inputs onEvent -> do
                                    privateTranscript <- newIORef state
                                    let privateBackend =
                                            claudeCodeOneShotBackend
                                                claudeOptions
                                                    { permission =
                                                        ClaudeCodeDontAsk
                                                    }
                                                (pure privateParams)
                                                privateTranscript
                                    privateBackend.submitTurn
                                        state
                                        previous
                                        inputs
                                        onEvent
                        if claudeBypassEnabled
                            then pure ()
                            else
                                case fullscreen of
                                    Just runtime ->
                                        emitUiEvent runtime
                                            (UiSystemMessage
                                                "Claude Code is in non-blocking restricted mode; restart with --yolo to bypass Claude Code permission checks.")
                                    Nothing -> do
                                        color <- resolveColor stderrHandle
                                        putTextLn stderrHandle $
                                            roleWarn color $
                                                glyphWarn
                                                    <> "Claude Code is restricted; restart with --yolo to bypass Claude Code permission checks."
                        writeIORef activeAccountRef claudeAuth.accountLabel
                        claudeTranscriptRef <-
                            newIORef =<< readLiveTranscript conversationRef
                        withClaudeCodeBackend
                            claudeOptions
                            initialPrevious
                            (readIORef paramsRef)
                            claudeTranscriptRef
                            \backend -> do
                                activeBackend <-
                                    prepareTransitionBackend
                                        projectRoot transition persist backend
                                result <- runSession
                                    (sessionRequest
                                        startupUnavailable
                                        Nothing
                                        Nothing
                                        Nothing
                                        compactRunner)
                                    SessionBackend
                                        { backend = activeBackend
                                        , btwBackend
                                        , resetBackendState =
                                            writeIORef claudeTranscriptRef []
                                        }
                                writeLiveTranscript conversationRef
                                    =<< readIORef claudeTranscriptRef
                                pure result
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
                                        (\childParams ->
                                            makeBackend
                                                (pure childParams))
                            Nothing -> pure ()
                        let backend =
                                withPendingInputs pendingNotices $
                                    withConnectionRecovery $
                                        makeBackend
                                            (readIORef paramsRef)
                            btwBackend privateParams =
                                makeBackend
                                    (pure privateParams)
                            compactRunner focus = do
                                historyRef <-
                                    newIORef =<< readLiveTranscript conversationRef
                                installLiveCompactOutcome conversationRef Nothing
                                    (case customGenericOptions of
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
                                                historyRef
                                        Nothing ->
                                            runProviderCompactWith
                                                Nothing
                                                recordCompactionUsage
                                                provider
                                                (Just tokenProvider)
                                                paramsRef
                                                historyRef)
                                    focus
                        activeBackend <-
                            prepareTransitionBackend
                                projectRoot transition persist backend
                        runSession
                            (sessionRequest
                                startupUnavailable
                                (Just tokenProvider)
                                loaded.loadedOpenAiPool
                                (Just selectHttpAccount)
                                compactRunner)
                            SessionBackend
                                { backend = activeBackend
                                , btwBackend
                                , resetBackendState = pure ()
                                }
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
                previousAccountId <- readIORef accountIdRef
                writeIORef accountIdRef credential.accountId
                when (previousAccountId /= credential.accountId) $
                    writeIORef selectionRef credential.accountId
                resolveLabel credential >>= writeIORef accountRef
                pure (Right credential)

-- | On Ctrl-C, print a copy-pasteable --resume line when a session exists.
withInterruptResume
    :: Maybe FullscreenRuntime
    -> String
    -> Persistence
    -> a
    -> IO a
    -> IO a
withInterruptResume fullscreen progName persist interrupted action =
    catchUserInterrupt action finishInterrupt
  where
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

sessionRunnerContinuation :: SessionRunner.SessionRunnerContinuation
sessionRunnerContinuation =
    SessionRunner.SessionRunnerContinuation
        { runnerRepl = repl
        , runnerReplWithDraft = replWithDraft
        , runnerRunPendingTurn = runPendingTurn
        , runnerFinishTurn = finishTurn
        , runnerFinishStartup = finishStartup
        , runnerPreparePromptSkillInputs = preparePromptSkillInputs
        , runnerRunSessionRecap = runSessionRecap
        , runnerRunSessionTurnSummary = runSessionTurnSummary
        }
runSession
    :: SessionRequest
    -> SessionBackend
    -> IO RunResult
runSession = SessionRunner.runSession sessionRunnerContinuation

sessionContinuation :: SessionContinuation
sessionContinuation =
    SessionContinuation
        { resumeSession = repl
        , resumeSessionWithDraft = replWithDraft
        }

runPendingTurn
    :: PendingTurnPresentation
    -> SessionEnv
    -> PendingTurn
    -> IO RunResult
runPendingTurn = SessionLifecycle.runPendingTurn sessionContinuation

finishTurn
    :: SessionEnv
    -> Bool
    -> TurnResult
    -> IO RunResult
finishTurn = SessionLifecycle.finishTurn sessionContinuation

repl :: SessionEnv -> IO RunResult
repl env = replWithDraft env ""

replWithDraft :: SessionEnv -> Text -> IO RunResult
replWithDraft env@SessionEnv
    { sessionCompact = compactRunner
    , sessionRender = render
    , sessionConversation = conversationRef
    , sessionProvider = provider
    , sessionConnection = connectionId
    , sessionModelCatalog = catalog
    , sessionDialect = dialect
    , sessionStartupUnavailable = startupUnavailableRef
    , sessionParams = paramsRef
    , sessionPolicy = policyRef
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
    , sessionActiveToolNames = readActiveToolNames
    , sessionGrokRuntime = grokRuntime
    , sessionDraft = draftRef
    , sessionPreviewId = previewIdRef
    , sessionInterrupt = interrupt
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
    , sessionConcurrentLimit = _
    , sessionSetConcurrentLimit = _
    , sessionReset = sessionReset
    } draft = do
    writeIORef draftRef draft
    refreshSkills False
    skillInvocations <- readIORef skillInvocationsRef
    let skillCommands =
            map skillInvocationCommand
                (filter (.invocationSkill.skillUserInvocable) skillInvocations)
    activeToolNames <- readActiveToolNames
    let slashCatalog =
            mkSlashCatalog
                (dialectId dialect)
                activeToolNames
                skillCommands
                (catalogModelIds catalog)
    stdoutColor <- resolveColor stdout
    planState <- readIORef planMode.planStateRef
    let planActive = planState == PlanActive
        planPending = planState == PlanPending
    params <- readIORef paramsRef
    policy <- readIORef policyRef
    pendingAttachments <- readLiveAttachments conversationRef
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
                readPrompt =
                    readIORef startupUnavailableRef >>= \case
                        Nothing ->
                            Right
                                <$> readFullscreenLineWithCatalog
                                    runtime
                                    slashCatalog
                                    promptState
                                    draft
                        Just unavailable ->
                            readFullscreenLineOrWithCatalog
                                runtime
                                slashCatalog
                                promptState
                                draft
                                unavailable
            withAsync (refreshAccountLimit runtime) \_ ->
                readPrompt
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
            result <- readReplLineWithCatalog
                slashCatalog
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
                slashCatalog
                skillInvocations
                stdoutColor
                planState
                policy
                mline
  where
    refreshAccountLimit runtime =
        case (provider, tokenProvider) of
            (XAIProvider, Just tokens)
                | tokenProviderBillingMode tokens == SubscriptionBilled ->
                    refreshWith
                        tokens
                        XAIUsage.fetchGrokUsage
                        formatGrokLimitStatus
            (OpenAIProvider, Just tokens)
                | tokenProviderBillingMode tokens == SubscriptionBilled ->
                    getNextToken tokens Nothing >>= \case
                        Left _ -> pure ()
                        Right credential
                            | Text.null (Text.strip credential.accountId) ->
                                pure ()
                            | otherwise ->
                                fetchUsage
                                    credential.accessToken
                                    credential.accountId >>= \case
                                        Left _ -> pure ()
                                        Right snapshot ->
                                            publish
                                                (formatOpenAiLimitStatus snapshot)
            (OpenRouterProvider, Just tokens) ->
                getNextToken tokens Nothing >>= \case
                    Left _ -> pure ()
                    Right credential ->
                        OpenRouterUsage.fetchOpenRouterUsage
                            credential.accessToken >>= \case
                                Left _ -> pure ()
                                Right snapshot ->
                                    publish
                                        (formatOpenRouterLimitStatus snapshot)
            _ -> pure ()
      where
        refreshWith tokens fetch formatStatus =
            getNextToken tokens Nothing >>= \case
                Left _ -> pure ()
                Right credential ->
                    fetch credential >>= \case
                        Left _ -> pure ()
                        Right snapshot -> publish (formatStatus snapshot)
        publish limitStatus =
            forM_
                limitStatus
                (emitUiEvent runtime . UiSetPromptLimitStatus . Just)

    handleReplLine
            slashCatalog skillInvocations
            stdoutColor planState policy = \case
        ReplEof -> do
            when (isNothing fullscreen) $
                putStrLn ""
            pure RunQuit
        ReplQuitInterrupt ->
            -- Confirmed double Ctrl-C: rethrow so withInterruptResume prints
            -- the --resume hint and the process exits.
            throwIO UserInterrupt
        ReplCycleMode keptDraft
            | provider == ClaudeCodeProvider -> do
                let message =
                        "Claude Code permissions are fixed when the provider starts; restart with --yolo or --no-yolo to change them."
                color <- resolveColor stderr
                displayInfo message $
                    putTextLn stderr (roleMuted color message)
                continueWith keptDraft
            | otherwise -> do
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
                        conversationRef
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
                        conversationRef
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
        ReplClipboardPasteOrText keptDraft pasted pastedDraft -> do
            pastedImages <- loadImagesFromPastedText pasted
            imagesResult <- case pastedImages of
                Just images@(_:_) -> pure (Just images)
                _ ->
                    nonEmptyClipboardImages
                        <$> readClipboardImagesImageFirst
            case imagesResult of
                Just images -> do
                    message <- queueAttachedImages
                        conversationRef
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
                _ -> do
                    fullscreenEvent (UiSetNotice Nothing)
                    continueWith pastedDraft
        ReplChooseModel keptDraft -> do
            writeIORef draftRef keptDraft
            chooseModel (continueWith keptDraft)
        ReplChooseEffort keptDraft ->
            chooseEffort (continueWith keptDraft)
        ReplChooseAccount keptDraft -> do
            writeIORef draftRef keptDraft
            chooseAccount (continueWith keptDraft)
        ReplPasted pasted ->
            submitLine slashCatalog skillInvocations
                continue stdoutColor True pasted
        ReplText line ->
            submitLine slashCatalog skillInvocations
                continue stdoutColor False line
    submitLine
            slashCatalog skillInvocations
            continue color pasted line = do
        attachmentCount <- length <$> readLiveAttachments conversationRef
        case submissionPromptText attachmentCount line of
            Nothing -> continue
            Just promptLine -> do
                let stripped = Text.strip promptLine
                when pasted do
                    let chip = formatPasteChip stripped
                    when (chip /= stripped && isNothing fullscreen) do
                        Text.putStrLn (roleMuted color chip)
                case parseReplLineWithCatalog slashCatalog promptLine of
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
                                    conversationRef
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
                                pendingImages <- modifyLiveAttachments conversationRef \imgs -> ([], imgs)
                                forM_ fullscreen \runtime ->
                                    setFullscreenImagePreviews runtime []
                                resetRenderPrintedText render
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
                    ReplExpandedPrompt original expanded ->
                        submitExpandedTurn
                            continue color original expanded
                    ReplInvokeSkill invocationName arguments ->
                        case resolveSkillInvocation
                            skillInvocations invocationName of
                            Left err -> do
                                displayError err $
                                    Text.hPutStrLn stderr
                                        (roleError color err)
                                continue
                            Right invocation -> do
                                pendingImages <-
                                    modifyLiveAttachments conversationRef
                                        \imgs -> ([], imgs)
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
                                resetRenderPrintedText render
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
                    ReplShowShell -> do
                        mode <- env.sessionShellMode
                        let message = "shell tools: " <> case mode of
                                ShellGhci -> "ghci"
                                ShellBash -> "bash"
                                ShellBoth -> "ghci + bash"
                                ShellNone -> "none"
                        displayInfo message $
                            Text.putStrLn
                                (roleMuted color (glyphSession <> message))
                        continue
                    ReplSetShell mode -> do
                        message <- env.sessionSetShellMode mode
                        displayInfo message $
                            Text.putStrLn
                                (roleMuted color (glyphOk <> message))
                        continue
                    ReplPaste pasteImmediate pasteCaption -> do
                        color <- resolveColor stdout
                        errColor <- resolveColor stderr
                        imagesResult <- readClipboardImagesForPaste
                        case imagesResult of
                            Left err -> do
                                displayError err $
                                    Text.hPutStrLn stderr
                                        (roleError errColor err)
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
                                        resetRenderPrintedText render
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
                                            conversationRef
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
                        pending <- readLiveAttachments conversationRef
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
                        modifyLiveAttachments conversationRef (\_ -> ([], ()))
                        forM_ fullscreen \runtime ->
                            setFullscreenImagePreviews runtime []
                        color <- resolveColor stdout
                        displayInfo "attachments cleared" $
                            Text.putStrLn
                                (roleMuted color
                                    (glyphOk <> "attachments cleared"))
                        continue
                    ReplShowAgentLimit -> do
                        limit <- env.sessionConcurrentLimit
                        let message =
                                "concurrent agent limit: "
                                    <> Text.pack (show limit)
                        color <- resolveColor stdout
                        displayInfo message $
                            Text.putStrLn
                                (roleMuted color (glyphSession <> message))
                        continue
                    ReplSetAgentLimit limit -> do
                        message <- env.sessionSetConcurrentLimit limit
                        color <- resolveColor stdout
                        displayInfo message $
                            Text.putStrLn
                                (roleMuted color (glyphOk <> message))
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
                    ReplMcp -> do
                        color <- resolveColor stderr
                        restart <-
                            legacy $
                                runMcpManager
                                    color
                                    env.sessionHome
                                    env.sessionMcpRegistrations
                                    env.sessionMcpWarnings
                        if restart
                            then requestMcpRestart
                                fullscreen persist
                            else continue
                    ReplGoalStatus -> do
                        color <- resolveColor stdout
                        case grokRuntime of
                            Nothing ->
                                displayError
                                    "goal commands are unavailable in this session" $
                                    Text.hPutStrLn stderr
                                        (roleError color
                                            "goal commands are unavailable in this session")
                            Just control ->
                                readGoal control.grokGoalRuntime >>= \case
                                    Nothing ->
                                        displayInfo "No goal is active." $
                                            Text.putStrLn
                                                (roleMuted color
                                                    "No goal is active.")
                                    Just goal -> do
                                        let message =
                                                formatGoalSnapshot goal
                                        displayInfo message $
                                            Text.putStrLn
                                                (roleMuted color message)
                        continue
                    ReplGoalPause -> do
                        color <- resolveColor stderr
                        case grokRuntime of
                            Nothing ->
                                displayError
                                    "goal commands are unavailable in this session" $
                                    Text.hPutStrLn stderr
                                        (roleError color
                                            "goal commands are unavailable in this session")
                            Just control ->
                                pauseGoal control.grokGoalRuntime >>= \case
                                    Left err ->
                                        displayError err $
                                            Text.hPutStrLn stderr
                                                (roleError color err)
                                    Right goal -> do
                                        let message =
                                                "Goal paused.\n"
                                                    <> formatGoalSnapshot goal
                                        displayInfo message $
                                            Text.hPutStrLn stderr
                                                (roleMuted color message)
                        continue
                    ReplGoalResume -> do
                        color <- resolveColor stderr
                        case grokRuntime of
                            Nothing ->
                                displayError
                                    "goal commands are unavailable in this session" $
                                    Text.hPutStrLn stderr
                                        (roleError color
                                            "goal commands are unavailable in this session")
                            Just control ->
                                resumeGoal control.grokGoalRuntime >>= \case
                                    Left err ->
                                        displayError err $
                                            Text.hPutStrLn stderr
                                                (roleError color err)
                                    Right goal -> do
                                        let message =
                                                "Goal resumed.\n"
                                                    <> formatGoalSnapshot goal
                                        displayInfo message $
                                            Text.hPutStrLn stderr
                                                (roleMuted color message)
                        continue
                    ReplGoalClear -> do
                        color <- resolveColor stderr
                        case grokRuntime of
                            Nothing ->
                                displayError
                                    "goal commands are unavailable in this session" $
                                    Text.hPutStrLn stderr
                                        (roleError color
                                            "goal commands are unavailable in this session")
                            Just control -> do
                                cleared <-
                                    clearGoal control.grokGoalRuntime
                                let message =
                                        if cleared
                                            then "Goal cleared."
                                            else "No goal was active."
                                displayInfo message $
                                    Text.hPutStrLn stderr
                                        (roleMuted color message)
                        continue
                    ReplGoalSet original objective budget expanded ->
                        case grokRuntime of
                            Nothing -> do
                                color <- resolveColor stderr
                                let err =
                                        "goal commands are unavailable in this session"
                                displayError err $
                                    Text.hPutStrLn stderr
                                        (roleError color err)
                                continue
                            Just control ->
                                activateGoal
                                    control.grokGoalRuntime
                                    objective
                                    budget >>= \case
                                        Left err -> do
                                            color <- resolveColor stderr
                                            displayError err $
                                                Text.hPutStrLn stderr
                                                    (roleError color err)
                                            continue
                                        Right _ ->
                                            submitExpandedTurn
                                                continue
                                                color
                                                original
                                                expanded
                    ReplWorkflowRuns -> do
                        color <- resolveColor stdout
                        case grokRuntime >>= (.grokWorkflowRuntime) of
                            Nothing ->
                                displayError
                                    "workflow commands are unavailable in this session" $
                                    Text.hPutStrLn stderr
                                        (roleError color
                                            "workflow commands are unavailable in this session")
                            Just runtime -> do
                                runs <- workflowRunSnapshots runtime
                                let message = formatWorkflowRuns runs
                                displayInfo message $
                                    Text.putStrLn
                                        (roleMuted color message)
                        continue
                    ReplWorkflowManage operation target -> do
                        color <- resolveColor stderr
                        let err =
                                "workflow_management_unsupported: /workflow "
                                    <> operation
                                    <> maybe "" (" " <>) target
                                    <> " is not supported by this host; use /workflow runs to inspect tracked runs."
                        displayError err $
                            Text.hPutStrLn stderr
                                (roleError color err)
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
                        chooseModel continue
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
                                    fullscreen choice persist >>= \case
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
                                    paramsRef render conversationRef persist
                                displayInfo message $
                                    Text.putStrLn
                                        (roleMuted color (glyphOk <> message))
                                continue
                    ReplToggleAlwaysApprove
                        | provider == ClaudeCodeProvider -> do
                            let message =
                                    "Claude Code permissions are fixed for this provider session; restart with --yolo or --no-yolo."
                            color <- resolveColor stderr
                            displayInfo message $
                                putTextLn stderr (roleMuted color message)
                            continue
                        | otherwise -> do
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
                    ReplPlan _
                        | provider == ClaudeCodeProvider -> do
                            let message =
                                    "Outer plan mode is unavailable for Claude Code because its tools run inside the Claude CLI."
                            color <- resolveColor stderr
                            displayInfo message $
                                putTextLn stderr (roleMuted color message)
                            continue
                    ReplPlan maybeDescription ->
                        enterPlanFromSlash env maybeDescription >>= \case
                            Just providerSwitch ->
                                pure (RunSwitchProvider providerSwitch)
                            Nothing -> continue
                    ReplBtw question -> do
                        runBtwQuestion True env question
                        continue
                    ReplRecap ->
                        case fullscreen of
                            Just runtime -> do
                                emitUiEvent runtime UiRecapStarted
                                env.sessionQueueRecap (RecapSession RecapManual)
                                continue
                            Nothing -> do
                                runSessionRecap True env RecapManual
                                continue
                    ReplResume maybeId -> do
                        handleResume databasePool fullscreen maybeId persist >>= \case
                            Nothing -> continue
                            Just result -> pure result
                    ReplSearch query -> do
                        handleConversationSearch
                            databasePool fullscreen query persist >>= \case
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
                                                , metaLastRecap = Nothing
                                                , metaLastTurnSummary = Nothing
                                                , metaLastRecapMainTurns = 0
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
                                now <- getCurrentTime
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
                                env.sessionOnPersisted handle'
                                env.sessionSetTempDir handle'.sessionTempDir
                                writeIORef slotRef
                                    (PersistenceActive handle')
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
                    ReplShowSessionInfo -> do
                        color <- resolveColor stdout
                        params <- readIORef paramsRef
                        usage <- readIORef usageRef
                        shellMode <- env.sessionShellMode
                        (persistenceState, sessionId, sessionTitle) <-
                            case persist of
                                PersistenceDisabled ->
                                    pure ("not_persisted", Nothing, Nothing)
                                PersistenceEnabled slotRef -> do
                                    slot <- readIORef slotRef
                                    pure $ case slot of
                                        PersistencePending _ pendingId _ ->
                                            ("pending", Just pendingId, Nothing)
                                        PersistenceActive handle ->
                                            ( "active"
                                            , Just handle.sessionMeta.metaId
                                            , Just handle.sessionMeta.metaTitle
                                            )
                        let toolNames =
                                Set.toAscList
                                    slashCatalog.slashCatalogToolNames
                            usageText =
                                let formatted = formatTokenUsage usage
                                in if Text.null formatted
                                    then "0 in · 0 out"
                                    else formatted
                            message = Text.unlines $
                                [ "session: "
                                    <> fromMaybe "(not persisted)" sessionId
                                , "state: " <> persistenceState
                                ]
                                    <> maybe
                                        []
                                        (\title -> ["title: " <> title])
                                        sessionTitle
                                    <> [ "provider: " <> providerSlug provider
                                       , "connection: " <> connectionId
                                       , "model: " <> currentModel params
                                       , "dialect: "
                                            <> dialectSlug
                                                (dialectId dialect)
                                       , "effort: " <> currentEffort params
                                       , "cwd: " <> toText cwd
                                       , "shell: "
                                            <> shellModeText shellMode
                                       , "tokens: " <> usageText
                                       , "tools: "
                                            <> if null toolNames
                                                then "(none)"
                                                else
                                                    Text.intercalate
                                                        ", "
                                                        toolNames
                                       ]
                        displayInfo message $
                            Text.putStrLn (roleMuted color message)
                        continue
                    ReplAfk rawTarget -> do
                        let failAfk err = do
                                color <- resolveColor stderr
                                displayError err $
                                    putTextLn stderr (roleError color err)
                                continue
                            finishAfk message = do
                                color <- resolveColor stderr
                                displayInfo message $
                                    putTextLn stderr
                                        (roleMuted color (glyphOk <> message))
                                pure RunQuit
                        case parseAfkTarget rawTarget of
                            Left err -> failAfk err
                            Right target -> case persist of
                                PersistenceDisabled ->
                                    failAfk "/afk requires a persisted interactive session"
                                PersistenceEnabled slotRef ->
                                    readIORef slotRef >>= \case
                                        PersistencePending _ _ _ ->
                                            failAfk
                                                "/afk is available after the first persisted turn"
                                        PersistenceActive handle ->
                                            case target of
                                                AfkLocal ->
                                                    handoffLocal
                                                        handle.sessionMeta.metaId
                                                        cwd >>= \case
                                                            Left err -> failAfk err
                                                            Right message ->
                                                                finishAfk message
                                                AfkRemote host path ->
                                                    loadSession
                                                        databasePool
                                                        (sessionsRoot env.sessionHome)
                                                        handle.sessionMeta.metaId
                                                        >>= \case
                                                            Left err -> failAfk err
                                                            Right (meta, turns) ->
                                                                handoffRemote
                                                                    host
                                                                    path
                                                                    handle.sessionDir
                                                                    SessionTransfer
                                                                        { transferMeta = meta
                                                                        , transferTurns = turns
                                                                        }
                                                                    >>= \case
                                                                        Left err -> failAfk err
                                                                        Right message ->
                                                                            finishAfk message
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
                            (formatSlashHelpWithCatalog
                                False slashCatalog maybeName) $
                            Text.putStrLn
                                (formatSlashHelpWithCatalog
                                    color slashCatalog maybeName)
                        continue
                    ReplCommandError err -> do
                        color <- resolveColor stderr
                        displayError err $
                            Text.hPutStrLn stderr (roleError color err)
                        continue
    submitExpandedTurn next color original expanded = do
        pendingImages <-
            modifyLiveAttachments conversationRef \imgs -> ([], imgs)
        forM_ fullscreen \runtime ->
            setFullscreenImagePreviews runtime []
        let turnInputs =
                if null pendingImages
                    then [UserMessage expanded]
                    else
                        [ UserMultimodal
                            { userText = expanded
                            , userImages = pendingImages
                            }
                        ]
        preparePromptSkillInputs env original turnInputs >>= \case
            Left err -> do
                displayError err $
                    Text.hPutStrLn stderr (roleError color err)
                next
            Right skillInputs -> do
                resetRenderPrintedText render
                fullscreenEvent (UiUserSubmitted original)
                result <- runOneTurn env original skillInputs
                finishTurn env False result
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
            readLiveAttachments conversationRef
                >>= setFullscreenImagePreviews runtime
    displayInfo message minimalAction = case fullscreen of
        Nothing -> minimalAction
        Just runtime -> emitUiEvent runtime (UiSystemMessage message)
    displayError message minimalAction = case fullscreen of
        Nothing -> minimalAction
        Just runtime -> emitUiEvent runtime (UiErrorMessage message)
    shellModeText = \case
        ShellGhci -> "ghci"
        ShellBash -> "bash"
        ShellBoth -> "ghci + bash"
        ShellNone -> "none"
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
    chooseModel next = do
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
                        paramsRef render conversationRef persist
                    displayInfo message $
                        Text.putStrLn
                            (roleMuted color
                                (glyphOk <> message))
                    next
                  else
                    requestModelTargetSwitch fullscreen choice persist >>= \case
                        Left err -> do
                            displayError err $
                                Text.hPutStrLn stderr
                                    (roleError color err)
                            next
                        Right result -> pure result
    chooseAccount next =
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
                                            -- Claude exposes display metadata,
                                            -- not a stable account identity.
                                            -- Revalidate and restart even when
                                            -- the synthetic id still matches.
                                            | selectedProvider == provider
                                            , selectedProvider
                                                /= ClaudeCodeProvider
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
                                    catalog fullscreen provider connectionId
                                    (currentModel params) (dialectId dialect)
                                    selectedProvider selectedSelectionId
                                    selectedAccountId persist >>= \case
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

concurrentlyAcquire
    :: IO a
    -> (a -> IO ())
    -> IO b
    -> (b -> IO ())
    -> IO (a, b)
concurrentlyAcquire acquireLeft releaseLeft acquireRight releaseRight =
    mask \restore ->
        withAsync (restore acquireLeft) \leftWorker ->
            withAsync (restore acquireRight) \rightWorker -> do
                let cleanupResult release = \case
                        Left _ -> pure ()
                        Right value -> release value
                    cancelAndCleanup = do
                        cancel leftWorker
                        cancel rightWorker
                        leftResult <- waitCatch leftWorker
                        rightResult <- waitCatch rightWorker
                        cleanupResult releaseLeft leftResult
                        cleanupResult releaseRight rightResult
                first <-
                    restore (waitEitherCatch leftWorker rightWorker)
                        `onException` cancelAndCleanup
                case first of
                    Left (Left exception) -> do
                        cancel rightWorker
                        waitCatch rightWorker >>= cleanupResult releaseRight
                        throwIO exception
                    Right (Left exception) -> do
                        cancel leftWorker
                        waitCatch leftWorker >>= cleanupResult releaseLeft
                        throwIO exception
                    Left (Right leftValue) -> do
                        rightResult <-
                            restore (waitCatch rightWorker)
                                `onException` releaseLeft leftValue
                        case rightResult of
                            Left exception -> do
                                releaseLeft leftValue
                                throwIO exception
                            Right rightValue ->
                                pure (leftValue, rightValue)
                    Right (Right rightValue) -> do
                        leftResult <-
                            restore (waitCatch leftWorker)
                                `onException` releaseRight rightValue
                        case leftResult of
                            Left exception -> do
                                releaseRight rightValue
                                throwIO exception
                            Right leftValue ->
                                pure (leftValue, rightValue)

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

requestMcpRestart
    :: Maybe FullscreenRuntime
    -> Persistence
    -> IO RunResult
requestMcpRestart fullscreen persist = do
    color <- resolveColor stderr
    let report message =
            case fullscreen of
                Nothing ->
                    putTextLn stderr
                        (roleMuted color (glyphSession <> message))
                Just runtime ->
                    emitUiEvent runtime (UiSystemMessage message)
    case persist of
        PersistenceDisabled -> do
            report
                "MCP configuration saved; restart the agent to apply it"
            pure RunQuit
        PersistenceEnabled slotRef -> do
            handle <- ensureSession slotRef
            report "restarting MCP servers…"
            pure (RunRestart handle.sessionMeta.metaId)

restartSessionOptions :: CliOptions -> Text -> CliOptions
restartSessionOptions options sessionId =
    options
        { optProvider = Nothing
        , optModel = Nothing
        , optCwd = Nothing
        , optWorktree = False
        , optEffort = Nothing
        , optPrompt = Nothing
        , optPromptFile = Nothing
        , optManagedTurnFile = Nothing
        , optResume = Just sessionId
        }

enterPlanFromSlash :: SessionEnv -> Maybe Text -> IO (Maybe ProviderTransition)
enterPlanFromSlash env@SessionEnv
    { sessionPlanMode = planMode
    , sessionPersist = persist
    , sessionRender = render
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
            resetRenderPrintedText render
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
                        putTrailingNewline render
                    pure Nothing

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

putTrailingNewline :: RenderConfig -> IO ()
putTrailingNewline render = do
    didPrint <- renderPrintedText render
    when didPrint (putTextLn render.renderStdout "")
