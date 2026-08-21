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
    , formatTokenUsage
    , run
    , withRestoredCurrentDirectory
    ) where

import Agent.CLI.Artifact (fencedCodeBlock, lastDiffBlock)
import Agent.CLI.Auth (LoadedAuth(..), loadAuth, probeLoadedAuth)
import Agent.CLI.AgentViewport
    ( AgentEntry(..)
    , AgentTarget(..)
    , AgentViewportEnv(..)
    , formatAgentStatus
    , pickAgentViewport
    , renderAgentViewportPanelFor
    , responseItemLines
    )
import Agent.CLI.SessionTitle
    ( invalidateSessionTitles
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
    , autoCompactOpenAiBackend
    , runProviderCompact
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
    )
import Agent.CLI.ReplMode
    ( ReplMode(..)
    , replModeFromState
    )
import Agent.CLI.Interrupt
    ( InterruptState
    , isWrappedUserInterrupt
    , newInterruptState
    , noteFullscreenCtrlC
    , withCtrlCHandler
    , withTurnCancel
    )
import Agent.CLI.Login (runLoginManager)
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
import Agent.CLI.Resume (pickResumeSession)
import Agent.CLI.Plan (cliPlanHooks)
import Agent.CLI.Progress
    ( osc9ProgressIndeterminate
    , osc9ProgressRemove
    , wrapOscForTmux
    )
import Agent.CLI.Project
    ( ProjectSettings(..)
    , loadProjectSettings
    , resolveProjectRoot
    )
import Agent.CLI.Prompt (defaultModelFor, systemPrompt)
import Agent.CLI.Request (requestParams)
import Agent.CLI.ProviderFallback
    ( automaticCooldownRetryDelay
    , fallbackCandidates
    )
import Agent.CLI.ProviderTransition
    ( PendingTurn(..)
    , ProviderTransition(..)
    , TransitionCause(..)
    , TurnResult(..)
    , applyProviderTransition
    , setPendingExitAfter
    )
import Agent.CLI.Render
    ( RenderConfig(..)
    , emptyMarkdownStreamState
    , putTextLn
    , renderAssistantText
    , renderEvent
    )
import Agent.CLI.Session
import Agent.CLI.SessionEnv (SessionEnv(..))
import Agent.CLI.Skills
    ( formatSkillsListing
    , loadSkillsCatalog
    , queueSkillCatalogContext
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
    ( FullscreenRuntime
    , emitUiEvent
    , newFullscreenRuntime
    , readFullscreenLine
    , requestFullscreenPermission
    , requestFullscreenChoice
    , requestFullscreenChoiceWithBody
    , requestFullscreenText
    , runFullscreen
    , withFullscreenSuspended
    )
import Agent.CLI.UI.Model
    ( PromptState(..)
    , UiEvent(..)
    , UiState
    , initialUiState
    , reduceUi
    )
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
    , buildSkillInvocations
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
import Agent.OpenAI.LoopBackend (openAiBackendReconnecting)
import Agent.Responses.Types
import Agent.OpenAI.Usage (fetchUsage)
import Agent.OpenAI.WebSocketClient
    ( CodexAuthFailed(..)
    , CodexConn
    , withCodexWsWithProvider
    )
import Agent.Provider
    ( AccountFailure(..)
    , Credential(..)
    , FailedCredential(..)
    , Provider(..)
    , TokenProvider
    , getNextToken
    , providerSlug
    )
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
import Agent.OsPath (OsPath, fromFilePath, fromText, toFilePath, toText)
import Agent.XAI.LoopBackend (xaiBackend)
import qualified Agent.XAI.Options as XAI
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (AsyncException(UserInterrupt))
import Control.Exception.Safe
    ( SomeException
    , catchAny
    , catchAsync
    , finally
    , throwIO
    , try
    )
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.IORef
import Data.List (elemIndex, findIndex)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Set as Set
import Data.Time.Clock
    ( NominalDiffTime
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
import System.OsPath ((</>), takeDirectory, takeFileName)
import System.Console.ANSI (getTerminalSize)
import System.Console.ANSI.Codes (clearFromCursorToLineEndCode)
import System.Exit (ExitCode(..), die, exitFailure)
import System.IO (Handle, hFlush, hIsTerminalDevice, stderr, stdin, stdout)
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
runAgentWithRestarts options =
    withRestoredCurrentDirectory (go options Nothing)
  where
    go current transition =
        runAgent current transition >>= \case
            RunResumeSession sessionId ->
                go
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
                go (applyProviderTransition current next) (Just next)
            RunProviderStartFailed apiError ->
                case transition of
                    Just failed
                        | failed.transitionCause == AutomaticFallback ->
                            continueAutomaticFallback failed apiError >>= \case
                                Just next ->
                                    go (applyProviderTransition current next) (Just next)
                                Nothing -> do
                                    reportProviderUnavailable apiError
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

runAgent :: CliOptions -> Maybe ProviderTransition -> IO RunResult
runAgent options transition = do
    home <- getHomeDirectory
    let root = sessionsRoot home
    resumed <- case options.optResume of
        Nothing -> pure Nothing
        Just sessionId ->
            loadSession root sessionId >>= \case
                Left err -> die (Text.unpack err)
                Right loaded -> pure (Just loaded)

    source <- maybe getCurrentDirectory makeAbsolute options.optCwd
    cwd <- case resumed of
        Just (meta, _)
            | isJustCwd options -> pure source
            | otherwise -> makeAbsolute meta.metaCwd
        Nothing
            | options.optWorktree -> do
                createWorktree source (worktreeRoot home) >>= either (die . Text.unpack) \path -> do
                    color <- resolveColor stderr
                    putTextLn stderr (roleMuted color (glyphSession <> "worktree: " <> toText path))
                    pure path
            | otherwise -> pure source
    setCurrentDirectory cwd

    projectRoot <- resolveProjectRoot cwd
    projectSettings <- loadProjectSettings projectRoot
    isTty <- hIsTerminalDevice stdin
    stdoutTty <- hIsTerminalDevice stdout
    let transitionTarget = (.transitionTarget) <$> transition
        pendingTurn = transition >>= (.transitionPendingTurn)
        unavailableProviders =
            maybe [] (.transitionUnavailableProviders) transition
        requestedProvider = case transitionTarget of
            Just target -> Just target.modelProvider
            Nothing -> case resumed of
                Just (meta, _) -> Just meta.metaProvider
                Nothing -> options.optProvider
    loaded <- loadAuth requestedProvider >>= either (die . Text.unpack) pure
    case (transitionTarget, resumed) of
        (Just target, _)
            | loaded.loadedProvider /= target.modelProvider ->
                die $ "provider transition requested "
                    <> Text.unpack (providerSlug target.modelProvider)
                    <> " but auth resolved "
                    <> Text.unpack (providerSlug loaded.loadedProvider)
        (Nothing, Just (meta, _))
            | loaded.loadedProvider /= meta.metaProvider ->
                die $ "session provider is "
                    <> Text.unpack (providerSlug meta.metaProvider)
                    <> " but auth resolved "
                    <> Text.unpack (providerSlug loaded.loadedProvider)
        _ -> pure ()

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
    let basePlanHooks =
            cliPlanHooks interrupt escPaused (resolveColor stderr)
        planHooks = fullscreenAwarePlanHooks uiRuntimeRef basePlanHooks
        provider = loaded.loadedProvider
        model = fromMaybe
            (case transitionTarget of
                Just target -> target.modelId
                Nothing ->
                    maybe (defaultModelFor provider) (.metaModel) (fst <$> resumed))
            options.optModel
        effort = fromMaybe
            (maybe (defaultEffortFor provider) (.metaEffort) (fst <$> resumed))
            options.optEffort
        policy = resolveApprovalPolicy options isTty
            projectSettings.settingsAutoApprove
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
                        toFilePath
                            (handle.sessionDir </> fromFilePath ".agent-running")
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
        writeIORef subagentForkSource (Just transcriptRef)
        prompt <- loadPrompt options
        let titleHint = case resumed of
                Just (meta, _) -> Just meta.metaTitle
                Nothing -> sessionTitleFromPrompt <$> prompt
        setCliWindowTitle stdoutTty stdout (cliWindowTitle cwd titleHint)
        startupContext <- loadAgentsContext options provider home cwd initialItems initialPrevious
        skills <- loadSkillsCatalog options home projectRoot cwd True
        skillsRef <- newIORef skills
        skillInvocationsRef <- newIORef
            (buildSkillInvocations reservedSlashNames skills)
        when (null initialItems && not (isJust initialPrevious)) $
            queueSkillCatalogContext startupContext skills

        persist <- preparePersistence options root provider model cwd effort prompt resumed
        writeIORef persistSlotRef persist
        usageRef <- newIORef $ case resumed of
            Just (meta, turns) -> sessionUsageFromTurns meta turns
            Nothing -> emptyTokenUsage
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
        withCtrlCHandler interrupt $
            withInterruptResume progName persist RunQuit do
                case provider of
                    OpenAIProvider ->
                        try @_ @CodexAuthFailed
                            (withCodexWsWithProvider loaded.loadedTokenProvider \conn _credential -> do
                                wsLock <- newMVar ()
                                wsHealthy <- newIORef True
                                case multiCtx of
                                    Just ctx ->
                                        setSubagentRunner ctx.multiRegistry $
                                            runCodexSubagent
                                                subagentRuntime
                                                loaded.loadedTokenProvider
                                                ctx.multiSendToRoot
                                    Nothing -> pure ()
                                let lockedBackend =
                                        lockedOpenAiBackend
                                            wsLock
                                            loaded.loadedTokenProvider
                                            wsHealthy
                                            conn
                                            (readIORef paramsRef)
                                            transcriptRef
                                            contextTokensRef
                                    noticingBackend =
                                        withPendingInputs pendingNotices lockedBackend
                                    btwBackend privateParams privateTranscript =
                                        freshOpenAiBackend
                                            loaded.loadedTokenProvider
                                            (readIORef privateParams)
                                            privateTranscript
                                activeBackend <-
                                    prepareTransitionBackend transition persist noticingBackend
                                runSession options provider policy tools toolEnv planMode uiRuntimeRef prompt pendingTurn unavailableProviders paramsRef transcriptRef initialTurns
                                    initialPrevious persist projectRoot home cwd (Just loaded.loadedTokenProvider) loaded.loadedOpenAiPool startupContext skillsRef skillInvocationsRef escPaused interrupt
                                    multiCtx rootTurnRef subagentSessions pendingNotices subagentStoreRoot usageRef claimCurrentSession activeBackend btwBackend)
                            >>= \case
                                Left (CodexAuthFailed err) ->
                                    case transition of
                                        Just active
                                            | active.transitionCause == AutomaticFallback ->
                                                pure (RunProviderStartFailed err)
                                        _ -> die ("openai auth: " <> show err)
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
                                            xaiBackend xaiOptions loaded.loadedTokenProvider
                                                (readIORef childParamsRef) childTranscript)
                            Nothing -> pure ()
                        let backend =
                                withPendingInputs pendingNotices $
                                    xaiBackend xaiOptions loaded.loadedTokenProvider
                                        (readIORef paramsRef) transcriptRef
                            btwBackend privateParams privateTranscript =
                                xaiBackend xaiOptions loaded.loadedTokenProvider
                                    (readIORef privateParams) privateTranscript
                        activeBackend <-
                            prepareTransitionBackend transition persist backend
                        runSession options provider policy tools toolEnv planMode uiRuntimeRef prompt pendingTurn unavailableProviders paramsRef transcriptRef initialTurns
                            initialPrevious persist projectRoot home cwd (Just loaded.loadedTokenProvider) loaded.loadedOpenAiPool startupContext skillsRef skillInvocationsRef escPaused interrupt
                            multiCtx rootTurnRef subagentSessions pendingNotices subagentStoreRoot usageRef claimCurrentSession activeBackend btwBackend
                    OpenRouterProvider -> do
                        openRouterOptions <- OpenRouter.clientOptionsFromEnv
                        case multiCtx of
                            Just ctx ->
                                setSubagentRunner ctx.multiRegistry $
                                    runHttpSubagent
                                        subagentRuntime
                                        OpenRouterProvider
                                        (\childParamsRef childTranscript ->
                                            openRouterBackend openRouterOptions loaded.loadedTokenProvider
                                                (readIORef childParamsRef) childTranscript)
                            Nothing -> pure ()
                        let backend =
                                withPendingInputs pendingNotices $
                                    openRouterBackend openRouterOptions loaded.loadedTokenProvider
                                        (readIORef paramsRef) transcriptRef
                            btwBackend privateParams privateTranscript =
                                openRouterBackend openRouterOptions loaded.loadedTokenProvider
                                    (readIORef privateParams) privateTranscript
                        activeBackend <-
                            prepareTransitionBackend transition persist backend
                        runSession options provider policy tools toolEnv planMode uiRuntimeRef prompt pendingTurn unavailableProviders paramsRef transcriptRef initialTurns
                            initialPrevious persist projectRoot home cwd (Just loaded.loadedTokenProvider) loaded.loadedOpenAiPool startupContext skillsRef skillInvocationsRef escPaused interrupt
                            multiCtx rootTurnRef subagentSessions pendingNotices subagentStoreRoot usageRef claimCurrentSession activeBackend btwBackend

preparePersistence
    :: CliOptions
    -> OsPath
    -> Provider
    -> Text
    -> OsPath
    -> Text
    -> Maybe Text
    -> Maybe (SessionMeta, [SessionTurn])
    -> IO Persistence
preparePersistence options root provider model cwd effort prompt resumed =
    case resumed of
        Just (meta, _) -> do
            let handle = SessionHandle
                    { sessionDir = root </> fromText meta.metaId
                    , sessionMetaPath =
                        root </> fromText meta.metaId </> fromFilePath "meta.json"
                    , sessionTranscriptPath =
                        root </> fromText meta.metaId </> fromFilePath "transcript.jsonl"
                    , sessionMeta = meta
                    }
            color <- resolveColor stderr
            putTextLn stderr
                (roleMuted color
                    (glyphSession <> "session: " <> meta.metaId <> " (resumed)"))
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
    -> IORef (Maybe FullscreenRuntime)
    -> Maybe Text
    -> Maybe PendingTurn
    -> [Provider]
    -> IORef ResponseCreateParams
    -> IORef [ResponseItem]
    -> [SessionTurn]
    -> Maybe Text
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
    -> (SessionHandle -> IO ())
    -> Backend
    -> BtwBackendFactory
    -> IO RunResult
runSession options provider policy tools toolEnv planMode uiRuntimeRef prompt pendingTurn unavailableProviders paramsRef transcriptRef initialTurns initialPrevious persist projectRoot home cwd tokenProvider openAiPool startupContext skillsRef skillInvocationsRef escPaused interrupt multiCtx rootTurnRef subagentSessions pendingNotices storeRoot usageRef onPersisted backend btwBackend =
  withSessionTitleManager btwBackend paramsRef \titleManager -> do
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
    ioLock <- newMVar ()
    previous <- newIORef initialPrevious
    titleTurnCount <- newIORef =<< sessionTitleTurnCountFromSlot persist
    selectedAgent <- newIORef AgentRoot
    let loadAgentEntries = do
            rootItems <- readIORef transcriptRef
            agents <- case multiCtx of
                Nothing -> pure []
                Just ctx -> listAgents ctx.multiRegistry Nothing
            let rootEntry = AgentEntry
                    { agentTarget = AgentRoot
                    , agentPath = "/root"
                    , agentStatus = "active"
                    , agentTranscript = responseItemLines rootItems
                    }
            children <- mapM materializeChild agents
            pure (rootEntry : children)
          where
            materializeChild (path, agentId, status) = do
                sessions <- readIORef subagentSessions
                stored <- case Map.lookup agentId sessions of
                    Nothing -> pure ["(" <> formatAgentStatus status <> ")"]
                    Just session ->
                        responseItemLines <$> readIORef session.subSessionTranscript
                let transcript = stored <> case status of
                        Completed (Just result)
                            | not (Text.null (Text.strip result)) ->
                                ["assistant: " <> Text.strip result]
                        Errored message ->
                            ["error: " <> Text.strip message]
                        _ -> []
                pure AgentEntry
                    { agentTarget = AgentChild agentId
                    , agentPath = taskPathText path
                    , agentStatus = formatAgentStatus status
                    , agentTranscript = transcript
                    }
        agentViewport = AgentViewportEnv
            { viewportSelected = selectedAgent
            , viewportEntries = loadAgentEntries
            }
    let sessionReset = do
            resetLiveConversation previous transcriptRef attachmentsRef planMode
            writeIORef usageRef emptyTokenUsage
            writeIORef lastAssistantRef Nothing
            writeIORef pendingNotices []
            writeIORef subagentSessions Map.empty
            writeIORef selectedAgent AgentRoot
            case multiCtx of
                Just ctx -> resetSubagentRegistry ctx.multiRegistry
                Nothing -> pure ()
            freshAgents <-
                loadAgentsContext options provider home cwd [] Nothing
            freshSkills <- loadSkillsCatalog options home projectRoot cwd True
            writeIORef skillsRef freshSkills
            writeIORef skillInvocationsRef
                (buildSkillInvocations reservedSlashNames freshSkills)
            queueSkillCatalogContext freshAgents freshSkills
            fresh <- readIORef freshAgents
            writeIORef startupContext fresh
        refreshSkills queueContext = do
            refreshed <- loadSkillsCatalog
                options home projectRoot cwd queueContext
            writeIORef skillsRef refreshed
            writeIORef skillInvocationsRef
                (buildSkillInvocations reservedSlashNames refreshed)
            when queueContext $
                queueSkillCatalogContext startupContext refreshed
    policyRef <- newIORef policy
    stderrTty <- hIsTerminalDevice stderr
    stdinTty <- hIsTerminalDevice stdin
    stdoutTty <- hIsTerminalDevice stdout
    terminal <- detectTerminalCapabilities stdout
    reportTerminalCwd terminal stdout (toFilePath cwd)
    useColor <- resolveColor stdout
    branch <- detectGitBranch cwd
    let fullscreenEnabled =
            stdinTty
                && stdoutTty
                && not (isOneShot options)
                && options.optScreenMode /= ScreenMinimal
        initialFullscreenState =
            reduceUi
                (UiSetRepository
                    branch
                    (toText (takeFileName (takeDirectory cwd))
                        <> "/"
                        <> toText (takeFileName cwd)))
                (hydrateUiHistory initialTurns)
    fullscreen <- if fullscreenEnabled
        then Just <$> newFullscreenRuntime
            (requestCancel toolEnv.toolCancel)
            (noteFullscreenCtrlC interrupt)
            (copyTerminalClipboard terminal stdout)
            (\active ->
                when terminal.terminalNativeProgress $
                    setNativeProgress stderr active)
            ((,) <$> readIORef selectedAgent <*> loadAgentEntries)
            useColor
            initialFullscreenState
        else pure Nothing
    writeIORef uiRuntimeRef fullscreen
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
                stderrTty && terminal.terminalNativeProgress
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
            , sessionRender = render
            , sessionProvider = provider
            , sessionUnavailableProviders = unavailableProvidersRef
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
            , sessionStoreRoot = storeRoot
            , sessionUsage = usageRef
            , sessionLastAssistant = lastAssistantRef
            , sessionTerminal = terminal
            , sessionFullscreen = fullscreen
            , sessionAgentViewport = Just agentViewport
            , sessionBeginSubagentTurn = beginSubagentTurn
            , sessionFinishSubagentTurn = finishSubagentTurn
            , sessionAbortSubagentTurn = abortSubagentTurn
            , sessionOnPersisted = onPersisted
            , sessionReset = sessionReset
            }
    let sessionAction = case pendingTurn of
            Just pending ->
                runPendingTurn env pending
            Nothing -> case prompt of
                Just text -> do
                    inputs <- preparePromptSkillInputs env text [UserMessage text]
                        >>= either (die . Text.unpack) pure
                    result <- runOneTurn env text inputs
                    finishTurn env True result
                Nothing ->
                    repl env
    result <-
        (case fullscreen of
            Nothing -> sessionAction
            Just runtime -> runFullscreen runtime sessionAction)
            `finally` writeIORef uiRuntimeRef Nothing
    _ <- waitForSessionTitleResults 5000000 titleManager
    applyPendingSessionTitles env
    pure result

runPendingTurn :: SessionEnv -> PendingTurn -> IO RunResult
runPendingTurn = runPendingTurnWithCooldownRetry True

runPendingTurnWithCooldownRetry
    :: Bool
    -> SessionEnv
    -> PendingTurn
    -> IO RunResult
runPendingTurnWithCooldownRetry allowCooldownRetry env pending = do
    writeIORef env.sessionPlanMode.planStateRef pending.pendingPlanState
    case env.sessionFullscreen of
        Nothing -> pure ()
        Just runtime ->
            emitUiEvent runtime (UiUserSubmitted pending.pendingPromptText)
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
            else do
                notifyAttention stderr InputRequested
                repl env
    TurnFailed ->
        if exitAfter
            then exitFailure
            else do
                case env.sessionFullscreen of
                    Nothing -> putTrailingNewline env.sessionPrinted
                    Just _ -> pure ()
                notifyAttention stderr InputRequested
                repl env
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
                        reportProviderUnavailable apiError
                        if exitAfter
                            then exitFailure
                            else do
                                notifyAttention stderr InputRequested
                                replWithDraft env pending.pendingPromptText

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
            emitUiEvent runtime (UiSetNotice (Just waitMessage))
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
                        (UiSetNotice (Just "automatic retry cancelled"))
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
                        (UiSetNotice (Just "retrying turn"))
                Nothing -> do
                    color <- resolveColor stderr
                    putTextLn stderr
                        (roleMuted color (glyphOk <> "retrying turn"))
            runPendingTurnWithCooldownRetry False env pending

reportProviderUnavailable :: ApiError -> IO ()
reportProviderUnavailable apiError = do
    color <- resolveColor stderr
    now <- getCurrentTime
    let detail = case apiError of
            CredentialsExhausted{retryAt} ->
                let delay = max 0 (diffUTCTime retryAt now)
                in "all configured accounts are temporarily unavailable"
                    <> if delay == 0
                        then "; retry now"
                        else "; retry in " <> formatDuration delay
            _ -> Text.pack (show apiError)
    putTextLn stderr $ roleError color $
        "provider unavailable; no usable fallback provider account is available: "
            <> detail

repl :: SessionEnv -> IO RunResult
repl env = replWithDraft env ""

replWithDraft :: SessionEnv -> Text -> IO RunResult
replWithDraft env@SessionEnv
    { sessionBtwBackend = btwBackend
    , sessionRender = render
    , sessionProvider = provider
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
    , sessionLastAssistant = lastAssistantRef
    , sessionTerminal = terminal
    , sessionFullscreen = fullscreen
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
    mline <- case fullscreen of
        Just runtime ->
            readFullscreenLine runtime skillCommands
                PromptState
                    { promptModel = currentModel params
                    , promptEffort = currentEffort params
                    , promptMode = idleMode
                    , promptUsage = usage
                    , promptAttachments = length pendingAttachments
                    }
                draft
        Nothing -> withMVar render.renderLock \_ -> do
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
    case mline of
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
                Just _ -> pure ()
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
                            Right message ->
                                displayInfo message $
                                    Text.putStrLn
                                        (roleMuted stdoutColor
                                            (glyphOk <> message))
            continueWith keptDraft
        ReplChooseModel keptDraft ->
            chooseModel (continueWith keptDraft)
        ReplChooseEffort keptDraft ->
            chooseEffort (continueWith keptDraft)
        ReplPasted pasted ->
            submitLine skillCommands skillInvocations
                continue stdoutColor True pasted
        ReplText line ->
            submitLine skillCommands skillInvocations
                continue stdoutColor False line
  where
    submitLine skillCommands skillInvocations continue color pasted line =
        let stripped = Text.strip line
        in if Text.null stripped
            then continue
            else do
                when pasted do
                    let chip = formatPasteChip stripped
                    when (chip /= stripped && isNothing fullscreen) do
                        Text.putStrLn (roleMuted color chip)
                case parseReplLineWithSkills skillCommands line of
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
                                displayInfo message $
                                    Text.putStrLn
                                        (roleMuted color
                                            (glyphOk <> message))
                                continue
                            _ -> do
                                pendingImages <- atomicModifyIORef' attachmentsRef \imgs -> ([], imgs)
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
                    ReplPaste{pasteImmediate, pasteCaption} -> do
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
                        chooseModel continue
                    ReplSetModel name -> do
                        color <- resolveColor stdout
                        message <- applyModelChange
                            provider name paramsRef render previous persist
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
                        result <- runProviderCompact provider tokenProvider paramsRef transcriptRef focus
                        case result of
                            Left err -> do
                                displayError err $
                                    Text.hPutStrLn stderr (roleError color err)
                                continue
                            Right outcome -> do
                                writeIORef transcriptRef outcome.compactHistory
                                writeIORef previous Nothing
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
                                                , turnUsage = Nothing
                                                }
                                        handle' <- appendTurn handle turn
                                        let meta = handle'.sessionMeta
                                                { metaLastResponseId = Nothing
                                                , metaUpdatedAt = now
                                                }
                                        writeSessionMeta handle'.sessionMetaPath meta
                                        writeIORef slotRef
                                            (PersistenceActive handle'{sessionMeta = meta})
                                continue
                    ReplPlan maybeDescription ->
                        enterPlanFromSlash env maybeDescription >>= \case
                            Just providerSwitch ->
                                pure (RunSwitchProvider providerSwitch)
                            Nothing -> continue
                    ReplBtw question -> do
                        color <- resolveColor stdout
                        fullscreenEvent
                            (UiSetNotice (Just "btw · asking…"))
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
                                tty <- hIsTerminalDevice stdout
                                setCliWindowTitle tty stdout
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
                                        tty <- hIsTerminalDevice stdout
                                        setCliWindowTitle tty stdout
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
    displayInfo message minimalAction = case fullscreen of
        Nothing -> minimalAction
        Just runtime -> emitUiEvent runtime (UiSystemMessage message)
    displayError message minimalAction = case fullscreen of
        Nothing -> minimalAction
        Just runtime -> emitUiEvent runtime (UiErrorMessage message)
    setEffort level = do
        color <- resolveColor stdout
        modifyIORef' paramsRef (setReasoningEffort level)
        displayInfo ("effort set to " <> level) $
            Text.putStrLn
                (roleMuted color
                    (glyphOk <> "effort set to " <> level))
        case persist of
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
    chooseEffort next = do
        params <- readIORef paramsRef
        effortChoice fullscreen (currentEffort params) >>= \case
            Nothing -> next
            Just level -> setEffort level >> next
    chooseModel next = do
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
                        provider choice.modelId paramsRef render previous persist
                    displayInfo message $
                        Text.putStrLn
                            (roleMuted color
                                (glyphOk <> message))
                    next
                | otherwise ->
                    requestModelProviderSwitch choice persist >>= \case
                        Left err -> do
                            displayError err $
                                Text.hPutStrLn stderr
                                    (roleError color err)
                            next
                        Right result -> pure result
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
                                            ("usage: " <> Text.pack (show err))
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
    :: Provider
    -> Text
    -> IORef ResponseCreateParams
    -> RenderConfig
    -> IORef (Maybe Text)
    -> Persistence
    -> IO Text
applyModelChange provider name paramsRef render previous persist = do
    modifyIORef' paramsRef (setModel name)
    writeIORef render.renderModelRef name
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
    :: ModelOption
    -> Persistence
    -> IO (Either Text RunResult)
requestModelProviderSwitch choice persist =
    prepareProviderTransition
        ManualTransition [] Nothing choice persist >>= \case
            Left err -> pure (Left err)
            Right transition -> do
                color <- resolveColor stdout
                Text.putStrLn $ roleMuted color $
                    glyphOk
                        <> "switching to "
                        <> providerSlug choice.modelProvider
                        <> "/"
                        <> choice.modelId
                        <> " (conversation continued locally)"
                pure (Right (RunSwitchProvider transition))

requestAutomaticProviderFallback
    :: SessionEnv
    -> ApiError
    -> PendingTurn
    -> IO (Maybe ProviderTransition)
requestAutomaticProviderFallback env apiError pending = do
    sessionId <- ensureTransitionSessionId env.sessionPersist
    unavailable <- readIORef env.sessionUnavailableProviders
    chooseAutomaticProviderTransition
        env.sessionProvider
        unavailable
        sessionId
        pending
        apiError

continueAutomaticFallback
    :: ProviderTransition
    -> ApiError
    -> IO (Maybe ProviderTransition)
continueAutomaticFallback failed apiError =
    case failed.transitionPendingTurn of
        Nothing -> pure Nothing
        Just pending ->
            chooseAutomaticProviderTransition
                failed.transitionTarget.modelProvider
                failed.transitionUnavailableProviders
                failed.transitionSessionId
                pending
                apiError

chooseAutomaticProviderTransition
    :: Provider
    -> [Provider]
    -> Maybe Text
    -> PendingTurn
    -> ApiError
    -> IO (Maybe ProviderTransition)
chooseAutomaticProviderTransition current unavailable0 sessionId pending apiError =
    tryCandidates unavailable candidates
  where
    unavailable = markUnavailable current unavailable0
    candidates = fallbackCandidates unavailable0 current apiError

    tryCandidates unavailable = \case
        [] -> pure Nothing
        choice : rest ->
            validateProviderTarget choice >>= \case
                Left err -> do
                    color <- resolveColor stderr
                    putTextLn stderr $ roleMuted color $
                        "skipping "
                            <> providerSlug choice.modelProvider
                            <> ": "
                            <> err
                    tryCandidates
                        (markUnavailable choice.modelProvider unavailable)
                        rest
                Right () -> do
                    color <- resolveColor stderr
                    putTextLn stderr $ roleWarn color $
                        glyphWarn
                            <> providerSlug current
                            <> " unavailable; trying this turn with "
                            <> providerSlug choice.modelProvider
                            <> "/"
                            <> choice.modelId
                    pure $ Just ProviderTransition
                        { transitionTarget = choice
                        , transitionSessionId = sessionId
                        , transitionPendingTurn = Just pending
                        , transitionUnavailableProviders = unavailable
                        , transitionCause = AutomaticFallback
                        }

prepareProviderTransition
    :: TransitionCause
    -> [Provider]
    -> Maybe PendingTurn
    -> ModelOption
    -> Persistence
    -> IO (Either Text ProviderTransition)
prepareProviderTransition cause unavailable pending choice persist =
    validateProviderTarget choice >>= \case
        Left err -> pure (Left err)
        Right () -> do
            sessionId <- ensureTransitionSessionId persist
            pure $ Right ProviderTransition
                { transitionTarget = choice
                , transitionSessionId = sessionId
                , transitionPendingTurn = pending
                , transitionUnavailableProviders = unavailable
                , transitionCause = cause
                }

validateProviderTarget :: ModelOption -> IO (Either Text ())
validateProviderTarget choice =
    loadAuth (Just choice.modelProvider) >>= \case
        Left err -> pure $ Left $
            "cannot switch to "
                <> providerSlug choice.modelProvider
                <> ": "
                <> err
        Right loaded ->
            probeLoadedAuth loaded >>= \case
                Left err -> pure $ Left $
                    "cannot switch to "
                        <> providerSlug choice.modelProvider
                        <> ": credentials unavailable: "
                        <> Text.pack (show err)
                Right usable
                    | usable.loadedProvider /= choice.modelProvider ->
                        pure $ Left $
                            "cannot switch to "
                                <> providerSlug choice.modelProvider
                                <> ": auth resolved "
                                <> providerSlug usable.loadedProvider
                    | otherwise -> pure (Right ())

ensureTransitionSessionId
    :: Persistence
    -> IO (Maybe Text)
ensureTransitionSessionId PersistenceDisabled = pure Nothing
ensureTransitionSessionId (PersistenceEnabled slotRef) = do
    handle <- ensureSession slotRef
    pure (Just handle.sessionMeta.metaId)

commitProviderTransition
    :: Maybe ProviderTransition
    -> Persistence
    -> IO ()
commitProviderTransition Nothing _ = pure ()
commitProviderTransition _ PersistenceDisabled = pure ()
commitProviderTransition (Just transition) (PersistenceEnabled slotRef) = do
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
            writeIORef slotRef (PersistenceActive handle { sessionMeta = meta })

prepareTransitionBackend
    :: Maybe ProviderTransition
    -> Persistence
    -> Backend
    -> IO Backend
prepareTransitionBackend Nothing _ backend = pure backend
prepareTransitionBackend (Just transition) persist backend
    | transition.transitionCause == ManualTransition = do
        commitProviderTransition (Just transition) persist
        pure backend
    | otherwise = do
        committed <- newIORef False
        pure $ commitBackendOnSuccess committed transition persist backend

commitBackendOnSuccess
    :: IORef Bool
    -> ProviderTransition
    -> Persistence
    -> Backend
    -> Backend
commitBackendOnSuccess committed transition persist (Backend submit) =
    Backend \previous inputs onEvent -> do
        result <- submit previous inputs onEvent
        case result of
            Right _ -> do
                shouldCommit <- atomicModifyIORef' committed \done ->
                    (True, not done)
                when shouldCommit $
                    commitProviderTransition (Just transition) persist
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
        -- Force a disk/env re-read by pretending the cached credential was
        -- rejected for authentication; the reloadable provider clears its cache.
        getNextToken tokenProvider (Just FailedCredential
            { credential = Credential
                { accessToken = ""
                , accountId = ""
                , leaseId = Nothing
                , provider
                }
            , failure = AccountAuthenticationRejected
            }) >>= \case
            Left err ->
                pure $ Left $
                    "reload-auth failed: " <> Text.pack (show err)
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
                                reportProviderUnavailable apiError
                                pure Nothing
                            Just providerTransition ->
                                pure (Just providerTransition)
                _ -> do
                    putTrailingNewline printed
                    pure Nothing

-- | Discover AGENTS.md once for a fresh session. Resumed transcripts keep
-- whatever instructions were already in history.
loadAgentsContext
    :: CliOptions
    -> Provider
    -> OsPath
    -> OsPath
    -> [ResponseItem]
    -> Maybe Text
    -> IO (IORef (Maybe Text))
loadAgentsContext options provider home cwd initialItems initialPrevious
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
                color <- resolveColor stderr
                putTextLn stderr
                    (roleMuted color
                        (glyphSession <> "agents.md: loaded "
                            <> Text.pack (show (length files))
                            <> if length files == 1 then " file" else " files"))
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
    (_, Just path) -> Just . Text.strip <$> Text.readFile (toFilePath path)
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
            pickResumeChoice fullscreen color root sessions >>= \case
                Nothing -> pure Nothing
                Just sessionId -> resume sessionId

pickResumeChoice
    :: Maybe FullscreenRuntime
    -> Bool
    -> OsPath
    -> [SessionMeta]
    -> IO (Maybe Text)
pickResumeChoice fullscreen color root sessions = case fullscreen of
    Nothing -> pickResumeSession color root sessions
    Just runtime ->
        requestFullscreenChoice
            runtime
            "Resume session"
            0
            [ ( meta.metaTitle
              , providerSlug meta.metaProvider
                    <> "/"
                    <> meta.metaModel
                    <> " · "
                    <> toText meta.metaCwd
              )
            | meta <- sessions
            ]
            >>= pure . (>>= (`atMay` map (.metaId) sessions))

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

-- | Serialize turns on the root OpenAI WebSocket connection because
-- 'receiveWsResponse' is not multiplexed.
lockedOpenAiBackend
    :: MVar ()
    -> TokenProvider
    -> IORef Bool
    -> CodexConn
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> IORef (Maybe (Int, Int))
    -> Backend
lockedOpenAiBackend wsLock provider connectionHealthy conn getParams transcript
        contextTokens =
    let Backend submit =
            openAiBackendReconnecting provider connectionHealthy conn getParams transcript
        serialized = Backend \previous inputs onEvent ->
            withMVar wsLock \_ -> submit previous inputs onEvent
    in autoCompactOpenAiBackend provider
        getParams transcript contextTokens serialized

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
                ["-C", toFilePath cwd, "rev-parse", "--abbrev-ref", "HEAD"]
                "")
            :: IO (Either SomeException (ExitCode, String, String))
    pure $ case result of
        Right (ExitSuccess, output, _) ->
            let branch = Text.strip (Text.pack output)
            in if Text.null branch
                then toText (takeFileName cwd)
                else branch
        _ -> toText (takeFileName cwd)

-- | Apply compact turns as full transcript replacements when resuming.
foldSessionItems :: [SessionTurn] -> [ResponseItem]
foldSessionItems = go []
  where
    go acc [] = acc
    go acc (turn:rest)
        | isTranscriptResetTurn turn.turnUserText =
            -- /clear and /new store an empty snapshot; /compact stores the
            -- rebuilt history. Either way, turnItems replaces prior history.
            go turn.turnItems rest
        | hasCompactionCheckpoint turn.turnItems =
            go turn.turnItems rest
        | otherwise = go (acc <> turn.turnItems) rest

hydrateUiHistory :: [SessionTurn] -> UiState
hydrateUiHistory = foldl addTurn initialUiState
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
            Just text -> reduceUi (UiSystemMessage text) cleared

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
