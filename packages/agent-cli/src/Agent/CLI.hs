-- | Command-line entry point: one-shot @-p@ or an interactive REPL.
module Agent.CLI
    ( DevResult(..)
    , afterDev
    , applyReplMode
    , cycleReplInteraction
    , devArgs
    , devMain
    , formatReplStatusLine
    , formatTokenUsage
    , run
    ) where

import Agent.CLI.Artifact (fencedCodeBlock, lastDiffBlock)
import Agent.CLI.Auth (LoadedAuth(..), loadAuth)
import Agent.CLI.AgentViewport
    ( AgentEntry(..)
    , AgentTarget(..)
    , AgentViewportEnv(..)
    , formatAgentStatus
    , pickAgentViewport
    , renderAgentViewportPanelFor
    , responseItemLines
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
    , childApprove
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
    , runProviderCompact
    )
import Agent.CLI.ImagePreview
    ( detectImagePreviewProtocol
    , previewColumnsFor
    , previewRowsFor
    , renderImagePreview
    )
import Agent.CLI.Input (ReplLine(..), formatPasteChip, readReplLineWithInitial)
import Agent.CLI.ReplMode
    ( ReplMode(..)
    , replModeFromState
    )
import Agent.CLI.Interrupt
    ( InterruptState
    , isWrappedUserInterrupt
    , newInterruptState
    , withCtrlCHandler
    , withTurnCancel
    )
import Agent.CLI.Login (runLoginManager)
import Agent.CLI.ModelPicker (pickModel)
import Agent.CLI.Models (ModelOption(..))
import Agent.CLI.Notification
    ( AttentionRequest(InputRequested)
    , notifyAttention
    )
import Agent.CLI.Options
import Agent.CLI.PendingInputs (withPendingInputs)
import Agent.CLI.Resume (pickResumeSession)
import Agent.CLI.Plan (cliPlanHooks)
import Agent.CLI.Progress
    ( osc9ProgressRemove
    , wrapOscForTmux
    )
import Agent.CLI.Project
    ( ProjectSettings(..)
    , loadProjectSettings
    , resolveProjectRoot
    )
import Agent.CLI.Prompt (defaultModelFor, systemPrompt)
import Agent.CLI.ProviderFallback (fallbackCandidates)
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
import Agent.CLI.Status
    ( applyReplMode
    , cycleReplInteraction
    , formatReplStatusLine
    , formatTokenUsage
    )
import Agent.CLI.SubagentStore
    ( SubagentDiskMeta(..)
    , loadSubagentState
    , saveSubagentState
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
    , notifyTerminal
    , withSynchronizedOutput
    )
import Agent.CLI.Tools (schemasFromAppTools)
import Agent.CLI.Turn (runOneTurn)
import Agent.CLI.Usage
    ( AccountUsageLine(..)
    , formatUsageReport
    )
import Agent.CLI.Worktree
    ( createWorktree
    , isUnderWorktreeRoot
    , removeWorktree
    , worktreeRoot
    )
import Agent.Loop
import Agent.Error (ApiError)
import Agent.InterAgentMessage (InterAgentMessage, interAgentMessagePayload)
import Agent.ProjectInstructions
    ( DiscoverOptions(..)
    , defaultDiscoverOptions
    , discoverProjectInstructions
    , formatAgentsMdForProvider
    , globalAgentsHomeDir
    , loadedInstructionFiles
    )
import Agent.OpenAI.Compaction
    ( clearSessionUserText
    , compactSessionUserText
    , isTranscriptResetTurn
    , newSessionUserText
    )
import qualified Agent.OpenAI.Auth as OpenAI
import Agent.OpenAI.LoopBackend (openAiBackend, openAiBackendReconnecting)
import Agent.OpenAI.Responses.Types
import Agent.OpenAI.Usage (fetchUsage)
import Agent.OpenAI.WebSocketClient
    ( CodexAuthFailed(..)
    , CodexConn
    , withCodexWsRetrying
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
    ( RunSubagent
    , RootTurnId
    , SubagentId(..)
    , SubagentRegistry
    , SubagentSpawnEnv(..)
    , SubagentStatus(..)
    , abortRootTurn
    , beginRootTurn
    , closeSubagentRegistry
    , resetSubagentRegistry
    , defaultSubagentConfig
    , formatCompletionNotice
    , getPreviousResponseId
    , getStatus
    , getSubagentCwd
    , getTaskPath
    , listAgents
    , newSubagentRegistry
    , restoreSubagent
    , restoreSubagentWithCwd
    , setPreviousResponseId
    , setSubagentOnComplete
    , setSubagentRunner
    )
import Agent.Tools
    ( CodingTools(..)
    , appToolHandlers
    , codingToolsFor
    , codingToolsForWithTypes
    , filterChildGrokTools
    )
import Agent.Tools.Grok.Task
    ( GrokSubagentSpec(..)
    , GrokSubagentSpecs
    , defaultSubagentType
    , lookupAgentModel
    , lookupAgentType
    , recordAgentSpec
    )
import Agent.Subagents.TaskPath (taskPathRoot, taskPathText)
import Agent.Tools.MultiAgents (MultiAgentContext(..), SubagentWorktree(..))
import Agent.Tools.PlanMode
    ( PlanModeEnv(..)
    , PlanModeHooks
    , PlanModeState(..)
    , activatePlanMode
    , deactivatePlanMode
    , planFilePath
    )
import Agent.Tools.Types (AppTool(..), ToolEnv(..), defaultToolEnv)
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
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Set as Set
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory.OsPath
    ( getCurrentDirectory
    , getHomeDirectory
    , makeAbsolute
    , setCurrentDirectory
    )
import System.Environment (getArgs, getProgName, lookupEnv)
import System.OsPath ((</>), takeDirectory)
import System.Console.ANSI (getTerminalSize)
import System.Console.ANSI.Codes (clearFromCursorToLineEndCode)
import System.Exit (die, exitFailure)
import System.IO (Handle, hFlush, hIsTerminalDevice, stderr, stdin, stdout)

-- | How the GHCi-driven agent REPL finished.
data DevResult
    = DevQuit
    | DevReload
    deriving (Eq, Show)

data RunResult
    = RunQuit
    | RunReload
    | RunSwitchProvider ProviderTransition
    | RunProviderStartFailed ApiError
    | RunResumeSession Text
      -- ^ Persisted session id. Consumed after the current provider-specific
      -- backend shuts down before starting the selected session.

-- | GHCi @:cmd@ helper: on 'DevReload', reload modules and re-enter 'devMain'.
afterDev :: DevResult -> IO String
afterDev = \case
    DevQuit -> pure ""
    DevReload -> pure $ unlines
        [ ":reload"
        , ":module +Agent.CLI"
        , ":cmd afterDev =<< devMain"
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

-- | Start the agent from GHCi (@repl@). Resumes the session written by @:reload@.
-- On first open (no resume pointer), selects OpenAI/gpt-5.6-sol with yolo
-- enabled and passes @--worktree@ unless the cwd is already under
-- @~/.haskell-agent/worktrees@.
devMain :: IO DevResult
devMain = do
    home <- getHomeDirectory
    resumeId <- readDevResumePointer home
    underWorktree <- case resumeId of
        Just _ -> pure True
        Nothing -> do
            cwd <- makeAbsolute =<< getCurrentDirectory
            root <- makeAbsolute (worktreeRoot home)
            pure (isUnderWorktreeRoot root cwd)
    let args = devArgs resumeId underWorktree
    case parseArgs args of
        Left err -> do
            clearDevResumePointer home
            die err
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
                DevQuit -> clearDevResumePointer home >> pure DevQuit
                DevReload -> pure DevReload

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
                DevReload -> do
                    home <- getHomeDirectory
                    clearDevResumePointer home
                    die ":reload is only available under `repl` (nix develop)"

-- | Tear down and rebuild provider-specific auth, tools, prompt, and transport.
-- Automatic transitions carry the exact failed turn in memory and commit
-- persisted provider metadata only after the replacement backend succeeds.
runAgentWithRestarts :: CliOptions -> IO DevResult
runAgentWithRestarts options = go options Nothing
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
                                Nothing -> pure DevQuit
                    _ -> pure DevQuit
            RunQuit -> pure DevQuit
            RunReload -> pure DevReload

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
        Left err -> die err
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
                Left err -> die err
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
    let planHooks = cliPlanHooks interrupt escPaused (resolveColor stderr)
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
    persistSlotRef <- newIORef
        (Nothing :: Maybe (IORef (Either SessionCreate SessionHandle)))
    -- Per-subagent transcripts / previous ids, shared across send_input / task.
    subagentSessions <- newIORef Map.empty
    subagentStoreRoot <- newIORef Nothing
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
            , multiSendToRoot = Just sendToRoot
            }
    coding <- codingToolsForWithTypes provider toolEnv (Just planHooks) multiCtx agentTypesRef
    case multiCtx of
        Just ctx ->
            setSubagentOnComplete ctx.multiRegistry \agentId status -> do
                atomicModifyIORef' pendingNotices \xs ->
                    (xs <> [UserMessage (formatCompletionNotice agentId status)], ())
                terminal <- detectTerminalCapabilities stderr
                notifyTerminal terminal stderr
                    ("Subagent completed: " <> agentId.unSubagentId)
                sessions <- readIORef subagentSessions
                case Map.lookup agentId sessions of
                    Just session ->
                        persistSubagentSnapshot subagentStoreRoot ctx.multiRegistry
                            agentTypesRef agentId session.subSessionTranscript
                    Nothing -> pure ()
        Nothing -> pure ()
    let claimCurrentSession handle
            | managedAgentSession = pure ()
            | otherwise =
                readIORef activeSessionLock >>= \case
                    Just _ -> pure ()
                    Nothing ->
                        acquireSessionLock handle >>= \case
                            Left err -> throwIO (userError (Text.unpack err))
                            Right lockPath ->
                                writeIORef activeSessionLock (Just lockPath)
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
                    closeSubagentRegistry ctx.multiRegistry
                    flushAllSubagentSnapshots subagentStoreRoot ctx.multiRegistry
                        subagentSessions agentTypesRef
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
            initialPrevious = case transition of
                Just _ -> Nothing
                Nothing -> resumed >>= \(meta, _) -> meta.metaLastResponseId
        paramsRef <- newIORef params
        transcriptRef <- newIORef initialItems
        prompt <- loadPrompt options
        let titleHint = case resumed of
                Just (meta, _) -> Just meta.metaTitle
                Nothing -> sessionTitleFromPrompt <$> prompt
        setCliWindowTitle stdoutTty stdout (cliWindowTitle cwd titleHint)
        agentsContext <- loadAgentsContext options provider home cwd initialItems initialPrevious

        persist <- preparePersistence options root provider model cwd effort prompt resumed
        writeIORef persistSlotRef persist
        usageRef <- newIORef $ case resumed of
            Just (meta, turns) -> sessionUsageFromTurns meta turns
            Nothing -> emptyTokenUsage
        case persist of
            Just slotRef -> do
                slot <- readIORef slotRef
                case slot of
                    Right handle -> do
                        claimCurrentSession handle
                        noteSessionDir handle.sessionDir
                    Left _ -> pure ()
            Nothing -> pure ()
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
                                                options
                                                policy
                                                planHooks
                                                paramsRef
                                                wsLock
                                                loaded.loadedTokenProvider
                                                wsHealthy
                                                conn
                                                ctx.multiRegistry
                                                subagentSessions
                                                subagentStoreRoot
                                                agentTypesRef
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
                                    noticingBackend =
                                        withPendingInputs pendingNotices lockedBackend
                                    btwBackend privateParams privateTranscript =
                                        freshOpenAiBackend
                                            loaded.loadedTokenProvider
                                            (readIORef privateParams)
                                            privateTranscript
                                activeBackend <-
                                    prepareTransitionBackend transition persist noticingBackend
                                runSession options provider policy tools toolEnv planMode prompt pendingTurn unavailableProviders paramsRef transcriptRef
                                    initialPrevious persist projectRoot home cwd (Just loaded.loadedTokenProvider) loaded.loadedOpenAiPool agentsContext escPaused interrupt
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
                                        options
                                        policy
                                        planHooks
                                        paramsRef
                                        XAIProvider
                                        (\childParamsRef childTranscript ->
                                            xaiBackend xaiOptions loaded.loadedTokenProvider
                                                (readIORef childParamsRef) childTranscript)
                                        ctx.multiRegistry
                                        subagentSessions
                                        agentTypesRef
                                        subagentStoreRoot
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
                        runSession options provider policy tools toolEnv planMode prompt pendingTurn unavailableProviders paramsRef transcriptRef
                            initialPrevious persist projectRoot home cwd (Just loaded.loadedTokenProvider) loaded.loadedOpenAiPool agentsContext escPaused interrupt
                            multiCtx rootTurnRef subagentSessions pendingNotices subagentStoreRoot usageRef claimCurrentSession activeBackend btwBackend
                    OpenRouterProvider -> do
                        openRouterOptions <- OpenRouter.clientOptionsFromEnv
                        case multiCtx of
                            Just ctx ->
                                setSubagentRunner ctx.multiRegistry $
                                    runHttpSubagent
                                        options
                                        policy
                                        planHooks
                                        paramsRef
                                        OpenRouterProvider
                                        (\childParamsRef childTranscript ->
                                            openRouterBackend openRouterOptions loaded.loadedTokenProvider
                                                (readIORef childParamsRef) childTranscript)
                                        ctx.multiRegistry
                                        subagentSessions
                                        agentTypesRef
                                        subagentStoreRoot
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
                        runSession options provider policy tools toolEnv planMode prompt pendingTurn unavailableProviders paramsRef transcriptRef
                            initialPrevious persist projectRoot home cwd (Just loaded.loadedTokenProvider) loaded.loadedOpenAiPool agentsContext escPaused interrupt
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
    -> IO (Maybe (IORef (Either SessionCreate SessionHandle)))
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
            Just <$> newIORef (Right handle)
        Nothing
            | shouldPersist options ->
                -- Defer directory creation until the first successful turn so
                -- an abandoned REPL does not leave empty session folders.
                Just <$> newIORef (Left SessionCreate
                    { createRoot = root
                    , createProvider = provider
                    , createModel = model
                    , createCwd = cwd
                    , createEffort = effort
                    , createTitleHint = sessionTitleFromPrompt <$> prompt
                    })
            | otherwise -> pure Nothing

-- | On Ctrl-C, print a copy-pasteable --resume line when a session exists.
withInterruptResume
    :: String
    -> Maybe (IORef (Either SessionCreate SessionHandle))
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
    -> Maybe (IORef (Either SessionCreate SessionHandle))
    -> IO ()
printResumeHint progName = \case
    Nothing -> pure ()
    Just slotRef -> do
        slot <- readIORef slotRef
        case slot of
            Left _ -> pure ()
            Right handle -> do
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
    -> Maybe Text
    -> Maybe PendingTurn
    -> [Provider]
    -> IORef ResponseCreateParams
    -> IORef [ResponseItem]
    -> Maybe Text
    -> Maybe (IORef (Either SessionCreate SessionHandle))
    -> OsPath
    -> OsPath
    -> OsPath
    -> Maybe TokenProvider
    -> Maybe OpenAI.Pool
    -> IORef (Maybe Text)
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
runSession options provider policy tools toolEnv planMode prompt pendingTurn unavailableProviders paramsRef transcriptRef initialPrevious persist projectRoot home cwd tokenProvider openAiPool agentsContext escPaused interrupt multiCtx rootTurnRef subagentSessions pendingNotices storeRoot usageRef onPersisted backend btwBackend = do
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
    allowedToolsRef <- newIORef Set.empty
    lastAssistantRef <- newIORef Nothing
    modelRef <- newIORef =<< (currentModel <$> readIORef paramsRef)
    unavailableProvidersRef <- newIORef unavailableProviders
    ioLock <- newMVar ()
    previous <- newIORef initialPrevious
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
                transcript <- case Map.lookup agentId sessions of
                    Nothing -> pure ["(" <> formatAgentStatus status <> ")"]
                    Just session ->
                        responseItemLines <$> readIORef session.subSessionTranscript
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
            fresh <- readIORef freshAgents
            writeIORef agentsContext fresh
    policyRef <- newIORef policy
    stderrTty <- hIsTerminalDevice stderr
    terminal <- detectTerminalCapabilities stdout
    reportTerminalCwd terminal stdout (toFilePath cwd)
    useColor <- resolveColor stdout
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
            -- OSC 9;4 is ignored by terminals that do not implement it.
            -- Gate on the same TTY check as the in-pane spinner so pipes
            -- and redirected stderr stay clean.
            , renderNativeProgress =
                stderrTty && terminal.terminalNativeProgress
            }
        config = LoopConfig
            { loopBackend = backend
            , loopHandlers = appToolHandlers tools
            , loopDispatch = defaultLoopDispatch
            , loopMaxTurns = options.optMaxTurns
            , loopOnEvent = renderEvent render
            , loopApprove = \call ->
                withMVar ioLock \_ ->
                    withStdinPaused escPaused $
                        approveToolDecision
                            policyRef allowedToolsRef tools planMode call
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
            , sessionPlanMode = planMode
            , sessionProjectRoot = projectRoot
            , sessionCwd = cwd
            , sessionHome = home
            , sessionTokenProvider = tokenProvider
            , sessionOpenAiPool = openAiPool
            , sessionAgentsContext = agentsContext
            , sessionEscPaused = escPaused
            , sessionAttachments = attachmentsRef
            , sessionPreviewId = previewIdRef
            , sessionInterrupt = interrupt
            , sessionStoreRoot = storeRoot
            , sessionUsage = usageRef
            , sessionLastAssistant = lastAssistantRef
            , sessionTerminal = terminal
            , sessionAgentViewport = Just agentViewport
            , sessionBeginSubagentTurn = beginSubagentTurn
            , sessionFinishSubagentTurn = finishSubagentTurn
            , sessionAbortSubagentTurn = abortSubagentTurn
            , sessionOnPersisted = onPersisted
            , sessionReset = sessionReset
            }
    case pendingTurn of
        Just pending ->
            runPendingTurn env pending
        Nothing -> case prompt of
            Just text -> do
                result <- runOneTurn env text [UserMessage text]
                finishTurn env True result
            Nothing ->
                repl env

runPendingTurn :: SessionEnv -> PendingTurn -> IO RunResult
runPendingTurn env pending = do
    writeIORef env.sessionPlanMode.planStateRef pending.pendingPlanState
    result <- runOneTurn env pending.pendingPromptText pending.pendingInputs
    finishTurn env pending.pendingExitAfter result

finishTurn
    :: SessionEnv
    -> Bool
    -> TurnResult
    -> IO RunResult
finishTurn env exitAfter = \case
    TurnSucceeded -> do
        writeIORef env.sessionUnavailableProviders []
        putTrailingNewline env.sessionPrinted
        if exitAfter
            then pure RunQuit
            else do
                notifyAttention stderr InputRequested
                repl env
    TurnFailed ->
        if exitAfter
            then exitFailure
            else do
                putTrailingNewline env.sessionPrinted
                notifyAttention stderr InputRequested
                repl env
    TurnProviderUnavailable apiError pending ->
        requestAutomaticProviderFallback
            env apiError (setPendingExitAfter exitAfter pending) >>= \case
            Just providerTransition ->
                pure (RunSwitchProvider providerTransition)
            Nothing ->
                if exitAfter
                    then exitFailure
                    else do
                        notifyAttention stderr InputRequested
                        repl env

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
    , sessionAttachments = attachmentsRef
    , sessionPreviewId = previewIdRef
    , sessionInterrupt = interrupt
    , sessionEscPaused = escPaused
    , sessionStoreRoot = storeRoot
    , sessionUsage = usageRef
    , sessionLastAssistant = lastAssistantRef
    , sessionTerminal = terminal
    , sessionAgentViewport = agentViewport
    , sessionReset = sessionReset
    } draft = do
    when terminal.terminalSemanticPrompts $
        emitTerminalSequence terminal stdout osc133PromptStart
    stdoutColor <- resolveColor stdout
    planState <- readIORef planMode.planStateRef
    let planActive = planState == PlanActive
        planPending = planState == PlanPending
    params <- readIORef paramsRef
    policy <- readIORef policyRef
    pendingAttachments <- readIORef attachmentsRef
    let idleMode = replModeFromState planState policy
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
    -- Status sits on the line above λ; the inline editor owns the prompt and
    -- any live completion rows below it.
    -- Token totals sit on the right of that line when the TTY width is known.
    usage <- readIORef usageRef
    withSynchronizedOutput terminal stdout do
        Text.putStrLn $ formatReplStatusLine stdoutColor termCols
            (currentModel params)
            (currentEffort params)
            idleMode
            usage
        hFlush stdout
    -- Solarized user wash under the prompt; the inline editor redraws it.
    -- Ctrl+U keeps the familiar kill-to-start behavior.
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
    mline <- readReplLineWithInitial interrupt chromePrompt draft
    when terminal.terminalSemanticPrompts $
        emitTerminalSequence terminal stdout osc133PromptEnd
    Text.putStr (endBackground stdoutColor)
    hFlush stdout
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
            -- The editor advanced a line; walk back over the previous status
            -- + prompt so the next chrome replaces them.
            putStr "\ESC[2A\r\ESC[J"
            hFlush stdout
            continueWith keptDraft
        ReplClipboardPaste keptDraft -> do
            errColor <- resolveColor stderr
            queueClipboardImages
                attachmentsRef previewIdRef stdoutColor errColor
            continueWith keptDraft
        ReplPasted pasted ->
            submitLine continue stdoutColor True pasted
        ReplText line ->
            submitLine continue stdoutColor False line
  where
    submitLine continue color pasted line =
        let stripped = Text.strip line
        in if Text.null stripped
            then continue
            else do
                when pasted do
                    let chip = formatPasteChip stripped
                    when (chip /= stripped) do
                        Text.putStrLn (roleMuted color chip)
                case parseReplLine line of
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
                                queueAttachedImages
                                    attachmentsRef previewIdRef color images
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
                                result <- runOneTurn env text turnInputs
                                finishTurn env False result
                    ReplPaste{pasteImmediate, pasteCaption} -> do
                        color <- resolveColor stdout
                        errColor <- resolveColor stderr
                        imagesResult <- readClipboardImages
                        case imagesResult of
                            Left err -> do
                                -- Fall back to a richer clipboard sniff for better errors.
                                content <- readClipboard
                                case content of
                                    ClipboardText _ ->
                                        Text.hPutStrLn stderr (roleError errColor
                                            "clipboard has text, not an image (paste text normally into the prompt)")
                                    ClipboardPaths paths ->
                                        Text.hPutStrLn stderr (roleError errColor
                                            ("clipboard has file path(s), but no loadable image: "
                                                <> Text.intercalate ", " (map Text.pack paths)))
                                    ClipboardEmpty ->
                                        Text.hPutStrLn stderr (roleError errColor err)
                                    ClipboardImage image ->
                                        -- Shouldn't happen if readClipboardImages failed, but be safe.
                                        modifyIORef' attachmentsRef (<> [image])
                                continue
                            Right [] -> do
                                Text.hPutStrLn stderr (roleError errColor "no image found on the clipboard")
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
                                        putImagePreview previewIdRef color images
                                        Text.putStrLn
                                            (roleMuted color (glyphOk <> "pasted " <> sizes))
                                        writeIORef printed False
                                        let turnInputs =
                                                [ UserMultimodal
                                                    { userText = promptText
                                                    , userImages = images
                                                    }
                                                ]
                                        result <- runOneTurn env promptText turnInputs
                                        finishTurn env False result
                                    else do
                                        queueAttachedImages
                                            attachmentsRef previewIdRef color images
                                        continue
                    ReplShowAttachments -> do
                        pending <- readIORef attachmentsRef
                        color <- resolveColor stdout
                        if null pending
                            then Text.putStrLn (roleMuted color (glyphSession <> "attachments: (none)"))
                            else Text.putStrLn $ roleMuted color $
                                glyphSession
                                    <> "attachments: "
                                    <> Text.intercalate ", "
                                        [ img.imageMime <> " (" <> formatImageSize (BS.length img.imageBytes) <> ")"
                                        | img <- pending
                                        ]
                        continue
                    ReplClearAttachments -> do
                        writeIORef attachmentsRef []
                        color <- resolveColor stdout
                        Text.putStrLn (roleMuted color (glyphOk <> "attachments cleared"))
                        continue
                    ReplAgents -> do
                        case agentViewport of
                            Nothing -> continue
                            Just viewport -> do
                                entries <- viewport.viewportEntries
                                selected <- readIORef viewport.viewportSelected
                                color <- resolveColor stderr
                                pickAgentViewport color selected entries >>= \case
                                    Nothing -> pure ()
                                    Just target ->
                                        writeIORef viewport.viewportSelected target
                                continue

                    ReplCopyLast -> do
                        answer <- readIORef lastAssistantRef
                        maybe (copyMissing "no assistant response to copy")
                            (copyValue terminal "last response") answer
                        continue
                    ReplCopyCode index -> do
                        answer <- readIORef lastAssistantRef
                        case answer >>= fencedCodeBlock index of
                            Nothing -> copyMissing
                                ("code block " <> Text.pack (show index) <> " was not found")
                            Just block -> copyValue terminal
                                ("code block " <> Text.pack (show index)) block
                        continue
                    ReplCopyDiff -> do
                        answer <- readIORef lastAssistantRef
                        case answer >>= lastDiffBlock of
                            Nothing -> copyMissing "no diff block was found"
                            Just block -> copyValue terminal "diff block" block
                        continue
                    ReplCopyPath -> do
                        copyValue terminal "worktree path" (toText cwd)
                        continue
                    ReplCopySession -> do
                        sessionId <- currentSessionId persist
                        maybe
                            (copyMissing "this session has no persisted id yet")
                            (copyValue terminal "session id")
                            sessionId
                        continue
                    ReplShowTerminal -> do
                        Text.putStrLn
                            (roleMuted color
                                (formatTerminalCapabilities terminal))
                        continue
                    ReplShowEffort -> do
                        color <- resolveColor stdout
                        params <- readIORef paramsRef
                        Text.putStrLn (roleMuted color (glyphSession <> "effort: " <> currentEffort params))
                        continue
                    ReplSetEffort level -> do
                        color <- resolveColor stdout
                        modifyIORef' paramsRef (setReasoningEffort level)
                        Text.putStrLn (roleMuted color (glyphOk <> "effort set to " <> level))
                        case persist of
                            Nothing -> pure ()
                            Just slotRef -> do
                                slot <- readIORef slotRef
                                case slot of
                                    Left pending ->
                                        writeIORef slotRef
                                            (Left pending { createEffort = level })
                                    Right handle -> do
                                        let meta = handle.sessionMeta { metaEffort = level }
                                        writeSessionMeta handle.sessionMetaPath meta
                                        writeIORef slotRef
                                            (Right handle { sessionMeta = meta })
                        continue
                    ReplShowModel -> do
                        color <- resolveColor stderr
                        params <- readIORef paramsRef
                        let current = currentModel params
                        pickModel color provider current >>= \case
                            Nothing -> continue
                            Just choice
                                | choice.modelProvider == provider
                                , choice.modelId == current -> do
                                    Text.putStrLn
                                        (roleMuted color
                                            (glyphSession
                                                <> "model: "
                                                <> providerSlug provider
                                                <> "/"
                                                <> choice.modelId))
                                    continue
                                | choice.modelProvider == provider -> do
                                    applyModelChange
                                        provider choice.modelId paramsRef render previous persist
                                    continue
                                | otherwise ->
                                    requestModelProviderSwitch choice persist >>= \case
                                        Left err -> do
                                            Text.hPutStrLn stderr (roleError color err)
                                            continue
                                        Right result -> pure result
                    ReplSetModel name -> do
                        applyModelChange
                            provider name paramsRef render previous persist
                        continue
                    ReplToggleAlwaysApprove -> do
                        toggleAlwaysApprove policyRef projectRoot
                        continue
                    ReplCompact focus -> do
                        color <- resolveColor stderr
                        result <- runProviderCompact provider tokenProvider paramsRef transcriptRef focus
                        case result of
                            Left err -> do
                                Text.hPutStrLn stderr (roleError color err)
                                continue
                            Right outcome -> do
                                writeIORef transcriptRef outcome.compactHistory
                                writeIORef previous Nothing
                                Text.hPutStrLn stderr $ roleMuted color $
                                    glyphSession
                                        <> "compacted "
                                        <> Text.pack (show outcome.compactBeforeTokens)
                                        <> " → "
                                        <> Text.pack (show outcome.compactAfterTokens)
                                        <> " tokens ("
                                        <> Text.pack (show (length outcome.compactHistory))
                                        <> " items)"
                                case persist of
                                    Nothing -> pure ()
                                    Just slotRef -> do
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
                                        writeIORef slotRef (Right handle')
                                        writeSessionMeta handle'.sessionMetaPath $
                                            handle'.sessionMeta
                                                { metaLastResponseId = Nothing
                                                , metaUpdatedAt = now
                                                }
                                continue
                    ReplPlan maybeDescription ->
                        enterPlanFromSlash env maybeDescription >>= \case
                            Just providerSwitch ->
                                pure (RunSwitchProvider providerSwitch)
                            Nothing -> continue
                    ReplBtw question -> do
                        color <- resolveColor stdout
                        putTextLn stdout
                            (roleMuted color (glyphSession <> "btw · asking…"))
                        result <- runBtwWithCancel
                            (\cancel action ->
                                withTurnCancel interrupt cancel $
                                    withEscCancel cancel escPaused action)
                            btwBackend
                            paramsRef
                            transcriptRef
                            question
                        case result of
                            Left err -> do
                                errorColor <- resolveColor stderr
                                putTextLn stderr
                                    (roleError errorColor (formatBtwError err))
                            Right answer ->
                                putTextLn stdout (renderAssistantText color answer)
                        continue
                    ReplResume maybeId -> do
                        handleResume maybeId persist >>= \case
                            Nothing -> continue
                            Just result -> pure result
                    ReplClear -> do
                        sessionReset
                        color <- resolveColor stderr
                        case persist of
                            Nothing ->
                                Text.hPutStrLn stderr
                                    (roleMuted color (glyphOk <> "conversation cleared"))
                            Just slotRef -> do
                                now <- getCurrentTime
                                slot <- readIORef slotRef
                                case slot of
                                    Left _ ->
                                        Text.hPutStrLn stderr
                                            (roleMuted color (glyphOk <> "conversation cleared"))
                                    Right handle -> do
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
                                            (Right handle'{sessionMeta = meta})
                                        Text.hPutStrLn stderr
                                            (roleMuted color
                                                (glyphOk
                                                    <> "conversation cleared (session "
                                                    <> meta.metaId
                                                    <> ")"))
                        continue
                    ReplNew -> do
                        sessionReset
                        color <- resolveColor stderr
                        case persist of
                            Nothing -> do
                                Text.hPutStrLn stderr
                                    (roleMuted color
                                        (glyphOk <> "started a fresh conversation"))
                                continue
                            Just slotRef -> do
                                now <- getCurrentTime
                                params <- readIORef paramsRef
                                slot <- readIORef slotRef
                                let model = currentModel params
                                    effort = currentEffort params
                                    create = case slot of
                                        Left pending ->
                                            pending
                                                { createModel = model
                                                , createEffort = effort
                                                , createTitleHint = Nothing
                                                }
                                        Right handle ->
                                            SessionCreate
                                                { createRoot =
                                                    takeDirectory handle.sessionDir
                                                , createProvider = provider
                                                , createModel = model
                                                , createCwd =
                                                    handle.sessionMeta.metaCwd
                                                , createEffort = effort
                                                , createTitleHint = Nothing
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
                                writeIORef slotRef
                                    (Right handle'{sessionMeta = meta})
                                writeIORef planMode.planSessionDir
                                    (Just handle'.sessionDir)
                                writeIORef storeRoot (Just handle'.sessionDir)
                                tty <- hIsTerminalDevice stdout
                                setCliWindowTitle tty stdout
                                    (cliWindowTitle meta.metaCwd
                                        (Just meta.metaTitle))
                                Text.hPutStrLn stderr
                                    (roleMuted color
                                        (glyphOk <> "new session: " <> meta.metaId))
                                continue
                    ReplShowSession -> do
                        color <- resolveColor stdout
                        case persist of
                            Nothing ->
                                Text.putStrLn (roleMuted color "session: (not persisted)")
                            Just slotRef -> do
                                slot <- readIORef slotRef
                                case slot of
                                    Left _ ->
                                        Text.putStrLn
                                            (roleMuted color
                                                "session: (pending until first turn)")
                                    Right handle ->
                                        Text.putStrLn
                                            (roleMuted color
                                                (glyphSession <> "session: " <> handle.sessionMeta.metaId))
                        continue
                    ReplLogin -> do
                        color <- resolveColor stderr
                        runLoginManager color
                        continue
                    ReplUsage -> do
                        showAccountUsage provider tokenProvider openAiPool
                        continue
                    ReplReloadAuth -> do
                        reloadAuth provider tokenProvider
                        continue
                    ReplHelp maybeName -> do
                        color <- resolveColor stdout
                        Text.putStrLn (formatSlashHelp color maybeName)
                        continue
                    ReplCommandError err -> do
                        color <- resolveColor stderr
                        Text.hPutStrLn stderr (roleError color err)
                        continue
    continue = continueWith ""
    continueWith keptDraft =
        replWithDraft env keptDraft

showAccountUsage
    :: Provider
    -> Maybe TokenProvider
    -> Maybe OpenAI.Pool
    -> IO ()
showAccountUsage provider tokenProvider openAiPool = do
    color <- resolveColor stdout
    now <- getCurrentTime
    case provider of
        OpenAIProvider ->
            case openAiPool of
                Just pool -> do
                    snapshots <- OpenAI.snapshotAccounts pool
                    lines_ <- mapM fetchSnapshot snapshots
                    Text.putStrLn (formatUsageReport color now lines_)
                Nothing ->
                    case tokenProvider of
                        Just provider_ ->
                            getNextToken provider_ Nothing >>= \case
                                Left err ->
                                    Text.putStrLn
                                        (roleError color
                                            ("usage: " <> Text.pack (show err)))
                                Right credential -> do
                                    result <- fetchUsage
                                        credential.accessToken credential.accountId
                                    Text.putStrLn $
                                        formatUsageReport color now
                                            [ AccountUsageLine
                                                { usageAccountId = credential.accountId
                                                , usageCooldownUntil = Nothing
                                                , usageResult = result
                                                }
                                            ]
                        Nothing ->
                            Text.putStrLn
                                (roleMuted color "usage: no OpenAI credentials loaded")
        _ ->
            Text.putStrLn $
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
copyMissing :: Text -> IO ()
copyMissing message = do
    color <- resolveColor stderr
    Text.hPutStrLn stderr (roleError color message)

copyValue :: TerminalCapabilities -> Text -> Text -> IO ()
copyValue terminal label payload = do
    copied <- copyTerminalClipboard terminal stdout payload
    color <- resolveColor stderr
    Text.hPutStrLn stderr $
        if copied
            then roleSuccess color (glyphOk <> "copied " <> label)
            else roleError color "terminal clipboard is unavailable"
applyModelChange
    :: Provider
    -> Text
    -> IORef ResponseCreateParams
    -> RenderConfig
    -> IORef (Maybe Text)
    -> Maybe (IORef (Either SessionCreate SessionHandle))
    -> IO ()
applyModelChange provider name paramsRef render previous persist = do
    color <- resolveColor stdout
    modifyIORef' paramsRef (setModel name)
    writeIORef render.renderModelRef name
    clearedChain <- case provider of
        OpenAIProvider ->
            atomicModifyIORef' previous \prev ->
                (Nothing, isJust prev)
        _ -> pure False
    if clearedChain
        then Text.putStrLn
            (roleMuted color
                (glyphOk <> "model set to " <> name
                    <> " (conversation continued locally)"))
        else Text.putStrLn
            (roleMuted color (glyphOk <> "model set to " <> name))
    case persist of
        Nothing -> pure ()
        Just slotRef -> do
            slot <- readIORef slotRef
            case slot of
                Left pending ->
                    writeIORef slotRef
                        (Left pending { createModel = name })
                Right handle -> do
                    let meta = handle.sessionMeta { metaModel = name }
                    writeSessionMeta handle.sessionMetaPath meta
                    writeIORef slotRef
                        (Right handle { sessionMeta = meta })

requestModelProviderSwitch
    :: ModelOption
    -> Maybe (IORef (Either SessionCreate SessionHandle))
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
        [] -> do
            color <- resolveColor stderr
            putTextLn stderr $ roleError color $
                "provider unavailable; no other configured provider account is available: "
                    <> Text.pack (show apiError)
            pure Nothing
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
                            <> " unavailable; switching to "
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
    -> Maybe (IORef (Either SessionCreate SessionHandle))
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
        Right loaded
            | loaded.loadedProvider /= choice.modelProvider ->
                pure $ Left $
                    "cannot switch to "
                        <> providerSlug choice.modelProvider
                        <> ": auth resolved "
                        <> providerSlug loaded.loadedProvider
            | otherwise -> pure (Right ())

ensureTransitionSessionId
    :: Maybe (IORef (Either SessionCreate SessionHandle))
    -> IO (Maybe Text)
ensureTransitionSessionId Nothing = pure Nothing
ensureTransitionSessionId (Just slotRef) = do
    handle <- ensureSession slotRef
    pure (Just handle.sessionMeta.metaId)

commitProviderTransition
    :: Maybe ProviderTransition
    -> Maybe (IORef (Either SessionCreate SessionHandle))
    -> IO ()
commitProviderTransition Nothing _ = pure ()
commitProviderTransition _ Nothing = pure ()
commitProviderTransition (Just transition) (Just slotRef) = do
    slot <- readIORef slotRef
    case slot of
        Left pending ->
            writeIORef slotRef $ Left pending
                { createProvider = transition.transitionTarget.modelProvider
                , createModel = transition.transitionTarget.modelId
                }
        Right handle -> do
            now <- getCurrentTime
            let meta = handle.sessionMeta
                    { metaProvider = transition.transitionTarget.modelProvider
                    , metaModel = transition.transitionTarget.modelId
                    , metaLastResponseId = Nothing
                    , metaUpdatedAt = now
                    }
            writeSessionMeta handle.sessionMetaPath meta
            writeIORef slotRef (Right handle { sessionMeta = meta })

prepareTransitionBackend
    :: Maybe ProviderTransition
    -> Maybe (IORef (Either SessionCreate SessionHandle))
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
    -> Maybe (IORef (Either SessionCreate SessionHandle))
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

reloadAuth :: Provider -> Maybe TokenProvider -> IO ()
reloadAuth provider = \case
    Nothing -> do
        color <- resolveColor stderr
        putTextLn stderr $ roleMuted color $
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
            Left err -> do
                color <- resolveColor stderr
                putTextLn stderr $ roleError color $
                    "reload-auth failed: " <> Text.pack (show err)
            Right credential -> do
                color <- resolveColor stdout
                Text.putStrLn $ roleSuccess color $
                    "auth reloaded ("
                        <> providerSlug provider
                        <> " account "
                        <> credential.accountId
                        <> ")"


requestReload
    :: Maybe (IORef (Either SessionCreate SessionHandle))
    -> IO RunResult
requestReload persist = do
    home <- getHomeDirectory
    color <- resolveColor stderr
    case persist of
        Nothing -> do
            putTextLn stderr
                (roleError color ":reload needs a persisted REPL session")
            pure RunQuit
        Just slotRef -> do
            handle <- ensureSession slotRef
            writeDevResumePointer home handle.sessionMeta.metaId
            putTextLn stderr
                (roleMuted color
                    (glyphSession <> "reloading; session " <> handle.sessionMeta.metaId))
            pure RunReload

enterPlanFromSlash :: SessionEnv -> Maybe Text -> IO (Maybe ProviderTransition)
enterPlanFromSlash env@SessionEnv{sessionPlanMode = planMode, sessionPersist = persist, sessionPrinted = printed} maybeDescription = do
    discardStore <- newIORef Nothing
    color <- resolveColor stderr
    case persist of
        Just slotRef -> do
            handle <- ensureSession slotRef
            writeIORef planMode.planSessionDir (Just handle.sessionDir)
            putTextLn stderr
                (roleMuted color
                    (glyphSession <> "session: " <> handle.sessionMeta.metaId))
        Nothing -> pure ()
    case maybeDescription of
        Nothing -> do
            writeIORef planMode.planStateRef PlanPending
            putTextLn stderr
                (roleMuted color
                    (glyphSession
                        <> "plan mode armed; send a prompt to activate (or /plan <description>)"))
            pure Nothing
        Just description -> do
            activatePlanMode planMode
            path <- planFilePath planMode
            putTextLn stderr
                (roleMuted color (glyphSession <> "plan mode on (" <> toText path <> ")"))
            writeIORef printed False
            let planEnv = env { sessionStoreRoot = discardStore }
                inputs = [UserMessage description]
            result <- runOneTurn planEnv description inputs
            case result of
                TurnProviderUnavailable apiError pending ->
                    requestAutomaticProviderFallback
                        planEnv apiError pending >>= \case
                            Nothing -> pure Nothing
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

-- | Drop Ghostty / Windows Terminal native progress (OSC 9;4) on stderr.
-- Safe when the bar was never shown; unknown terminals ignore the sequence.
clearNativeProgress :: Handle -> IO ()
clearNativeProgress handle = do
    tty <- hIsTerminalDevice handle
    when tty do
        inTmux <- isJust <$> lookupEnv "TMUX"
        Text.hPutStr handle (wrapOscForTmux inTmux osc9ProgressRemove)
        hFlush handle

-- | Queue clipboard / Finder-paste images and draw an in-terminal thumbnail
-- (Kitty graphics or iTerm2 OSC 1337, matching Grok Build).
queueAttachedImages
    :: IORef [ImageAttachment]
    -> IORef Int
    -> Bool
    -> [ImageAttachment]
    -> IO ()
queueAttachedImages attachmentsRef previewIdRef color images = do
    modifyIORef' attachmentsRef (<> images)
    pending <- readIORef attachmentsRef
    let sizes =
            Text.intercalate ", "
                [ img.imageMime <> " (" <> formatImageSize (BS.length img.imageBytes) <> ")"
                | img <- images
                ]
    putImagePreview previewIdRef color images
    Text.putStrLn
        (roleMuted color
            (glyphOk
                <> "attached "
                <> sizes
                <> " — send with next message ("
                <> Text.pack (show (length pending))
                <> " queued)"))

queueClipboardImages
    :: IORef [ImageAttachment]
    -> IORef Int
    -> Bool
    -> Bool
    -> IO ()
queueClipboardImages attachmentsRef previewIdRef color errColor = do
    imagesResult <- readClipboardImages
    case imagesResult of
        Right images@(_:_) ->
            queueAttachedImages attachmentsRef previewIdRef color images
        Right [] ->
            Text.hPutStrLn stderr
                (roleError errColor "no image found on the clipboard")
        Left err -> reportClipboardImageError err
  where
    reportClipboardImageError err =
        readClipboard >>= \case
            ClipboardText _ ->
                Text.hPutStrLn stderr
                    (roleError errColor
                        "clipboard has text, not an image (paste text normally into the prompt)")
            ClipboardPaths paths ->
                Text.hPutStrLn stderr
                    (roleError errColor
                        ("clipboard has file path(s), but no loadable image: "
                            <> Text.intercalate ", " (map Text.pack paths)))
            ClipboardEmpty ->
                Text.hPutStrLn stderr (roleError errColor err)
            ClipboardImage image ->
                queueAttachedImages attachmentsRef previewIdRef color [image]

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
    :: Maybe Text
    -> Maybe (IORef (Either SessionCreate SessionHandle))
    -> IO (Maybe RunResult)
handleResume maybeId persist = do
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
                            Text.hPutStrLn stderr (roleError color (Text.pack err))
                            pure Nothing
                        Right _ -> pure (Just (RunResumeSession sessionId))
    case maybeId of
        Just sessionId -> resume sessionId
        Nothing -> do
            sessions <- listSessions root
            pickResumeSession color root sessions >>= \case
                Nothing -> pure Nothing
                Just sessionId -> resume sessionId

currentSessionId
    :: Maybe (IORef (Either SessionCreate SessionHandle))
    -> IO (Maybe Text)
currentSessionId = \case
    Nothing -> pure Nothing
    Just slotRef -> do
        slot <- readIORef slotRef
        pure $ case slot of
            Left _ -> Nothing
            Right handle -> Just handle.sessionMeta.metaId

data SubagentSession = SubagentSession
    { subSessionTranscript :: !(IORef [ResponseItem])
    }

-- | Optional on-disk root for child transcripts (@sessionDir/agents/<id>@).
type SubagentStoreRoot = IORef (Maybe OsPath)

-- | Prefer an explicit store root; otherwise fall back to planMode's session dir.
syncStoreRootFromPlan :: SubagentStoreRoot -> PlanModeEnv -> IO ()
syncStoreRootFromPlan storeRootRef planMode = do
    mroot <- readIORef storeRootRef
    case mroot of
        Just _ -> pure ()
        Nothing -> do
            sessionDir <- readIORef planMode.planSessionDir
            case sessionDir of
                Just dir -> writeIORef storeRootRef (Just dir)
                Nothing -> pure ()

persistSubagentSnapshot
    :: SubagentStoreRoot
    -> SubagentRegistry
    -> GrokSubagentSpecs
    -> SubagentId
    -> IORef [ResponseItem]
    -> IO ()
persistSubagentSnapshot storeRootRef registry typesRef agentId transcriptRef = do
    mroot <- readIORef storeRootRef
    case mroot of
        Nothing -> pure ()
        Just sessionDir -> do
            items <- readIORef transcriptRef
            previous <- getPreviousResponseId registry agentId
            agentType <- lookupAgentType typesRef agentId
            agentModel <- lookupAgentModel typesRef agentId
            agentCwd <- getSubagentCwd registry agentId
            _ <- saveSubagentState
                sessionDir agentId items previous agentType agentModel agentCwd
            pure ()

flushAllSubagentSnapshots
    :: SubagentStoreRoot
    -> SubagentRegistry
    -> IORef (Map SubagentId SubagentSession)
    -> GrokSubagentSpecs
    -> IO ()
flushAllSubagentSnapshots storeRootRef registry sessionsRef typesRef = do
    sessions <- readIORef sessionsRef
    mapM_
        (\(agentId, session) ->
            persistSubagentSnapshot storeRootRef registry typesRef agentId
                session.subSessionTranscript)
        (Map.toList sessions)

-- | Rehydrate a closed/missing agent from @sessionDir/agents/<id>@ so
-- 'resume_agent' / 'resume_from' can continue the prior transcript.
restoreAgentFromDisk
    :: SubagentStoreRoot
    -> SubagentRegistry
    -> IORef (Map SubagentId SubagentSession)
    -> GrokSubagentSpecs
    -> SubagentId
    -> IO (Either Text ())
restoreAgentFromDisk storeRootRef registry sessionsRef typesRef agentId = do
    status <- getStatus registry agentId
    case status of
        NotFound -> restore
        Closed -> restore
        _ -> pure (Right ())
  where
    restore = do
        mroot <- readIORef storeRootRef
        case mroot of
            Nothing ->
                pure (Left "no session directory; cannot restore subagent from disk")
            Just sessionDir ->
                loadSubagentState sessionDir agentId >>= \case
                    Left err -> pure (Left err)
                    Right Nothing ->
                        -- Same-process close with no disk yet: still reopen.
                        reopenInMemory Nothing Nothing
                    Right (Just (items, meta)) -> do
                        result <- reopenInMemory meta.diskPreviousResponseId meta.diskCwd
                        case result of
                            Left err -> pure (Left err)
                            Right () -> do
                                case meta.diskAgentType of
                                    Just agentType ->
                                        recordAgentSpec typesRef agentId GrokSubagentSpec
                                            { agentType
                                            , modelOverride = meta.diskAgentModel
                                            }
                                    Nothing -> pure ()
                                transcript <- newIORef items
                                let session =
                                        SubagentSession
                                            { subSessionTranscript = transcript
                                            }
                                atomicModifyIORef' sessionsRef \m ->
                                    (Map.insert agentId session m, ())
                                pure (Right ())
    reopenInMemory previous requestedCwd = do
        restored <- case requestedCwd of
            Just childCwd ->
                restoreSubagentWithCwd
                    registry childCwd agentId Nothing 1 Nothing previous
            Nothing ->
                restoreSubagent registry agentId Nothing 1 Nothing previous
        pure $ case restored of
            Left err -> Left err
            Right _ -> Right ()

-- | Serialize OpenAI WebSocket turns: parent and children share one connection,
-- and 'receiveWsResponse' is not multiplexed.
lockedOpenAiBackend
    :: MVar ()
    -> TokenProvider
    -> IORef Bool
    -> CodexConn
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> Backend
lockedOpenAiBackend wsLock provider connectionHealthy conn getParams transcript =
    let Backend submit =
            openAiBackendReconnecting provider connectionHealthy conn getParams transcript
    in Backend \previous inputs onEvent ->
        withMVar wsLock \_ -> submit previous inputs onEvent

-- | Use a disposable WebSocket for side questions so cancellation cannot
-- leave abandoned response frames queued on the main conversation connection.
freshOpenAiBackend
    :: TokenProvider
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> Backend
freshOpenAiBackend provider getParams transcript = Backend \previous inputs onEvent ->
    withCodexWsRetrying provider \conn _credential ->
        let Backend submit = openAiBackend conn getParams transcript
        in submit previous inputs onEvent
-- | Child Codex agent: per-agent transcript (retained across send_input), same
-- WS (locked), nested multi-agent tools.
runCodexSubagent
    :: CliOptions
    -> ApprovalPolicy
    -> PlanModeHooks
    -> IORef ResponseCreateParams
    -> MVar ()
    -> TokenProvider
    -> IORef Bool
    -> CodexConn
    -> SubagentRegistry
    -> IORef (Map SubagentId SubagentSession)
    -> SubagentStoreRoot
    -> GrokSubagentSpecs
    -> Maybe (InterAgentMessage -> IO (Either Text Text))
    -> RunSubagent
runCodexSubagent options policy planHooks paramsRef wsLock tokenProvider connectionHealthy conn registry sessionsRef storeRootRef typesRef sendToRoot =
    \env previous prompt onEvent -> do
        parentParams <- readIORef paramsRef
        childEnv <- defaultToolEnv env.subCwd
        childPath <- fromMaybe taskPathRoot <$> getTaskPath registry env.subId
        -- Inherit soft-cancel from the registry-owned child flag.
        let childToolEnv = childEnv { toolCancel = env.subCancel }
            childCtx = MultiAgentContext
                { multiRegistry = registry
                , multiSelfId = Just env.subId
                , multiDepth = env.subDepth
                , multiTaskPath = childPath
                , multiRootTurnId = pure env.subRootTurnId
                , multiResumeFromDisk = Nothing
                , multiCreateWorktree = Nothing
                , multiSendToRoot = sendToRoot
                }
        -- Child tools create their own PlanModeEnv; sync store root from parent
        -- params is handled by noteSessionDir on the parent path. If the parent
        -- already has a session dir in storeRootRef, we persist; otherwise skip.
        session <- lookupOrCreateSubagentSession sessionsRef storeRootRef typesRef env.subId
        coding <- codingToolsFor OpenAIProvider childToolEnv (Just planHooks) (Just childCtx)
        syncStoreRootFromPlan storeRootRef coding.codingPlanMode
        flip finally coding.codingClose do
            today <- utctDay <$> getCurrentTime
            let model = fromMaybe (defaultModelFor OpenAIProvider) parentParams.model
                effort = case parentParams.reasoning of
                    Just cfg -> fromMaybe (defaultEffortFor OpenAIProvider) cfg.effort
                    Nothing -> defaultEffortFor OpenAIProvider
                baseInstructions =
                    fromMaybe
                        (systemPrompt OpenAIProvider env.subCwd today True)
                        parentParams.instructions
                instructions =
                    baseInstructions
                        <> "\n\nYou are a Codex subagent. Complete the assigned task and "
                        <> "report results clearly. Your agent id is "
                        <> env.subId.unSubagentId
                        <> "."
                tools = coding.codingAppTools
                childParams = requestParams model instructions
                    (schemasFromAppTools OpenAIProvider tools) effort
            childParamsRef <- newIORef childParams
            let backend =
                    lockedOpenAiBackend wsLock tokenProvider connectionHealthy conn
                        (readIORef childParamsRef)
                        session.subSessionTranscript
                config = LoopConfig
                    { loopBackend = backend
                    , loopHandlers = appToolHandlers tools
                    , loopDispatch = defaultLoopDispatch
                    , loopMaxTurns = options.optMaxTurns
                    , loopOnEvent = onEvent
                    , loopApprove = \call -> childApprove policy tools call
                    , loopCancel = env.subCancel
                    }
            result <- runLoopInputs config previous [AgentMessage prompt]
            case result of
                Right loopResult ->
                    setPreviousResponseId registry env.subId loopResult.finalResponseId
                Left _ -> pure ()
            persistSubagentSnapshot storeRootRef registry typesRef env.subId
                session.subSessionTranscript
            pure result

-- | Child XAI/OpenRouter agent: HTTP backend, filtered tools by subagent_type.
runHttpSubagent
    :: CliOptions
    -> ApprovalPolicy
    -> PlanModeHooks
    -> IORef ResponseCreateParams
    -> Provider
    -> (IORef ResponseCreateParams -> IORef [ResponseItem] -> Backend)
    -> SubagentRegistry
    -> IORef (Map SubagentId SubagentSession)
    -> GrokSubagentSpecs
    -> SubagentStoreRoot
    -> RunSubagent
runHttpSubagent options policy planHooks paramsRef provider mkBackend registry sessionsRef typesRef storeRootRef =
    \env previous prompt onEvent -> do
        parentParams <- readIORef paramsRef
        childEnv <- defaultToolEnv env.subCwd
        childPath <- fromMaybe taskPathRoot <$> getTaskPath registry env.subId
        let childToolEnv = childEnv { toolCancel = env.subCancel }
            childCtx = MultiAgentContext
                { multiRegistry = registry
                , multiSelfId = Just env.subId
                , multiDepth = env.subDepth
                , multiTaskPath = childPath
                , multiRootTurnId = pure env.subRootTurnId
                , multiResumeFromDisk = Nothing
                , multiCreateWorktree = Nothing
                , multiSendToRoot = Nothing
                }
        session <- lookupOrCreateSubagentSession sessionsRef storeRootRef typesRef env.subId
        agentType <- fromMaybe defaultSubagentType <$> lookupAgentType typesRef env.subId
        childModel <- lookupAgentModel typesRef env.subId
        coding <- codingToolsFor provider childToolEnv (Just planHooks) (Just childCtx)
        flip finally coding.codingClose do
            today <- utctDay <$> getCurrentTime
            let model = fromMaybe
                    (fromMaybe (defaultModelFor provider) parentParams.model)
                    childModel
                effort = case parentParams.reasoning of
                    Just cfg -> fromMaybe (defaultEffortFor provider) cfg.effort
                    Nothing -> defaultEffortFor provider
                baseInstructions = systemPrompt provider env.subCwd today True
                instructions =
                    baseInstructions
                        <> "\n\n"
                        <> grokSubagentSuffix agentType env.subId
                tools = filterChildGrokTools agentType coding.codingAppTools
                childParams = requestParams model instructions
                    (schemasFromAppTools provider tools) effort
            childParamsRef <- newIORef childParams
            let backend = mkBackend childParamsRef session.subSessionTranscript
                config = LoopConfig
                    { loopBackend = backend
                    , loopHandlers = appToolHandlers tools
                    , loopDispatch = defaultLoopDispatch
                    , loopMaxTurns = options.optMaxTurns
                    , loopOnEvent = onEvent
                    , loopApprove = \call -> childApprove policy tools call
                    , loopCancel = env.subCancel
                    }
            -- XAI/OpenRouter ignore previous_response_id and replay local
            -- transcripts; still pass previous for API symmetry.
            result <- runLoop config previous (interAgentMessagePayload prompt)
            case result of
                Right loopResult ->
                    setPreviousResponseId registry env.subId loopResult.finalResponseId
                Left _ -> pure ()
            persistSubagentSnapshot storeRootRef registry typesRef env.subId
                session.subSessionTranscript
            pure result

grokSubagentSuffix :: Text -> SubagentId -> Text
grokSubagentSuffix agentType agentId =
    "You are a Grok Build subagent — a focused worker delegated a specific task.\n\
    \Complete the assigned task directly and efficiently. Do not broaden scope beyond what was asked. \
    \Report blocked or unverified work explicitly.\n\n\
    \Subagent type: "
        <> agentType
        <> "\nAgent id: "
        <> agentId.unSubagentId
        <> case agentType of
            "explore" ->
                "\n\n=== READ-ONLY MODE ===\n\
                \You have no file editing or command execution tools. Search broadly, narrow down, \
                \and return absolute file paths and relevant findings."
            "plan" ->
                "\n\n=== READ-ONLY MODE ===\n\
                \Do not create, modify, or delete implementation files. Explore the codebase, \
                \consider trade-offs, and produce a concrete implementation strategy. End with \
                \a Critical Files for Implementation section listing 3-5 files."
            _ ->
                "\n\nStart broad and narrow down. Check multiple locations and naming conventions. \
                \Never create documentation files unless explicitly requested."

lookupOrCreateSubagentSession
    :: IORef (Map SubagentId SubagentSession)
    -> SubagentStoreRoot
    -> GrokSubagentSpecs
    -> SubagentId
    -> IO SubagentSession
lookupOrCreateSubagentSession sessionsRef storeRootRef typesRef agentId = do
    sessions <- readIORef sessionsRef
    case Map.lookup agentId sessions of
        Just session -> pure session
        Nothing -> do
            mroot <- readIORef storeRootRef
            loaded <- case mroot of
                Just sessionDir -> loadSubagentState sessionDir agentId
                Nothing -> pure (Right Nothing)
            let (items, meta) = case loaded of
                    Right (Just (xs, m)) -> (xs, Just m)
                    _ -> ([], Nothing)
            transcript <- newIORef items
            case meta >>= (.diskAgentType) of
                Just agentType ->
                    recordAgentSpec typesRef agentId GrokSubagentSpec
                        { agentType
                        , modelOverride = meta >>= (.diskAgentModel)
                        }
                Nothing -> pure ()
            let session = SubagentSession { subSessionTranscript = transcript }
            atomicModifyIORef' sessionsRef \m -> (Map.insert agentId session m, ())
            pure session

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
        | otherwise = go (acc <> turn.turnItems) rest

-- | Codex requires @store = false@. Continuation still uses
-- @previous_response_id@, with the local transcript available for recovery.
requestParams
    :: Text
    -> Text
    -> [ResponseTool]
    -> Text
    -> ResponseCreateParams
requestParams modelName instructionText toolSchemas effort =
    case defaultResponseCreateParams of
        ResponseCreateParams{..} ->
            ResponseCreateParams
                { model = Just modelName
                , instructions = Just instructionText
                , tools = Just toolSchemas
                , reasoning = Just ReasoningConfig
                    { context = Nothing
                    , effort = Just effort
                    , generateSummary = Nothing
                    , reasoningMode = Nothing
                    , summary = Nothing
                    , extraFields = KeyMap.empty
                    }
                , store = Just False
                , ..
                }
