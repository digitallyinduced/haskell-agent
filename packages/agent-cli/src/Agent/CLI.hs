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

import Agent.CLI.Auth (LoadedAuth(..), loadAuth)
import Agent.CLI.Approval
    ( approveToolDecision
    , childApprove
    , toggleAlwaysApprove
    )
import Agent.CLI.CancelWatch (withStdinPaused)
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
    , newInterruptState
    , withCtrlCHandler
    )
import Agent.CLI.ModelPicker (pickModel)
import Agent.CLI.Models (ModelOption(..))
import Agent.CLI.Options
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
    , putTextLn
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
import Agent.CLI.Terminal (resolveColor)
import Agent.CLI.Tools (schemasFromAppTools)
import Agent.CLI.Turn (runOneTurn)
import Agent.CLI.Worktree (createWorktree, isUnderWorktreeRoot, worktreeRoot)
import Agent.Loop
import Agent.Error (ApiError)
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
import Agent.OpenAI.LoopBackend (openAiBackendReconnecting)
import Agent.OpenAI.Responses.Types
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
    ( RunSubagent
    , SubagentId(..)
    , SubagentRegistry
    , SubagentSpawnEnv(..)
    , SubagentStatus(..)
    , closeSubagentRegistry
    , resetSubagentRegistry
    , defaultSubagentConfig
    , formatCompletionNotice
    , getPreviousResponseId
    , getStatus
    , getTaskPath
    , newSubagentRegistry
    , restoreSubagent
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
import Agent.Tools.Grok.Task (defaultSubagentType, lookupAgentType, recordAgentType)
import Agent.Subagents.TaskPath (taskPathRoot)
import Agent.Tools.MultiAgents (MultiAgentContext(..))
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
import Agent.XAI.LoopBackend (xaiBackend)
import qualified Agent.XAI.Options as XAI
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (AsyncException(UserInterrupt))
import Control.Exception.Safe (catchAsync, finally, throwIO, try)
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
import System.Directory (getCurrentDirectory, getHomeDirectory, makeAbsolute, setCurrentDirectory)
import System.Environment (getArgs, getProgName, lookupEnv)
import System.FilePath ((</>), takeDirectory)
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
        Right ListSessions -> runListSessions >> pure DevQuit
        Right (ShowSession sessionId) -> runShowSession sessionId >> pure DevQuit
        Right (RunAgent options) -> do
            result <- runAgentWithProviderSwitches options
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
        Right ListSessions -> runListSessions
        Right (ShowSession sessionId) -> runShowSession sessionId
        Right (RunAgent options) -> do
            result <- runAgentWithProviderSwitches options
            case result of
                DevQuit -> pure ()
                DevReload -> do
                    home <- getHomeDirectory
                    clearDevResumePointer home
                    die ":reload is only available under `repl` (nix develop)"

-- | Tear down and rebuild provider-specific auth, tools, prompt, and transport.
-- Automatic transitions carry the exact failed turn in memory and commit
-- persisted provider metadata only after the replacement backend succeeds.
runAgentWithProviderSwitches :: CliOptions -> IO DevResult
runAgentWithProviderSwitches options = go options Nothing
  where
    go current transition =
        runAgent current transition >>= \case
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
                createWorktree source (worktreeRoot home) >>= either die \path -> do
                    color <- resolveColor stderr
                    putTextLn stderr (roleMuted color (glyphSession <> "worktree: " <> Text.pack path))
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
    loaded <- loadAuth requestedProvider >>= either die pure
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
    -- Per-subagent transcripts / previous ids, shared across send_input / task.
    subagentSessions <- newIORef Map.empty
    subagentStoreRoot <- newIORef Nothing
    pendingNotices <- newIORef ([] :: [Text])
    registry <- newSubagentRegistry defaultSubagentConfig cwd
        (\_ _ _ _ -> pure $ Left LoopNoResponseId)
        (\_ _ -> pure ())
    agentTypesRef <- newIORef Map.empty
    let multiCtx = Just MultiAgentContext
            { multiRegistry = registry
            , multiSelfId = Nothing
            , multiDepth = 0
            , multiTaskPath = taskPathRoot
            , multiResumeFromDisk = Just
                (restoreAgentFromDisk subagentStoreRoot registry subagentSessions agentTypesRef)
            }
    coding <- codingToolsForWithTypes provider toolEnv (Just planHooks) multiCtx agentTypesRef
    case multiCtx of
        Just ctx ->
            setSubagentOnComplete ctx.multiRegistry \agentId status -> do
                atomicModifyIORef' pendingNotices \xs ->
                    (xs <> [formatCompletionNotice agentId status], ())
                sessions <- readIORef subagentSessions
                case Map.lookup agentId sessions of
                    Just session ->
                        persistSubagentSnapshot subagentStoreRoot ctx.multiRegistry
                            agentTypesRef agentId session.subSessionTranscript
                    Nothing -> pure ()
        Nothing -> pure ()
    let tools = coding.codingAppTools
        planMode = coding.codingPlanMode
        -- Keep planSessionDir and subagent store root in sync.
        noteSessionDir dir = do
            writeIORef planMode.planSessionDir (Just dir)
            writeIORef subagentStoreRoot (Just dir)
        closeAll = do
            case multiCtx of
                Just ctx -> closeSubagentRegistry ctx.multiRegistry
                Nothing -> pure ()
            coding.codingClose
    flip finally closeAll do
        today <- utctDay <$> getCurrentTime
        let model = fromMaybe
                (case transitionTarget of
                    Just target -> target.modelId
                    Nothing ->
                        maybe (defaultModelFor provider) (.metaModel) (fst <$> resumed))
                options.optModel
            instructions = systemPrompt provider cwd today (isOneShot options)
            effort = fromMaybe
                (maybe (defaultEffortFor provider) (.metaEffort) (fst <$> resumed))
                options.optEffort
            params = requestParams model instructions
                (schemasFromAppTools provider tools) effort
            policy = resolveApprovalPolicy options isTty
                projectSettings.settingsAutoApprove
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
        usageRef <- newIORef $ case resumed of
            Just (meta, turns) -> sessionUsageFromTurns meta turns
            Nothing -> emptyTokenUsage
        case persist of
            Just slotRef -> do
                slot <- readIORef slotRef
                case slot of
                    Right handle ->
                        noteSessionDir handle.sessionDir
                    Left _ -> pure ()
            Nothing -> pure ()
        progName <- getProgName
        withCtrlCHandler interrupt $
            withInterruptResume progName persist do
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
                                        withPendingNotices pendingNotices lockedBackend
                                activeBackend <-
                                    prepareTransitionBackend transition persist noticingBackend
                                runSession options provider policy tools toolEnv planMode prompt pendingTurn unavailableProviders paramsRef transcriptRef
                                    initialPrevious persist projectRoot home cwd (Just loaded.loadedTokenProvider) agentsContext escPaused interrupt
                                    multiCtx subagentSessions pendingNotices subagentStoreRoot usageRef activeBackend)
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
                                withPendingNotices pendingNotices $
                                    xaiBackend xaiOptions loaded.loadedTokenProvider
                                        (readIORef paramsRef) transcriptRef
                        activeBackend <-
                            prepareTransitionBackend transition persist backend
                        runSession options provider policy tools toolEnv planMode prompt pendingTurn unavailableProviders paramsRef transcriptRef
                            initialPrevious persist projectRoot home cwd (Just loaded.loadedTokenProvider) agentsContext escPaused interrupt
                            multiCtx subagentSessions pendingNotices subagentStoreRoot usageRef activeBackend
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
                                withPendingNotices pendingNotices $
                                    openRouterBackend openRouterOptions loaded.loadedTokenProvider
                                        (readIORef paramsRef) transcriptRef
                        activeBackend <-
                            prepareTransitionBackend transition persist backend
                        runSession options provider policy tools toolEnv planMode prompt pendingTurn unavailableProviders paramsRef transcriptRef
                            initialPrevious persist projectRoot home cwd (Just loaded.loadedTokenProvider) agentsContext escPaused interrupt
                            multiCtx subagentSessions pendingNotices subagentStoreRoot usageRef activeBackend

preparePersistence
    :: CliOptions
    -> FilePath
    -> Provider
    -> Text
    -> FilePath
    -> Text
    -> Maybe Text
    -> Maybe (SessionMeta, [SessionTurn])
    -> IO (Maybe (IORef (Either SessionCreate SessionHandle)))
preparePersistence options root provider model cwd effort prompt resumed =
    case resumed of
        Just (meta, _) -> do
            let handle = SessionHandle
                    { sessionDir = root </> Text.unpack meta.metaId
                    , sessionMetaPath = root </> Text.unpack meta.metaId </> "meta.json"
                    , sessionTranscriptPath =
                        root </> Text.unpack meta.metaId </> "transcript.jsonl"
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
    -> IO a
    -> IO a
withInterruptResume progName persist action =
    action `catchAsync` \(e :: AsyncException) ->
        case e of
            UserInterrupt -> do
                printResumeHint progName persist
                throwIO e
            _ -> throwIO e

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
    -> FilePath
    -> FilePath
    -> FilePath
    -> Maybe TokenProvider
    -> IORef (Maybe Text)
    -> IORef Bool
    -> InterruptState
    -> Maybe MultiAgentContext
    -> IORef (Map SubagentId SubagentSession)
    -> IORef [Text]
    -> SubagentStoreRoot
    -> IORef TokenUsage
    -> Backend
    -> IO RunResult
runSession options provider policy tools toolEnv planMode prompt pendingTurn unavailableProviders paramsRef transcriptRef initialPrevious persist projectRoot home cwd tokenProvider agentsContext escPaused interrupt multiCtx subagentSessions pendingNotices storeRoot usageRef backend = do
    printed <- newIORef False
    attachmentsRef <- newIORef []
    previewIdRef <- newIORef (1 :: Int)
    textBuffer <- newIORef ""
    liveActive <- newIORef False
    thinkingVisible <- newIORef False
    spinnerRef <- newIORef Nothing
    reasoningBuffer <- newIORef ""
    activityRef <- newIORef "Thinking…"
    startedAtRef <- newIORef Nothing
    allowedToolsRef <- newIORef Set.empty
    modelRef <- newIORef =<< (currentModel <$> readIORef paramsRef)
    unavailableProvidersRef <- newIORef unavailableProviders
    ioLock <- newMVar ()
    previous <- newIORef initialPrevious
    let sessionReset = do
            resetLiveConversation previous transcriptRef attachmentsRef planMode
            writeIORef usageRef emptyTokenUsage
            writeIORef pendingNotices []
            writeIORef subagentSessions Map.empty
            case multiCtx of
                Just ctx -> resetSubagentRegistry ctx.multiRegistry
                Nothing -> pure ()
            freshAgents <-
                loadAgentsContext options provider home cwd [] Nothing
            fresh <- readIORef freshAgents
            writeIORef agentsContext fresh
    policyRef <- newIORef policy
    stderrTty <- hIsTerminalDevice stderr
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
            , renderNativeProgress = stderrTty
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
        env = SessionEnv
            { sessionLoop = config
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
            , sessionHome = home
            , sessionTokenProvider = tokenProvider
            , sessionAgentsContext = agentsContext
            , sessionEscPaused = escPaused
            , sessionAttachments = attachmentsRef
            , sessionPreviewId = previewIdRef
            , sessionInterrupt = interrupt
            , sessionStoreRoot = storeRoot
            , sessionUsage = usageRef
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
            else repl env
    TurnFailed ->
        if exitAfter
            then exitFailure
            else do
                putTrailingNewline env.sessionPrinted
                repl env
    TurnProviderUnavailable apiError pending ->
        requestAutomaticProviderFallback
            env apiError (setPendingExitAfter exitAfter pending) >>= \case
            Just providerTransition ->
                pure (RunSwitchProvider providerTransition)
            Nothing ->
                if exitAfter
                    then exitFailure
                    else repl env

repl :: SessionEnv -> IO RunResult
repl env = replWithDraft env ""

replWithDraft :: SessionEnv -> Text -> IO RunResult
replWithDraft env@SessionEnv
    { sessionRender = render
    , sessionProvider = provider
    , sessionPrevious = previous
    , sessionPrinted = printed
    , sessionParams = paramsRef
    , sessionPolicy = policyRef
    , sessionTranscript = transcriptRef
    , sessionPersist = persist
    , sessionPlanMode = planMode
    , sessionProjectRoot = projectRoot
    , sessionTokenProvider = tokenProvider
    , sessionAttachments = attachmentsRef
    , sessionPreviewId = previewIdRef
    , sessionInterrupt = interrupt
    , sessionStoreRoot = storeRoot
    , sessionUsage = usageRef
    , sessionReset = sessionReset
    } draft = do
    stdoutColor <- resolveColor stdout
    planState <- readIORef planMode.planStateRef
    let planActive = planState == PlanActive
        planPending = planState == PlanPending
    params <- readIORef paramsRef
    policy <- readIORef policyRef
    let idleMode = replModeFromState planState policy
    -- Status sits on the line above λ so haskeline can keep the cursor on
    -- the input. Haskeline cannot park a persistent footer under the draft.
    -- Token totals sit on the right of that line when the TTY width is known.
    termCols <- fmap snd <$> getTerminalSize
    usage <- readIORef usageRef
    Text.putStrLn $ formatReplStatusLine stdoutColor termCols
        (currentModel params)
        (currentEffort params)
        idleMode
        usage
    hFlush stdout
    -- Solarized user wash under the prompt; haskeline redraws it on edit.
    -- Cmd+Delete / Ctrl+U kill-to-start via haskeline Emacs bindings.
    let modeTag
            | planActive = roleWarn stdoutColor "[plan] "
            | planPending = roleMuted stdoutColor "[plan…] "
            | idleMode == ReplModeAlwaysApprove =
                roleSuccess stdoutColor "[yolo] "
            | otherwise = ""
        chromePrompt =
            beginBackground stdoutColor userBackground
                <> modeTag
                <> rolePrompt stdoutColor "λ "
                <> if stdoutColor
                    then Text.pack clearFromCursorToLineEndCode
                    else mempty
    mline <- readReplLineWithInitial interrupt chromePrompt draft
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
            -- Haskeline already advanced a line; walk back over the
            -- previous status + prompt so the next chrome replaces them.
            putStr "\ESC[2A\r\ESC[J"
            hFlush stdout
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
                case parseReplLine stripped of
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
                    ReplResume maybeId -> do
                        handleResume maybeId persist
                        continue
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
                <> Text.pack err
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
                (roleMuted color (glyphSession <> "plan mode on (" <> Text.pack path <> ")"))
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
    -> FilePath
    -> FilePath
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
    (_, Just path) -> Just . Text.strip <$> Text.readFile path
    _ -> pure Nothing

handleResume
    :: Maybe Text
    -> Maybe (IORef (Either SessionCreate SessionHandle))
    -> IO ()
handleResume maybeId persist = do
    color <- resolveColor stderr
    home <- getHomeDirectory
    prog <- getProgName
    let printHint sessionId =
            Text.hPutStrLn stderr
                (roleMuted color (resumeHint prog sessionId))
    case maybeId of
        Just sessionId -> printHint sessionId
        Nothing -> do
            sessions <- listSessions (sessionsRoot home)
            currentId <- currentSessionId persist
            pickResumeSession color sessions >>= \case
                Nothing -> pure ()
                Just sessionId
                    | Just sessionId == currentId ->
                        Text.hPutStrLn stderr
                            (roleMuted color
                                (glyphSession <> "already on session " <> sessionId))
                    | otherwise -> printHint sessionId

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
type SubagentStoreRoot = IORef (Maybe FilePath)

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
    -> IORef (Map SubagentId Text)
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
            _ <- saveSubagentState sessionDir agentId items previous agentType
            pure ()

flushAllSubagentSnapshots
    :: SubagentStoreRoot
    -> SubagentRegistry
    -> IORef (Map SubagentId SubagentSession)
    -> IORef (Map SubagentId Text)
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
    -> IORef (Map SubagentId Text)
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
                        reopenInMemory Nothing
                    Right (Just (items, meta)) -> do
                        result <- reopenInMemory meta.diskPreviousResponseId
                        case result of
                            Left err -> pure (Left err)
                            Right () -> do
                                case meta.diskAgentType of
                                    Just agentType ->
                                        recordAgentType typesRef agentId agentType
                                    Nothing -> pure ()
                                transcript <- newIORef items
                                let session =
                                        SubagentSession
                                            { subSessionTranscript = transcript
                                            }
                                atomicModifyIORef' sessionsRef \m ->
                                    (Map.insert agentId session m, ())
                                pure (Right ())
    reopenInMemory previous = do
        restored <- restoreSubagent registry agentId Nothing 1 Nothing previous
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

-- | Prepend drained subagent completion notices to the next parent turn.
withPendingNotices :: IORef [Text] -> Backend -> Backend
withPendingNotices pending (Backend submit) = Backend \previous inputs onEvent -> do
    notices <- atomicModifyIORef' pending \xs -> ([], xs)
    let prefixed
            | null notices = inputs
            | otherwise = UserMessage (Text.intercalate "\n\n" notices) : inputs
    submit previous prefixed onEvent

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
    -> IORef (Map SubagentId Text)
    -> RunSubagent
runCodexSubagent options policy planHooks paramsRef wsLock tokenProvider connectionHealthy conn registry sessionsRef storeRootRef typesRef =
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
                , multiResumeFromDisk = Nothing
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
            result <- runLoop config previous prompt
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
    -> IORef (Map SubagentId Text)
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
                , multiResumeFromDisk = Nothing
                }
        session <- lookupOrCreateSubagentSession sessionsRef storeRootRef typesRef env.subId
        agentType <- fromMaybe defaultSubagentType <$> lookupAgentType typesRef env.subId
        coding <- codingToolsFor provider childToolEnv (Just planHooks) (Just childCtx)
        flip finally coding.codingClose do
            today <- utctDay <$> getCurrentTime
            let model = fromMaybe (defaultModelFor provider) parentParams.model
                effort = case parentParams.reasoning of
                    Just cfg -> fromMaybe (defaultEffortFor provider) cfg.effort
                    Nothing -> defaultEffortFor provider
                baseInstructions =
                    fromMaybe
                        (systemPrompt provider env.subCwd today True)
                        parentParams.instructions
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
            result <- runLoop config previous prompt
            persistSubagentSnapshot storeRootRef registry typesRef env.subId
                session.subSessionTranscript
            pure result

grokSubagentSuffix :: Text -> SubagentId -> Text
grokSubagentSuffix agentType agentId =
    "You are a Grok Build subagent (type: "
        <> agentType
        <> "). Your agent id is "
        <> agentId.unSubagentId
        <> ". Complete the assigned task directly and report results clearly."
        <> case agentType of
            "explore" ->
                "\n\n=== READ-ONLY MODE ===\n\
                \Do not create, modify, or delete files. Use run_terminal_cmd only \
                \for read-only commands (ls, git status, git log, git diff, find, cat, head, tail)."
            "plan" ->
                "\n\n=== READ-ONLY MODE ===\n\
                \Do not create, modify, or delete files except plan.md while in plan mode. \
                \Use run_terminal_cmd only for read-only commands."
            _ -> ""

lookupOrCreateSubagentSession
    :: IORef (Map SubagentId SubagentSession)
    -> SubagentStoreRoot
    -> IORef (Map SubagentId Text)
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
                    atomicModifyIORef' typesRef \m ->
                        (Map.insertWith (\_ new -> new) agentId agentType m, ())
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
