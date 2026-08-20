-- | Command-line entry point: one-shot @-p@ or an interactive REPL.
module Agent.CLI
    ( DevResult(..)
    , afterDev
    , devMain
    , run
    ) where

import Agent.CLI.Auth (LoadedAuth(..), loadAuth)
import Agent.CLI.CancelWatch (withEscCancel, withStdinPaused)
import Agent.CLI.Clipboard (formatImageSize, readClipboardImage)
import Agent.CLI.Command
import Agent.CLI.Input (ReplLine(..), readApprovalLine, readReplLine)
import Agent.CLI.Interrupt
    ( InterruptState
    , newInterruptState
    , withCtrlCHandler
    , withTurnCancel
    )
import Agent.CLI.Options
import Agent.CLI.Plan
    ( cliPlanHooks
    , extractProposedPlan
    )
import Agent.CLI.Project
    ( ProjectSettings(..)
    , loadProjectSettings
    , resolveProjectRoot
    , saveProjectAutoApprove
    )
import Agent.CLI.Prompt (defaultModelFor, systemPrompt)
import Agent.CLI.Render
    ( RenderConfig(..)
    , clearThinking
    , formatLoopErrorColored
    , formatTurnStatus
    , putTextLn
    , renderAssistantText
    , renderEvent
    , summarizeToolCall
    )
import Agent.CLI.Session
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
import Agent.CLI.Timestamp (stampTurnInputs, stripBracketedTimestamps)
import Agent.CLI.Tools (lookupAppTool, schemasFromAppTools)
import Agent.CLI.Worktree (createWorktree, isUnderWorktreeRoot, worktreeRoot)
import Agent.Loop
import Agent.ProjectInstructions
    ( DiscoverOptions(..)
    , defaultDiscoverOptions
    , discoverProjectInstructions
    , formatAgentsMdForProvider
    , globalAgentsHomeDir
    , loadedInstructionFiles
    )
import Agent.OpenAI.LoopBackend (openAiBackend, toolResultToItem)
import Agent.OpenAI.Responses.Types
import Agent.OpenAI.WebSocketClient (CodexAuthFailed(..), withCodexWsWithProvider)
import Agent.Provider
    ( AccountFailure(..)
    , Credential(..)
    , FailedCredential(..)
    , Provider(..)
    , TokenProvider
    , getNextToken
    , providerSlug
    )
import Agent.ToolDispatch (ToolCall(..))
import Agent.Tools (CodingTools(..), appToolHandlers, codingToolsFor)
import Agent.Tools.PlanMode
    ( PlanDecision(..)
    , PlanModeEnv(..)
    , PlanModeHooks(..)
    , PlanModeState(..)
    , activatePlanMode
    , deactivatePlanMode
    , isPlanFileEditTarget
    , isPlanModeActive
    , planFilePath
    , planModeBlockedEditMessage
    , planModeReminder
    , writePlanMarkdown
    )
import Agent.Tools.Types (AppTool(..), ToolEnv(..), defaultToolEnv, toolAllowsWithoutPrompt)
import Agent.OpenRouter.LoopBackend (openRouterBackend)
import qualified Agent.OpenRouter.Options as OpenRouter
import Agent.XAI.LoopBackend (xaiBackend)
import qualified Agent.XAI.Options as XAI
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Exception (AsyncException(UserInterrupt))
import Control.Exception.Safe (catchAsync, finally, throwIO, try)
import Control.Monad (unless, when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import Data.IORef
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as Text
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory (getCurrentDirectory, getHomeDirectory, makeAbsolute, setCurrentDirectory)
import System.Environment (getArgs, getProgName, lookupEnv)
import System.FilePath ((</>))
import System.Console.ANSI.Codes (clearFromCursorToLineEndCode)
import System.Exit (die, exitFailure)
import System.IO (Handle, hFlush, hIsTerminalDevice, stderr, stdin, stdout)

-- | How the GHCi-driven agent REPL finished.
data DevResult
    = DevQuit
    | DevReload
    deriving (Eq, Show)

-- | GHCi @:cmd@ helper: on 'DevReload', reload modules and re-enter 'devMain'.
afterDev :: DevResult -> IO String
afterDev = \case
    DevQuit -> pure ""
    DevReload -> pure $ unlines
        [ ":reload"
        , ":module +Agent.CLI"
        , ":cmd afterDev =<< devMain"
        ]

-- | Start the agent from GHCi (@repl@). Resumes the session written by @:reload@.
-- On first open (no resume pointer), passes @--worktree@ unless the cwd is
-- already under @~/.haskell-agent/worktrees@.
devMain :: IO DevResult
devMain = do
    home <- getHomeDirectory
    resumeId <- readDevResumePointer home
    args <- case resumeId of
        Just sessionId -> pure ["--resume", Text.unpack sessionId]
        Nothing -> do
            cwd <- makeAbsolute =<< getCurrentDirectory
            root <- makeAbsolute (worktreeRoot home)
            pure $ if isUnderWorktreeRoot root cwd then [] else ["--worktree"]
    case parseArgs args of
        Left err -> do
            clearDevResumePointer home
            die err
        Right ShowHelp -> putStr usage >> pure DevQuit
        Right ShowVersion -> putStrLn "agent-cli 0.1.0.0" >> pure DevQuit
        Right ListSessions -> runListSessions >> pure DevQuit
        Right (ShowSession sessionId) -> runShowSession sessionId >> pure DevQuit
        Right (RunAgent options) -> do
            result <- runAgent options
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
            result <- runAgent options
            case result of
                DevQuit -> pure ()
                DevReload -> do
                    home <- getHomeDirectory
                    clearDevResumePointer home
                    die ":reload is only available under `repl` (nix develop)"

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

runAgent :: CliOptions -> IO DevResult
runAgent options = do
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
    let requestedProvider = case resumed of
            Just (meta, _) -> Just meta.metaProvider
            Nothing -> options.optProvider
    loaded <- loadAuth requestedProvider >>= either die pure
    case resumed of
        Just (meta, _)
            | loaded.loadedProvider /= meta.metaProvider ->
                die $ "session provider is "
                    <> Text.unpack (providerSlug meta.metaProvider)
                    <> " but auth resolved "
                    <> Text.unpack (providerSlug loaded.loadedProvider)
            | otherwise -> pure ()
        Nothing -> pure ()

    toolEnv <- defaultToolEnv cwd
    interrupt <- newInterruptState \msg -> do
        -- Drop an in-place "thinking…" status so the hint is its own line.
        Text.hPutStr stderr "\r\ESC[K"
        hFlush stderr
        color <- resolveColor stderr
        putTextLn stderr (roleMuted color msg)
    let planHooks = cliPlanHooks interrupt (resolveColor stderr)
    coding <- codingToolsFor loaded.loadedProvider toolEnv (Just planHooks)
    let tools = coding.codingAppTools
        planMode = coding.codingPlanMode
    flip finally coding.codingClose do
        today <- utctDay <$> getCurrentTime
        let provider = loaded.loadedProvider
            model = fromMaybe
                (maybe (defaultModelFor provider) (.metaModel) (fst <$> resumed))
                options.optModel
            instructions = systemPrompt provider cwd today (isOneShot options)
            effort = fromMaybe
                (maybe (defaultEffortFor provider) (.metaEffort) (fst <$> resumed))
                options.optEffort
            params = requestParams provider model instructions
                (schemasFromAppTools provider tools) effort
            policy = resolveApprovalPolicy options isTty
                projectSettings.settingsAutoApprove
            initialItems = maybe [] (concatMap (.turnItems) . snd) resumed
            initialPrevious = resumed >>= \(meta, _) -> meta.metaLastResponseId
        paramsRef <- newIORef params
        transcriptRef <- newIORef initialItems
        prompt <- loadPrompt options
        let titleHint = case resumed of
                Just (meta, _) -> Just meta.metaTitle
                Nothing -> sessionTitleFromPrompt <$> prompt
        setCliWindowTitle stdoutTty stdout (cliWindowTitle cwd titleHint)
        agentsContext <- loadAgentsContext options provider home cwd initialItems initialPrevious

        persist <- preparePersistence options root provider model cwd effort prompt resumed
        case persist of
            Just slotRef -> do
                slot <- readIORef slotRef
                case slot of
                    Right handle ->
                        writeIORef planMode.planSessionDir (Just handle.sessionDir)
                    Left _ -> pure ()
            Nothing -> pure ()
        progName <- getProgName
        withCtrlCHandler interrupt $
            withInterruptResume progName persist do
                case provider of
                    OpenAIProvider ->
                        try @_ @CodexAuthFailed
                            (withCodexWsWithProvider loaded.loadedTokenProvider \conn _credential ->
                                runSession options provider policy tools toolEnv planMode prompt paramsRef transcriptRef
                                    initialPrevious persist projectRoot Nothing agentsContext interrupt
                                    (openAiBackend conn (readIORef paramsRef) transcriptRef))
                            >>= \case
                                Left (CodexAuthFailed err) -> die ("openai auth: " <> show err)
                                Right result -> pure result
                    XAIProvider -> do
                        xaiOptions <- XAI.clientOptionsFromEnv
                        let backend =
                                xaiBackend xaiOptions loaded.loadedTokenProvider
                                    (readIORef paramsRef) transcriptRef
                        runSession options provider policy tools toolEnv planMode prompt paramsRef transcriptRef
                            initialPrevious persist projectRoot (Just loaded.loadedTokenProvider) agentsContext interrupt backend
                    OpenRouterProvider -> do
                        openRouterOptions <- OpenRouter.clientOptionsFromEnv
                        let backend =
                                openRouterBackend openRouterOptions loaded.loadedTokenProvider
                                    (readIORef paramsRef) transcriptRef
                        runSession options provider policy tools toolEnv planMode prompt paramsRef transcriptRef
                            initialPrevious persist projectRoot (Just loaded.loadedTokenProvider) agentsContext interrupt backend

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

isLeftSlot :: Either a b -> Bool
isLeftSlot = \case
    Left _ -> True
    Right _ -> False

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
                -- Drop an in-place "thinking…" status so the hint is its own line.
                Text.hPutStr stderr "\r\ESC[K"
                hFlush stderr
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
    -> IORef ResponseCreateParams
    -> IORef [ResponseItem]
    -> Maybe Text
    -> Maybe (IORef (Either SessionCreate SessionHandle))
    -> FilePath
    -> Maybe TokenProvider
    -> IORef (Maybe Text)
    -> InterruptState
    -> Backend
    -> IO DevResult
runSession options provider policy tools toolEnv planMode prompt paramsRef transcriptRef initialPrevious persist projectRoot tokenProvider agentsContext interrupt backend = do
    printed <- newIORef False
    escPaused <- newIORef False
    textBuffer <- newIORef ""
    thinkingVisible <- newIORef False
    spinnerRef <- newIORef Nothing
    modelRef <- newIORef =<< (currentModel <$> readIORef paramsRef)
    ioLock <- newMVar ()
    previous <- newIORef initialPrevious
    policyRef <- newIORef policy
    stderrTty <- hIsTerminalDevice stderr
    useColor <- resolveColor stdout
    let render = RenderConfig
            { renderShowThinking = stderrTty
            , renderThinkingVisible = thinkingVisible
            , renderThinkingSpinner = spinnerRef
            , renderColor = useColor
            , renderPrintedText = printed
            , renderTextBuffer = textBuffer
            , renderLock = ioLock
            , renderStdout = stdout
            , renderStderr = stderr
            , renderModelRef = modelRef
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
                        approveToolDecision policyRef tools planMode call projectRoot
            , loopCancel = toolEnv.toolCancel
            }
    case prompt of
        Just text -> do
            ok <- runOneTurn config render previous printed transcriptRef persist
                planMode agentsContext escPaused interrupt text [UserMessage text]
            if ok
                then putTrailingNewline printed >> pure DevQuit
                else exitFailure
        Nothing ->
            repl config render provider previous printed paramsRef policyRef
                transcriptRef persist planMode projectRoot tokenProvider agentsContext escPaused interrupt

repl
    :: LoopConfig
    -> RenderConfig
    -> Provider
    -> IORef (Maybe Text)
    -> IORef Bool
    -> IORef ResponseCreateParams
    -> IORef ApprovalPolicy
    -> IORef [ResponseItem]
    -> Maybe (IORef (Either SessionCreate SessionHandle))
    -> PlanModeEnv
    -> FilePath
    -> Maybe TokenProvider
    -> IORef (Maybe Text)
    -> IORef Bool
    -> InterruptState
    -> IO DevResult
repl config render provider previous printed paramsRef policyRef transcriptRef persist planMode projectRoot tokenProvider agentsContext escPaused interrupt = do
    stdoutColor <- resolveColor stdout
    planActive <- isPlanModeActive planMode
    planPending <- (== PlanPending) <$> readIORef planMode.planStateRef
    -- Solarized user wash under the prompt; haskeline redraws it on edit.
    -- Cmd+Delete / Ctrl+U kill-to-start via haskeline Emacs bindings.
    let modeTag
            | planActive = roleWarn stdoutColor "[plan] "
            | planPending = roleMuted stdoutColor "[plan…] "
            | otherwise = ""
        chromePrompt =
            beginBackground stdoutColor userBackground
                <> modeTag
                <> rolePrompt stdoutColor "λ "
                <> if stdoutColor
                    then Text.pack clearFromCursorToLineEndCode
                    else mempty
    mline <- readReplLine interrupt chromePrompt
    Text.putStr (endBackground stdoutColor)
    hFlush stdout
    case mline of
        ReplEof -> do
            putStrLn ""
            pure DevQuit
        ReplQuitInterrupt ->
            -- Confirmed double Ctrl-C: rethrow so withInterruptResume prints
            -- the --resume hint and the process exits.
            throwIO UserInterrupt
        ReplText line ->
            let stripped = Text.strip line
            in if Text.null stripped
                then continue
                else case parseReplLine stripped of
                    ReplQuit -> pure DevQuit
                    ReplReload -> requestReload persist
                    ReplPrompt text -> do
                        writeIORef printed False
                        _ <- runOneTurn config render previous printed
                            transcriptRef persist planMode agentsContext escPaused interrupt text
                            [UserMessage text]
                        putTrailingNewline printed
                        continue
                    ReplPaste caption -> do
                        clipboard <- readClipboardImage
                        case clipboard of
                            Left err -> do
                                color <- resolveColor stderr
                                Text.hPutStrLn stderr (roleError color err)
                                continue
                            Right image -> do
                                let promptText =
                                        if Text.null caption
                                            then "See attached image."
                                            else caption
                                    size = formatImageSize (BS.length image.imageBytes)
                                color <- resolveColor stdout
                                Text.putStrLn
                                    (roleMuted color
                                        (glyphOk <> "pasted " <> image.imageMime <> " (" <> size <> ")"))
                                writeIORef printed False
                                _ <- runOneTurn config render previous printed
                                    transcriptRef persist planMode agentsContext escPaused interrupt promptText
                                    [ UserMultimodal
                                        { userText = promptText
                                        , userImages = [image]
                                        }
                                    ]
                                putTrailingNewline printed
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
                        params <- readIORef paramsRef
                        Text.putStrLn (glyphSession <> "model: " <> currentModel params)
                        continue
                    ReplSetModel name -> do
                        modifyIORef' paramsRef (setModel name)
                        writeIORef render.renderModelRef name
                        clearedChain <- case provider of
                            OpenAIProvider ->
                                atomicModifyIORef' previous \prev ->
                                    (Nothing, isJust prev)
                            _ -> pure False
                        if clearedChain
                            then Text.putStrLn
                                ("model set to " <> name
                                    <> " (conversation continued locally)")
                            else Text.putStrLn ("model set to " <> name)
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
                        continue
                    ReplToggleAlwaysApprove -> do
                        toggleAlwaysApprove policyRef projectRoot
                        continue
                    ReplPlan maybeDescription -> do
                        enterPlanFromSlash planMode persist maybeDescription
                            config render previous printed transcriptRef agentsContext escPaused interrupt
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
                    ReplCommandError err -> do
                        color <- resolveColor stderr
                        Text.hPutStrLn stderr (roleError color err)
                        continue
  where
    continue =
        repl config render provider previous printed paramsRef policyRef
            transcriptRef persist planMode projectRoot tokenProvider agentsContext escPaused interrupt

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
    -> IO DevResult
requestReload persist = do
    home <- getHomeDirectory
    color <- resolveColor stderr
    case persist of
        Nothing -> do
            putTextLn stderr
                (roleError color ":reload needs a persisted REPL session")
            pure DevQuit
        Just slotRef -> do
            handle <- ensureSession slotRef
            writeDevResumePointer home handle.sessionMeta.metaId
            putTextLn stderr
                (roleMuted color
                    (glyphSession <> "reloading; session " <> handle.sessionMeta.metaId))
            pure DevReload

enterPlanFromSlash
    :: PlanModeEnv
    -> Maybe (IORef (Either SessionCreate SessionHandle))
    -> Maybe Text
    -> LoopConfig
    -> RenderConfig
    -> IORef (Maybe Text)
    -> IORef Bool
    -> IORef [ResponseItem]
    -> IORef (Maybe Text)
    -> IORef Bool
    -> InterruptState
    -> IO ()
enterPlanFromSlash planMode persist maybeDescription config render previous printed
    transcriptRef agentsContext escPaused interrupt = do
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
        Just description -> do
            activatePlanMode planMode
            path <- planFilePath planMode
            putTextLn stderr
                (roleMuted color (glyphSession <> "plan mode on (" <> Text.pack path <> ")"))
            writeIORef printed False
            _ <- runOneTurn config render previous printed transcriptRef persist
                planMode agentsContext escPaused interrupt description [UserMessage description]
            putTrailingNewline printed

runOneTurn
    :: LoopConfig
    -> RenderConfig
    -> IORef (Maybe Text)
    -> IORef Bool
    -> IORef [ResponseItem]
    -> Maybe (IORef (Either SessionCreate SessionHandle))
    -> PlanModeEnv
    -> IORef (Maybe Text)
    -> IORef Bool
    -> InterruptState
    -> Text
    -> [TurnInput]
    -> IO Bool
runOneTurn config render previous printed transcriptRef persist planMode agentsContext escPaused interrupt promptText inputs =
  withTurnCancel interrupt config.loopCancel $
  withEscCancel config.loopCancel escPaused do
    pending <- readIORef planMode.planStateRef
    when (pending == PlanPending) (activatePlanMode planMode)
    case persist of
        Just slotRef -> do
            slot <- readIORef slotRef
            case slot of
                Right handle ->
                    writeIORef planMode.planSessionDir (Just handle.sessionDir)
                Left _ -> pure ()
        Nothing -> pure ()
    prev <- readIORef previous
    beforeItems <- readIORef transcriptRef
    pendingAgents <- atomicModifyIORef' agentsContext \pendingCtx -> (Nothing, pendingCtx)
    planActive <- isPlanModeActive planMode
    planPath <- planFilePath planMode
    let planReminder =
            if planActive
                then Just (planModeReminder planPath)
                else Nothing
        baseInputs = case pendingAgents of
            Just agents | null beforeItems && isNothing prev ->
                UserMessage agents : inputs
            _ -> inputs
        turnInputs0 = case planReminder of
            Just reminder -> UserMessage reminder : baseInputs
            Nothing -> baseInputs
    turnInputs <- stampTurnInputs turnInputs0
    result <- runLoopInputs config prev turnInputs
    clearThinking render
    case result of
        Left (LoopCancelled toolResults) -> do
            unless (null toolResults) do
                modifyIORef' transcriptRef
                    (<> map toolResultToItem toolResults)
            color <- resolveColor stderr
            putTextLn stderr (formatLoopErrorColored color (LoopCancelled toolResults))
            model <- readIORef render.renderModelRef
            putTextLn stderr (formatTurnStatus color "cancelled" model)
            pure True
        Left err -> do
            color <- resolveColor stderr
            putTextLn stderr (formatLoopErrorColored color err)
            model <- readIORef render.renderModelRef
            putTextLn stderr (formatTurnStatus color "error" model)
            pure False
        Right loopResult -> do
            writeIORef previous (Just loopResult.finalResponseId)
            do
                color <- resolveColor stderr
                model <- readIORef render.renderModelRef
                let turns = Text.pack (show loopResult.turnsUsed)
                    unit = if loopResult.turnsUsed == 1 then " turn" else " turns"
                putTextLn stderr
                    (formatTurnStatus color "ok" (model <> " · " <> turns <> unit))
            followUp <- handleProposedPlan planMode loopResult.finalText
            printedText <- readIORef printed
            let assistantText =
                    fmap stripBracketedTimestamps loopResult.finalText
            case (printedText, assistantText) of
                (False, Just text) | not (Text.null (Text.strip text)) -> do
                    useColor <- resolveColor stdout
                    putTextLn stdout (renderAssistantText useColor text)
                _ -> pure ()
            afterItems <- readIORef transcriptRef
            let newItems = drop (length beforeItems) afterItems
            case persist of
                Nothing -> pure ()
                Just slotRef -> do
                    now <- getCurrentTime
                    created <- isLeftSlot <$> readIORef slotRef
                    handle <- ensureSession slotRef
                    writeIORef planMode.planSessionDir (Just handle.sessionDir)
                    if created
                        then do
                            color <- resolveColor stderr
                            putTextLn stderr
                                (roleMuted color
                                    (glyphSession <> "session: " <> handle.sessionMeta.metaId))
                        else pure ()
                    let turn = SessionTurn
                            { turnAt = now
                            , turnUserText = promptText
                            , turnAssistantText = assistantText
                            , turnResponseId = Just loopResult.finalResponseId
                            , turnItems = newItems
                            }
                    handle' <- appendTurn handle turn
                    writeIORef slotRef (Right handle')
                    when (handle'.sessionMeta.metaTitle /= handle.sessionMeta.metaTitle) do
                        tty <- hIsTerminalDevice stdout
                        setCliWindowTitle tty stdout
                            (cliWindowTitle handle'.sessionMeta.metaCwd
                                (Just handle'.sessionMeta.metaTitle))
            case followUp of
                Nothing -> pure True
                Just notes -> do
                    writeIORef printed False
                    _ <- runOneTurn config render previous printed transcriptRef persist
                        planMode agentsContext escPaused interrupt notes [UserMessage notes]
                    pure True

handleProposedPlan :: PlanModeEnv -> Maybe Text -> IO (Maybe Text)
handleProposedPlan planMode = \case
    Nothing -> pure Nothing
    Just text -> do
        active <- isPlanModeActive planMode
        case (active, extractProposedPlan text) of
            (True, Just planBody) -> do
                _ <- writePlanMarkdown planMode planBody
                let PlanModeHooks{ planDecideExit = decideExit } = planMode.planHooks
                decision <- decideExit planBody
                case decision of
                    PlanApprove -> do
                        deactivatePlanMode planMode
                        pure Nothing
                    PlanCancel -> do
                        deactivatePlanMode planMode
                        pure Nothing
                    PlanRequestChanges notes ->
                        pure $ Just $
                            "The user requested changes to the plan. Stay in plan mode and revise.\n\
                            \Feedback:\n"
                                <> notes
            _ -> pure Nothing


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

-- | Color when the handle is a TTY and NO_COLOR is unset.
resolveColor :: Handle -> IO Bool
resolveColor handle = do
    isTty <- hIsTerminalDevice handle
    noColor <- lookupEnv "NO_COLOR"
    pure (isTty && maybe True (\_ -> False) noColor)

putTrailingNewline :: IORef Bool -> IO ()
putTrailingNewline printed = do
    didPrint <- readIORef printed
    if didPrint then putStrLn "" else pure ()

loadPrompt :: CliOptions -> IO (Maybe Text)
loadPrompt options = case (options.optPrompt, options.optPromptFile) of
    (Just text, _) -> pure (Just text)
    (_, Just path) -> Just . Text.strip <$> Text.readFile path
    _ -> pure Nothing

approveToolDecision
    :: IORef ApprovalPolicy
    -> [AppTool]
    -> PlanModeEnv
    -> ToolCall
    -> FilePath
    -> IO (Either Text Bool)
approveToolDecision policyRef tools planMode call projectRoot = do
    policy <- readIORef policyRef
    planActive <- isPlanModeActive planMode
    planPath <- planFilePath planMode
    -- Plan mode: reject mutating file edits except plan.md (even under yolo).
    -- Grok search_replace also enforces this in-tool; this covers apply_patch
    -- and any other write tool before dispatch.
    blocked <- planModeBlocksCall planMode planActive planPath call
    if blocked
        then do
            let msg = planModeBlockedEditMessage planPath
            color <- resolveColor stderr
            putTextLn stderr (roleWarn color msg)
            pure (Left msg)
        else do
            readOnly <- case lookupAppTool call.name tools of
                Nothing -> pure False
                Just tool -> toolAllowsWithoutPrompt tool call
            -- plan.md edits are auto-approved while plan mode is active.
            planFileOk <- isPlanFileWrite planMode planActive planPath call
            if planFileOk
                then pure (Right True)
                else case policy of
                    ApproveAll -> pure (Right True)
                    DenyMutating -> pure (Right readOnly)
                    PromptMutating
                        | readOnly -> pure (Right True)
                        | otherwise -> do
                            color <- resolveColor stderr
                            let question =
                                    roleWarn color
                                        (glyphWarn <> "Allow " <> summarizeToolCall call <> "? [y/N/a] ")
                            readApprovalLine question >>= \case
                                Nothing -> pure (Right False)
                                Just raw -> case parseApprovalAnswer raw of
                                    AllowOnce -> pure (Right True)
                                    AllowAlways -> do
                                        writeIORef policyRef ApproveAll
                                        saveProjectAutoApprove projectRoot True
                                        putTextLn stderr
                                            (roleSuccess color
                                                glyphOk <> "auto-approve on (saved for project)")
                                        pure (Right True)
                                    Deny -> pure (Right False)

planModeBlocksCall :: PlanModeEnv -> Bool -> FilePath -> ToolCall -> IO Bool
planModeBlocksCall _planMode active planPath call
    | not active = pure False
    | call.name == "apply_patch" = pure True
    | call.name == "search_replace" =
        let target = jsonArg "file_path" call.arguments
        in pure $
            Text.null target
                || not (isPlanFileEditTarget planPath (Text.unpack target))
    | otherwise = pure False

isPlanFileWrite :: PlanModeEnv -> Bool -> FilePath -> ToolCall -> IO Bool
isPlanFileWrite _planMode active planPath call
    | not active = pure False
    | call.name == "search_replace" =
        let target = jsonArg "file_path" call.arguments
        in pure $
            not (Text.null target)
                && isPlanFileEditTarget planPath (Text.unpack target)
    | otherwise = pure False

jsonArg :: Text -> Text -> Text
jsonArg key arguments = case Aeson.decodeStrict (TextEncoding.encodeUtf8 arguments) of
    Just (Aeson.Object object) -> case KeyMap.lookup (Key.fromText key) object of
        Just (Aeson.String value) -> value
        _ -> ""
    _ -> ""

toggleAlwaysApprove :: IORef ApprovalPolicy -> FilePath -> IO ()
toggleAlwaysApprove policyRef projectRoot = do
    color <- resolveColor stderr
    next <- atomicModifyIORef' policyRef \policy ->
        if policy == ApproveAll
            then (PromptMutating, PromptMutating)
            else (ApproveAll, ApproveAll)
    saveProjectAutoApprove projectRoot (next == ApproveAll)
    putTextLn stderr (case next of
        ApproveAll -> roleSuccess color glyphOk <> "auto-approve on (saved for project)"
        _ -> roleMuted color glyphSession <> "auto-approve off (saved for project)")

-- | Rebuild from the constructor: 'input' is also a field on 'CustomToolCall'.
-- OpenAI keeps @store = true@ so @previous_response_id@ can continue a chain;
-- xAI/OpenRouter force @store = false@ and replay local transcripts instead.
requestParams
    :: Provider
    -> Text
    -> Text
    -> [ResponseTool]
    -> Text
    -> ResponseCreateParams
requestParams provider modelName instructionText toolSchemas effort =
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
                , store = Just (provider == OpenAIProvider)
                , ..
                }
