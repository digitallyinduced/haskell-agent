-- | Command-line entry point: one-shot @-p@ or an interactive REPL.
module Agent.CLI
    ( DevResult(..)
    , afterDev
    , devMain
    , run
    ) where

import Agent.CLI.Auth (LoadedAuth(..), loadAuth)
import Agent.CLI.CancelWatch (withEscCancel, withStdinPaused)
import Agent.CLI.Clipboard
    ( ClipboardContent(..)
    , formatImageSize
    , readClipboard
    , readClipboardImages
    )
import Agent.CLI.Command
import Agent.CLI.Compaction
    ( CompactOutcome(..)
    , runProviderCompact
    )
import Agent.CLI.Input (ReplLine(..), readApprovalLine, readReplLine)
import Agent.CLI.Interrupt
    ( InterruptState
    , newInterruptState
    , withCtrlCHandler
    , withTurnCancel
    )
import Agent.CLI.ModelPicker (pickModel)
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
import Agent.OpenAI.Compaction
    ( compactSessionUserText
    , isCompactSessionTurn
    )
import Agent.OpenAI.LoopBackend (openAiBackend, toolResultToItem)
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
import Agent.ToolDispatch (ToolCall(..))
import Agent.Subagents
    ( RunSubagent
    , SubagentConfig(..)
    , SubagentId(..)
    , SubagentRegistry
    , SubagentSpawnEnv(..)
    , SubagentStatus(..)
    , closeSubagentRegistry
    , defaultSubagentConfig
    , formatCompletionNotice
    , newSubagentRegistry
    , setSubagentOnComplete
    , setSubagentRunner
    )
import Agent.Tools (CodingTools(..), appToolHandlers, codingToolsFor)
import Agent.Tools.MultiAgents (MultiAgentContext(..))
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
import Agent.Tools.Dangerous (shellCommandBlocked)
import Agent.Tools.Types (AppTool(..), ToolEnv(..), defaultToolEnv, toolAllowsWithoutPrompt)
import Agent.OpenRouter.LoopBackend (openRouterBackend)
import qualified Agent.OpenRouter.Options as OpenRouter
import Agent.XAI.LoopBackend (xaiBackend)
import qualified Agent.XAI.Options as XAI
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (AsyncException(UserInterrupt))
import Control.Exception.Safe (catchAsync, finally, throwIO, try)
import Control.Monad (unless, when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
    -- Shared with Esc cancel and plan prompts so arrow-key pickers own stdin.
    escPaused <- newIORef False
    let planHooks = cliPlanHooks interrupt escPaused (resolveColor stderr)
        provider = loaded.loadedProvider
    multiCtx <- case provider of
        OpenAIProvider -> do
            registry <- newSubagentRegistry defaultSubagentConfig cwd
                (\_ _ _ _ -> pure $ Left LoopNoResponseId)
                (\_ _ -> pure ())
            pure $ Just MultiAgentContext
                { multiRegistry = registry
                , multiSelfId = Nothing
                , multiDepth = 0
                }
        _ -> pure Nothing
    -- Per-subagent OpenAI transcripts / previous ids, shared across send_input.
    subagentSessions <- newIORef Map.empty
    pendingNotices <- newIORef ([] :: [Text])
    case multiCtx of
        Just ctx ->
            setSubagentOnComplete ctx.multiRegistry \agentId status ->
                atomicModifyIORef' pendingNotices \xs ->
                    (xs <> [formatCompletionNotice agentId status], ())
        Nothing -> pure ()
    coding <- codingToolsFor provider toolEnv (Just planHooks) multiCtx
    let tools = coding.codingAppTools
        planMode = coding.codingPlanMode
        closeAll = do
            case multiCtx of
                Just ctx -> closeSubagentRegistry ctx.multiRegistry
                Nothing -> pure ()
            coding.codingClose
    flip finally closeAll do
        today <- utctDay <$> getCurrentTime
        let model = fromMaybe
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
            initialItems = maybe [] (foldSessionItems . snd) resumed
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
                            (withCodexWsWithProvider loaded.loadedTokenProvider \conn _credential -> do
                                wsLock <- newMVar ()
                                case multiCtx of
                                    Just ctx ->
                                        setSubagentRunner ctx.multiRegistry $
                                            runCodexSubagent
                                                options
                                                policy
                                                planHooks
                                                paramsRef
                                                wsLock
                                                conn
                                                ctx.multiRegistry
                                                subagentSessions
                                    Nothing -> pure ()
                                let lockedBackend =
                                        lockedOpenAiBackend wsLock conn
                                            (readIORef paramsRef)
                                            transcriptRef
                                    noticingBackend =
                                        withPendingNotices pendingNotices lockedBackend
                                runSession options provider policy tools toolEnv planMode prompt paramsRef transcriptRef
                                    initialPrevious persist projectRoot (Just loaded.loadedTokenProvider) agentsContext escPaused interrupt
                                    noticingBackend)
                            >>= \case
                                Left (CodexAuthFailed err) -> die ("openai auth: " <> show err)
                                Right result -> pure result
                    XAIProvider -> do
                        xaiOptions <- XAI.clientOptionsFromEnv
                        let backend =
                                xaiBackend xaiOptions loaded.loadedTokenProvider
                                    (readIORef paramsRef) transcriptRef
                        runSession options provider policy tools toolEnv planMode prompt paramsRef transcriptRef
                            initialPrevious persist projectRoot (Just loaded.loadedTokenProvider) agentsContext escPaused interrupt backend
                    OpenRouterProvider -> do
                        openRouterOptions <- OpenRouter.clientOptionsFromEnv
                        let backend =
                                openRouterBackend openRouterOptions loaded.loadedTokenProvider
                                    (readIORef paramsRef) transcriptRef
                        runSession options provider policy tools toolEnv planMode prompt paramsRef transcriptRef
                            initialPrevious persist projectRoot (Just loaded.loadedTokenProvider) agentsContext escPaused interrupt backend

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
    -> IORef Bool
    -> InterruptState
    -> Backend
    -> IO DevResult
runSession options provider policy tools toolEnv planMode prompt paramsRef transcriptRef initialPrevious persist projectRoot tokenProvider agentsContext escPaused interrupt backend = do
    printed <- newIORef False
    attachmentsRef <- newIORef []
    textBuffer <- newIORef ""
    liveRows <- newIORef (0 :: Int)
    liveEndsNL <- newIORef False
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
            , renderLiveRows = liveRows
            , renderLiveEndsWithNewline = liveEndsNL
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
                transcriptRef persist planMode projectRoot tokenProvider agentsContext
                escPaused attachmentsRef interrupt

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
    -> IORef [ImageAttachment]
    -> InterruptState
    -> IO DevResult
repl config render provider previous printed paramsRef policyRef transcriptRef persist planMode projectRoot tokenProvider agentsContext escPaused attachmentsRef interrupt = do
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
                        _ <- runOneTurn config render previous printed
                            transcriptRef persist planMode agentsContext escPaused interrupt text
                            turnInputs
                        putTrailingNewline printed
                        continue
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
                                        Text.putStrLn
                                            (roleMuted color (glyphOk <> "pasted " <> sizes))
                                        writeIORef printed False
                                        _ <- runOneTurn config render previous printed
                                            transcriptRef persist planMode agentsContext escPaused interrupt promptText
                                            [ UserMultimodal
                                                { userText = promptText
                                                , userImages = images
                                                }
                                            ]
                                        putTrailingNewline printed
                                        continue
                                    else do
                                        modifyIORef' attachmentsRef (<> images)
                                        pending <- readIORef attachmentsRef
                                        Text.putStrLn
                                            (roleMuted color
                                                (glyphOk
                                                    <> "attached "
                                                    <> sizes
                                                    <> " — send with next message ("
                                                    <> Text.pack (show (length pending))
                                                    <> " queued)"))
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
                            Just name
                                | name == current -> do
                                    Text.putStrLn
                                        (roleMuted color
                                            (glyphSession <> "model: " <> name))
                                    continue
                                | otherwise -> do
                                    applyModelChange
                                        provider name paramsRef render previous persist
                                    continue
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
                                                }
                                        handle' <- appendTurn handle turn
                                        writeIORef slotRef (Right handle')
                                        writeSessionMeta handle'.sessionMetaPath $
                                            handle'.sessionMeta
                                                { metaLastResponseId = Nothing
                                                , metaUpdatedAt = now
                                                }
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
            transcriptRef persist planMode projectRoot tokenProvider agentsContext
            escPaused attachmentsRef interrupt

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
    -- Hard deny for catastrophic shell deletes, even under ApproveAll / yolo.
    case shellCommandBlocked call.name call.arguments of
        Just msg -> do
            color <- resolveColor stderr
            putTextLn stderr (roleWarn color (glyphWarn <> msg))
            pure (Left msg)
        Nothing -> do
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
        ApproveAll -> roleSuccess color (glyphOk <> "auto-approve on (saved for project)")
        _ -> roleMuted color (glyphSession <> "auto-approve off (saved for project)"))

data SubagentSession = SubagentSession
    { subSessionTranscript :: !(IORef [ResponseItem])
    }

-- | Serialize OpenAI WebSocket turns: parent and children share one connection,
-- and 'receiveWsResponse' is not multiplexed.
lockedOpenAiBackend
    :: MVar ()
    -> CodexConn
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> Backend
lockedOpenAiBackend wsLock conn getParams transcript =
    let Backend submit = openAiBackend conn getParams transcript
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
    -> CodexConn
    -> SubagentRegistry
    -> IORef (Map SubagentId SubagentSession)
    -> RunSubagent
runCodexSubagent options policy planHooks paramsRef wsLock conn registry sessionsRef =
    \env previous prompt onEvent -> do
        parentParams <- readIORef paramsRef
        childEnv <- defaultToolEnv env.subCwd
        -- Inherit soft-cancel from the registry-owned child flag.
        let childToolEnv = childEnv { toolCancel = env.subCancel }
            childCtx = MultiAgentContext
                { multiRegistry = registry
                , multiSelfId = Just env.subId
                , multiDepth = env.subDepth
                }
        session <- lookupOrCreateSubagentSession sessionsRef env.subId
        coding <- codingToolsFor OpenAIProvider childToolEnv (Just planHooks) (Just childCtx)
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
                childParams = requestParams OpenAIProvider model instructions
                    (schemasFromAppTools OpenAIProvider tools) effort
            childParamsRef <- newIORef childParams
            let backend =
                    lockedOpenAiBackend wsLock conn
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
            runLoop config previous prompt

lookupOrCreateSubagentSession
    :: IORef (Map SubagentId SubagentSession)
    -> SubagentId
    -> IO SubagentSession
lookupOrCreateSubagentSession sessionsRef agentId = do
    sessions <- readIORef sessionsRef
    case Map.lookup agentId sessions of
        Just session -> pure session
        Nothing -> do
            transcript <- newIORef ([] :: [ResponseItem])
            let session = SubagentSession { subSessionTranscript = transcript }
            atomicModifyIORef' sessionsRef \m -> (Map.insert agentId session m, ())
            pure session

childApprove :: ApprovalPolicy -> [AppTool] -> ToolCall -> IO (Either Text Bool)
childApprove policy tools call = case policy of
    ApproveAll -> pure (Right True)
    DenyMutating -> do
        allowed <- case lookupAppTool call.name tools of
            Just tool -> toolAllowsWithoutPrompt tool call
            Nothing -> pure False
        pure $ if allowed then Right True else Right False
    PromptMutating -> do
        allowed <- case lookupAppTool call.name tools of
            Just tool -> toolAllowsWithoutPrompt tool call
            Nothing -> pure False
        if allowed
            then pure (Right True)
            else pure $ Left
                "Subagent cannot prompt for approval on mutating tools. \
                \Re-run the parent with auto-approve/--yolo, or have the \
                \parent perform this edit."

-- | Rebuild from the constructor: 'input' is also a field on 'CustomToolCall'.
-- OpenAI keeps @store = true@ so @previous_response_id@ can continue a chain;
-- xAI/OpenRouter force @store = false@ and replay local transcripts instead.

-- | Apply compact turns as full transcript replacements when resuming.
foldSessionItems :: [SessionTurn] -> [ResponseItem]
foldSessionItems = go []
  where
    go acc [] = acc
    go acc (turn:rest)
        | isCompactSessionTurn turn.turnUserText
            && not (null turn.turnItems) =
            go turn.turnItems rest
        | otherwise = go (acc <> turn.turnItems) rest

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
