-- | Command-line entry point: one-shot @-p@ or an interactive REPL.
module Agent.CLI
    ( run
    ) where

import Agent.CLI.Auth (LoadedAuth(..), loadAuth)
import Agent.CLI.Clipboard (formatImageSize, readClipboardImage)
import Agent.CLI.Command
import Agent.CLI.Options
import Agent.CLI.Prompt (defaultModelFor, systemPrompt)
import Agent.CLI.Render
    ( RenderConfig(..)
    , clearThinking
    , formatLoopErrorColored
    , putTextLn
    , renderAssistantText
    , renderEvent
    , summarizeToolCall
    )
import Agent.CLI.Style
    ( roleError
    , roleMuted
    , rolePrompt
    , roleSuccess
    , roleWarn
    )
import Agent.CLI.Session
import Agent.CLI.Tools (lookupAppTool, schemasFromAppTools)
import Agent.CLI.Worktree (createWorktree, worktreeRoot)
import Agent.Loop
import Agent.ProjectInstructions
    ( DiscoverOptions(..)
    , defaultDiscoverOptions
    , discoverProjectInstructions
    , formatAgentsMdForProvider
    , globalAgentsHomeDir
    , loadedInstructionFiles
    )
import Agent.OpenAI.LoopBackend (openAiBackend)
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
import Agent.Tools (appToolHandlers, codingToolsFor)
import Agent.Tools.Types (AppTool(..), defaultToolEnv)
import Agent.OpenRouter.LoopBackend (openRouterBackend)
import qualified Agent.OpenRouter.Options as OpenRouter
import Agent.XAI.LoopBackend (xaiBackend)
import qualified Agent.XAI.Options as XAI
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Exception (AsyncException(UserInterrupt))
import Control.Exception.Safe (catchAsync, finally, throwIO, try)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.Aeson.KeyMap as KeyMap
import System.Directory (getCurrentDirectory, getHomeDirectory, makeAbsolute, setCurrentDirectory)
import System.Environment (getArgs, getProgName, lookupEnv)
import System.FilePath ((</>))
import System.Exit (die, exitFailure)
import System.IO (Handle, hFlush, hIsTerminalDevice, isEOF, stderr, stdin, stdout)

run :: IO ()
run = do
    args <- getArgs
    case parseArgs args of
        Left err -> die err
        Right ShowHelp -> putStr usage
        Right ShowVersion -> putStrLn "agent-cli 0.1.0.0"
        Right ListSessions -> runListSessions
        Right (ShowSession sessionId) -> runShowSession sessionId
        Right (RunAgent options) -> runAgent options

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

runAgent :: CliOptions -> IO ()
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
                    putTextLn stderr (roleMuted color ("worktree: " <> Text.pack path))
                    pure path
            | otherwise -> pure source
    setCurrentDirectory cwd

    isTty <- hIsTerminalDevice stdin
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

    (tools, closeTools) <- codingToolsFor loaded.loadedProvider (defaultToolEnv cwd)
    flip finally closeTools do
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
            initialItems = maybe [] (concatMap (.turnItems) . snd) resumed
            initialPrevious = resumed >>= \(meta, _) -> meta.metaLastResponseId
        paramsRef <- newIORef params
        transcriptRef <- newIORef initialItems
        prompt <- loadPrompt options
        agentsContext <- loadAgentsContext options provider home cwd initialItems initialPrevious

        persist <- preparePersistence options root provider model cwd effort prompt resumed
        progName <- getProgName
        withInterruptResume progName persist do
            case provider of
                OpenAIProvider ->
                    try @_ @CodexAuthFailed
                        (withCodexWsWithProvider loaded.loadedTokenProvider \conn _credential ->
                            runSession options provider policy tools prompt paramsRef transcriptRef
                                initialPrevious persist Nothing agentsContext
                                (openAiBackend conn (readIORef paramsRef) transcriptRef))
                        >>= \case
                            Left (CodexAuthFailed err) -> die ("openai auth: " <> show err)
                            Right () -> pure ()
                XAIProvider -> do
                    xaiOptions <- XAI.clientOptionsFromEnv
                    let backend =
                            xaiBackend xaiOptions loaded.loadedTokenProvider
                                (readIORef paramsRef) transcriptRef
                    runSession options provider policy tools prompt paramsRef transcriptRef
                        initialPrevious persist (Just loaded.loadedTokenProvider) agentsContext backend
                OpenRouterProvider -> do
                    openRouterOptions <- OpenRouter.clientOptionsFromEnv
                    let backend =
                            openRouterBackend openRouterOptions loaded.loadedTokenProvider
                                (readIORef paramsRef) transcriptRef
                    runSession options provider policy tools prompt paramsRef transcriptRef
                        initialPrevious persist (Just loaded.loadedTokenProvider) agentsContext backend

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
                    ("session: " <> meta.metaId <> " (resumed)"))
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
    -> Maybe Text
    -> IORef ResponseCreateParams
    -> IORef [ResponseItem]
    -> Maybe Text
    -> Maybe (IORef (Either SessionCreate SessionHandle))
    -> Maybe TokenProvider
    -> IORef (Maybe Text)
    -> Backend
    -> IO ()
runSession options provider policy tools prompt paramsRef transcriptRef initialPrevious persist tokenProvider agentsContext backend = do
    printed <- newIORef False
    textBuffer <- newIORef ""
    thinkingVisible <- newIORef False
    ioLock <- newMVar ()
    previous <- newIORef initialPrevious
    policyRef <- newIORef policy
    stderrTty <- hIsTerminalDevice stderr
    useColor <- resolveColor stdout
    let render = RenderConfig
            { renderShowThinking = stderrTty
            , renderThinkingVisible = thinkingVisible
            , renderColor = useColor
            , renderPrintedText = printed
            , renderTextBuffer = textBuffer
            , renderLock = ioLock
            , renderStdout = stdout
            , renderStderr = stderr
            }
        config = LoopConfig
            { loopBackend = backend
            , loopHandlers = appToolHandlers tools
            , loopDispatch = defaultLoopDispatch
            , loopMaxTurns = options.optMaxTurns
            , loopOnEvent = renderEvent render
            , loopApprove = \call ->
                withMVar ioLock \_ ->
                    approveTool policyRef tools call
            }
    case prompt of
        Just text -> do
            ok <- runOneTurn config render previous printed transcriptRef persist agentsContext text
                [UserMessage text]
            if ok then putTrailingNewline printed else exitFailure
        Nothing ->
            repl config render provider previous printed paramsRef policyRef
                transcriptRef persist tokenProvider agentsContext

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
    -> Maybe TokenProvider
    -> IORef (Maybe Text)
    -> IO ()
repl config render provider previous printed paramsRef policyRef transcriptRef persist tokenProvider agentsContext = do
    stdoutColor <- resolveColor stdout
    Text.putStr (rolePrompt stdoutColor "λ> ")
    hFlush stdout
    done <- isEOF
    if done
        then putStrLn ""
        else do
            line <- Text.strip <$> Text.getLine
            if Text.null line
                then continue
                else case parseReplLine line of
                    ReplQuit -> pure ()
                    ReplPrompt text -> do
                        writeIORef printed False
                        _ <- runOneTurn config render previous printed
                            transcriptRef persist agentsContext text [UserMessage text]
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
                                        ("pasted " <> image.imageMime <> " (" <> size <> ")"))
                                writeIORef printed False
                                _ <- runOneTurn config render previous printed
                                    transcriptRef persist agentsContext promptText
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
                        Text.putStrLn (roleMuted color ("effort: " <> currentEffort params))
                        continue
                    ReplSetEffort level -> do
                        color <- resolveColor stdout
                        modifyIORef' paramsRef (setReasoningEffort level)
                        Text.putStrLn (roleMuted color ("effort set to " <> level))
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
                        Text.putStrLn ("model: " <> currentModel params)
                        continue
                    ReplSetModel name -> do
                        modifyIORef' paramsRef (setModel name)
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
                        toggleAlwaysApprove policyRef
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
                                                ("session: " <> handle.sessionMeta.metaId))
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
            transcriptRef persist tokenProvider agentsContext

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

runOneTurn
    :: LoopConfig
    -> RenderConfig
    -> IORef (Maybe Text)
    -> IORef Bool
    -> IORef [ResponseItem]
    -> Maybe (IORef (Either SessionCreate SessionHandle))
    -> IORef (Maybe Text)
    -> Text
    -> [TurnInput]
    -> IO Bool
runOneTurn config render previous printed transcriptRef persist agentsContext promptText inputs = do
    prev <- readIORef previous
    beforeItems <- readIORef transcriptRef
    pendingAgents <- atomicModifyIORef' agentsContext \pending -> (Nothing, pending)
    let turnInputs = case pendingAgents of
            Just agents | null beforeItems && isNothing prev ->
                UserMessage agents : inputs
            _ -> inputs
    result <- runLoopInputs config prev turnInputs
    clearThinking render
    case result of
        Left err -> do
            color <- resolveColor stderr
            putTextLn stderr (formatLoopErrorColored color err)
            pure False
        Right loopResult -> do
            writeIORef previous (Just loopResult.finalResponseId)
            printedText <- readIORef printed
            case (printedText, loopResult.finalText) of
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
                    if created
                        then do
                            color <- resolveColor stderr
                            putTextLn stderr
                                (roleMuted color
                                    ("session: " <> handle.sessionMeta.metaId))
                        else pure ()
                    let turn = SessionTurn
                            { turnAt = now
                            , turnUserText = promptText
                            , turnAssistantText = loopResult.finalText
                            , turnResponseId = Just loopResult.finalResponseId
                            , turnItems = newItems
                            }
                    handle' <- appendTurn handle turn
                    writeIORef slotRef (Right handle')
            pure True


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
                        ("agents.md: loaded "
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

approveTool :: IORef ApprovalPolicy -> [AppTool] -> ToolCall -> IO Bool
approveTool policyRef tools call = do
    policy <- readIORef policyRef
    let readOnly = maybe False (.appToolReadOnly) (lookupAppTool call.name tools)
    case policy of
        ApproveAll -> pure True
        DenyMutating -> pure readOnly
        PromptMutating
            | readOnly -> pure True
            | otherwise -> do
                color <- resolveColor stderr
                putTextLn stderr
                    (roleWarn color ("Allow " <> summarizeToolCall call <> "? [y/N/a]"))
                eof <- isEOF
                if eof
                    then pure False
                    else do
                        answer <- parseApprovalAnswer <$> Text.getLine
                        case answer of
                            AllowOnce -> pure True
                            AllowAlways -> do
                                writeIORef policyRef ApproveAll
                                putTextLn stderr (roleSuccess color "auto-approve on")
                                pure True
                            Deny -> pure False

toggleAlwaysApprove :: IORef ApprovalPolicy -> IO ()
toggleAlwaysApprove policyRef = do
    color <- resolveColor stderr
    next <- atomicModifyIORef' policyRef \policy ->
        if policy == ApproveAll
            then (PromptMutating, PromptMutating)
            else (ApproveAll, ApproveAll)
    putTextLn stderr (case next of
        ApproveAll -> roleSuccess color "auto-approve on"
        _ -> roleMuted color "auto-approve off")

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
