-- | Command-line entry point: one-shot @-p@ or an interactive REPL.
module Agent.CLI
    ( run
    ) where

import Agent.CLI.Auth (LoadedAuth(..), loadAuth)
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
import Agent.OpenAI.LoopBackend (openAiBackend)
import Agent.OpenAI.Responses.Types
import Agent.OpenAI.WebSocketClient (CodexAuthFailed(..), withCodexWsWithProvider)
import Agent.Provider
    ( Credential(..)
    , Provider(..)
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
import Control.Exception (finally, try)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.Aeson.KeyMap as KeyMap
import System.Directory (getCurrentDirectory, getHomeDirectory, makeAbsolute, setCurrentDirectory)
import System.Environment (getArgs, lookupEnv)
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

        persist <- preparePersistence options root provider model cwd effort prompt resumed
        case provider of
            OpenAIProvider ->
                try @CodexAuthFailed
                    (withCodexWsWithProvider loaded.loadedTokenProvider \conn _credential ->
                        runSession options policy tools prompt paramsRef transcriptRef
                            initialPrevious persist
                            (openAiBackend conn (readIORef paramsRef) transcriptRef))
                    >>= \case
                        Left (CodexAuthFailed err) -> die ("openai auth: " <> show err)
                        Right () -> pure ()
            XAIProvider -> do
                xaiOptions <- XAI.clientOptionsFromEnv
                credential <- firstCredential loaded
                let backend = xaiBackend xaiOptions credential (readIORef paramsRef) transcriptRef
                runSession options policy tools prompt paramsRef transcriptRef
                    initialPrevious persist backend
            OpenRouterProvider -> do
                openRouterOptions <- OpenRouter.clientOptionsFromEnv
                credential <- firstCredential loaded
                let backend =
                        openRouterBackend openRouterOptions credential
                            (readIORef paramsRef) transcriptRef
                runSession options policy tools prompt paramsRef transcriptRef
                    initialPrevious persist backend

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

shouldPersist :: CliOptions -> Bool
shouldPersist options = not (isOneShot options) || options.optSaveSession

isJustCwd :: CliOptions -> Bool
isJustCwd options = case options.optCwd of
    Just _ -> True
    Nothing -> False


runSession
    :: CliOptions
    -> ApprovalPolicy
    -> [AppTool]
    -> Maybe Text
    -> IORef ResponseCreateParams
    -> IORef [ResponseItem]
    -> Maybe Text
    -> Maybe (IORef (Either SessionCreate SessionHandle))
    -> Backend
    -> IO ()
runSession options policy tools prompt paramsRef transcriptRef initialPrevious persist backend = do
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
            ok <- runOneTurn config render previous printed transcriptRef persist text
            if ok then putTrailingNewline printed else exitFailure
        Nothing ->
            repl config render previous printed paramsRef policyRef transcriptRef persist

repl
    :: LoopConfig
    -> RenderConfig
    -> IORef (Maybe Text)
    -> IORef Bool
    -> IORef ResponseCreateParams
    -> IORef ApprovalPolicy
    -> IORef [ResponseItem]
    -> Maybe (IORef (Either SessionCreate SessionHandle))
    -> IO ()
repl config render previous printed paramsRef policyRef transcriptRef persist = do
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
                            transcriptRef persist text
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
                    ReplCommandError err -> do
                        color <- resolveColor stderr
                        Text.hPutStrLn stderr (roleError color err)
                        continue
  where
    continue = repl config render previous printed paramsRef policyRef transcriptRef persist

runOneTurn
    :: LoopConfig
    -> RenderConfig
    -> IORef (Maybe Text)
    -> IORef Bool
    -> IORef [ResponseItem]
    -> Maybe (IORef (Either SessionCreate SessionHandle))
    -> Text
    -> IO Bool
runOneTurn config render previous printed transcriptRef persist prompt = do
    prev <- readIORef previous
    beforeItems <- readIORef transcriptRef
    result <- runLoop config prev prompt
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
                            , turnUserText = prompt
                            , turnAssistantText = loopResult.finalText
                            , turnResponseId = Just loopResult.finalResponseId
                            , turnItems = newItems
                            }
                    handle' <- appendTurn handle turn
                    writeIORef slotRef (Right handle')
            pure True


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

firstCredential :: LoadedAuth -> IO Credential
firstCredential loaded =
    getNextToken loaded.loadedTokenProvider Nothing >>= \case
        Left err -> die ("credential: " <> show err)
        Right credential -> pure credential

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
