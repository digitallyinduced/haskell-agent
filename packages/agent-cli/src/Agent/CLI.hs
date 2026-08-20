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
    , formatLoopError
    , putTextLn
    , renderAssistantText
    , renderEvent
    , summarizeToolCall
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
import System.IO (hFlush, hIsTerminalDevice, hPutStrLn, isEOF, stderr, stdin, stdout)

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
                    hPutStrLn stderr ("worktree: " <> path)
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
            params = requestParams model instructions
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
    -> IO (Maybe (IORef SessionHandle))
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
            hPutStrLn stderr ("session: " <> Text.unpack meta.metaId <> " (resumed)")
            Just <$> newIORef handle
        Nothing
            | shouldPersist options -> do
                handle <- createSession root provider model cwd effort
                    (sessionTitleFromPrompt <$> prompt)
                hPutStrLn stderr ("session: " <> Text.unpack handle.sessionMeta.metaId)
                Just <$> newIORef handle
            | otherwise -> pure Nothing

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
    -> Maybe (IORef SessionHandle)
    -> Backend
    -> IO ()
runSession options policy tools prompt paramsRef transcriptRef initialPrevious persist backend = do
    printed <- newIORef False
    textBuffer <- newIORef ""
    thinkingVisible <- newIORef False
    ioLock <- newMVar ()
    previous <- newIORef initialPrevious
    policyRef <- newIORef policy
    stdoutTty <- hIsTerminalDevice stdout
    stderrTty <- hIsTerminalDevice stderr
    noColor <- lookupEnv "NO_COLOR"
    let useColor = stdoutTty && maybe True (\_ -> False) noColor
        render = RenderConfig
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
    -> Maybe (IORef SessionHandle)
    -> IO ()
repl config render previous printed paramsRef policyRef transcriptRef persist = do
    putStr "agent> "
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
                        params <- readIORef paramsRef
                        Text.putStrLn ("effort: " <> currentEffort params)
                        continue
                    ReplSetEffort level -> do
                        modifyIORef' paramsRef (setReasoningEffort level)
                        Text.putStrLn ("effort set to " <> level)
                        case persist of
                            Nothing -> pure ()
                            Just handleRef -> do
                                handle <- readIORef handleRef
                                let meta = handle.sessionMeta { metaEffort = level }
                                writeSessionMeta handle.sessionMetaPath meta
                                writeIORef handleRef handle { sessionMeta = meta }
                        continue
                    ReplToggleAlwaysApprove -> do
                        toggleAlwaysApprove policyRef
                        continue
                    ReplShowSession -> do
                        case persist of
                            Nothing -> Text.putStrLn "session: (not persisted)"
                            Just handleRef -> do
                                handle <- readIORef handleRef
                                Text.putStrLn ("session: " <> handle.sessionMeta.metaId)
                        continue
                    ReplCommandError err -> do
                        Text.hPutStrLn stderr err
                        continue
  where
    continue = repl config render previous printed paramsRef policyRef transcriptRef persist

runOneTurn
    :: LoopConfig
    -> RenderConfig
    -> IORef (Maybe Text)
    -> IORef Bool
    -> IORef [ResponseItem]
    -> Maybe (IORef SessionHandle)
    -> Text
    -> IO Bool
runOneTurn config render previous printed transcriptRef persist prompt = do
    prev <- readIORef previous
    beforeItems <- readIORef transcriptRef
    result <- runLoop config prev prompt
    clearThinking render
    case result of
        Left err -> do
            putTextLn stderr (formatLoopError err)
            pure False
        Right loopResult -> do
            writeIORef previous (Just loopResult.finalResponseId)
            printedText <- readIORef printed
            case (printedText, loopResult.finalText) of
                (False, Just text) | not (Text.null (Text.strip text)) -> do
                    color <- hIsTerminalDevice stdout
                    noColor <- lookupEnv "NO_COLOR"
                    let useColor = color && maybe True (\_ -> False) noColor
                    putTextLn stdout (renderAssistantText useColor text)
                _ -> pure ()
            afterItems <- readIORef transcriptRef
            let newItems = drop (length beforeItems) afterItems
            case persist of
                Nothing -> pure ()
                Just handleRef -> do
                    now <- getCurrentTime
                    handle <- readIORef handleRef
                    let turn = SessionTurn
                            { turnAt = now
                            , turnUserText = prompt
                            , turnAssistantText = loopResult.finalText
                            , turnResponseId = Just loopResult.finalResponseId
                            , turnItems = newItems
                            }
                    handle' <- appendTurn handle turn
                    writeIORef handleRef handle'
            pure True

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
                putTextLn stderr ("Allow " <> summarizeToolCall call <> "? [y/N/a]")
                eof <- isEOF
                if eof
                    then pure False
                    else do
                        answer <- parseApprovalAnswer <$> Text.getLine
                        case answer of
                            AllowOnce -> pure True
                            AllowAlways -> do
                                writeIORef policyRef ApproveAll
                                putTextLn stderr "auto-approve on"
                                pure True
                            Deny -> pure False

toggleAlwaysApprove :: IORef ApprovalPolicy -> IO ()
toggleAlwaysApprove policyRef = do
    next <- atomicModifyIORef' policyRef \policy ->
        if policy == ApproveAll
            then (PromptMutating, PromptMutating)
            else (ApproveAll, ApproveAll)
    putTextLn stderr (case next of
        ApproveAll -> "auto-approve on"
        _ -> "auto-approve off")

-- | Rebuild from the constructor: 'input' is also a field on 'CustomToolCall'.
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
