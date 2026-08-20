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
    )
import Agent.ToolDispatch (ToolCall(..))
import Agent.Tools (appToolHandlers, codingToolsFor)
import Agent.Tools.Types (AppTool(..), defaultToolEnv)
import Agent.OpenRouter.LoopBackend (openRouterBackend)
import qualified Agent.OpenRouter.Options as OpenRouter
import Agent.XAI.LoopBackend (xaiBackend)
import qualified Agent.XAI.Options as XAI
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Exception (try)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (getCurrentTime, utctDay)
import qualified Data.Aeson.KeyMap as KeyMap
import System.Directory (getCurrentDirectory, getHomeDirectory, makeAbsolute, setCurrentDirectory)
import System.Environment (getArgs, lookupEnv)
import System.Exit (die, exitFailure)
import System.IO (hFlush, hIsTerminalDevice, hPutStrLn, isEOF, stderr, stdin, stdout)

run :: IO ()
run = do
    args <- getArgs
    case parseArgs args of
        Left err -> die err
        Right ShowHelp -> putStr usage
        Right ShowVersion -> putStrLn "agent-cli 0.1.0.0"
        Right (RunAgent options) -> runAgent options

runAgent :: CliOptions -> IO ()
runAgent options = do
    source <- maybe getCurrentDirectory makeAbsolute options.optCwd
    cwd <- if options.optWorktree
        then do
            home <- getHomeDirectory
            createWorktree source (worktreeRoot home) >>= either die \path -> do
                hPutStrLn stderr ("worktree: " <> path)
                pure path
        else pure source
    setCurrentDirectory cwd
    isTty <- hIsTerminalDevice stdin
    loaded <- loadAuth options.optProvider >>= either die pure
    tools <- codingToolsFor loaded.loadedProvider (defaultToolEnv cwd)
    today <- utctDay <$> getCurrentTime
    let provider = loaded.loadedProvider
        model = fromMaybe (defaultModelFor provider) options.optModel
        instructions = systemPrompt provider cwd today (isOneShot options)
        effort = fromMaybe (defaultEffortFor provider) options.optEffort
        params = requestParams model instructions
            (schemasFromAppTools provider tools) effort
        policy = resolveApprovalPolicy options isTty
    paramsRef <- newIORef params
    prompt <- loadPrompt options
    case provider of
        OpenAIProvider ->
            try @CodexAuthFailed
                (withCodexWsWithProvider loaded.loadedTokenProvider \conn _credential ->
                    runSession options policy tools prompt paramsRef
                        (openAiBackend conn (readIORef paramsRef)))
                >>= \case
                    Left (CodexAuthFailed err) -> die ("openai auth: " <> show err)
                    Right () -> pure ()
        XAIProvider -> do
            xaiOptions <- XAI.clientOptionsFromEnv
            credential <- firstCredential loaded
            backend <- xaiBackend xaiOptions credential (readIORef paramsRef)
            runSession options policy tools prompt paramsRef backend
        OpenRouterProvider -> do
            openRouterOptions <- OpenRouter.clientOptionsFromEnv
            credential <- firstCredential loaded
            backend <- openRouterBackend openRouterOptions credential (readIORef paramsRef)
            runSession options policy tools prompt paramsRef backend

runSession
    :: CliOptions
    -> ApprovalPolicy
    -> [AppTool]
    -> Maybe Text
    -> IORef ResponseCreateParams
    -> Backend
    -> IO ()
runSession options policy tools prompt paramsRef backend = do
    printed <- newIORef False
    textBuffer <- newIORef ""
    thinkingVisible <- newIORef False
    ioLock <- newMVar ()
    previous <- newIORef Nothing
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
            ok <- runOneTurn config render previous printed text
            if ok then putTrailingNewline printed else exitFailure
        Nothing -> repl config render previous printed paramsRef policyRef

repl
    :: LoopConfig
    -> RenderConfig
    -> IORef (Maybe Text)
    -> IORef Bool
    -> IORef ResponseCreateParams
    -> IORef ApprovalPolicy
    -> IO ()
repl config render previous printed paramsRef policyRef = do
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
                        _ <- runOneTurn config render previous printed text
                        putTrailingNewline printed
                        continue
                    ReplShowEffort -> do
                        params <- readIORef paramsRef
                        Text.putStrLn ("effort: " <> currentEffort params)
                        continue
                    ReplSetEffort level -> do
                        modifyIORef' paramsRef (setReasoningEffort level)
                        Text.putStrLn ("effort set to " <> level)
                        continue
                    ReplToggleAlwaysApprove -> do
                        toggleAlwaysApprove policyRef
                        continue
                    ReplCommandError err -> do
                        Text.hPutStrLn stderr err
                        continue
  where
    continue = repl config render previous printed paramsRef policyRef

runOneTurn
    :: LoopConfig
    -> RenderConfig
    -> IORef (Maybe Text)
    -> IORef Bool
    -> Text
    -> IO Bool
runOneTurn config render previous printed prompt = do
    prev <- readIORef previous
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
