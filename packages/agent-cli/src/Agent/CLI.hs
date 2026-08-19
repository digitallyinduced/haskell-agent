-- | Command-line entry point: one-shot @-p@ or an interactive REPL.
module Agent.CLI
    ( run
    ) where

import Agent.CLI.Auth (LoadedAuth(..), loadAuth)
import Agent.CLI.Options
import Agent.CLI.Prompt (defaultModelFor, systemPrompt)
import Agent.CLI.Render
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
import Agent.XAI.LoopBackend (xaiBackend)
import Agent.XAI.Options (clientOptionsFromEnv)
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
import System.Environment (getArgs)
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
        instructions = systemPrompt provider cwd today
        params = requestParams model instructions (schemasFromAppTools tools) options.optEffort
        policy = resolveApprovalPolicy options isTty
    prompt <- loadPrompt options
    case provider of
        OpenAIProvider ->
            try @CodexAuthFailed
                (withCodexWsWithProvider loaded.loadedTokenProvider \conn _credential ->
                    runSession options policy tools prompt
                        (openAiBackend conn params))
                >>= \case
                    Left (CodexAuthFailed err) -> die ("openai auth: " <> show err)
                    Right () -> pure ()
        XAIProvider -> do
            xaiOptions <- clientOptionsFromEnv
            credential <- firstCredential loaded
            backend <- xaiBackend xaiOptions credential params
            runSession options policy tools prompt backend

runSession
    :: CliOptions
    -> ApprovalPolicy
    -> [AppTool]
    -> Maybe Text
    -> Backend
    -> IO ()
runSession options policy tools prompt backend = do
    printed <- newIORef False
    approveLock <- newMVar ()
    previous <- newIORef Nothing
    let render = RenderConfig
            { renderShowReasoning = options.optShowReasoning
            , renderPrintedText = printed
            }
        config = LoopConfig
            { loopBackend = backend
            , loopHandlers = appToolHandlers tools
            , loopDispatch = defaultLoopDispatch
            , loopMaxTurns = options.optMaxTurns
            , loopOnEvent = renderEvent render
            , loopApprove = \call ->
                withMVar approveLock \_ ->
                    approveTool policy tools call
            }
    case prompt of
        Just text -> do
            ok <- runOneTurn config previous printed text
            if ok then putTrailingNewline printed else exitFailure
        Nothing -> repl config previous printed

repl :: LoopConfig -> IORef (Maybe Text) -> IORef Bool -> IO ()
repl config previous printed = do
    putStr "agent> "
    hFlush stdout
    done <- isEOF
    if done
        then putStrLn ""
        else do
            line <- Text.strip <$> Text.getLine
            if Text.null line
                then repl config previous printed
                else if line == ":q" || line == ":quit"
                    then pure ()
                    else do
                        writeIORef printed False
                        _ <- runOneTurn config previous printed line
                        putTrailingNewline printed
                        repl config previous printed

runOneTurn
    :: LoopConfig
    -> IORef (Maybe Text)
    -> IORef Bool
    -> Text
    -> IO Bool
runOneTurn config previous printed prompt = do
    prev <- readIORef previous
    result <- runLoop config prev prompt
    case result of
        Left err -> do
            Text.hPutStrLn stderr (formatLoopError err)
            pure False
        Right loopResult -> do
            writeIORef previous (Just loopResult.finalResponseId)
            printedText <- readIORef printed
            case (printedText, loopResult.finalText) of
                (False, Just text) | not (Text.null (Text.strip text)) ->
                    Text.putStrLn text
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

approveTool :: ApprovalPolicy -> [AppTool] -> ToolCall -> IO Bool
approveTool policy tools call =
    let readOnly = maybe False (.appToolReadOnly) (lookupAppTool call.name tools)
    in case policy of
        ApproveAll -> pure True
        DenyMutating -> pure readOnly
        PromptMutating
            | readOnly -> pure True
            | otherwise -> do
                hPutStrLn stderr ("Allow " <> Text.unpack (summarizeToolCall call) <> "? [y/N]")
                hFlush stderr
                eof <- isEOF
                if eof
                    then pure False
                    else do
                        answer <- Text.toLower . Text.strip <$> Text.getLine
                        pure (answer == "y" || answer == "yes")

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
