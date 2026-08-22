module Agent.ClaudeCode.LoopBackendSpec (spec) where

import Agent.ClaudeCode.LoopBackend
    ( claudeCodeOneShotBackend
    , withClaudeCodeBackend
    )
import Agent.ClaudeCode.Session (defaultClaudeCodeOptions)
import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Loop
    ( Backend(..)
    , ImageAttachment(..)
    , LoopEvent(..)
    , TurnInput(..)
    , TurnOutput(..)
    )
import Agent.Responses.Types
    ( MessageContent(..)
    , ReasoningConfig(..)
    , ResponseContentPart(..)
    , ResponseCreateParams(..)
    , ResponseItem(..)
    , ResponseMessage(..)
    , defaultResponseCreateParams
    )
import Agent.Responses.LoopBackend (turnInputsToItems)
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolCallResult(..)
    )
import Control.Exception.Safe (bracket, finally)
import qualified Data.ByteString as ByteString
import Data.IORef
    ( modifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory
    ( createDirectory
    , getTemporaryDirectory
    , removeDirectoryRecursive
    , removeFile
    )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Posix.Files
    ( ownerExecuteMode
    , ownerReadMode
    , ownerWriteMode
    , setFileMode
    , unionFileModes
    )
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = do
    describe "Claude Code loop backend" do
        it "uses a PTY, tails JSONL, and persists normalized host messages" $
            withFakeClaude \fake -> do
                transcript <- newIORef []
                events <- newIORef []
                let params =
                        defaultResponseCreateParams
                            { model = Just "sonnet"
                            , reasoning = Just ReasoningConfig
                                { context = Nothing
                                , effort = Just "xhigh"
                                , generateSummary = Nothing
                                , reasoningMode = Nothing
                                , summary = Nothing
                                , extraFields = mempty
                                }
                            , instructions =
                                Just "Use the outer harness instructions."
                            }
                    options =
                        defaultClaudeCodeOptions
                            fake.executable
                            fake.workingDirectory
                result <- timeout 5_000_000 $
                    withClaudeCodeBackend
                        options
                        Nothing
                        (pure params)
                        transcript
                        \backend ->
                            backend.submitTurn
                                Nothing
                                [UserMessage "hello"]
                                (\event ->
                                    modifyIORef' events (<> [event]))
                turn <- case result of
                    Nothing ->
                        expectationFailure "fake Claude turn timed out"
                            >> fail "unreachable"
                    Just (Left err) ->
                        expectationFailure
                            ("fake Claude turn failed: " <> show err)
                            >> fail "unreachable"
                    Just (Right value) ->
                        pure value

                turn.responseId `shouldSatisfy` looksLikeUuid
                turn.assistantText `shouldBe` Just "fake response"
                turn.toolCalls `shouldBe` []
                observedEvents <- readIORef events
                observedEvents `shouldContain` [TextDelta "fake response"]
                observedEvents `shouldContain`
                    [ToolStarted expectedFakeToolCall]
                observedEvents `shouldContain`
                    [ToolFinished expectedFakeToolResult]

                history <- readIORef transcript
                map responseMessageText
                    [message | MessageItem message <- history]
                    `shouldBe`
                        [ "hello"
                        , "fake response"
                        ]
                length history `shouldBe` 2

                submitted <- readFile fake.promptLog
                submitted `shouldContain`
                    "Instructions supplied by the outer agent harness"
                submitted `shouldContain` "hello"
                arguments <- readFile fake.argumentLog
                arguments `shouldContain` "<--ax-screen-reader>"
                arguments `shouldContain`
                    "<--disallowedTools>\n<AskUserQuestion>"
                arguments `shouldContain` "<--safe-mode>"

        it "disables all Claude tools for isolated auxiliary requests" $
            withFakeClaude \fake -> do
                transcript <- newIORef []
                let backend =
                        claudeCodeOneShotBackend
                            (defaultClaudeCodeOptions
                                fake.executable
                                fake.workingDirectory)
                            (pure defaultResponseCreateParams)
                            transcript
                result <- timeout 5_000_000 $
                    backend.submitTurn
                        Nothing
                        [UserMessage "title this"]
                        (\_ -> pure ())
                result `shouldSatisfy` \case
                    Just (Right _) -> True
                    _ -> False
                arguments <- readFile fake.argumentLog
                arguments `shouldContain` "<--tools>\n<>"
                arguments `shouldContain`
                    "<--disallowedTools>\n<AskUserQuestion>"

        it "rejects image attachments before starting Claude Code" do
            transcript <- newIORef []
            let backend =
                    claudeCodeOneShotBackend
                        (defaultClaudeCodeOptions
                            "/definitely/not/started"
                            "/")
                        (pure defaultResponseCreateParams)
                        transcript
                image = ImageAttachment
                    { imageMime = "image/png"
                    , imageBytes = ByteString.singleton 0
                    }
            result <-
                backend.submitTurn
                    Nothing
                    [UserMultimodal "look" [image]]
                    (\_ -> pure ())
            result `shouldSatisfy` \case
                Left ProviderError{errorType = InvalidImageError} -> True
                _ -> False

        it "maps none effort to Claude Code's default effort" $
            withFakeClaude \fake -> do
                transcript <- newIORef []
                let params =
                        defaultResponseCreateParams
                            { reasoning = Just ReasoningConfig
                                { context = Nothing
                                , effort = Just "none"
                                , generateSummary = Nothing
                                , reasoningMode = Nothing
                                , summary = Nothing
                                , extraFields = mempty
                                }
                            }
                    backend =
                        claudeCodeOneShotBackend
                            (defaultClaudeCodeOptions
                                fake.executable
                                fake.workingDirectory)
                            (pure params)
                            transcript
                result <- timeout 5_000_000 $
                    backend.submitTurn
                        Nothing
                        [UserMessage "hello"]
                        (\_ -> pure ())
                result `shouldSatisfy` \case
                    Just (Right _) -> True
                    _ -> False
                arguments <- readFile fake.argumentLog
                arguments `shouldNotContain` "<--effort>"

        it "returns terminal transcript errors instead of hanging" $
            withFakeClaude \fake ->
                withEnvironmentVariables
                    [("FAKE_CLAUDE_TERMINAL_ERROR", Just "1")]
                    do
                        transcript <- newIORef []
                        let backend =
                                claudeCodeOneShotBackend
                                    (defaultClaudeCodeOptions
                                        fake.executable
                                        fake.workingDirectory)
                                    (pure defaultResponseCreateParams)
                                    transcript
                        result <- timeout 5_000_000 $
                            backend.submitTurn
                                Nothing
                                [UserMessage "terminal error"]
                                (\_ -> pure ())
                        result `shouldSatisfy` \case
                            Just (Left ProviderError
                                { errorType = ApiErrorType
                                }) -> True
                            _ -> False

        it "accepts end_turn when Claude exits before writing turn_duration" $
            withFakeClaude \fake ->
                withEnvironmentVariables
                    [("FAKE_CLAUDE_EXIT_AFTER_END_TURN", Just "1")]
                    do
                        transcript <- newIORef []
                        let backend =
                                claudeCodeOneShotBackend
                                    (defaultClaudeCodeOptions
                                        fake.executable
                                        fake.workingDirectory)
                                    (pure defaultResponseCreateParams)
                                    transcript
                        result <- timeout 5_000_000 $
                            backend.submitTurn
                                Nothing
                                [UserMessage "exit fallback"]
                                (\_ -> pure ())
                        result `shouldSatisfy` \case
                            Just (Right turn) ->
                                turn.assistantText == Just "fake response"
                            _ -> False

        it "debounces end_turn when a live Claude process omits turn_duration" $
            withFakeClaude \fake ->
                withEnvironmentVariables
                    [("FAKE_CLAUDE_SKIP_TURN_DURATION", Just "1")]
                    do
                        transcript <- newIORef []
                        let backend =
                                claudeCodeOneShotBackend
                                    (defaultClaudeCodeOptions
                                        fake.executable
                                        fake.workingDirectory)
                                    (pure defaultResponseCreateParams)
                                    transcript
                        result <- timeout 5_000_000 $
                            backend.submitTurn
                                Nothing
                                [UserMessage "grace fallback"]
                                (\_ -> pure ())
                        result `shouldSatisfy` \case
                            Just (Right turn) ->
                                turn.assistantText == Just "fake response"
                            _ -> False

        it "imports existing host history into a fresh Claude session" $
            withFakeClaude \fake -> do
                transcript <- newIORef
                    (turnInputsToItems [UserMessage "older context"])
                result <- timeout 5_000_000 $
                    withClaudeCodeBackend
                        (defaultClaudeCodeOptions
                            fake.executable
                            fake.workingDirectory)
                        Nothing
                        (pure defaultResponseCreateParams)
                        transcript
                        \backend ->
                            backend.submitTurn
                                Nothing
                                [UserMessage "continued request"]
                                (\_ -> pure ())
                result `shouldSatisfy` \case
                    Just (Right _) -> True
                    _ -> False
                submitted <- readFile fake.promptLog
                submitted `shouldContain`
                    "Prior conversation imported from the outer agent harness"
                submitted `shouldContain` "older context"
                submitted `shouldContain` "continued request"

        it "resumes a Claude UUID without re-injecting host history" $
            withFakeClaude \fake -> do
                let priorSessionId =
                        "00000000-0000-4000-8000-000000000001"
                transcript <- newIORef
                    (turnInputsToItems [UserMessage "must not be re-sent"])
                result <- timeout 5_000_000 $
                    withClaudeCodeBackend
                        (defaultClaudeCodeOptions
                            fake.executable
                            fake.workingDirectory)
                        (Just priorSessionId)
                        (pure defaultResponseCreateParams)
                        transcript
                        \backend ->
                            backend.submitTurn
                                Nothing
                                [UserMessage "resumed request"]
                                (\_ -> pure ())
                result `shouldSatisfy` \case
                    Just (Right turn) ->
                        turn.responseId == priorSessionId
                    _ -> False
                submitted <- readFile fake.promptLog
                submitted `shouldNotContain` "must not be re-sent"
                starts <- lines <$> readFile fake.startLog
                starts `shouldBe`
                    ["resume " <> Text.unpack priorSessionId]

        it "reuses a process, resumes on model or effort changes, and starts a fresh UUID after reset" $
            withFakeClaude \fake -> do
                transcript <- newIORef []
                paramsRef <- newIORef
                    defaultResponseCreateParams
                        { model = Just "sonnet" }
                let options =
                        defaultClaudeCodeOptions
                            fake.executable
                            fake.workingDirectory
                    submit
                        :: Backend
                        -> Maybe Text
                        -> Text
                        -> IO (Either ApiError TurnOutput)
                    submit backend previous prompt =
                        backend.submitTurn
                            previous
                            [UserMessage prompt]
                            (\_ -> pure ())
                turns <- timeout 5_000_000 $
                    withClaudeCodeBackend
                        options
                        Nothing
                        (readIORef paramsRef)
                        transcript
                        \backend -> do
                            first <- expectTurn =<<
                                submit backend Nothing "one"
                            second <- expectTurn =<<
                                submit backend
                                    (Just first.responseId)
                                    "two"
                            writeIORef paramsRef
                                defaultResponseCreateParams
                                    { model = Just "opus" }
                            afterModelChange <- expectTurn =<<
                                submit backend
                                    (Just second.responseId)
                                    "three"
                            writeIORef paramsRef
                                defaultResponseCreateParams
                                    { model = Just "opus"
                                    , reasoning = Just ReasoningConfig
                                        { context = Nothing
                                        , effort = Just "high"
                                        , generateSummary = Nothing
                                        , reasoningMode = Nothing
                                        , summary = Nothing
                                        , extraFields = mempty
                                        }
                                    }
                            afterEffortChange <- expectTurn =<<
                                submit backend
                                    (Just afterModelChange.responseId)
                                    "four"
                            let switchedSessionId =
                                    "00000000-0000-4000-8000-000000000002"
                            afterSessionSwitch <- expectTurn =<<
                                submit backend
                                    (Just switchedSessionId)
                                    "five"
                            afterReset <- expectTurn =<<
                                submit backend Nothing "six"
                            pure
                                ( first
                                , second
                                , afterModelChange
                                , afterEffortChange
                                , afterSessionSwitch
                                , afterReset
                                )
                ( first
                    , second
                    , afterModelChange
                    , afterEffortChange
                    , afterSessionSwitch
                    , afterReset
                    ) <-
                    maybe
                        (expectationFailure "multi-turn fake timed out"
                            >> fail "unreachable")
                        pure
                        turns
                second.responseId `shouldBe` first.responseId
                afterModelChange.responseId `shouldBe` first.responseId
                afterEffortChange.responseId `shouldBe` first.responseId
                afterSessionSwitch.responseId `shouldBe`
                    "00000000-0000-4000-8000-000000000002"
                afterReset.responseId `shouldNotBe`
                    afterSessionSwitch.responseId
                starts <- lines <$> readFile fake.startLog
                starts `shouldBe`
                    [ "new " <> Text.unpack first.responseId
                    , "resume " <> Text.unpack first.responseId
                    , "resume " <> Text.unpack first.responseId
                    , "resume 00000000-0000-4000-8000-000000000002"
                    , "new " <> Text.unpack afterReset.responseId
                    ]

data FakeClaude = FakeClaude
    { executable :: !FilePath
    , workingDirectory :: !FilePath
    , promptLog :: !FilePath
    , startLog :: !FilePath
    , argumentLog :: !FilePath
    }

withFakeClaude :: (FakeClaude -> IO a) -> IO a
withFakeClaude action =
    withScratchDirectory "agent-claude-code-test" \root -> do
        let executable = root </> "fake-claude"
            workingDirectory = root </> "work"
            configDirectory = root </> "config"
            promptLog = root </> "prompt.log"
            startLog = root </> "start.log"
            argumentLog = root </> "arguments.log"
        createDirectory workingDirectory
        createDirectory configDirectory
        writeFile executable
            (fakeClaudeScript promptLog startLog argumentLog)
        setFileMode executable $
            ownerReadMode
                `unionFileModes` ownerWriteMode
                `unionFileModes` ownerExecuteMode
        withEnvironmentVariables
            [ ("CLAUDE_CONFIG_DIR", Just configDirectory)
            , ("ANTHROPIC_API_KEY", Just "must-not-leak")
            , ("ANTHROPIC_AUTH_TOKEN", Just "must-not-leak")
            , ("ANTHROPIC_FUTURE_OVERRIDE", Just "must-not-leak")
            , ("CLAUDE_CODE_USE_BEDROCK", Just "1")
            , ("CLAUDE_CODE_USE_VERTEX", Just "1")
            , ("CLAUDE_CODE_USE_FOUNDRY", Just "1")
            , ("CLAUDE_CODE_API_BASE_URL", Just "https://example.invalid")
            , ("AWS_BEARER_TOKEN_BEDROCK", Just "must-not-leak")
            , ("AWS_ACCESS_KEY_ID", Just "tool-credential")
            ] $
            action FakeClaude{..}

withScratchDirectory :: String -> (FilePath -> IO a) -> IO a
withScratchDirectory prefix =
    bracket acquire removeDirectoryRecursive
  where
    acquire = do
        temporary <- getTemporaryDirectory
        (path, handle) <- openTempFile temporary prefix
        hClose handle
        removeFile path
        createDirectory path
        pure path

withEnvironmentVariables
    :: [(String, Maybe String)]
    -> IO a
    -> IO a
withEnvironmentVariables variables action = do
    previous <- mapM
        (\(name, _) -> do
            value <- lookupEnv name
            pure (name, value))
        variables
    mapM_ (uncurry install) variables
    action `finally` mapM_ (uncurry install) previous
  where
    install name = \case
        Nothing -> unsetEnv name
        Just value -> setEnv name value

fakeClaudeScript :: FilePath -> FilePath -> FilePath -> String
fakeClaudeScript promptLog startLog argumentLog =
    unlines
        [ "#!/bin/sh"
        , "test -t 0 && test -t 1 && test -t 2 || exit 42"
        , "test \"$AWS_ACCESS_KEY_ID\" = tool-credential || exit 44"
        , "if env | grep -E '^(ANTHROPIC_|CLAUDE_CODE_USE_(BEDROCK|VERTEX|FOUNDRY)=|CLAUDE_CODE_API_BASE_URL=|AWS_BEARER_TOKEN_BEDROCK=)' >/dev/null; then"
        , "  exit 45"
        , "fi"
        , "printf '<%s>\\n' \"$@\" >> " <> shellQuote argumentLog
        , "session_id=''"
        , "start_mode=''"
        , "while [ \"$#\" -gt 0 ]; do"
        , "  case \"$1\" in"
        , "    --session-id)"
        , "      start_mode='new'"
        , "      session_id=\"$2\""
        , "      shift 2"
        , "      ;;"
        , "    --resume)"
        , "      start_mode='resume'"
        , "      session_id=\"$2\""
        , "      shift 2"
        , "      ;;"
        , "    --model|--effort|--permission-mode|--disallowedTools|--tools)"
        , "      shift 2"
        , "      ;;"
        , "    *) shift ;;"
        , "  esac"
        , "done"
        , "test -n \"$session_id\" || exit 43"
        , "printf '%s %s\\n' \"$start_mode\" \"$session_id\" >> " <> shellQuote startLog
        , "slug=$(printf '%s' \"$PWD\" | sed 's/[^A-Za-z0-9]/-/g')"
        , "transcript_dir=\"$CLAUDE_CONFIG_DIR/projects/$slug\""
        , "mkdir -p \"$transcript_dir\""
        , "transcript=\"$transcript_dir/$session_id.jsonl\""
        , "printf '\\n$\\033[2G'"
        , "while IFS= read -r prompt; do"
        , "  printf '%s\\n' \"$prompt\" >> " <> shellQuote promptLog
        , "  if [ \"$FAKE_CLAUDE_TERMINAL_ERROR\" = 1 ]; then"
        , "    printf '%s\\n' '{\"type\":\"system\",\"subtype\":\"api_error\",\"retryAttempt\":1,\"maxRetries\":10,\"error\":{\"status\":401,\"message\":\"login expired\"}}' >> \"$transcript\""
        , "    continue"
        , "  fi"
        , "  printf '%s\\n' '{\"type\":\"assistant\",\"message\":{\"id\":\"fake-tool-message\",\"content\":[{\"type\":\"tool_use\",\"id\":\"fake-tool\",\"name\":\"Read\",\"input\":{\"file_path\":\"README.md\"}}],\"stop_reason\":\"tool_use\"}}' >> \"$transcript\""
        , "  printf '%s\\n' '{\"type\":\"user\",\"message\":{\"content\":[{\"type\":\"tool_result\",\"tool_use_id\":\"fake-tool\",\"content\":\"fake contents\"}]}}' >> \"$transcript\""
        , "  printf '%s' '{\"type\":\"assistant\",\"message\":{\"id\":\"fake-message\",\"content\":[{\"type\":\"text\",\"text\":\"fake response\"}],\"stop_reason\":\"end_turn\",\"usage\":{\"input_tokens\":2,\"cache_creation_input_tokens\":3,\"cache_read_input_tokens\":5,\"output_tokens\":7}}}' >> \"$transcript\""
        , "  if [ \"$FAKE_CLAUDE_EXIT_AFTER_END_TURN\" = 1 ]; then"
        , "    exit 0"
        , "  fi"
        , "  printf '\\n' >> \"$transcript\""
        , "  if [ \"$FAKE_CLAUDE_SKIP_TURN_DURATION\" = 1 ]; then"
        , "    continue"
        , "  fi"
        , "  printf '%s\\n' '{\"type\":\"system\",\"subtype\":\"turn_duration\",\"durationMs\":1}' >> \"$transcript\""
        , "  printf '\\n$\\033[2G'"
        , "done"
        ]

shellQuote :: String -> String
shellQuote value =
    "'" <> concatMap escape value <> "'"
  where
    escape '\'' = "'\"'\"'"
    escape character = [character]

responseMessageText :: ResponseMessage -> Text.Text
responseMessageText message =
    case message.content of
        MessageContentText text -> text
        MessageContentParts parts ->
            Text.intercalate "\n"
                [ text
                | part <- parts
                , text <- case part of
                    InputTextPart{text} -> [text]
                    OutputTextPart{text} -> [text]
                    _ -> []
                ]

expectedFakeToolCall :: ToolCall
expectedFakeToolCall = ToolCall
    { callId = "fake-tool"
    , name = "Read"
    , arguments = "{\"file_path\":\"README.md\"}"
    , callKind = FunctionCallKind
    , argumentsEncrypted = False
    }

expectedFakeToolResult :: ToolCallResult
expectedFakeToolResult = ToolCallResult
    { callId = "fake-tool"
    , output = "fake contents"
    , callKind = FunctionCallKind
    }

looksLikeUuid :: Text.Text -> Bool
looksLikeUuid value =
    case Text.splitOn "-" value of
        [a, b, c, d, e] ->
            map Text.length [a, b, c, d, e] == [8, 4, 4, 4, 12]
                && Text.all isHex valueWithoutDashes
        _ -> False
  where
    valueWithoutDashes = Text.filter (/= '-') value
    isHex character =
        character >= '0' && character <= '9'
            || character >= 'a' && character <= 'f'
            || character >= 'A' && character <= 'F'

expectTurn :: Either ApiError TurnOutput -> IO TurnOutput
expectTurn = \case
    Left err ->
        expectationFailure ("expected successful turn, got " <> show err)
            >> fail "unreachable"
    Right turn ->
        pure turn
