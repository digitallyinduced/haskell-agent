module Agent.Claude.LoopBackendSpec (spec) where

import Agent.Claude.LoopBackend
    ( appendHostTranscript
    , claudeCodeOneShotBackend
    , withClaudeCodeBackend
    )
import Agent.Claude.Options
    ( ClaudeCodeOptions(..)
    , ClaudeCodePermission(..)
    , defaultClaudeCodeOptions
    )
import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Json (rawJsonBytes)
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , FileAttachment(..)
    , ImageAttachment(..)
    , LoopEvent(..)
    , TokenUsage(..)
    , TurnInput(..)
    , TurnOutput(..)
    )
import Agent.Responses.LoopBackend (turnInputsToItems)
import Agent.Responses.Types
    ( FunctionCall(..)
    , FunctionCallOutput(..)
    , MessageContent(..)
    , ReasoningConfig(..)
    , ResponseContentPart(..)
    , ResponseCreateParams(..)
    , ResponseItem(..)
    , ResponseMessage(..)
    , ResponseRole(..)
    , defaultResponseCreateParams
    )
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolCallResult(..)
    )
import Control.Exception.Safe (bracket, finally)
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
    , doesFileExist
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

submitBackend
    :: Backend
    -> Maybe Text
    -> [TurnInput]
    -> (LoopEvent -> IO ())
    -> IO (Either ApiError TurnOutput)
submitBackend backend previous inputs onEvent =
    fmap (.backendOutput) <$> backend.submitTurn [] previous inputs onEvent

spec :: Spec
spec = do
    describe "appendHostTranscript" do
        it "appends turn inputs followed by assistant text" $ do
            let history =
                    appendHostTranscript
                        []
                        [UserMessage "hello", UserMessage "world"]
                        (Just "response")
            map responseMessageText
                [message | MessageItem message <- history]
                `shouldBe` ["hello", "world", "response"]

        it "preserves existing history and uses empty text when absent" $ do
            let initial = turnInputsToItems [UserMessage "prior"]
                history = appendHostTranscript initial [] Nothing
            map responseMessageText
                [message | MessageItem message <- history]
                `shouldBe` ["prior", ""]

    describe "Claude Code loop backend" do
        it "renders validated structured JSONL and persists normalized host messages" $
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
                            submitBackend backend
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
                turn.tokenUsage.inputTokens `shouldBe` 10
                turn.tokenUsage.outputTokens `shouldBe` 7
                turn.tokenUsage.cachedTokens `shouldBe` 5
                observedEvents <- readIORef events
                observedEvents `shouldBe`
                    [ ToolStarted expectedFakeToolCall
                    , ToolFinished expectedFakeToolResult
                    , TextDelta "fake response"
                    ]

                history <- readIORef transcript
                map responseMessageText
                    [message | MessageItem message <- history]
                    `shouldBe`
                        [ "hello"
                        , "fake response"
                        ]
                let persistedCalls =
                        [ (call.callId, call.name, call.arguments)
                        | FunctionCallItem call <- history
                        ]
                    persistedOutputs =
                        [ output.callId
                        | FunctionCallOutputItem output <- history
                        ]
                persistedCalls `shouldBe`
                    [("fake-tool", "Read", "{\"file_path\":\"README.md\"}")]
                persistedOutputs `shouldBe` ["fake-tool"]
                length history `shouldBe` 4

                submitted <- readFile fake.promptLog
                submitted `shouldContain`
                    "Instructions supplied by the outer agent harness"
                submitted `shouldContain` "hello"
                submitted `shouldContain` "\"type\":\"user\""
                submitted `shouldContain` "\"role\":\"user\""
                submitted `shouldContain` "\"parent_tool_use_id\":null"
                arguments <- readFile fake.argumentLog
                arguments `shouldContain` "<-p>"
                arguments `shouldContain`
                    "<--input-format>\n<stream-json>"
                arguments `shouldContain`
                    "<--output-format>\n<stream-json>"
                arguments `shouldNotContain` "<--include-partial-messages>"
                arguments `shouldContain` "<--verbose>"
                arguments `shouldContain`
                    "<--disallowedTools>\n<AskUserQuestion>"
                arguments `shouldContain`
                    "<--permission-mode>\n<dontAsk>"
                arguments `shouldContain` "<--safe-mode>"
                arguments `shouldContain` "<--disable-slash-commands>"
                arguments `shouldContain` "<--strict-mcp-config>"
                arguments `shouldContain`
                    "<--mcp-config>\n<{\"mcpServers\":{}}>"
                arguments `shouldContain`
                    "<--setting-sources>\n<>"
                arguments `shouldContain` "<--no-chrome>"
                arguments `shouldNotContain` "<--ax-screen-reader>"

        it "publishes Claude Task starts before the terminal result" $
            withFakeClaude \fake -> do
                transcript <- newIORef []
                observedBeforeResult <- newIORef []
                let resultMarker =
                        fake.workingDirectory <> "/result-emitted"
                result <- withEnvironmentVariables
                    [ ("FAKE_CLAUDE_TOOL_NAME", Just "Task")
                    , ("FAKE_CLAUDE_PAUSE_AFTER_TOOL", Just "1")
                    , ("FAKE_CLAUDE_RESULT_MARKER", Just resultMarker)
                    ]
                    $ timeout 5_000_000
                    $ withClaudeCodeBackend
                        (defaultClaudeCodeOptions
                            fake.executable
                            fake.workingDirectory)
                        Nothing
                        (pure defaultResponseCreateParams)
                        transcript
                        \backend ->
                            submitBackend backend
                                Nothing
                                [UserMessage "spawn a reviewer"]
                                \case
                                    ToolStarted call
                                        | call.name == "Task" -> do
                                            resultAlreadyEmitted <-
                                                doesFileExist resultMarker
                                            modifyIORef'
                                                observedBeforeResult
                                                (<> [not resultAlreadyEmitted])
                                    _ -> pure ()
                result `shouldSatisfy` \case
                    Just (Right _) -> True
                    _ -> False
                readIORef observedBeforeResult `shouldReturn` [True]

        it "retracts superseded live tools without committing them" $
            withFakeClaude \fake ->
                withEnvironmentVariables
                    [("FAKE_CLAUDE_RETRACT_TOOL", Just "1")]
                    do
                        transcript <- newIORef []
                        events <- newIORef []
                        result <- timeout 5_000_000 $
                            withClaudeCodeBackend
                                (defaultClaudeCodeOptions
                                    fake.executable
                                    fake.workingDirectory)
                                Nothing
                                (pure defaultResponseCreateParams)
                                transcript
                                \backend ->
                                    submitBackend backend
                                        Nothing
                                        [UserMessage "replace the tool"]
                                        (\event ->
                                            modifyIORef' events (<> [event]))
                        result `shouldSatisfy` \case
                            Just (Right _) -> True
                            _ -> False
                        observed <- readIORef events
                        observed `shouldContain`
                            [ ToolStarted expectedFakeToolCall
                            , ToolRetracted "fake-tool"
                            ]
                        history <- readIORef transcript
                        [call | FunctionCallItem call <- history]
                            `shouldBe` []

        it "streams interim text and thinking live and persists the whole reply" $
            withFakeClaude \fake ->
                withEnvironmentVariables
                    [("FAKE_CLAUDE_INTERIM", Just "1")]
                    do
                        transcript <- newIORef []
                        events <- newIORef []
                        result <- timeout 5_000_000 $
                            withClaudeCodeBackend
                                (defaultClaudeCodeOptions
                                    fake.executable
                                    fake.workingDirectory)
                                Nothing
                                (pure defaultResponseCreateParams)
                                transcript
                                \backend ->
                                    submitBackend backend
                                        Nothing
                                        [UserMessage "read it"]
                                        (\event ->
                                            modifyIORef' events (<> [event]))
                        turn <- case result of
                            Just (Right value) -> pure value
                            other -> do
                                expectationFailure
                                    ("unexpected turn: " <> show other)
                                fail "unreachable"
                        observed <- readIORef events
                        observed `shouldBe`
                            [ ReasoningDelta "weighing options"
                            , TextDelta "Let me read it."
                            , ToolStarted expectedFakeToolCall
                            , ToolFinished expectedFakeToolResult
                            , TextDelta "fake response"
                            ]
                        -- Claude Code's result record only carries the last
                        -- text block; the turn keeps everything, and the
                        -- host transcript preserves the order seen live.
                        turn.assistantText
                            `shouldBe` Just "Let me read it.\n\nfake response"
                        history <- readIORef transcript
                        map historyTag history
                            `shouldBe`
                                [ "user:read it"
                                , "assistant:Let me read it."
                                , "call:fake-tool"
                                , "output:fake-tool"
                                , "assistant:fake response"
                                ]

        it "discards the attempt when displayed text is retracted and replays survivors" $
            withFakeClaude \fake ->
                withEnvironmentVariables
                    [ ("FAKE_CLAUDE_INTERIM", Just "1")
                    , ("FAKE_CLAUDE_RETRACT_TEXT", Just "1")
                    ]
                    do
                        transcript <- newIORef []
                        events <- newIORef []
                        result <- timeout 5_000_000 $
                            withClaudeCodeBackend
                                (defaultClaudeCodeOptions
                                    fake.executable
                                    fake.workingDirectory)
                                Nothing
                                (pure defaultResponseCreateParams)
                                transcript
                                \backend ->
                                    submitBackend backend
                                        Nothing
                                        [UserMessage "read it"]
                                        (\event ->
                                            modifyIORef' events (<> [event]))
                        turn <- case result of
                            Just (Right value) -> pure value
                            other -> do
                                expectationFailure
                                    ("unexpected turn: " <> show other)
                                fail "unreachable"
                        observed <- readIORef events
                        observed `shouldBe`
                            [ ReasoningDelta "weighing options"
                            , TextDelta "Let me read it."
                            , ToolStarted expectedFakeToolCall
                            , ResponseAttemptDiscarded
                            , ReasoningDelta "weighing options"
                            , ToolStarted expectedFakeToolCall
                            , ToolFinished expectedFakeToolResult
                            , TextDelta "fake response"
                            ]
                        turn.assistantText `shouldBe` Just "fake response"
                        history <- readIORef transcript
                        map responseMessageText
                            [message | MessageItem message <- history]
                            `shouldBe` ["read it", "fake response"]

        it "renders and persists structured tool results as text" $
            withFakeClaude \fake ->
                withEnvironmentVariables
                    [("FAKE_CLAUDE_STRUCTURED_RESULT", Just "1")]
                    do
                        transcript <- newIORef []
                        events <- newIORef []
                        result <- timeout 5_000_000 $
                            withClaudeCodeBackend
                                (defaultClaudeCodeOptions
                                    fake.executable
                                    fake.workingDirectory)
                                Nothing
                                (pure defaultResponseCreateParams)
                                transcript
                                \backend ->
                                    submitBackend backend
                                        Nothing
                                        [UserMessage "search tools"]
                                        (\event ->
                                            modifyIORef' events (<> [event]))
                        result `shouldSatisfy` \case
                            Just (Right _) -> True
                            _ -> False
                        observed <- readIORef events
                        observed `shouldContain`
                            [ ToolFinished expectedFakeToolResult
                                { output = "Tool reference: WebFetch\nloaded" }
                            ]
                        history <- readIORef transcript
                        [ rawJsonBytes output.output
                            | FunctionCallOutputItem output <- history
                            ]
                            `shouldBe` ["\"Tool reference: WebFetch\\nloaded\""]

        it "starts from partial tool records and enriches canonical arguments" $
            withFakeClaude \fake ->
                withEnvironmentVariables
                    [("FAKE_CLAUDE_PARTIAL_TOOL", Just "1")]
                    do
                        transcript <- newIORef []
                        events <- newIORef []
                        _ <- timeout 5_000_000 $
                            withClaudeCodeBackend
                                (defaultClaudeCodeOptions
                                    fake.executable
                                    fake.workingDirectory)
                                Nothing
                                (pure defaultResponseCreateParams)
                                transcript
                                \backend ->
                                    submitBackend backend
                                        Nothing
                                        [UserMessage "read it"]
                                        (\event ->
                                            modifyIORef' events (<> [event]))
                        observed <- readIORef events
                        map eventTag observed `shouldContain`
                            ["start:{}", "update:{\"file_path\":\"README.md\"}"]

        it "forwards pasted images as Claude image content blocks" $
            withFakeClaude \fake -> do
                transcript <- newIORef []
                result <- timeout 5_000_000 $
                    withClaudeCodeBackend
                        (defaultClaudeCodeOptions
                            fake.executable
                            fake.workingDirectory)
                        Nothing
                        (pure defaultResponseCreateParams)
                        transcript
                        \backend ->
                            submitBackend backend
                                Nothing
                                [ UserMultimodal
                                    { userText = "describe this image"
                                    , userImages =
                                        [ ImageAttachment
                                            { imageMime = "image/png"
                                            , imageBytes = "png-bytes"
                                            }
                                        ]
                                    }
                                ]
                                (\_ -> pure ())
                result `shouldSatisfy` \case
                    Just (Right _) -> True
                    _ -> False

                submitted <- readFile fake.promptLog
                submitted `shouldContain` "\"type\":\"image\""
                submitted `shouldContain` "\"type\":\"base64\""
                submitted `shouldContain`
                    "\"media_type\":\"image/png\""
                submitted `shouldContain`
                    "\"data\":\"cG5nLWJ5dGVz\""
                submitted `shouldContain` "describe this image"

                history <- readIORef transcript
                case history of
                    MessageItem ResponseMessage
                        { content = MessageContentParts
                            ( InputTextPart{text}
                            : InputImagePart{imageUrl}
                            : _
                            )
                        }
                        : _ -> do
                            text `shouldBe` "describe this image"
                            imageUrl `shouldBe`
                                Just
                                    "data:image/png;base64,cG5nLWJ5dGVz"
                    _ ->
                        expectationFailure
                            "multimodal user input was not persisted"

        it "includes attached files in the Claude prompt fallback" $
            withFakeClaude \fake -> do
                transcript <- newIORef []
                result <- timeout 5_000_000 $ do
                    let filePath = fake.workingDirectory <> "/attachment.txt"
                    writeFile filePath "file-bytes"
                    withClaudeCodeBackend
                        (defaultClaudeCodeOptions
                            fake.executable
                            fake.workingDirectory)
                        Nothing
                        (pure defaultResponseCreateParams)
                        transcript
                        \backend ->
                            submitBackend backend
                                Nothing
                                [ UserMultimodalFiles
                                    { userText = "describe this file"
                                    , userImages = []
                                    , userFiles =
                                        [ FileAttachment
                                            { fileName = Just "attachment.txt"
                                            , fileMime = "text/plain"
                                            , fileBytes = "file-bytes"
                                            }
                                        ]
                                    }
                                ]
                                (\_ -> pure ())
                result `shouldSatisfy` \case
                    Just (Right _) -> True
                    _ -> False
                submitted <- readFile fake.promptLog
                submitted `shouldContain` "[Attached file]"
                submitted `shouldContain` "attachment.txt"
                submitted `shouldContain` "describe this file"

        it "converts cumulative modelUsage snapshots to per-turn deltas" $
            withFakeClaude \fake -> do
                transcript <- newIORef []
                turns <- timeout 5_000_000 $
                    withClaudeCodeBackend
                        (defaultClaudeCodeOptions
                            fake.executable
                            fake.workingDirectory)
                        Nothing
                        (pure defaultResponseCreateParams)
                        transcript
                        \backend -> do
                            first <- expectTurn =<<
                                submitBackend backend
                                    Nothing
                                    [UserMessage "one"]
                                    (\_ -> pure ())
                            second <- expectTurn =<<
                                submitBackend backend
                                    (Just first.responseId)
                                    [UserMessage "two"]
                                    (\_ -> pure ())
                            pure (first, second)
                (first, second) <-
                    maybe
                        (expectationFailure "usage fake timed out"
                            >> fail "unreachable")
                        pure
                        turns
                first.tokenUsage `shouldBe` TokenUsage
                    { inputTokens = 10
                    , outputTokens = 7
                    , cachedTokens = 5
                    }
                second.tokenUsage `shouldBe` first.tokenUsage

        it "does not recount fallback usage when cumulative modelUsage resumes" $
            withFakeClaude \fake ->
                withEnvironmentVariables
                    [("FAKE_CLAUDE_OMIT_MODEL_USAGE_TURN", Just "2")]
                    do
                        transcript <- newIORef []
                        turns <- timeout 5_000_000 $
                            withClaudeCodeBackend
                                (defaultClaudeCodeOptions
                                    fake.executable
                                    fake.workingDirectory)
                                Nothing
                                (pure defaultResponseCreateParams)
                                transcript
                                \backend -> do
                                    first <- expectTurn =<<
                                        submitBackend backend
                                            Nothing
                                            [UserMessage "one"]
                                            (\_ -> pure ())
                                    second <- expectTurn =<<
                                        submitBackend backend
                                            (Just first.responseId)
                                            [UserMessage "two"]
                                            (\_ -> pure ())
                                    third <- expectTurn =<<
                                        submitBackend backend
                                            (Just second.responseId)
                                            [UserMessage "three"]
                                            (\_ -> pure ())
                                    pure [first, second, third]
                        completed <-
                            maybe
                                (expectationFailure "usage recovery fake timed out"
                                    >> fail "unreachable")
                                pure
                                turns
                        map (.tokenUsage) completed `shouldBe`
                            replicate 3 (TokenUsage
                                { inputTokens = 10
                                , outputTokens = 7
                                , cachedTokens = 5
                                })

        it "maps bypass permission mode and can disable safe mode" $
            withFakeClaude \fake -> do
                transcript <- newIORef []
                let options =
                        (defaultClaudeCodeOptions
                            fake.executable
                            fake.workingDirectory)
                            { permission = ClaudeCodeBypass
                            , safeMode = False
                            }
                    backend =
                        claudeCodeOneShotBackend
                            options
                            (pure defaultResponseCreateParams)
                            transcript
                result <- timeout 5_000_000 $
                    submitBackend backend
                        Nothing
                        [UserMessage "bypass"]
                        (\_ -> pure ())
                result `shouldSatisfy` \case
                    Just (Right _) -> True
                    _ -> False
                arguments <- readFile fake.argumentLog
                arguments `shouldContain`
                    "<--allow-dangerously-skip-permissions>"
                arguments `shouldContain`
                    "<--permission-mode>\n<bypassPermissions>"
                arguments `shouldNotContain` "<--safe-mode>"
                arguments `shouldNotContain` "<--disable-slash-commands>"
                arguments `shouldContain` "<--setting-sources>\n<>"
                arguments `shouldContain` "<--strict-mcp-config>"
                arguments `shouldContain`
                    "<--mcp-config>\n<{\"mcpServers\":{}}>"
                arguments `shouldContain` "<--no-chrome>"

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
                    submitBackend backend
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
                    submitBackend backend
                        Nothing
                        [UserMessage "hello"]
                        (\_ -> pure ())
                result `shouldSatisfy` \case
                    Just (Right _) -> True
                    _ -> False
                arguments <- readFile fake.argumentLog
                arguments `shouldNotContain` "<--effort>"

        it "returns terminal result errors instead of hanging" $
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
                            submitBackend backend
                                Nothing
                                [UserMessage "terminal error"]
                                (\_ -> pure ())
                        result `shouldSatisfy` \case
                            Just (Left ProviderError
                                { errorType = ApiErrorType
                                , message
                                }) ->
                                    "login expired" `Text.isInfixOf` message
                            _ -> False

        it "rejects non-subscription auth as soon as init arrives" $
            withFakeClaude \fake ->
                withEnvironmentVariables
                    [ ( "FAKE_CLAUDE_API_KEY_SOURCE"
                      , Just "ANTHROPIC_API_KEY"
                      )
                    , ("FAKE_CLAUDE_BLOCK_AFTER_INIT", Just "1")
                    ]
                    do
                        transcript <- newIORef []
                        events <- newIORef []
                        let backend =
                                claudeCodeOneShotBackend
                                    (defaultClaudeCodeOptions
                                        fake.executable
                                        fake.workingDirectory)
                                    (pure defaultResponseCreateParams)
                                    transcript
                        result <- timeout 5_000_000 $
                            submitBackend backend
                                Nothing
                                [UserMessage "must not be API billed"]
                                (\event ->
                                    modifyIORef' events (<> [event]))
                        result `shouldSatisfy` \case
                            Just (Left ProviderError
                                { errorType = ApiErrorType
                                , message
                                }) ->
                                    "non-subscription credential source"
                                        `Text.isInfixOf` message
                                        && "ANTHROPIC_API_KEY"
                                            `Text.isInfixOf` message
                            _ -> False
                        readIORef events `shouldReturn` []

        it "ignores subscription metadata from nested subagent records" $
            withFakeClaude \fake ->
                withEnvironmentVariables
                    [("FAKE_CLAUDE_NESTED_API_KEY_INIT", Just "1")]
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
                            submitBackend backend
                                Nothing
                                [UserMessage "nested auth metadata"]
                                (\_ -> pure ())
                        result `shouldSatisfy` \case
                            Just (Right turn) ->
                                turn.assistantText == Just "fake response"
                            _ -> False

        it "rejects a result for a different Claude session" $
            withFakeClaude \fake ->
                withEnvironmentVariables
                    [ ( "FAKE_CLAUDE_RESULT_SESSION_ID"
                      , Just "00000000-0000-4000-8000-000000000009"
                      )
                    ]
                    do
                        transcript <- newIORef []
                        events <- newIORef []
                        let backend =
                                claudeCodeOneShotBackend
                                    (defaultClaudeCodeOptions
                                        fake.executable
                                        fake.workingDirectory)
                                    (pure defaultResponseCreateParams)
                                    transcript
                        result <- timeout 5_000_000 $
                            submitBackend backend
                                Nothing
                                [UserMessage "wrong session"]
                                (\event ->
                                    modifyIORef' events (<> [event]))
                        result `shouldSatisfy` \case
                            Just (Left ProviderError
                                { errorType = ApiErrorType
                                , message
                                }) ->
                                    "00000000-0000-4000-8000-000000000009"
                                        `Text.isInfixOf` message
                                        && "was active"
                                            `Text.isInfixOf` message
                            _ -> False
                        -- Text is exposed as it arrives; the failed turn
                        -- discards the whole displayed attempt.
                        readIORef events `shouldReturn`
                            [ ToolStarted expectedFakeToolCall
                            , ToolFinished expectedFakeToolResult
                            , TextDelta "fake response"
                            , ResponseAttemptDiscarded
                            ]

        it "reports malformed structured output" $
            withFakeClaude \fake ->
                withEnvironmentVariables
                    [("FAKE_CLAUDE_MALFORMED", Just "1")]
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
                            submitBackend backend
                                Nothing
                                [UserMessage "malformed"]
                                (\_ -> pure ())
                        result `shouldSatisfy` \case
                            Just (Left JsonDecodeError{}) -> True
                            _ -> False

        it "reports EOF before the result record" $
            withFakeClaude \fake ->
                withEnvironmentVariables
                    [("FAKE_CLAUDE_EXIT_EARLY", Just "1")]
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
                            submitBackend backend
                                Nothing
                                [UserMessage "exit early"]
                                (\_ -> pure ())
                        result `shouldSatisfy` \case
                            Just (Left (ConnectionError message)) ->
                                "before completing the turn"
                                    `Text.isInfixOf` message
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
                            submitBackend backend
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
                            submitBackend backend
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

        it "starts fresh for a non-Claude previous response ID" $
            withFakeClaude \fake -> do
                transcript <- newIORef
                    (turnInputsToItems
                        [UserMessage "foreign provider history"])
                result <- timeout 5_000_000 $
                    withClaudeCodeBackend
                        (defaultClaudeCodeOptions
                            fake.executable
                            fake.workingDirectory)
                        (Just "resp_foreign_provider")
                        (pure defaultResponseCreateParams)
                        transcript
                        \backend ->
                            submitBackend backend
                                Nothing
                                [UserMessage "new Claude request"]
                                (\_ -> pure ())
                turn <- case result of
                    Just (Right completed) -> pure completed
                    other -> do
                        expectationFailure
                            ("expected a successful fresh turn, got "
                                <> show other)
                        fail "unreachable"
                turn.responseId `shouldSatisfy` looksLikeUuid
                starts <- lines <$> readFile fake.startLog
                starts `shouldBe`
                    ["new " <> Text.unpack turn.responseId]
                submitted <- readFile fake.promptLog
                submitted `shouldContain` "foreign provider history"
                submitted `shouldContain` "new Claude request"

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
                        submitBackend backend
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
                            -- The CLI traverses the committed transcript before
                            -- the next prompt. Entering the lazy append must not
                            -- look like a host-side rollback.
                            committed <- readIORef transcript
                            committed `seq` pure ()
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
                                        }
                                    }
                            afterEffortChange <- expectTurn =<<
                                submit backend
                                    (Just afterModelChange.responseId)
                                    "four"
                            let switchedSessionId =
                                    "00000000-0000-4000-8000-000000000002"
                            writeIORef transcript $
                                turnInputsToItems
                                    [UserMessage "switched session history"]
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

        it "starts fresh when the host rolls back a completed turn" $
            withFakeClaude \fake -> do
                let initialHistory =
                        turnInputsToItems
                            [UserMessage "retained-history-marker"]
                transcript <- newIORef initialHistory
                turns <- timeout 5_000_000 $
                    withClaudeCodeBackend
                        (defaultClaudeCodeOptions
                            fake.executable
                            fake.workingDirectory)
                        Nothing
                        (pure defaultResponseCreateParams)
                        transcript
                        \backend -> do
                            first <- expectTurn =<<
                                submitBackend backend
                                    Nothing
                                    [UserMessage "rolled-back-prompt-marker"]
                                    (\_ -> pure ())
                            -- This is the same rollback performed by the host
                            -- when cancellation wins immediately after the
                            -- backend has returned a successful turn.
                            writeIORef transcript initialHistory
                            second <- expectTurn =<<
                                submitBackend backend
                                    (Just (Text.toUpper first.responseId))
                                    [UserMessage "replacement-prompt-marker"]
                                    (\_ -> pure ())
                            pure (first, second)
                (first, second) <-
                    maybe
                        (expectationFailure "rollback recovery fake timed out"
                            >> fail "unreachable")
                        pure
                        turns
                second.responseId `shouldNotBe` first.responseId
                starts <- lines <$> readFile fake.startLog
                starts `shouldBe`
                    [ "new " <> Text.unpack first.responseId
                    , "new " <> Text.unpack second.responseId
                    ]
                submitted <- lines <$> readFile fake.promptLog
                length submitted `shouldBe` 2
                let replacementPrompt = last submitted
                replacementPrompt `shouldContain`
                    "retained-history-marker"
                replacementPrompt `shouldContain`
                    "replacement-prompt-marker"
                replacementPrompt `shouldNotContain`
                    "rolled-back-prompt-marker"

        it "times out blocked prompt writes and starts fresh" $
            withFakeClaude \fake ->
                withEnvironmentVariables
                    [("FAKE_CLAUDE_BLOCK_AFTER_FIRST_TURN", Just "1")]
                    do
                        transcript <- newIORef []
                        let options =
                                (defaultClaudeCodeOptions
                                    fake.executable
                                    fake.workingDirectory)
                                    { promptWriteTimeoutMicros = 200_000 }
                            blockedPrompt =
                                Text.replicate (4 * 1024 * 1024) "x"
                        turns <- timeout 8_000_000 $
                            withClaudeCodeBackend
                                options
                                Nothing
                                (pure defaultResponseCreateParams)
                                transcript
                                \backend -> do
                                    first <- expectTurn =<<
                                        submitBackend backend
                                            Nothing
                                            [UserMessage "one"]
                                            (\_ -> pure ())
                                    failed <-
                                        submitBackend backend
                                            (Just first.responseId)
                                            [UserMessage blockedPrompt]
                                            (\_ -> pure ())
                                    third <- expectTurn =<<
                                        submitBackend backend
                                            (Just first.responseId)
                                            [UserMessage "three"]
                                            (\_ -> pure ())
                                    pure (first, failed, third)
                        (first, failed, third) <-
                            maybe
                                (expectationFailure "write-timeout fake timed out"
                                    >> fail "unreachable")
                                pure
                                turns
                        failed `shouldSatisfy` \case
                            Left (ConnectionError message) ->
                                "prompt-write timeout"
                                    `Text.isInfixOf` message
                            _ -> False
                        third.responseId `shouldNotBe` first.responseId
                        starts <- lines <$> readFile fake.startLog
                        starts `shouldBe`
                            [ "new " <> Text.unpack first.responseId
                            , "new " <> Text.unpack third.responseId
                            ]
                        history <- readIORef transcript
                        map responseMessageText
                            [message | MessageItem message <- history]
                            `shouldBe`
                                [ "one"
                                , "fake response"
                                , "three"
                                , "fake response"
                                ]

        it "starts fresh after a failed turn in an established session" $
            withFakeClaude \fake ->
                withEnvironmentVariables
                    [("FAKE_CLAUDE_FAIL_TURN", Just "2")]
                    do
                        transcript <- newIORef []
                        turns <- timeout 5_000_000 $
                            withClaudeCodeBackend
                                (defaultClaudeCodeOptions
                                    fake.executable
                                    fake.workingDirectory)
                                Nothing
                                (pure defaultResponseCreateParams)
                                transcript
                                \backend -> do
                                    first <- expectTurn =<<
                                        submitBackend backend
                                            Nothing
                                            [UserMessage "one"]
                                            (\_ -> pure ())
                                    failed <-
                                        submitBackend backend
                                            (Just first.responseId)
                                            [UserMessage "two"]
                                            (\_ -> pure ())
                                    resumed <- expectTurn =<<
                                        submitBackend backend
                                            (Just first.responseId)
                                            [UserMessage "three"]
                                            (\_ -> pure ())
                                    pure (first, failed, resumed)
                        (first, failed, resumed) <-
                            maybe
                                (expectationFailure "recovery fake timed out"
                                    >> fail "unreachable")
                                pure
                                turns
                        failed `shouldSatisfy` \case
                            Left ProviderError
                                { errorType = ApiErrorType
                                , message
                                } ->
                                    "login expired"
                                        `Text.isInfixOf` message
                            _ -> False
                        resumed.responseId `shouldNotBe` first.responseId
                        starts <- lines <$> readFile fake.startLog
                        starts `shouldBe`
                            [ "new " <> Text.unpack first.responseId
                            , "new " <> Text.unpack resumed.responseId
                            ]
                        submitted <- readFile fake.promptLog
                        submitted `shouldContain`
                            "Prior conversation imported from the outer agent harness"

        it "starts fresh after a turn callback throws" $
            withFakeClaude \fake -> do
                transcript <- newIORef []
                turns <- timeout 5_000_000 $
                    withClaudeCodeBackend
                        (defaultClaudeCodeOptions
                            fake.executable
                            fake.workingDirectory)
                        Nothing
                        (pure defaultResponseCreateParams)
                        transcript
                        \backend -> do
                            first <- expectTurn =<<
                                submitBackend backend
                                    Nothing
                                    [UserMessage "one"]
                                    (\_ -> pure ())
                            failed <-
                                submitBackend backend
                                    (Just first.responseId)
                                    [UserMessage "two"]
                                    (\_ -> ioError (userError "renderer failed"))
                            resumed <- expectTurn =<<
                                submitBackend backend
                                    (Just first.responseId)
                                    [UserMessage "three"]
                                    (\_ -> pure ())
                            pure (first, failed, resumed)
                (first, failed, resumed) <-
                    maybe
                        (expectationFailure "callback recovery fake timed out"
                            >> fail "unreachable")
                        pure
                        turns
                failed `shouldSatisfy` \case
                    Left (ConnectionError message) ->
                        "renderer failed" `Text.isInfixOf` message
                    _ -> False
                resumed.responseId `shouldNotBe` first.responseId
                starts <- lines <$> readFile fake.startLog
                starts `shouldBe`
                    [ "new " <> Text.unpack first.responseId
                    , "new " <> Text.unpack resumed.responseId
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
    withScratchDirectory "agent-claude-test" \root -> do
        let executable = root </> "fake-claude"
            workingDirectory = root </> "work"
            promptLog = root </> "prompt.log"
            startLog = root </> "start.log"
            argumentLog = root </> "arguments.log"
        createDirectory workingDirectory
        writeFile executable
            (fakeClaudeScript promptLog startLog argumentLog)
        setFileMode executable $
            ownerReadMode
                `unionFileModes` ownerWriteMode
                `unionFileModes` ownerExecuteMode
        withEnvironmentVariables
            [ ("ANTHROPIC_API_KEY", Just "must-not-leak")
            , ("ANTHROPIC_AUTH_TOKEN", Just "must-not-leak")
            , ("ANTHROPIC_FUTURE_OVERRIDE", Just "must-not-leak")
            , ("CLAUDE_CODE_USE_BEDROCK", Just "1")
            , ("CLAUDE_CODE_USE_VERTEX", Just "1")
            , ("CLAUDE_CODE_USE_FOUNDRY", Just "1")
            , ("CLAUDE_CODE_USE_ANTHROPIC_AWS", Just "1")
            , ("CLAUDE_CODE_USE_ANTHROPIC_GOOGLE_CLOUD", Just "1")
            , ("CLAUDE_CODE_USE_GATEWAY", Just "1")
            , ("CLAUDE_CODE_USE_MANTLE", Just "1")
            , ("CLAUDE_CODE_PROVIDER_MANAGED_BY_HOST", Just "1")
            , ("CLAUDE_CODE_API_BASE_URL", Just "https://example.invalid")
            , ("CLAUDE_CODE_OAUTH_TOKEN", Just "must-not-leak")
            , ("CLAUDE_CODE_API_KEY_FILE_DESCRIPTOR", Just "9")
            , ("CLAUDE_CODE_HOST_CREDS_FILE", Just "/tmp/credentials")
            , ("CLAUDE_CODE_HFI_BEARER_TOKEN", Just "must-not-leak")
            , ( "_CLAUDE_CODE_ASSUME_FIRST_PARTY_BASE_URL"
              , Just "1"
              )
            , ("AGENT_PROXY_URL", Just "https://example.invalid")
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
        , "test ! -t 0 && test ! -t 1 && test ! -t 2 || exit 42"
        , "test \"$AWS_ACCESS_KEY_ID\" = tool-credential || exit 44"
        , "test \"$ENABLE_CLAUDEAI_MCP_SERVERS\" = 0 || exit 46"
        , "test \"$CLAUDE_CODE_ENTRYPOINT\" = sdk-cli || exit 47"
        , "test \"$CLAUDE_AGENT_SDK_CLIENT_APP\" = haskell-agent || exit 49"
        , "if env | grep -E '^(ANTHROPIC_|_CLAUDE_CODE_ASSUME_FIRST_PARTY_BASE_URL=|AGENT_PROXY_URL=|AWS_BEARER_TOKEN_BEDROCK=)' >/dev/null; then"
        , "  exit 45"
        , "fi"
        , "if env | grep '^CLAUDE_CODE_' | grep -v '^CLAUDE_CODE_ENTRYPOINT=sdk-cli$' >/dev/null; then"
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
        , "    --resume=*)"
        , "      start_mode='resume'"
        , "      session_id=${1#--resume=}"
        , "      shift"
        , "      ;;"
        , "    --model|--effort|--permission-mode|--disallowedTools|--tools|--input-format|--output-format|--setting-sources|--mcp-config)"
        , "      shift 2"
        , "      ;;"
        , "    *) shift ;;"
        , "  esac"
        , "done"
        , "test -n \"$session_id\" || exit 43"
        , "printf '%s %s\\n' \"$start_mode\" \"$session_id\" >> " <> shellQuote startLog
        , "turn=0"
        , "while IFS= read -r prompt; do"
        , "  turn=$((turn + 1))"
        , "  printf '%s\\n' \"$prompt\" >> " <> shellQuote promptLog
        , "  printf '%s' \"$prompt\" | grep '\"type\":\"user\"' >/dev/null || exit 48"
        , "  if [ \"$FAKE_CLAUDE_EXIT_EARLY\" = 1 ]; then"
        , "    exit 17"
        , "  fi"
        , "  if [ \"$FAKE_CLAUDE_MALFORMED\" = 1 ]; then"
        , "    printf '%s\\n' '{not-json'"
        , "    continue"
        , "  fi"
        , "  api_key_source=${FAKE_CLAUDE_API_KEY_SOURCE:-none}"
        , "  printf '{\"type\":\"system\",\"subtype\":\"init\",\"uuid\":\"init-%s\",\"session_id\":\"%s\",\"apiKeySource\":\"%s\"}\\n' \"$turn\" \"$session_id\" \"$api_key_source\""
        , "  if [ \"$FAKE_CLAUDE_NESTED_API_KEY_INIT\" = 1 ]; then"
        , "    printf '{\"type\":\"system\",\"subtype\":\"init\",\"uuid\":\"nested-init-%s\",\"session_id\":\"%s\",\"parent_tool_use_id\":\"agent-tool\",\"apiKeySource\":\"ANTHROPIC_API_KEY\"}\\n' \"$turn\" \"$session_id\""
        , "  fi"
        , "  if [ \"$FAKE_CLAUDE_BLOCK_AFTER_INIT\" = 1 ]; then sleep 30; fi"
        , "  fail_turn=0"
        , "  if [ \"$FAKE_CLAUDE_TERMINAL_ERROR\" = 1 ]; then fail_turn=1; fi"
        , "  if [ -n \"$FAKE_CLAUDE_FAIL_TURN\" ] && [ \"$FAKE_CLAUDE_FAIL_TURN\" = \"$turn\" ]; then fail_turn=1; fi"
        , "  if [ \"$fail_turn\" = 1 ]; then"
        , "    printf '{\"type\":\"result\",\"subtype\":\"error_during_execution\",\"uuid\":\"error-%s\",\"session_id\":\"%s\",\"is_error\":true,\"api_error_status\":401,\"errors\":[\"login expired\"],\"usage\":{}}\\n' \"$turn\" \"$session_id\""
        , "    continue"
        , "  fi"
        , "  printf '{\"type\":\"stream_event\",\"uuid\":\"message-start-%s\",\"session_id\":\"%s\",\"event\":{\"type\":\"message_start\",\"message\":{\"id\":\"message-%s\"}}}\\n' \"$turn\" \"$session_id\" \"$turn\""
        , "  printf '{\"type\":\"stream_event\",\"uuid\":\"delta-a-%s\",\"session_id\":\"%s\",\"event\":{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"fake \"}}}\\n' \"$turn\" \"$session_id\""
        , "  printf '{\"type\":\"stream_event\",\"uuid\":\"delta-b-%s\",\"session_id\":\"%s\",\"event\":{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"response\"}}}\\n' \"$turn\" \"$session_id\""
        , "  if [ \"$FAKE_CLAUDE_PARTIAL_TOOL\" = 1 ]; then"
        , "    printf '{\"type\":\"stream_event\",\"uuid\":\"partial-tool-%s\",\"session_id\":\"%s\",\"event\":{\"type\":\"content_block_start\",\"index\":1,\"content_block\":{\"type\":\"tool_use\",\"id\":\"fake-tool\",\"name\":\"Read\",\"input\":{}}}}\\n' \"$turn\" \"$session_id\""
        , "  fi"
        , "  if [ \"$FAKE_CLAUDE_INTERIM\" = 1 ]; then"
        , "    printf '{\"type\":\"assistant\",\"uuid\":\"thinking-%s\",\"session_id\":\"%s\",\"message\":{\"content\":[{\"type\":\"thinking\",\"thinking\":\"weighing options\",\"signature\":\"sig\"}]}}\\n' \"$turn\" \"$session_id\""
        , "    printf '{\"type\":\"assistant\",\"uuid\":\"interim-%s\",\"session_id\":\"%s\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"Let me read it.\"}]}}\\n' \"$turn\" \"$session_id\""
        , "  fi"
        , "  tool_name=${FAKE_CLAUDE_TOOL_NAME:-Read}"
        , "  printf '{\"type\":\"assistant\",\"uuid\":\"tool-%s\",\"session_id\":\"%s\",\"message\":{\"content\":[{\"type\":\"tool_use\",\"id\":\"fake-tool\",\"name\":\"%s\",\"input\":{\"file_path\":\"README.md\"}}]}}\\n' \"$turn\" \"$session_id\" \"$tool_name\""
        , "  if [ \"$FAKE_CLAUDE_RETRACT_TOOL\" = 1 ]; then"
        , "    printf '{\"type\":\"system\",\"subtype\":\"model_refusal_fallback\",\"uuid\":\"retract-%s\",\"session_id\":\"%s\",\"retracted_message_uuids\":[\"tool-%s\"]}\\n' \"$turn\" \"$session_id\" \"$turn\""
        , "  fi"
        , "  if [ \"$FAKE_CLAUDE_RETRACT_TEXT\" = 1 ]; then"
        , "    printf '{\"type\":\"system\",\"subtype\":\"model_refusal_fallback\",\"uuid\":\"retract-text-%s\",\"session_id\":\"%s\",\"retracted_message_uuids\":[\"interim-%s\"]}\\n' \"$turn\" \"$session_id\" \"$turn\""
        , "  fi"
        , "  if [ \"$FAKE_CLAUDE_PAUSE_AFTER_TOOL\" = 1 ]; then sleep 1; fi"
        , "  if [ \"$FAKE_CLAUDE_STRUCTURED_RESULT\" = 1 ]; then"
        , "    printf '{\"type\":\"user\",\"uuid\":\"tool-result-%s\",\"session_id\":\"%s\",\"message\":{\"content\":[{\"type\":\"tool_result\",\"tool_use_id\":\"fake-tool\",\"content\":[{\"type\":\"tool_reference\",\"tool_name\":\"WebFetch\"},{\"type\":\"text\",\"text\":\"loaded\"}]}]}}\\n' \"$turn\" \"$session_id\""
        , "  else"
        , "    printf '{\"type\":\"user\",\"uuid\":\"tool-result-%s\",\"session_id\":\"%s\",\"message\":{\"content\":[{\"type\":\"tool_result\",\"tool_use_id\":\"fake-tool\",\"content\":\"fake contents\"}]}}\\n' \"$turn\" \"$session_id\""
        , "  fi"
        , "  printf '{\"type\":\"assistant\",\"uuid\":\"assistant-%s\",\"session_id\":\"%s\",\"message\":{\"id\":\"message-%s\",\"content\":[{\"type\":\"text\",\"text\":\"fake response\"}]}}\\n' \"$turn\" \"$session_id\" \"$turn\""
        , "  result_session_id=${FAKE_CLAUDE_RESULT_SESSION_ID:-$session_id}"
        , "  if [ -n \"$FAKE_CLAUDE_RESULT_MARKER\" ]; then : > \"$FAKE_CLAUDE_RESULT_MARKER\"; fi"
        , "  cumulative_input=$((turn * 2))"
        , "  cumulative_cache_creation=$((turn * 3))"
        , "  cumulative_cache_read=$((turn * 5))"
        , "  cumulative_output=$((turn * 7))"
        , "  if [ \"$FAKE_CLAUDE_OMIT_MODEL_USAGE_TURN\" = \"$turn\" ]; then"
        , "    printf '{\"type\":\"result\",\"subtype\":\"success\",\"uuid\":\"result-%s\",\"session_id\":\"%s\",\"is_error\":false,\"result\":\"fake response\",\"usage\":{\"input_tokens\":2,\"cache_creation_input_tokens\":3,\"cache_read_input_tokens\":5,\"output_tokens\":7}}\\n' \"$turn\" \"$result_session_id\""
        , "  else"
        , "    printf '{\"type\":\"result\",\"subtype\":\"success\",\"uuid\":\"result-%s\",\"session_id\":\"%s\",\"is_error\":false,\"result\":\"fake response\",\"usage\":{\"input_tokens\":2,\"cache_creation_input_tokens\":3,\"cache_read_input_tokens\":5,\"output_tokens\":7},\"modelUsage\":{\"fake-model\":{\"inputTokens\":%s,\"cacheCreationInputTokens\":%s,\"cacheReadInputTokens\":%s,\"outputTokens\":%s}}}\\n' \"$turn\" \"$result_session_id\" \"$cumulative_input\" \"$cumulative_cache_creation\" \"$cumulative_cache_read\" \"$cumulative_output\""
        , "  fi"
        , "  if [ \"$FAKE_CLAUDE_BLOCK_AFTER_FIRST_TURN\" = 1 ] && [ \"$turn\" = 1 ]; then"
        , "    sleep 30"
        , "  fi"
        , "done"
        ]

shellQuote :: String -> String
shellQuote value =
    "'" <> concatMap escape value <> "'"
  where
    escape '\'' = "'\"'\"'"
    escape character = [character]

responseMessageText :: ResponseMessage -> Text
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

historyTag :: ResponseItem -> Text
historyTag = \case
    MessageItem message
        | message.role == RoleAssistant ->
            "assistant:" <> responseMessageText message
        | otherwise -> "user:" <> responseMessageText message
    FunctionCallItem call -> "call:" <> call.callId
    FunctionCallOutputItem output -> "output:" <> output.callId
    _ -> "other"

eventTag :: LoopEvent -> Text
eventTag = \case
    ToolStarted call -> "start:" <> call.arguments
    ToolUpdated call -> "update:" <> call.arguments
    _ -> ""

looksLikeUuid :: Text -> Bool
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
