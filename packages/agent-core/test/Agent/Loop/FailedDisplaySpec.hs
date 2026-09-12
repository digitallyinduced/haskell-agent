module Agent.Loop.FailedDisplaySpec (spec) where

import Agent.Error (ApiError(..))
import Agent.Loop
import Agent.Loop.Fixtures
import Agent.ToolDispatch
import Data.IORef
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
    it "keeps the latest adjacent argument or metadata snapshot in its original order" do
        let first = functionToolCall "c1" "read_file" "first"
            final = functionToolCall "c1" "grep" "final"
            events =
                [ ToolStarted first
                , ToolArgumentsUpdated first
                , ToolUpdated final
                , ToolArgumentsUpdated final
                , TextDelta "after"
                ]
        let backend = Backend \_state _prev _inputs onEvent -> do
                mapM_ onEvent events
                pure (Left (ConnectionError "down"))
        config <- testConfig backend
        execution <- runLoopInputsDetailed config Nothing [UserMessage "hello"]
        execution.executionUncommittedDisplayEvents
            `shouldBe` [ToolStarted first, ToolArgumentsUpdated final, TextDelta "after"]

    it "does not collapse snapshot kinds or reused ids across restart boundaries" do
        let call = functionToolCall "same" "read_file" "{}"
            events =
                [ ToolUpdated call
                , ToolOutputUpdated "same" "old"
                , ToolOutputUpdated "same" "latest"
                , ResponseRestarted "retry"
                , ToolArgumentsUpdated call
                , ToolArgumentsUpdated (setToolCallArguments "new" call)
                ]
            backend = Backend \_state _prev _inputs onEvent -> do
                mapM_ onEvent events
                pure (Left (ConnectionError "down"))
        config <- testConfig backend
        execution <- runLoopInputsDetailed config Nothing [UserMessage "hello"]
        execution.executionUncommittedDisplayEvents `shouldBe`
            [ ToolUpdated call
            , ToolOutputUpdated "same" "latest"
            , ResponseRestarted "retry"
            , ToolArgumentsUpdated (setToolCallArguments "new" call)
            ]

    it "marks a transport failure after streamed output as interrupted" do
        observed <- newIORef []
        let backend = Backend \_state _prev _inputs onEvent -> do
                onEvent (TextDelta "partial")
                pure (Left (ConnectionError "down"))
        config0 <- testConfig backend
        let config = config0
                { loopOnEvent = \event ->
                    modifyIORef' observed (<> [event])
                }
        execution <-
            runLoopInputsDetailed config Nothing [UserMessage "hello"]
        execution.executionResult `shouldBe`
            Left (LoopTransportAfterOutput (ConnectionError "down"))
        execution.executionUncommittedDisplayEvents
            `shouldBe` [TextDelta "partial"]
        readIORef observed `shouldReturn`
            [TurnStarted, TextDelta "partial", ResponseAttemptFailed]

    it "coalesces retained text without crossing tool boundaries" do
        let call =
                functionToolCall "c1" "shell_command"
                    "{\"command\":\"git status\"}"
            backend = Backend \_state _prev _inputs onEvent -> do
                onEvent (TextDelta "before")
                onEvent (TextDelta " tool")
                onEvent (ToolStarted call)
                onEvent (TextDelta "after")
                onEvent (TextDelta " tool")
                pure (Left (ConnectionError "down"))
        config <- testConfig backend
        execution <-
            runLoopInputsDetailed config Nothing [UserMessage "hello"]
        execution.executionUncommittedDisplayEvents
            `shouldBe`
                [ TextDelta "before tool"
                , ToolStarted call
                , TextDelta "after tool"
                ]

    it "retains tool-only activity as display metadata on failure" do
        let call =
                functionToolCall "c1" "shell_command"
                    "{\"command\":\"git status\"}"
            backend = Backend \_state _prev _inputs onEvent -> do
                onEvent (ToolStarted call)
                onEvent (ToolOutputUpdated "c1" "still running")
                pure (Left (ConnectionError "down"))
        config <- testConfig backend
        execution <-
            runLoopInputsDetailed config Nothing [UserMessage "hello"]
        execution.executionResult `shouldBe`
            Left (LoopTransportAfterOutput (ConnectionError "down"))
        execution.executionUncommittedDisplayEvents
            `shouldBe`
                [ ToolStarted call
                , ToolOutputUpdated "c1" "still running"
                ]

    it "keeps earlier restarted attempts in display metadata" do
        let backend = Backend \_state _prev _inputs onEvent -> do
                onEvent (TextDelta "fir")
                onEvent (TextDelta "st")
                onEvent (ResponseRestarted "retrying")
                onEvent (TextDelta "sec")
                onEvent (TextDelta "ond")
                pure (Left (ConnectionError "down"))
        config <- testConfig backend
        execution <-
            runLoopInputsDetailed config Nothing [UserMessage "hello"]
        execution.executionUncommittedDisplayEvents
            `shouldBe`
                [ TextDelta "first"
                , ResponseRestarted "retrying"
                , TextDelta "second"
                ]
        execution.executionUncommittedAssistantText
            `shouldBe` Just "first\n\nsecond"

    it "still marks failure after a later retry attempt is discarded" do
        observed <- newIORef []
        let backend = Backend \_state _prev _inputs onEvent -> do
                onEvent (TextDelta "fir")
                onEvent (TextDelta "st")
                onEvent (ResponseRestarted "retrying")
                onEvent (TextDelta "discard")
                onEvent (TextDelta " me")
                onEvent ResponseAttemptDiscarded
                pure (Left (ConnectionError "down"))
        config0 <- testConfig backend
        let config = config0
                { loopOnEvent = \event ->
                    modifyIORef' observed (<> [event])
                }
        execution <-
            runLoopInputsDetailed config Nothing [UserMessage "hello"]
        execution.executionResult `shouldBe`
            Left (LoopTransportAfterOutput (ConnectionError "down"))
        execution.executionUncommittedDisplayEvents
            `shouldBe`
                [ TextDelta "first"
                , ResponseRestarted "retrying"
                ]
        observedEvents <- readIORef observed
        filter
            (\case
                TextDelta _ -> False
                _ -> True)
            observedEvents
            `shouldBe`
                [ TurnStarted
                , ResponseRestarted "retrying"
                , ResponseAttemptDiscarded
                , ResponseAttemptFailed
                ]

    it "treats a transport failure after a discarded attempt as pre-output" do
        let backend = Backend \_state _prev _inputs onEvent -> do
                onEvent (TextDelta "partial")
                onEvent ResponseAttemptDiscarded
                pure (Left (ConnectionError "down"))
        config <- testConfig backend
        result <- runLoop config Nothing "hello"
        result `shouldBe` Left (LoopTransport (ConnectionError "down"))

    it "replaces snapshots at their latest positions without losing intervening text" do
        let call = functionToolCall "c1" "shell_command" "{}"
            updated = setToolCallArguments "{\"command\":\"pwd\"}" call
            finished = ToolCallResult
                "c1" "done" FunctionCallKind BlockingToolCall [] Nothing
            events =
                [ ToolStarted call
                , ToolUpdated call
                , TextDelta "first"
                , ToolArgumentsUpdated updated
                , ToolOutputUpdated "c1" "old"
                , TextDelta "second"
                , ToolOutputUpdated "c1" "new"
                , ToolFinished finished
                ]
            backend = Backend \_state _prev _inputs onEvent -> do
                mapM_ onEvent events
                pure (Left (ConnectionError "down"))
        config <- testConfig backend
        execution <- runLoopInputsDetailed config Nothing [UserMessage "hello"]
        execution.executionUncommittedDisplayEvents `shouldBe`
            [ ToolStarted call
            , TextDelta "first"
            , ToolArgumentsUpdated updated
            , TextDelta "second"
            , ToolFinished finished
            ]

    it "scopes retraction and discard to the latest attempt when tool IDs repeat" do
        let call = functionToolCall "same" "shell_command" "{}"
            events =
                [ ToolStarted call
                , ToolOutputUpdated "same" "retained"
                , ResponseRestarted "retry"
                , ToolStarted call
                , ToolOutputUpdated "same" "removed"
                , ToolRetracted "same"
                , TextDelta "discarded"
                , ResponseAttemptDiscarded
                , ResponseAttemptDiscarded
                , ToolStarted call
                , ToolOutputUpdated "same" "latest"
                ]
            backend = Backend \_state _prev _inputs onEvent -> do
                mapM_ onEvent events
                pure (Left (ConnectionError "down"))
        config <- testConfig backend
        execution <- runLoopInputsDetailed config Nothing [UserMessage "hello"]
        execution.executionUncommittedDisplayEvents `shouldBe`
            [ ToolStarted call
            , ToolOutputUpdated "same" "retained"
            , ResponseRestarted "retry"
            , ToolStarted call
            , ToolOutputUpdated "same" "latest"
            ]

    it "resumes the latest text chunk after retracting a trailing tool" do
        let call = functionToolCall "c1" "shell_command" "{}"
            backend = Backend \_state _prev _inputs onEvent -> do
                mapM_ onEvent
                    [ TextDelta "before"
                    , ToolStarted call
                    , ToolOutputUpdated "c1" "removed"
                    , ToolRetracted "c1"
                    , TextDelta " after"
                    ]
                pure (Left (ConnectionError "down"))
        config <- testConfig backend
        execution <- runLoopInputsDetailed config Nothing [UserMessage "hello"]
        execution.executionUncommittedDisplayEvents
            `shouldBe` [TextDelta "before after"]

    it "bounds completed tool output retained after failure" do
        let oversized =
                Text.replicate (3 * 1024 * 1024) "x" <> "newest-tail"
            backend = Backend \_state _prev _inputs onEvent -> do
                onEvent
                    (ToolFinished
                        (ToolCallResult
                            "large"
                            oversized
                            FunctionCallKind BlockingToolCall [] Nothing))
                pure (Left (ConnectionError "down"))
        config <- testConfig backend
        execution <-
            runLoopInputsDetailed config Nothing [UserMessage "hello"]
        case execution.executionUncommittedDisplayEvents of
            [ToolFinished result] -> do
                Text.length result.output
                    `shouldSatisfy` (<= 2 * 1024 * 1024)
                result.output `shouldSatisfy`
                    Text.isPrefixOf "[earlier tool output truncated]"
                result.output `shouldSatisfy`
                    Text.isSuffixOf "newest-tail"
            other ->
                expectationFailure
                    ("unexpected display events: " <> show other)
