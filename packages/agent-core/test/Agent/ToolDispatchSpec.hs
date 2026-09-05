module Agent.ToolDispatchSpec (spec) where

import qualified Agent.Json.Decode as Json
import Agent.ToolArgs (objectArgs, reqText)
import Agent.ToolDispatch
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , ToolAsyncCapability(..)
    , ToolExecutionPolicy(..)
    , ToolSchema(..)
    , dispatchRegisteredToolCall
    , dispatchRegisteredToolCallDetailed
    , jsonAppTool
    , mkToolRegistry
    , toolAcceptsCall
    , appToolSupportsAsync
    , withAsyncToolCalls
    )
import qualified Control.Exception as Exception
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

newtype EchoArgs = EchoArgs
    { message :: Text
    } deriving (Show, Eq)

echoArgsDecoder :: Json.Decoder EchoArgs
echoArgsDecoder = objectArgs $ \object -> EchoArgs
        <$> reqText object "message"

spec :: Spec
spec = describe "dispatchToolCall" do
    it "defaults constructed calls to blocking and compares call mode" do
        let blocking = functionToolCall "call-1" "echo" "{}"
            asynchronous = withToolCallMode AsyncToolCall blocking
        toolCallMode blocking `shouldBe` BlockingToolCall
        blocking `shouldNotBe` asynchronous

    it "retags tool results without changing their payload or outcome" do
        let blocking =
                ToolCallResultWithOutcome
                    "call-1"
                    "done"
                    FunctionCallKind
                    [ToolResultImage "data:image/png;base64,eA==" Nothing]
                    ToolFailed
            asynchronous =
                withToolCallResultMode AsyncToolCall blocking
        toolCallResultMode asynchronous `shouldBe` AsyncToolCall
        toolCallResultOutcome asynchronous `shouldBe` Just ToolFailed
        toolCallResultImages asynchronous `shouldBe` toolCallResultImages blocking
        withToolCallResultMode BlockingToolCall asynchronous
            `shouldBe` blocking

    it "requires tools to opt in to asynchronous calls" do
        let tool =
                jsonAppTool
                    "echo"
                    "echo"
                    []
                    AlwaysReadOnly
                    (noArgsTool "echo" (pure (Right "ok")))
        appToolSupportsAsync tool `shouldBe` False
        appToolSupportsAsync (withAsyncToolCalls tool) `shouldBe` True

    it "retains success even when successful output looks like an error or denial" do
        mapM_ (\message -> do
            result <- dispatchToolCall testConfig
                [noArgsTool "echo" (pure (Right message))]
                (functionToolCall "call" "echo" "{}")
            result.output `shouldBe` message
            toolCallResultOutcome result `shouldBe` Just ToolSucceeded)
            ["Error: quoted log entry", "tool call rejected by user", "Exit code: 42", "cancelled"]

    it "retains failure independently of custom output formatting" do
        let config = testConfig { toolDispatchFormatResult = either id id }
        result <- dispatchToolCall config [noArgsTool "fail" (pure (Left "plain explanation"))]
            (functionToolCall "call" "fail" "{}")
        result.output `shouldBe` "plain explanation"
        toolCallResultOutcome result `shouldBe` Just ToolFailed

    it "retains the async mode together with the typed outcome" do
        result <- dispatchToolCall testConfig
            [noArgsTool "echo" (pure (Right "ok"))]
            (withToolCallMode AsyncToolCall $
                functionToolCall "call" "echo" "{}")
        toolCallResultMode result `shouldBe` AsyncToolCall
        toolCallResultOutcome result `shouldBe` Just ToolSucceeded

    it "keeps plaintext tool arguments useful in Show output" do
        let rendered =
                show (functionToolCall "call-visible" "echo" "{\"message\":\"hello\"}")
        rendered `shouldContain` "call-visible"
        rendered `shouldContain` "echo"
        rendered `shouldContain` "message"
        rendered `shouldContain` "hello"

    it "redacts encrypted tool arguments from Show output" do
        let secret = "encrypted-tool-argument"
            call = (functionToolCall "call-secret" "collaboration.spawn_agent" secret)
                { argumentsEncrypted = True }
            rendered = show call
        rendered `shouldContain` "call-secret"
        rendered `shouldContain` "collaboration.spawn_agent"
        rendered `shouldContain` "<redacted>"
        rendered `shouldNotContain` "encrypted-tool-argument"

    it "decodes typed tool arguments before running the handler" do
        result <- dispatchToolCall testConfig
            [ typedTool "echo" echoArgsDecoder \(EchoArgs message) ->
                pure (Right ("echo:" <> message))
            ]
            (functionToolCall "call-1" "echo" "{\"message\":\"hello\"}")
        result `shouldBe` functionResult "call-1" "echo:hello"

    it "dispatches current Grok Build public aliases to stable handlers" do
        result <- dispatchToolCall testConfig
            [noArgsTool "run_terminal_cmd" (pure (Right "ran"))]
            (functionToolCall "call-1" "run_terminal_command" "{}")
        result `shouldBe` functionResult "call-1" "ran"

    it "canonicalizes Claude Code built-ins that share a host tool shape" do
        map canonicalToolName
            [ "Bash"
            , "Read"
            , "Edit"
            , "Grep"
            , "TodoWrite"
            , "TaskOutput"
            , "TaskStop"
            , "EnterPlanMode"
            , "ExitPlanMode"
            , "AskUserQuestion"
            ]
            `shouldBe`
                [ "run_terminal_cmd"
                , "read_file"
                , "search_replace"
                , "grep"
                , "todo_write"
                , "get_task_output"
                , "kill_task"
                , "enter_plan_mode"
                , "exit_plan_mode"
                , "ask_user_question"
                ]
        -- Tools without a compatible host equivalent keep their wire names.
        map canonicalToolName ["Write", "Glob", "WebFetch", "Agent", "Task"]
            `shouldBe` ["Write", "Glob", "WebFetch", "Agent", "Task"]

    it "canonicalizes every current Grok Build public tool name" do
        map canonicalToolName
            [ "run_terminal_command"
            , "spawn_subagent"
            , "get_command_or_subagent_output"
            , "wait_commands_or_subagents"
            , "kill_command_or_subagent"
            ]
            `shouldBe`
                [ "run_terminal_cmd"
                , "task"
                , "get_task_output"
                , "wait_tasks"
                , "kill_task"
                ]

    it "leaves tool argument bytes unchanged" do
        let value = toolArgumentsValue
                "{\"prompt\":\"inspect\",\"description\":\"inspect code\",\"background\":false}"
        canonicalToolArguments "spawn_subagent" value
            `shouldBe` value

    it "passes the originating call to call-aware typed handlers" do
        result <- dispatchToolCall testConfig
            [ typedToolWithCall "echo" echoArgsDecoder \call (EchoArgs message) ->
                pure (Right (call.callId <> ":" <> message))
            ]
            (functionToolCall "call-1" "echo" "{\"message\":\"hello\"}")
        result `shouldBe` functionResult "call-1" "call-1:hello"

    it "preserves rich image results without exposing image data in Show" do
        let secretDataUrl = "data:image/png;base64,c2VjcmV0"
        result <- dispatchToolCall testConfig
            [ typedRichToolWithCall "echo" echoArgsDecoder
                \_call (EchoArgs message) ->
                    pure $ Right ToolHandlerResult
                        { resultText = "echo:" <> message
                        , resultImages =
                            [ ToolResultImage
                                { imageUrl = secretDataUrl
                                , imageDetail = Just "high"
                                }
                            ]
                        }
            ]
            (functionToolCall "call-1" "echo" "{\"message\":\"hello\"}")
        result.output `shouldBe` "echo:hello"
        toolCallResultImages result `shouldBe`
            [ToolResultImage secretDataUrl (Just "high")]
        show result `shouldContain` "images = <1>"
        show result `shouldNotContain` "c2VjcmV0"

    it "turns typed decode failures into formatted tool output" do
        result <- dispatchToolCall testConfig
            [ typedTool "echo" echoArgsDecoder \(EchoArgs message) ->
                pure (Right ("echo:" <> message))
            ]
            (functionToolCall "call-1" "echo" "{}")
        result.output `shouldSatisfy` Text.isInfixOf "ERR"
        result.output `shouldSatisfy` Text.isInfixOf "message"

    it "retains invalid argument text for the decoder to reject" do
        toolArgumentsValue "not-json" `shouldBe` "not-json"

    it "lets custom tools decode raw text through a JSON string decoder" do
        result <- dispatchToolCall testConfig
            [ typedTool "patch" Json.text \patch ->
                pure (Right ("patch:" <> patch))
            ]
            (customToolCall "call-1" "patch" "*** Begin Patch")
        result `shouldBe`
            ToolCallResultWithOutcome "call-1" "patch:*** Begin Patch" CustomCallKind [] ToolSucceeded

    it "supports no-argument tools" do
        result <- dispatchToolCall testConfig
            [noArgsTool "ping" (pure (Right "pong"))]
            (functionToolCall "call-1" "ping" "{this is ignored")
        result `shouldBe` functionResult "call-1" "pong"

    it "finalizes formatted output before returning the tool result" do
        seen <- newIORef []
        let config = testConfig
                { toolDispatchFormatResult = either ("formatted:" <>) id
                , toolDispatchFinalizeOutput = \call output -> do
                    modifyIORef' seen (<> [(call.callId, output)])
                    pure ("finalized:" <> output)
                }
        result <- dispatchToolCall config
            [noArgsTool "ping" (pure (Right "pong"))]
            (functionToolCall "call-1" "ping" "{}")
        result `shouldBe` functionResult "call-1" "finalized:pong"
        readIORef seen `shouldReturn` [("call-1", "pong")]

    it "forwards snapshots from streaming typed tools with their call" do
        seen <- newIORef []
        let call = functionToolCall "call-1" "echo" "{\"message\":\"hello\"}"
            config = testConfig
                { toolDispatchOnOutput = \seenCall output ->
                    modifyIORef' seen (<> [(seenCall.callId, output)])
                }
        result <- dispatchToolCall config
            [ typedStreamingTool "echo" echoArgsDecoder \emit (EchoArgs message) -> do
                emit ("partial:" <> message)
                pure (Right ("echo:" <> message))
            ]
            call
        result `shouldBe` functionResult "call-1" "echo:hello"
        readIORef seen `shouldReturn` [("call-1", "partial:hello")]

    it "formats unknown tools consistently" do
        result <- dispatchToolCall testConfig [] (functionToolCall "call-1" "missing" "{}")
        result `shouldBe` withToolCallOutcome (Just ToolFailed) (functionResult "call-1" "ERR unknown:missing")

    it "preserves snapshots and images through typed and freeform streaming adapters" do
        let image = ToolResultImage "data:image/png;base64,aW1hZ2U=" Nothing
            run emit message = do
                emit ("partial:" <> message)
                pure (Right (ToolHandlerResult message [image]))
            handlers =
                [ ( typedStreamingRichTool "echo" echoArgsDecoder
                        (\emit (EchoArgs message) -> run emit message)
                  , functionToolCall "typed" "echo" "{\"message\":\"hello\"}"
                  )
                , ( streamingRichTextTool "echo" run
                  , customToolCall "freeform" "echo" "hello"
                  )
                ]
        mapM_ (\(handler, call) -> do
            snapshots <- newIORef []
            let config = testConfig
                    { toolDispatchOnOutput = \seenCall value ->
                        modifyIORef' snapshots (<> [(seenCall, value)])
                    }
            outcome <- dispatchToolHandlerDetailed config (Just handler) call
            outcome.toolDispatchSucceeded `shouldBe` True
            outcome.toolDispatchResult.output `shouldBe` "hello"
            toolCallResultImages outcome.toolDispatchResult `shouldBe` [image]
            readIORef snapshots `shouldReturn` [(call, "partial:hello")]
            ) handlers

    it "forwards the original call to a passthrough broker without decoding it" do
        calls <- newIORef []
        let call = (customToolCall "broker-call" "collaboration.spawn_agent" "not JSON")
                { argumentsEncrypted = True }
            handler = passthroughTool "spawn_agent" \emit received -> do
                modifyIORef' calls (<> [received])
                emit "forwarded"
                pure (Left "broker rejected request")
        outcome <- dispatchToolCallDetailed testConfig [handler] call
        readIORef calls `shouldReturn` [call]
        outcome.toolDispatchSucceeded `shouldBe` False
        outcome.toolDispatchResult.output `shouldBe` "ERR broker rejected request"

    it "formats exceptions and invokes the exception hook" do
        seen <- newIORef []
        let config = testConfig
                { toolDispatchOnException = \name _ ->
                    modifyIORef' seen (name :)
                }
        result <- dispatchToolCall config
            [noArgsTool "explode" (Exception.throwIO (userError "boom"))]
            (functionToolCall "call-1" "explode" "{}")
        result `shouldBe` withToolCallOutcome (Just ToolFailed) (functionResult "call-1" "EX explode")
        readIORef seen `shouldReturn` ["explode"]

    it "does not let a synchronous exception hook replace the tool failure" do
        let config = testConfig
                { toolDispatchOnException = \_ _ ->
                    Exception.throwIO (userError "logging failed")
                }
        result <- dispatchToolCall config
            [noArgsTool "explode" (Exception.throwIO (userError "boom"))]
            (functionToolCall "call-1" "explode" "{}")
        result `shouldBe` withToolCallOutcome (Just ToolFailed) (functionResult "call-1" "EX explode")

    it "does not turn asynchronous cancellation into tool output" do
        dispatchToolCall testConfig
            [noArgsTool "cancel" (Exception.throwIO Exception.ThreadKilled)]
            (functionToolCall "call-1" "cancel" "{}")
            `shouldThrow` (== Exception.ThreadKilled)

    it "accepts only privileged computer kinds at the hosted handler" do
        let hosted = computerTool (pure (Right "ok"))
            ordinary =
                hosted
                    { appToolSchema = JsonFunctionSchema []
                    }
            call kind = ToolCall
                { callId = "computer-1"
                , name = "computer"
                , arguments = "{}"
                , callKind = kind
                , argumentsEncrypted = False
                }
        map (toolAcceptsCall hosted . call)
            [ FunctionCallKind
            , CustomCallKind
            , ComputerCallKind
            , ComputerFunctionCallKind
            ]
            `shouldBe` [False, False, True, True]
        map (toolAcceptsCall ordinary . call)
            [ FunctionCallKind
            , CustomCallKind
            , ComputerCallKind
            , ComputerFunctionCallKind
            ]
            `shouldBe` [True, True, False, False]

    it "rejects mismatched computer calls in both registry dispatch paths" do
        executions <- newIORef (0 :: Int)
        let hosted = computerTool do
                modifyIORef' executions (+ 1)
                pure (Right "desktop changed")
            registry =
                either (error . Text.unpack) id (mkToolRegistry [hosted])
            spoofed = functionToolCall "computer-1" "computer" "{}"
        result <- dispatchRegisteredToolCall testConfig registry spoofed
        outcome <-
            dispatchRegisteredToolCallDetailed testConfig registry spoofed
        result.output `shouldBe` "ERR unknown:computer"
        outcome.toolDispatchResult.output `shouldBe` "ERR unknown:computer"
        outcome.toolDispatchSucceeded `shouldBe` False
        readIORef executions `shouldReturn` 0

testConfig :: ToolDispatchConfig
testConfig = ToolDispatchConfig
    { toolDispatchUnknownTool = \name -> "unknown:" <> name
    , toolDispatchFormatResult = either ("ERR " <>) id
    , toolDispatchFormatException = \name _ -> "EX " <> name
    , toolDispatchOnException = \_ _ -> pure ()
    , toolDispatchOnOutput = \_ _ -> pure ()
    , toolDispatchFinalizeOutput = \_call output -> pure output
    }

functionResult :: Text -> Text -> ToolCallResult
functionResult callId output = ToolCallResultWithOutcome
    { callId
    , output
    , callKind = FunctionCallKind
    , toolResultImages = []
    , toolResultOutcome = ToolSucceeded
    }

computerTool :: IO (Either Text Text) -> AppTool
computerTool action = AppTool
    { appToolName = "computer"
    , appToolDescription = "Control the desktop."
    , appToolSchema = HostedComputerSchema
    , appToolHandler = noArgsTool "computer" action
    , appToolApproval = AlwaysPrompt
    , appToolExecution = TurnSequential
    , appToolResourceClaims = Nothing
    , appToolAsyncCapability = BlockingOnly
    }
