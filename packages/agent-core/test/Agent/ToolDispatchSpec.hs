module Agent.ToolDispatchSpec (spec) where

import qualified Agent.Json.Decode as Json
import Agent.ToolArgs (objectArgs, reqText)
import Agent.ToolDispatch
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
            ToolCallResult "call-1" "patch:*** Begin Patch" CustomCallKind

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
        result `shouldBe` functionResult "call-1" "ERR unknown:missing"

    it "formats exceptions and invokes the exception hook" do
        seen <- newIORef []
        let config = testConfig
                { toolDispatchOnException = \name _ ->
                    modifyIORef' seen (name :)
                }
        result <- dispatchToolCall config
            [noArgsTool "explode" (Exception.throwIO (userError "boom"))]
            (functionToolCall "call-1" "explode" "{}")
        result `shouldBe` functionResult "call-1" "EX explode"
        readIORef seen `shouldReturn` ["explode"]

    it "does not let a synchronous exception hook replace the tool failure" do
        let config = testConfig
                { toolDispatchOnException = \_ _ ->
                    Exception.throwIO (userError "logging failed")
                }
        result <- dispatchToolCall config
            [noArgsTool "explode" (Exception.throwIO (userError "boom"))]
            (functionToolCall "call-1" "explode" "{}")
        result `shouldBe` functionResult "call-1" "EX explode"

    it "does not turn asynchronous cancellation into tool output" do
        dispatchToolCall testConfig
            [noArgsTool "cancel" (Exception.throwIO Exception.ThreadKilled)]
            (functionToolCall "call-1" "cancel" "{}")
            `shouldThrow` (== Exception.ThreadKilled)

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
functionResult callId output = ToolCallResult
    { callId
    , output
    , callKind = FunctionCallKind
    }
