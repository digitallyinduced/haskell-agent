module Agent.ToolDispatchSpec (spec) where

import Agent.ToolArgs (objectArgs, reqText)
import Agent.ToolDispatch
import qualified Control.Exception as Exception
import Data.Aeson (FromJSON(..), Value(..))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

newtype EchoArgs = EchoArgs
    { message :: Text
    } deriving (Show, Eq)

instance FromJSON EchoArgs where
    parseJSON = objectArgs $ \object -> EchoArgs
        <$> reqText object "message"

spec :: Spec
spec = do
  describe "canonicalToolName" do
    it "accepts provider function namespace prefixes" do
        canonicalToolName "functions.shell_command"
            `shouldBe` "shell_command"

  describe "dispatchToolCall" do
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
            [ typedTool "echo" \(EchoArgs message) ->
                pure (Right ("echo:" <> message))
            ]
            (functionToolCall "call-1" "echo" "{\"message\":\"hello\"}")
        result `shouldBe` functionResult "call-1" "echo:hello"

    it "turns typed decode failures into formatted tool output" do
        result <- dispatchToolCall testConfig
            [ typedTool "echo" \(EchoArgs message) ->
                pure (Right ("echo:" <> message))
            ]
            (functionToolCall "call-1" "echo" "{}")
        result `shouldBe` functionResult "call-1" "ERR Missing parameter: message"

    it "preserves invalid JSON arguments as a string value" do
        toolArgumentsValue "not-json" `shouldBe` String "not-json"

    it "supports no-argument tools" do
        result <- dispatchToolCall testConfig
            [noArgsTool "ping" (pure (Right "pong"))]
            (functionToolCall "call-1" "ping" "{this is ignored")
        result `shouldBe` functionResult "call-1" "pong"

    it "supplies the active nested-tool runtime to context-aware handlers" do
        nestedCalls <- newIORef []
        let runtime = ToolRuntime
                { invokeNestedTool = \call -> do
                    modifyIORef' nestedCalls (<> [call])
                    pure (functionResult call.callId "nested-result")
                }
            config = testConfig { toolDispatchRuntime = Just runtime }
        result <- dispatchToolCall config
            [ typedToolWithRuntimeAndCall "orchestrate"
                \active outerCall (EchoArgs message) -> do
                    nested <- active.invokeNestedTool
                        (functionToolCall
                            (outerCall.callId <> "/nested")
                            "echo"
                            ("{\"message\":\"" <> message <> "\"}"))
                    pure (Right nested.output)
            ]
            (functionToolCall
                "call-outer"
                "orchestrate"
                "{\"message\":\"hello\"}")
        result `shouldBe` functionResult "call-outer" "nested-result"
        map (.name) <$> readIORef nestedCalls `shouldReturn` ["echo"]

    it "fails context-aware handlers outside an active agent loop" do
        result <- dispatchToolCall testConfig
            [ typedToolWithRuntimeAndCall "orchestrate"
                \_runtime _call (EchoArgs message) ->
                    pure (Right message)
            ]
            (functionToolCall
                "call-outer"
                "orchestrate"
                "{\"message\":\"hello\"}")
        result.output
            `shouldSatisfy` Text.isInfixOf "requires an active agent-loop runtime"

    it "forwards snapshots from streaming typed tools with their call" do
        seen <- newIORef []
        let call = functionToolCall "call-1" "echo" "{\"message\":\"hello\"}"
            config = testConfig
                { toolDispatchOnOutput = \seenCall output ->
                    modifyIORef' seen (<> [(seenCall.callId, output)])
                }
        result <- dispatchToolCall config
            [ typedStreamingTool "echo" \emit (EchoArgs message) -> do
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
    , toolDispatchRuntime = Nothing
    }

functionResult :: Text -> Text -> ToolCallResult
functionResult callId output = ToolCallResult
    { callId
    , output
    , callKind = FunctionCallKind
    }
