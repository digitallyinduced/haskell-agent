module Agent.ToolDispatchSpec (spec) where

import Agent.ToolArgs (objectArgs, reqText)
import Agent.ToolDispatch
import qualified Control.Exception as Exception
import Data.Aeson (FromJSON(..), Value(..))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Test.Hspec

newtype EchoArgs = EchoArgs
    { message :: Text
    } deriving (Show, Eq)

instance FromJSON EchoArgs where
    parseJSON = objectArgs $ \object -> EchoArgs
        <$> reqText object "message"

spec :: Spec
spec = describe "dispatchToolCall" do
    it "decodes typed tool arguments before running the handler" do
        result <- dispatchToolCall testConfig
            [ typedTool "echo" \(EchoArgs message) ->
                pure (Right ("echo:" <> message))
            ]
            (toolCall "call-1" "echo" "{\"message\":\"hello\"}")
        result `shouldBe` ToolCallResult "call-1" "echo:hello"

    it "turns typed decode failures into formatted tool output" do
        result <- dispatchToolCall testConfig
            [ typedTool "echo" \(EchoArgs message) ->
                pure (Right ("echo:" <> message))
            ]
            (toolCall "call-1" "echo" "{}")
        result `shouldBe` ToolCallResult "call-1" "ERR Missing parameter: message"

    it "preserves invalid JSON arguments as a string value" do
        toolArgumentsValue "not-json" `shouldBe` String "not-json"

    it "supports no-argument tools" do
        result <- dispatchToolCall testConfig
            [noArgsTool "ping" (pure (Right "pong"))]
            (toolCall "call-1" "ping" "{this is ignored")
        result `shouldBe` ToolCallResult "call-1" "pong"

    it "formats unknown tools consistently" do
        result <- dispatchToolCall testConfig [] (toolCall "call-1" "missing" "{}")
        result `shouldBe` ToolCallResult "call-1" "ERR unknown:missing"

    it "formats exceptions and invokes the exception hook" do
        seen <- newIORef []
        let config = testConfig
                { toolDispatchOnException = \name _ ->
                    modifyIORef' seen (name :)
                }
        result <- dispatchToolCall config
            [noArgsTool "explode" (Exception.throwIO (userError "boom"))]
            (toolCall "call-1" "explode" "{}")
        result `shouldBe` ToolCallResult "call-1" "EX explode"
        readIORef seen `shouldReturn` ["explode"]

testConfig :: ToolDispatchConfig
testConfig = ToolDispatchConfig
    { toolDispatchUnknownTool = \name -> "unknown:" <> name
    , toolDispatchFormatResult = either ("ERR " <>) id
    , toolDispatchFormatException = \name _ -> "EX " <> name
    , toolDispatchOnException = \_ _ -> pure ()
    }

toolCall :: Text -> Text -> Text -> ToolCall
toolCall callId name arguments = ToolCall { callId, name, arguments }
