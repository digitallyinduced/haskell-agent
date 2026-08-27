module Agent.Tools.CodeMode.ProtocolSpec (spec) where

import Agent.Tools.CodeMode.Protocol
import qualified Agent.Json.Decode as Json
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = describe "code-mode worker protocol" do
    it "decodes a ready notification" do
        decodeProtocolMessage
            "{\"jsonrpc\":\"2.0\",\"method\":\"ready\"}"
            `shouldBe` Right WorkerReady

    it "decodes and validates tool call arguments" do
        let message = LBS.toStrict $ Aeson.encode $ object
                [ "jsonrpc" .= ("2.0" :: String)
                , "id" .= ("tool-1" :: String)
                , "method" .= ("tool/call" :: String)
                , "params" .= object
                    [ "name" .= ("read_file" :: String)
                    , "arguments" .= object ["path" .= ("README.md" :: String)]
                    ]
                ]
        decodeProtocolMessage message `shouldSatisfy` \case
            Right (WorkerToolInvocation invocation) ->
                invocation.invocationId == "tool-1"
                    && invocation.invocationName == "read_file"
            _ -> False

    it "rejects unknown worker methods" do
        decodeProtocolMessage
            "{\"jsonrpc\":\"2.0\",\"method\":\"surprise\"}"
            `shouldSatisfy` \case
                Left _ -> True
                Right _ -> False

    it "ignores unknown protocol fields at every message boundary" do
        decodeProtocolMessage
            "{\"jsonrpc\":\"2.0\",\"method\":\"ready\",\"unexpected\":true}"
            `shouldBe` Right WorkerReady
        decodeProtocolMessage
            "{\"jsonrpc\":\"2.0\",\"method\":\"content\",\"params\":{\"value\":null,\"unexpected\":true}}"
            `shouldBe` Right (WorkerContent Aeson.Null)

    it "decodes content, yield, and notification messages" do
        decodeProtocolMessage
            "{\"jsonrpc\":\"2.0\",\"method\":\"content\",\"params\":{\"value\":{\"type\":\"text\",\"text\":\"partial\"}}}"
            `shouldBe`
                Right (WorkerContent
                    (object
                        [ "type" .= ("text" :: String)
                        , "text" .= ("partial" :: String)
                        ]))
        decodeProtocolMessage
            "{\"jsonrpc\":\"2.0\",\"method\":\"yield\",\"params\":{\"value\":{\"content\":[]}}}"
            `shouldBe`
                Right (WorkerYielded
                    (object ["content" .= ([] :: [Aeson.Value])]))
        decodeProtocolMessage
            "{\"jsonrpc\":\"2.0\",\"method\":\"notify\",\"params\":{\"text\":\"working\"}}"
            `shouldBe` Right (WorkerNotification "working")

    it "decodes successful store writes" do
        decodeProtocolMessage
            "{\"jsonrpc\":\"2.0\",\"id\":\"exec\",\"result\":{\"content\":[]},\"stored_value_writes\":{\"answer\":42}}"
            `shouldBe`
                Right WorkerExecSucceeded
                    { responseId = "exec"
                    , responseValue =
                        object ["content" .= ([] :: [Aeson.Value])]
                    , responseStoredValueWrites =
                        Map.singleton "answer" (Aeson.Number 42)
                    }

    it "decodes failed execution state without discarding partial effects" do
        decodeProtocolMessage
            "{\"jsonrpc\":\"2.0\",\"id\":\"exec\",\"error\":{\"code\":-32000,\"message\":\"boom\"},\"partial_result\":{\"content\":[]},\"stored_value_writes\":{\"candidate\":true}}"
            `shouldBe`
                Right WorkerExecFailed
                    { responseId = "exec"
                    , responseError = "boom"
                    , responseValue =
                        object ["content" .= ([] :: [Aeson.Value])]
                    , responseStoredValueWrites =
                        Map.singleton "candidate" (Aeson.Bool True)
                    }

    it "encodes enabled tool metadata and the session store" do
        let encoded = encodeExecRequestWithState
                "exec"
                "text(load(\"answer\"));"
                [ CodeModeToolMetadata "inspect" "Inspect a value." ]
                (Map.singleton "answer" (Aeson.Number 42))
        Json.decodeEither execRequestProjection encoded
            `shouldBe`
                Right
                    ( [ ("inspect", "Inspect a value.") ]
                    , Map.singleton "answer" 42
                    , True
                    )

execRequestProjection
    :: Json.Decoder ([(Text, Text)], Map.Map Text Int, Bool)
execRequestProjection =
    Json.object $
        Json.atKey "params" $
            (\tools storedValues imageDetailVisible ->
                (tools, storedValues, imageDetailVisible)
            )
                <$> Json.atKey "tools" (Json.list toolProjection)
                <*> Json.atKey "stored_values" (Json.objectAsMap pure Json.int)
                <*> Json.atKey "image_detail_visible" Json.bool
  where
    toolProjection =
        Json.object $
            (,)
                <$> Json.atKey "name" Json.text
                <*> Json.atKey "description" Json.text
