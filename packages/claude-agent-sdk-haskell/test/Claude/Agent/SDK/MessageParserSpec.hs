module Claude.Agent.SDK.MessageParserSpec (spec) where

import Agent.Json (rawJsonBytes)
import Claude.Agent.SDK.Internal.MessageParser (decodeMessageLine)
import Claude.Agent.SDK.Types
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = describe "decodeMessageLine" do
    it "retains autonomous origin identifiers and the raw origin object" do
        let originBytes =
                "{\"kind\":\"task_notification\",\"task_id\":\"task-7\",\
                \\"agent_id\":\"agent-2\",\"attempt\":3}"
            line =
                "{\"type\":\"user\",\"uuid\":\"notification\",\
                \\"origin\":" <> originBytes <> ",\
                \\"message\":{\"role\":\"user\",\"content\":\"done\"}}"
        case decodeMessageLine line of
            Right (MessageUser UserMessage{origin = Just origin}) -> do
                origin.kind `shouldBe` "task_notification"
                origin.identifiers `shouldBe` Map.fromList
                    [ ("agent_id", "agent-2")
                    , ("task_id", "task-7")
                    ]
                rawJsonBytes origin.raw `shouldBe` originBytes
            other ->
                expectationFailure ("unexpected decode: " <> show other)

    describe "tool_result content" do
        it "keeps string content" do
            block <- decodeToolResult "\"hello\""
            block.content `shouldSatisfy` \case
                Just ToolResultContent{raw, renderedText} ->
                    rawJsonBytes raw == "\"hello\""
                        && renderedText == "hello"
                Nothing -> False

        it "joins text blocks from array content" do
            block <- decodeToolResult
                "[{\"type\":\"text\",\"text\":\"first\"},\
                \{\"type\":\"text\",\"text\":\"second\"}]"
            renderedTextOf block `shouldBe` Just "first\nsecond"
            rawBytesOf block `shouldBe`
                Just
                    "[{\"type\":\"text\",\"text\":\"first\"},\
                    \{\"type\":\"text\",\"text\":\"second\"}]"

        it "labels tool references and images" do
            -- Claude Code answers ToolSearch with tool_reference blocks and
            -- image reads with base64 image blocks.
            block <- decodeToolResult
                "[{\"type\":\"tool_reference\",\"tool_name\":\"WebFetch\"},\
                \{\"type\":\"tool_reference\",\"tool_name\":\"WebSearch\"},\
                \{\"type\":\"image\",\"source\":{\"type\":\"base64\",\
                \\"media_type\":\"image/png\",\"data\":\"iVBORw0KGgo=\"}}]"
            renderedTextOf block `shouldBe`
                Just
                    "Tool reference: WebFetch\nTool reference: WebSearch\n\
                    \[image image/png]"

        it "renders objects without text as raw JSON" do
            block <- decodeToolResult
                "[{\"type\":\"mystery\",\"value\":2}]"
            renderedTextOf block `shouldBe`
                Just "{\"type\":\"mystery\",\"value\":2}"

        it "renders mixed arrays including scalars and nested arrays" do
            block <- decodeToolResult
                "[\"plain\",7,true,null,[{\"type\":\"text\",\"text\":\"deep\"}]]"
            renderedTextOf block `shouldBe` Just "plain\n7\ntrue\n\ndeep"

        it "renders a single object with text" do
            block <- decodeToolResult
                "{\"type\":\"text\",\"text\":\"only\"}"
            renderedTextOf block `shouldBe` Just "only"

        it "treats null content as absent" do
            block <- decodeToolResult "null"
            block.content `shouldBe` Nothing

        it "continues decoding sibling fields after array content" do
            block <- decodeToolResult
                "[{\"type\":\"text\",\"text\":\"failed\"}]"
            block.isError `shouldBe` Just True
            block.toolUseId `shouldBe` "tool-1"

        it "decodes array content inside assistant advisor results" do
            let line =
                    "{\"type\":\"assistant\",\"uuid\":\"advisor\",\
                    \\"message\":{\"content\":[{\"type\":\"advisor_tool_result\",\
                    \\"tool_use_id\":\"advisor-1\",\"content\":[{\"type\":\"text\",\
                    \\"text\":\"advice\"}]}]}}"
            case decodeMessageLine line of
                Right
                    (MessageAssistant
                        AssistantMessage
                            { content =
                                [ ServerToolResultBlock
                                    { content = Just ToolResultContent{renderedText}
                                    }
                                ]
                            }) ->
                    renderedText `shouldBe` "advice"
                other ->
                    expectationFailure ("unexpected decode: " <> show other)

decodeToolResult :: ByteString -> IO ContentBlock
decodeToolResult content =
    case decodeMessageLine (userToolResultLine content) of
        Right (MessageUser UserMessage{content = [block@ToolResultBlock{}]}) ->
            pure block
        other -> do
            expectationFailure ("unexpected decode: " <> show other)
            fail "unreachable"

userToolResultLine :: ByteString -> ByteString
userToolResultLine content =
    "{\"type\":\"user\",\"uuid\":\"tool-result\",\
    \\"session_id\":\"123e4567-e89b-42d3-a456-426614174000\",\
    \\"message\":{\"role\":\"user\",\"content\":[{\"type\":\"tool_result\",\
    \\"tool_use_id\":\"tool-1\",\"content\":"
        <> content
        <> ",\"is_error\":true}]}}"

renderedTextOf :: ContentBlock -> Maybe Text
renderedTextOf = \case
    ToolResultBlock{content = Just ToolResultContent{renderedText}} ->
        Just renderedText
    _ -> Nothing

rawBytesOf :: ContentBlock -> Maybe ByteString
rawBytesOf = \case
    ToolResultBlock{content = Just ToolResultContent{raw}} ->
        Just (rawJsonBytes raw)
    _ -> Nothing
