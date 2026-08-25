module Agent.Responses.StreamAssemblySpec (spec) where

import Agent.Error (ApiError(..))
import Agent.Responses.SSE (parseSseEvents)
import Agent.Responses.StreamAssembly
import Agent.Responses.Types
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "buildStreamResponse" do
    it "merges output_item.done events into the terminal response" do
        events <- expectRight $ parseSseEvents $ Text.intercalate ""
            [ sseBlock "response.output_item.done"
                "{\"type\":\"response.output_item.done\",\"output_index\":0,\"item\":{\"type\":\"function_call\",\"call_id\":\"call-1\",\"name\":\"echo\",\"arguments\":\"{}\"}}"
            , sseBlock "response.completed"
                "{\"type\":\"response.completed\",\"response\":{\"id\":\"resp-1\",\"created_at\":0,\"model\":\"test\",\"status\":\"completed\",\"output\":[]}}"
            ]
        response <- expectRight (buildStreamResponse config events)
        [name | FunctionCallItem FunctionCall { name } <- response.output]
            `shouldBe` ["echo"]

    it "assembles a partial response.done after a function call" do
        events <- expectRight $ parseSseEvents $ Text.intercalate ""
            [ sseBlock "response.created"
                "{\"type\":\"response.created\",\"response\":{\"id\":\"resp-done\"}}"
            , sseBlock "response.output_item.done"
                "{\"type\":\"response.output_item.done\",\"output_index\":0,\"item\":{\"type\":\"function_call\",\"call_id\":\"call-done\",\"name\":\"shell_command\",\"arguments\":\"{\\\"command\\\":\\\"echo done\\\"}\"}}"
            , sseBlock "response.done"
                "{\"type\":\"response.done\",\"response\":{\"usage\":{\"input_tokens\":10,\"output_tokens\":2,\"total_tokens\":12}}}"
            ]
        response <- expectRight
            (buildStreamResponseWithModel config (Just "request-model") events)
        response.responseId `shouldBe` "resp-done"
        response.model `shouldBe` "request-model"
        response.status `shouldBe` ResponseCompleted
        fmap (.totalTokens) response.usage `shouldBe` Just 12
        [name | FunctionCallItem FunctionCall { name } <- response.output]
            `shouldBe` ["shell_command"]

    it "assembles minimal created and completed lifecycle fragments" do
        events <- expectRight $ parseSseEvents $ Text.intercalate ""
            [ sseBlock "response.created"
                "{\"type\":\"response.created\",\"response\":{\"id\":\"resp-partial\"}}"
            , sseBlock "response.completed"
                "{\"type\":\"response.completed\",\"response\":{\"id\":\"resp-partial\",\"usage\":{\"input_tokens\":7,\"output_tokens\":3,\"total_tokens\":10}}}"
            ]
        response <- expectRight
            (buildStreamResponseWithModel config (Just "request-model") events)
        response.responseId `shouldBe` "resp-partial"
        response.createdAt `shouldBe` 0
        response.model `shouldBe` "request-model"
        response.object `shouldBe` "response"
        response.status `shouldBe` ResponseCompleted
        response.output `shouldBe` []
        fmap (.totalTokens) response.usage `shouldBe` Just 10

    it "classifies an incomplete lifecycle fragment as a failure" do
        events <- expectRight $ parseSseEvents $ Text.intercalate ""
            [ sseBlock "response.created"
                "{\"type\":\"response.created\",\"response\":{\"id\":\"resp-incomplete\"}}"
            , sseBlock "response.incomplete"
                "{\"type\":\"response.incomplete\",\"response\":{\"incomplete_details\":{\"reason\":\"max_output_tokens\"}}}"
            ]
        buildStreamResponseWithModel config (Just "request-model") events
            `shouldBe`
                Left (ConnectionError
                    "failed: response.incomplete: max_output_tokens")

    it "replaces indexed added items with done items without duplicates" do
        events <- expectRight $ parseSseEvents $ Text.intercalate ""
            [ sseBlock "response.created"
                "{\"type\":\"response.created\",\"response\":{\"id\":\"resp-indexed\"}}"
            , sseBlock "response.output_item.added"
                "{\"type\":\"response.output_item.added\",\"output_index\":1,\"item\":{\"type\":\"function_call\",\"call_id\":\"call-2\",\"name\":\"stale-second\",\"arguments\":\"\"}}"
            , sseBlock "response.output_item.added"
                "{\"type\":\"response.output_item.added\",\"output_index\":0,\"item\":{\"type\":\"function_call\",\"call_id\":\"call-1\",\"name\":\"first\",\"arguments\":\"{}\"}}"
            , sseBlock "response.output_item.done"
                "{\"type\":\"response.output_item.done\",\"output_index\":1,\"item\":{\"type\":\"function_call\",\"call_id\":\"call-2\",\"name\":\"second\",\"arguments\":\"{}\"}}"
            , sseBlock "response.completed"
                "{\"type\":\"response.completed\",\"response\":{\"id\":\"resp-indexed\"}}"
            ]
        response <- expectRight
            (buildStreamResponseWithModel config (Just "request-model") events)
        [name | FunctionCallItem FunctionCall { name } <- response.output]
            `shouldBe` ["first", "second"]
        length response.output `shouldBe` 2

    it "assembles indexless done-only output items in wire order" do
        events <- expectRight $ parseSseEvents $ Text.intercalate ""
            [ sseBlock "response.created"
                "{\"type\":\"response.created\",\"response\":{\"id\":\"resp-indexless\"}}"
            , sseBlock "response.output_item.done"
                "{\"type\":\"response.output_item.done\",\"item\":{\"type\":\"function_call\",\"call_id\":\"call-1\",\"name\":\"first\",\"arguments\":\"{}\"}}"
            , sseBlock "response.output_item.done"
                "{\"type\":\"response.output_item.done\",\"item\":{\"type\":\"function_call\",\"call_id\":\"call-2\",\"name\":\"second\",\"arguments\":\"{}\"}}"
            , sseBlock "response.done"
                "{\"type\":\"response.done\",\"response\":{}}"
            ]
        response <- expectRight
            (buildStreamResponseWithModel config (Just "request-model") events)
        [name | FunctionCallItem FunctionCall { name } <- response.output]
            `shouldBe` ["first", "second"]

    it "assembles custom-tool input events without output_index" do
        events <- expectRight $ parseSseEvents $ Text.intercalate ""
            [ sseBlock "response.created"
                "{\"type\":\"response.created\",\"response\":{\"id\":\"resp-custom\"}}"
            , sseBlock "response.output_item.added"
                "{\"type\":\"response.output_item.added\",\"output_index\":0,\"item\":{\"type\":\"custom_tool_call\",\"id\":\"ctc-1\",\"call_id\":\"call-1\",\"name\":\"apply_patch\",\"input\":\"\"}}"
            , sseBlock "response.custom_tool_call_input.delta"
                "{\"type\":\"response.custom_tool_call_input.delta\",\"item_id\":\"not-the-item\",\"call_id\":\"call-1\",\"delta\":\"*** Begin\"}"
            , sseBlock "response.custom_tool_call_input.delta"
                "{\"type\":\"response.custom_tool_call_input.delta\",\"call_id\":\"call-1\",\"delta\":\" Patch\"}"
            , sseBlock "response.custom_tool_call_input.done"
                "{\"type\":\"response.custom_tool_call_input.done\",\"item_id\":\"ctc-1\",\"input\":\"*** Begin Patch\"}"
            , sseBlock "response.completed"
                "{\"type\":\"response.completed\",\"response\":{\"id\":\"resp-custom\"}}"
            ]
        response <- expectRight
            (buildStreamResponseWithModel config (Just "request-model") events)
        case response.output of
            [CustomToolCallItem CustomToolCall { name, input }] ->
                (name, input)
                    `shouldBe` ("apply_patch", "*** Begin Patch")
            other -> expectationFailure
                ("expected one custom tool call, got " <> show other)

    it "assembles reasoning summary part and text events by item id" do
        events <- expectRight $ parseSseEvents $ Text.intercalate ""
            [ sseBlock "response.created"
                "{\"type\":\"response.created\",\"response\":{\"id\":\"resp-reasoning\"}}"
            , sseBlock "response.output_item.added"
                "{\"type\":\"response.output_item.added\",\"output_index\":0,\"item\":{\"type\":\"reasoning\",\"id\":\"rs-1\",\"summary\":[]}}"
            , sseBlock "response.reasoning_summary_part.added"
                "{\"type\":\"response.reasoning_summary_part.added\",\"item_id\":\"rs-1\",\"summary_index\":0,\"part\":{\"type\":\"summary_text\",\"text\":\"\"}}"
            , sseBlock "response.reasoning_summary_text.done"
                "{\"type\":\"response.reasoning_summary_text.done\",\"item_id\":\"rs-1\",\"summary_index\":0,\"text\":\"Checked the repository.\"}"
            , sseBlock "response.completed"
                "{\"type\":\"response.completed\",\"response\":{\"id\":\"resp-reasoning\"}}"
            ]
        response <- expectRight
            (buildStreamResponseWithModel config (Just "request-model") events)
        case response.output of
            [ReasoningItemValue ReasoningItem
                { summary = [ReasoningSummaryPart { text = Just partText }]
                }] ->
                    partText `shouldBe` "Checked the repository."
            other -> expectationFailure
                ("expected one reasoning summary, got " <> show other)

    it "assembles indexless reasoning summary text deltas by item id" do
        events <- expectRight $ parseSseEvents $ Text.intercalate ""
            [ sseBlock "response.created"
                "{\"type\":\"response.created\",\"response\":{\"id\":\"resp-reasoning-delta\"}}"
            , sseBlock "response.output_item.added"
                "{\"type\":\"response.output_item.added\",\"item\":{\"type\":\"reasoning\",\"id\":\"rs-1\",\"summary\":[]}}"
            , sseBlock "response.reasoning_summary_part.added"
                "{\"type\":\"response.reasoning_summary_part.added\",\"item_id\":\"rs-1\",\"summary_index\":0,\"part\":{\"type\":\"summary_text\",\"text\":\"\"}}"
            , sseBlock "response.reasoning_summary_text.delta"
                "{\"type\":\"response.reasoning_summary_text.delta\",\"item_id\":\"rs-1\",\"summary_index\":0,\"delta\":\"Checked \"}"
            , sseBlock "response.reasoning_summary_text.delta"
                "{\"type\":\"response.reasoning_summary_text.delta\",\"item_id\":\"rs-1\",\"summary_index\":0,\"delta\":\"the repository.\"}"
            , sseBlock "response.completed"
                "{\"type\":\"response.completed\",\"response\":{\"id\":\"resp-reasoning-delta\"}}"
            ]
        response <- expectRight
            (buildStreamResponseWithModel config (Just "request-model") events)
        case response.output of
            [ReasoningItemValue ReasoningItem
                { summary = [ReasoningSummaryPart { text = Just partText }]
                }] ->
                    partText `shouldBe` "Checked the repository."
            other -> expectationFailure
                ("expected one reasoning summary, got " <> show other)

    it "uses provider classifiers when no terminal response is present" do
        streamEvents <- expectRight $ parseSseEvents $ sseBlock "error"
            "{\"type\":\"error\",\"error\":{\"message\":\"stream broke\"}}"
        buildStreamResponse config streamEvents
            `shouldBe` Left (ConnectionError "stream: stream broke")

        failedEvents <- expectRight $ parseSseEvents $ sseBlock "response.failed"
            "{\"type\":\"response.failed\",\"response\":{\"id\":\"resp-f\",\"created_at\":0,\"model\":\"test\",\"status\":\"failed\",\"incomplete_details\":{\"reason\":\"overloaded\"}}}"
        buildStreamResponse config failedEvents
            `shouldBe` Left (ConnectionError "failed: response.failed: overloaded")

        emptyFailedEvents <- expectRight $ parseSseEvents $
            sseBlock "response.failed"
                "{\"type\":\"response.failed\",\"response\":{}}"
        buildStreamResponse config emptyFailedEvents
            `shouldBe` Left
                (ConnectionError "failed: response.failed (no details)")

        messageFailedEvents <- expectRight $ parseSseEvents $
            sseBlock "response.failed"
                "{\"type\":\"response.failed\",\"response\":{\"error\":{\"message\":\"exploded\"}}}"
        buildStreamResponse config messageFailedEvents
            `shouldBe` Left (ConnectionError "failed: exploded")

    it "accepts code-only stream errors" do
        streamEvents <- expectRight $ parseSseEvents $ sseBlock "error"
            "{\"type\":\"error\",\"code\":\"rate_limit\"}"
        case streamEvents of
            [ResponseErrorEvent { streamError }] -> do
                streamError.code `shouldBe` Just "rate_limit"
                streamError.message `shouldBe` ""
            other -> expectationFailure
                ("expected one stream error event, got " <> show other)

    it "uses the configured missing-completion message" do
        case buildStreamResponse config [] of
            Left (JsonDecodeError message _) ->
                message `shouldBe` "custom missing completion"
            other -> expectationFailure
                ("expected missing-completion JsonDecodeError, got " <> show other)
  where
    config = StreamAssemblyConfig
        { missingCompletionMessage = "custom missing completion"
        , classifyStreamError =
            \streamError -> ConnectionError ("stream: " <> streamError.message)
        , classifyFailedResponse =
            \failure ->
                ConnectionError
                    ("failed: " <> failedStreamResponseMessage failure)
        , incompleteAsFailure = True
        }

sseBlock :: Text -> Text -> Text
sseBlock eventType dataText =
    "event: " <> eventType <> "\ndata: " <> dataText <> "\n\n"

expectRight :: Show error => Either error value -> IO value
expectRight = \case
    Left err ->
        expectationFailure ("expected Right, got Left " <> show err)
            >> fail "unreachable"
    Right value -> pure value
