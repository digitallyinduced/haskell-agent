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
                "{\"type\":\"response.created\",\"response\":{\"id\":\"resp-done\",\"created_at\":0,\"model\":\"test\",\"status\":\"in_progress\",\"output\":[]}}"
            , sseBlock "response.output_item.done"
                "{\"type\":\"response.output_item.done\",\"output_index\":0,\"item\":{\"type\":\"function_call\",\"call_id\":\"call-done\",\"name\":\"shell_command\",\"arguments\":\"{\\\"command\\\":\\\"echo done\\\"}\"}}"
            , sseBlock "response.done"
                "{\"type\":\"response.done\",\"response\":{\"usage\":{\"input_tokens\":10,\"output_tokens\":2,\"total_tokens\":12}}}"
            ]
        response <- expectRight (buildStreamResponse config events)
        response.responseId `shouldBe` "resp-done"
        response.status `shouldBe` ResponseCompleted
        fmap (.totalTokens) response.usage `shouldBe` Just 12
        [name | FunctionCallItem FunctionCall { name } <- response.output]
            `shouldBe` ["shell_command"]

    it "uses provider classifiers when no terminal response is present" do
        streamEvents <- expectRight $ parseSseEvents $ sseBlock "error"
            "{\"type\":\"error\",\"error\":{\"message\":\"stream broke\"}}"
        buildStreamResponse config streamEvents
            `shouldBe` Left (ConnectionError "stream: stream broke")

        failedEvents <- expectRight $ parseSseEvents $ sseBlock "response.failed"
            "{\"type\":\"response.failed\",\"response\":{\"id\":\"resp-f\",\"created_at\":0,\"model\":\"test\",\"status\":\"failed\",\"incomplete_details\":{\"reason\":\"overloaded\"}}}"
        buildStreamResponse config failedEvents
            `shouldBe` Left (ConnectionError "failed: response.failed: overloaded")

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
            \response -> ConnectionError ("failed: " <> failedResponseMessage response)
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
