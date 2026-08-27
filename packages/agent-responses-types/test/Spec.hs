module Main (main) where

import qualified Agent.Json.Decoder as Decoder
import qualified Agent.Json.Encoder as Encoder
import Agent.Json (lookupExtension, rawJsonBytes)
import Agent.Responses.Types
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Test.Hspec

main :: IO ()
main = hspec do
    describe "direct Responses request codec" do
        it "round-trips typed input, tools, metadata, and extensions" do
            assertSemanticRoundTrip
                responseCreateParamsDecoder
                responseCreateParamsEncoder
                requestFixture

        it "rejects a scalar request root" do
            Decoder.decode responseCreateParamsDecoder "[]"
                `shouldSatisfy` isLeft

        it "preserves legacy null defaults for object and output" do
            response <- expectDecode responseDecoder
                ( "{\"id\":\"r\",\"created_at\":0,\"model\":\"m\","
                    <> "\"status\":\"completed\",\"object\":null,"
                    <> "\"output\":null}"
                )
            response.object `shouldBe` "response"
            response.output `shouldBe` []

    describe "direct Responses response codec" do
        it "round-trips output items, usage, and unknown fields" do
            assertSemanticRoundTrip
                responseDecoder
                responseEncoder
                responseFixture

        it "rejects missing required response identity fields" do
            Decoder.decode responseDecoder
                ( "{\"created_at\":1,\"model\":\"gpt-5\","
                    <> "\"status\":\"completed\"}"
                )
                `shouldSatisfy` isLeft

    describe "direct Responses stream codecs" do
        it "round-trips output text deltas as a dedicated constructor" do
            event <-
                expectDecode responseStreamEventDecoder outputDeltaFixture
            event `shouldSatisfy` \case
                ResponseOutputTextDeltaEvent{} -> True
                _ -> False
            assertSemanticRoundTrip
                responseStreamEventDecoder
                responseStreamEventEncoder
                outputDeltaFixture

        it "round-trips reasoning deltas as a dedicated constructor" do
            event <-
                expectDecode responseStreamEventDecoder reasoningDeltaFixture
            event `shouldSatisfy` \case
                ResponseReasoningTextDeltaEvent{} -> True
                _ -> False
            assertSemanticRoundTrip
                responseStreamEventDecoder
                responseStreamEventEncoder
                reasoningDeltaFixture

        it "fills an omitted JSON type from the SSE event name" do
            event <-
                expectDecode
                    (responseStreamEventDecoderWithType
                        (Just "response.output_text.delta"))
                    "{\"sequence_number\":1,\"delta\":\"hello\"}"
            responseStreamEventType event
                `shouldBe` EventOutputTextDelta

        it "rejects disagreement between SSE and JSON event types" do
            Decoder.decode
                (responseStreamEventDecoderWithType
                    (Just "response.output_text.delta"))
                "{\"type\":\"response.output_text.done\"}"
                `shouldSatisfy` \case
                    Left _ -> True
                    Right _ -> False

        it "uses the final duplicate known field" do
            event <- expectDecode responseStreamEventDecoder
                ( "{\"type\":\"response.output_text.delta\","
                    <> "\"delta\":\"first\",\"delta\":\"last\"}"
                )
            case event of
                ResponseOutputTextDeltaEvent { delta } ->
                    delta `shouldBe` Just "last"
                _ -> expectationFailure "unexpected event constructor"

        it "re-encodes lifecycle response fragments without invented fields" do
            let payload =
                    "{\"type\":\"response.done\",\"response\":"
                        <> "{\"status\":\"completed\","
                        <> "\"vendor\":{\"x\":1}}}"
            event <- expectDecode responseStreamEventDecoder payload
            decodeAeson
                (Encoder.encode responseStreamEventEncoder event)
                `shouldBe` decodeAeson payload

        it "lets a duplicate null clear an earlier optional value" do
            event <- expectDecode responseStreamEventDecoder
                ( "{\"type\":\"response.output_text.delta\","
                    <> "\"delta\":\"first\",\"delta\":null}"
                )
            case event of
                ResponseOutputTextDeltaEvent
                    { delta
                    , eventExtraFields
                    } -> do
                        delta `shouldBe` Nothing
                        rawJsonBytes
                            <$> lookupExtension
                                "delta"
                                eventExtraFields
                            `shouldBe` Just "null"
                _ -> expectationFailure "unexpected event constructor"

        it "retains a complete nested unknown extension" do
            event <- expectDecode responseStreamEventDecoder
                ( "{\"type\":\"response.output_text.delta\","
                    <> "\"vendor\":{\"nested\":[1,true,null]}}"
                )
            case event of
                ResponseOutputTextDeltaEvent { eventExtraFields } ->
                    rawJsonBytes
                        <$> lookupExtension
                            "vendor"
                            eventExtraFields
                        `shouldBe`
                            Just "{\"nested\":[1,true,null]}"
                _ -> expectationFailure "unexpected event constructor"

assertSemanticRoundTrip
    :: Decoder.Decoder value
    -> Encoder.Encoder value
    -> BS.ByteString
    -> Expectation
assertSemanticRoundTrip decoder encoder input = do
    value <- expectDecode decoder input
    let encoded = Encoder.encode encoder value
    decodeAeson encoded `shouldBe` decodeAeson input

expectDecode
    :: Decoder.Decoder value
    -> BS.ByteString
    -> IO value
expectDecode decoder input =
    case Decoder.decode decoder input of
        Left err ->
            expectationFailure (show err) >> fail "unreachable"
        Right value -> pure value

decodeAeson :: BS.ByteString -> Either String Aeson.Value
decodeAeson =
    Aeson.eitherDecodeStrict'

isLeft :: Either error value -> Bool
isLeft = \case
    Left _ -> True
    Right _ -> False

requestFixture :: BS.ByteString
requestFixture =
    "{\"model\":\"gpt-5\",\"input\":[{\"type\":\"message\","
        <> "\"role\":\"user\",\"content\":[{\"type\":\"input_text\","
        <> "\"text\":\"hello\",\"detail\":1,\"vendor_part\":1}],"
        <> "\"vendor_item\":true}],"
        <> "\"tools\":[{\"type\":\"function\",\"name\":\"lookup\","
        <> "\"parameters\":{\"type\":\"object\",\"properties\":{}}}],"
        <> "\"metadata\":{\"trace\":\"abc\"},\"vendor_request\":{\"x\":1}}"

responseFixture :: BS.ByteString
responseFixture =
    "{\"id\":\"resp_1\",\"created_at\":1,\"model\":\"gpt-5\","
        <> "\"error\":null,\"incomplete_details\":null,"
        <> "\"metadata\":null,"
        <> "\"usage\":{\"input_tokens\":2,\"output_tokens\":3,"
        <> "\"total_tokens\":5},\"status\":\"completed\","
        <> "\"output\":[{\"type\":\"message\","
        <> "\"id\":\"msg_1\",\"role\":\"assistant\",\"status\":\"completed\","
        <> "\"content\":[{\"type\":\"output_text\",\"text\":\"hello\","
        <> "\"annotations\":[],\"vendor_part\":true}]}],"
        <> "\"vendor_response\":{\"x\":1}}"

outputDeltaFixture :: BS.ByteString
outputDeltaFixture =
    "{\"type\":\"response.output_text.delta\",\"sequence_number\":1,"
        <> "\"item_id\":\"msg_1\",\"output_index\":0,\"content_index\":0,"
        <> "\"delta\":\"hel\",\"arguments\":{\"x\":1},"
        <> "\"vendor_event\":true}"

reasoningDeltaFixture :: BS.ByteString
reasoningDeltaFixture =
    "{\"type\":\"response.reasoning_text.delta\",\"sequence_number\":2,"
        <> "\"item_id\":\"rs_1\",\"output_index\":0,\"content_index\":0,"
        <> "\"delta\":\"thinking\",\"vendor_event\":true}"
