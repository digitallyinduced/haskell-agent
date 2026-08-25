module Agent.Responses.SSESpec (spec) where

import Agent.Error (ApiError(..))
import Agent.Responses.SSE
import Agent.Responses.Types
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Test.Hspec

spec :: Spec
spec = describe "Responses SSE decoder" do
    it "decodes arbitrary HTTP chunk boundaries, including split UTF-8" do
        mapM_ checkSplit [0 .. BS.length splitBytes]

    it "decodes a stream fed one byte at a time" do
        events <- decodeChunks (map BS.singleton (BS.unpack splitBytes))
        eventTypes events
            `shouldBe` [EventOutputItemDone, EventResponseCompleted]

    it "accepts CRLF, multiline data, comments, and a final unterminated event" do
        events <- expectRight $ parseSseEvents $ Text.intercalate ""
            [ ": keepalive\r\n\r\n"
            , "event: response.output_item.done\r\n"
            , "data: {\"type\":\"response.output_item.done\",\r\n"
            , "data: \"output_index\":0,\"item\":{\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"héllo\"}]}}\r\n\r\n"
            , "data: [DONE]\r\n\r\n"
            , Text.dropEnd 2 completedBlock
            ]
        eventTypes events
            `shouldBe` [EventOutputItemDone, EventResponseCompleted]

    it "reads the event type from data and ignores blocks without data" do
        events <- expectRight $ parseSseEvents $
            "event: ping\nid: 1\n\n"
                <> "data: " <> completedJson <> "\n\n"
        eventTypes events `shouldBe` [EventResponseCompleted]

    it "skips malformed event payloads while preserving unknown events" do
        events <- expectRight $ parseSseEvents $ Text.intercalate ""
            [ sseBlock "response.output_item.done"
                "{\"type\":\"response.output_item.done\",\"output_index\":0,\"item\":3}"
            , sseBlock "response.output_text.delta" "{not-json"
            , sseBlock "response.future_event"
                "{\"type\":\"response.future_event\",\"vendor_field\":true}"
            , completedBlock
            ]
        eventTypes events
            `shouldBe` [StreamEventUnknown "response.future_event", EventResponseCompleted]

    it "rejects invalid UTF-8 only after a complete event block arrives" do
        (decoder, events) <- expectRight $
            feedSseDecoder newSseDecoder "data: \xc3"
        events `shouldBe` []
        case feedSseDecoder decoder "\x28\n\n" of
            Left JsonDecodeError{} -> pure ()
            Left other -> expectationFailure
                ("expected invalid UTF-8 error, got " <> show other)
            Right _ -> expectationFailure "expected invalid UTF-8 error"

checkSplit :: Int -> IO ()
checkSplit offset = do
    let (first, second) = BS.splitAt offset splitBytes
    events <- decodeChunks [first, second]
    eventTypes events
        `shouldBe` [EventOutputItemDone, EventResponseCompleted]

decodeChunks :: [BS.ByteString] -> IO [ResponseStreamEvent]
decodeChunks chunks = do
    (decoder, reversedEvents) <- foldl step (pure (newSseDecoder, [])) chunks
    trailing <- expectRight (finishSseDecoder decoder)
    pure (reverse reversedEvents <> trailing)
  where
    step accumulated chunk = do
        (decoder, reversedEvents) <- accumulated
        (nextDecoder, events) <- expectRight (feedSseDecoder decoder chunk)
        pure (nextDecoder, reverse events <> reversedEvents)

eventTypes :: [ResponseStreamEvent] -> [StreamEventType]
eventTypes = map responseStreamEventType

splitBytes :: BS.ByteString
splitBytes = Text.encodeUtf8 (itemBlock <> completedBlock)

itemBlock :: Text
itemBlock = sseBlock "response.output_item.done" itemJson

itemJson :: Text
itemJson =
    "{\"type\":\"response.output_item.done\",\"output_index\":0,\
    \\"item\":{\"type\":\"message\",\"role\":\"assistant\",\"content\":\
    \[{\"type\":\"output_text\",\"text\":\"héllo\"}]}}"

completedBlock :: Text
completedBlock = sseBlock "response.completed" completedJson

completedJson :: Text
completedJson =
    "{\"type\":\"response.completed\",\"response\":{\"id\":\"resp-final\",\
    \\"created_at\":0,\"model\":\"test-model\",\"status\":\"completed\"}}"

sseBlock :: Text -> Text -> Text
sseBlock eventType dataText =
    "event: " <> eventType <> "\ndata: " <> dataText <> "\n\n"

expectRight :: Show error => Either error value -> IO value
expectRight = \case
    Left err ->
        expectationFailure ("expected Right, got Left " <> show err)
            >> fail "unreachable"
    Right value -> pure value
