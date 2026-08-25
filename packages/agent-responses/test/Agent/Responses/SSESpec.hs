module Agent.Responses.SSESpec (spec) where

import Agent.Error (ApiError(..))
import Agent.Responses.SSE
import Agent.Responses.Types
import Control.Monad (foldM)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
    ( Arbitrary(..)
    , Gen
    , Property
    , chooseInt
    , conjoin
    , counterexample
    , elements
    , frequency
    , listOf
    , oneof
    , vectorOf
    , (===)
    )

spec :: Spec
spec = describe "Responses SSE decoder" do
    modifyMaxSuccess (const 500) $
        prop "is invariant under generated HTTP chunk boundaries" $
            chunkingInvariant

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
decodeChunks chunks =
    expectRight (decodeChunksEither chunks)

decodeChunksEither
    :: [BS.ByteString]
    -> Either ApiError [ResponseStreamEvent]
decodeChunksEither chunks = do
    (decoder, reversedEvents) <-
        foldM step (newSseDecoder, []) chunks
    trailing <- finishSseDecoder decoder
    pure (reverse reversedEvents <> trailing)
  where
    step (decoder, reversedEvents) chunk = do
        (nextDecoder, events) <- feedSseDecoder decoder chunk
        pure (nextDecoder, reverse events <> reversedEvents)

data GeneratedChunkedStream = GeneratedChunkedStream
    { generatedBody :: !Text
    , generatedChunks :: ![BS.ByteString]
    }

instance Show GeneratedChunkedStream where
    show stream =
        "GeneratedChunkedStream { body = "
            <> show stream.generatedBody
            <> ", chunkSizes = "
            <> show (map BS.length stream.generatedChunks)
            <> " }"

instance Arbitrary GeneratedChunkedStream where
    arbitrary = do
        body <- genSseBody
        chunks <- genChunks (Text.encodeUtf8 body)
        pure GeneratedChunkedStream
            { generatedBody = body
            , generatedChunks = chunks
            }

    shrink _ = []

chunkingInvariant :: GeneratedChunkedStream -> Property
chunkingInvariant stream =
    conjoin
        [ counterexample
            ("chunks do not reconstruct body: " <> show stream)
            (BS.concat stream.generatedChunks
                === Text.encodeUtf8 stream.generatedBody)
        , counterexample
            ("chunked decoder differs from one-shot decoder: " <> show stream)
            (decodeChunksEither stream.generatedChunks
                === parseSseEvents stream.generatedBody)
        ]

genSseBody :: Gen Text
genSseBody = do
    lineEnding <- elements ["\n", "\r\n"]
    eventCount <- chooseInt (1, 8)
    events <- mapM genEvent [0 .. eventCount - 1]
    includeEventLines <- vectorOf eventCount (frequency [(3, pure True), (1, pure False)])
    includeComments <- vectorOf eventCount (frequency [(1, pure True), (3, pure False)])
    includeDone <- frequency [(1, pure True), (3, pure False)]
    unterminated <- frequency [(1, pure True), (3, pure False)]
    let blocks =
            concat
                [ [ if comment
                        then ": generated keepalive" <> lineEnding <> lineEnding
                        else ""
                  , eventBlock lineEnding withEvent event
                  ]
                | (event, withEvent, comment) <-
                    zip3 events includeEventLines includeComments
                ]
        withDone =
            if includeDone
                then blocks
                    <> ["data: [DONE]" <> lineEnding <> lineEnding]
                else blocks
        body = Text.concat withDone
    pure $
        if unterminated
            then dropFinalBlankLine lineEnding body
            else body

genEvent :: Int -> Gen ResponseStreamEvent
genEvent index =
    oneof
        [ genLifecycleEvent index
        , genOutputItemEvent index
        , genCustomToolDelta index
        ]

genLifecycleEvent :: Int -> Gen ResponseStreamEvent
genLifecycleEvent index = do
    model <- genText
    status <-
        elements ["completed", "incomplete", "failed"]
            :: Gen Text
    let response =
            Aeson.object
                [ "id" .= ("resp-" <> Text.pack (show index))
                , "created_at" .= index
                , "model" .= model
                , "status" .= status
                ]
    elements
        [ ResponseCreatedEvent response (Just index) KeyMap.empty
        , ResponseInProgressEvent response (Just index) KeyMap.empty
        , ResponseCompletedEvent response (Just index) KeyMap.empty
        , ResponseDoneEvent response (Just index) KeyMap.empty
        , ResponseFailedEvent response (Just index) KeyMap.empty
        , ResponseIncompleteEvent response (Just index) KeyMap.empty
        , ResponseQueuedEvent response (Just index) KeyMap.empty
        ]

genOutputItemEvent :: Int -> Gen ResponseStreamEvent
genOutputItemEvent index = do
    body <- genText
    let item =
            MessageItem ResponseMessage
                { messageId = Just ("msg-" <> Text.pack (show index))
                , content =
                    MessageContentParts
                        [ OutputTextPart
                            { text = body
                            , annotations = Nothing
                            , logprobs = Nothing
                            , extraFields = KeyMap.empty
                            }
                        ]
                , role = RoleAssistant
                , status = Just ItemCompleted
                , phase = Nothing
                , passthrough = Nothing
                , extraFields = KeyMap.empty
                }
    elements
        [ ResponseOutputItemAddedEvent
            item (Just index) (Just index) KeyMap.empty
        , ResponseOutputItemDoneEvent
            item (Just index) (Just index) KeyMap.empty
        ]

genCustomToolDelta :: Int -> Gen ResponseStreamEvent
genCustomToolDelta index = do
    delta <- genText
    let itemId = Just ("item-" <> Text.pack (show index))
        callId = Just ("call-" <> Text.pack (show index))
    elements
        [ ResponseCustomToolInputDeltaEvent
            (Just delta) itemId callId (Just index) (Just index) KeyMap.empty
        , ResponseCustomToolInputDoneEvent
            (Just delta) itemId callId (Just index) (Just index) KeyMap.empty
        ]

genText :: Gen Text
genText = do
    count <- chooseInt (0, 80)
    Text.pack <$> vectorOf count genTextChar

genTextChar :: Gen Char
genTextChar =
    frequency
        [ (20, elements ['a' .. 'z'])
        , (5, elements ['0' .. '9'])
        , (4, elements [' ', '\n', '\t', '"', '\\'])
        , (3, elements ['é', '界', '🙂', '\x0301'])
        ]

eventBlock :: Text -> Bool -> ResponseStreamEvent -> Text
eventBlock lineEnding includeEventLine event =
    eventLine
        <> "data: "
        <> Text.decodeUtf8 (LBS.toStrict (Aeson.encode event))
        <> lineEnding
        <> lineEnding
  where
    eventLine
        | includeEventLine =
            "event: "
                <> streamEventTypeText (responseStreamEventType event)
                <> lineEnding
        | otherwise = ""

dropFinalBlankLine :: Text -> Text -> Text
dropFinalBlankLine lineEnding body =
    case Text.stripSuffix (lineEnding <> lineEnding) body of
        Just prefix -> prefix
        Nothing -> body

genChunks :: BS.ByteString -> Gen [BS.ByteString]
genChunks bytes = do
    sizes <- listOf (chooseInt (1, max 1 (min 64 (BS.length bytes))))
    includeEmpty <- frequency [(1, pure True), (3, pure False)]
    let chunks = splitBySizes sizes bytes
    pure $
        if includeEmpty
            then BS.empty : intersperseEmpty chunks <> [BS.empty]
            else chunks

splitBySizes :: [Int] -> BS.ByteString -> [BS.ByteString]
splitBySizes _ bytes | BS.null bytes = []
splitBySizes [] bytes = [bytes]
splitBySizes (size : sizes) bytes =
    let (chunk, rest) = BS.splitAt size bytes
    in chunk : splitBySizes sizes rest

intersperseEmpty :: [BS.ByteString] -> [BS.ByteString]
intersperseEmpty [] = []
intersperseEmpty [chunk] = [chunk]
intersperseEmpty (chunk : chunks) =
    chunk : BS.empty : intersperseEmpty chunks

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
