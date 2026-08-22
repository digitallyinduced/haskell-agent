module Agent.OpenAI.ResponsesSpec (spec) where

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Text
import Test.Hspec

import Agent.OpenAI.Http (decodeCodexHttpBody)
import qualified Agent.Responses.Codec as Codec
import qualified Agent.Responses.Types as Responses
import Agent.OpenAI.ToolDSL
import Agent.OpenAI.WebSocketClient

spec :: Spec
spec = do
    describe "canonical Responses request" do
        it "round-trips API fields, structured tool output, file_url, and extensions" do
            let original = canonicalRequestJson
            case Aeson.fromJSON original :: Aeson.Result Responses.ResponseCreateParams of
                Aeson.Success request -> Aeson.toJSON request `shouldBe` original
                Aeson.Error err -> expectationFailure err

        it "uses the canonical request unchanged at the WebSocket boundary" do
            let payload = buildWsPayloadWithOptions
                    CodexWsOptions { compactThreshold = Just 120_000 }
                    sampleRequest
                    (Just "resp_previous")
            field "type" payload `shouldBe` Just (Aeson.String "response.create")
            field "previous_response_id" payload `shouldBe` Just (Aeson.String "resp_previous")
            field "model" payload `shouldBe` Just (Aeson.String "gpt-5.6")
            field "store" payload `shouldBe` Just (Aeson.Bool False)
            field "max_output_tokens" payload `shouldBe` Just (Aeson.Number 4096)
            field "context_management" payload `shouldSatisfy` maybe False isNonEmptyArray

    describe "canonical Responses response" do
        it "preserves statuses, rich output text, usage, and unknown fields" do
            let original = canonicalResponseJson "failed"
            case Aeson.fromJSON original :: Aeson.Result Responses.Response of
                Aeson.Success response -> Aeson.toJSON response `shouldBe` original
                Aeson.Error err -> expectationFailure err

        it "decodes a completed SSE payload directly into the canonical response" do
            let body = "event: response.completed\ndata: "
                    <> jsonText (Aeson.object
                        [ "type" .= ("response.completed" :: Text)
                        , "response" .= canonicalResponseJson "completed"
                        ])
                    <> "\n\n"
            case decodeCodexHttpBody body of
                Right response -> do
                    response.responseId `shouldBe` "resp_1"
                    response.status `shouldBe` Responses.ResponseCompleted
                Left err -> expectationFailure (show err)

        it "turns a failed canonical response into the typed error channel" do
            decodeCodexHttpBody (jsonText (canonicalResponseJson "failed"))
                `shouldSatisfy` isLeft

    describe "canonical Responses items and tools" do
        it "redacts encrypted reasoning content from Show output" do
            let secret = "encrypted-reasoning-secret"
                item = Responses.ReasoningItem
                    { itemId = Just "reasoning_1"
                    , summary =
                        [ Responses.ReasoningSummaryPart
                            { partType = "summary_text"
                            , text = Just "safe summary"
                            , extraFields = mempty
                            }
                        ]
                    , content = Nothing
                    , encryptedContent = Just secret
                    , status = Just Responses.ItemCompleted
                    , extraFields = KeyMap.singleton "safe_extension" (Aeson.String "visible")
                    }
                rendered = show item
            rendered `shouldNotContain` T.unpack secret
            rendered `shouldContain` "encryptedContent = Just <redacted>"
            rendered `shouldContain` "reasoning_1"
            rendered `shouldContain` "safe_extension"
            rendered `shouldContain` "ItemCompleted"

        it "builds strict function tools in the canonical tool union" do
            case buildTool "lookup" "Look something up"
                    [PropertySchema "query" PropertyString True Nothing] of
                Responses.FunctionToolValue tool -> do
                    tool.name `shouldBe` "lookup"
                    tool.strict `shouldBe` Just True
                    tool.parameters `shouldSatisfy` (/= Nothing)
                other -> expectationFailure (show other)

    describe "canonical Responses streaming events" do
        it "recognises every currently documented event discriminator" do
            mapM_ assertKnownEvent documentedStreamEventTypes

        it "decodes output-item completion into a typed event constructor" do
            let original = Aeson.object
                    [ "type" .= ("response.output_item.done" :: Text)
                    , "sequence_number" .= (3 :: Int)
                    , "output_index" .= (0 :: Int)
                    , "item" .= Aeson.object
                        [ "type" .= ("message" :: Text)
                        , "role" .= ("assistant" :: Text)
                        , "content" .= ([] :: [Aeson.Value])
                        ]
                    ]
            case Aeson.fromJSON original :: Aeson.Result Responses.ResponseStreamEvent of
                Aeson.Success Responses.ResponseOutputItemDoneEvent { item, outputIndex } -> do
                    outputIndex `shouldBe` 0
                    item `shouldSatisfy` \case
                        Responses.MessageItem{} -> True
                        _ -> False
                Aeson.Success other -> expectationFailure ("unexpected event: " <> show other)
                Aeson.Error err -> expectationFailure err

        it "decodes response completion into a typed response" do
            let original = Aeson.object
                    [ "type" .= ("response.completed" :: Text)
                    , "sequence_number" .= (4 :: Int)
                    , "response" .= canonicalResponseJson "completed"
                    ]
            case Aeson.fromJSON original :: Aeson.Result Responses.ResponseStreamEvent of
                Aeson.Success Responses.ResponseCompletedEvent { response } ->
                    response.responseId `shouldBe` "resp_1"
                Aeson.Success other -> expectationFailure ("unexpected event: " <> show other)
                Aeson.Error err -> expectationFailure err

        it "decodes response incomplete into its typed terminal constructor" do
            let original = Aeson.object
                    [ "type" .= ("response.incomplete" :: Text)
                    , "sequence_number" .= (5 :: Int)
                    , "response" .= canonicalResponseJson "incomplete"
                    ]
            case Aeson.fromJSON original :: Aeson.Result Responses.ResponseStreamEvent of
                Aeson.Success Responses.ResponseIncompleteEvent { response } -> do
                    response.status `shouldBe` Responses.ResponseIncomplete
                    response.responseId `shouldBe` "resp_1"
                Aeson.Success other -> expectationFailure ("unexpected event: " <> show other)
                Aeson.Error err -> expectationFailure err

        it "decodes the Codex nested error envelope into a typed error" do
            let original = Aeson.object
                    [ "type" .= ("error" :: Text)
                    , "error" .= Aeson.object
                        [ "type" .= ("usage_limit_reached" :: Text)
                        , "message" .= ("quota exhausted" :: Text)
                        , "resets_in_seconds" .= (120 :: Int)
                        ]
                    ]
            case Aeson.fromJSON original :: Aeson.Result Responses.ResponseStreamEvent of
                Aeson.Success Responses.ResponseNestedErrorEvent { streamError } -> do
                    streamError.errorType `shouldBe` Just "usage_limit_reached"
                    streamError.retryAfter `shouldBe` Just 120
                    Aeson.toJSON (Responses.ResponseNestedErrorEvent streamError Nothing mempty)
                        `shouldBe` original
                Aeson.Success other -> expectationFailure ("unexpected event: " <> show other)
                Aeson.Error err -> expectationFailure err

        it "keeps unknown event types and fields losslessly" do
            let original = Aeson.object
                    [ "type" .= ("response.future.delta" :: Text)
                    , "sequence_number" .= (9 :: Int)
                    , "payload" .= Aeson.object ["x" .= (1 :: Int)]
                    ]
            case Aeson.fromJSON original :: Aeson.Result Responses.ResponseStreamEvent of
                Aeson.Success event -> do
                    Responses.responseStreamEventType event
                        `shouldBe` Responses.StreamEventUnknown "response.future.delta"
                    Aeson.toJSON event `shouldBe` original
                Aeson.Error err -> expectationFailure err

        it "rejects disagreement between the SSE name and JSON type" do
            let value = Aeson.object
                    [ "type" .= ("response.output_text.done" :: Text)
                    , "sequence_number" .= (7 :: Int)
                    ]
            Codec.decodeResponseStreamEventWithType "response.output_text.delta" value
                `shouldSatisfy` isLeft

canonicalRequestJson :: Aeson.Value
canonicalRequestJson = Aeson.object
    [ "model" .= ("gpt-5.6" :: Text)
    , "input" .=
        [ Aeson.object
            [ "type" .= ("message" :: Text)
            , "role" .= ("system" :: Text)
            , "content" .=
                [ Aeson.object
                    [ "type" .= ("input_file" :: Text)
                    , "file_url" .= ("https://example.com/spec.pdf" :: Text)
                    , "detail" .= ("high" :: Text)
                    , "future_content_field" .= True
                    ]
                ]
            , "phase" .= ("commentary" :: Text)
            ]
        , Aeson.object
            [ "type" .= ("function_call_output" :: Text)
            , "call_id" .= ("call_1" :: Text)
            , "output" .=
                [ Aeson.object
                    [ "type" .= ("input_text" :: Text)
                    , "text" .= ("structured output" :: Text)
                    ]
                ]
            ]
        , Aeson.object
            [ "type" .= ("future_item" :: Text)
            , "payload" .= (42 :: Int)
            ]
        ]
    , "instructions" .= ("Be precise" :: Text)
    , "max_output_tokens" .= (4096 :: Int)
    , "max_tool_calls" .= (8 :: Int)
    , "parallel_tool_calls" .= False
    , "store" .= True
    , "stream" .= False
    , "reasoning" .= Aeson.object
        [ "context" .= ("all_turns" :: Text)
        , "effort" .= ("max" :: Text)
        , "mode" .= ("pro" :: Text)
        , "summary" .= ("detailed" :: Text)
        ]
    , "tool_choice" .= Aeson.object
        [ "type" .= ("function" :: Text)
        , "name" .= ("lookup" :: Text)
        ]
    , "tools" .=
        [ Aeson.object
            [ "type" .= ("function" :: Text)
            , "name" .= ("lookup" :: Text)
            , "description" .= ("Look something up" :: Text)
            , "parameters" .= Aeson.object ["type" .= ("object" :: Text)]
            , "strict" .= True
            , "defer_loading" .= True
            ]
        , Aeson.object
            [ "type" .= ("mcp" :: Text)
            , "server_label" .= ("docs" :: Text)
            , "server_url" .= ("https://example.com/mcp" :: Text)
            ]
        ]
    , "top_logprobs" .= (3 :: Int)
    , "temperature" .= (0.2 :: Double)
    , "top_p" .= (0.9 :: Double)
    , "truncation" .= ("disabled" :: Text)
    , "future_request_field" .= Aeson.object ["enabled" .= True]
    ]

canonicalResponseJson :: Text -> Aeson.Value
canonicalResponseJson responseStatus = Aeson.object
    [ "id" .= ("resp_1" :: Text)
    , "created_at" .= (1_700_000_000 :: Double)
    , "error" .= if responseStatus == "failed"
        then Aeson.object
            [ "code" .= ("server_error" :: Text)
            , "message" .= ("failed" :: Text)
            , "trace_id" .= ("trace_1" :: Text)
            ]
        else Aeson.Null
    , "incomplete_details" .= Aeson.Null
    , "instructions" .= ("Be precise" :: Text)
    , "metadata" .= Aeson.Null
    , "model" .= ("gpt-5.6" :: Text)
    , "object" .= ("response" :: Text)
    , "output" .=
        [ Aeson.object
            [ "type" .= ("message" :: Text)
            , "id" .= ("msg_1" :: Text)
            , "role" .= ("assistant" :: Text)
            , "status" .= ("completed" :: Text)
            , "phase" .= ("final_answer" :: Text)
            , "content" .=
                [ Aeson.object
                    [ "type" .= ("output_text" :: Text)
                    , "text" .= ("answer" :: Text)
                    , "annotations" .= ([] :: [Aeson.Value])
                    , "logprobs" .= ([] :: [Aeson.Value])
                    ]
                ]
            ]
        ]
    , "parallel_tool_calls" .= True
    , "tool_choice" .= ("auto" :: Text)
    , "tools" .= ([] :: [Aeson.Value])
    , "status" .= responseStatus
    , "usage" .= Aeson.object
        [ "input_tokens" .= (10 :: Int)
        , "input_tokens_details" .= Aeson.object ["cached_tokens" .= (4 :: Int)]
        , "output_tokens" .= (5 :: Int)
        , "output_tokens_details" .= Aeson.object ["reasoning_tokens" .= (2 :: Int)]
        , "total_tokens" .= (15 :: Int)
        , "future_usage_field" .= (1 :: Int)
        ]
    , "future_response_field" .= ("preserved" :: Text)
    ]

sampleRequest :: Responses.ResponseCreateParams
sampleRequest = Responses.defaultResponseCreateParams
    { Responses.model = Just "gpt-5.6"
    , Responses.instructions = Just "Be precise"
    , Responses.input = Just (Responses.ResponseInputText "hello")
    , Responses.maxOutputTokens = Just 4096
    , Responses.store = Just False
    , Responses.stream = Just True
    , Responses.reasoning = Just Responses.ReasoningConfig
        { Responses.context = Nothing
        , Responses.effort = Just "high"
        , Responses.generateSummary = Nothing
        , Responses.reasoningMode = Nothing
        , Responses.summary = Just "auto"
        , Responses.extraFields = KeyMap.empty
        }
    }

assertKnownEvent :: Text -> Expectation
assertKnownEvent eventName = do
    let original = Aeson.Object $ KeyMap.fromList
            ([ ("type", Aeson.String eventName)
             , ("sequence_number", Aeson.Number 12)
             , ("future_event_field", Aeson.object ["x" .= (1 :: Int)])
             ] <> requiredEventFields eventName)
    case Aeson.fromJSON original :: Aeson.Result Responses.ResponseStreamEvent of
        Aeson.Success event -> do
            Responses.streamEventTypeText (Responses.responseStreamEventType event) `shouldBe` eventName
            Aeson.toJSON event `shouldBe` original
        Aeson.Error err -> expectationFailure (show eventName <> ": " <> err)

requiredEventFields :: Text -> [(Key.Key, Aeson.Value)]
requiredEventFields eventName
    | eventName `elem`
        [ "response.created"
        , "response.in_progress"
        , "response.completed"
        , "response.failed"
        , "response.incomplete"
        , "response.queued"
        ] = [("response", canonicalResponseJson "completed")]
    | eventName `elem` ["response.output_item.added", "response.output_item.done"] =
        [ ("output_index", Aeson.Number 0)
        , ("item", Aeson.object
            [ "type" .= ("message" :: Text)
            , "role" .= ("assistant" :: Text)
            , "content" .= ([] :: [Aeson.Value])
            ])
        ]
    | eventName == "error" =
        [ ("code", Aeson.String "stream_error")
        , ("message", Aeson.String "stream failed")
        , ("param", Aeson.Null)
        ]
    | otherwise = []

field :: Text -> Aeson.Value -> Maybe Aeson.Value
field key (Aeson.Object object) = KeyMap.lookup (Key.fromText key) object
field _ _ = Nothing

isNonEmptyArray :: Aeson.Value -> Bool
isNonEmptyArray (Aeson.Array values) = not (null values)
isNonEmptyArray _ = False

jsonText :: Aeson.Value -> Text
jsonText = Text.decodeUtf8 . LBS.toStrict . Aeson.encode

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

documentedStreamEventTypes :: [Text]
documentedStreamEventTypes =
    [ "response.created", "response.in_progress", "response.completed"
    , "response.failed", "response.incomplete", "response.output_item.added"
    , "response.output_item.done", "response.content_part.added", "response.content_part.done"
    , "response.output_text.delta", "response.output_text.done", "response.refusal.delta"
    , "response.refusal.done", "response.function_call_arguments.delta"
    , "response.function_call_arguments.done", "response.file_search_call.in_progress"
    , "response.file_search_call.searching", "response.file_search_call.completed"
    , "response.web_search_call.in_progress", "response.web_search_call.searching"
    , "response.web_search_call.completed", "response.reasoning_summary_part.added"
    , "response.reasoning_summary_part.done", "response.reasoning_summary_text.delta"
    , "response.reasoning_summary_text.done", "response.reasoning_text.delta"
    , "response.reasoning_text.done", "response.image_generation_call.completed"
    , "response.image_generation_call.generating", "response.image_generation_call.in_progress"
    , "response.image_generation_call.partial_image", "response.mcp_call_arguments.delta"
    , "response.mcp_call_arguments.done", "response.mcp_call.completed", "response.mcp_call.failed"
    , "response.mcp_call.in_progress", "response.mcp_list_tools.completed"
    , "response.mcp_list_tools.failed", "response.mcp_list_tools.in_progress"
    , "response.code_interpreter_call.in_progress", "response.code_interpreter_call.interpreting"
    , "response.code_interpreter_call.completed", "response.code_interpreter_call_code.delta"
    , "response.code_interpreter_call_code.done", "response.output_text.annotation.added"
    , "response.queued", "response.custom_tool_call_input.delta"
    , "response.custom_tool_call_input.done", "error", "response.audio.delta"
    , "response.audio.done", "response.audio.transcript.delta", "response.audio.transcript.done"
    , "response.shell_call_command.added", "response.shell_call_command.delta"
    , "response.shell_call_command.done", "response.shell_call_output_content.delta"
    , "response.shell_call_output_content.done"
    ]
