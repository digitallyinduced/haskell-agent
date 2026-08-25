module Agent.OpenRouter.RequestSpec (spec) where

import Agent.Error (ApiError(..))
import Agent.Responses.Types
import Agent.OpenRouter.Options
import Agent.OpenRouter.Request
import Agent.OpenRouter.Stream
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Test.Hspec

spec :: Spec
spec = do
    describe "mapModel" do
        it "prefers exact overrides, passes slugs through, and falls back otherwise" do
            let options = defaultClientOptions
                    { modelOverrides = [("gpt-5.1", "anthropic/claude-sonnet-4")]
                    , defaultModel = "openai/gpt-5.1"
                    }
            mapModel options "gpt-5.1" `shouldBe` "anthropic/claude-sonnet-4"
            mapModel options "x-ai/grok-4" `shouldBe` "x-ai/grok-4"
            mapModel options "gpt-4o" `shouldBe` "openai/gpt-5.1"

    describe "buildRequest" do
        it "forces a stateless streaming Responses request" do
            let value = requestValue defaultClientOptions sampleRequest
            object <- expectObject value

            KeyMap.lookup "model" object `shouldBe` Just (Aeson.String "openai/gpt-5.1")
            KeyMap.lookup "store" object `shouldBe` Just (Aeson.Bool False)
            KeyMap.lookup "stream" object `shouldBe` Just (Aeson.Bool True)
            KeyMap.lookup "previous_response_id" object `shouldBe` Nothing
            KeyMap.lookup "instructions" object
                `shouldBe` Just (Aeson.String "You are a bookkeeping agent.")
            KeyMap.lookup "prompt_cache_key" object `shouldBe` Just (Aeson.String "cache-1")

        it "keeps instructions as a Responses field rather than a system item" do
            let value = requestValue defaultClientOptions sampleRequest
            object <- expectObject value
            input <- expectArray (KeyMap.lookup "input" object)
            firstItem <- case input of
                (item : _) -> expectObject item
                [] -> expectationFailure "input is empty" >> fail "unreachable"
            KeyMap.lookup "role" firstItem `shouldBe` Just (Aeson.String "user")
            length input `shouldBe` 1

        it "maps web_search, keeps function tools, and drops the computer tool" do
            let value = requestValue defaultClientOptions sampleRequest
            object <- expectObject value
            tools <- expectArray (KeyMap.lookup "tools" object)
            toolObjects <- traverse expectObject tools
            map (KeyMap.lookup "type") toolObjects `shouldBe`
                [ Just (Aeson.String "function")
                , Just (Aeson.String "web_search")
                ]
            Maybe.mapMaybe (KeyMap.lookup "external_web_access") toolObjects `shouldBe`
                [Aeson.Bool True]

        it "preserves Codex custom and namespace tools" do
            let codexTools =
                    [ KnownResponseTool ToolCustom TaggedObject
                        { tag = "custom"
                        , fields = KeyMap.singleton
                            "name"
                            (Aeson.String "apply_patch")
                        }
                    , KnownResponseTool ToolNamespace TaggedObject
                        { tag = "namespace"
                        , fields = KeyMap.singleton
                            "name"
                            (Aeson.String "collaboration")
                        }
                    ]
                request = withTools (Just codexTools) sampleRequest
            object <- expectObject
                (requestValue defaultClientOptions request)
            tools <- expectArray (KeyMap.lookup "tools" object)
            toolObjects <- traverse expectObject tools
            map (KeyMap.lookup "type") toolObjects `shouldBe`
                [ Just (Aeson.String "custom")
                , Just (Aeson.String "namespace")
                ]

        it "uses the configured default when the request has no model" do
            let value = requestValue defaultClientOptions
                    (withModel Nothing sampleRequest)
            object <- expectObject value
            KeyMap.lookup "model" object `shouldBe` Just (Aeson.String "openai/gpt-5.1")

        it "preserves reasoning as supplied" do
            let value = requestValue defaultClientOptions sampleRequest
            object <- expectObject value
            KeyMap.lookup "reasoning" object `shouldBe` Just (Aeson.object
                [ "effort" .= ("high" :: Text)
                ])

    describe "SSE assembly" do
        it "decodes typed event constructors and builds the merged final response" do
            let sse = Text.intercalate ""
                    [ sseBlock "response.output_item.done"
                        "{\"type\":\"response.output_item.done\",\"output_index\":0,\"item\":{\"type\":\"function_call\",\"call_id\":\"call-1\",\"name\":\"echo\",\"arguments\":\"{}\"}}"
                    , sseBlock "response.completed"
                        "{\"type\":\"response.completed\",\"response\":{\"id\":\"resp-1\",\"created_at\":0,\"model\":\"openai/gpt-5.1\",\"status\":\"completed\",\"output\":[],\"usage\":{\"input_tokens\":10,\"output_tokens\":5,\"total_tokens\":15}}}"
                    ]
            events <- expectRight (parseSseEvents sse)
            map responseStreamEventType events
                `shouldBe` [EventOutputItemDone, EventResponseCompleted]
            response <- expectRight (buildResponse events)
            response.responseId `shouldBe` "resp-1"
            fmap (.inputTokens) response.usage `shouldBe` Just 10
            [name | FunctionCallItem FunctionCall { name } <- response.output]
                `shouldBe` ["echo"]

        it "reads the event type from the data object when no event line exists" do
            events <- expectRight $ parseSseEvents
                "data: {\"type\":\"response.completed\",\"response\":{\"id\":\"resp-2\",\"created_at\":0,\"model\":\"openai/gpt-5.1\",\"status\":\"completed\"}}\n\n"
            map responseStreamEventType events `shouldBe` [EventResponseCompleted]

        it "returns a terminal incomplete response" do
            events <- expectRight $ parseSseEvents $ sseBlock "response.incomplete"
                "{\"type\":\"response.incomplete\",\"response\":{\"id\":\"resp-i\",\"created_at\":0,\"model\":\"openai/gpt-5.1\",\"status\":\"incomplete\",\"output\":[],\"incomplete_details\":{\"reason\":\"max_output_tokens\"}}}"
            response <- expectRight (buildResponse events)
            response.responseId `shouldBe` "resp-i"
            response.status `shouldBe` ResponseIncomplete

        it "accepts a final event without a trailing blank line" do
            events <- expectRight $ parseSseEvents finalEventWithoutBlankLine
            map responseStreamEventType events `shouldBe` [EventResponseCompleted]

        it "decodes arbitrary HTTP chunk boundaries" do
            mapM_ checkSplit [0 .. BS.length splitBytes]

        it "maps response.failed and missing completion to transport-level errors" do
            failedEvents <- expectRight $ parseSseEvents $ sseBlock "response.failed"
                "{\"type\":\"response.failed\",\"response\":{\"id\":\"resp-f\",\"created_at\":0,\"model\":\"openai/gpt-5.1\",\"status\":\"failed\",\"incomplete_details\":{\"reason\":\"overloaded\"}}}"
            case buildResponse failedEvents of
                Left (ConnectionError message) ->
                    message `shouldSatisfy` Text.isInfixOf "overloaded"
                other -> expectationFailure ("expected ConnectionError, got " <> show other)

            case buildResponse [] of
                Left (JsonDecodeError message _) ->
                    message `shouldSatisfy` Text.isInfixOf "terminal response"
                other -> expectationFailure ("expected JsonDecodeError, got " <> show other)

sseBlock :: Text -> Text -> Text
sseBlock eventType dataText =
    "event: " <> eventType <> "\ndata: " <> dataText <> "\n\n"

requestValue :: ClientOptions -> ResponseCreateParams -> Aeson.Value
requestValue options = Aeson.toJSON . buildRequest options

sampleRequest :: ResponseCreateParams
sampleRequest = defaultResponseCreateParams
    { model = Just "openai/gpt-5.1"
    , instructions = Just "You are a bookkeeping agent."
    , previousResponseId = Just "resp-should-drop"
    , store = Just True
    , input = Just (ResponseInputItems
        [ MessageItem ResponseMessage
            { messageId = Nothing
            , role = RoleUser
            , content = MessageContentParts [InputTextPart "hello" Nothing mempty]
            , status = Nothing
            , phase = Nothing
            , passthrough = Nothing
            , extraFields = mempty
            }
        ])
    , tools = Just
        [ FunctionToolValue FunctionTool
            { name = "echo_text"
            , description = Just "Echo the text back"
            , parameters = Just (Aeson.object [])
            , strict = Nothing
            , extraFields = mempty
            }
        , KnownResponseTool ToolWebSearch TaggedObject
            { tag = "web_search"
            , fields = KeyMap.singleton "external_web_access" (Aeson.Bool True)
            }
        , KnownResponseTool ToolComputer TaggedObject
            { tag = "computer"
            , fields = mempty
            }
        ]
    , reasoning = Just ReasoningConfig
        { context = Nothing
        , effort = Just "high"
        , generateSummary = Nothing
        , reasoningMode = Nothing
        , summary = Nothing
        , extraFields = mempty
        }
    , include = Just [ResponseInclude "reasoning.encrypted_content"]
    , promptCacheKey = Just "cache-1"
    }

withModel :: Maybe Text -> ResponseCreateParams -> ResponseCreateParams
withModel nextModel ResponseCreateParams { model = _, .. } =
    ResponseCreateParams { model = nextModel, .. }

withTools :: Maybe [ResponseTool] -> ResponseCreateParams -> ResponseCreateParams
withTools nextTools ResponseCreateParams { tools = _, .. } =
    ResponseCreateParams { tools = nextTools, .. }

expectObject :: Aeson.Value -> IO Aeson.Object
expectObject = \case
    Aeson.Object object -> pure object
    other -> expectationFailure ("expected object, got " <> show other) >> fail "unreachable"

expectArray :: Maybe Aeson.Value -> IO [Aeson.Value]
expectArray = \case
    Just (Aeson.Array values) -> pure (foldr (:) [] values)
    other -> expectationFailure ("expected array, got " <> show other) >> fail "unreachable"

expectRight :: Show e => Either e a -> IO a
expectRight = \case
    Left err -> expectationFailure ("expected Right, got Left " <> show err) >> fail "unreachable"
    Right value -> pure value

finalEventWithoutBlankLine :: Text
finalEventWithoutBlankLine = Text.dropEnd 2 completedBlock

completedBlock :: Text
completedBlock = sseBlock "response.completed" completedJson

completedJson :: Text
completedJson = "{\"type\":\"response.completed\",\"response\":{\"id\":\"resp-final\",\"created_at\":0,\"model\":\"openai/gpt-5.1\",\"status\":\"completed\"}}"

splitBytes :: BS.ByteString
splitBytes = TextEncoding.encodeUtf8 (itemBlock <> completedBlock)

itemBlock :: Text
itemBlock = sseBlock "response.output_item.done" itemJson

itemJson :: Text
itemJson = "{\"type\":\"response.output_item.done\",\"output_index\":0,\"item\":{\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"héllo\"}]}}"

checkSplit :: Int -> IO ()
checkSplit offset = do
    let (first, second) = BS.splitAt offset splitBytes
    (decoder, firstEvents) <- expectRight
        (feedSseDecoder newSseDecoder first)
    (finalDecoder, secondEvents) <- expectRight
        (feedSseDecoder decoder second)
    trailing <- expectRight (finishSseDecoder finalDecoder)
    map responseStreamEventType (firstEvents <> secondEvents <> trailing)
        `shouldBe` [EventOutputItemDone, EventResponseCompleted]
