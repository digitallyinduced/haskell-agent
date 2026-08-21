module Agent.XAI.ResponsesSpec (spec) where

import Agent.Error
    ( ApiError(..)
    , ErrorType(..)
    , RetryDisposition(..)
    , retryDisposition
    )
import Agent.XAI.Error
import Agent.XAI.Options
import Agent.XAI.Request
import Agent.XAI.Stream
import Agent.OpenAI.Responses.Types
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
    describe "mapModel" do
        it "prefers exact overrides, passes grok names through, and falls back otherwise" do
            let options = defaultClientOptions
                    { modelOverrides = [("gpt-5.6-sol", "grok-4.6-mini")]
                    , defaultModel = "grok-4.6"
                    }
            mapModel options "gpt-5.6-sol" `shouldBe` "grok-4.6-mini"
            mapModel options "grok-3" `shouldBe` "grok-3"
            mapModel options "gpt-5.6-terra" `shouldBe` "grok-4.6"

    describe "buildRequest" do
        it "maps canonical Responses fields onto the Grok proxy dialect" do
            let value = requestValue defaultClientOptions sampleRequest
            object <- expectObject value

            KeyMap.lookup "model" object `shouldBe` Just (Aeson.String "grok-4.6")
            KeyMap.lookup "store" object `shouldBe` Just (Aeson.Bool False)
            KeyMap.lookup "stream" object `shouldBe` Just (Aeson.Bool True)
            KeyMap.lookup "reasoning" object `shouldBe` Just (Aeson.object
                [ "effort" .= ("high" :: Text)
                , "summary" .= ("concise" :: Text)
                ])
            KeyMap.lookup "include" object `shouldBe` Just (Aeson.toJSON
                [ "reasoning.encrypted_content" :: Text ])
            KeyMap.lookup "prompt_cache_key" object `shouldBe` Just (Aeson.String "cache-1")

            -- ChatGPT-only fields must not reach the proxy.
            KeyMap.lookup "instructions" object `shouldBe` Nothing
            KeyMap.lookup "text" object `shouldBe` Nothing
            KeyMap.lookup "tool_choice" object `shouldBe` Nothing
            KeyMap.lookup "parallel_tool_calls" object `shouldBe` Nothing

        it "uses the configured default when the canonical model is absent" do
            let request = setModel Nothing sampleRequest
                value = requestValue defaultClientOptions request
            object <- expectObject value
            KeyMap.lookup "model" object
                `shouldBe` Just (Aeson.String defaultClientOptions.defaultModel)

        it "turns instructions into a leading system message item" do
            let value = requestValue defaultClientOptions sampleRequest
            object <- expectObject value
            input <- expectArray (KeyMap.lookup "input" object)
            firstItem <- case input of
                (item : _) -> expectObject item
                [] -> expectationFailure "input is empty" >> fail "unreachable"
            KeyMap.lookup "type" firstItem `shouldBe` Just (Aeson.String "message")
            KeyMap.lookup "role" firstItem `shouldBe` Just (Aeson.String "system")
            length input `shouldBe` 2

        it "omits the system item when instructions are blank" do
            let value = requestValue defaultClientOptions
                    (setInstructions (Just "  ") sampleRequest)
            object <- expectObject value
            input <- expectArray (KeyMap.lookup "input" object)
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
            -- external_web_access is a ChatGPT knob the proxy does not know.
            Maybe.mapMaybe (KeyMap.lookup "external_web_access") toolObjects `shouldBe` []

        it "omits include when the request asks for none" do
            let value = requestValue defaultClientOptions
                    (setInclude Nothing sampleRequest)
            object <- expectObject value
            KeyMap.lookup "include" object `shouldBe` Nothing

        it "clamps reasoning efforts grok does not offer" do
            let effortOf request = do
                    object <- expectObject (requestValue defaultClientOptions request)
                    reasoning <- expectObject =<< maybe
                        (expectationFailure "missing reasoning" >> fail "unreachable")
                        pure
                        (KeyMap.lookup "reasoning" object)
                    pure (KeyMap.lookup "effort" reasoning)
            effortOf (withEffort "none" sampleRequest)
                >>= (`shouldBe` Just (Aeson.String "low"))
            effortOf (withEffort "minimal" sampleRequest)
                >>= (`shouldBe` Just (Aeson.String "low"))
            effortOf (withEffort "medium" sampleRequest)
                >>= (`shouldBe` Just (Aeson.String "medium"))
            effortOf (withEffort "low" sampleRequest)
                >>= (`shouldBe` Just (Aeson.String "low"))
            effortOf (withEffort "xhigh" sampleRequest)
                >>= (`shouldBe` Just (Aeson.String "high"))
            effortOf (withEffort "max" sampleRequest)
                >>= (`shouldBe` Just (Aeson.String "high"))
            -- Unset effort defaults to high for Grok.
            effortOf (clearEffort sampleRequest)
                >>= (`shouldBe` Just (Aeson.String "high"))

    describe "classifyFailure" do
        it "types a bare 429 and honours the Retry-After header" do
            classifyFailure 429 (Just 90) "too many requests"
                `shouldBe` ProviderError RateLimitError "too many requests" (Just 90)

        it "keeps the typed envelope and only fills a missing retry interval" do
            let envelope = "{\"error\":{\"type\":\"usage_limit_reached\",\"message\":\"limited\",\"resets_in_seconds\":300}}"
            classifyFailure 429 (Just 90) envelope
                `shouldBe` ProviderError UsageLimitReached "limited" (Just 300)
            let bare = "{\"error\":{\"type\":\"rate_limit_error\",\"message\":\"limited\"}}"
            classifyFailure 429 (Just 90) bare
                `shouldBe` ProviderError RateLimitError "limited" (Just 90)

        it "recognises the subscription upsell as an exhausted usage window" do
            let body = "You've reached your free Grok Build usage limit for now. Get SuperGrok for much higher limits, or try again later: https://grok.com/supergrok?referrer=grok-build"
            case classifyFailure 429 Nothing body of
                ProviderError UsageLimitReached _ _ -> pure ()
                other -> expectationFailure ("expected UsageLimitReached, got " <> show other)

        it "recognises an exhausted Grok Build usage balance" do
            let body = "{\"error\":\"Grok Build usage balance exhausted\"}"
            classifyFailure 402 Nothing body
                `shouldBe` ProviderError UsageBalanceExhausted body Nothing
            retryDisposition (classifyFailure 402 (Just 3600) body)
                `shouldBe` RetryAfterLimitReset

        it "types Grok's flat invalid-image envelope" do
            classifyFailure 400 Nothing
                "{\"code\":\"invalid_image\",\"error\":\"Invalid PNG image.\"}"
                `shouldBe` ProviderError InvalidImageError
                    "Invalid PNG image. (code: invalid_image)"
                    Nothing

        it "leaves other statuses as plain HTTP errors" do
            classifyFailure 503 Nothing "unavailable"
                `shouldBe` HttpError 503 "unavailable"

        it "types capacity pressure as OverloadedError with a 30s retry" do
            let body =
                    "The model is currently at capacity due to high demand. \
                    \Please try again in a few minutes, or use a higher service \
                    \tier for priority processing: \
                    \https://docs.x.ai/developers/advanced-api-usage/priority-processing"
            classifyFailure 503 Nothing body
                `shouldBe` ProviderError OverloadedError body (Just 30)
            classifyFailure 429 (Just 12) body
                `shouldBe` ProviderError OverloadedError body (Just 12)

    describe "classifyStreamError" do
        it "uses a code-only stream error as the typed discriminator" do
            let streamError = ResponseStreamError
                    { errorType = Nothing
                    , code = Just "context_length_exceeded"
                    , message = "prompt is too long"
                    , param = Nothing
                    , retryAfter = Nothing
                    , extraFields = mempty
                    }
            classifyStreamError streamError
                `shouldBe` ProviderError ContextWindowExceeded
                    "prompt is too long (code: context_length_exceeded)"
                    Nothing

        it "types unstructured capacity stream errors as OverloadedError" do
            let message =
                    "The model is currently at capacity due to high demand. \
                    \Please try again in a few minutes, or use a higher service \
                    \tier for priority processing"
                streamError = ResponseStreamError
                    { errorType = Nothing
                    , code = Nothing
                    , message
                    , param = Nothing
                    , retryAfter = Nothing
                    , extraFields = mempty
                    }
            classifyStreamError streamError
                `shouldBe` ProviderError OverloadedError message (Just 30)

    describe "SSE assembly" do
        it "decodes typed event constructors and builds the merged final response" do
            let sse = Text.intercalate ""
                    [ sseBlock "response.output_item.done"
                        "{\"type\":\"response.output_item.done\",\"output_index\":0,\"item\":{\"type\":\"function_call\",\"call_id\":\"call-1\",\"name\":\"echo\",\"arguments\":\"{}\"}}"
                    , sseBlock "response.completed"
                        "{\"type\":\"response.completed\",\"response\":{\"id\":\"resp-1\",\"created_at\":0,\"model\":\"grok-4.6\",\"status\":\"completed\",\"output\":[],\"usage\":{\"input_tokens\":10,\"output_tokens\":5,\"total_tokens\":15}}}"
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
                "data: {\"type\":\"response.completed\",\"response\":{\"id\":\"resp-2\",\"created_at\":0,\"model\":\"grok-4.6\",\"status\":\"completed\"}}\n\n"
            map responseStreamEventType events `shouldBe` [EventResponseCompleted]

        it "returns a terminal incomplete response instead of hanging" do
            events <- expectRight $ parseSseEvents $ sseBlock "response.incomplete"
                "{\"type\":\"response.incomplete\",\"response\":{\"id\":\"resp-i\",\"created_at\":0,\"model\":\"grok-4.6\",\"status\":\"incomplete\",\"output\":[],\"incomplete_details\":{\"reason\":\"max_output_tokens\"}}}"
            response <- expectRight (buildResponse events)
            response.responseId `shouldBe` "resp-i"
            response.status `shouldBe` ResponseIncomplete

        it "surfaces typed stream errors" do
            events <- expectRight $ parseSseEvents $ sseBlock "error"
                "{\"type\":\"error\",\"error\":{\"type\":\"usage_limit_reached\",\"message\":\"limited\",\"resets_in_seconds\":120}}"
            buildResponse events
                `shouldBe` Left (ProviderError UsageLimitReached "limited" (Just 120))

        it "maps response.failed and missing completion to transport-level errors" do
            failedEvents <- expectRight $ parseSseEvents $ sseBlock "response.failed"
                "{\"type\":\"response.failed\",\"response\":{\"id\":\"resp-f\",\"created_at\":0,\"model\":\"grok-4.6\",\"status\":\"failed\",\"incomplete_details\":{\"reason\":\"overloaded\"}}}"
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
    { model = Just "gpt-5.6-terra"
    , instructions = Just "You are a bookkeeping agent."
    , input = Just (ResponseInputItems
        [ MessageItem ResponseMessage
            { messageId = Nothing
            , role = RoleUser
            , content = MessageContentParts [InputTextPart "hello" Nothing mempty]
            , status = Nothing
            , phase = Nothing
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
    , reasoning = Just (reasoningConfig "high")
    , include = Just [ResponseInclude "reasoning.encrypted_content"]
    , promptCacheKey = Just "cache-1"
    }

reasoningConfig :: Text -> ReasoningConfig
reasoningConfig effort = ReasoningConfig
    { context = Nothing
    , effort = Just effort
    , generateSummary = Nothing
    , reasoningMode = Nothing
    , summary = Nothing
    , extraFields = mempty
    }

withEffort :: Text -> ResponseCreateParams -> ResponseCreateParams
withEffort effort ResponseCreateParams { reasoning = _, .. } =
    ResponseCreateParams { reasoning = Just (reasoningConfig effort), .. }

clearEffort :: ResponseCreateParams -> ResponseCreateParams
clearEffort ResponseCreateParams { reasoning = _, .. } =
    ResponseCreateParams { reasoning = Nothing, .. }

setInstructions :: Maybe Text -> ResponseCreateParams -> ResponseCreateParams
setInstructions newInstructions ResponseCreateParams { instructions = _, .. } =
    ResponseCreateParams { instructions = newInstructions, .. }

setInclude :: Maybe [ResponseInclude] -> ResponseCreateParams -> ResponseCreateParams
setInclude newInclude ResponseCreateParams { include = _, .. } =
    ResponseCreateParams { include = newInclude, .. }

setModel :: Maybe Text -> ResponseCreateParams -> ResponseCreateParams
setModel newModel ResponseCreateParams { model = _, .. } =
    ResponseCreateParams { model = newModel, .. }

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
