module Agent.OpenAI.ClientSpec (spec) where

import Agent.OpenAI.Client
import Agent.OpenAI.Credential (staticBearerProvider)
import Agent.Error
import Agent.OpenAI.Http
import Agent.OpenAI.WebSocketClient
    ( newCodexTurnState
    , readCodexTurnState
    , recordCodexTurnState
    )
import Agent.Provider
import Agent.Responses.Types
import Control.Concurrent (threadDelay)
import Control.Retry (constantDelay, limitRetries)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as Builder
import qualified Data.CaseInsensitive as CI
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Test.Hspec

spec :: Spec
spec = do
    describe "rejectFailedCodexResponse" do
        it "turns an HTTP-200 server_is_overloaded payload into a typed error" do
            let response = responseWithStatus "failed" (Just (Aeson.object
                    [ "type" .= ("server_error" :: Text)
                    , "code" .= ("server_is_overloaded" :: Text)
                    , "message" .= ("Our servers are currently overloaded" :: Text)
                    ]))
            rejectFailedCodexResponse response
                `shouldBe` Left (ProviderError OverloadedError
                    "Our servers are currently overloaded (code: server_is_overloaded)"
                    Nothing)

        it "keeps completed responses" do
            let response = responseWithStatus "completed" Nothing
            rejectFailedCodexResponse response `shouldBe` Right response

        it "turns an incomplete response into an error with its reason" do
            let response = decodeResponse (Aeson.object
                    [ "id" .= ("response-id" :: Text)
                    , "created_at" .= (0 :: Int)
                    , "model" .= ("gpt-5.6-sol" :: Text)
                    , "status" .= ("incomplete" :: Text)
                    , "incomplete_details" .= Aeson.object
                        [ "reason" .= ("max_output_tokens" :: Text) ]
                    , "output" .= ([] :: [Aeson.Value])
                    ])
            rejectFailedCodexResponse response
                `shouldBe` Left (ProviderError ApiErrorType
                    "response.incomplete: max_output_tokens"
                    Nothing)

        it "keeps an incomplete response that already contains tool calls" do
            let response = decodeResponse (Aeson.object
                    [ "id" .= ("response-id" :: Text)
                    , "created_at" .= (0 :: Int)
                    , "model" .= ("gpt-5.6-sol" :: Text)
                    , "status" .= ("incomplete" :: Text)
                    , "incomplete_details" .= Aeson.object
                        [ "reason" .= ("max_output_tokens" :: Text) ]
                    , "output" .=
                        [ Aeson.object
                            [ "type" .= ("function_call" :: Text)
                            , "call_id" .= ("call-1" :: Text)
                            , "name" .= ("shell_command" :: Text)
                            , "arguments" .= ("{}" :: Text)
                            ]
                        ]
                    ])
            rejectFailedCodexResponse response `shouldBe` Right response

        it "keeps an incomplete response that already contains reasoning" do
            let response = decodeResponse (Aeson.object
                    [ "id" .= ("response-id" :: Text)
                    , "created_at" .= (0 :: Int)
                    , "model" .= ("gpt-5.6-sol" :: Text)
                    , "status" .= ("incomplete" :: Text)
                    , "incomplete_details" .= Aeson.object
                        [ "reason" .= ("max_output_tokens" :: Text) ]
                    , "output" .=
                        [ Aeson.object
                            [ "type" .= ("reasoning" :: Text)
                            , "id" .= ("rs-1" :: Text)
                            , "summary" .= ([] :: [Aeson.Value])
                            ]
                        ]
                    ])
            rejectFailedCodexResponse response `shouldBe` Right response

    describe "retryTransientCodexResultWithPolicy" do
        it "retries ordinary Left overloads before returning success" do
            let overload = ProviderError OverloadedError "server_is_overloaded" Nothing
            responses <- newIORef
                [ Left overload
                , Left overload
                , Left overload
                , Right ("completed" :: Text)
                ]
            result <- retryTransientCodexResultWithPolicy
                (constantDelay 0 <> limitRetries 3)
                (atomicModifyIORef' responses \case
                    next : rest -> (rest, next)
                    [] -> error "unexpected extra Codex request")
            result `shouldBe` Right "completed"
            readIORef responses `shouldReturn` []

        it "returns the last overload after the retry budget is exhausted" do
            let overload = ProviderError OverloadedError "server_is_overloaded" Nothing
            attempts <- newIORef (0 :: Int)
            result <- retryTransientCodexResultWithPolicy
                (constantDelay 0 <> limitRetries 3)
                (modifyIORef' attempts (+ 1)
                    >> pure (Left overload :: Either ApiError Text))
            result `shouldBe` Left overload
            readIORef attempts `shouldReturn` 4

        it "does not retry quota errors" do
            attempts <- newIORef (0 :: Int)
            let quota = ProviderError UsageLimitReached "quota" (Just 3600)
            result <- retryTransientCodexResultWithPolicy
                (constantDelay 0 <> limitRetries 3)
                (modifyIORef' attempts (+ 1)
                    >> pure (Left quota :: Either ApiError Text))
            result `shouldBe` Left quota
            readIORef attempts `shouldReturn` 1

    describe "decodeCodexHttpBody" do
        it "rejects an empty incomplete SSE response" do
            let body = Text.concat
                    [ "event: response.created\n"
                    , "data: {\"type\":\"response.created\","
                    , "\"response\":{\"id\":\"resp-empty-incomplete\"}}\n\n"
                    , "event: response.incomplete\n"
                    , "data: "
                    , Text.decodeUtf8 (LBS.toStrict (Aeson.encode (Aeson.object
                        [ "response" .= Aeson.object
                            [ "id" .= ("resp-empty-incomplete" :: Text)
                            , "created_at" .= (0 :: Int)
                            , "model" .= ("gpt-test" :: Text)
                            , "status" .= ("incomplete" :: Text)
                            , "incomplete_details" .= Aeson.object
                                [ "reason" .= ("max_output_tokens" :: Text) ]
                            , "output" .= ([] :: [Aeson.Value])
                            ]
                        ])))
                    , "\n\n"
                    ]
            decodeCodexHttpBody body `shouldBe` Left
                (ProviderError ApiErrorType
                    "response.incomplete: max_output_tokens"
                    Nothing)

        it "reads function-call arguments from SSE deltas when completed output is empty" do
            let body = Text.concat
                    [ "event: response.output_item.added\n"
                    , "data: {\"output_index\":0,\"item\":"
                    , "{\"type\":\"function_call\",\"id\":\"fc-1\",\"call_id\":\"call-1\",\"name\":\"read_file\",\"arguments\":\"\"}"
                    , "}\n\n"
                    , "event: response.function_call_arguments.done\n"
                    , "data: {\"item_id\":\"fc-1\",\"output_index\":0,\"name\":\"read_file\",\"arguments\":\"{\\\"target_file\\\":\\\"README.md\\\"}\"}\n\n"
                    , "event: response.completed\n"
                    , "data: "
                    , Text.decodeUtf8 (LBS.toStrict (Aeson.encode (Aeson.object
                        [ "response" .= Aeson.object
                            [ "id" .= ("resp-args" :: Text)
                            , "created_at" .= (0 :: Int)
                            , "model" .= ("gpt-test" :: Text)
                            , "status" .= ("completed" :: Text)
                            , "output" .= ([] :: [Aeson.Value])
                            ]
                        ])))
                    , "\n\n"
                    ]
            case decodeCodexHttpBody body of
                Right response -> do
                    response.responseId `shouldBe` "resp-args"
                    [(name, arguments) | FunctionCallItem FunctionCall { name, arguments } <- response.output]
                        `shouldBe` [("read_file", "{\"target_file\":\"README.md\"}")]
                Left err -> expectationFailure ("expected response, got " <> show err)

        it "reads an incomplete SSE response that already contains a function call" do
            let body = Text.concat
                    [ "event: response.output_item.done\n"
                    , "data: {\"item\":"
                    , "{\"type\":\"function_call\",\"call_id\":\"call-1\",\"name\":\"shell_command\",\"arguments\":\"{}\"}"
                    , "}\n\n"
                    , "event: response.incomplete\n"
                    , "data: "
                    , Text.decodeUtf8 (LBS.toStrict (Aeson.encode (Aeson.object
                        [ "response" .= Aeson.object
                            [ "id" .= ("resp-incomplete" :: Text)
                            , "created_at" .= (0 :: Int)
                            , "model" .= ("gpt-test" :: Text)
                            , "status" .= ("incomplete" :: Text)
                            , "incomplete_details" .= Aeson.object
                                [ "reason" .= ("max_output_tokens" :: Text) ]
                            , "output" .= ([] :: [Aeson.Value])
                            ]
                        ])))
                    , "\n\n"
                    ]
            case decodeCodexHttpBody body of
                Right response -> do
                    response.responseId `shouldBe` "resp-incomplete"
                    response.status `shouldBe` ResponseIncomplete
                    [name | FunctionCallItem FunctionCall { name } <- response.output]
                        `shouldBe` ["shell_command"]
                Left err -> expectationFailure ("expected response, got " <> show err)

        it "reads the terminal SSE response.completed event" do
            let body = Text.concat
                    [ "event: response.output_item.done\n"
                    , "data: {\"item\":"
                    , Text.decodeUtf8 (LBS.toStrict (Aeson.encode (assistantMessage "from-item")))
                    , "}\n\n"
                    , "event: response.completed\n"
                    , "data: "
                    , Text.decodeUtf8 (LBS.toStrict (Aeson.encode (Aeson.object
                        [ "response" .= Aeson.object
                            [ "id" .= ("resp-sse" :: Text)
                            , "created_at" .= (0 :: Int)
                            , "model" .= ("gpt-test" :: Text)
                            , "status" .= ("completed" :: Text)
                            , "output" .= ([] :: [Aeson.Value])
                            ]
                        ])))
                    , "\n\n"
                    ]
            case decodeCodexHttpBody body of
                Right response -> do
                    response.responseId `shouldBe` "resp-sse"
                    extractAssistantText response `shouldBe` Just "from-item"
                Left err -> expectationFailure ("expected response, got " <> show err)

        it "uses the request model for partial lifecycle frames" do
            let body = Text.concat
                    [ "event: response.created\n"
                    , "data: {\"type\":\"response.created\","
                    , "\"response\":{\"id\":\"resp-partial\"}}\n\n"
                    , "event: response.completed\n"
                    , "data: {\"type\":\"response.completed\","
                    , "\"response\":{}}\n\n"
                    ]
            case decodeCodexHttpBodyWithModel (Just "gpt-request") body of
                Right response -> response.model `shouldBe` "gpt-request"
                Left err -> expectationFailure
                    ("expected response, got " <> show err)

        it "accepts a non-streaming JSON Responses body" do
            let body = Text.decodeUtf8 $ LBS.toStrict $ Aeson.encode $ Aeson.object
                    [ "id" .= ("resp-json" :: Text)
                    , "created_at" .= (0 :: Int)
                    , "model" .= ("gpt-test" :: Text)
                    , "status" .= ("completed" :: Text)
                    , "output" .= [assistantMessage "Apartment"]
                    ]
            case decodeCodexHttpBody body of
                Right response -> do
                    response.responseId `shouldBe` "resp-json"
                    extractAssistantText response `shouldBe` Just "Apartment"
                Left err -> expectationFailure ("expected response, got " <> show err)

    describe "createCodexMessageWithProviderAt" do
        it "POSTs to the given base URL without a ChatGPT account header" do
            recorded <- newIORef []
            let handler _request = pure $ jsonCompleted "Apartment"
            withMockResponses recorded handler \baseUrl -> do
                result <- createCodexMessageWithProviderAt
                    baseUrl
                    (staticBearerProvider "router-key")
                    (helloRequest "hi")
                response <- expectRight result
                extractAssistantText response `shouldBe` Just "Apartment"

            [request] <- readIORef recorded
            request.path `shouldBe` "/v1/responses"
            lookup "Authorization" request.headers `shouldBe` Just "Bearer router-key"
            lookup "chatgpt-account-id" request.headers `shouldBe` Nothing
            lookup "x-codex-beta-features" request.headers
                `shouldBe` Just "remote_compaction_v2"
            lookup "x-openai-internal-codex-responses-lite" request.headers
                `shouldBe` Just "true"
            case request.body of
                Aeson.Object object ->
                    KeyMap.lookup "stream" object `shouldBe` Just (Aeson.Bool True)
                other ->
                    expectationFailure
                        ("expected JSON request object, got " <> show other)

        it "replays first-write-wins turn state across HTTP tool continuations" do
            recorded <- newIORef []
            responseNumber <- newIORef (0 :: Int)
            turnState <- newCodexTurnState
            let handler _request = do
                    number <- atomicModifyIORef' responseNumber \current ->
                        let next = current + 1
                        in (next, next)
                    pure $ case number of
                        1 -> jsonFunctionCall
                            [("x-codex-turn-state", "ts-1")]
                            "call-1"
                        2 -> jsonFunctionCall
                            [("x-codex-turn-state", "ts-2")]
                            "call-2"
                        3 -> jsonCompleted "done"
                        4 -> jsonCompleted "new turn"
                        _ -> error "unexpected extra request"
            withMockResponses recorded handler \baseUrl -> do
                _ <- expectRight =<<
                    createCodexMessageWithProviderAtWithTurnState
                        baseUrl turnState
                        (staticBearerProvider "router-key")
                        (helloRequest "first")
                readCodexTurnState turnState `shouldReturn` Just "ts-1"

                _ <- expectRight =<<
                    createCodexMessageWithProviderAtWithTurnState
                        baseUrl turnState
                        (staticBearerProvider "router-key")
                        (helloRequest "tool one")
                -- A later response cannot replace the sticky-routing token.
                readCodexTurnState turnState `shouldReturn` Just "ts-1"

                _ <- expectRight =<<
                    createCodexMessageWithProviderAtWithTurnState
                        baseUrl turnState
                        (staticBearerProvider "router-key")
                        (helloRequest "tool two")
                readCodexTurnState turnState `shouldReturn` Nothing

                _ <- expectRight =<<
                    createCodexMessageWithProviderAtWithTurnState
                        baseUrl turnState
                        (staticBearerProvider "router-key")
                        (helloRequest "next turn")
                pure ()

            requests <- readIORef recorded
            map (lookup "x-codex-turn-state" . (.headers)) requests
                `shouldBe`
                    [ Nothing
                    , Just "ts-1"
                    , Just "ts-1"
                    , Nothing
                    ]

        it "preserves shared turn state after inline remote compaction" do
            recorded <- newIORef []
            turnState <- newCodexTurnState
            recordCodexTurnState turnState "ts-compaction"
            let handler _request = pure $ jsonCompleted "compacted"
            withMockResponses recorded handler \baseUrl -> do
                _ <- expectRight =<<
                    createCodexMessageWithProviderAtWithOptionsAndTurnState
                        remoteCompactionV2RequestOptions
                        baseUrl
                        turnState
                        (staticBearerProvider "router-key")
                        (helloRequest "compact")
                readCodexTurnState turnState
                    `shouldReturn` Just "ts-compaction"

            [request] <- readIORef recorded
            lookup "x-codex-turn-state" request.headers
                `shouldBe` Just "ts-compaction"

        it "returns as soon as terminal SSE arrives without waiting for EOF" do
            recorded <- newIORef []
            let options = defaultCodexRequestOptions
                    { responseIdleTimeoutMicros = 20_000 }
                handler _request =
                    pure $ hangingSseCompleted "from-terminal"
            withMockResponses recorded handler \baseUrl -> do
                response <- expectRight =<<
                    createCodexMessageWithProviderAtWithOptions
                        options
                        baseUrl
                        (staticBearerProvider "router-key")
                        (helloRequest "hi")
                extractAssistantText response `shouldBe` Just "from-terminal"

        it "keeps HTTP 429 and Retry-After when the error body stalls" do
            recorded <- newIORef []
            let options = defaultCodexRequestOptions
                    { responseIdleTimeoutMicros = 20_000 }
                handler _request = pure stalledRateLimit
            withMockResponses recorded handler \baseUrl -> do
                result <- createCodexMessageWithProviderAtWithOptions
                    options
                    baseUrl
                    (staticBearerProvider "router-key")
                    (helloRequest "hi")
                case result of
                    Left CredentialsExhausted
                            { exhaustionReasons =
                                [ExhaustedByRateLimit
                                    { exhaustionErrorType
                                    , exhaustionStatusCode
                                    , exhaustionRetryAfter
                                    }]
                            } -> do
                        exhaustionErrorType `shouldBe` Just RateLimitError
                        exhaustionStatusCode `shouldBe` Nothing
                        exhaustionRetryAfter `shouldBe` Just 17
                    other -> expectationFailure
                        ("expected typed rate-limit exhaustion, got " <> show other)

        it "preserves a base URL query while appending the responses path" do
            recorded <- newIORef []
            let handler _request = pure $ jsonCompleted "ok"
            withMockResponses recorded handler \baseUrl -> do
                _ <- expectRight =<< createCodexMessageWithProviderAt
                    (baseUrl <> "?route=blue")
                    (staticBearerProvider "router-key")
                    (helloRequest "hi")
                pure ()

            [request] <- readIORef recorded
            request.path `shouldBe` "/v1/responses"
            request.query `shouldBe` "?route=blue"

        it "strips prompt cache retention from the HTTP request body" do
            recorded <- newIORef []
            let handler _request = pure $ jsonCompleted "Apartment"
                request = withPromptCacheRetention
                    (Just "24h")
                    (helloRequest "hi")
            withMockResponses recorded handler \baseUrl -> do
                _ <- expectRight =<< createCodexMessageWithProviderAt
                    baseUrl
                    (staticBearerProvider "router-key")
                    request
                pure ()

            [recordedRequest] <- readIORef recorded
            case recordedRequest.body of
                Aeson.Object object ->
                    KeyMap.lookup "prompt_cache_retention" object
                        `shouldBe` Nothing
                other ->
                    expectationFailure
                        ("expected JSON request object, got " <> show other)

        it "sends chatgpt-account-id when the credential has one" do
            recorded <- newIORef []
            let handler _request = pure $ jsonCompleted "ok"
                credential = Credential
                    { accessToken = "chatgpt-token"
                    , accountId = "acc-123"
                    , leaseId = Nothing
                    , provider = OpenAIProvider
                    }
            withMockResponses recorded handler \baseUrl -> do
                provider <- pure $
                    tokenProvider SubscriptionBilled \_ ->
                        pure (Right credential)
                _ <- expectRight =<< createCodexMessageWithProviderAt
                    baseUrl
                    provider
                    (helloRequest "hi")
                pure ()

            [request] <- readIORef recorded
            lookup "chatgpt-account-id" request.headers `shouldBe` Just "acc-123"

        it "parses an SSE completed stream from a compatible proxy" do
            recorded <- newIORef []
            let handler _request = pure $ sseCompleted "from-sse"
            withMockResponses recorded handler \baseUrl -> do
                result <- createCodexMessageWithProviderAt
                    baseUrl
                    (staticBearerProvider "router-key")
                    (helloRequest "hi")
                response <- expectRight result
                extractAssistantText response `shouldBe` Just "from-sse"

        it "advertises remote compaction v2 and merges its streamed output item" do
            recorded <- newIORef []
            let handler _request = pure sseCompactionCompleted
            withMockResponses recorded handler \baseUrl -> do
                result <- createCodexMessageWithProviderAtWithOptions
                    remoteCompactionV2RequestOptions
                    baseUrl
                    (staticBearerProvider "router-key")
                    (helloRequest "compact")
                response <- expectRight result
                case response.output of
                    [CompactionItemValue item] ->
                        item.encryptedContent `shouldBe` Just "opaque"
                    other ->
                        expectationFailure
                            ("expected one compaction output, got " <> show other)

            [request] <- readIORef recorded
            lookup "x-codex-beta-features" request.headers
                `shouldBe` Just "remote_compaction_v2"
            case request.body of
                Aeson.Object object ->
                    KeyMap.lookup "stream" object `shouldBe` Just (Aeson.Bool True)
                other ->
                    expectationFailure
                        ("expected JSON request object, got " <> show other)

responseWithStatus :: Text -> Maybe Aeson.Value -> Response
responseWithStatus status errorValue = decodeResponse (Aeson.object
    [ "id" .= ("response-id" :: Text)
    , "created_at" .= (0 :: Int)
    , "model" .= ("gpt-5.6-sol" :: Text)
    , "status" .= status
    , "error" .= errorValue
    , "output" .= ([] :: [Aeson.Value])
    ])

data RecordedRequest = RecordedRequest
    { path :: !Text
    , query :: !Text
    , headers :: ![(Text, Text)]
    , body :: !Aeson.Value
    }

withMockResponses
    :: IORef [RecordedRequest]
    -> (RecordedRequest -> IO Wai.Response)
    -> (Text -> IO a)
    -> IO a
withMockResponses recorded handler action =
    Warp.testWithApplication (pure app) \port ->
        action ("http://127.0.0.1:" <> Text.pack (show port) <> "/v1")
  where
    app waiRequest respond = do
        requestBody <- Wai.strictRequestBody waiRequest
        let request = RecordedRequest
                { path = "/" <> Text.intercalate "/" (Wai.pathInfo waiRequest)
                , query = Text.decodeUtf8 (Wai.rawQueryString waiRequest)
                , headers =
                    [ (Text.decodeUtf8 (CI.original name), Text.decodeUtf8 value)
                    | (name, value) <- Wai.requestHeaders waiRequest
                    ]
                , body =
                    case Aeson.eitherDecode requestBody of
                        Right value -> value
                        Left _ -> Aeson.Null
                }
        atomicModifyIORef' recorded \requests -> (requests <> [request], ())
        respond =<< handler request

jsonCompleted :: Text -> Wai.Response
jsonCompleted text = jsonResponse
    []
    [assistantMessage text]

jsonFunctionCall :: HTTP.ResponseHeaders -> Text -> Wai.Response
jsonFunctionCall headers callId = jsonResponse
    headers
    [ Aeson.object
        [ "type" .= ("function_call" :: Text)
        , "call_id" .= callId
        , "name" .= ("shell_command" :: Text)
        , "arguments" .= ("{}" :: Text)
        , "status" .= ("completed" :: Text)
        ]
    ]

jsonResponse :: HTTP.ResponseHeaders -> [Aeson.Value] -> Wai.Response
jsonResponse headers output = Wai.responseLBS HTTP.status200
    (("Content-Type", "application/json") : headers)
    (Aeson.encode (Aeson.object
        [ "id" .= ("resp-json" :: Text)
        , "created_at" .= (0 :: Int)
        , "model" .= ("gpt-test" :: Text)
        , "status" .= ("completed" :: Text)
        , "output" .= output
        ]))

sseCompleted :: Text -> Wai.Response
sseCompleted text =
    let payload = Aeson.object
            [ "type" .= ("response.completed" :: Text)
            , "response" .= Aeson.object
                [ "id" .= ("resp-sse" :: Text)
                , "created_at" .= (0 :: Int)
                , "model" .= ("gpt-test" :: Text)
                , "status" .= ("completed" :: Text)
                , "output" .= [assistantMessage text]
                ]
            ]
        body = "event: response.completed\ndata: "
            <> Text.decodeUtf8 (LBS.toStrict (Aeson.encode payload))
            <> "\n\n"
    in Wai.responseLBS HTTP.status200
        [("Content-Type", "text/event-stream")]
        (LBS.fromStrict (Text.encodeUtf8 body))

hangingSseCompleted :: Text -> Wai.Response
hangingSseCompleted text =
    let payload = Aeson.object
            [ "type" .= ("response.completed" :: Text)
            , "response" .= Aeson.object
                [ "id" .= ("resp-sse" :: Text)
                , "created_at" .= (0 :: Int)
                , "model" .= ("gpt-test" :: Text)
                , "status" .= ("completed" :: Text)
                , "output" .= [assistantMessage text]
                ]
            ]
        body = "event: response.completed\ndata: "
            <> Aeson.encode payload
            <> "\n\n"
    in Wai.responseStream HTTP.status200
        [("Content-Type", "text/event-stream")]
        \send flush -> do
            send (Builder.lazyByteString body)
            flush
            threadDelay 200_000

stalledRateLimit :: Wai.Response
stalledRateLimit =
    Wai.responseStream HTTP.status429
        [ ("Content-Type", "application/json")
        , ("Retry-After", "17")
        ]
        \_send _flush -> threadDelay 200_000

sseCompactionCompleted :: Wai.Response
sseCompactionCompleted =
    let item = Aeson.object
            [ "type" .= ("compaction" :: Text)
            , "encrypted_content" .= ("opaque" :: Text)
            ]
        done = Aeson.object
            [ "type" .= ("response.output_item.done" :: Text)
            , "item" .= item
            ]
        completed = Aeson.object
            [ "type" .= ("response.completed" :: Text)
            , "response" .= Aeson.object
                [ "id" .= ("resp-compact" :: Text)
                , "created_at" .= (0 :: Int)
                , "model" .= ("gpt-test" :: Text)
                , "status" .= ("completed" :: Text)
                , "output" .= ([] :: [Aeson.Value])
                ]
            ]
        body = Text.concat
            [ "event: response.output_item.done\ndata: "
            , Text.decodeUtf8 (LBS.toStrict (Aeson.encode done))
            , "\n\n"
            , "event: response.completed\ndata: "
            , Text.decodeUtf8 (LBS.toStrict (Aeson.encode completed))
            , "\n\n"
            ]
    in Wai.responseLBS HTTP.status200
        [("Content-Type", "text/event-stream")]
        (LBS.fromStrict (Text.encodeUtf8 body))

assistantMessage :: Text -> Aeson.Value
assistantMessage text = Aeson.object
    [ "type" .= ("message" :: Text)
    , "role" .= ("assistant" :: Text)
    , "content" .=
        [ Aeson.object
            [ "type" .= ("output_text" :: Text)
            , "text" .= text
            ]
        ]
    ]

helloRequest :: Text -> ResponseCreateParams
helloRequest prompt = defaultResponseCreateParams
    { model = Just "gpt-5.6-luna"
    , instructions = Just "You are a test agent."
    , input = Just (ResponseInputText prompt)
    }

withPromptCacheRetention
    :: Maybe Text -> ResponseCreateParams -> ResponseCreateParams
withPromptCacheRetention nextRetention
        ResponseCreateParams { promptCacheRetention = _, .. } =
    ResponseCreateParams { promptCacheRetention = nextRetention, .. }

decodeResponse :: Aeson.Value -> Response
decodeResponse value = case Aeson.fromJSON value of
    Aeson.Success response -> response
    Aeson.Error err -> error err

extractAssistantText :: Response -> Maybe Text
extractAssistantText response = case
    [ value
    | MessageItem message <- response.output
    , message.role == RoleAssistant
    , value <- case message.content of
        MessageContentText text -> [text]
        MessageContentParts parts -> [text | OutputTextPart { text } <- parts]
    ] of
        [] -> Nothing
        values -> Just (Text.intercalate "\n" values)

expectRight :: Show e => Either e a -> IO a
expectRight = \case
    Left err -> expectationFailure ("expected Right, got Left " <> show err) >> fail "unreachable"
    Right value -> pure value
