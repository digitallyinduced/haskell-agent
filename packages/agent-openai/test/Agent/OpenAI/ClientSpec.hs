module Agent.OpenAI.ClientSpec (spec) where

import Agent.OpenAI.Client
import Agent.OpenAI.Credential (staticBearerProvider)
import Agent.Error
import Agent.OpenAI.Http
import Agent.Provider
import Agent.OpenAI.Responses.Types
import Control.Retry (constantDelay, limitRetries)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
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
                provider <- pure $ TokenProvider \_ -> pure (Right credential)
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
    , headers :: ![(Text, Text)]
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
        let request = RecordedRequest
                { path = "/" <> Text.intercalate "/" (Wai.pathInfo waiRequest)
                , headers =
                    [ (Text.decodeUtf8 (CI.original name), Text.decodeUtf8 value)
                    | (name, value) <- Wai.requestHeaders waiRequest
                    ]
                }
        atomicModifyIORef' recorded \requests -> (requests <> [request], ())
        respond =<< handler request

jsonCompleted :: Text -> Wai.Response
jsonCompleted text = Wai.responseLBS HTTP.status200
    [("Content-Type", "application/json")]
    (Aeson.encode (Aeson.object
        [ "id" .= ("resp-json" :: Text)
        , "created_at" .= (0 :: Int)
        , "model" .= ("gpt-test" :: Text)
        , "status" .= ("completed" :: Text)
        , "output" .= [assistantMessage text]
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
