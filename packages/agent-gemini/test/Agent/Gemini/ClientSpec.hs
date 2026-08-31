module Agent.Gemini.ClientSpec (spec) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Gemini.Client (createResponseWithEventsPolicy)
import Agent.Gemini.Options (ClientOptions(..), defaultClientOptions)
import Agent.Gemini.Response (GeminiStreamEvent(..))
import Agent.Gemini.TestSupport (withLoopbackApplication)
import Agent.Provider (Credential(..), Provider(..))
import Agent.Responses.Types
    ( Response(..)
    , ResponseContentPart(..)
    , ResponseCreateParams(..)
    , ResponseInput(..)
    , ResponseItem(..)
    , MessageContent(..)
    , ResponseMessage(..)
    , ResponseRole(..)
    , defaultResponseCreateParams
    )
import Control.Retry (limitRetries)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isLeft)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import Test.Hspec

spec :: Spec
spec = describe "Gemini client validation" do
    it "rejects credentials belonging to another provider without networking" do
        let credential = Credential "secret" "account" Nothing OpenAIProvider
        createResponseWithEventsPolicy
            (limitRetries 0)
            defaultClientOptions
            credential
            defaultResponseCreateParams
            (const (pure ()))
            `shouldReturn`
                Left
                    (ProviderError
                        ApiErrorType
                        "agent-gemini requires a Gemini credential"
                        Nothing)

    it "rejects an empty Gemini API key without networking" do
        let credential = Credential "" "gemini" Nothing GeminiProvider
        createResponseWithEventsPolicy
            (limitRetries 0)
            defaultClientOptions
            credential
            defaultResponseCreateParams
            (const (pure ()))
            `shouldReturn` Left (CredentialError "Gemini API key is empty")

    it "uses the native streaming endpoint and x-goog-api-key header" do
        recorded <- newIORef Nothing
        let app request respond = do
                body <- Wai.strictRequestBody request
                writeIORef recorded $ Just
                    ( Wai.rawPathInfo request
                    , Wai.rawQueryString request
                    , lookup "x-goog-api-key" (Wai.requestHeaders request)
                    , body
                    )
                respond $ Wai.responseLBS HTTP.status200
                    [("Content-Type", "text/event-stream")]
                    geminiSse
        withLoopbackApplication (pure app) \port -> do
            let options = defaultClientOptions
                    { baseUrl = "http://127.0.0.1:"
                        <> show port <> "/v1beta"
                    , requestTimeoutSeconds = 10
                    }
                credential =
                    Credential "google-key" "gemini" Nothing GeminiProvider
                request = defaultResponseCreateParams
                    { model = Just "gemini-test"
                    , instructions = Just "Be concise"
                    , input = Just (ResponseInputText "hello")
                    }
            result <- createResponseWithEventsPolicy
                (limitRetries 0)
                options
                credential
                request
                (const (pure ()))
            case result of
                Left err ->
                    expectationFailure
                        ("expected Gemini response, got " <> show err)
                Right response -> do
                    response.responseId `shouldBe` "resp-1"
                    assistantText response `shouldBe` Just "hello back"
        readIORef recorded >>= \case
            Nothing -> expectationFailure "mock server received no request"
            Just (path, query, apiKey, body) -> do
                path `shouldBe`
                    "/v1beta/models/gemini-test:streamGenerateContent"
                query `shouldBe` "?alt=sse"
                apiKey `shouldBe` Just "google-key"
                Aeson.decode body `shouldBe` Just expectedRequestBody

    it "uses bearer auth and the wrapped Code Assist streaming endpoint" do
        recorded <- newIORef Nothing
        let app request respond = do
                body <- Wai.strictRequestBody request
                writeIORef recorded $ Just
                    ( Wai.rawPathInfo request
                    , Wai.rawQueryString request
                    , lookup "Authorization" (Wai.requestHeaders request)
                    , lookup "x-goog-api-key" (Wai.requestHeaders request)
                    , body
                    )
                respond $ Wai.responseLBS HTTP.status200
                    [("Content-Type", "text/event-stream")]
                    codeAssistSse
        withLoopbackApplication (pure app) \port -> do
            let options = defaultClientOptions
                    { codeAssistBaseUrl = "http://127.0.0.1:"
                        <> show port <> "/v1internal"
                    , requestTimeoutSeconds = 10
                    }
                credential = Credential
                    "oauth-token"
                    "account@example.com"
                    (Just "code-assist:project-123")
                    GeminiProvider
                request = defaultResponseCreateParams
                    { model = Just "gemini-test"
                    , instructions = Just "Be concise"
                    , input = Just (ResponseInputText "hello")
                    }
            result <- createResponseWithEventsPolicy
                (limitRetries 0)
                options
                credential
                request
                (const (pure ()))
            case result of
                Left err ->
                    expectationFailure
                        ("expected Code Assist response, got " <> show err)
                Right response -> do
                    response.responseId `shouldBe` "trace-1"
                    assistantText response `shouldBe` Just "hello back"
        readIORef recorded >>= \case
            Nothing -> expectationFailure "mock server received no request"
            Just (path, query, authorization, apiKey, body) -> do
                path `shouldBe`
                    "/v1internal:streamGenerateContent"
                query `shouldBe` "?alt=sse"
                authorization `shouldBe` Just "Bearer oauth-token"
                apiKey `shouldBe` Nothing
                case Aeson.decode body of
                    Just (Aeson.Object object) -> do
                        KeyMap.lookup "model" object
                            `shouldBe` Just (Aeson.String "gemini-test")
                        KeyMap.lookup "project" object
                            `shouldBe` Just (Aeson.String "project-123")
                        KeyMap.lookup "request" object
                            `shouldBe` Just expectedRequestBody
                        case KeyMap.lookup "user_prompt_id" object of
                            Just (Aeson.String promptId) ->
                                promptId `shouldSatisfy`
                                    Text.isPrefixOf "gemini-gemini-test-"
                            _ -> expectationFailure
                                "Code Assist request has no user_prompt_id"
                    value -> expectationFailure
                        ("invalid Code Assist request: " <> show value)

    it "rejects an empty successful stream" do
        withGeminiServer "" \options -> do
            result <- createResponseWithEventsPolicy
                (limitRetries 0)
                options
                geminiCredential
                defaultResponseCreateParams
                (const (pure ()))
            result `shouldBe` Left
                (ConnectionError
                    "Gemini stream ended before a terminal finish reason")

    it "does not treat an unspecified prompt block as terminal" do
        withGeminiServer unspecifiedPromptBlockSse \options -> do
            result <- createResponseWithEventsPolicy
                (limitRetries 0)
                options
                geminiCredential
                defaultResponseCreateParams
                (const (pure ()))
            result `shouldBe` Left
                (ConnectionError
                    "Gemini stream ended before a terminal finish reason")

    it "does not let unspecified reasons mask a terminal reason" do
        withGeminiServer unspecifiedAroundTerminalSse \options -> do
            result <- createResponseWithEventsPolicy
                (limitRetries 0)
                options
                geminiCredential
                defaultResponseCreateParams
                (const (pure ()))
            result `shouldSatisfy` \case
                Right _ -> True
                Left _ -> False

    it "does not treat an unspecified finish reason as terminal" do
        withGeminiServer unspecifiedFinishSse \options -> do
            result <- createResponseWithEventsPolicy
                (limitRetries 0)
                options
                geminiCredential
                defaultResponseCreateParams
                (const (pure ()))
            result `shouldBe` Left
                (ConnectionError
                    "Gemini stream ended before a terminal finish reason")

    it "reports a truncated stream after preserving its emitted delta" do
        events <- newIORef []
        withGeminiServer truncatedGeminiSse \options -> do
            result <- createResponseWithEventsPolicy
                (limitRetries 0)
                options
                geminiCredential
                defaultResponseCreateParams
                (\event -> writeIORef events . (<> [event])
                    =<< readIORef events)
            result `shouldBe` Left
                (ConnectionError
                    "Gemini stream ended before a terminal finish reason")
        readIORef events `shouldReturn`
            [GeminiTextDelta "partial "]

    it "classifies non-success Google error envelopes" do
        withGeminiHttpResponse
            HTTP.status400
            [("Content-Type", "application/json")]
            invalidKeyBody
            \options -> do
                result <- createResponseWithEventsPolicy
                    (limitRetries 0)
                    options
                    geminiCredential
                    defaultResponseCreateParams
                    (const (pure ()))
                result `shouldBe` Left
                    (ProviderError
                        AuthenticationError
                        "API key not valid. Please pass a valid API key."
                        Nothing)

    it "redacts an API key echoed by an error response" do
        withGeminiHttpResponse
            HTTP.status400
            [("Content-Type", "text/plain")]
            "rejected google-key"
            \options -> do
                result <- createResponseWithEventsPolicy
                    (limitRetries 0)
                    options
                    geminiCredential
                    defaultResponseCreateParams
                    (const (pure ()))
                show result `shouldNotContain` "google-key"
                show result `shouldContain` "<redacted>"

    it "never forwards an API key across redirects" do
        redirected <- newIORef (Nothing :: Maybe HTTP.RequestHeaders)
        withLoopbackApplication
            (pure \request respond -> do
                writeIORef redirected
                    (Just (Wai.requestHeaders request))
                respond (Wai.responseLBS HTTP.status200 [] ""))
            \targetPort ->
                withLoopbackApplication
                    (pure \_ respond ->
                        respond
                            (Wai.responseLBS HTTP.status307
                                [ ( "Location"
                                  , BS8.pack
                                        ("http://127.0.0.1:"
                                            <> show targetPort
                                            <> "/captured")
                                  )
                                ]
                                ""))
                    \originPort -> do
                        let options = defaultClientOptions
                                { baseUrl =
                                    "http://127.0.0.1:"
                                        <> show originPort
                                        <> "/v1beta"
                                }
                        result <- createResponseWithEventsPolicy
                            (limitRetries 0)
                            options
                            geminiCredential
                            defaultResponseCreateParams
                            (const (pure ()))
                        result `shouldSatisfy` isLeft
        readIORef redirected `shouldReturn` Nothing

geminiSse :: LBS.ByteString
geminiSse =
    "data: {\"responseId\":\"resp-1\",\"modelVersion\":\"gemini-test\",\"candidates\":[{\"index\":0,\"content\":{\"role\":\"model\",\"parts\":[{\"text\":\"hello back\"}]},\"finishReason\":\"STOP\"}],\"usageMetadata\":{\"promptTokenCount\":2,\"candidatesTokenCount\":3,\"totalTokenCount\":5}}\n\n"

codeAssistSse :: LBS.ByteString
codeAssistSse =
    "data: {\"response\":{\"modelVersion\":\"gemini-test\",\"candidates\":[{\"index\":0,\"content\":{\"role\":\"model\",\"parts\":[{\"text\":\"hello back\"}]},\"finishReason\":\"STOP\"}],\"usageMetadata\":{\"promptTokenCount\":2,\"candidatesTokenCount\":3,\"totalTokenCount\":5}},\"traceId\":\"trace-1\"}\n\n"

truncatedGeminiSse :: LBS.ByteString
truncatedGeminiSse =
    "data: {\"candidates\":[{\"index\":0,\"content\":{\"role\":\"model\",\"parts\":[{\"text\":\"partial \"}]}}]}\n\n"

unspecifiedFinishSse :: LBS.ByteString
unspecifiedFinishSse =
    "data: {\"candidates\":[{\"index\":0,\"finishReason\":\"FINISH_REASON_UNSPECIFIED\"}]}\n\n"

unspecifiedPromptBlockSse :: LBS.ByteString
unspecifiedPromptBlockSse =
    "data: {\"promptFeedback\":{\"blockReason\":\"BLOCK_REASON_UNSPECIFIED\"}}\n\n"

unspecifiedAroundTerminalSse :: LBS.ByteString
unspecifiedAroundTerminalSse =
    "data: {\"candidates\":[{\"index\":0,\"finishReason\":\"FINISH_REASON_UNSPECIFIED\"}]}\n\n\
    \data: {\"candidates\":[{\"index\":0,\"finishReason\":\"STOP\"}]}\n\n\
    \data: {\"candidates\":[{\"index\":0,\"finishReason\":\"FINISH_REASON_UNSPECIFIED\"}]}\n\n"

invalidKeyBody :: LBS.ByteString
invalidKeyBody =
    "{\"error\":{\"code\":400,\"message\":\"API key not valid. Please pass a valid API key.\",\"status\":\"INVALID_ARGUMENT\"}}"

geminiCredential :: Credential
geminiCredential =
    Credential "google-key" "gemini" Nothing GeminiProvider

withGeminiServer
    :: LBS.ByteString
    -> (ClientOptions -> IO result)
    -> IO result
withGeminiServer =
    withGeminiHttpResponse
        HTTP.status200
        [("Content-Type", "text/event-stream")]

withGeminiHttpResponse
    :: HTTP.Status
    -> HTTP.ResponseHeaders
    -> LBS.ByteString
    -> (ClientOptions -> IO result)
    -> IO result
withGeminiHttpResponse status headers body action =
    withLoopbackApplication
        (pure \_ respond ->
            respond $ Wai.responseLBS status headers body)
        \port ->
            action defaultClientOptions
                { baseUrl = "http://127.0.0.1:"
                    <> show port <> "/v1beta"
                , defaultModel = "gemini-test"
                , requestTimeoutSeconds = 10
                }

expectedRequestBody :: Aeson.Value
expectedRequestBody = Aeson.object
    [ "contents" Aeson..=
        [ Aeson.object
            [ "role" Aeson..= ("user" :: Text)
            , "parts" Aeson..=
                [Aeson.object ["text" Aeson..= ("hello" :: Text)]]
            ]
        ]
    , "systemInstruction" Aeson..= Aeson.object
        [ "parts" Aeson..=
            [Aeson.object ["text" Aeson..= ("Be concise" :: Text)]]
        ]
    ]

assistantText :: Response -> Maybe Text
assistantText response = case
    [ text
    | MessageItem message <- response.output
    , message.role == RoleAssistant
    , OutputTextPart{text} <- case message.content of
        MessageContentParts parts -> parts
        MessageContentText _ -> []
    ] of
        text : _ -> Just text
        [] -> Nothing
