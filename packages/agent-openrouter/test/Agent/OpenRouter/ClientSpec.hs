-- | Functional tests for the OpenRouter client against an in-process HTTP mock.
module Agent.OpenRouter.ClientSpec (spec) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.OpenRouter.Client
import Agent.OpenRouter.Options
import Agent.Provider (Credential(..), Provider(..))
import Agent.Responses.Types
import Control.Concurrent.MVar
import Control.Monad (when)
import Control.Retry (constantDelay, limitRetries)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Builder as Builder
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = do
    describe "createResponseWith" do
        it "POSTs the mapped request with OpenRouter headers and parses the SSE response" do
            recorded <- newIORef []
            let handler _request = do
                    pure $ sseResponse
                        [ outputItemDone (assistantMessage "hello world")
                        , completedEvent "resp-1" []
                        ]
            withMockOpenRouter recorded handler \options -> do
                result <- createResponseWith options
                    (openRouterCredential "token-a")
                    (helloRequest "hi")
                response <- expectRight result
                response.responseId `shouldBe` "resp-1"
                extractAssistantText response `shouldBe` Just "hello world"

            [request] <- readIORef recorded
            request.path `shouldBe` "/v1/responses"
            lookup "Authorization" request.headers `shouldBe` Just "Bearer token-a"
            lookup "HTTP-Referer" request.headers `shouldBe` Just "https://example.com"
            lookup "X-Title" request.headers `shouldBe` Just "haskell-agent-test"
            requestModel request `shouldBe` Just "openai/gpt-5.1"
            (instructionsOf <$> requestBodyObject request)
                `shouldBe` Just (Just "You are a test agent.")

        it "streams callbacks before the response completes" do
            recorded <- newIORef []
            callbackSeen <- newEmptyMVar
            serverSawCallback <- newIORef False
            let handler _ = pure $
                    streamingResponse callbackSeen serverSawCallback
            withMockOpenRouter recorded handler \options -> do
                result <- createResponseWithEvents options
                    (openRouterCredential "token-a")
                    (helloRequest "hi")
                    (recordOutputItem callbackSeen)
                response <- expectRight result
                response.responseId `shouldBe` "resp-stream"
            readIORef serverSawCallback `shouldReturn` True

        it "rejects a non-OpenRouter credential before contacting the host" do
            recorded <- newIORef []
            let handler _request = pure $ sseResponse []
            withMockOpenRouter recorded handler \options -> do
                result <- createResponseWith options
                    (Credential
                        { accessToken = "openai-token"
                        , accountId = "acc"
                        , leaseId = Nothing
                        , provider = OpenAIProvider
                        })
                    (helloRequest "hi")
                case result of
                    Left (ProviderError ApiErrorType message _) ->
                        message `shouldSatisfy` Text.isInfixOf "OpenRouter"
                    other -> expectationFailure
                        ("expected OpenRouter credential error, got " <> show other)
            readIORef recorded `shouldReturn` []

    describe "retry boundaries" do
        it "retries a transient HTTP failure before streaming starts" do
            recorded <- newIORef []
            attempts <- newIORef (0 :: Int)
            let handler _request = do
                    attempt <- atomicModifyIORef' attempts \current ->
                        let next = current + 1
                        in (next, next)
                    pure $ if attempt == 1
                        then Wai.responseLBS HTTP.status503
                            [("Content-Type", "text/plain")]
                            "temporarily unavailable"
                        else sseResponse
                            [ outputItemDone (assistantMessage "after retry")
                            , completedEvent "resp-retry" []
                            ]
            withMockOpenRouter recorded handler \options -> do
                result <- createResponseWithEventsPolicy
                    (constantDelay 0 <> limitRetries 3)
                    options
                    (openRouterCredential "token-a")
                    (helloRequest "hi")
                    (const (pure ()))
                response <- expectRight result
                response.responseId `shouldBe` "resp-retry"
            length <$> readIORef recorded `shouldReturn` 2

        it "reports a terminal stream failure after one request" do
            recorded <- newIORef []
            let handler _request = pure $ sseResponse
                    [ outputItemDone (assistantMessage "partial answer")
                    , sseEvent "response.failed" $ Aeson.object
                        [ "type" Aeson..= ("response.failed" :: Text)
                        , "response" Aeson..= Aeson.object
                            [ "id" Aeson..= ("resp-failed" :: Text)
                            , "created_at" Aeson..= (0 :: Int)
                            , "model" Aeson..= ("openai/gpt-5.1" :: Text)
                            , "status" Aeson..= ("failed" :: Text)
                            , "incomplete_details" Aeson..= Aeson.object
                                ["reason" Aeson..= ("overloaded" :: Text)]
                            ]
                        ]
                    ]
            withMockOpenRouter recorded handler \options -> do
                result <- createResponseWith options
                    (openRouterCredential "token-a")
                    (helloRequest "hi")
                case result of
                    Left ConnectionError{} -> pure ()
                    other -> expectationFailure ("expected ConnectionError, got " <> show other)

            requests <- readIORef recorded
            length requests `shouldBe` 1

        it "does not retry a transient failure after a callback" do
            attempts <- newIORef (0 :: Int)
            callbacks <- newIORef (0 :: Int)
            let overload =
                    ProviderError OverloadedError "temporarily overloaded" Nothing
                request emit = do
                    modifyIORef' attempts (+ 1)
                    emit ()
                    pure (Left overload :: Either ApiError Text)
            result <- retryTransientOpenRouterResultWithPolicy
                (constantDelay 0 <> limitRetries 3)
                request
                (const (modifyIORef' callbacks (+ 1)))
            result `shouldBe` Left overload
            readIORef attempts `shouldReturn` 1
            readIORef callbacks `shouldReturn` 1

        it "does not retry when the stream callback throws" do
            checkThrowingCallback

        it "does not retry quota errors before streaming starts" do
            attempts <- newIORef (0 :: Int)
            let quota = ProviderError UsageLimitReached "quota" (Just 3600)
                request _emit = do
                    modifyIORef' attempts (+ 1)
                    pure (Left quota :: Either ApiError Text)
            result <- retryTransientOpenRouterResultWithPolicy
                (constantDelay 0 <> limitRetries 3)
                request
                (const (pure ()))
            result `shouldBe` Left quota
            readIORef attempts `shouldReturn` 1

        it "does not retry an unknown-code permanent client error" do
            checkPermanentClientError

        it "classifies an OpenRouter 401 envelope as authentication rejected" do
            recorded <- newIORef []
            let handler _request = pure $ Wai.responseLBS HTTP.status401
                    [("Content-Type", "application/json")]
                    "{\"error\":{\"code\":\"unauthorized\",\"message\":\"bad key\"}}"
            withMockOpenRouter recorded handler \options -> do
                result <- createResponseWith options
                    (openRouterCredential "bad")
                    (helloRequest "hi")
                case result of
                    Left (ProviderError AuthenticationError message _) ->
                        message `shouldSatisfy` Text.isInfixOf "bad key"
                    other -> expectationFailure
                        ("expected AuthenticationError, got " <> show other)

--------------------------------------------------------------------------------
-- Mock server
--------------------------------------------------------------------------------

data RecordedRequest = RecordedRequest
    { path :: !Text
    , headers :: ![(Text, Text)]
    , body :: !LBS.ByteString
    } deriving (Eq, Show)

withMockOpenRouter
    :: IORef [RecordedRequest]
    -> (RecordedRequest -> IO Wai.Response)
    -> (ClientOptions -> IO a)
    -> IO a
withMockOpenRouter recorded handler action =
    Warp.testWithApplication (pure app) \port ->
        action defaultClientOptions
            { baseUrl = "http://127.0.0.1:" <> show port <> "/v1"
            , requestTimeoutSeconds = 10
            , httpReferer = Just "https://example.com"
            , appTitle = Just "haskell-agent-test"
            }
  where
    app waiRequest respond = do
        requestBody <- Wai.strictRequestBody waiRequest
        let request = RecordedRequest
                { path = "/" <> Text.intercalate "/" (Wai.pathInfo waiRequest)
                , headers =
                    [ (Text.decodeUtf8 (CI.original name), Text.decodeUtf8 value)
                    | (name, value) <- Wai.requestHeaders waiRequest
                    ]
                , body = requestBody
                }
        atomicModifyIORef' recorded \requests -> (requests <> [request], ())
        respond =<< handler request

sseResponse :: [Text] -> Wai.Response
sseResponse events = Wai.responseLBS HTTP.status200
    [("Content-Type", "text/event-stream")]
    (LBS.fromStrict (Text.encodeUtf8 (Text.concat events)))

outputItemDone :: Aeson.Value -> Text
outputItemDone item = sseEvent "response.output_item.done" $ Aeson.object
    [ "type" Aeson..= ("response.output_item.done" :: Text)
    , "output_index" Aeson..= (0 :: Int)
    , "item" Aeson..= item
    ]

completedEvent :: Text -> [Aeson.Value] -> Text
completedEvent responseId output = sseEvent "response.completed" $ Aeson.object
    [ "type" Aeson..= ("response.completed" :: Text)
    , "response" Aeson..= Aeson.object
        [ "id" Aeson..= responseId
        , "created_at" Aeson..= (0 :: Int)
        , "model" Aeson..= ("openai/gpt-5.1" :: Text)
        , "status" Aeson..= ("completed" :: Text)
        , "output" Aeson..= output
        , "usage" Aeson..= Aeson.object
            [ "input_tokens" Aeson..= (10 :: Int)
            , "output_tokens" Aeson..= (5 :: Int)
            , "total_tokens" Aeson..= (15 :: Int)
            ]
        ]
    ]

sseEvent :: Text -> Aeson.Value -> Text
sseEvent eventType payload =
    "event: " <> eventType <> "\ndata: "
        <> Text.decodeUtf8 (LBS.toStrict (Aeson.encode payload))
        <> "\n\n"

assistantMessage :: Text -> Aeson.Value
assistantMessage text = Aeson.object
    [ "type" Aeson..= ("message" :: Text)
    , "role" Aeson..= ("assistant" :: Text)
    , "content" Aeson..= [Aeson.object
        [ "type" Aeson..= ("output_text" :: Text)
        , "text" Aeson..= text
        ]]
    ]

openRouterCredential :: Text -> Credential
openRouterCredential token = Credential
    { accessToken = token
    , accountId = ""
    , leaseId = Nothing
    , provider = OpenRouterProvider
    }

helloRequest :: Text -> ResponseCreateParams
helloRequest prompt = defaultResponseCreateParams
    { model = Just "openai/gpt-5.1"
    , instructions = Just "You are a test agent."
    , input = Just (ResponseInputText prompt)
    , tools = Just []
    }

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

requestBodyObject :: RecordedRequest -> Maybe Aeson.Object
requestBodyObject request = case Aeson.decode request.body of
    Just (Aeson.Object object) -> Just object
    _ -> Nothing

requestModel :: RecordedRequest -> Maybe Text
requestModel request = do
    object <- requestBodyObject request
    case KeyMap.lookup "model" object of
        Just (Aeson.String model) -> Just model
        _ -> Nothing

instructionsOf :: Aeson.Object -> Maybe Text
instructionsOf object = case KeyMap.lookup "instructions" object of
    Just (Aeson.String text) -> Just text
    _ -> Nothing

expectRight :: Show e => Either e a -> IO a
expectRight = \case
    Left err -> expectationFailure ("expected Right, got Left " <> show err) >> fail "unreachable"
    Right value -> pure value

checkPermanentClientError :: IO ()
checkPermanentClientError = do
    recorded <- newIORef []
    withMockOpenRouter recorded permanentClientErrorResponse \options -> do
        result <- createResponseWithEventsPolicy
            (constantDelay 0 <> limitRetries 3)
            options
            (openRouterCredential "token-a")
            (helloRequest "hi")
            (const (pure ()))
        case result of
            Left (ProviderError InvalidRequestError _ _) -> pure ()
            other -> expectationFailure
                ("expected InvalidRequestError, got " <> show other)
    length <$> readIORef recorded `shouldReturn` 1

checkThrowingCallback :: IO ()
checkThrowingCallback = do
    recorded <- newIORef []
    callbacks <- newIORef (0 :: Int)
    withMockOpenRouter recorded throwingCallbackResponse \options -> do
        result <- createResponseWithEventsPolicy
            (constantDelay 0 <> limitRetries 3)
            options
            (openRouterCredential "token-a")
            (helloRequest "hi")
            (throwingCallback callbacks)
        case result of
            Left ConnectionError{} -> pure ()
            other -> expectationFailure
                ("expected callback ConnectionError, got " <> show other)
    length <$> readIORef recorded `shouldReturn` 1
    readIORef callbacks `shouldReturn` 1

permanentClientErrorResponse _request = pure $
    Wai.responseLBS (HTTP.mkStatus 422 "Unprocessable Entity")
        [("Content-Type", "application/json")]
        "{\"error\":{\"code\":\"vendor_validation_failed\",\"message\":\"bad request\"}}"

throwingCallbackResponse _request = pure $ sseResponse
    [ outputItemDone (assistantMessage "hello")
    , completedEvent "resp-callback" []
    ]

throwingCallback callbacks _event =
    modifyIORef' callbacks (+ 1)
        >> ioError (userError "callback failed")

recordOutputItem :: MVar () -> ResponseStreamEvent -> IO ()
recordOutputItem callbackSeen event =
    when (responseStreamEventType event == EventOutputItemDone) do
        tryPutMVar callbackSeen ()
        pure ()

streamingResponse :: MVar () -> IORef Bool -> Wai.Response
streamingResponse callbackSeen serverSawCallback =
    Wai.responseStream HTTP.status200
        [("Content-Type", "text/event-stream")]
        \write flush -> do
            writeSse write (outputItemDone (assistantMessage "hello"))
            flush
            seen <- timeout 2_000_000 (readMVar callbackSeen)
            writeIORef serverSawCallback (maybe False (const True) seen)
            writeSse write (completedEvent "resp-stream" [])
            flush

writeSse :: (Builder.Builder -> IO ()) -> Text -> IO ()
writeSse write = write . Builder.byteString . Text.encodeUtf8
