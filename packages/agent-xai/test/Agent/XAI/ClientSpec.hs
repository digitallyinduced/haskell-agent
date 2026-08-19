-- | Functional tests for the xAI client against an in-process HTTP mock.
module Agent.XAI.ClientSpec (spec) where

import Agent.Error (ApiError(..))
import Agent.XAI.Client
import Agent.XAI.Options
import Agent.Provider (Credential(..), Provider(..))
import Agent.OpenAI.Responses.Types
import qualified Data.Aeson as Aeson
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
import Test.Hspec

spec :: Spec
spec = do
    describe "createResponseWith" do
        it "POSTs the mapped request with subscription headers and parses the SSE response" do
            recorded <- newIORef []
            let handler _request = do
                    pure $ sseResponse
                        [ outputItemDone (assistantMessage "hello world")
                        , completedEvent "resp-1" []
                        ]
            withMockGrok recorded handler \options -> do
                result <- createResponseWith options (xaiCredential "token-a") (helloRequest "hi")
                response <- expectRight result
                response.responseId `shouldBe` "resp-1"
                extractAssistantText response `shouldBe` Just "hello world"

            [request] <- readIORef recorded
            request.path `shouldBe` "/v1/responses"
            lookup "Authorization" request.headers `shouldBe` Just "Bearer token-a"
            lookup "X-XAI-Token-Auth" request.headers `shouldBe` Just "xai-grok-cli"
            requestModel request `shouldBe` Just "grok-4.5"
            -- instructions travel as the leading system item
            (inputRoles <$> requestBodyObject request) `shouldBe` Just ["system", "user"]

    describe "retry boundaries" do
        it "reports a terminal stream failure after one request" do
            recorded <- newIORef []
            let handler _request = pure $ sseResponse
                    [ outputItemDone (assistantMessage "partial answer")
                    , sseEvent "response.failed" $ Aeson.object
                        [ "type" Aeson..= ("response.failed" :: Text)
                        , "response" Aeson..= Aeson.object
                            [ "id" Aeson..= ("resp-failed" :: Text)
                            , "created_at" Aeson..= (0 :: Int)
                            , "model" Aeson..= ("grok-4.5" :: Text)
                            , "status" Aeson..= ("failed" :: Text)
                            , "incomplete_details" Aeson..= Aeson.object
                                ["reason" Aeson..= ("overloaded" :: Text)]
                            ]
                        ]
                    ]
            withMockGrok recorded handler \options -> do
                result <- createResponseWith options (xaiCredential "token-a") (helloRequest "hi")
                case result of
                    Left ConnectionError{} -> pure ()
                    other -> expectationFailure ("expected ConnectionError, got " <> show other)

            requests <- readIORef recorded
            length requests `shouldBe` 1

--------------------------------------------------------------------------------
-- Mock server
--------------------------------------------------------------------------------

data RecordedRequest = RecordedRequest
    { path :: !Text
    , headers :: ![(Text, Text)]
    , body :: !LBS.ByteString
    }

withMockGrok
    :: IORef [RecordedRequest]
    -> (RecordedRequest -> IO Wai.Response)
    -> (ClientOptions -> IO a)
    -> IO a
withMockGrok recorded handler action =
    Warp.testWithApplication (pure app) \port ->
        action defaultClientOptions
            { baseUrl = "http://127.0.0.1:" <> show port <> "/v1"
            , requestTimeoutSeconds = 10
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
        , "model" Aeson..= ("grok-4.5" :: Text)
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

--------------------------------------------------------------------------------
-- Providers and credentials
--------------------------------------------------------------------------------

xaiCredential :: Text -> Credential
xaiCredential token = Credential
    { accessToken = token
    , accountId = "xai-" <> token
    , leaseId = Nothing
    , provider = XAIProvider
    }

--------------------------------------------------------------------------------
-- Request/response helpers
--------------------------------------------------------------------------------

helloRequest :: Text -> ResponseCreateParams
helloRequest prompt = defaultResponseCreateParams
    { model = Just "gpt-5.6-terra"
    , instructions = Just "You are a test agent."
    , input = Just (ResponseInputText prompt)
    , tools = Just []
    , reasoning = Just (ReasoningConfig Nothing (Just "low") Nothing Nothing Nothing mempty)
    , include = Just [ResponseInclude "reasoning.encrypted_content"]
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

inputRoles :: Aeson.Object -> [Text]
inputRoles object = case KeyMap.lookup "input" object of
    Just (Aeson.Array items) ->
        [ role
        | Aeson.Object item <- foldr (:) [] items
        , Just (Aeson.String role) <- [KeyMap.lookup "role" item]
        ]
    _ -> []

expectRight :: Show e => Either e a -> IO a
expectRight = \case
    Left err -> expectationFailure ("expected Right, got Left " <> show err) >> fail "unreachable"
    Right value -> pure value
