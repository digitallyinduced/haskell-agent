module Agent.Responses.GenericClientSpec (spec) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Json (rawJsonBytes, rawJsonFromEncoding)
import qualified Agent.Responses.Codec as Codec
import Agent.Responses.GenericClient
import Agent.Responses.Request (setResponseModel)
import Agent.Responses.Types
import Control.Retry (constantDelay, limitRetries)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Simple (setRequestHeader)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Test.Hspec

spec :: Spec
spec = do
    describe "buildRequest" do
        it "uses the configured wire model and disables remote state" do
            let request = defaultResponseCreateParams
                    { model = Just "friendly-alias"
                    , store = Just True
                    , stream = Just False
                    , previousResponseId = Just "resp_previous"
                    }
                projected = buildRequest options request
            projected.model `shouldBe` Just "wire-model"
            projected.store `shouldBe` Just False
            projected.stream `shouldBe` Just True
            projected.previousResponseId `shouldBe` Nothing

        it "preserves standard tools" do
            let tool = FunctionToolValue FunctionTool
                    { name = "read_file"
                    , description = Nothing
                    , parameters = Nothing
                    , strict = Nothing

                    }
                request = case defaultResponseCreateParams of
                    ResponseCreateParams{..} -> ResponseCreateParams
                        { tools = Just [tool]

                        , ..
                        }
                projected = buildRequest options request
            projected.tools `shouldBe` Just [tool]

        it "preserves custom grammar and namespace tool fields" do
            let grammar = rawJsonFromEncoding $ Aeson.pairs $
                    "type" Aeson..= ("grammar" :: Text)
                        <> "syntax" Aeson..= ("root: /.+/" :: Text)
                custom = CustomToolValue CustomTool
                    { name = "shell"
                    , description = Just "Run a command"
                    , format = Just grammar
                    }
                namespace = NamespaceToolValue NamespaceTool
                    { name = "tools"
                    , description = Just "Available tools"
                    , tools = [custom]
                    }
                request = withTools [namespace] defaultResponseCreateParams
                encoded = Codec.encodeResponseCreateParams request
            case Codec.decodeResponseCreateParams (LBS.toStrict encoded) of
                Right ResponseCreateParams
                    { tools =
                        Just
                            [ NamespaceToolValue NamespaceTool
                                { name = "tools"
                                , description = Just "Available tools"
                                , tools =
                                    [ CustomToolValue CustomTool
                                        { name = "shell"
                                        , description = Just "Run a command"
                                        , format = Just decodedGrammar
                                        }
                                    ]
                                }
                            ]
                    } ->
                        rawJsonBytes decodedGrammar
                            `shouldSatisfy`
                                BS.isInfixOf "\"syntax\":\"root: /.+/\""
                other -> expectationFailure
                    ("unexpected tool round trip: " <> show other)

    describe "classifyFailure" do
        it "decodes OpenAI error envelopes and preserves Retry-After" do
            classifyFailure
                429
                (Just 12)
                "{\"error\":{\"type\":\"rate_limit_error\",\"message\":\"slow\"}}"
                `shouldBe`
                    ProviderError RateLimitError "slow" (Just 12)

    describe "retryTransientResultWithPolicy" do
        it "retries transient failures before any event is emitted" do
            attempts <- newIORef (0 :: Int)
            result <- retryTransientResultWithPolicy
                (constantDelay 0 <> limitRetries 2)
                (\_ -> do
                    attempt <- atomicModifyIORef' attempts (\n -> (n + 1, n))
                    pure $
                        if attempt == 0
                            then Left (ConnectionError "temporary")
                            else Right ("ok" :: String))
                (const (pure ()))
            result `shouldBe` Right "ok"
            readIORef attempts `shouldReturn` 2

        it "does not retry after a stream event has been delivered" do
            attempts <- newIORef (0 :: Int)
            delivered <- newIORef ([] :: [Int])
            result <- retryTransientResultWithPolicy
                (constantDelay 0 <> limitRetries 2)
                (\emit -> do
                    modifyIORef' attempts (+ 1)
                    emit (1 :: Int)
                    pure
                        (Left (ConnectionError "after output")
                            :: Either ApiError String))
                (\event -> modifyIORef' delivered (<> [event]))
            result `shouldBe` Left (ConnectionError "after output")
            readIORef attempts `shouldReturn` 1
            readIORef delivered `shouldReturn` [1]

    describe "createResponseWithProviderPolicy" do
        it "applies provider hooks through the shared transport and retry path" do
            attempts <- newIORef (0 :: Int)
            recordedModels <- newIORef []
            recordedHeaders <- newIORef []
            Warp.testWithApplication
                (pure (providerHookApp attempts recordedModels recordedHeaders))
                \port -> do
                    let config = providerHookConfig port
                    result <- createResponseWithProviderPolicy
                        (constantDelay 0 <> limitRetries 2)
                        config
                        defaultResponseCreateParams
                        Nothing
                    response <- expectRight result
                    response.responseId `shouldBe` "resp-provider-hook"
            readIORef attempts `shouldReturn` 2
            readIORef recordedModels
                `shouldReturn` [Just "provider-model", Just "provider-model"]
            readIORef recordedHeaders `shouldReturn` [True, True]
  where
    options = GenericClientOptions
        { baseUrl = "http://localhost:8000/v1"
        , model = "wire-model"
        , bearerToken = Nothing
        , requestTimeoutSeconds = 60
        }

providerHookConfig :: Warp.Port -> ProviderClientConfig
providerHookConfig port = ProviderClientConfig
    { providerExceptionPrefix = "Provider hook request failed"
    , providerBaseUrl = "http://127.0.0.1:" <> show port <> "/v1"
    , providerRequestTimeoutSeconds = 10
    , providerBuildRequest = setResponseModel "provider-model"
    , providerConfigureRequest =
        setRequestHeader "X-Provider-Hook" ["enabled"]
    , providerClassifyFailure = \_status _retryAfter body ->
        ConnectionError ("provider hook: " <> body)
    , providerAssemblyConfig = streamAssemblyConfig
    , providerRetryableFailure = \case
        ConnectionError message ->
            "provider hook:" `Text.isPrefixOf` message
        _ -> False
    }

providerHookApp attempts recordedModels recordedHeaders request respond = do
    body <- Wai.strictRequestBody request
    let decoded = Codec.decodeResponseCreateParams (LBS.toStrict body)
        requestModel = either (const Nothing) (.model) decoded
        hasProviderHeader =
            lookup "X-Provider-Hook" (Wai.requestHeaders request)
                == Just "enabled"
    modifyIORef' recordedModels (<> [requestModel])
    modifyIORef' recordedHeaders (<> [hasProviderHeader])
    attempt <- atomicModifyIORef' attempts \current ->
        let next = current + 1
        in (next, next)
    respond $
        if attempt == 1
            then Wai.responseLBS HTTP.status418
                [("Content-Type", "text/plain")]
                "try again"
            else providerHookResponse

providerHookResponse :: Wai.Response
providerHookResponse = Wai.responseLBS HTTP.status200
    [("Content-Type", "text/event-stream")]
    ("event: response.completed\ndata: " <> Aeson.encode payload <> "\n\n")
  where
    payload = Aeson.object
        [ "type" Aeson..= ("response.completed" :: Text)
        , "response" Aeson..= Aeson.object
            [ "id" Aeson..= ("resp-provider-hook" :: Text)
            , "created_at" Aeson..= (0 :: Int)
            , "model" Aeson..= ("provider-model" :: Text)
            , "status" Aeson..= ("completed" :: Text)
            , "output" Aeson..= ([] :: [Aeson.Value])
            ]
        ]

expectRight :: Show error => Either error value -> IO value
expectRight = \case
    Left err ->
        expectationFailure ("expected Right, got Left " <> show err)
            >> fail "unreachable"
    Right value -> pure value

withTools :: [ResponseTool] -> ResponseCreateParams -> ResponseCreateParams
withTools value request = request { tools = Just value }
