module Agent.OpenAI.ModelsClientSpec (spec) where

import Agent.OpenAI.Models.Client
import Agent.OpenAI.Models.Cache (ModelsCacheKey(..))
import Agent.OpenAI.Models.Types (ModelInfo(..), ModelsResponse(..))
import Agent.OpenAI.TestSupport (withLoopbackApplication)
import Agent.Provider
    ( BillingMode(..)
    , Credential(..)
    , Provider(..)
    , tokenProvider
    )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import Test.Hspec

spec :: Spec
spec = do
    describe "listModelsWithCredentialAt" do
        it "requests /models with client_version, auth, account, and parses ETag" do
            recorded <- newIORef []
            withModelsServer recorded fetchedResponse \baseUrl -> do
                result <- listModelsWithCredentialAt
                    baseUrl
                    "1.2.3"
                    Nothing
                    chatGptCredential
                case result of
                    Right ModelsFetched{catalog, etag} -> do
                        map (.slug) catalog.models `shouldBe` ["remote-model"]
                        etag `shouldBe` Just "\"models-v1\""
                    other ->
                        expectationFailure
                            ("expected fetched model catalog, got " <> show other)

            [request] <- readIORef recorded
            request.path `shouldBe` "/v1/models"
            request.query `shouldBe` [("client_version", Just "1.2.3")]
            lookup "Authorization" request.headers
                `shouldBe` Just "Bearer test-token"
            lookup "ChatGPT-Account-ID" request.headers
                `shouldBe` Just "account-123"
            lookup "User-Agent" request.headers
                `shouldBe` Just "haskell-agent/1.2.3"
            lookup "Originator" request.headers
                `shouldBe` Just "haskell-agent"

        it "sends If-None-Match and accepts HTTP 304" do
            recorded <- newIORef []
            withModelsServer recorded notModifiedResponse \baseUrl -> do
                result <- listModelsWithCredentialAt
                    baseUrl
                    "1.2.3"
                    (Just "\"models-v1\"")
                    chatGptCredential
                result `shouldBe`
                    Right ModelsNotModified
                        { etag = Just "\"models-v1\""
                        , cacheKey =
                            modelsCacheKeyForCredential
                                baseUrl
                                chatGptCredential
                        }

            [request] <- readIORef recorded
            lookup "If-None-Match" request.headers
                `shouldBe` Just "\"models-v1\""

        it "preserves provider query parameters when adding client_version" do
            recorded <- newIORef []
            withModelsServer recorded fetchedResponse \baseUrl -> do
                result <- listModelsWithCredentialAt
                    (baseUrl <> "?existing=value")
                    "1.2.3"
                    Nothing
                    chatGptCredential
                result `shouldSatisfy` \case
                    Right ModelsFetched{} -> True
                    _ -> False

            [request] <- readIORef recorded
            request.query `shouldBe`
                [ ("existing", Just "value")
                , ("client_version", Just "1.2.3")
                ]

        it "scopes conditional ETags to the credential account" do
            recorded <- newIORef []
            withModelsServer recorded fetchedResponse \baseUrl -> do
                let provider = tokenProvider SubscriptionBilled \_ ->
                        pure (Right chatGptCredential)
                    client = modelsEndpointClient
                        ModelsClientConfig
                            { baseUrl
                            , clientVersion = "1.2.3"
                            }
                        provider
                    wrongAccount = ModelsCacheKey
                        { providerId = "openai"
                        , baseUrl
                        , accountId = Just "another-account"
                        }
                result <- client.fetchModels $ Just ModelsFetchCondition
                    { etag = "\"wrong-account-etag\""
                    , cacheKey = wrongAccount
                    }
                result `shouldSatisfy` \case
                    Right ModelsFetched
                        { cacheKey = actualKey
                        } ->
                            actualKey
                                == modelsCacheKeyForCredential
                                    baseUrl
                                    chatGptCredential
                    _ -> False

            [request] <- readIORef recorded
            lookup "If-None-Match" request.headers `shouldBe` Nothing

        it "only enables dynamic Codex models for subscription credentials" do
            let config = ModelsClientConfig
                    { baseUrl = defaultModelsBaseUrl
                    , clientVersion = "1.2.3"
                    }
                provider billing = tokenProvider billing \_ ->
                    pure (Right chatGptCredential)
                subscription =
                    modelsEndpointClient config (provider SubscriptionBilled)
                apiKey =
                    modelsEndpointClient config (provider ApiBilled)
            subscription.allowsRemoteRefresh `shouldBe` True
            subscription.usesChatGptAuth `shouldBe` True
            apiKey.allowsRemoteRefresh `shouldBe` False
            apiKey.usesChatGptAuth `shouldBe` False

data RecordedRequest = RecordedRequest
    { path :: !Text
    , query :: ![(Text, Maybe Text)]
    , headers :: ![(Text, Text)]
    }

withModelsServer
    :: IORef [RecordedRequest]
    -> Wai.Response
    -> (Text -> IO value)
    -> IO value
withModelsServer recorded response action =
    withLoopbackApplication (pure app) \port ->
        action ("http://127.0.0.1:" <> Text.pack (show port) <> "/v1")
  where
    app request respond = do
        let captured = RecordedRequest
                { path = "/" <> Text.intercalate "/" (Wai.pathInfo request)
                , query =
                    [ ( Text.decodeUtf8 key
                      , Text.decodeUtf8 <$> maybeValue
                      )
                    | (key, maybeValue) <- Wai.queryString request
                    ]
                , headers =
                    [ ( Text.decodeUtf8 (CI.original name)
                      , Text.decodeUtf8 value
                      )
                    | (name, value) <- Wai.requestHeaders request
                    ]
                }
        atomicModifyIORef' recorded \requests ->
            (requests <> [captured], ())
        respond response

fetchedResponse :: Wai.Response
fetchedResponse =
    Wai.responseLBS HTTP.status200
        [ ("Content-Type", "application/json")
        , ("ETag", "\"models-v1\"")
        ]
        (Aeson.encode (Aeson.object
            [ "models" Aeson..=
                [ Aeson.object
                    [ "slug" Aeson..= ("remote-model" :: Text)
                    , "display_name" Aeson..= ("Remote Model" :: Text)
                    , "visibility" Aeson..= ("list" :: Text)
                    , "base_instructions" Aeson..= ("Remote prompt" :: Text)
                    ]
                ]
            ]))

notModifiedResponse :: Wai.Response
notModifiedResponse =
    Wai.responseLBS HTTP.status304 [] LBS.empty

chatGptCredential :: Credential
chatGptCredential = Credential
    { accessToken = "test-token"
    , accountId = "account-123"
    , leaseId = Nothing
    , provider = OpenAIProvider
    }
