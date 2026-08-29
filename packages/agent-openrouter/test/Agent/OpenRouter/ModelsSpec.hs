module Agent.OpenRouter.ModelsSpec (spec) where

import Agent.OpenRouter.Models
import Agent.OpenRouter.Options
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.CaseInsensitive as CI
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Test.Hspec

spec :: Spec
spec = do
    describe "decodeModels" do
        it "decodes model metadata and derives tool support" do
            decodeModels (LBS.pack
                "{\"data\":[{\"id\":\"openai/gpt-5.1\",\
                \\"name\":\"OpenAI: GPT-5.1\",\"context_length\":400000,\
                \\"created\":1720000000,\"supported_parameters\":\
                \[\"tools\",\"temperature\"]}]}")
                `shouldBe`
                    Right
                        [ OpenRouterModel
                            { modelId = "openai/gpt-5.1"
                            , modelDisplayName = "OpenAI: GPT-5.1"
                            , modelContextLength = Just 400000
                            , modelCreated = Just 1720000000
                            , modelSupportsTools = True
                            }
                        ]

        it "falls back to the id when optional metadata is absent" do
            decodeModels (LBS.pack
                "{\"data\":[{\"id\":\"vendor/model\",\
                \\"name\":null,\"context_length\":null,\
                \\"supported_parameters\":null}]}")
                `shouldBe`
                    Right
                        [ OpenRouterModel
                            { modelId = "vendor/model"
                            , modelDisplayName = "vendor/model"
                            , modelContextLength = Nothing
                            , modelCreated = Nothing
                            , modelSupportsTools = False
                            }
                        ]

        it "sorts newest first while preserving ties and undated entries" do
            let modelBody =
                    "{\"data\":[\
                    \{\"id\":\"old\",\"name\":\"Old\",\"created\":10},\
                    \{\"id\":\"new-a\",\"name\":\"New A\",\"created\":30},\
                    \{\"id\":\"tie-a\",\"name\":\"Tie A\",\"created\":20},\
                    \{\"id\":\"tie-b\",\"name\":\"Tie B\",\"created\":20},\
                    \{\"id\":\"undated-a\",\"name\":\"Undated A\"},\
                    \{\"id\":\"undated-b\",\"name\":\"Undated B\"}]}"
            fmap (map (.modelId)) (decodeModels (LBS.pack modelBody))
                `shouldBe`
                    Right
                        [ "new-a"
                        , "tie-a"
                        , "tie-b"
                        , "old"
                        , "undated-a"
                        , "undated-b"
                        ]

        it "returns a stable error for malformed responses" do
            decodeModels "{\"data\":{}}"
                `shouldBe`
                    Left "OpenRouter returned an unreadable models response."

    describe "fetchOpenRouterModelsWith" do
        it "gets /models with the configured headers and decodes the body" do
            requests <- newIORef []
            withMockModels requests
                (Wai.responseLBS HTTP.status200
                    [("Content-Type", "application/json")]
                    (LBS.pack
                        "{\"data\":[{\"id\":\"vendor/model\",\
                        \\"name\":\"Vendor Model\",\"context_length\":8192,\
                        \\"created\":1720000000,\
                        \\"supported_parameters\":[\"tools\"]}]}"))
                \options -> do
                    result <- fetchOpenRouterModelsWith options (Just "secret")
                    result `shouldBe`
                        Right
                            [ OpenRouterModel
                                { modelId = "vendor/model"
                                , modelDisplayName = "Vendor Model"
                                , modelContextLength = Just 8192
                                , modelCreated = Just 1720000000
                                , modelSupportsTools = True
                                }
                            ]

            [request] <- readIORef requests
            request.requestMethod `shouldBe` "GET"
            request.requestPath `shouldBe` "/v1/models"
            requestHeader "Authorization" request
                `shouldBe` Just "Bearer secret"
            requestHeader "Accept" request
                `shouldBe` Just "application/json"
            requestHeader "User-Agent" request
                `shouldBe` Just "haskell-agent"
            requestHeader "HTTP-Referer" request
                `shouldBe` Just "https://example.com"
            requestHeader "X-Title" request
                `shouldBe` Just "haskell-agent-test"

        it "allows public discovery without an authorization header" do
            requests <- newIORef []
            withMockModels requests
                (Wai.responseLBS HTTP.status200
                    [("Content-Type", "application/json")]
                    "{\"data\":[]}")
                \options -> do
                    fetchOpenRouterModelsWith options Nothing
                        `shouldReturn` Right []

            [request] <- readIORef requests
            requestHeader "Authorization" request `shouldBe` Nothing

        it "reports HTTP failures without exposing response internals" do
            requests <- newIORef []
            withMockModels requests
                (Wai.responseLBS HTTP.status503
                    [("Content-Type", "text/plain")]
                    "temporary backend detail")
                \options -> do
                    fetchOpenRouterModelsWith options Nothing
                        `shouldReturn`
                            Left "OpenRouter models returned HTTP 503"

        it "reports malformed successful responses clearly" do
            requests <- newIORef []
            withMockModels requests
                (Wai.responseLBS HTTP.status200
                    [("Content-Type", "application/json")]
                    "{\"not_data\":[]}")
                \options -> do
                    fetchOpenRouterModelsWith options Nothing
                        `shouldReturn`
                            Left
                                "OpenRouter returned an unreadable models response."

data RecordedRequest = RecordedRequest
    { requestMethod :: !Text
    , requestPath :: !Text
    , requestHeaders :: ![(Text, Text)]
    }
    deriving (Eq, Show)

withMockModels
    :: IORef [RecordedRequest]
    -> Wai.Response
    -> (ClientOptions -> IO a)
    -> IO a
withMockModels requests response action =
    Warp.testWithApplication (pure app) \port ->
        action defaultClientOptions
            { baseUrl = "http://127.0.0.1:" <> show port <> "/v1"
            , requestTimeoutSeconds = 10
            , httpReferer = Just "https://example.com"
            , appTitle = Just "haskell-agent-test"
            }
  where
    app request respond = do
        let recorded = RecordedRequest
                { requestMethod = Text.decodeUtf8 (Wai.requestMethod request)
                , requestPath =
                    "/" <> Text.intercalate "/" (Wai.pathInfo request)
                , requestHeaders =
                    [ ( Text.decodeUtf8 (CI.original name)
                      , Text.decodeUtf8 value
                      )
                    | (name, value) <- Wai.requestHeaders request
                    ]
                }
        atomicModifyIORef' requests \values -> (values <> [recorded], ())
        respond response

requestHeader :: Text -> RecordedRequest -> Maybe Text
requestHeader name request =
    lookup name request.requestHeaders
