module Agent.Responses.GenericClientSpec (spec) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Json (rawJsonBytes, rawJsonFromEncoding)
import qualified Agent.Responses.Codec as Codec
import Agent.Responses.GenericClient
import Agent.Responses.Types
import Control.Retry (constantDelay, limitRetries)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Data.Text (Text)
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
  where
    options = GenericClientOptions
        { baseUrl = "http://localhost:8000/v1"
        , model = "wire-model"
        , bearerToken = Nothing
        , requestTimeoutSeconds = 60
        }

withTools :: [ResponseTool] -> ResponseCreateParams -> ResponseCreateParams
withTools value request = request { tools = Just value }
