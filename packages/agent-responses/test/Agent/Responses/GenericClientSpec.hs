module Agent.Responses.GenericClientSpec (spec) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Responses.GenericClient
import Agent.Responses.Types
import Control.Retry (constantDelay, limitRetries)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef
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
