module Agent.Gemini.ErrorSpec (spec) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Gemini.Error (classifyFailure)
import Test.Hspec

spec :: Spec
spec = describe "Gemini error classification" do
    it "recognizes invalid API keys even when Google returns HTTP 400" do
        classifyFailure
            400
            Nothing
            "{\"error\":{\"code\":400,\"message\":\"API key not valid. Please pass a valid API key.\",\"status\":\"INVALID_ARGUMENT\"}}"
            `shouldBe`
                ProviderError
                    AuthenticationError
                    "API key not valid. Please pass a valid API key."
                    Nothing

    it "preserves retry timing for resource exhaustion" do
        classifyFailure
            429
            (Just 17)
            "{\"error\":{\"code\":429,\"message\":\"quota exhausted\",\"status\":\"RESOURCE_EXHAUSTED\"}}"
            `shouldBe` ProviderError RateLimitError "quota exhausted" (Just 17)
