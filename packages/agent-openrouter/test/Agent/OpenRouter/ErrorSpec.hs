module Agent.OpenRouter.ErrorSpec (spec) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.OpenRouter.Error
import Agent.OpenRouter.Stream
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = describe "classifyFailure" do
    it "types a bare 429 and honours the Retry-After header" do
        classifyFailure 429 (Just 90) "too many requests"
            `shouldBe` ProviderError RateLimitError "too many requests" (Just 90)

    it "keeps a typed OpenAI envelope and only fills a missing retry interval" do
        let envelope = "{\"error\":{\"type\":\"usage_limit_reached\",\"message\":\"limited\",\"resets_in_seconds\":300}}"
        classifyFailure 429 (Just 90) envelope
            `shouldBe` ProviderError UsageLimitReached "limited" (Just 300)
        let bare = "{\"error\":{\"type\":\"rate_limit_error\",\"message\":\"limited\"}}"
        classifyFailure 429 (Just 90) bare
            `shouldBe` ProviderError RateLimitError "limited" (Just 90)

    it "decodes OpenRouter's code/message envelope" do
        let body = "{\"error\":{\"code\":\"invalid_prompt\",\"message\":\"Missing required parameter: 'model'.\"},\"metadata\":null}"
        classifyFailure 400 Nothing body
            `shouldBe` ProviderError InvalidRequestError
                "Missing required parameter: 'model'."
                Nothing

    it "maps 401 and 402 from status when the body is unstructured" do
        case classifyFailure 401 Nothing "nope" of
            ProviderError AuthenticationError _ _ -> pure ()
            other -> expectationFailure ("expected AuthenticationError, got " <> show other)
        case classifyFailure 402 Nothing "pay up" of
            ProviderError BillingError _ _ -> pure ()
            other -> expectationFailure ("expected BillingError, got " <> show other)

    it "maps OpenRouter billing codes" do
        let body = "{\"error\":{\"code\":\"insufficient_credits\",\"message\":\"out of credits\"}}"
        classifyFailure 402 Nothing body
            `shouldBe` ProviderError BillingError "out of credits" Nothing

    it "surfaces typed stream errors" do
        events <- expectRight $ parseSseEvents $ sseBlock "error"
            "{\"type\":\"error\",\"error\":{\"type\":\"rate_limit_error\",\"message\":\"limited\",\"resets_in_seconds\":120}}"
        buildResponse events
            `shouldBe` Left (ProviderError RateLimitError "limited" (Just 120))

    it "leaves other statuses as plain HTTP errors" do
        classifyFailure 503 Nothing "unavailable"
            `shouldBe` HttpError 503 "unavailable"

sseBlock :: Text -> Text -> Text
sseBlock eventType dataText =
    "event: " <> eventType <> "\ndata: " <> dataText <> "\n\n"

expectRight :: Show e => Either e a -> IO a
expectRight = \case
    Left err -> expectationFailure ("expected Right, got Left " <> show err) >> fail "unreachable"
    Right value -> pure value
