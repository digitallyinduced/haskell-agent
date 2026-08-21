module Agent.ErrorSpec (spec) where

import Agent.Error
import qualified Data.Aeson as Aeson
import Test.Hspec

spec :: Spec
spec = do
  describe "ErrorType" do
    it "maps wire discriminators directly" do
        errorTypeFromText "usage_limit_reached" `shouldBe` UsageLimitReached
        errorTypeFromText "invalid_image" `shouldBe` InvalidImageError
        errorTypeFromText "insufficient_quota" `shouldBe` QuotaExceeded
        errorTypeText UsageBalanceExhausted `shouldBe` "usage_balance_exhausted"

    it "preserves unknown discriminators" do
        let unknown = UnknownErrorType "future_error"
        errorTypeFromText "future_error" `shouldBe` unknown
        Aeson.fromJSON (Aeson.toJSON unknown) `shouldBe` Aeson.Success unknown

  describe "retryDisposition" do
    it "separates transient retries from usage-reset retries" do
        retryDisposition
            (ProviderError UsageBalanceExhausted "balance exhausted" Nothing)
            `shouldBe` RetryAfterLimitReset
        retryDisposition
            (ProviderError OverloadedError "busy" (Just 30))
            `shouldBe` RetryTransiently
        retryDisposition
            (ProviderError InvalidRequestError "bad request" Nothing)
            `shouldBe` DoNotRetry
