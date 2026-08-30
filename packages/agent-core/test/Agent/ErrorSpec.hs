module Agent.ErrorSpec (spec) where

import Agent.Error
import qualified Agent.Json.Decode as Json
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
        Json.decodeText errorTypeDecoder "\"future_error\""
            `shouldBe` Right unknown

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

  describe "credentialExhaustionReasonFromApiError" do
    it "retains structured rate-limit metadata without provider bodies" do
        credentialExhaustionReasonFromApiError
            (ProviderError UsageLimitReached
                "sensitive provider detail"
                (Just 90))
            `shouldBe` Just ExhaustedByRateLimit
                { exhaustionErrorType = Just UsageLimitReached
                , exhaustionStatusCode = Nothing
                , exhaustionRetryAfter = Just 90
                }

    it "retains only the status for HTTP authentication failures" do
        credentialExhaustionReasonFromApiError
            (HttpError 401 "sensitive response body")
            `shouldBe` Just ExhaustedByAuthentication
                { exhaustionErrorType = Nothing
                , exhaustionStatusCode = Just 401
                }

    it "does not mislabel permission or local credential failures as authentication" do
        credentialExhaustionReasonFromApiError
            (HttpError 403 "sensitive response body")
            `shouldBe` Nothing
        credentialExhaustionReasonFromApiError
            (CredentialError "sensitive local path")
            `shouldBe` Nothing
