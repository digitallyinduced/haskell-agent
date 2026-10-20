{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, NumericUnderscores, BlockArguments #-}
module Agent.TurnPolicySpec (spec) where

import Prelude
import Test.Hspec
import Agent.Error (ApiError (..), ErrorType (..))
import Agent.Turn

spec :: Spec
spec = describe "shared Codex request retry policy" do
    it "retries provider capacity errors and HTTP rate limits" do
        turnErrorIsRetryable
            (ProviderError RateLimitError
                "Codex is at capacity, please try again later (code: turn_rate_limit_exceeded)" Nothing)
            `shouldBe` True
        turnErrorIsRetryable (HttpError 429 "rate limited")
            `shouldBe` True

    it "recognises overload and service outages across provider error formats" do
        map turnErrorIsRetryable
            [ ProviderError OverloadedError "overloaded" Nothing
            , ProviderError (UnknownErrorType "server_error") "internal failure" Nothing
            , ProviderError (UnknownErrorType "service_unavailable_error") "unavailable" Nothing
            , HttpError 503 "unavailable"
            ] `shouldBe` replicate 4 True

    it "leaves connection repair, quota cooldown and permanent rejection to their owners" do
        map turnErrorIsRetryable
            [ ConnectionError "closed"
            , ProviderError UsageLimitReached "quota" Nothing
            , ProviderError UsageBalanceExhausted "balance" Nothing
            , ProviderError WebSocketConnectionLimitReached "socket saturated" Nothing
            , ProviderError AuthenticationError "expired" Nothing
            , ProviderError InvalidRequestError "invalid" Nothing
            ] `shouldBe` replicate 6 False

    it "backs off without overflowing on a large attempt count" do
        let err = ProviderError OverloadedError "busy" Nothing
        map (requestRetryDelayMicros err) [1, 2, 3, 100]
            `shouldBe` [5_000_000, 10_000_000, 20_000_000, 60_000_000]

    it "honours positive retry hints and bounds the wait" do
        let delay hint = requestRetryDelayMicros
                (ProviderError RateLimitError "capacity" (Just hint)) 2
        map delay [2, 120, 0, -1]
            `shouldBe` [2_000_000, 60_000_000, 10_000_000, 10_000_000]
