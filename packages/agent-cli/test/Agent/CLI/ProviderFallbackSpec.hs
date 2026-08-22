module Agent.CLI.ProviderFallbackSpec (spec) where

import Agent.CLI.Models (ModelOption(..))
import Agent.CLI.ProviderFallback
    ( allowsAutomaticBillingFallback
    , automaticCooldownRetryDelay
    , fallbackCandidates
    , rankedModels
    )
import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Provider (BillingMode(..), Provider(..))
import Data.List (elemIndex)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), addUTCTime)
import Test.Hspec

spec :: Spec
spec = do
    describe "allowsAutomaticBillingFallback" do
        it "blocks subscription-to-API-credit fallback" do
            allowsAutomaticBillingFallback
                SubscriptionBilled ApiBilled
                `shouldBe` False

        it "allows fallback between subscription accounts" do
            allowsAutomaticBillingFallback
                SubscriptionBilled SubscriptionBilled
                `shouldBe` True

        it "does not restrict a session already using API credits" do
            allowsAutomaticBillingFallback
                ApiBilled ApiBilled
                `shouldBe` True

    describe "rankedModels" do
        it "prefers sol over luna" do
            let ids = map (.modelId) rankedModels
            elemIndex "gpt-5.6-sol" ids
                `shouldSatisfy` (< elemIndex "gpt-5.6-luna" ids)

        it "puts the strongest OpenAI model first" do
            fmap (\model -> (model.modelProvider, model.modelId))
                (safeHead rankedModels)
                `shouldBe` Just (OpenAIProvider, "gpt-5.6-sol")

    describe "fallbackCandidates" do
        let exhausted =
                CredentialsExhausted
                    (UTCTime (fromGregorian 2026 8 20) 0)

        it "selects the best model for each other provider" do
            map (\model -> (model.modelProvider, model.modelId))
                (fallbackCandidates [] XAIProvider exhausted)
                `shouldBe`
                    [ (OpenAIProvider, "gpt-5.6-sol")
                    , (OpenRouterProvider, "openai/gpt-5.1")
                    ]

        it "falls back from OpenAI to the best configured alternatives" do
            map (\model -> (model.modelProvider, model.modelId))
                (fallbackCandidates [] OpenAIProvider exhausted)
                `shouldBe`
                    [ (XAIProvider, "grok-4.6")
                    , (OpenRouterProvider, "openai/gpt-5.1")
                    ]

        it "skips providers already found unavailable" do
            map (.modelProvider)
                (fallbackCandidates [OpenAIProvider] XAIProvider exhausted)
                `shouldBe` [OpenRouterProvider]

        it "accepts direct usage-limit errors from every provider" do
            fallbackCandidates [] OpenRouterProvider
                (ProviderError UsageLimitReached "quota exhausted" (Just 3600))
                `shouldSatisfy` (not . null)

        it "accepts other definitive account and billing exhaustion errors" do
            map
                (not . null . fallbackCandidates [] XAIProvider)
                [ ProviderError UsageBalanceExhausted "balance exhausted" Nothing
                , ProviderError QuotaExceeded "quota exhausted" Nothing
                , ProviderError UsageNotIncluded "not included" Nothing
                , ProviderError BillingError "billing unavailable" Nothing
                ]
                `shouldBe` replicate 4 True

        it "does not switch for transient capacity failures" do
            fallbackCandidates [] XAIProvider
                (ProviderError OverloadedError "busy" (Just 30))
                `shouldBe` []

        it "can continue past a replacement provider with rejected auth" do
            map (.modelProvider)
                (fallbackCandidates [XAIProvider] OpenAIProvider
                    (ProviderError AuthenticationError "rejected" Nothing))
                `shouldBe` [OpenRouterProvider]
            map (.modelProvider)
                (fallbackCandidates [XAIProvider] OpenAIProvider
                    (CredentialError "credential file is invalid"))
                `shouldBe` [OpenRouterProvider]

    describe "automaticCooldownRetryDelay" do
        let now = UTCTime (fromGregorian 2026 8 21) 0

        it "waits through a brief credential cooldown" do
            automaticCooldownRetryDelay now
                (CredentialsExhausted (addUTCTime 60 now))
                `shouldBe` Just 60

        it "retries immediately when the reset time has just passed" do
            automaticCooldownRetryDelay now
                (CredentialsExhausted (addUTCTime (-1) now))
                `shouldBe` Just 0

        it "returns control for a long cooldown" do
            automaticCooldownRetryDelay now
                (CredentialsExhausted (addUTCTime 121 now))
                `shouldBe` Nothing

        it "does not retry authentication failures as cooldowns" do
            automaticCooldownRetryDelay now
                (ProviderError AuthenticationError "expired" Nothing)
                `shouldBe` Nothing

safeHead :: [a] -> Maybe a
safeHead = \case
    [] -> Nothing
    value : _ -> Just value
