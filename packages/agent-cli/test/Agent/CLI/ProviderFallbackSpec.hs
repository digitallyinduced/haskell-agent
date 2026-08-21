module Agent.CLI.ProviderFallbackSpec (spec) where

import Agent.CLI.Models (ModelOption(..))
import Agent.CLI.ProviderFallback
    ( fallbackCandidates
    , rankedModels
    )
import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Provider (Provider(..))
import Data.List (elemIndex)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..))
import Test.Hspec

spec :: Spec
spec = do
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

        it "does not switch for transient capacity failures" do
            fallbackCandidates [] XAIProvider
                (ProviderError OverloadedError "busy" (Just 30))
                `shouldBe` []

        it "can continue past a replacement provider with rejected auth" do
            map (.modelProvider)
                (fallbackCandidates [XAIProvider] OpenAIProvider
                    (ProviderError AuthenticationError "rejected" Nothing))
                `shouldBe` [OpenRouterProvider]

safeHead :: [a] -> Maybe a
safeHead = \case
    [] -> Nothing
    value : _ -> Just value
