module Agent.CLI.ProviderAvailabilitySpec (spec) where

import Agent.CLI.Auth (LoadedAuth(..), staticCredentialProvider)
import Agent.CLI.ProviderAvailability
    ( openAiUsageFailure
    , openRouterUsageFailure
    , probeLoadedAvailabilityWith
    , xaiUsageFailure
    )
import Agent.Error (ApiError(..), ErrorType(..))
import qualified Agent.OpenAI.Usage as OpenAI
import qualified Agent.OpenRouter.Usage as OpenRouter
import Agent.Provider
    ( BillingMode(..)
    , Credential(..)
    , Provider(..)
    , getNextToken
    )
import qualified Agent.XAI.Usage as XAI
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), addUTCTime)
import Test.Hspec

spec :: Spec
spec = do
    describe "provider usage availability" do
        let now = UTCTime (fromGregorian 2026 8 22) 0

        it "classifies exhausted Grok usage with its reset delay" do
            xaiUsageFailure now XAI.GrokUsageSnapshot
                { usedPercent = 100
                , windowSeconds = 3600
                , resetsAt = addUTCTime 90 now
                }
                `shouldBe`
                    Just
                        (ProviderError
                            UsageLimitReached
                            "Grok usage is exhausted for the current billing period."
                            (Just 90))

        it "leaves Grok credentials usable below the limit" do
            xaiUsageFailure now XAI.GrokUsageSnapshot
                { usedPercent = 99
                , windowSeconds = 3600
                , resetsAt = addUTCTime 90 now
                }
                `shouldBe` Nothing

        it "classifies a rejected Codex usage window" do
            openAiUsageFailure now OpenAI.UsageSnapshot
                { planType = "plus"
                , rateLimit = Just OpenAI.UsageLimit
                    { allowed = False
                    , limitReached = True
                    , primaryWindow = Just OpenAI.UsageWindow
                        { usedPercent = 100
                        , limitWindowSeconds = 18000
                        , resetAfterSeconds = 75
                        , resetAt = 0
                        }
                    , secondaryWindow = Nothing
                    }
                , additionalRateLimits = []
                }
                `shouldBe`
                    Just
                        (ProviderError
                            UsageLimitReached
                            "Codex usage is exhausted for the current account."
                            (Just 75))

        it "classifies an exhausted OpenRouter key limit" do
            openRouterUsageFailure OpenRouter.OpenRouterUsage
                { keyLabel = Just "coding"
                , keyUsage = Just 10
                , keyLimit = Just 10
                , keyLimitRemaining = Just 0
                , isFreeTier = Just False
                , totalCredits = Just 50
                , totalUsage = Just 10
                }
                `shouldBe`
                    Just
                        (ProviderError
                            BillingError
                            "OpenRouter key usage limit is exhausted."
                            Nothing)

        it "does not treat a zero credit balance as total OpenRouter unavailability" do
            openRouterUsageFailure OpenRouter.OpenRouterUsage
                { keyLabel = Nothing
                , keyUsage = Nothing
                , keyLimit = Nothing
                , keyLimitRemaining = Nothing
                , isFreeTier = Just True
                , totalCredits = Just 0
                , totalUsage = Just 0
                }
                `shouldBe` Nothing

    describe "probeLoadedAvailabilityWith" do
        it "preserves the checked credential for the backend" do
            let credential = Credential
                    { accessToken = "token"
                    , accountId = "account"
                    , leaseId = Nothing
                    , provider = XAIProvider
                    }
                loaded = LoadedAuth
                    { loadedProvider = XAIProvider
                    , loadedTokenProvider =
                        staticCredentialProvider SubscriptionBilled credential
                    , loadedAccountLabel = const (pure "account")
                    , loadedOpenAiPool = Nothing
                    }
            probeLoadedAvailabilityWith (const (pure Nothing)) loaded >>= \case
                Left err ->
                    expectationFailure
                        ("availability probe unexpectedly failed: " <> show err)
                Right usable ->
                    getNextToken usable.loadedTokenProvider Nothing
                        `shouldReturn` Right credential

        it "feeds definitive exhaustion through account failover" do
            let credential = Credential
                    { accessToken = "token"
                    , accountId = "account"
                    , leaseId = Nothing
                    , provider = XAIProvider
                    }
                loaded = LoadedAuth
                    { loadedProvider = XAIProvider
                    , loadedTokenProvider =
                        staticCredentialProvider SubscriptionBilled credential
                    , loadedAccountLabel = const (pure "account")
                    , loadedOpenAiPool = Nothing
                    }
            result <- probeLoadedAvailabilityWith
                (const
                    (pure
                        (Just
                            (ProviderError
                                UsageLimitReached
                                "exhausted"
                                (Just 60)))))
                loaded
            case result of
                Left CredentialsExhausted{} -> pure ()
                Left err ->
                    expectationFailure
                        ("expected credential exhaustion, got " <> show err)
                Right _ ->
                    expectationFailure
                        "expected credential exhaustion, got usable auth"
