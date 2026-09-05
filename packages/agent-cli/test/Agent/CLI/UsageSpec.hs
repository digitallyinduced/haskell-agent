module Agent.CLI.UsageSpec (spec) where

import Agent.CLI.Usage
import Agent.Error (ApiError(..), ErrorType(..))
import Agent.OpenAI.Usage
import qualified Agent.OpenRouter.Usage as OpenRouter
import Agent.TUI.Model (PromptLimitStatus(..))
import qualified Agent.XAI.Usage as XAI
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), addUTCTime)
import Test.Hspec

spec :: Spec
spec = do
    describe "formatUsageWindow" do
        it "shows remaining reserve and that it lasts until reset" do
            let rendered = formatUsageWindow False sampleWindow
            rendered `shouldSatisfy` ("69% reserve" `Text.isInfixOf`)
            rendered `shouldSatisfy` ("lasts until reset in 2h 30m" `Text.isInfixOf`)

        it "marks a spent window as exhausted until reset" do
            let rendered = formatUsageWindow False sampleWindow { usedPercent = 100 }
            rendered `shouldSatisfy` ("0% reserve" `Text.isInfixOf`)
            rendered `shouldSatisfy` ("exhausted until reset in 2h 30m" `Text.isInfixOf`)

    describe "formatAccountUsage" do
        it "includes plan, windows, and local pacing" do
            let line = AccountUsageLine
                    { usageAccountId = "acc-1234567890"
                    , usageCooldownUntil = Just (addUTCTime 90 epoch)
                    , usageResult = Right sampleSnapshot
                    }
                rendered = formatAccountUsage False epoch line
            rendered `shouldSatisfy` ("account acc-1234" `Text.isInfixOf`)
            rendered `shouldSatisfy` ("pacing until" `Text.isInfixOf`)
            rendered `shouldSatisfy` ("plan plus" `Text.isInfixOf`)
            rendered `shouldSatisfy` ("5h  69% reserve" `Text.isInfixOf`)
            rendered `shouldSatisfy` ("7d  28% reserve" `Text.isInfixOf`)
            rendered `shouldSatisfy` ("lasts until reset" `Text.isInfixOf`)

        it "uses friendly provider errors when usage cannot be loaded" do
            let line = AccountUsageLine
                    { usageAccountId = "acc-123"
                    , usageCooldownUntil = Nothing
                    , usageResult =
                        Left
                            (ProviderError AuthenticationError
                                "expired token"
                                Nothing)
                    }
                rendered = formatAccountUsage False epoch line
            rendered `shouldSatisfy`
                Text.isInfixOf "Authentication failed"
            rendered `shouldSatisfy` Text.isInfixOf "/login"
            rendered `shouldNotSatisfy`
                Text.isInfixOf "ProviderError"

    describe "formatUsageReport" do
        it "explains an empty pool" do
            formatUsageReport False epoch []
                `shouldBe` "usage: no ChatGPT accounts in the current pool"

    describe "formatUsageSummary" do
        it "compacts plan, windows, and pacing for account picker rows" do
            let rendered =
                    formatUsageSummary
                        epoch
                        (Just (addUTCTime 90 epoch))
                        (Right sampleSnapshot)
            rendered `shouldSatisfy` ("pacing 1m" `Text.isInfixOf`)
            rendered `shouldSatisfy` ("plus" `Text.isInfixOf`)
            rendered `shouldSatisfy` ("5h 69% left" `Text.isInfixOf`)
            rendered `shouldSatisfy` ("7d 28% left" `Text.isInfixOf`)

        it "keeps accounts visible when usage cannot be loaded" do
            formatUsageSummary
                epoch
                Nothing
                (Left (ConnectionError "offline"))
                `shouldBe` "usage unavailable"

    describe "formatModelUsageSummary" do
        it "compacts gateway model windows without account metadata" do
            formatModelUsageSummary sampleSnapshot
                `shouldBe` Just "5h 69% left · 7d 28% left"

        it "marks a reached gateway model limit" do
            let snapshot = UsageSnapshot
                    { planType = sampleSnapshot.planType
                    , rateLimit =
                        fmap
                            (\limit -> limit { limitReached = True })
                            sampleSnapshot.rateLimit
                    , additionalRateLimits = sampleSnapshot.additionalRateLimits
                    }
            formatModelUsageSummary snapshot
                `shouldBe`
                    Just "limit reached · 5h 69% left · 7d 28% left"

    describe "composer limit status" do
        it "uses OpenAI's weekly window ahead of its 5-hour window" do
            formatOpenAiLimitStatus sampleSnapshot
                `shouldBe`
                    Just PromptLimitStatus
                        { promptLimitText = "Weekly limit left: 28%"
                        , promptLimitWarning = False
                        }

        it "falls back to OpenAI's 5-hour window" do
            let snapshot = UsageSnapshot
                    { planType = sampleSnapshot.planType
                    , rateLimit =
                        Just UsageLimit
                            { allowed = True
                            , limitReached = False
                            , primaryWindow = Just sampleWindow
                            , secondaryWindow = Nothing
                            }
                    , additionalRateLimits = []
                    }
            formatOpenAiLimitStatus snapshot
                `shouldBe`
                    Just PromptLimitStatus
                        { promptLimitText = "5h limit left: 69%"
                        , promptLimitWarning = False
                        }

        it "recognizes a weekly window returned in the primary slot" do
            let snapshot = UsageSnapshot
                    { planType = sampleSnapshot.planType
                    , rateLimit =
                        Just UsageLimit
                            { allowed = True
                            , limitReached = False
                            , primaryWindow = Just weeklyWindow
                            , secondaryWindow = Nothing
                            }
                    , additionalRateLimits = []
                    }
            formatOpenAiLimitStatus snapshot
                `shouldBe`
                    Just PromptLimitStatus
                        { promptLimitText = "Weekly limit left: 28%"
                        , promptLimitWarning = False
                        }
            formatUsageSummary epoch Nothing (Right snapshot)
                `shouldBe` "plus · 7d 28% left"

        it "formats Grok's weekly reserve" do
            formatGrokLimitStatus XAI.GrokUsageSnapshot
                { XAI.usedPercent = 96
                , XAI.periodType = "USAGE_PERIOD_TYPE_WEEKLY"
                , XAI.windowSeconds = 604800
                , XAI.resetsAt = epoch
                }
                `shouldBe`
                    Just PromptLimitStatus
                        { promptLimitText = "Weekly limit left: 4%"
                        , promptLimitWarning = True
                        }

        it "formats an OpenRouter key limit as a percentage" do
            formatOpenRouterLimitStatus sampleOpenRouterUsage
                `shouldBe`
                    Just PromptLimitStatus
                        { promptLimitText = "Key limit left: 25%"
                        , promptLimitWarning = False
                        }

        it "falls back to the OpenRouter credit balance" do
            formatOpenRouterLimitStatus sampleOpenRouterUsage
                { OpenRouter.keyLimit = Nothing
                , OpenRouter.keyLimitRemaining = Nothing
                }
                `shouldBe`
                    Just PromptLimitStatus
                        { promptLimitText = "Credits left: $37.50"
                        , promptLimitWarning = False
                        }

sampleWindow :: UsageWindow
sampleWindow = UsageWindow
    { usedPercent = 31
    , limitWindowSeconds = 18000
    , resetAfterSeconds = 9000
    , resetAt = 1783880000
    }

sampleSnapshot :: UsageSnapshot
sampleSnapshot = UsageSnapshot
    { planType = "plus"
    , rateLimit = Just UsageLimit
        { allowed = True
        , limitReached = False
        , primaryWindow = Just sampleWindow
        , secondaryWindow = Just weeklyWindow
        }
    , additionalRateLimits = []
    }

weeklyWindow :: UsageWindow
weeklyWindow = UsageWindow
    { usedPercent = 72
    , limitWindowSeconds = 604800
    , resetAfterSeconds = 400000
    , resetAt = 1784280000
    }

sampleOpenRouterUsage :: OpenRouter.OpenRouterUsage
sampleOpenRouterUsage = OpenRouter.OpenRouterUsage
    { OpenRouter.keyLabel = Just "coding"
    , OpenRouter.keyUsage = Just 75
    , OpenRouter.keyLimit = Just 100
    , OpenRouter.keyLimitRemaining = Just 25
    , OpenRouter.isFreeTier = Just False
    , OpenRouter.totalCredits = Just 50
    , OpenRouter.totalUsage = Just 12.5
    }

epoch :: UTCTime
epoch = UTCTime (fromGregorian 1970 1 1) 0
