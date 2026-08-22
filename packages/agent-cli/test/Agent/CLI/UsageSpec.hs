module Agent.CLI.UsageSpec (spec) where

import Agent.CLI.Usage
import Agent.Error (ApiError(..), ErrorType(..))
import Agent.OpenAI.Usage
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
        , secondaryWindow = Just UsageWindow
            { usedPercent = 72
            , limitWindowSeconds = 604800
            , resetAfterSeconds = 400000
            , resetAt = 1784280000
            }
        }
    , additionalRateLimits = []
    }

epoch :: UTCTime
epoch = UTCTime (fromGregorian 1970 1 1) 0
