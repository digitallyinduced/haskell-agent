module Agent.XAI.UsageSpec (spec) where

import Agent.XAI.Usage
import qualified Data.ByteString.Lazy.Char8 as LBS
import Test.Hspec

spec :: Spec
spec =
    describe "decodeGrokUsage" do
        it "decodes and floors the current subscription period" do
            let body = LBS.pack
                    "{\"config\":{\"creditUsagePercent\":31.9,\
                    \\"currentPeriod\":{\"type\":\"USAGE_PERIOD_TYPE_WEEKLY\",\
                    \\"start\":\"2026-08-20T00:00:00Z\",\
                    \\"end\":\"2026-08-21T00:00:00Z\"}}}"
            case decodeGrokUsage body of
                Left err -> expectationFailure (show err)
                Right snapshot -> do
                    snapshot.usedPercent `shouldBe` 31
                    snapshot.periodType `shouldBe` "USAGE_PERIOD_TYPE_WEEKLY"
                    snapshot.windowSeconds `shouldBe` 86400
                    weeklyLimitLeft snapshot `shouldBe` Just 69

        it "clamps percentages to the provider range" do
            let body = LBS.pack
                    "{\"config\":{\"creditUsagePercent\":140,\
                    \\"currentPeriod\":{\"start\":\"2026-08-20T00:00:00Z\",\
                    \\"end\":\"2026-08-21T00:00:00Z\"}}}"
            fmap (.usedPercent) (decodeGrokUsage body) `shouldBe` Right 100

        it "hides parser internals for unreadable responses" do
            decodeGrokUsage "not json"
                `shouldBe` Left "Grok returned an unreadable usage response."

        it "does not label non-weekly periods as weekly" do
            let body = LBS.pack
                    "{\"config\":{\"creditUsagePercent\":20,\
                    \\"currentPeriod\":{\"type\":\"USAGE_PERIOD_TYPE_MONTHLY\",\
                    \\"start\":\"2026-08-01T00:00:00Z\",\
                    \\"end\":\"2026-09-01T00:00:00Z\"}}}"
            fmap weeklyLimitLeft (decodeGrokUsage body)
                `shouldBe` Right Nothing
