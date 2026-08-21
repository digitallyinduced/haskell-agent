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
                    \\"currentPeriod\":{\"start\":\"2026-08-20T00:00:00Z\",\
                    \\"end\":\"2026-08-21T00:00:00Z\"}}}"
            case decodeGrokUsage body of
                Left err -> expectationFailure (show err)
                Right snapshot -> do
                    snapshot.usedPercent `shouldBe` 31
                    snapshot.windowSeconds `shouldBe` 86400

        it "clamps percentages to the provider range" do
            let body = LBS.pack
                    "{\"config\":{\"creditUsagePercent\":140,\
                    \\"currentPeriod\":{\"start\":\"2026-08-20T00:00:00Z\",\
                    \\"end\":\"2026-08-21T00:00:00Z\"}}}"
            fmap (.usedPercent) (decodeGrokUsage body) `shouldBe` Right 100
