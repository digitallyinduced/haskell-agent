module Agent.OpenAI.UsageSpec (spec) where

import Agent.OpenAI.Usage
import qualified Data.ByteString.Lazy.Char8 as LBS
import Test.Hspec

spec :: Spec
spec = describe "decodeUsageResponse" do
    it "decodes primary, secondary, and additional usage windows" do
        let payload = LBS.pack $ concat
                [ "{\"plan_type\":\"plus\",\"rate_limit\":{"
                , "\"allowed\":true,\"limit_reached\":false,"
                , "\"primary_window\":{\"used_percent\":31,\"limit_window_seconds\":18000,"
                , "\"reset_after_seconds\":9000,\"reset_at\":1783880000},"
                , "\"secondary_window\":{\"used_percent\":72,\"limit_window_seconds\":604800,"
                , "\"reset_after_seconds\":400000,\"reset_at\":1784280000}},"
                , "\"additional_rate_limits\":[{\"limit_name\":\"Code Review\","
                , "\"metered_feature\":\"codex_review\",\"rate_limit\":null}]}"
                ]
        case decodeUsageResponse payload of
            Left err -> expectationFailure (show err)
            Right usage -> do
                usage.planType `shouldBe` "plus"
                fmap (.usedPercent) (usage.rateLimit >>= (.primaryWindow)) `shouldBe` Just 31
                fmap (.usedPercent) (usage.rateLimit >>= (.secondaryWindow)) `shouldBe` Just 72
                map (.meteredFeature) usage.additionalRateLimits `shouldBe` ["codex_review"]

    it "accepts null and absent optional limit data" do
        decodeUsageResponse (LBS.pack "{\"plan_type\":\"free\",\"rate_limit\":null}")
            `shouldBe` Right UsageSnapshot
                { planType = "free"
                , rateLimit = Nothing
                , additionalRateLimits = []
                }
