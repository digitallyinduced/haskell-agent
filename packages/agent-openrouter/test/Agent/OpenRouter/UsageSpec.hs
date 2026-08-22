module Agent.OpenRouter.UsageSpec (spec) where

import Agent.OpenRouter.Usage
import qualified Data.ByteString.Lazy.Char8 as LBS
import Test.Hspec

spec :: Spec
spec = do
    describe "decodeKeyInfo" do
        it "decodes key usage, limit, and free-tier status" do
            decodeKeyInfo (LBS.pack
                "{\"data\":{\"label\":\"agent\",\"usage\":12.5,\"limit\":50,\
                \\"limit_remaining\":37.5,\"is_free_tier\":false}}")
                `shouldBe` Right
                    (Just "agent", Just 12.5, Just 50, Just 37.5, Just False)

        it "hides parser internals for unreadable responses" do
            decodeKeyInfo "not json"
                `shouldBe`
                    Left "OpenRouter returned an unreadable key-usage response."

    describe "decodeCredits" do
        it "decodes total credits and usage" do
            decodeCredits (LBS.pack
                "{\"data\":{\"total_credits\":100,\"total_usage\":25.25}}")
                `shouldBe` Right (Just 100, Just 25.25)

        it "hides parser internals for unreadable responses" do
            decodeCredits "not json"
                `shouldBe`
                    Left "OpenRouter returned an unreadable credits response."
