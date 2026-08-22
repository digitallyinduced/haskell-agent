module Agent.Http.HeaderSpec (spec) where

import Agent.Http.Header (parseRetryAfterSeconds)
import Test.Hspec

spec :: Spec
spec = describe "parseRetryAfterSeconds" do
    it "parses the first integer header value" do
        parseRetryAfterSeconds ["42"] `shouldBe` Just 42
        parseRetryAfterSeconds ["7", "9"] `shouldBe` Just 7

    it "clamps non-positive delays to one second" do
        parseRetryAfterSeconds ["0"] `shouldBe` Just 1
        parseRetryAfterSeconds ["-4"] `shouldBe` Just 1

    it "rejects absent, malformed, and HTTP-date values" do
        parseRetryAfterSeconds [] `shouldBe` Nothing
        parseRetryAfterSeconds ["42s"] `shouldBe` Nothing
        parseRetryAfterSeconds ["Sat, 22 Aug 2026 16:43:00 GMT"] `shouldBe` Nothing
