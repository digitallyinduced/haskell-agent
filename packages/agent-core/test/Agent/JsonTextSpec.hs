module Agent.JsonTextSpec (spec) where

import Agent.JsonText
import Test.Hspec

spec :: Spec
spec = describe "jsonTextField" do
    it "reads a string field" do
        jsonTextField "command" "{\"command\":\"ls\"}" `shouldBe` Just "ls"
        jsonTextFieldDefault "command" "{\"command\":\"ls\"}" `shouldBe` "ls"

    it "returns Nothing / empty for missing or non-string fields" do
        jsonTextField "command" "{\"description\":\"x\"}" `shouldBe` Nothing
        jsonTextFieldDefault "command" "not-json" `shouldBe` ""
        jsonTextField "n" "{\"n\":1}" `shouldBe` Nothing
