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

    it "reads a string field while its JSON value is still streaming" do
        jsonTextFieldPartial "command" "{\"command\":\"git sta"
            `shouldBe` Just "git sta"
        jsonTextFieldPartial "command" "{\"command\": \"git status\"}"
            `shouldBe` Just "git status"

    it "drops an incomplete escape until the remaining JSON arrives" do
        jsonTextFieldPartial "command" "{\"command\":\"printf foo\\"
            `shouldBe` Just "printf foo"
        jsonTextFieldPartial "command" "{\"command\":\"printf foo\\nbar"
            `shouldBe` Just "printf foo\nbar"
        jsonTextFieldPartial "command" "{\"command\":\"printf \\u26"
            `shouldBe` Just "printf "
