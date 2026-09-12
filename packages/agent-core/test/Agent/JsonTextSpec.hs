module Agent.JsonTextSpec (spec) where

import Agent.JsonText
import Control.Monad (forM_)
import qualified Data.Text as Text
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

    it "preserves Unicode while scanning an unfinished string" do
        jsonTextFieldPartial "command" "{\"command\":\"café 日本語 😀"
            `shouldBe` Just "café 日本語 😀"

    it "stops at the first unescaped closing quote despite an unfinished suffix" do
        jsonTextFieldPartial "command" "{\"command\":\"café 😀\",\"other\":"
            `shouldBe` Just "café 😀"

    it "distinguishes odd and even backslash runs before a quote" do
        forM_ [0 .. 8] \count -> do
            let slashes = Text.replicate count "\\"
                arguments = "{\"command\":\"a" <> slashes <> "\"tail"
                expected = "a" <> Text.replicate (count `div` 2) "\\"
                    <> if odd count then "\"tail" else ""
            jsonTextFieldPartial "command" arguments `shouldBe` Just expected

    it "omits only the unfinished trailing backslash" do
        forM_ [0 .. 8] \count ->
            jsonTextFieldPartial "command"
                ("{\"command\":\"a" <> Text.replicate count "\\")
                `shouldBe` Just ("a" <> Text.replicate (count `div` 2) "\\")

    it "decodes complete JSON escapes in an unfinished value" do
        jsonTextFieldPartial "command"
            "{\"command\":\"\\\"\\\\\\/\\b\\f\\n\\r\\t\\u263a"
            `shouldBe` Just "\"\\/\b\f\n\r\t☺"

    it "waits for all four hexadecimal digits of a Unicode escape" do
        forM_ ["", "2", "26", "263"] \digits ->
            jsonTextFieldPartial "command" ("{\"command\":\"a\\u" <> digits)
                `shouldBe` Just "a"

    it "rejects malformed escapes and unescaped control characters" do
        forM_ ["\\q", "\\uZZ", "\n", "\t"] \body ->
            jsonTextFieldPartial "command" ("{\"command\":\"" <> body)
                `shouldBe` Nothing

    it "returns an empty preview as soon as the opening value quote arrives" do
        jsonTextFieldPartial "command" "{\"command\":\"" `shouldBe` Just ""
        jsonTextFieldPartial "command" "{\"command\":" `shouldBe` Nothing
