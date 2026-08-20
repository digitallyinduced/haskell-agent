module Agent.CLI.StyleSpec (spec) where

import Agent.CLI.Style
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
    describe "style" do
        it "leaves text unchanged when color is off" do
            style False [] "plain" `shouldBe` "plain"

        it "wraps text in SGR codes when color is on" do
            let out = rolePrompt True "λ>"
            out `shouldSatisfy` Text.isInfixOf "λ>"
            out `shouldSatisfy` Text.isInfixOf "\ESC["
            out `shouldSatisfy` (not . Text.isPrefixOf "λ>")

    describe "roles" do
        it "keeps tool labels readable with color off" do
            roleToolName False "read_file" `shouldBe` "read_file"
            roleError False "boom" `shouldBe` "boom"
            roleMuted False "session: 1" `shouldBe` "session: 1"

    describe "cliWindowTitle" do
        it "uses the cwd basename when no session title is set" do
            cliWindowTitle "/tmp/haskell-agent" Nothing
                `shouldBe` "haskell-agent"

        it "prefers a real session title over cwd" do
            cliWindowTitle "/tmp/haskell-agent" (Just "fix the title")
                `shouldBe` "fix the title"

        it "ignores untitled placeholders" do
            cliWindowTitle "/tmp/haskell-agent" (Just "untitled")
                `shouldBe` "haskell-agent"
