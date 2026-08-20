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
            let out = rolePrompt True "agent>"
            out `shouldSatisfy` Text.isInfixOf "agent>"
            out `shouldSatisfy` Text.isInfixOf "\ESC["
            out `shouldSatisfy` (not . Text.isPrefixOf "agent>")

    describe "roles" do
        it "keeps tool labels readable with color off" do
            roleToolName False "read_file" `shouldBe` "read_file"
            roleError False "boom" `shouldBe` "boom"
            roleMuted False "session: 1" `shouldBe` "session: 1"
