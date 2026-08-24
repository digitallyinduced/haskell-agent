module Agent.CLI.StyleSpec (spec) where

import Agent.CLI.Style
import System.OsPath (unsafeEncodeUtf)
import qualified Data.Text as Text
import Test.Hspec

fromFilePath = unsafeEncodeUtf

spec :: Spec
spec = do
    describe "style" do
        it "leaves text unchanged when color is off" do
            style False [] "plain" `shouldBe` "plain"

        it "wraps text in SGR codes when color is on" do
            let out = rolePrompt True "λ"
            out `shouldSatisfy` Text.isInfixOf "λ"
            out `shouldSatisfy` Text.isInfixOf "\ESC["
            out `shouldSatisfy` (not . Text.isPrefixOf "λ")

        it "uses the terminal default background after nested styling" do
            let out = styleBase True agentBackground [] "hi"
            out `shouldSatisfy` (not . Text.isInfixOf "48;")

    describe "paintBackgroundLines" do
        it "leaves text unchanged when color is off" do
            paintBackgroundLines False agentBackground "a\nb" `shouldBe` "a\nb"

        it "leaves lines on the terminal theme's default background" do
            paintBackgroundLines True agentBackground "a\nb\n"
                `shouldBe` "a\nb\n"

        it "does not erase with the terminal default background" do
            paintBackgroundLines True agentBackground "text"
                `shouldSatisfy` (not . Text.isInfixOf "\ESC[K")

    describe "roles" do
        it "keeps tool labels readable with color off" do
            roleToolName False "Read" `shouldBe` "Read"
            roleToolPath False "src/A.hs" `shouldBe` "src/A.hs"
            roleToolCommand False "git status" `shouldBe` "git status"
            roleError False "boom" `shouldBe` "boom"
            roleMuted False "session: 1" `shouldBe` "session: 1"

        it "uses the terminal cyan slot without forcing a background" do
            let out = rolePrompt True "λ"
            out `shouldSatisfy` Text.isInfixOf "\ESC[1;36m"
            out `shouldSatisfy` (not . Text.isInfixOf "48;")

    describe "chrome glyphs" do
        it "exposes the shared Unicode markers" do
            glyphTool `shouldSatisfy` (`elem` ["◆ ", "* "])
            glyphToolAccent `shouldSatisfy` (`elem` ["❙ ", "| "])
            glyphOk `shouldSatisfy` (`elem` ["✓ ", "+ "])
            glyphErr `shouldSatisfy` (`elem` ["✗ ", "x "])
            glyphWarn `shouldSatisfy` (`elem` ["⚠ ", "! "])
            glyphCancel `shouldSatisfy` (`elem` ["⊘ ", "o "])
            glyphSession `shouldSatisfy` (`elem` ["⧉ ", "# "])
            glyphThink `shouldSatisfy` (`elem` ["◆ ", "* "])
            glyphToolOut `shouldSatisfy` (`elem` ["┊ ", "| "])
            spinnerFrames `shouldSatisfy` (not . null)

    describe "cliWindowTitle" do
        it "uses a stable placeholder when no session title is set" do
            cliWindowTitle (fromFilePath "/tmp/haskell-agent") Nothing
                `shouldBe` "New session"

        it "prefers a real session title over cwd" do
            cliWindowTitle (fromFilePath "/tmp/haskell-agent") (Just "fix the title")
                `shouldBe` "fix the title"

        it "ignores untitled placeholders" do
            cliWindowTitle (fromFilePath "/tmp/haskell-agent") (Just "untitled")
                `shouldBe` "New session"
