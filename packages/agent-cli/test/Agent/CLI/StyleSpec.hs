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
            let out = rolePrompt True "λ"
            out `shouldSatisfy` Text.isInfixOf "λ"
            out `shouldSatisfy` Text.isInfixOf "\ESC["
            out `shouldSatisfy` (not . Text.isPrefixOf "λ")

        it "restores a base wash after nested styling" do
            let out = styleBase True agentBackground [] "hi"
            -- Reset then reopen Solarized base03 (combined into one SGR sequence).
            out `shouldSatisfy` Text.isInfixOf "\ESC[0;48;2;0;43;54m"

    describe "paintBackgroundLines" do
        it "leaves text unchanged when color is off" do
            paintBackgroundLines False agentBackground "a\nb" `shouldBe` "a\nb"

        it "paints each line and preserves a trailing newline" do
            let out = paintBackgroundLines True agentBackground "a\nb\n"
            Text.count "\ESC[48;2;0;43;54m" out `shouldBe` 2
            out `shouldSatisfy` Text.isSuffixOf "\n"
            out `shouldSatisfy` Text.isInfixOf "\ESC[0K"

    describe "roles" do
        it "keeps tool labels readable with color off" do
            roleToolName False "Read" `shouldBe` "Read"
            roleToolPath False "src/A.hs" `shouldBe` "src/A.hs"
            roleToolCommand False "git status" `shouldBe` "git status"
            roleError False "boom" `shouldBe` "boom"
            roleMuted False "session: 1" `shouldBe` "session: 1"

        it "keeps the user wash under the prompt glyph" do
            let out = rolePrompt True "λ"
            -- Background may share an SGR sequence with bold/cyan attrs (truecolor).
            out `shouldSatisfy` Text.isInfixOf "48;2;7;54;66"
            out `shouldSatisfy` Text.isInfixOf "\ESC[0;48;2;7;54;66m"

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
        it "uses the cwd basename when no session title is set" do
            cliWindowTitle "/tmp/haskell-agent" Nothing
                `shouldBe` "haskell-agent"

        it "prefers a real session title over cwd" do
            cliWindowTitle "/tmp/haskell-agent" (Just "fix the title")
                `shouldBe` "fix the title"

        it "ignores untitled placeholders" do
            cliWindowTitle "/tmp/haskell-agent" (Just "untitled")
                `shouldBe` "haskell-agent"
