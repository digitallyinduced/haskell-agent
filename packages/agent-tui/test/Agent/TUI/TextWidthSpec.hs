module Agent.TUI.TextWidthSpec (spec) where

import Agent.TUI.TextWidth
import Test.Hspec

spec :: Spec
spec = describe "terminal character width" do
    it "classifies ASCII, CJK, emoji, combining, and ambiguous characters" do
        charCellWidth 'a' `shouldBe` 1
        charCellWidth '界' `shouldBe` 2
        charCellWidth '🙂' `shouldBe` 2
        charCellWidth '\x0301' `shouldBe` 0
        charCellWidth '·' `shouldBe` 1

    it "keeps raw controls zero-width but display placeholders one cell wide" do
        charCellWidth '\BEL' `shouldBe` 0
        charCellWidth '\x200d' `shouldBe` 1
        displayCharCellWidth '\BEL' `shouldBe` 1
        displayCharCellWidth '\t' `shouldBe` 1
        displayCharCellWidth '\x200d' `shouldBe` 1
        displayTerminalText "\ESC]0;owned\BEL\t\r"
            `shouldBe` "␛]0;owned␇⇥↵"

    it "exposes the shared wide-character classification" do
        isWideCharacter '界' `shouldBe` True
        isWideCharacter '🙂' `shouldBe` True
        isWideCharacter 'a' `shouldBe` False
        isWideCharacter '\x0301' `shouldBe` False
        isWideCharacter '·' `shouldBe` False
