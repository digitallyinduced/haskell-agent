module Agent.CLI.ProgressSpec (spec) where

import Agent.CLI.Progress
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
    describe "osc9ProgressSequence" do
        it "emits ConEmu/Ghostty remove and indeterminate states" do
            osc9ProgressRemove `shouldBe` "\ESC]9;4;0\BEL"
            osc9ProgressIndeterminate `shouldBe` "\ESC]9;4;3\BEL"

        it "includes a clamped percent for determinate progress" do
            osc9ProgressSequence 1 (Just 42) `shouldBe` "\ESC]9;4;1;42\BEL"
            osc9ProgressSequence 1 (Just 150) `shouldBe` "\ESC]9;4;1;100\BEL"
            osc9ProgressSequence 1 (Just (-3)) `shouldBe` "\ESC]9;4;1;0\BEL"

    describe "wrapOscForTmux" do
        it "leaves the sequence alone outside tmux" do
            wrapOscForTmux False osc9ProgressIndeterminate
                `shouldBe` osc9ProgressIndeterminate

        it "doubles ESC so tmux forwards the inner OSC" do
            wrapOscForTmux True osc9ProgressRemove
                `shouldBe` "\ESCPtmux;\ESC\ESC]9;4;0\BEL\ESC\\"
            wrapOscForTmux True osc9ProgressIndeterminate
                `shouldSatisfy` Text.isPrefixOf "\ESCPtmux;"
            wrapOscForTmux True osc9ProgressIndeterminate
                `shouldSatisfy` Text.isSuffixOf "\ESC\\"
