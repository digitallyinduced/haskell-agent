module Agent.CLI.InterruptSpec (spec) where

import Agent.CLI.Interrupt
import Test.Hspec

spec :: Spec
spec = describe "decideCtrlC" do
    it "warns on first idle Ctrl-C" do
        decideCtrlC Idle False `shouldBe` WarnExit

    it "exits on second idle Ctrl-C within the window" do
        decideCtrlC Idle True `shouldBe` ForceExit

    it "soft-cancels the first Ctrl-C during a turn" do
        decideCtrlC (TurnActive False) False `shouldBe` SoftCancel
        decideCtrlC (TurnActive False) True `shouldBe` SoftCancel

    it "force-exits when Ctrl-C arrives after the turn is already cancelled" do
        decideCtrlC (TurnActive True) False `shouldBe` ForceExit
