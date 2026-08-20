module Agent.CLI.CancelWatchSpec (spec) where

import Agent.CLI.CancelWatch (isBareEscape)
import Test.Hspec

spec :: Spec
spec = describe "isBareEscape" do
    it "accepts a lone ESC" do
        isBareEscape "\ESC" `shouldBe` True

    it "rejects CSI / empty / other" do
        isBareEscape "\ESC[" `shouldBe` False
        isBareEscape "" `shouldBe` False
        isBareEscape "a" `shouldBe` False
