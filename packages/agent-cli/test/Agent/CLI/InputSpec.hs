module Agent.CLI.InputSpec (spec) where

import Agent.CLI.Input (approvalKeyText, replHistoryPath)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = do
    describe "replHistoryPath" do
        it "is ~/.haskell-agent/history" do
            replHistoryPath "/home/marc"
                `shouldBe` "/home/marc" </> ".haskell-agent" </> "history"

    describe "approvalKeyText" do
        it "keeps a printable key as a one-character answer" do
            approvalKeyText 'y' `shouldBe` "y"
            approvalKeyText 'A' `shouldBe` "A"
            approvalKeyText 'n' `shouldBe` "n"

        it "maps Enter / Return to the empty default deny" do
            approvalKeyText '\n' `shouldBe` ""
            approvalKeyText '\r' `shouldBe` ""
