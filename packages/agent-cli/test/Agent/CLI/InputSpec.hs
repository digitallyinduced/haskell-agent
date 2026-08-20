module Agent.CLI.InputSpec (spec) where

import Agent.CLI.Input (replHistoryPath)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = do
    describe "replHistoryPath" do
        it "is ~/.haskell-agent/history" do
            replHistoryPath "/home/marc"
                `shouldBe` "/home/marc" </> ".haskell-agent" </> "history"
