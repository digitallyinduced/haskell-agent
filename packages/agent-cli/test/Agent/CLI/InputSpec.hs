module Agent.CLI.InputSpec (spec) where

import Agent.CLI.Input
    ( ChoiceKey(..)
    , approvalKeyText
    , choiceMoveIndex
    , parseChoiceKey
    , replHistoryPath
    )
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

    describe "parseChoiceKey" do
        it "maps arrows, vim keys, enter, cancel, and digits" do
            parseChoiceKey "\ESC[A" `shouldBe` Just ChoiceUp
            parseChoiceKey "\ESC[B" `shouldBe` Just ChoiceDown
            parseChoiceKey "\ESCOA" `shouldBe` Just ChoiceUp
            parseChoiceKey "\ESCOB" `shouldBe` Just ChoiceDown
            parseChoiceKey "k" `shouldBe` Just ChoiceUp
            parseChoiceKey "j" `shouldBe` Just ChoiceDown
            parseChoiceKey "\n" `shouldBe` Just ChoiceEnter
            parseChoiceKey "\r" `shouldBe` Just ChoiceEnter
            parseChoiceKey "\ESC" `shouldBe` Just ChoiceCancel
            parseChoiceKey "q" `shouldBe` Just ChoiceCancel
            parseChoiceKey "3" `shouldBe` Just (ChoiceDigit 3)
            parseChoiceKey "x" `shouldBe` Nothing

    describe "choiceMoveIndex" do
        it "wraps at both ends" do
            choiceMoveIndex 3 0 ChoiceUp `shouldBe` 2
            choiceMoveIndex 3 2 ChoiceDown `shouldBe` 0
            choiceMoveIndex 3 1 ChoiceUp `shouldBe` 0
            choiceMoveIndex 3 1 ChoiceDown `shouldBe` 2
            choiceMoveIndex 3 1 ChoiceEnter `shouldBe` 1
