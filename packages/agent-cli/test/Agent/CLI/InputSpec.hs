module Agent.CLI.InputSpec (spec) where

import Agent.CLI.Input
    ( ChoiceKey(..)
    , approvalKeyText
    , choiceMoveIndex
    , classifyPastedText
    , decodeBracketedPastePayload
    , displayEditorText
    , formatPasteChip
    , isClipboardPasteCsiBody
    , isClipboardPasteKey
    , parseChoiceKey
    , replHistoryPath
    , terminalTextWidth
    , truncateDisplayText
    , visibleEditorText
    )
import Data.Either (isLeft)
import qualified Data.Text as Text
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

    describe "classifyPastedText" do
        it "detects bracketed-paste CSI wrappers" do
            let payload = "hello from clipboard"
                wrapped = "\ESC[200~" <> payload <> "\ESC[201~"
            classifyPastedText wrapped `shouldBe` (payload, True)
            classifyPastedText payload `shouldBe` (payload, False)

        it "detects printable sentinels from older input versions" do
            let payload = "hello from clipboard"
                wrapped = Text.pack [toEnum 0x27E6] <> payload
                    <> Text.pack [toEnum 0x27E7]
            classifyPastedText wrapped `shouldBe` (payload, True)

        it "treats a 4-line burst as a paste" do
            let burst = Text.unlines ["one", "two", "three", "four"]
            classifyPastedText burst `shouldBe` (burst, True)

    describe "formatPasteChip" do
        it "keeps short pastes inline and chips long ones" do
            formatPasteChip "one line" `shouldBe` "one line"
            formatPasteChip (Text.unlines ["a", "b", "c", "d"])
                `shouldBe` "[Pasted: 4 lines]"

    describe "decodeBracketedPastePayload" do
        it "extracts a payload through the first end marker" do
            decodeBracketedPastePayload 20 "hello\ESC[201~ignored"
                `shouldBe` Right "hello"

        it "rejects incomplete and oversized pastes" do
            decodeBracketedPastePayload 20 "no end marker"
                `shouldSatisfy` isLeft
            decodeBracketedPastePayload 4 "hello\ESC[201~"
                `shouldSatisfy` isLeft

        it "accepts a payload exactly at the configured limit" do
            decodeBracketedPastePayload 5 "hello\ESC[201~"
                `shouldBe` Right "hello"

    describe "safe editor rendering" do
        it "renders pasted terminal controls as visible characters" do
            displayEditorText "\ESC]0;owned\BEL"
                `shouldBe` "␛]0;owned␇"

        it "measures wide and combining Unicode in terminal columns" do
            terminalTextWidth "a界🙂e\x0301" `shouldBe` 6
            visibleEditorText 3 "a界b" 2 `shouldBe` ("界b", 2)

        it "truncates the complete row without exceeding its column budget" do
            let truncated = truncateDisplayText 5 "/always-approve"
            truncated `shouldBe` "/alw…"
            terminalTextWidth truncated `shouldBe` 5

    describe "clipboard image paste key" do
        it "recognizes legacy Ctrl+V without treating ordinary v as paste" do
            isClipboardPasteKey '\SYN' `shouldBe` True
            isClipboardPasteKey 'v' `shouldBe` False

        it "recognizes Kitty keyboard Ctrl+V and Cmd+V press events" do
            isClipboardPasteCsiBody "118;5u" `shouldBe` True
            isClipboardPasteCsiBody "118;9u" `shouldBe` True
            isClipboardPasteCsiBody "118;9:1u" `shouldBe` True
            isClipboardPasteCsiBody "118:86:86;9u" `shouldBe` True

        it "rejects unmodified v, other modified keys, and key releases" do
            isClipboardPasteCsiBody "118u" `shouldBe` False
            isClipboardPasteCsiBody "99;9u" `shouldBe` False
            isClipboardPasteCsiBody "118;9:3u" `shouldBe` False
            isClipboardPasteCsiBody "not-a-key" `shouldBe` False
