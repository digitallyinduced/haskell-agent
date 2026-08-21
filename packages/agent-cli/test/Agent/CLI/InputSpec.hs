module Agent.CLI.InputSpec (spec) where

import Agent.CLI.Input
    ( ChoiceKey(..)
    , approvalKeyText
    , choiceMoveIndex
    , classifyPastedText
    , clipboardPastePrefsText
    , dropCycleModeSentinel
    , formatPasteChip
    , isCycleModeSentinel
    , pastePrefsText
    , parseChoiceKey
    , replHistoryPath
    , shiftTabPrefsText
    )
import Control.Exception (bracket)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Console.Haskeline (readPrefs)
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
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

        it "detects the sentinels inserted by haskeline bindings" do
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

    describe "cycle mode sentinel" do
        it "detects and strips the Shift+Tab marker" do
            let marked = "hello" <> Text.singleton '\xFFFC'
            isCycleModeSentinel marked `shouldBe` True
            dropCycleModeSentinel marked `shouldBe` "hello"
            isCycleModeSentinel "hello" `shouldBe` False
            dropCycleModeSentinel "hello" `shouldBe` "hello"

        it "parses Shift+Tab keyseq and bind lines" $ do
            tmp <- getTemporaryDirectory
            bracket
                (openTempFile tmp "haskeline-shift-tab")
                (\(path, _) -> removeFile path)
                \(path, handle) -> do
                    Text.hPutStr handle shiftTabPrefsText
                    hClose handle
                    prefs <- readPrefs path
                    show prefs `shouldSatisfy` ("f24" `Text.isInfixOf`) . Text.pack
                    show prefs `shouldSatisfy` ("\\ESC[Z" `Text.isInfixOf`) . Text.pack

        it "parses bracketed-paste keyseq and bind lines" $ do
            tmp <- getTemporaryDirectory
            bracket
                (openTempFile tmp "haskeline-bracketed-paste")
                (\(path, _) -> removeFile path)
                \(path, handle) -> do
                    Text.hPutStr handle pastePrefsText
                    hClose handle
                    prefs <- readPrefs path
                    show prefs `shouldSatisfy` ("f23" `Text.isInfixOf`) . Text.pack
                    show prefs `shouldSatisfy`
                        ("\\ESC[200~" `Text.isInfixOf`) . Text.pack

        it "parses the Ctrl+V clipboard-image binding" $ do
            tmp <- getTemporaryDirectory
            bracket
                (openTempFile tmp "haskeline-clipboard-paste")
                (\(path, _) -> removeFile path)
                \(path, handle) -> do
                    Text.hPutStr handle clipboardPastePrefsText
                    hClose handle
                    prefs <- readPrefs path
                    show prefs `shouldSatisfy` ("ctrl-v" `Text.isInfixOf`) . Text.pack
