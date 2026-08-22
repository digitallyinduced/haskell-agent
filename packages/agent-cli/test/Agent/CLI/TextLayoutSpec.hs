module Agent.CLI.TextLayoutSpec (spec) where

import Agent.CLI.TextLayout
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
    describe "clampSelectionIndex" do
        it "clamps selection to the available values" do
            clampSelectionIndex 3 (-1) `shouldBe` 0
            clampSelectionIndex 3 1 `shouldBe` 1
            clampSelectionIndex 3 4 `shouldBe` 2
            clampSelectionIndex 0 4 `shouldBe` 0

    describe "selectionWindow" do
        it "keeps selections at the start, middle, and end visible" do
            let values = ["a", "b", "c", "d"] :: [String]
            selectionWindow 2 0 values
                `shouldBe` [(0, "a"), (1, "b")]
            selectionWindow 2 2 values
                `shouldBe` [(2, "c"), (3, "d")]
            selectionWindow 2 3 values
                `shouldBe` [(2, "c"), (3, "d")]

        it "handles oversized and empty windows" do
            selectionWindow 5 1 (["a", "b"] :: [String])
                `shouldBe` [(0, "a"), (1, "b")]
            selectionWindow 2 0 ([] :: [String])
                `shouldBe` []

    describe "transcriptPreviewRows" do
        it "wraps logical lines and returns the requested tail" do
            transcriptPreviewRows 3 2 ["abcd", "ef"]
                `shouldBe` ["d", "ef"]

        it "renders an empty transcript placeholder" do
            transcriptPreviewRows 20 2 []
                `shouldBe` ["(empty transcript)"]

        it "preserves blank lines and clamps non-positive widths" do
            transcriptPreviewRows 0 3 ["", "ab"]
                `shouldBe` ["", "a", "b"]

        it "returns no rows when none are requested" do
            transcriptPreviewRows 20 0 ["text"]
                `shouldBe` []

    describe "fitTextCell" do
        it "pads, sanitizes, and truncates cell text" do
            fitTextCell 4 "ab" `shouldBe` "ab  "
            fitTextCell 4 "a\nb" `shouldBe` "a b "
            fitTextCell 4 "abcde" `shouldBe` "abc…"
            fitTextCell 1 "ab" `shouldBe` "…"
            fitTextCell 0 "ab" `shouldBe` ""

    describe "renderSplitPaneFrame" do
        it "keeps the selected item visible and previews its transcript" do
            let frame = renderSplitPaneFrame SplitPaneFrame
                    { splitPaneMinColumns = 20
                    , splitPaneColumns = 20
                    , splitPaneBodyRows = 2
                    , splitPaneLeftMinWidth = 6
                    , splitPaneLeftMaxWidth = 6
                    , splitPaneDivider = " │ "
                    , splitPaneTitle = "pick"
                    , splitPaneHeaderDetail =
                        \count -> Text.pack (show count) <> " items"
                    , splitPaneLeftHeading = "items"
                    , splitPaneRightHeading =
                        maybe "preview" ("preview " <>)
                    , splitPaneItems = ["one", "two", "three"]
                    , splitPaneSelectedIndex = 2
                    , splitPaneLeftLabel = \_ item -> item
                    , splitPaneTranscript = \item -> [item <> " transcript"]
                    , splitPaneEmptyTranscript = "(none)"
                    , splitPaneFooter = "keys"
                    , splitPanePromptStyle = id
                    , splitPaneMutedStyle = id
                    , splitPaneSelectedStyle = ("selected:" <>)
                    }
            frame `shouldSatisfy` Text.isInfixOf "  two "
            frame `shouldSatisfy` Text.isInfixOf "selected:› thr…"
            frame `shouldSatisfy` Text.isInfixOf "three trans"
            frame `shouldSatisfy` Text.isInfixOf "cript"
            length (Text.lines frame) `shouldBe` 5
