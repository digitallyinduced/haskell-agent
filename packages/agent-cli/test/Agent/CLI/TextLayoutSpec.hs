module Agent.CLI.TextLayoutSpec (spec) where

import Agent.CLI.Input (terminalTextWidth)
import Agent.CLI.TextLayout
import qualified Data.Text as Text
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
    ( Arbitrary(..)
    , Gen
    , Property
    , chooseInt
    , conjoin
    , counterexample
    , elements
    , vectorOf
    , (===)
    )

data TextCellCase = TextCellCase !Int !Text.Text
    deriving (Show)

data SplitPaneCase = SplitPaneCase
    { paneColumns :: !Int
    , paneBodyRows :: !Int
    , paneTitle :: !Text.Text
    , paneDetail :: !Text.Text
    , paneItems :: ![Text.Text]
    , paneTranscript :: ![Text.Text]
    , paneFooter :: !Text.Text
    }
    deriving (Show)

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
            fitTextCell 4 "界界" `shouldBe` "界界"

        it "keeps newline-plus-variation-selector input on one frame row" do
            let raw = Text.pack ['a', '\n', '\xfe0f', 'b']
                fitted = fitTextCell 4 raw
            fitted `shouldBe` "a b "
            fitted `shouldSatisfy`
                Text.all (`notElem` ['\t', '\r', '\n'])
            terminalTextWidth fitted `shouldBe` 4

        modifyMaxSuccess (const 500) $
            prop "always occupies exactly its requested terminal-cell width" $
                fitTextCellProperty

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

        it "does not let joined newlines split generated frame fields" do
            let joinedNewline = Text.pack ['x', '\n', '\xfe0f', 'y']
                frame = renderSplitPaneFrame SplitPaneFrame
                    { splitPaneMinColumns = 12
                    , splitPaneColumns = 20
                    , splitPaneBodyRows = 1
                    , splitPaneLeftMinWidth = 4
                    , splitPaneLeftMaxWidth = 6
                    , splitPaneDivider = " │ "
                    , splitPaneTitle = joinedNewline
                    , splitPaneHeaderDetail = const joinedNewline
                    , splitPaneLeftHeading = joinedNewline
                    , splitPaneRightHeading = const joinedNewline
                    , splitPaneItems = [joinedNewline]
                    , splitPaneSelectedIndex = 0
                    , splitPaneLeftLabel = \_ item -> item
                    , splitPaneTranscript = const [joinedNewline]
                    , splitPaneEmptyTranscript = joinedNewline
                    , splitPaneFooter = joinedNewline
                    , splitPanePromptStyle = id
                    , splitPaneMutedStyle = id
                    , splitPaneSelectedStyle = id
                    }
                rows = Text.splitOn "\n" frame
            length rows `shouldBe` 4
            map terminalTextWidth rows `shouldBe` replicate 4 20

        modifyMaxSuccess (const 300) $
            prop "keeps every generated Unicode row within the pane geometry" $
                splitPaneGeometryProperty

fitTextCellProperty :: TextCellCase -> Property
fitTextCellProperty (TextCellCase width raw) =
    conjoin
        [ counterexample "terminal width"
            (terminalTextWidth fitted === width)
        , counterexample "embedded layout controls"
            (Text.all (`notElem` ['\t', '\r', '\n']) fitted === True)
        , counterexample "idempotence"
            (fitTextCell width fitted === fitted)
        ]
  where
    fitted = fitTextCell width raw

splitPaneGeometryProperty :: SplitPaneCase -> Property
splitPaneGeometryProperty generated =
    conjoin $
        [ counterexample
            ("row " <> show index <> " has terminal width "
                <> show (terminalTextWidth row)
                <> " instead of " <> show generated.paneColumns
                <> ": " <> show row)
            (terminalTextWidth row === generated.paneColumns)
        | (index, row) <- zip [0 :: Int ..] rows
        ]
            <> [ counterexample "rendered row count"
                    (length rows === generated.paneBodyRows + 3)
               ]
  where
    values =
        case generated.paneItems of
            [] -> [""]
            items -> items
    rendered =
        renderSplitPaneFrame SplitPaneFrame
            { splitPaneMinColumns = 5
            , splitPaneColumns = generated.paneColumns
            , splitPaneBodyRows = generated.paneBodyRows
            , splitPaneLeftMinWidth = 1
            , splitPaneLeftMaxWidth =
                max 1 (generated.paneColumns - 4)
            , splitPaneDivider = " │ "
            , splitPaneTitle = generated.paneTitle
            , splitPaneHeaderDetail = const generated.paneDetail
            , splitPaneLeftHeading = "項目"
            , splitPaneRightHeading = const "preview 🚀"
            , splitPaneItems = values
            , splitPaneSelectedIndex = length values - 1
            , splitPaneLeftLabel = \_ item -> item
            , splitPaneTranscript = const generated.paneTranscript
            , splitPaneEmptyTranscript = "(空)"
            , splitPaneFooter = generated.paneFooter
            , splitPanePromptStyle = id
            , splitPaneMutedStyle = id
            , splitPaneSelectedStyle = id
            }
    rows = Text.splitOn "\n" rendered

instance Arbitrary TextCellCase where
    arbitrary =
        TextCellCase
            <$> chooseInt (0, 100)
            <*> genDisplayText

instance Arbitrary SplitPaneCase where
    arbitrary = do
        columns <- chooseInt (5, 120)
        bodyRows <- chooseInt (0, 10)
        title <- genDisplayText
        detail <- genDisplayText
        itemCount <- chooseInt (0, 12)
        items <- vectorOf itemCount genDisplayText
        transcriptCount <- chooseInt (0, 12)
        transcript <- vectorOf transcriptCount genDisplayText
        footer <- genDisplayText
        pure SplitPaneCase
            { paneColumns = columns
            , paneBodyRows = bodyRows
            , paneTitle = title
            , paneDetail = detail
            , paneItems = items
            , paneTranscript = transcript
            , paneFooter = footer
            }

genDisplayText :: Gen Text.Text
genDisplayText = do
    size <- chooseInt (0, 160)
    Text.pack <$> vectorOf size genDisplayChar

genDisplayChar :: Gen Char
genDisplayChar =
    elements $
        ['a' .. 'z']
            <> ['0' .. '9']
            <> [' ', ' ', '\t', '\r', '\n']
            <> ['界', '語', '🙂', '🚀', 'é', 'ø', '\x0301', '\xFE0F']
