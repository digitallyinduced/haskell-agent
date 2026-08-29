module Agent.TUI.Markdown.BlockSpec (spec) where

import Agent.TUI.Markdown.Block
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
    ( Gen
    , chooseInt
    , elements
    , forAll
    , listOf1
    , vectorOf
    , (===)
    )

spec :: Spec
spec = describe "Markdown block parsing" do
    it "parses heading levels and requires following whitespace" do
        headingParts "  ### Title  " `shouldBe` Just (3, "Title")
        headingParts "###Title" `shouldBe` Nothing
        headingParts "####### Title" `shouldBe` Nothing
        headingPartsWith (== ' ') "#\tTitle" `shouldBe` Nothing

    it "parses indented unordered and ordered list items" do
        bulletParts "  * item" `shouldBe` Just ("  ", "item")
        bulletParts "\t-\titem" `shouldBe` Just ("\t", "item")
        bulletPartsWith (== ' ') "\t-\titem" `shouldBe` Nothing
        orderedParts "    12. item" `shouldBe`
            Just ("    ", "12", "item")

    it "removes only the block quote marker" do
        blockQuoteRemainder "  >  quote" `shouldBe` Just "  quote"
        blockQuoteRemainder "plain" `shouldBe` Nothing

    it "recognizes thematic breaks" do
        isThematicBreak " * * * " `shouldBe` True
        isThematicBreak "--" `shouldBe` False
        isThematicBreak "-*-" `shouldBe` False

    it "parses tables and consumes the separator row" do
        takeTableRows
            [ "| name | value |"
            , "| :--- | ---: |"
            , "| a | b |"
            , "after"
            ]
            `shouldBe`
                Just
                    ( MarkdownTable
                        { tableAlignments = [AlignLeft, AlignRight]
                        , tableRows = [["name", "value"], ["a", "b"]]
                        }
                    , ["after"]
                    )

    it "preserves escaped and code-span pipes inside cells" do
        splitTableRow "| a\\|b | `c|d` | e |" `shouldBe`
            Just ["a|b", "`c|d`", "e"]

    it "rejects malformed tables" do
        takeTableRows ["a | b", "---", "c | d"] `shouldBe` Nothing
        takeTableRows ["a | b", "--- | --- | ---"] `shouldBe` Nothing

    prop "round-trips generated tables and GFM alignments" $
        forAll generatedTable $ \(outerPipes, alignments, header, body) ->
            let input =
                    [ renderRow outerPipes header
                    , renderRow outerPipes (map alignmentMarker alignments)
                    , renderRow outerPipes body
                    , "after"
                    ]
            in takeTableRows input
                === Just
                    ( MarkdownTable
                        { tableAlignments = alignments
                        , tableRows = [header, body]
                        }
                    , ["after"]
                    )

generatedTable :: Gen (Bool, [TableAlignment], [Text], [Text])
generatedTable = do
    columnCount <- chooseInt (2, 8)
    outerPipes <- elements [False, True]
    alignments <- vectorOf columnCount $ elements
        [AlignDefault, AlignLeft, AlignCenter, AlignRight]
    header <- vectorOf columnCount safeCell
    body <- vectorOf columnCount safeCell
    pure (outerPipes, alignments, header, body)

safeCell :: Gen Text
safeCell = Text.pack <$> listOf1 (elements (['a' .. 'z'] <> ['0' .. '9']))

renderRow :: Bool -> [Text] -> Text
renderRow outerPipes cells =
    let row = Text.intercalate " | " cells
    in if outerPipes then "| " <> row <> " |" else row

alignmentMarker :: TableAlignment -> Text
alignmentMarker alignment = case alignment of
    AlignDefault -> "---"
    AlignLeft -> ":---"
    AlignCenter -> ":---:"
    AlignRight -> "---:"
