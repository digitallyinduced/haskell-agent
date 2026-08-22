module Agent.TUI.MarkdownSpec (spec) where

import Agent.TUI.Markdown
import Brick
    ( ViewportType(..)
    , Widget
    , renderWidget
    , txt
    , viewport
    )
import Data.List (isInfixOf)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "fullscreen Markdown inline parsing" do
    it "parses strong, emphasis, code, and links" do
        parseInline
            "Use **bold**, *italics*, `code`, and [docs](https://example.com)."
            `shouldBe`
                [ InlineSpan InlinePlain "Use "
                , InlineSpan InlineStrong "bold"
                , InlineSpan InlinePlain ", "
                , InlineSpan InlineEmphasis "italics"
                , InlineSpan InlinePlain ", "
                , InlineSpan InlineCode "code"
                , InlineSpan InlinePlain ", and "
                , InlineSpan InlineLink "docs (https://example.com)"
                , InlineSpan InlinePlain "."
                ]

    it "supports multi-backtick code and avoids snake_case emphasis" do
        let spans = parseInline "``a ` b`` and snake_case"
        inlinePlainText spans `shouldBe` "a ` b and snake_case"
        spans `shouldContain` [InlineSpan InlineCode "a ` b"]

    it "leaves unmatched delimiters visible" do
        inlinePlainText (parseInline "unfinished **bold")
            `shouldBe` "unfinished **bold"

    it "keeps long unstyled input in one plain span" do
        let input = Text.replicate 10000 "a"
        parseInline input
            `shouldBe` [InlineSpan InlinePlain input]

    it "renders numbered controls for fenced code block headers" do
        let widget :: Widget ()
            widget =
                markdownWidgetWithCodeControls
                    (\index language ->
                        txt (Text.pack (show index) <> ":" <> language))
                    "```bash\none\n```\n\n~~~haskell\ntwo\n~~~"
            rendered = show (renderWidget Nothing [widget] (40, 12))
        rendered `shouldSatisfy` isInfixOf "1:bash"
        rendered `shouldSatisfy` isInfixOf "2:haskell"

    it "renders thematic breaks inside a vertical viewport" do
        let widget :: Widget ()
            widget =
                viewport () Vertical $
                    markdownWidget "---\n\n***\n\n___"
            rendered = show (renderWidget Nothing [widget] (40, 12))
        rendered `shouldSatisfy` (not . null)
