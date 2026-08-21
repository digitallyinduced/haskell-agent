module Agent.TUI.MarkdownSpec (spec) where

import Agent.TUI.Markdown
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
