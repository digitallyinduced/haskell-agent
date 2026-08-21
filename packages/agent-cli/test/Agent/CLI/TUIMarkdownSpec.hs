module Agent.CLI.TUIMarkdownSpec (spec) where

import Agent.CLI.TUI.Markdown
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
