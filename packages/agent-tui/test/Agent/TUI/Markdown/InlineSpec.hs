module Agent.TUI.Markdown.InlineSpec (spec) where

import Agent.TUI.Markdown.Inline
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "shared inline Markdown parsing" do
    it "parses recursively composed formatting" do
        parseInline "**bold and *italic* with `code`**"
            `shouldBe`
                [ InlineStrong
                    [ InlineText "bold and "
                    , InlineEmphasis [InlineText "italic"]
                    , InlineText " with "
                    , InlineCode "code"
                    ]
                ]

    it "parses formatting inside link labels" do
        parseInline "[**important** `docs`](https://example.com)"
            `shouldBe`
                [ InlineLink
                    "https://example.com"
                    [ InlineStrong [InlineText "important"]
                    , InlineText " "
                    , InlineCode "docs"
                    ]
                ]

    it "supports multi-backtick code spans" do
        parseInline "``code `with` ticks``"
            `shouldBe` [InlineCode "code `with` ticks"]

    it "unescapes ASCII punctuation and keeps non-punctuation slashes" do
        inlinePlainText (parseInline "\\*literal\\* C:\\Users")
            `shouldBe` "*literal* C:\\Users"

    it "supports balanced and escaped parentheses in link destinations" do
        let parsed =
                parseInline
                    "[one](https://example.com/a_(b)) [two](https://example.com/a_\\(b\\))"
        inlinePlainText parsed
            `shouldBe`
                "one (https://example.com/a_(b)) two (https://example.com/a_(b))"
        parsed
            `shouldBe`
                [ InlineLink
                    "https://example.com/a_(b)"
                    [InlineText "one"]
                , InlineText " "
                , InlineLink
                    "https://example.com/a_(b)"
                    [InlineText "two"]
                ]

    it "parses bare HTTP(S) URLs without changing their visible text" do
        let url = "https://github.com/digitallyinduced/haskell-agent/pull/339"
        parseInline ("PR: " <> url)
            `shouldBe`
                [ InlineText "PR: "
                , InlineLink url [InlineText url]
                ]
        inlinePlainText (parseInline ("PR: " <> url))
            `shouldBe` "PR: " <> url

    it "leaves trailing sentence punctuation outside bare URLs" do
        parseInline
            "See https://example.com/a_(b)), then http://example.net/path."
            `shouldBe`
                [ InlineText "See "
                , InlineLink
                    "https://example.com/a_(b)"
                    [InlineText "https://example.com/a_(b)"]
                , InlineText "), then "
                , InlineLink
                    "http://example.net/path"
                    [InlineText "http://example.net/path"]
                , InlineText "."
                ]

    it "does not autolink URLs inside code or embedded in words" do
        parseInline "`https://example.com` abchttps://example.com"
            `shouldBe`
                [ InlineCode "https://example.com"
                , InlineText " abchttps://example.com"
                ]

    it "preserves intraword underscore delimiters" do
        parseInline "foo__bar__baz snake_case"
            `shouldBe` [InlineText "foo__bar__baz snake_case"]

    it "parses flanked underscore emphasis" do
        parseInline "use _established terms_ here"
            `shouldBe`
                [ InlineText "use "
                , InlineEmphasis [InlineText "established terms"]
                , InlineText " here"
                ]

    it "leaves unmatched constructs literal" do
        parseInline "unfinished **bold and [link](url"
            `shouldBe`
                [InlineText "unfinished **bold and [link](url"]

    it "does not parse constructs across lines" do
        parseInline "**first\nsecond**"
            `shouldBe` [InlineText "**first\nsecond**"]

    it "appends visible destinations only when needed" do
        inlinePlainText
            (parseInline
                "[docs](https://example.com) [https://same](https://same) [](empty)")
            `shouldBe`
                "docs (https://example.com) https://same [](empty)"

    it "coalesces long plain input" do
        parseInline "plain text"
            `shouldBe` [InlineText "plain text"]

    it "preserves escaped delimiters in long link labels and destinations" do
        parseInline "[a\\]b](https://example.com/a\\(b\\))"
            `shouldBe`
                [ InlineLink
                    "https://example.com/a(b)"
                    [InlineText "a]b"]
                ]

    it "keeps unmatched multi-backtick spans literal" do
        parseInline "``unfinished"
            `shouldBe` [InlineText "``unfinished"]

    it "does not flatten prior text while scanning repeated unmatched markers" do
        let source = Text.replicate 200 "* "
        parseInline source `shouldBe` [InlineText source]
