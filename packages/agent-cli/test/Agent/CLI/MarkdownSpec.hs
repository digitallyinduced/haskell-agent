module Agent.CLI.MarkdownSpec (spec) where

import Agent.CLI.Input (terminalTextWidth)
import Agent.CLI.Markdown
    ( renderMarkdown
    , renderMarkdownFragment
    , splitMarkdownFragment
    )
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

stripAnsi :: Text -> Text
stripAnsi text = case Text.break (== '\ESC') text of
    (before, rest)
        | Text.null rest -> before
        | otherwise ->
            let afterEsc = Text.drop 1 rest
                dropped = Text.drop 1 (Text.dropWhile (/= 'm') afterEsc)
            in before <> stripAnsi dropped

spec :: Spec
spec = do
    describe "renderMarkdown" do
        it "leaves text unchanged when color is off" do
            let sample = "see `file.txt` and **bold**"
            renderMarkdown False sample `shouldBe` sample

        it "styles inline code when color is on" do
            let out = renderMarkdown True "see `file.txt` now"
            out `shouldSatisfy` Text.isInfixOf "file.txt"
            out `shouldSatisfy` Text.isInfixOf "\ESC["
            out `shouldSatisfy` (not . Text.isInfixOf "`file.txt`")

        it "does not treat fence interiors as inline code" do
            let sample = "```\nuse `code` here\n```"
                out = renderMarkdown True sample
            out `shouldSatisfy` Text.isInfixOf "use `code` here"
            Text.count "`code`" out `shouldBe` 1

        it "dims fence bodies and drops fence markers" do
            let out = renderMarkdown True "```haskell\nmain = pure ()\n```"
            out `shouldSatisfy` Text.isInfixOf "main = pure ()"
            out `shouldSatisfy` (not . Text.isInfixOf "```")
            out `shouldSatisfy` Text.isInfixOf "haskell"
            out `shouldSatisfy` Text.isInfixOf "\ESC["

        it "supports tilde fences" do
            let out = renderMarkdown True "~~~\nbody\n~~~"
            out `shouldSatisfy` Text.isInfixOf "body"
            out `shouldSatisfy` (not . Text.isInfixOf "~~~")

        it "hides heading markers and colors titles by level" do
            let h1 = renderMarkdown True "# Title"
                h2 = renderMarkdown True "## Title"
            h1 `shouldSatisfy` Text.isInfixOf "Title"
            h1 `shouldSatisfy` Text.isInfixOf "\ESC["
            h1 `shouldSatisfy` (not . Text.isInfixOf "# Title")
            h2 `shouldSatisfy` Text.isInfixOf "Title"
            h2 `shouldSatisfy` (not . Text.isInfixOf "## Title")
            h1 `shouldSatisfy` (/= h2)

        it "colors unordered and ordered list markers" do
            let ul = renderMarkdown True "- item"
                ol = renderMarkdown True "1. item"
            ul `shouldSatisfy` Text.isInfixOf "item"
            ul `shouldSatisfy` Text.isInfixOf "\ESC["
            ul `shouldSatisfy` (not . Text.isInfixOf "- item")
            ol `shouldSatisfy` Text.isInfixOf "item"
            ol `shouldSatisfy` Text.isInfixOf "\ESC["

        it "preserves indentation for nested lists" do
            let out = renderMarkdown True "- parent\n  - child\n    1. grandchild"
                cleaned = Text.lines (stripAnsi out)
            cleaned `shouldBe` ["• parent", "  • child", "    1. grandchild"]

        it "mutes plain blockquotes" do
            let out = renderMarkdown True "> note"
            out `shouldSatisfy` Text.isInfixOf "note"
            out `shouldSatisfy` Text.isInfixOf "│"
            out `shouldSatisfy` Text.isInfixOf "\ESC["
            out `shouldSatisfy` (not . Text.isInfixOf "> note")

        it "styles bold markers" do
            let out = renderMarkdown True "say **hello** there"
            out `shouldSatisfy` Text.isInfixOf "hello"
            out `shouldSatisfy` (not . Text.isInfixOf "**hello**")

        it "does not italicize snake_case identifiers" do
            let out = renderMarkdown True "use snake_case_name please"
            out `shouldBe` "use snake_case_name please"

        it "still italicizes flanked underscores" do
            let out = renderMarkdown True "use _already established_ terms"
            out `shouldSatisfy` Text.isInfixOf "already established"
            out `shouldSatisfy` (not . Text.isInfixOf "_already established_")

        it "supports multi-backtick inline code" do
            let out = renderMarkdown True "see ``code `with` ticks`` now"
            out `shouldSatisfy` Text.isInfixOf "code `with` ticks"
            out `shouldSatisfy` (not . Text.isInfixOf "``")

        it "styles inline code inside headings" do
            let out = renderMarkdown True "# see `Render.hs`"
            out `shouldSatisfy` Text.isInfixOf "Render.hs"
            out `shouldSatisfy` (not . Text.isInfixOf "`Render.hs`")

        it "composes nested inline styles and formatted link labels" do
            let out =
                    renderMarkdown True
                        "**bold and *italic* with `code` and \
                        \[docs **important**](https://example.com)**"
            mapM_
                (\fragment ->
                    out `shouldSatisfy` Text.isInfixOf fragment)
                [ "bold and "
                , "italic"
                , "code"
                , "docs "
                , "important"
                , " (https://example.com)"
                ]
            out `shouldSatisfy` (not . Text.isInfixOf "**")
            out `shouldSatisfy` (not . Text.isInfixOf "*italic*")

        it "supports escapes and balanced parentheses in link destinations" do
            let out =
                    renderMarkdown True
                        "\\*literal\\* [x](https://example.com/a_(b))"
            stripAnsi out `shouldSatisfy` Text.isInfixOf "*literal*"
            out `shouldSatisfy`
                Text.isInfixOf "https://example.com/a_(b)"

        it "wraps bare URLs in OSC 8 hyperlinks" do
            let url = "https://github.com/digitallyinduced/haskell-agent/pull/339"
                out = renderMarkdown True ("PR: " <> url)
            out `shouldSatisfy`
                Text.isInfixOf ("\ESC]8;;" <> url <> "\ESC\\")
            out `shouldSatisfy` Text.isInfixOf url
            out `shouldSatisfy` Text.isInfixOf "\ESC]8;;\ESC\\"

        it "keeps a link's displayed URL suffix inside its OSC 8 hyperlink" do
            let url = "https://github.com/digitallyinduced/haskell-agent/pull/537"
                opener = "\ESC]8;;" <> url <> "\ESC\\"
                out = renderMarkdown True ("Merged PR [#537](" <> url <> ").")
                afterOpener = Text.drop (Text.length opener) (snd (Text.breakOn opener out))
                linkedPayload = fst (Text.breakOn "\ESC]8;;\ESC\\" afterOpener)
            linkedPayload `shouldSatisfy` Text.isInfixOf "#537"
            linkedPayload `shouldSatisfy` Text.isInfixOf url

        it "restores heading styling after nested code" do
            let out = renderMarkdown True "# before `code` after"
                afterCode = snd (Text.breakOn "code" out)
            afterCode `shouldSatisfy` Text.isInfixOf "\ESC["
            stripAnsi out `shouldBe` "before code after"

        it "keeps box borders aligned when cells have inline markers" do
            let mdTable = Text.unlines
                    [ "| a | b |"
                    , "| --- | --- |"
                    , "| **x** | `y` |"
                    ]
                out = renderMarkdown True mdTable
                cleaned =
                    map stripAnsi
                        (filter (not . Text.null . Text.strip) (Text.lines out))
                body =
                    [ l
                    | l <- cleaned
                    , "│" `Text.isInfixOf` l
                    , not ("─" `Text.isInfixOf` l)
                    ]
            case body of
                [headerRow, bodyRow] ->
                    Text.length headerRow `shouldBe` Text.length bodyRow
                _ ->
                    expectationFailure
                        ("expected exactly two table rows, got " <> show body)

        it "keeps table borders aligned around multi-codepoint graphemes" do
            let womanTechnologist =
                    Text.pack ['\x1f469', '\x200d', '\x1f4bb']
                mdTable =
                    "| kind | value |\n\
                    \| --- | --- |\n\
                    \| emoji | " <> womanTechnologist <> " |"
                rows =
                    filter (not . Text.null . Text.strip)
                        (map stripAnsi (Text.lines (renderMarkdown True mdTable)))
            map terminalTextWidth rows `shouldSatisfy` allEqual

        it "preserves identifiers and styled links in table cells" do
            let sample =
                    "| key | value |\n\
                    \| --- | --- |\n\
                    \| snake_case | [docs](https://example.com) |"
                out = renderMarkdown True sample
                cleaned = stripAnsi out
            cleaned `shouldSatisfy` Text.isInfixOf "snake_case"
            cleaned `shouldSatisfy` Text.isInfixOf "docs"
            out `shouldSatisfy` Text.isInfixOf "https://example.com"

        it "uses strict shared fence rules" do
            let overIndented =
                    renderMarkdown True "    ```haskell\nbody\n    ```"
                invalidInfo =
                    renderMarkdown True "```bad`info\nbody\n```"
            overIndented `shouldSatisfy` Text.isInfixOf "```haskell"
            invalidInfo `shouldSatisfy` Text.isPrefixOf "``"
            invalidInfo `shouldSatisfy` Text.isInfixOf "bad"
            invalidInfo `shouldSatisfy` Text.isInfixOf "info"
            invalidInfo `shouldSatisfy` Text.isInfixOf "body"

        it "renders a basic pipe table" do
            let sample = "| file | status |\n| --- | --- |\n| a.hs | ok |"
                out = renderMarkdown True sample
            out `shouldSatisfy` Text.isInfixOf "file"
            out `shouldSatisfy` Text.isInfixOf "a.hs"
            out `shouldSatisfy` (not . Text.isInfixOf "|")

        it "strips raw ESC from model output" do
            let out = renderMarkdown True ("hi" <> "\ESC[31m" <> "x")
            out `shouldSatisfy` (not . Text.isInfixOf "\ESC[31m")
            out `shouldSatisfy` Text.isInfixOf "x"

        it "preserves a trailing newline" do
            renderMarkdown True "hi\n" `shouldSatisfy` Text.isSuffixOf "\n"

        it "restores the terminal default background after inline spans" do
            let out = renderMarkdown True "see `file.txt` now"
            out `shouldSatisfy` Text.isInfixOf "\ESC[1;36m"
            out `shouldSatisfy` (not . Text.isInfixOf "48;")

    describe "splitMarkdownFragment" do
        it "holds a bold span until its closing delimiter arrives" do
            let (ready1, pending1, context1) =
                    splitMarkdownFragment Nothing "say **"
                (ready2, pending2, context2) =
                    splitMarkdownFragment context1 (pending1 <> "hello")
                (ready3, pending3, _context3) =
                    splitMarkdownFragment context2 (pending2 <> "** there")
                out = renderMarkdownFragment True context2 ready3
            ready1 `shouldBe` "say "
            pending1 `shouldBe` "**"
            ready2 `shouldBe` ""
            pending2 `shouldBe` "**hello"
            pending3 `shouldBe` ""
            stripAnsi out `shouldBe` "hello there"

        it "handles a bold delimiter split one star at a time" do
            let (ready1, pending1, context1) =
                    splitMarkdownFragment Nothing "*"
                (ready2, pending2, context2) =
                    splitMarkdownFragment context1 (pending1 <> "*a*")
                (ready3, pending3, _context3) =
                    splitMarkdownFragment context2 (pending2 <> "*")
            ready1 `shouldBe` ""
            ready2 `shouldBe` ""
            pending3 `shouldBe` ""
            stripAnsi (renderMarkdownFragment True context2 ready3)
                `shouldBe` "a"

        it "does not turn a chunked snake_case identifier into emphasis" do
            let (ready1, pending1, context1) =
                    splitMarkdownFragment Nothing "snake"
                (ready2, pending2, _context2) =
                    splitMarkdownFragment context1 (pending1 <> "_case_name")
            pending2 `shouldBe` ""
            renderMarkdownFragment True Nothing ready1
                <> renderMarkdownFragment True context1 ready2
                `shouldBe` "snake_case_name"

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (first : rest) = all (== first) rest
