module Agent.CLI.MarkdownSpec (spec) where

import Agent.CLI.Input (terminalTextWidth)
import Agent.CLI.Markdown
    ( MarkdownFragmentSplit(..)
    , renderMarkdown
    , renderMarkdownFragment
    , splitMarkdownFragment
    )
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
    ( Gen
    , counterexample
    , elements
    , forAll
    , listOf1
    , (===)
    )

stripAnsi :: Text -> Text
stripAnsi text = case Text.break (== '\ESC') text of
    (before, rest)
        | Text.null rest -> before
        | otherwise ->
            let afterEsc = Text.drop 1 rest
                dropped = Text.drop 1 (Text.dropWhile (/= 'm') afterEsc)
            in before <> stripAnsi dropped

osc8Payload :: Text -> Text -> Maybe Text
osc8Payload url output = do
    let opener = "\ESC]8;;" <> url <> "\ESC\\"
        afterOpener = snd (Text.breakOn opener output)
    if Text.null afterOpener
        then Nothing
        else
            let payload =
                    Text.drop (Text.length opener) afterOpener
                (linked, closer) =
                    Text.breakOn "\ESC]8;;\ESC\\" payload
            in if Text.null closer then Nothing else Just linked

safeLink :: Gen (Text, Text)
safeLink = do
    label <- Text.pack <$> listOf1 (elements safeLabelCharacters)
    path <- Text.pack <$> listOf1 (elements safePathCharacters)
    pure (label, "https://example.test/" <> path)
  where
    safeLabelCharacters = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']
    safePathCharacters = safeLabelCharacters <> "-_/"

generatedRightAlignedTable :: Gen (Text, Text)
generatedRightAlignedTable = do
    firstValue <- Text.pack <$> listOf1 (elements ['0' .. '9'])
    secondValue <- Text.pack <$> listOf1 (elements ['0' .. '9'])
    pure (firstValue, secondValue)

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

        it "dims fence bodies and drops fence markers and language labels" do
            let out = renderMarkdown True "```haskell\nmain = pure ()\n```"
            out `shouldSatisfy` Text.isInfixOf "main = pure ()"
            out `shouldSatisfy` (not . Text.isInfixOf "```")
            out `shouldSatisfy` (not . Text.isInfixOf "haskell")
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
                out = renderMarkdown True ("Merged PR [#537](" <> url <> ").")
            stripAnsi <$> osc8Payload url out
                `shouldBe` Just ("#537 (" <> url <> ")")

        prop "keeps every generated displayed link inside its OSC 8 span" $
            forAll safeLink \(label, url) ->
                let out =
                        renderMarkdown True
                            ("[" <> label <> "](" <> url <> ")")
                    expected = label <> " (" <> url <> ")"
                in counterexample (show out) $
                    (stripAnsi <$> osc8Payload url out) === Just expected

        it "restores heading styling after nested code" do
            let out = renderMarkdown True "# before `code` after"
                afterCode = snd (Text.breakOn "code" out)
            afterCode `shouldSatisfy` Text.isInfixOf "\ESC["
            stripAnsi out `shouldBe` "before code after"

        it "keeps table columns aligned when cells have inline markers" do
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
                cleaned = stripAnsi out
            cleaned `shouldSatisfy` Text.isInfixOf "┌──────┬────────┐"
            cleaned `shouldSatisfy` Text.isInfixOf "│ file │ status │"
            cleaned `shouldSatisfy` Text.isInfixOf "├──────┼────────┤"
            cleaned `shouldSatisfy` Text.isInfixOf "│ a.hs │ ok     │"
            cleaned `shouldSatisfy` Text.isInfixOf "└──────┴────────┘"
            out `shouldSatisfy` (not . Text.isInfixOf "|")

        it "renders full-grid tables with GFM column alignment" do
            let sample =
                    "| Name | Footprint |\n\
                    \| --- | ---: |\n\
                    \| WebKit | 27.7 GiB |\n\
                    \| Control Center | 4.6 GiB |"
                rows =
                    map stripAnsi
                        (filter (not . Text.null . Text.strip)
                            (Text.lines (renderMarkdown True sample)))
                bodyRows =
                    drop 1 (filter (Text.isPrefixOf "│") rows)
                valueEnd needle row =
                    let offset = Text.length (fst (Text.breakOn needle row))
                    in offset + Text.length needle
                contentRows = filter (Text.isPrefixOf "│") rows
            rows `shouldSatisfy` any (Text.isPrefixOf "┌")
            rows `shouldSatisfy` any (Text.isPrefixOf "├")
            rows `shouldSatisfy` any (Text.isPrefixOf "└")
            contentRows `shouldSatisfy` all (Text.isSuffixOf "│")
            case bodyRows of
                webkit : control : _ ->
                    valueEnd "27.7 GiB" webkit
                        `shouldBe` valueEnd "4.6 GiB" control
                _ -> expectationFailure "expected two table body rows"

        prop "right-aligns generated table values inside a full grid" $
            forAll generatedRightAlignedTable $ \(firstValue, secondValue) ->
                let sample =
                        "Name | Value\n--- | ---:\nfirst | " <> firstValue
                            <> "\nsecond | " <> secondValue
                    rows =
                        map stripAnsi
                            (filter (not . Text.null . Text.strip)
                                (Text.lines (renderMarkdown True sample)))
                    bodyRows =
                        drop 1 (filter (Text.isPrefixOf "│") rows)
                    valueEnd needle row =
                        Text.length (fst (Text.breakOn needle row))
                            + Text.length needle
                    hasFullGrid = case rows of
                        top : (_ : rest) ->
                            Text.isPrefixOf "┌" top
                                && any (Text.isPrefixOf "└") rest
                                && all
                                    (Text.isSuffixOf "│")
                                    (filter (Text.isPrefixOf "│") rows)
                        _ -> False
                in case bodyRows of
                    firstRow : secondRow : _ ->
                        counterexample (show rows) $
                            ( hasFullGrid
                            , valueEnd firstValue firstRow
                            )
                                === (True, valueEnd secondValue secondRow)
                    _ -> counterexample (show rows) False

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
            let split1 = splitMarkdownFragment Nothing "say **"
                split2 =
                    splitMarkdownFragment
                        split1.markdownPrevChar
                        (split1.markdownPending <> "hello")
                split3 =
                    splitMarkdownFragment
                        split2.markdownPrevChar
                        (split2.markdownPending <> "** there")
                out =
                    renderMarkdownFragment
                        True
                        split2.markdownPrevChar
                        split3.markdownReady
            split1.markdownReady `shouldBe` "say "
            split1.markdownPending `shouldBe` "**"
            split2.markdownReady `shouldBe` ""
            split2.markdownPending `shouldBe` "**hello"
            split3.markdownPending `shouldBe` ""
            stripAnsi out `shouldBe` "hello there"

        it "handles a bold delimiter split one star at a time" do
            let split1 = splitMarkdownFragment Nothing "*"
                split2 =
                    splitMarkdownFragment
                        split1.markdownPrevChar
                        (split1.markdownPending <> "*a*")
                split3 =
                    splitMarkdownFragment
                        split2.markdownPrevChar
                        (split2.markdownPending <> "*")
            split1.markdownReady `shouldBe` ""
            split2.markdownReady `shouldBe` ""
            split3.markdownPending `shouldBe` ""
            stripAnsi
                (renderMarkdownFragment
                    True
                    split2.markdownPrevChar
                    split3.markdownReady)
                `shouldBe` "a"

        it "does not turn a chunked snake_case identifier into emphasis" do
            let split1 = splitMarkdownFragment Nothing "snake"
                split2 =
                    splitMarkdownFragment
                        split1.markdownPrevChar
                        (split1.markdownPending <> "_case_name")
            split2.markdownPending `shouldBe` ""
            renderMarkdownFragment True Nothing split1.markdownReady
                <> renderMarkdownFragment
                    True
                    split1.markdownPrevChar
                    split2.markdownReady
                `shouldBe` "snake_case_name"

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (first : rest) = all (== first) rest
