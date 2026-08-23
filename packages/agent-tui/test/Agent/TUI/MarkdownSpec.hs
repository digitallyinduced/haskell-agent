module Agent.TUI.MarkdownSpec (spec) where

import Agent.TUI.Markdown
import Agent.Syntax (loadSyntaxHighlighterFrom)
import Agent.TUI.TextWidth (displayCharCellWidth)
import qualified Agent.TUI.Theme as Theme
import Brick
    ( (<=>)
    , ViewportType(..)
    , Widget
    , renderWidget
    , txt
    , viewport
    )
import Control.Monad (forM_)
import Data.Foldable (toList)
import Data.List (findIndex, isInfixOf)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Graphics.Vty as V
import Graphics.Vty.PictureToSpans (displayOpsForPic)
import Graphics.Vty.Span (SpanOp(..))
import System.Environment (lookupEnv)
import Test.Hspec

spec :: Spec
spec = describe "fullscreen Markdown rendering" do
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
                , InlineSpan
                    (InlineLink "https://example.com")
                    "docs (https://example.com)"
                , InlineSpan InlinePlain "."
                ]

    it "renders links with native terminal hyperlink metadata" do
        let widget :: Widget ()
            widget = markdownWidget "[docs](https://example.com)"
            rendered = show (renderWidget Nothing [widget] (40, 3))
        rendered `shouldSatisfy`
            isInfixOf "attrURL = SetTo \"https://example.com\""

    it "parses links nested inside strong and emphasis spans" do
        parseInline
            "**[PR #316](https://example.com/316)** and *[docs](https://example.com)*"
            `shouldBe`
                [ InlineSpan
                    (InlineStrongLink "https://example.com/316")
                    "PR #316 (https://example.com/316)"
                , InlineSpan InlinePlain " and "
                , InlineSpan
                    (InlineEmphasisLink "https://example.com")
                    "docs (https://example.com)"
                ]

    it "renders a strong link with hyperlink metadata and bold styling" do
        let widget :: Widget ()
            widget =
                markdownWidget
                    "Merged **[PR #316](https://example.com/316)**."
            spans = concat (renderSpanRows 80
                "Merged **[PR #316](https://example.com/316)**.")
            rendered = show (renderWidget Nothing [widget] (80, 3))
        spans `shouldSatisfy`
            any (hasUrlWithStyle "https://example.com/316" V.bold)
        rendered `shouldSatisfy` (not . isInfixOf "[PR #316]")

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

    it "renders a four-space fence nested under a list as a code block" do
        let widget :: Widget ()
            widget =
                markdownWidgetWithCodeControls
                    (\index language ->
                        txt (Text.pack (show index) <> ":" <> language))
                    "- Changed from:\n\
                    \    ```text\n\
                    \    -N -M8G -A64m\n\
                    \    ```"
            rendered = show (renderWidget Nothing [widget] (40, 8))
        rendered `shouldSatisfy` isInfixOf "1:text"
        rendered `shouldSatisfy` isInfixOf "-N -M8G -A64m"
        rendered `shouldSatisfy` (not . isInfixOf "```text")

    it "caches closed fence bodies but leaves open streaming fences uncached" do
        let render input =
                let widget :: Widget ()
                    widget =
                        markdownWidgetWithSyntaxHighlighting
                            Nothing
                            (\index widget ->
                                txt ("cached-" <> Text.pack (show index))
                                    <=> widget)
                            (\index language ->
                                txt (Text.pack (show index) <> ":" <> language))
                            input
                in show (renderWidget Nothing [widget] (40, 12))
            closed = render "```haskell\nmain = pure ()\n```"
            open = render "```haskell\nmain = pure ()"
        closed `shouldSatisfy` isInfixOf "cached-1"
        open `shouldSatisfy` (not . isInfixOf "cached-1")

    it "keeps shorter nested markers inside a longer fence" do
        let widget :: Widget ()
            widget =
                markdownWidgetWithCodeControls
                    (\index language ->
                        txt (Text.pack (show index) <> ":" <> language))
                    "````markdown\n```haskell\nmain = pure ()\n```\n````"
            rendered = show (renderWidget Nothing [widget] (40, 12))
        rendered `shouldSatisfy` isInfixOf "1:markdown"
        rendered `shouldSatisfy` (not . isInfixOf "2:haskell")

    it "applies semantic syntax attributes only after a fence closes" do
        syntaxDirectory <- sourceSyntaxDirectory
        loadSyntaxHighlighterFrom syntaxDirectory >>= \case
            Left message -> expectationFailure (Text.unpack message)
            Right highlighter -> do
                let render input =
                        let widget :: Widget ()
                            widget =
                                markdownWidgetWithSyntaxHighlighting
                                    (Just highlighter)
                                    (\_ body -> body)
                                    (\_ _ -> txt "")
                                    input
                        in show $
                            renderWidget
                                (Just Theme.solarizedDark)
                                [widget]
                                (80, 8)
                    closed =
                        render "```haskell\nmain = putStrLn \"hello\"\n```"
                    open =
                        render "```haskell\nmain = putStrLn \"hello\""
                closed `shouldSatisfy` isInfixOf "RGBColor 38 139 210"
                open `shouldSatisfy` (not . isInfixOf "RGBColor 38 139 210")

    it "renders thematic breaks inside a vertical viewport" do
        let widget :: Widget ()
            widget =
                viewport () Vertical $
                    markdownWidget "---\n\n***\n\n___"
            rendered = show (renderWidget Nothing [widget] (40, 12))
        rendered `shouldSatisfy` (not . null)

    describe "tables" do
        it "renders a naturally sized table with aligned box geometry" do
            renderRows 80
                "| A | BB |\n| --- | --- |\n| x | yy |"
                `shouldBe`
                    [ "┌───┬────┐"
                    , "│ A │ BB │"
                    , "├───┼────┤"
                    , "│ x │ yy │"
                    , "└───┴────┘"
                    ]

        it "wraps constrained cells without clipping or losing borders" do
            let rows =
                    renderRows 48 $
                        Text.unlines
                            [ "| Product | Description | Difference |"
                            , "| --- | --- | --- |"
                            , "| Codex | short description | 0123456789ABCDEFVISIBLE |"
                            ]
                bodyRows =
                    filter (Text.isPrefixOf "│") rows
            map rowDisplayWidth rows
                `shouldBe` replicate (length rows) 48
            map (characterColumns '│') bodyRows
                `shouldSatisfy` allEqual
            Text.unlines rows `shouldSatisfy` Text.isInfixOf "VISIBLE"
            Text.unlines rows `shouldSatisfy` Text.isInfixOf "description"

        it "keeps short columns natural and splits remaining width fairly" do
            let longLeft = Text.replicate 50 "l"
                longRight = Text.replicate 50 "r"
                rows =
                    renderRows 40 $
                        Text.unlines
                            [ "| X | Left | Right |"
                            , "| --- | --- | --- |"
                            , "| x | " <> longLeft <> " | " <> longRight <> " |"
                            ]
            case rows of
                top : _ -> do
                    let junctions = characterColumns '┬' top
                    rowDisplayWidth top `shouldBe` 40
                    case junctions of
                        [first, second] -> do
                            first `shouldBe` 4
                            let leftWidth = second - first - 3
                                rightWidth =
                                    rowDisplayWidth top - second - 4
                            abs (leftWidth - rightWidth) `shouldSatisfy` (<= 1)
                        _ -> expectationFailure
                            ("expected two junctions, got " <> show junctions)
                [] -> expectationFailure "expected a rendered table"

        it "reflows the same table when the viewport is resized" do
            let input =
                    Text.unlines
                        [ "| A | B |"
                        , "| --- | --- |"
                        , "| x | a verbose cell ending in TAILMARK |"
                        ]
                narrow = renderRows 24 input
                wide = renderRows 80 input
            Text.unlines narrow `shouldSatisfy` Text.isInfixOf "TAILMARK"
            Text.unlines wide `shouldSatisfy` Text.isInfixOf "TAILMARK"
            length narrow `shouldSatisfy` (> length wide)
            map rowDisplayWidth narrow `shouldSatisfy` all (<= 24)
            map rowDisplayWidth wide `shouldSatisfy` all (<= 80)

        it "uses a compact fallback below the minimum grid width" do
            let input =
                    "| A | B | C |\n| --- | --- | --- |\n| x | y | z |"
                compact = renderRows 6 input
                grid = renderRows 7 input
                compactText = Text.unlines compact
            compactText `shouldSatisfy` (not . Text.isInfixOf "┌")
            mapM_ (\value ->
                compactText `shouldSatisfy` Text.isInfixOf value)
                ["A", "B", "C", "x", "y", "z"]
            expectFirstRow grid (shouldSatisfyText (Text.isPrefixOf "┌"))
            map rowDisplayWidth grid
                `shouldBe` replicate (length grid) 7

        it "preserves every ASCII column down to a one-cell viewport" do
            let input =
                    "| A | B | C |\n| --- | --- | --- |\n| x | y | z |"
            forM_ [1 .. 6] \width -> do
                let rows = renderRows width input
                    plain = Text.unlines rows
                map rowDisplayWidth rows `shouldSatisfy` all (<= width)
                mapM_ (\value ->
                    plain `shouldSatisfy` Text.isInfixOf value)
                    ["A", "B", "C", "x", "y", "z"]

        it "switches cell padding only when the padded grid fits" do
            let input =
                    "| A | B |\n| --- | --- |\n| x | y |"
                compactGrid = renderRows 8 input
                paddedGrid = renderRows 9 input
            expectFirstRow compactGrid
                (`shouldBe` "┌─┬─┐")
            expectFirstRow paddedGrid
                (`shouldBe` "┌───┬───┐")

        it "accounts for wide glyphs when choosing grid or compact layout" do
            let input =
                    "| A | B |\n| --- | --- |\n| 漢 | z |"
                compact = renderRows 5 input
                grid = renderRows 6 input
            Text.unlines compact `shouldSatisfy` Text.isInfixOf "漢"
            Text.unlines compact `shouldSatisfy` Text.isInfixOf "z"
            expectFirstRow compact
                (shouldSatisfyText (not . Text.isPrefixOf "┌"))
            expectFirstRow grid
                (shouldSatisfyText (Text.isPrefixOf "┌"))
            map rowDisplayWidth grid
                `shouldBe` replicate (length grid) 6

        it "preserves styled text and hyperlink metadata while wrapping" do
            let url = "https://example.com"
                input =
                    "| Kind | Value |\n\
                    \| --- | --- |\n\
                    \| styled | **bold** *emphasis* `code` [docs](https://example.com) |"
                rows = renderRows 30 input
                plain = Text.concat rows
                spans = concat (renderSpanRows 30 input)
            plain `shouldSatisfy` Text.isInfixOf "bold"
            plain `shouldSatisfy` Text.isInfixOf "emphasis"
            plain `shouldSatisfy` Text.isInfixOf "code"
            plain `shouldSatisfy` Text.isInfixOf "docs"
            plain `shouldSatisfy` (not . Text.isInfixOf "**")
            plain `shouldSatisfy` (not . Text.isInfixOf "`code`")
            spans `shouldSatisfy` any (hasUrl url)

        it "preserves multiple records and links in compact layout" do
            let url = "https://example.com"
                input =
                    "| A | B |\n\
                    \| --- | --- |\n\
                    \| first | [one](https://example.com) |\n\
                    \| second | two |"
                rows = renderRows 4 input
                plain = Text.concat rows
                spans = concat (renderSpanRows 4 input)
            expectFirstRow rows
                (shouldSatisfyText (not . Text.isPrefixOf "┌"))
            mapM_ (\value ->
                plain `shouldSatisfy` Text.isInfixOf value)
                ["first", "one", "second", "two"]
            spans `shouldSatisfy` any (hasUrl url)

        it "aligns CJK, emoji, and combining-mark cells by display width" do
            let input =
                    "| Kind | Value |\n\
                    \| --- | --- |\n\
                    \| wide | 漢字🙂 |\n\
                    \| combining | é |"
                rows = renderRows 80 input
                plain = Text.unlines rows
            plain `shouldSatisfy` Text.isInfixOf "漢字🙂"
            plain `shouldSatisfy` Text.isInfixOf "é"
            map rowDisplayWidth rows
                `shouldSatisfy` allEqual

        it "keeps escaped and code-span pipes inside their cells" do
            let input =
                    "| Kind | Value |\n\
                    \| --- | --- |\n\
                    \| escaped | a \\| b |\n\
                    \| code | `c|d` |\n\
                    \| multi | ``e|f`` |"
                rows = renderRows 80 input
                plain = Text.unlines rows
                bodyRows = filter (Text.isPrefixOf "│") (drop 2 rows)
            plain `shouldSatisfy` Text.isInfixOf "a | b"
            plain `shouldSatisfy` Text.isInfixOf "c|d"
            plain `shouldSatisfy` Text.isInfixOf "e|f"
            plain `shouldSatisfy` (not . Text.isInfixOf "`")
            map (characterColumns '│') bodyRows
                `shouldSatisfy` allEqual

        it "handles odd and even backslash runs before pipes" do
            let oddEscaped =
                    renderRows 80
                        "| A | B |\n\
                        \| --- | --- |\n\
                        \| odd | left \\| right |"
                evenDelimiter =
                    renderRows 80
                        "| A | B |\n\
                        \| --- | --- |\n\
                        \| even | left \\\\| TRUNCATED |"
                oddText = Text.unlines oddEscaped
                evenText = Text.unlines evenDelimiter
            oddText `shouldSatisfy` Text.isInfixOf "left | right"
            evenText `shouldSatisfy` Text.isInfixOf "left \\"
            evenText `shouldSatisfy` (not . Text.isInfixOf "TRUNCATED")

        it "treats unmatched backticks as text without hiding delimiters" do
            let rows =
                    renderRows 80 $
                        "| A | B |\n\
                        \| --- | --- |\n\
                        \| unmatched `tick | tail |"
                plain = Text.unlines rows
            plain `shouldSatisfy` Text.isInfixOf "unmatched `tick"
            plain `shouldSatisfy` Text.isInfixOf "tail"
            map rowDisplayWidth rows `shouldSatisfy` allEqual

        it "preserves empty cells and normalizes body rows to the header" do
            let rows =
                    renderRows 80 $
                        "| A | B |\n\
                        \| --- | --- |\n\
                        \|| value |\n\
                        \| x ||\n\
                        \| one | two | EXTRA |"
                plain = Text.unlines rows
                bodyRows = filter (Text.isPrefixOf "│") (drop 2 rows)
            plain `shouldSatisfy` Text.isInfixOf "value"
            plain `shouldSatisfy` Text.isInfixOf "one"
            plain `shouldSatisfy` Text.isInfixOf "two"
            plain `shouldSatisfy` (not . Text.isInfixOf "EXTRA")
            map (characterColumns '│') bodyRows
                `shouldSatisfy` allEqual

        it "accepts alignment markers and rejects malformed separators" do
            let aligned =
                    renderRows 80
                        "| A | B | C |\n| :--- | ---: | :---: |\n| x | y | z |"
                malformed =
                    renderRows 80
                        "| A | B |\n| ---x | --- |\n| x | y |"
            expectFirstRow aligned
                (shouldSatisfyText (Text.isPrefixOf "┌"))
            Text.unlines malformed `shouldSatisfy`
                (not . Text.isInfixOf "┌")
            Text.unlines malformed `shouldSatisfy`
                Text.isInfixOf "---x"

        it "accepts tables without optional outer pipes" do
            let rows =
                    renderRows 80
                        "A | B\n--- | ---\nx | y"
                plain = Text.unlines rows
            expectFirstRow rows
                (shouldSatisfyText (Text.isPrefixOf "┌"))
            plain `shouldSatisfy` Text.isInfixOf "x"
            plain `shouldSatisfy` Text.isInfixOf "y"

        it "rejects separator column-count mismatches and false pipe rows" do
            let mismatched =
                    renderRows 80
                        "| A | B |\n| --- | --- | --- |\n| x | y |"
                escapedOnly =
                    renderRows 80
                        "| a \\| b\n| --- |\n| value |"
            Text.unlines mismatched `shouldSatisfy`
                (not . Text.isInfixOf "┌")
            Text.unlines escapedOnly `shouldSatisfy`
                (not . Text.isInfixOf "┌")

        it "renders a header-only table and leaves following prose outside it" do
            let headerOnly =
                    renderRows 80
                        "| A | B |\n| --- | --- |"
                withProse =
                    renderRows 80
                        "| A | B |\n| --- | --- |\n| x | y |\nafter table"
                bottomIndex = findIndex (Text.isPrefixOf "└") withProse
                proseIndex = findIndex (Text.isInfixOf "after table") withProse
            length headerOnly `shouldBe` 4
            case (bottomIndex, proseIndex) of
                (Just bottom, Just prose) ->
                    prose `shouldSatisfy` (> bottom)
                _ -> expectationFailure
                    "expected both the table bottom and following prose"

sourceSyntaxDirectory :: IO FilePath
sourceSyntaxDirectory =
    lookupEnv "AGENT_SYNTAX_DIR" >>= \case
        Nothing -> do
            expectationFailure
                "AGENT_SYNTAX_DIR is not set; run tests from nix develop"
            fail "unreachable"
        Just syntaxDirectory ->
            pure syntaxDirectory

renderRows :: Int -> Text.Text -> [Text.Text]
renderRows width =
    map spanRowText . renderSpanRows width

renderSpanRows :: Int -> Text.Text -> [[SpanOp]]
renderSpanRows width input =
    reverse $
        dropWhile (Text.null . Text.strip . spanRowText) $
            reverse rows
  where
    region = (width, 200)
    widget :: Widget ()
    widget = markdownWidget input
    rows =
        map toList $
            toList $
                displayOpsForPic
                    (renderWidget Nothing [widget] region)
                    region

spanRowText :: [SpanOp] -> Text.Text
spanRowText =
    Text.dropWhileEnd (== ' ')
        . Text.concat
        . map \case
            TextSpan{textSpanText} ->
                LazyText.toStrict textSpanText
            Skip count -> Text.replicate count " "
            RowEnd count -> Text.replicate count " "

rowDisplayWidth :: Text.Text -> Int
rowDisplayWidth =
    Text.foldl'
        (\width character -> width + displayCharCellWidth character)
        0

characterColumns :: Char -> Text.Text -> [Int]
characterColumns target = go 0 . Text.unpack
  where
    go _ [] = []
    go column (character : rest) =
        let following =
                go (column + displayCharCellWidth character) rest
        in if character == target
            then column : following
            else following

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (first : rest) = all (== first) rest

hasUrl :: Text.Text -> SpanOp -> Bool
hasUrl url = \case
    TextSpan{textSpanAttr} ->
        V.attrURL textSpanAttr == V.SetTo url
    Skip _ -> False
    RowEnd _ -> False

hasUrlWithStyle :: Text.Text -> V.Style -> SpanOp -> Bool
hasUrlWithStyle url style = \case
    TextSpan{textSpanAttr} ->
        V.attrURL textSpanAttr == V.SetTo url
            && V.attrStyle textSpanAttr == V.SetTo style
    Skip _ -> False
    RowEnd _ -> False

expectFirstRow :: [Text.Text] -> (Text.Text -> Expectation) -> Expectation
expectFirstRow rows assertion =
    case rows of
        first : _ -> assertion first
        [] -> expectationFailure "expected at least one rendered row"

shouldSatisfyText
    :: (Text.Text -> Bool)
    -> Text.Text
    -> Expectation
shouldSatisfyText predicate value =
    value `shouldSatisfy` predicate
