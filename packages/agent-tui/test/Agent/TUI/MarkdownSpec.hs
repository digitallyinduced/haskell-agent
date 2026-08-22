module Agent.TUI.MarkdownSpec (spec) where

import Agent.TUI.Markdown
import Agent.Syntax (loadSyntaxHighlighterFrom)
import qualified Agent.TUI.Theme as Theme
import Brick
    ( (<=>)
    , ViewportType(..)
    , Widget
    , renderWidget
    , txt
    , viewport
    )
import Data.List (isInfixOf)
import qualified Data.Text as Text
import System.Environment (lookupEnv)
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

    it "wraps long table cells instead of clipping later columns" do
        let widget :: Widget ()
            widget =
                markdownWidget $
                    Text.unlines
                        [ "| Product | Description | Difference |"
                        , "| --- | --- | --- |"
                        , "| Codex | short description | 0123456789ABCDEFVISIBLE |"
                        ]
            rendered = show (renderWidget Nothing [widget] (48, 20))
        rendered `shouldSatisfy` isInfixOf "VISIBLE"
        rendered `shouldSatisfy` isInfixOf "description"

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

sourceSyntaxDirectory :: IO FilePath
sourceSyntaxDirectory =
    lookupEnv "AGENT_SYNTAX_DIR" >>= \case
        Nothing -> do
            expectationFailure
                "AGENT_SYNTAX_DIR is not set; run tests from nix develop"
            fail "unreachable"
        Just syntaxDirectory ->
            pure syntaxDirectory
