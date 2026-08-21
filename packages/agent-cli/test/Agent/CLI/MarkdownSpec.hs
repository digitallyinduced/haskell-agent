module Agent.CLI.MarkdownSpec (spec) where

import Agent.CLI.Markdown (renderMarkdown)
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
            length body `shouldBe` 2
            Text.length (head body) `shouldBe` Text.length (body !! 1)

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

        it "restores the agent wash after inline spans" do
            let out = renderMarkdown True "see `file.txt` now"
            -- Nested Reset must re-open Solarized base03 so line painting sticks.
            out `shouldSatisfy` Text.isInfixOf "\ESC[0;48;2;0;43;54m"
