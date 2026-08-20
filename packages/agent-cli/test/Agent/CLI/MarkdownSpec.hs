module Agent.CLI.MarkdownSpec (spec) where

import Agent.CLI.Markdown (renderMarkdown)
import qualified Data.Text as Text
import Test.Hspec

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

        it "keeps fence body at normal intensity and drops fence markers" do
            let out = renderMarkdown True "```haskell\nmain = pure ()\n```"
            out `shouldSatisfy` Text.isInfixOf "main = pure ()"
            out `shouldSatisfy` (not . Text.isInfixOf "```")
            out `shouldSatisfy` Text.isInfixOf "haskell"

        it "supports tilde fences" do
            let out = renderMarkdown True "~~~\nbody\n~~~"
            out `shouldBe` "body"

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
