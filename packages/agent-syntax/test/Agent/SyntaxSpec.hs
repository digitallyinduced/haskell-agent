module Agent.SyntaxSpec (spec) where

import Agent.Syntax
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Environment (lookupEnv)
import Test.Hspec

spec :: Spec
spec = describe "syntax highlighting" do
    describe "fence language resolution" do
        it "normalizes common aliases and case" do
            resolveFenceLanguage "HS" `shouldBe` Just "haskell"
            resolveFenceLanguage "py linenums" `shouldBe` Just "python"
            resolveFenceLanguage "c++" `shouldBe` Just "cpp"
            resolveFenceLanguage "PATCH" `shouldBe` Just "diff"

        it "resolves Grok line-range file paths by extension" do
            resolveFenceLanguage "12:40:src/Agent/TUI/Markdown.hs"
                `shouldBe` Just "haskell"
            resolveFenceLanguage "1:8:flake.nix"
                `shouldBe` Just "nix"
            resolveFenceLanguage "1:8:Dockerfile"
                `shouldBe` Just "dockerfile"

        it "keeps unknown languages available for a recoverable lookup failure" do
            resolveFenceLanguage "made-up-language"
                `shouldBe` Just "made-up-language"

        it "treats plain text aliases as explicitly unhighlighted" do
            map resolveFenceLanguage ["text", "txt", "plain", "plaintext"]
                `shouldBe` replicate 4 Nothing

    it "loads the packaged grammar set and highlights representative languages" do
        highlighter <- requireHighlighter
        mapM_
            (\(language, source) ->
                highlightCode highlighter language source
                    `shouldSatisfy` either (const False) (not . null))
            [ ("haskell", "main = putStrLn \"hello\"")
            , ("bash", "printf '%s\\n' hello")
            , ("c", "int main(void) { return 0; }")
            , ("cpp", "auto answer = 42;")
            , ("cs", "class Program { static void Main() {} }")
            , ("css", "body { color: red; }")
            , ("dockerfile", "FROM scratch")
            , ("go", "package main\nfunc main() {}")
            , ("html", "<main>Hello</main>")
            , ("java", "class Main {}")
            , ("python", "print(\"hello\")")
            , ("javascript", "const answer = 42;")
            , ("typescript", "const answer: number = 42;")
            , ("json", "{\"answer\": 42}")
            , ("kotlin", "fun main() = println(\"hello\")")
            , ("lua", "local answer = 42")
            , ("markdown", "# Hello")
            , ("nix", "{ pkgs, ... }: pkgs.hello")
            , ("rust", "fn main() { println!(\"hello\"); }")
            , ("diff", "-before\n+after")
            , ("sql", "select answer from results;")
            , ("swift", "let answer = 42")
            , ("toml", "answer = 42")
            , ("xml", "<answer>42</answer>")
            , ("yml", "answer: 42")
            , ("zig", "pub fn main() void {}")
            ]

    it "preserves source text including Unicode, blank lines, and a trailing newline" do
        highlighter <- requireHighlighter
        let source = "{- λ\n\ncomment\n-}\nmain = putStrLn \"hello\"\n"
        highlighted <- requireHighlight (highlightCode highlighter "haskell" source)
        reconstruct highlighted `shouldBe` source

    it "retains multiline lexer state across blank lines" do
        highlighter <- requireHighlighter
        highlighted <-
            requireHighlight $
                highlightCode
                    highlighter
                    "haskell"
                    "{-\n\ninside comment\n-}\nmain = pure ()"
        let lineInsideComment = highlighted !! 2
        lineInsideComment `shouldSatisfy` (not . null)
        map (.syntaxClass) lineInsideComment
            `shouldSatisfy` all (== SyntaxComment)

    it "returns recoverable failures for unknown and explicit plain languages" do
        highlighter <- requireHighlighter
        highlightCode highlighter "not-a-real-language" "hello"
            `shouldSatisfy` isLeft
        highlightCode highlighter "text" "hello"
            `shouldSatisfy` isLeft

    it "refuses blocks above the byte and line limits" do
        highlighter <- requireHighlighter
        highlightCode highlighter "haskell" (Text.replicate (256 * 1024 + 1) "a")
            `shouldSatisfy` isLeft
        highlightCode highlighter "haskell" (Text.replicate 5000 "\n")
            `shouldSatisfy` isLeft

    it "counts UTF-8 bytes at the highlighting boundary" do
        highlighter <- requireHighlighter
        let prefix = "--"
            exactSource = prefix <> Text.replicate 131071 "é"
            oversizedSource = exactSource <> "é"
            byteLimitError = \case
                Left message ->
                    message == "Code block exceeds the syntax-highlighting byte limit"
                Right _ -> False
        highlightCode highlighter "haskell" exactSource
            `shouldSatisfy` (not . byteLimitError)
        highlightCode highlighter "haskell" oversizedSource
            `shouldSatisfy` byteLimitError

requireHighlighter :: IO SyntaxHighlighter
requireHighlighter = do
    syntaxDirectory <- sourceSyntaxDirectory
    loadSyntaxHighlighterFrom syntaxDirectory >>= \case
        Left message -> expectationFailure (Text.unpack message) >> fail "unreachable"
        Right highlighter -> pure highlighter

sourceSyntaxDirectory :: IO FilePath
sourceSyntaxDirectory =
    lookupEnv "AGENT_SYNTAX_DIR" >>= \case
        Nothing -> do
            expectationFailure
                "AGENT_SYNTAX_DIR is not set; run tests from nix develop"
            fail "unreachable"
        Just syntaxDirectory ->
            pure syntaxDirectory

requireHighlight
    :: Either Text [HighlightedLine]
    -> IO [HighlightedLine]
requireHighlight = \case
    Left message -> expectationFailure (Text.unpack message) >> fail "unreachable"
    Right highlighted -> pure highlighted

reconstruct :: [HighlightedLine] -> Text
reconstruct =
    Text.intercalate "\n"
        . map (Text.concat . map (.syntaxText))
