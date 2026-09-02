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
            resolveFenceLanguage "src/Agent/TUI/Markdown.hs"
                `shouldBe` Just "haskell"
            resolveFenceLanguage "src\\Agent\\TUI\\Markdown.hs"
                `shouldBe` Just "haskell"
            resolveFenceLanguage "1:8:flake.nix"
                `shouldBe` Just "nix"
            resolveFenceLanguage "1:8:Dockerfile"
                `shouldBe` Just "dockerfile"
            resolveFenceLanguage "asp.net"
                `shouldBe` Just "asp.net"

        it "keeps unknown languages available for a recoverable lookup failure" do
            resolveFenceLanguage "made-up-language"
                `shouldBe` Just "made-up-language"

        it "treats plain text aliases as explicitly unhighlighted" do
            map resolveFenceLanguage ["text", "txt", "plain", "plaintext"]
                `shouldBe` replicate 4 Nothing

    describe "file path language resolution" do
        it "uses the complete path when filenames contain whitespace" do
            resolvePathLanguage "Main module.hs"
                `shouldBe` Just "haskell"
            resolvePathLanguage "src/my module.py"
                `shouldBe` Just "python"
            resolvePathLanguage "web\\typed module.ts"
                `shouldBe` Just "typescript"

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

    it "loads only a requested syntax and its include dependencies" do
        syntaxDirectory <- sourceSyntaxDirectory
        emptyHighlighter <-
            requireLoaded =<< newSyntaxHighlighterFrom syntaxDirectory
        highlightCode emptyHighlighter "haskell" "main = pure ()"
            `shouldSatisfy` isLeft
        haskellHighlighter <-
            requireLoaded
                =<< loadSyntaxLanguage emptyHighlighter "haskell"
        highlightCode haskellHighlighter "haskell" "main = pure ()"
            `shouldSatisfy` either (const False) (not . null)
        -- Haskell embeds JavaScript quasiquotes, so that dependency is loaded.
        highlightCode haskellHighlighter "javascript" "const answer = 42"
            `shouldSatisfy` either (const False) (not . null)
        -- An unrelated definition remains absent until it is requested.
        highlightCode haskellHighlighter "python" "print('hello')"
            `shouldSatisfy` isLeft

    it "resolves alternative syntax names without eagerly parsing all XML" do
        syntaxDirectory <- sourceSyntaxDirectory
        emptyHighlighter <-
            requireLoaded =<< newSyntaxHighlighterFrom syntaxDirectory
        csharpHighlighter <-
            requireLoaded
                =<< loadSyntaxLanguage emptyHighlighter "csharp"
        highlightCode csharpHighlighter "cs" "class Program {}"
            `shouldSatisfy` either (const False) (not . null)

    it "loads dependencies referenced by context switches and keyword lists" do
        syntaxDirectory <- sourceSyntaxDirectory
        emptyHighlighter <-
            requireLoaded =<< newSyntaxHighlighterFrom syntaxDirectory
        dockerHighlighter <-
            requireLoaded
                =<< loadSyntaxLanguage emptyHighlighter "dockerfile"
        highlightCode dockerHighlighter "bash" "printf '%s\\n' hello"
            `shouldSatisfy` either (const False) (not . null)
        groovyHighlighter <-
            requireLoaded
                =<< loadSyntaxLanguage dockerHighlighter "groovy"
        highlightCode groovyHighlighter "java" "class Main {}"
            `shouldSatisfy` either (const False) (not . null)

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
    requireLoaded =<< loadSyntaxHighlighterFrom syntaxDirectory

requireLoaded :: Either Text SyntaxHighlighter -> IO SyntaxHighlighter
requireLoaded = \case
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
