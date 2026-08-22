module Agent.CLI.ArtifactSpec (spec) where

import Agent.CLI.Artifact
import Test.Hspec

spec :: Spec
spec = do
    describe "fencedCodeBlock" do
        it "selects numbered Markdown fences" do
            let text = "before\n```haskell\nmain = pure ()\n```\nafter\n```sh\necho ok\n```\n"
            fencedCodeBlock 1 text `shouldBe` Just "main = pure ()\n"
            fencedCodeBlock 2 text `shouldBe` Just "echo ok\n"
            fencedCodeBlock 3 text `shouldBe` Nothing

        it "matches tilde fences shown by the fullscreen renderer" do
            let text = "~~~bash\nprintf 'copy me\\n'\n~~~\n"
            fencedCodeBlock 1 text `shouldBe` Just "printf 'copy me\\n'\n"

        it "matches marker type and requires a sufficiently long closer" do
            let text =
                    "````haskell\n"
                        <> "```\n"
                        <> "main = pure ()\n"
                        <> "~~~\n"
                        <> "`````\n"
                        <> "~~~bash\n"
                        <> "echo ok\n"
                        <> "~~~\n"
            fencedCodeBlock 1 text
                `shouldBe` Just "```\nmain = pure ()\n~~~\n"
            fencedCodeBlock 2 text `shouldBe` Just "echo ok\n"

        it "includes unterminated fences with their exact body" do
            fencedCodeBlock 1 "before\n```text\nno final newline"
                `shouldBe` Just "no final newline"

        it "numbers mixed fence types in source order" do
            let text =
                    "~~~a\none\n~~~\n"
                        <> "````b\ntwo\n````\n"
                        <> "```c\nthree\n```\n"
            map (`fencedCodeBlock` text) [1, 2, 3, 4]
                `shouldBe`
                    [ Just "one\n"
                    , Just "two\n"
                    , Just "three\n"
                    , Nothing
                    ]

        it "copies deindented fenced code nested under a list item" do
            let text =
                    "- Changed from:\n\
                    \    ```text\n\
                    \    -N -M8G -A64m\n\
                    \    ```\n"
            fencedCodeBlock 1 text `shouldBe` Just "-N -M8G -A64m\n"

    describe "lastDiffBlock" do
        it "selects the last diff-like fence" do
            let text = "```diff\n-old\n+new\n```\n```patch\n-a\n+b\n```\n"
            lastDiffBlock text `shouldBe` Just "-a\n+b\n"
