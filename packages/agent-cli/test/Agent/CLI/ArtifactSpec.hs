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

    describe "lastDiffBlock" do
        it "selects the last diff-like fence" do
            let text = "```diff\n-old\n+new\n```\n```patch\n-a\n+b\n```\n"
            lastDiffBlock text `shouldBe` Just "-a\n+b\n"
