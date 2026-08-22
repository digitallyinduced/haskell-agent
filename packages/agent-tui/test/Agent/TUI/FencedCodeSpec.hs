module Agent.TUI.FencedCodeSpec (spec) where

import Agent.TUI.FencedCode
import Test.Hspec

spec :: Spec
spec = describe "fencedBlocks" do
    it "returns prose and fences in source order for rendering" do
        let chunks =
                fenceChunks
                    "before\n```hs\none\n```\nbetween\n~~~sh\ntwo\n"
        case chunks of
            [ FenceText before
                , FenceBlock first
                , FenceText between
                , FenceBlock second
                ] -> do
                    before `shouldBe` "before\n"
                    first.fencedIndex `shouldBe` 1
                    first.fencedBody `shouldBe` "one\n"
                    first.fencedClosed `shouldBe` True
                    between `shouldBe` "between\n"
                    second.fencedIndex `shouldBe` 2
                    second.fencedBody `shouldBe` "two\n"
                    second.fencedClosed `shouldBe` False
            _ -> expectationFailure ("unexpected chunks: " <> show chunks)

    it "matches the opener character and accepts a longer closer" do
        let blocks =
                fencedBlocks
                    "````haskell\n```\nmain = pure ()\n`````\n~~~sh\necho ok\n~~~\n"
        map (.fencedInfo) blocks `shouldBe` ["haskell", "sh"]
        map (.fencedBody) blocks
            `shouldBe` ["```\nmain = pure ()\n", "echo ok\n"]
        map (.fencedClosed) blocks `shouldBe` [True, True]
        map (.fencedIndex) blocks `shouldBe` [1, 2]

    it "does not close a fence with a different marker or a shorter run" do
        case fencedBlocks "````\na\n~~~\nb\n```\nc\n" of
            [block] -> do
                block.fencedBody `shouldBe` "a\n~~~\nb\n```\nc\n"
                block.fencedClosed `shouldBe` False
            blocks ->
                expectationFailure
                    ("expected one block, got " <> show blocks)

    it "allows up to three leading spaces but not four" do
        let blocks =
                fencedBlocks
                    "    ```ignored\nx\n   ~~~ diff\n-old\n+new\n  ~~~   \n"
        map (.fencedInfo) blocks `shouldBe` ["diff"]
        map (.fencedBody) blocks `shouldBe` ["-old\n+new\n"]

    it "preserves an unterminated body and its final newline state" do
        case
            ( fencedBlocks "```text\none"
            , fencedBlocks "```text\none\n"
            ) of
            ([withoutNewline], [withNewline]) -> do
                withoutNewline.fencedBody `shouldBe` "one"
                withNewline.fencedBody `shouldBe` "one\n"
                withoutNewline.fencedClosed `shouldBe` False
                withNewline.fencedClosed `shouldBe` False
            blocks ->
                expectationFailure
                    ("expected one block in each input, got " <> show blocks)

    it "rejects backticks in a backtick fence info string" do
        fenceOpener "```lang`bad" `shouldBe` Nothing
