module Agent.CLI.RenderSpec (spec) where

import Agent.CLI.Render
import Agent.Loop (LoopError(..), TurnOutput(..))
import Agent.ToolDispatch (customToolCall, functionToolCall)
import Test.Hspec

spec :: Spec
spec = do
    describe "summarizeToolCall" do
        it "includes JSON argument highlights" do
            summarizeToolCall (functionToolCall "c1" "read_file" "{\"target_file\":\"src/A.hs\"}")
                `shouldBe` "read_file src/A.hs"
            summarizeToolCall (functionToolCall "c2" "shell_command" "{\"command\":\"ls -l\"}")
                `shouldBe` "shell_command ls -l"
            summarizeToolCall (functionToolCall "c3" "run_terminal_cmd" "{\"command\":\"git status\"}")
                `shouldBe` "run_terminal_cmd git status"

        it "pulls the first path out of an apply_patch body" do
            let patch = "*** Begin Patch\n*** Update File: src/Foo.hs\n@@\n-a\n+b\n*** End Patch"
            summarizeToolCall (customToolCall "c4" "apply_patch" patch)
                `shouldBe` "apply_patch src/Foo.hs"

    describe "truncateToolOutput" do
        it "keeps the first line and marks empty output" do
            truncateToolOutput "Exit code: 0\nhello" `shouldBe` "Exit code: 0"
            truncateToolOutput "   " `shouldBe` "(empty)"

    describe "formatLoopError" do
        it "explains a max-turn stop" do
            formatLoopError (LoopMaxTurns TurnOutput
                { responseId = "r"
                , toolCalls = []
                , assistantText = Just "almost"
                })
                `shouldSatisfy` (/= "")
