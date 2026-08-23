module Agent.TUI.PresentationSpec (spec) where

import Agent.TUI.Presentation
import Agent.ToolDispatch
    ( customToolCall
    , functionToolCall
    )
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "tool presentation" do
    it "extracts tool details from function and custom calls" do
        toolDetail
            (functionToolCall "read" "read_file"
                "{\"target_file\":\"src/Main.hs\"}")
            `shouldBe` "src/Main.hs"
        toolDetail
            (customToolCall "patch" "apply_patch"
                "*** Begin Patch\n*** Update File: src/Main.hs\n*** End Patch")
            `shouldBe` "src/Main.hs"
        summarizeToolCall
            (functionToolCall "continue" "write_stdin" "{\"session_id\":12}")
            `shouldBe` "Continued session 12"

    it "parses and truncates search-replace diffs once for all renderers" do
        let oldText = Text.intercalate "\\n"
                ["old" <> Text.pack (show n) | n <- [1 :: Int .. 15]]
            newText = Text.intercalate "\\n"
                ["new" <> Text.pack (show n) | n <- [1 :: Int .. 15]]
            arguments =
                "{\"file_path\":\"src/Main.hs\",\"old_string\":\""
                    <> oldText
                    <> "\",\"new_string\":\""
                    <> newText
                    <> "\"}"
            parsed = parseSearchReplaceDiff arguments
        parsed.diffPath `shouldBe` "src/Main.hs"
        parsed.diffAction `shouldBe` Nothing
        length parsed.diffLines `shouldBe` 20
        parsed.diffHiddenLines `shouldBe` 10

    it "formats structured collaboration output" do
        let call = functionToolCall
                "agents" "collaboration.list_agents" "{}"
        formatToolOutput call
            "{\"agents\":[{\"agent_name\":\"/root/reviewer\",\
            \\"agent_status\":\"running\"}]}"
            `shouldBe` "/root/reviewer · running"
        formatToolOutput call
            "{\"agents\":[\
            \{\"agent_name\":\" /root/worker \",\"agent_status\":\" idle \"},\
            \{\"agent_name\":\"/root/queued\"},\
            \{\"agent_name\":\" \",\"agent_status\":\"running\"},42]}"
            `shouldBe` "/root/worker · idle\n/root/queued"
        formatToolOutput call
            "{\"agents\":[{\"agent_status\":\"running\"},null]}"
            `shouldBe` "(no live agents)"
