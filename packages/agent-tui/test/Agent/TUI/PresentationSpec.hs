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
        toolDetail
            (functionToolCall
                "ask"
                "ask_user_question"
                "{\"questions\":[{\"question\":\"Choose a backend\",\"options\":[]}]}")
            `shouldBe` "Choose a backend"
        summarizeToolCall
            (functionToolCall
                "wait"
                "wait_commands_or_subagents"
                "{\"task_ids\":[\"t1\"]}")
            `shouldBe` "Waited"
        summarizeToolCall
            (functionToolCall
                "secret"
                "ask_secret"
                "{\"prompt\":\"Enter token\",\"purpose\":\"Configure Telegram\"}")
            `shouldBe` "Requested secret Configure Telegram"

    it "separates GHCi expressions from their compact retained heading" do
        let call =
                functionToolCall
                    "ghci"
                    "run_ghci"
                    "{\"expression\":\"do { putStrLn \\\"one\\\"; putStrLn \\\"two\\\" }\"}"
        toolCallTitle call `shouldBe` "$ ghci"
        toolCallInput call
            `shouldBe` "do { putStrLn \"one\"; putStrLn \"two\" }"
        summarizeToolCall call
            `shouldBe` "$ do { putStrLn \"one\"; putStrLn \"two\" }"
        permissionToolCallPrompt call
            `shouldBe`
                "Evaluate this Haskell code in GHCi?\n\n\
                \do { putStrLn \"one\"; putStrLn \"two\" }"

    it "shows complete multiline code and shell commands for permission" do
        permissionToolCallPrompt
            (functionToolCall
                "ghci"
                "run_ghci"
                "{\"expression\":\"do\\n  putStrLn \\\"one\\\"\\n  putStrLn \\\"two\\\"\"}")
            `shouldBe`
                "Evaluate this Haskell code in GHCi?\n\n\
                \do\n  putStrLn \"one\"\n  putStrLn \"two\""
        permissionToolCallPrompt
            (functionToolCall
                "shell"
                "shell_command"
                "{\"command\":\"git status --short\\ngit diff --check\"}")
            `shouldBe`
                "Run this shell command?\n\ngit status --short\ngit diff --check"
        permissionToolCallPrompt
            (functionToolCall
                "shell"
                "shell_command"
                "{\"command\":\"printf '\\u001b]0;owned\\u0007'\"}")
            `shouldBe`
                "Run this shell command?\n\nprintf '␛]0;owned␇'"

    it "falls back to the safe prompt text for ask_secret detail" do
        toolDetail
            (functionToolCall
                "secret"
                "ask_secret"
                "{\"prompt\":\"Enter API token\"}")
            `shouldBe` "Enter API token"

    it "renders learned-skill mutations with scope, slug, and approval intent" do
        let create = functionToolCall
                "skill"
                "skill_create"
                "{\"scope\":\"repository\",\"slug\":\"postgres-sessions\"}"
        summarizeToolCall create
            `shouldBe` "Learned repository/postgres-sessions"
        permissionToolCallPrompt create
            `shouldBe`
                "Create learned skill repository/postgres-sessions?"
        formatToolOutput create
            "{\"status\":\"applied\",\"skill\":{\"scope\":\"repository\",\
            \\"slug\":\"postgres-sessions\",\"revision\":2,\
            \\"activation\":\"relevant\"}}"
            `shouldBe`
                "repository/postgres-sessions · revision 2 · relevant"

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
