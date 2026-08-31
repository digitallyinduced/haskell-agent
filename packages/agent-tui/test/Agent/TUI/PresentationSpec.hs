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
    it "shows filesystem paths relative to the workspace" do
        let workspace =
                "/Users/marc/.haskell-agent/worktrees/haskell-agent/wt"
            absolute = workspace <> "/nix/modules/telegram.nix"
            edit =
                functionToolCall
                    "edit"
                    "search_replace"
                    ("{\"file_path\":\"" <> absolute <> "\"}")
            listing =
                functionToolCall
                    "ls"
                    "list_dir"
                    ("{\"target_directory\":\"" <> workspace <> "\"}")
            readCall =
                functionToolCall
                    "read"
                    "read_file"
                    ("{\"target_file\":\"" <> absolute <> "\"}")
        workspaceRelativeDisplayPath workspace absolute
            `shouldBe` "nix/modules/telegram.nix"
        workspaceRelativeDisplayPath workspace (workspace <> "/")
            `shouldBe` "."
        workspaceRelativeDisplayPath workspace "src/Main.hs"
            `shouldBe` "src/Main.hs"
        workspaceRelativeDisplayPath workspace "/tmp/outside.hs"
            `shouldBe` "/tmp/outside.hs"
        workspaceRelativeDisplayPath
            workspace
            (workspace <> "/./nix/../nix/modules/telegram.nix")
            `shouldBe` "nix/modules/telegram.nix"
        workspaceRelativeDisplayPath workspace "src/../nix/foo.nix"
            `shouldBe` "nix/foo.nix"
        workspaceRelativeDisplayPath
            "/worktree"
            "/worktree-other/Foo.hs"
            `shouldBe` "/worktree-other/Foo.hs"
        summarizeToolCallRelative workspace edit
            `shouldBe` "Edited nix/modules/telegram.nix"
        summarizeToolCallRelative workspace listing
            `shouldBe` "Listed ."
        toolCallTitleRelative workspace readCall
            `shouldBe` "Read nix/modules/telegram.nix"
        permissionToolCallPromptRelative workspace edit
            `shouldBe` "Allow Edited nix/modules/telegram.nix?"
        formatToolOutputRelative
            workspace
            edit
            ("The file " <> absolute <> " has been updated successfully.")
            `shouldBe`
                "The file nix/modules/telegram.nix has been updated successfully."
        formatToolOutputRelative workspace listing
            ("Directory listing for " <> workspace <> ":\nFoo.hs")
            `shouldBe` "Directory listing for .:\nFoo.hs"
        formatToolOutputRelative workspace readCall absolute
            `shouldBe` absolute

    it "keeps computer-call secrets out of approval chrome" do
        let call =
                functionToolCall
                    "computer-call"
                    "computer"
                    "{\"actions\":[{\"type\":\"click\",\"x\":10,\"y\":20},{\"type\":\"type\",\"text\":\"password\"}]}"
        summarizeToolCall call
            `shouldBe` "Control computer computer action"
        permissionToolCallPrompt call
            `shouldSatisfy` (not . Text.isInfixOf "password")

    it "summarizes Claude Code built-in tools with host chrome" do
        summarizeToolCall
            (functionToolCall "b" "Bash"
                "{\"command\":\"git status\\nls\",\"description\":\"Show status\"}")
            `shouldBe` "$ git status"
        summarizeToolCall
            (functionToolCall "r" "Read" "{\"file_path\":\"src/Main.hs\"}")
            `shouldBe` "Read src/Main.hs"
        summarizeToolCall
            (functionToolCall "e" "Edit"
                "{\"file_path\":\"src/Main.hs\",\"old_string\":\"a\",\"new_string\":\"b\"}")
            `shouldBe` "Edited src/Main.hs"
        summarizeToolCall
            (functionToolCall "w" "Write"
                "{\"file_path\":\"src/New.hs\",\"content\":\"main = pure ()\"}")
            `shouldBe` "Wrote src/New.hs"
        summarizeToolCall
            (functionToolCall "g" "Glob" "{\"pattern\":\"**/*.hs\"}")
            `shouldBe` "Globbed **/*.hs"
        summarizeToolCall
            (functionToolCall "s" "Grep" "{\"pattern\":\"TODO\",\"path\":\"src\"}")
            `shouldBe` "Searched TODO"
        summarizeToolCall
            (functionToolCall "f" "WebFetch"
                "{\"url\":\"https://example.com\",\"prompt\":\"summarize\"}")
            `shouldBe` "Fetched https://example.com"
        summarizeToolCall
            (functionToolCall "t" "ToolSearch" "{\"query\":\"select:WebFetch\"}")
            `shouldBe` "Searched tools select:WebFetch"
        summarizeToolCall
            (functionToolCall "a" "Agent"
                "{\"description\":\"Find flaky tests\",\"prompt\":\"...\"}")
            `shouldBe` "Spawned agent Find flaky tests"
        summarizeToolCall
            (functionToolCall "m" "mcp__playwright__browser_click" "{}")
            `shouldBe` "playwright: browser_click"
        toolPathArgument
            (functionToolCall "r" "Read" "{\"file_path\":\"/tmp/a.hs\"}")
            `shouldBe` Just "/tmp/a.hs"
        formatToolDiffRelative ""
            (functionToolCall "w" "Write"
                "{\"file_path\":\"src/New.hs\",\"content\":\"main = pure ()\\n\"}")
            `shouldBe` "  write src/New.hs\n  +main = pure ()"
        todoListFromToolArguments
            "{\"todos\":[{\"content\":\"Find repos\",\"status\":\"in_progress\",\
            \\"activeForm\":\"Finding repos\"},{\"content\":\"Fix\",\"status\":\"pending\"}]}"
            `shouldBe`
                Just
                    [ TodoDisplayLine TodoDisplayInProgress "Find repos"
                    , TodoDisplayLine TodoDisplayPending "Fix"
                    ]
        todoListFromToolArguments "{\"todos\":[]}" `shouldBe` Just []
        todoListFromToolArguments "Todos have been modified" `shouldBe` Nothing

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

    it "renders exec source and hides successful protocol boilerplate" do
        let source = "const result = await tools.grep({pattern: \"needle\"});\ntext(result);"
            call = customToolCall "exec" "exec" source
        toolCallTitle call `shouldBe` "$ exec"
        toolCallInput call `shouldBe` source
        formatToolOutput call
            "Script completed\nWall time 0.1 seconds\nOutput:\nmatch"
            `shouldBe` "match"
        formatToolOutput call
            "Script completed\nWall time 0.0 seconds\nOutput:\n"
            `shouldBe` ""
        formatToolOutput call
            "Script failed\nWall time 0.1 seconds\nOutput:\nScript error:\nboom"
            `shouldBe`
                "Script failed\nWall time 0.1 seconds\nOutput:\nScript error:\nboom"

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
        summarizeToolCall
            (functionToolCall
                "view"
                "view_skill"
                "{\"scope\":\"repository\",\"name\":\"postgres-sessions\"}")
            `shouldBe` "Viewed skill repository/postgres-sessions"
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

    it "formats compact multi-file apply_patch diffs" do
        let workspace = "/workspace"
            patch =
                Text.unlines
                    [ "*** Begin Patch"
                    , "*** Update File: /workspace/src/A.hs"
                    , "@@"
                    , " *** Add File: this-is-context"
                    , "-old"
                    , "+new"
                    , "*** Add File: /workspace/src/B.hs"
                    , "+one"
                    , "*** Update File: /workspace/src/Old.hs"
                    , "*** Move to: /workspace/src/New.hs"
                    , "@@"
                    , "-before"
                    , "+after"
                    , "*** Delete File: /workspace/src/C.hs"
                    , "*** End Patch"
                    ]
            call = customToolCall "patch" "apply_patch" patch
            parsed = parseApplyPatchDiffs patch
        map (.diffAction) parsed
            `shouldBe`
                [ Nothing
                , Just SearchReplaceCreate
                , Just (SearchReplaceMove "/workspace/src/New.hs")
                , Just SearchReplaceDelete
                ]
        formatToolDiffRelative workspace call
            `shouldBe`
                "  -old\n\
                \  +new\n\
                \  create src/B.hs\n\
                \  +one\n\
                \  move src/Old.hs → src/New.hs\n\
                \  -before\n\
                \  +after\n\
                \  delete src/C.hs"

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
        let worktreeSpawn = functionToolCall
                "spawn" "collaboration.spawn_agent_in_worktree"
                "{\"task_name\":\"worker\",\"message\":\"task\"}"
        summarizeToolCall worktreeSpawn
            `shouldBe` "Spawned worktree agent worker"
        formatToolOutput worktreeSpawn
            "{\"task_name\":\"/root/worker\",\"worktree\":\"/tmp/worker\"}"
            `shouldBe` "Agent: /root/worker"

    it "summarizes conversation search calls while preserving text output" do
        let call = functionToolCall
                "search"
                "conversation_search"
                "{\"query\":\"postgres memory\",\"limit\":5}"
        summarizeToolCall call
            `shouldBe` "Searched conversations postgres memory"
        formatToolOutput call "Match 1\nUser:\n  A question"
            `shouldBe` "Match 1\nUser:\n  A question"

    it "summarizes persisted agent-session conversation tools" do
        let create = functionToolCall
                "create" "create_agent_session"
                "{\"message\":\"work\",\"title\":\"research\"}"
            readSession = functionToolCall
                "read" "read_agent_session"
                "{\"session_id\":\"session-1\",\"limit\":20}"
            message = functionToolCall
                "message" "send_agent_session_message"
                "{\"session_id\":\"session-1\",\"message\":\"continue\"}"
        summarizeToolCall create `shouldBe` "Created agent session research"
        summarizeToolCall readSession `shouldBe` "Read agent session session-1"
        summarizeToolCall message `shouldBe`
            "Messaged agent session session-1"

    it "keeps todo_write chrome on the wire name and renders checklist glyphs" do
        let call = functionToolCall
                "todo"
                "todo_write"
                "{\"todos\":[{\"id\":\"1\",\"content\":\"Find and clone repos\",\"status\":\"completed\"}]}"
        toolCallTitle call `shouldBe` "todo_write"
        todoCallPreview call `shouldBe` "Find and clone repos"
        formatToolOutput call
            "- [completed] 1: Find and clone repos\n\
            \- [in_progress] 2: Investigate Grok Build\n\
            \- [pending] 3: Investigate Codex\n\
            \- [cancelled] 4: Skip leftover work"
            `shouldBe`
                "✓ Find and clone repos\n\
                \▶ Investigate Grok Build\n\
                \□ Investigate Codex\n\
                \✗ Skip leftover work"

    it "formats Codex update_plan output as the same checklist" do
        let call = functionToolCall "plan" "update_plan" "{\"plan\":[]}"
        toolCallTitle call `shouldBe` "update_plan"
        formatToolOutput call
            "Plan updated:\n- [completed] Clone the repo\n- [pending] Open a PR"
            `shouldBe`
                "✓ Clone the repo\n\
                \□ Open a PR"

    it "keeps a live todo panel while work remains and truncates overflow" do
        let todos =
                [ TodoDisplayLine TodoDisplayCompleted "Find repos"
                , TodoDisplayLine TodoDisplayInProgress "Investigate Grok"
                , TodoDisplayLine TodoDisplayPending "Investigate Codex"
                ]
        todoListHasOpenWork todos `shouldBe` True
        todoListHasInProgress todos `shouldBe` True
        todoListHasOpenWork [TodoDisplayLine TodoDisplayPending "later"]
            `shouldBe` True
        todoListHasInProgress [TodoDisplayLine TodoDisplayPending "later"]
            `shouldBe` False
        todoListHasOpenWork [TodoDisplayLine TodoDisplayCompleted "done"]
            `shouldBe` False
        liveTodoPanelLines 2 todos
            `shouldBe`
                [ "✓ Find repos"
                , "… +2 more"
                ]
        todoListFromToolOutput "ok" `shouldBe` Nothing
        todoListFromToolOutput "No tasks currently tracked."
            `shouldBe` Just []
