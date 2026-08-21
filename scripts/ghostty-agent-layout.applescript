-- Create a Ghostty workspace for the harness.
--
-- Usage:
--   osascript scripts/ghostty-agent-layout.applescript /absolute/project/path
on run argv
    if (count of argv) is 0 then
        error "usage: ghostty-agent-layout.applescript PROJECT_DIR"
    end if

    set projectDir to item 1 of argv

    tell application "Ghostty"
        activate

        set cfg to new surface configuration
        set initial working directory of cfg to projectDir

        set win to new window with configuration cfg
        set paneAgent to terminal 1 of selected tab of win
        set paneShell to split paneAgent direction right with configuration cfg
        set paneGit to split paneShell direction down with configuration cfg
        set paneAgents to split paneAgent direction down with configuration cfg

        input text "agent-cli" to paneAgent
        send key "enter" to paneAgent

        input text "git status --short --branch" to paneGit
        send key "enter" to paneGit

        input text "printf '\\nSubagent snapshots are stored below session directories in ~/.haskell-agent/sessions/*/agents\\n\\n'" to paneAgents
        send key "enter" to paneAgents

        input text "printf '\\nShell pane ready in the agent worktree.\\n\\n'" to paneShell
        send key "enter" to paneShell

        focus paneAgent
    end tell
end run
