module Agent.CLI.Command.Catalog
    ( slashCommands
    ) where

import Agent.CLI.Command.Types (SlashCommand(..))
import Agent.Dialect (DialectId(..))

slashCommands :: [SlashCommand]
slashCommands =
    [ cmd "help" [] "/help [NAME]" "List slash commands, or describe one" True
    , cmd "init" [] "/init" "Create an AGENTS.md contributor guide" False
    , cmd "review" [] "/review [INSTRUCTIONS]" "Review current changes and find issues" True
    , cmd "diff" [] "/diff" "Show Git diff, including untracked files" False
    , cmd "fork" [] "/fork [--worktree|--no-worktree] [DIRECTIVE]" "Fork the current chat into a peer session" True
    , cmd "export" [] "/export [PATH]" "Export the conversation as Markdown" True
    , cmd "history" [] "/history" "Search prompt history and reuse a prompt" False
    , cmd "find" [] "/find [TEXT]" "Search this conversation in a pager" True
    , cmd "permissions" [] "/permissions" "Choose the tool approval policy" False
    , cmd "model" ["m"] "/model [NAME]" "Open the model picker, or set a model" True
    , cmd "effort" [] "/effort [none|low|medium|high|xhigh|max]" "Show or set reasoning effort" True
    , codexCmd "fast" [] "/fast" "Toggle the Fast service tier" False
    , cmd "plan" [] "/plan [description]" "Enter plan mode (or Shift+Tab)" True
    , cmd "view-plan" ["show-plan", "plan-view"] "/view-plan" "Show the saved session plan" False
    , cmd "queue" [] "/queue" "Show prompts waiting in the input queue" False
    , cmd "transcript" ["log"] "/transcript" "Open the session transcript in a pager" False
    , cmd "edit-prompt" [] "/edit-prompt" "Edit a prompt draft without submitting it" False
    , cmd "context" [] "/context" "Show context-window usage and estimates" False
    , cmd "btw" [] "/btw <QUESTION>" "Ask a side question without changing the conversation" True
    , cmd "meta" ["configure"] "/meta <REQUEST>" "Configure the harness without changing the conversation" True
    , cmd "recap" ["summarize"] "/recap" "Summarize the session so far" False
    , cmd "retry" [] "/retry" "Retry the last failed turn exactly" False
    , cmd "session" [] "/session" "Print the current session id" False
    , cmd "session-info" ["status", "info"] "/session-info" "Show session details (model, tools, and context usage)" False
    , cmd "desktop" [] "/desktop" "Open this conversation in the Haskell Agent desktop app" False
    , cmd "afk" [] "/afk [HOST:PATH]" "Move this session into tmux, locally or over SSH" True
    , cmd "worktree" [] "/worktree" "Start a fresh session in a new git worktree" False
    , cmd "rename" ["title"] "/rename <TITLE>|--auto" "Rename the current session, or restore automatic titles" True
    , cmd "login" ["accounts"] "/login" "Manage provider credentials and usage" False
    , cmd "resume" [] "/resume [ID]" "Pick a session to resume, or resume ID" True
    , cmd "search" [] "/search <QUERY>" "Search past conversations and resume a match" True
    , cmd "compact" [] "/compact [FOCUS]" "Summarize history to free context" True
    , cmd "clear" [] "/clear" "Reset the live conversation (same session id)" False
    , cmd "new" [] "/new" "Start a fresh persisted session id" False
    , cmd "usage" [] "/usage" "Show usage, pacing, and reset times for connected accounts" False
    , cmd "reload-auth" [] "/reload-auth" "Re-read provider credentials" False
    , cmd "paste" [] "/paste [--send] [TEXT]" "Attach a clipboard image (Cmd+V / Ctrl+V) and preview it in the terminal" True
    , cmd "attachments" [] "/attachments" "List queued clipboard images" False
    , cmd "clear-attachments" [] "/clear-attachments" "Drop queued clipboard images" False
    , cmd "copy" ["copy-last"] "/copy" "Copy the last assistant response" False
    , cmd "copy-code" [] "/copy-code [N]" "Copy fenced code block N from the last response" True
    , cmd "copy-diff" [] "/copy-diff" "Copy the last diff block" False
    , cmd "copy-path" [] "/copy-path" "Copy the active worktree path" False
    , cmd "copy-session" [] "/copy-session" "Copy the current session id" False
    , cmd "terminal" ["ghostty"] "/terminal" "Show detected terminal capabilities" False
    , cmd "agents" ["a"] "/agents [limit [N]]" "Browse agents, or show/set the concurrent subagent cap" True
    , cmd "mcp" ["mcps"] "/mcp [prompt <server> <name> [key=value…]]" "Manage MCP servers or run a server prompt" False
    , grokToolCmd "scheduler_create" "loop" [] "/loop [interval] <prompt>" "Run a prompt on a recurring interval" True
    , grokToolCmd "update_goal" "goal" [] "/goal <objective> [--budget N] | status | pause | resume | clear" "Set, manage, or check an autonomous goal" True
    , grokToolCmd "workflow" "workflow" [] "/workflow runs | <name> [input]" "Launch a named workflow or list workflow runs" True
    , grokToolCmd "workflow" "deep-research" [] "/deep-research <query>" "Run bounded background research, cross-check evidence, and write a cited report" True
    , cmd "skills" [] "/skills [reload]" "List discovered skills or reload them from disk" True
    , cmd "shell" [] "/shell [ghci|bash|both|none]" "Show or select the allowed shell tools" True
    , cmd "codemod" ["code-mode"] "/codemod" "Enable JavaScript code mode for this session" False
    , cmd "always-approve" ["yolo"] "/always-approve" "Toggle project auto-approve (or Shift+Tab)" False
    , cmd "quit" ["exit"] "/quit" "Exit the current session" False
    ]
  where
    cmd name aliases usage summary takesArguments =
        SlashCommand
            { slashName = name
            , slashAliases = aliases
            , slashUsage = usage
            , slashSummary = summary
            , slashTakesArguments = takesArguments
            , slashDialects = Nothing
            , slashRequiredTools = []
            }
    grokToolCmd requiredTool name aliases usage summary takesArguments =
        (cmd name aliases usage summary takesArguments)
            { slashDialects = Just [GrokBuildDialect]
            , slashRequiredTools = [requiredTool]
            }
    codexCmd name aliases usage summary takesArguments =
        (cmd name aliases usage summary takesArguments)
            { slashDialects = Just [CodexDialect] }
