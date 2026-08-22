# agent-claude-code

`agent-claude-code` adapts an authenticated, interactive Claude Code process to
the harness's provider-neutral `Backend`.

The package deliberately does **not** use `claude -p`. It launches the normal
interactive CLI behind a pseudo-terminal, using Claude Code's screen-reader
renderer so startup and turn boundaries can be synchronized without scraping
assistant output from the terminal. Claude Code can therefore use an existing
Claude subscription. Authentication is accepted only when
`claude auth status --json` reports:

- `loggedIn: true`
- `authMethod: "claude.ai"`
- `apiProvider: "firstParty"`
- a non-empty `subscriptionType`

Anthropic API-key and third-party-provider routing environment variables are
removed from both the authentication probe and the child session environment.
No credential file is read by this package.

Assistant text, tool activity, completion, and usage are derived from Claude
Code's JSONL transcript under `~/.claude/projects`. Thinking blocks are ignored
and never surfaced. Claude Code executes its own tools; tool records are emitted
only as display events and are never returned to the harness for dispatch.

Safe mode is enabled by default. It preserves auth, model selection,
permissions, and built-in tools while disabling Claude-specific project and
user customizations such as `CLAUDE.md`, skills, plugins, hooks, MCP servers,
custom commands, and custom agents. Callers can still supply harness
instructions through `ResponseCreateParams`.

```haskell
import Agent.ClaudeCode
import Data.IORef

main = do
    Right auth <- loadClaudeCodeAuth
    transcript <- newIORef []
    let options =
            (defaultClaudeCodeOptions auth.executable "/path/to/project")
                { permission = ClaudeCodeBypass }
    withClaudeCodeBackend options Nothing getParams transcript \backend ->
        -- Install `backend` in Agent.Loop.
        pure ()
```

The long-lived backend restarts and resumes its Claude session when the selected
model or effort changes. Its response id is the Claude session UUID, allowing
the surrounding harness to persist and resume the subscription-backed session.
