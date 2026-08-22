# agent-claude-code

`agent-claude-code` adapts an authenticated Claude Code process to the
harness's provider-neutral `Backend`.

The package keeps one `claude -p` subprocess alive and communicates through
Claude Code's Agent-SDK-compatible bidirectional `stream-json` protocol. Each
user message is written as one JSONL record; assistant messages, tool events,
completion, usage, and the session UUID are read from structured stdout. It
does not scrape terminal rendering or tail Claude's local transcript files.
Visible records are buffered until the matching result so Claude's
refusal-fallback retractions can be applied before anything reaches the
append-only harness renderers.

Anthropic's [June 15, 2026 subscription-policy
update](https://support.claude.com/en/articles/15036540-use-the-claude-agent-sdk-with-your-claude-plan)
says that Claude Agent SDK, `claude -p`, and third-party app usage currently
draw from Claude subscription usage limits. That policy may change, so callers
should consult the linked notice for current terms.

Authentication is accepted only when `claude auth status --json` reports:

- `loggedIn: true`
- `authMethod: "claude.ai"`
- `apiProvider: "firstParty"`
- a non-empty `subscriptionType`

Anthropic API-key and third-party-provider routing environment variables are
removed from both the authentication probe and the child session environment.
No credential file is read by this package.

Thinking blocks are ignored and never surfaced. Claude Code executes its own
tools; tool records are emitted only as display events and are never returned
to the harness for dispatch.

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
    history <- newIORef []
    let options =
            (defaultClaudeCodeOptions auth.executable "/path/to/project")
                { permission = ClaudeCodeBypass }
    withClaudeCodeBackend options Nothing getParams history \backend ->
        -- Install `backend` in Agent.Loop.
        pure ()
```

The long-lived backend reuses the same process across turns and restarts with
`--resume` when the selected model or effort changes. Its response id is the
Claude session UUID, allowing the surrounding harness to persist and resume the
subscription-backed session.
