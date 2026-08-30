# agent-claude

`agent-claude` adapts an authenticated Claude Code process to the
harness's provider-neutral `Backend`. Process transport, typed messages,
session management, and query execution come from
[`claude-agent-sdk-haskell`](../claude-agent-sdk-haskell/README.md); this
package contains only subscription policy and `Agent.Loop` translation.

The SDK keeps one `claude -p` subprocess alive and communicates through Claude
Code's bidirectional `stream-json` protocol. This adapter validates
subscription authentication, turns SDK messages into harness display events,
and persists the Claude session UUID as the provider response ID. It does not
scrape terminal rendering or tail Claude's local transcript files.

Anthropic's [June 15, 2026 subscription-policy
update](https://support.claude.com/en/articles/15036540-use-the-claude-agent-sdk-with-your-claude-plan)
says that Claude Agent SDK, `claude -p`, and third-party app usage currently
draw from Claude subscription usage limits. Anthropic's current
[Agent SDK documentation](https://platform.claude.com/docs/en/agent-sdk/overview)
also says third-party developers need prior approval to offer Claude.ai login
or subscription rate limits in their products. Technical availability does
not replace that approval requirement; consult the linked documents for
current terms before distributing this integration.

Authentication is accepted only when `claude auth status --json` reports:

- `loggedIn: true`
- `authMethod: "claude.ai"`
- `apiProvider: "firstParty"`
- a non-empty `subscriptionType`

Anthropic API-key and third-party-provider routing environment variables are
removed from both the authentication probe and the child session environment.
No credential file is read by this package.

An explicit gateway mode keeps the Claude Code subprocess and its tools local
while sending model traffic through an Anthropic-compatible gateway. Set both
variables (or neither):

```sh
export HASKELL_AGENT_GATEWAY_URL=https://gateway.example
export HASKELL_AGENT_GATEWAY_TOKEN='...'
```

Gateway mode sanitizes all ambient Anthropic/Claude provider overrides and
injects only `ANTHROPIC_BASE_URL=$HASKELL_AGENT_GATEWAY_URL/anthropic` and the
gateway token into Claude Code. It does not require a local Claude login. The
token is redacted from `Show` and status output.

Thinking blocks are surfaced as reasoning progress events (and are never
copied into the persisted conversation prompt). Claude Code executes its
built-in tools; their records are emitted only as display events and are never
returned to the harness for dispatch. Interactive hosts may also install
permission callbacks and a synthetic SDK MCP server for complementary harness
tools. Those MCP calls are approved and dispatched by the host rather than
duplicating Claude's shell and filesystem tools.

Safe mode is enabled by default. It preserves auth, model selection,
permissions, and built-in tools while disabling Claude-specific project and
user customizations such as `CLAUDE.md`, skills, plugins, hooks, MCP servers,
custom commands, and custom agents. Callers can still supply harness
instructions through `ResponseCreateParams`.

```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Agent.Claude
import Agent.Responses.Types (defaultResponseCreateParams)
import Data.IORef

main = do
    Right auth <- loadClaudeCodeAuth
    history <- newIORef []
    let options =
            (defaultClaudeCodeOptions auth.executable "/path/to/project")
                { permission = ClaudeCodeBypass }
        getParams = pure defaultResponseCreateParams
    withClaudeCodeBackend options Nothing getParams history \backend ->
        -- Install `backend` in Agent.Loop.
        pure ()
```

The long-lived backend reuses the same process across turns and restarts with
`--resume` when the selected model or effort changes. Its response id is the
Claude session UUID, allowing the surrounding harness to persist and resume the
subscription-backed session. Each successful process turn is checkpointed
against the authoritative host snapshot; rollback, cancellation, or host-side
compaction invalidates the continuation and forces a fresh process so discarded
Claude context cannot leak into the next turn.
