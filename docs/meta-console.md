# Meta Console

Meta Console is a private natural-language configuration surface for the
running agent.

## Open it

- Press `Cmd+K`. Some terminal protocols report the same shortcut as `Alt+K`,
  which is accepted too.
- Or enter `/meta <request>`.

The overlay has its own draft and does not alter the ordinary composer. Press
`Esc` or `Cmd+K` to close it, and `Enter` to submit. If an agent turn is
running, the request is queued and interpreted at the next safe REPL boundary.
It does not steer or interrupt the active turn.

Examples:

```text
add this MCP server https://docs.example.com/mcp
connect my Grok account
use gpt-5.6-sol with high effort
enable web fetch for example.com
configure haskell-language-server-wrapper for .hs files
set the maximum number of concurrent agents to 6
```

## Supported changes

The typed action language covers:

- active model, reasoning effort, Fast mode, shell-tool mode, code mode,
  always-approve, skill reload, and the live agent limit;
- provider account connection and account selection;
- remote HTTP and local stdio MCP server add/update/remove/enable actions,
  protocol and timeout settings, OAuth scopes, and OAuth login;
- MCP initialization strategy;
- web-fetch enablement, domain allowlist, timeouts, and content limits;
- LSP enablement and stdio server command, arguments, extension mapping,
  workspace, and timeout settings;
- persisted maximum concurrent agents.

Harness configuration changes are validated and written atomically to
`~/.haskell-agent/config.json`. Existing secret environment values, OAuth
client credentials, and opaque LSP settings are preserved when public fields
are updated. Changes that affect startup-built runtimes prompt a session
restart.

## Safety boundary

Meta Console is intentionally separate from the coding agent:

1. The planner starts with an empty conversation and no previous response id.
2. It receives only current public session settings, the model catalog, and a
   recursively redacted harness configuration.
3. Tools are disabled, tool choice is `none`, and response persistence is
   disabled.
4. The planner can return only strict JSON decoded into a small typed action
   language. Unknown fields and unsupported commands are rejected. One
   format-only repair attempt is allowed.
5. The host displays an exact, secret-free preview. Mutations follow the
   current approval policy: always-approve applies, prompt mode asks, and
   deny-mutations blocks them.
6. OAuth and provider login run through the existing trusted login flows.
   Environment values are collected afterward in masked host-owned prompts;
   the value is never part of planner input, plan JSON, preview text, or the
   main transcript.

Do not paste credentials directly into the natural-language request. Ask Meta
Console to configure the relevant secret environment key; it will open a
masked prompt after the plan is approved.
