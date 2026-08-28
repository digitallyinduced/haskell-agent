# Local MCP servers

Configure local stdio MCP servers in `~/.haskell-agent/config.json`:

```json
{
  "version": 1,
  "mcpInitStrategy": "auto",
  "mcpServers": {
    "seo-mcp": {
      "command": "nix",
      "args": ["run", "/absolute/path/to/seo-mcp"],
      "env": {
        "GOOGLE_APPLICATION_CREDENTIALS": "/absolute/path/to/credentials.json"
      },
      "startupTimeoutSeconds": 120,
      "requestTimeoutSeconds": 60
    }
  }
}
```

In an interactive session, `/mcp` opens the local-server manager. Use the
arrow keys or `j`/`k` to navigate, Enter to inspect tools, `a` to add a server,
Space to enable or disable it, `x` to remove it, and `r` to restart the MCP
runtime. Saved changes restart the runtime while preserving the session.
Environment variable values are never displayed.

`mcpInitStrategy` accepts `auto`, `progressive`, or `blocking`. `auto` starts
servers progressively for interactive sessions so the prompt is immediately
available, while one-shot commands wait for MCP initialization.

Enabled servers are shared with subagents. Only tools explicitly annotated
`readOnlyHint: true` are exposed. Blocking startup publishes them as
`server__tool`. Progressive startup exposes `mcp_search` and `mcp_call` while
servers connect, then publishes each server's read-only catalog. Failed stdio
transports are restarted once before a progressive call is retried.
