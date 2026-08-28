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

Enabled servers are shared with subagents. Tools explicitly annotated
`readOnlyHint: true` run without generic mutation approval. All other tools are
exposed as mutations and follow the session's normal approval policy; Telegram
uses its scoped inline approval buttons, while `--deny-mutations` blocks them.
Blocking startup publishes tools as `server__tool`. Progressive startup exposes
`mcp_search` and `mcp_call` while servers connect, then publishes each server's
catalog. Failed stdio transports are restarted and retried only for read-only
calls, because retrying a mutation could duplicate its side effect.

## Remote Streamable HTTP servers

Remote MCP endpoints use `url` instead of `command`:

```json
{
  "mcpServers": {
    "remote": {
      "url": "https://example.com/mcp",
      "env": { "MCP_ACCESS_TOKEN": "..." }
    }
  }
}
```

The client preserves the `Mcp-Session-Id` returned by initialization. OAuth
protected-resource discovery, authorization-server discovery, dynamic client
registration, and refresh-token exchange are available in the MCP OAuth layer.
Until the interactive PKCE login UI is wired in, an access token can be supplied
as the redacted `MCP_ACCESS_TOKEN` environment entry.

### OAuth token refresh

Remote servers can point `env.MCP_OAUTH_TOKEN_FILE` at a private JSON token
record:

```json
{
  "client_id": "registered-public-client-id",
  "token_endpoint": "https://example.com/oauth/token",
  "access_token": "...",
  "refresh_token": "...",
  "expires_at": null
}
```

The access token is loaded for each request. If the server returns `401`, the
client takes an in-process and cross-process exclusive refresh lock, re-reads
the record, exchanges its refresh token, atomically persists the new access
and rotated refresh tokens with mode `0600`, and retries the MCP request once.
A failed refresh returns an actionable error instead of repeatedly attempting
the authorization flow.
