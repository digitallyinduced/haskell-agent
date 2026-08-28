# MCP servers

The harness is an MCP client for the current specification revision
(`2026-07-28`) and for the earlier `initialize`-based revisions
(`2025-11-25` and before). Configure servers in `~/.haskell-agent/config.json`:

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
      "requestTimeoutSeconds": 60,
      "protocol": "auto"
    }
  }
}
```

In an interactive session, `/mcp` opens the server manager. Use the arrow
keys or `j`/`k` to navigate, Enter to inspect tools, `a` to add a server,
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
catalog. Every mode also exposes `mcp_list_resources` and `mcp_read_resource`
for browsing server resources and following `resource_link` results.

## Protocol negotiation

`protocol` selects how a server is contacted:

- `auto` (default) sends `server/discover` first. A modern server answers with
  its supported versions and capabilities; a server that returns any other
  error, an HTTP error without a modern JSON-RPC body, or nothing within five
  seconds is treated as legacy and initialized with the `initialize`
  handshake (requesting `2025-11-25`). The era is remembered across
  reconnects.
- `modern` requires `server/discover` to succeed.
- `legacy` skips the probe and starts with `initialize`.

With a modern server every request carries the protocol version, client
identity, and client capabilities in `_meta`. Over Streamable HTTP the client
also sends the `MCP-Protocol-Version`, `Mcp-Method`, and `Mcp-Name` headers,
mirrors `x-mcp-header` tool parameters into `Mcp-Param-*` headers (tools with
invalid annotations are dropped with a warning), and treats an
`UnsupportedProtocolVersion` error as a request to retry with an advertised
version. Legacy HTTP servers keep their `Mcp-Session-Id`, which is terminated
with `DELETE` on shutdown.

## Requests, progress, and cancellation

Tool calls, prompt resolution, and resource reads follow the multi round-trip
pattern: an `input_required` result is answered with the requested input and
retried with the server's opaque `requestState`, for up to eight rounds. Task
results (`resultType: "task"`, extension `io.modelcontextprotocol/tasks`) are
polled with `tasks/get` at the server's suggested interval; `input_required`
tasks are answered through `tasks/update`, and the task is cancelled when the
call gives up.

`requestTimeoutSeconds` is an idle timeout. Progress notifications
(`notifications/progress`) extend it, up to ten times the configured value,
and are shown as live output on the running tool. A stdio request that times
out is cancelled with `notifications/cancelled`; over HTTP the response
stream is closed. Server `ping` requests are answered on every transport.

## Elicitation

Servers may ask the user for input while a tool runs. Form-mode requests are
rendered field by field with the restricted schema the specification allows
(text, numbers, booleans, single and multi-select enums), validated locally,
and reviewed before they are sent. URL-mode requests show the full URL and
its host and only open the browser after explicit consent; the page is never
loaded by the agent. Decline and cancel are always available. Non-interactive
runs do not declare the `elicitation` capability and cancel any request that
arrives anyway.

## Catalog changes and subscriptions

Modern servers that advertise `listChanged` receive a `subscriptions/listen`
stream for tool, prompt, and resource list changes; legacy servers deliver
the same notifications unsolicited. A `notifications/tools/list_changed`
re-lists the server's tools and updates the catalog used by `mcp_search`,
`mcp_call`, and the `/mcp` manager. Statically registered `server__tool`
handlers keep working as long as the server still offers the tool.

## Server instructions, prompts, and resources

Instructions advertised by a server are appended to the system prompt under an
"MCP server instructions" heading. In progressive mode they are delivered as a
system reminder once the servers settle.

`/mcp prompt <server> <name> [key=value ...]` resolves a server prompt
template and submits its messages as the next turn. Resources are available to
the model through `mcp_list_resources` and `mcp_read_resource`.

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

Responses are consumed as they stream: request-scoped notifications are
processed as they arrive and the response ends the stream. Authorization uses
the OAuth 2.1 flow described in [MCP OAuth](mcp-oauth.md): run
`agent mcp login <url>` to authorize, or supply an access token through the
redacted `MCP_ACCESS_TOKEN` environment entry. A `401` or `403` response
surfaces the server's `WWW-Authenticate` challenge, including the scopes it
requires.

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

## Not implemented

- Sampling, roots, and logging (`logging/setLevel`) are deprecated in
  `2026-07-28` and are not offered; a server that requests them receives a
  method-not-found error.
- Image and audio content blocks are described to the model but their bytes
  are not forwarded.
- Task identifiers are not persisted across restarts.
- Skills-over-MCP metadata is discovered but not yet activated by the CLI.
