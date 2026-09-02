# Agent server

`agent-server` exposes the in-process haskell-agent runtime through a versioned
HTTP API. It manages durable PostgreSQL sessions, bounded process-local turn
state, human approval requests, and a replayable Server-Sent Events stream.

## Start it

From the repository:

```console
nix run .#agent-server
```

The default listener is `127.0.0.1:4096`. It allows only the current directory
as a workspace root, does not enable CORS, and requires a strict local `Host`
header. The OpenAPI 3.1 document is served at
<http://127.0.0.1:4096/openapi.json>.

Useful options:

```console
agent-server \
  --host 127.0.0.1 \
  --port 4096 \
  --workspace-root /path/to/project \
  --workspace-root /another/allowed/root \
  --max-concurrent-turns 3 \
  --max-queued-turns 100 \
  --event-replay-limit 1000
```

Workspace roots and requested working directories are canonicalized before
use. A symlink cannot be used to select a directory outside an allowed root.

## Basic workflow

List the current boundary's models and create an empty durable session:

```console
curl http://127.0.0.1:4096/v1/models

curl -X POST http://127.0.0.1:4096/v1/sessions \
  -H 'Content-Type: application/json' \
  -d '{"cwd":"/path/to/project","model":"gpt-5.6-sol"}'
```

Queue a turn using the returned session id:

```console
curl -X POST \
  http://127.0.0.1:4096/v1/sessions/SESSION_ID/turns \
  -H 'Content-Type: application/json' \
  -d '{"input":"Inspect this project and run its focused tests."}'
```

Watch lifecycle, streaming, tool, and approval events:

```console
curl -N http://127.0.0.1:4096/v1/events
```

Reconnect with the last received SSE id to replay the bounded event window:

```console
curl -N \
  -H 'Last-Event-ID: 42' \
  http://127.0.0.1:4096/v1/events
```

If the id fell outside the replay window—or a connected client was too slow
for its bounded queue—the stream emits `replay.reset`. Refetch the relevant
REST resources before continuing.

## Turns and input requests

Turns move through:

```text
queued -> running -> waiting_for_input -> running
                                  \-> completed | failed | cancelled
```

Only one turn may be active for a session. The process runs up to three turns
from different sessions concurrently by default. Turn state is process-local;
the most recent 1,000 terminal turn records are retained, while conversation
history is durable.

When a mutating tool, root access, or plan interaction needs a decision:

```console
curl http://127.0.0.1:4096/v1/requests

curl -X POST \
  http://127.0.0.1:4096/v1/requests/REQUEST_ID/resolve \
  -H 'Content-Type: application/json' \
  -d '{"decision":"allow_once"}'
```

Use one of the request's advertised `options`. Plan change requests can also
send a `value` containing feedback.

Cancel a queued, running, or waiting turn with:

```console
curl -X POST http://127.0.0.1:4096/v1/turns/TURN_ID/cancel
```

The runtime first requests an in-band interruption, then performs structured
worker cancellation and joins the worker before returning.

## Session history and failed output

Session lists and history use keyset cursors:

```console
curl 'http://127.0.0.1:4096/v1/sessions?archive=active&limit=50'
curl 'http://127.0.0.1:4096/v1/sessions/SESSION_ID/history?limit=50'
```

Use `nextCursor` from each response for the next request. History turn objects
keep `items` (canonical model context) separate from `displayItems` (failed,
uncommitted partial output). Clients may render `displayItems`, but must never
feed it back to a model. SSE retry, discard, failure, and tool-retraction
events carry explicit display-only boundaries for the same reason.

Sessions can be renamed or archived with `PATCH` (one field per request),
deleted with `DELETE`, and forked at the active transcript or through a
durable turn index. Historical forks inherit their title and working directory
and can be renamed in a later `PATCH`. Mutation and fork requests return
`409 session_busy` while a turn is active.

## Security

The default loopback mode is intended for a single local user:

- the bind address defaults to `127.0.0.1`;
- `Host` must exactly match an allowed loopback host and configured port;
- browser origins are rejected unless explicitly listed with
  `--cors-origin`;
- JSON bodies and event/tool projections are bounded;
- prompts and model/tool output are not request-logged.

A non-loopback bind is rejected unless `--allow-remote` is present. Remote
mode additionally requires a bearer token from exactly one of:

```console
AGENT_SERVER_TOKEN='long-random-value' \
  agent-server --host 0.0.0.0 --allow-remote

agent-server --host 0.0.0.0 --allow-remote \
  --token-file ~/.config/haskell-agent/server-token
```

Token files must be regular, non-symlink files owned by the current user and
must not be accessible by group or other users. Tokens are accepted only as
`Authorization: Bearer ...`; command-line token values and query-string tokens
are intentionally unsupported. `agent-server` does not terminate TLS, so a
remote listener must be placed behind trusted TLS termination (or confined to
an equivalently protected network); otherwise the bearer token travels in
plaintext.

Providing either token source on a loopback bind also enables bearer mode.
This is useful when a local TLS reverse proxy forwards authenticated requests
to the server.

Organization-gateway credential identity is an isolation boundary. Each HTTP
operation is performed under a credential read lease, queued turns retain the
identity captured at admission, an executing turn holds the admission-aware
lease for its complete runtime and terminal event, and every SSE write
revalidates its captured identity. Session queries apply the identity predicate
inside PostgreSQL before ordering and limiting.

## Version 1 scope

The HTTP API deliberately excludes arbitrary CLI argument forwarding,
auto-approval (`--yolo`), worktree creation, computer use, attachments, and
mid-turn steering. Those capabilities require dedicated typed protocol
designs rather than stringly command passthrough.

Every error uses:

```json
{
  "error": {
    "code": "session_busy",
    "message": "the session has an active turn",
    "requestId": "request-12"
  }
}
```

See [`packages/agent-server/openapi.json`](../packages/agent-server/openapi.json)
for the complete route and schema reference.
