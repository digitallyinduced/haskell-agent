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
  --max-event-subscribers 256 \
  --max-event-subscribers-per-tenant 8 \
  --event-replay-limit 1000
```

Workspace roots and requested working directories are canonicalized before
use. A symlink cannot be used to select a directory outside an allowed root.

## Multi-tenant sandbox deployment

Multi-tenant mode uses an opaque bearer credential to select a tenant. Each
tenant gets a disjoint workspace, server-owned state directory, PostgreSQL
database and restricted runtime role. Model-controlled filesystem, shell,
process, and network tools execute in one gVisor sandbox per tenant, shared
by that tenant's sessions and started lazily on the first sandboxed tool call.
Provider API calls, authorization, and PostgreSQL access remain in the host
server; database credentials and provider secrets are not copied into the
guest. Host-side project instructions, filesystem skills, Git status, and
project settings are disabled in this mode so tenant-controlled workspace
symlinks cannot turn startup discovery into a host read or write.

Sandbox execution tools are auto-approved by default: workspace edits, shell
commands, builds and tests do not create human approval requests. This applies
to existing sessions on their next turn as well as new sessions. It is scoped
to tools actually proxied into the tenant sandbox, not a global `--yolo` policy:
host services, including mutating MCP calls, retain their existing approvals.
Plan-mode restrictions, dangerous-command checks and the sandbox boundary are
unchanged. Single-tenant servers without a sandbox and local CLI sessions keep
their existing approval behavior.

Sandbox networking remains available, so automatic shell approval is not a
semantic guarantee against external side effects (for example a command using
credentials that a user has placed in the workspace). Do not place production
credentials in an auto-approved sandbox unless that access is intended.

Use the exported NixOS module for a production multi-tenant deployment:

```nix
{
  inputs.haskell-agent.url = "github:digitallyinduced/haskell-agent";

  outputs = { nixpkgs, haskell-agent, ... }: {
    nixosConfigurations.agent-host = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        haskell-agent.nixosModules.agent-server
        {
          services.haskell-agent.server = {
            enable = true;
            host = "0.0.0.0";
            allowRemote = true;
            tenantRegistryFile = "/run/credentials/agent-tenants.json";
            workspaceRoots = [
              "/srv/agent-workspaces/acme"
            ];
            maxActiveTenants = 16;
          };
        }
      ];
    };
  };
}
```

Deploy the resulting system configuration rather than invoking the server from
an ordinary shell:

```console
sudo nixos-rebuild switch --flake .#agent-host
```

The module creates a dedicated account without supplementary groups, a private
state directory, and a root-owned trusted copy of each immutable runner under
`/run/haskell-agent-server-runners/<state-directory>/<store-generation>/`.
Each generated unit names its exact runner generation, so a later activation
or rollback never replaces the runner used by an existing server process. The
unit fails closed if that executable is absent. It places the server in a
`supervisor` subgroup, delegates exactly the `cpu`, `memory`, and `pids` cgroup
v2 controllers, removes capabilities, enables `NoNewPrivileges`, and applies
the filesystem and process limits required by the runner. A direct
`agent-server --sandbox-runner ...` launch from an ordinary shell is
intentionally unsupported because it cannot establish or attest that service
boundary.

The owner-only registry is strict, versioned JSON:

```json
{
  "version": 1,
  "tenants": [
    {
      "id": "018f6a14-7d52-7a52-9c00-66d5e7d70334",
      "workspaceRoot": "/srv/agent-workspaces/acme",
      "credentials": [
        {
          "id": "018f6a14-7d52-7a52-9c00-66d5e7d70335",
          "tokenFile": "/run/credentials/acme-agent-token"
        }
      ]
    }
  ]
}
```

Tenant and credential ids must be canonical UUIDs. Provision the registry and
every credential file as regular, non-symlink, mode-0600 files owned by the
configured service user (by default `haskell-agent-server`). Tenant tokens must
contain at least 32 bytes and must be unique. Every registry workspace must be
listed exactly in the module's `workspaceRoots`. Workspace roots must exist,
must not overlap one another or server state, and cannot contain the registry
or a token file. Their parent ancestry must be root- or server-owned and not
group/other-writable. The sandbox runner has the same trusted-ancestry
requirement and must be outside every tenant workspace and per-tenant state
directory. Registry, environment-file, and workspace paths configured through
the module must be canonical absolute non-root paths: `.` and `..` components,
repeated or trailing separators, and systemd `%` specifiers are rejected.
`maxActiveTenants` must cover the complete registry.

The gVisor sandbox receives only two writable directory bind mounts: the
tenant workspace as `/workspace` and a dedicated guest-data directory as
`/state`. The bootstrap trace remains inside the bounded `/run` tmpfs. On
startup failure the runner emits only a bounded tail to its private stderr;
a successful bootstrap deletes the trace before starting the worker. Runtime
bundles, locks, sockets, registry data, and credentials otherwise stay in
host-only paths. The runner pins both directory mounts by open descriptors
before starting `runsc`, so a later pathname replacement cannot redirect a
mount. It also compares the workspace descriptor's device and inode with the
identity recorded when the registry was loaded, rejecting a pre-launch
substitution.

Outbound sandbox networking is available for development tools through
`slirp4netns`. Its immutable nftables policy rejects loopback, private,
link-local, metadata, reserved, IPv6, and all host addresses captured at
sandbox launch. The runner monitors host address changes and terminates stale
sandboxes; the next sandboxed call starts a replacement with a fresh deny set.
There is no inbound sandbox service or SSH. Failure to start, attest, or
communicate with gVisor fails the tool call closed; the server never falls back
to host execution. The protocol input must be a read-only pipe so closing the
server's writer produces an unambiguous EOF.

The managed host PostgreSQL cluster provisions a separate database and
`NOSUPERUSER`, `NOCREATEDB`, `NOCREATEROLE`, `NOINHERIT`, `NOBYPASSRLS`
runtime role for each tenant. Public database connectivity is revoked, runtime
roles receive only the application grants in their own database, and custom
scope role names include the tenant namespace. The sandbox has no PostgreSQL
credentials or socket mount.

The NixOS module delegates the `cpu`, `memory`, and `pids` cgroup v2 controllers
to the service's `supervisor` subgroup. Operators must additionally enforce
filesystem quotas, PostgreSQL database quotas/backups, TLS termination, and
authentication rate limits. The server bounds global/per-tenant turns, queues,
active tenant runtimes, SSE subscribers, replay buffers, request bodies,
protocol frames, and sandbox tool output. Each sandbox process tree, including
its network helper, runs in a dedicated cgroup limited to two CPUs, 2 GiB RAM
without swap, and 512 processes. gVisor uses the `systrap` platform, an
immutable Nix root filesystem, a private 4 GiB overlay, and a fresh 256 MiB
tmpfs for mutable Nix database and build-log state. Workspace and guest-state
storage remain operator-owned host capacity and must be quota-limited by the
deployment. Cleanup uses global TERM and KILL deadlines. If descendant
quiescence cannot be proved, the runner fail-stops while retaining the tenant
lock until its supervisor kills the complete process group; a stale
tenant-named cgroup also blocks replacement launches.

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

Human requests retain their complete prompt and options, including plaintext
tool arguments and plans (encrypted arguments remain redacted). Requests
exceeding 64 KiB of encoded JSON or 100 options
are rejected rather than truncated: approving a preview must not authorize
an unseen suffix. Tool/plan approval adapters fail closed when such a request
cannot be published.

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

In multi-tenant mode, the tenant identity derived from the opaque bearer is the
outer authorization boundary. Organization-gateway identity is nested inside
that tenant and is never accepted as a substitute for tenant authentication.

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
