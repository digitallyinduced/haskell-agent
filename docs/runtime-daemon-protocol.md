# Runtime daemon protocol

The runtime daemon is a per-user local service. Its Haskell package is
`agent-runtime-daemon`; task execution is supplied separately through the
`Supervisor` seam.

## Transport and negotiation

Messages are UTF-8 JSON prefixed by an unsigned 32-bit big-endian byte length.
The daemon rejects frames larger than its configured limit (1 MiB by default)
before allocating the body.

The first client message must be `hello`, containing a stable client ID, the
protocol versions it supports, and optionally the sequence number of the last
event it durably acknowledged. The daemon replies with `welcome` and the
negotiated version, or `version_rejected` with its supported versions. Protocol
version 2 is currently supported.

A connection with no resume cursor receives a full snapshot. A reconnecting
client receives all retained events after its cursor. If the cursor predates
retention, it receives a fresh snapshot instead. Every event has a strictly
increasing sequence number. Clients send `ack` after durably applying an event
and answer `heartbeat` with `pong`.

Snapshots are sent as bounded `snapshot_chunk` messages. Each chunk contains
an independently base64-encoded slice of the snapshot JSON, plus a zero-based
chunk index and total chunk count. Clients decode each slice and concatenate
them in index order before decoding the snapshot JSON.

Commands and command results carry client-generated command IDs. Their JSON
payload is deliberately opaque to this package so the task supervisor can
evolve independently of the transport.

## Local security

The default endpoint is
`~/.haskell-agent/runtime/daemon.sock` and can be relocated with
`HASKELL_AGENT_RUNTIME_DIR`. The runtime directory and socket use modes `0700`
and `0600`. Existing non-socket paths, symlinked runtime directories, and
sockets not owned by the effective user are rejected. Accepted connections are
authenticated with the operating system's Unix peer credentials
(`getpeereid` via `network` on macOS).

A process-held exclusive lock serializes socket creation and stale-socket
removal. Shutdown only unlinks the device/inode created by that listener, so a
replacement path is never removed. Socket reads, writes, command execution,
and outbound queues have deadlines; per-client event queues are bounded and an
overflowed client is disconnected.

## Durability

Task snapshots are atomically replaced and fsynced. Events are appended,
fsynced, and monotonically numbered. On startup, queued or running tasks become
`TaskInterrupted`; the daemon never retries them automatically. Event and task
log retention are bounded, and credential-shaped JSON fields and log lines are
redacted before persistence.

Journal directories and files must be non-symlinked and owned by the effective
user. Corrupt snapshots, malformed or non-contiguous event suffixes, and
oversized recovery files fail startup closed. A persistence error poisons the
live journal so the process cannot reuse an uncertain sequence number.
