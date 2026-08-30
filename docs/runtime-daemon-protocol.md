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
version 3 is currently supported.

A connection with no resume cursor receives a full snapshot. A reconnecting
client receives all retained events after its cursor. If the cursor predates
retention, it receives a fresh snapshot instead. Every event has a strictly
increasing sequence number. Clients send `ack` after durably applying an event
and answer `heartbeat` with `pong`.

Snapshots are sent as bounded `snapshot_chunk` messages. Each chunk contains
an independently base64-encoded slice of the snapshot JSON, plus a zero-based
chunk index and total chunk count. Clients decode each slice and concatenate
them in index order before decoding the snapshot JSON.

Events that fit the negotiated frame bound use `event`. Larger events use the
same scheme in `event_chunk` messages: clients concatenate decoded slices and
then decode the resulting event envelope. This keeps persisted events
deliverable without increasing the transport frame bound.

Commands and command results carry client-generated command IDs. The public
task adapter accepts the following versioned command payloads. Unknown versions,
types, invalid combinations, empty identifiers, and values beyond the
documented bounds are rejected without changing scheduler state. Scheduler
command admission is bounded and nonblocking: a full queue returns an immediate
error rather than allowing an expired client request to execute later.

```json
{"version":1,"type":"submit","task_id":"stable-client-id","session_id":"optional-existing-session","prompt":"...","cwd":"/absolute/or/relative/path","provider":"openai","model":"gpt-5.6","effort":"high","worktree":false}
{"version":1,"type":"cancel","task_id":"stable-client-id"}
{"version":1,"type":"list"}
{"version":1,"type":"set_limit","limit":4}
{"version":1,"type":"approval","task_id":"stable-client-id","approval_id":"request-id","decision":"approve"}
{"version":1,"type":"retry","task_id":"stable-client-id"}
```

`task_id` is a caller-generated stable identifier and cannot be reused.
`session_id` is the stable persisted session to resume; tasks for one session
are serialized. A missing session identifies a fresh session and does not
conflict with other fresh tasks. `provider` and `model` must either both be
present or both absent, and `worktree` is valid only for a fresh session.
Task/session/approval IDs are at most 256 characters, prompts 8,192
characters, paths 4,096 characters, and the concurrency limit is 1 through 32.
Approval decisions are syntactically limited to `approve`, `deny`, or
`approve_session`, but the current daemon runner does **not** support
interactive approval resolution. Every `approval` command fails visibly with
an unsupported-operation error and emits no `approval_resolved` event. Tasks
that require an interactive approval must therefore fail or be run by an
interactive client; clients must not present daemon approval controls as
functional.

Successful results contain `"version": 1`; submit, cancel, and retry results
also contain the durable task. `list` returns every retained durable task.
Task transitions are journaled as `task_changed` events before the mutating
command reports success. A failed, cancelled, or interrupted task is rerun
only by an explicit `retry` command while the same daemon process still holds
the original, unredacted input in memory. Raw prompts are deliberately not
persisted separately from the redacted task description. After daemon restart,
`retry` therefore fails with `task input is unavailable`; the client must
submit the input again under a new task ID. A completed or active task cannot
be retried.

The shipped daemon runner executes `agent-cli` directly without a shell
(`HASKELL_AGENT_CLI` can override its path), using the real one-shot CLI flags
for prompt, session resume, cwd, provider/model/effort, session persistence,
and optional worktree creation. It always passes `--no-yolo`, so a noninteractive
task fails closed rather than implicitly approving mutating tools. Stdout and
stderr are concurrently drained in
bounded chunks before journal log limits are applied, total output is capped,
and every process task has a six-hour wall-clock deadline. Embedders can supply
a typed `TaskRunner` while retaining the same scheduler, persistence, and wire
protocol; the adapter intentionally exposes no approval-resolver hook.

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
