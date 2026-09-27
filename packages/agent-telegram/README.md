# Standalone Telegram session integration

The standalone gateway retains its existing account, allowlist, group, command,
streaming-draft and human-interaction bridge policies. Text, transcribed voice,
and downloaded media execute through the shared
`Agent.Telegram.Connector.Session.advanceSessionExecution` lifecycle using
`Agent.Telegram.Session.Local.localSessionBackend`.
Conversation queues use the shared `runConversationQueue` coordinator for
ordered work, retry waiting, synchronous failure handling and cancellation
propagation. Media recognition uses `Agent.Telegram.Connector.Media`.

The local backend identifies work by its persisted Telegram update identifier,
not by the message text or the replied-to message identifier. Edits and reactions
therefore remain distinct requests. Session identity and the complete prepared
prompt must match a restored execution checkpoint.

## Process recovery

Private `executions/<update-id>.json` files beneath the existing gateway state
directory record admission before launching the managed process and terminal
output before publishing a pending Telegram reply. The existing pending action,
reply and delivery state formats remain readable.

Completed output can be recovered without launching the process again.
An interrupted admitted process has an uncertain outcome: unlike the agent-server
backend, the managed-process interface does not expose an idempotent request
identifier. Such work is **not automatically relaunched**. Inspect the agent
session before explicitly requesting replacement work. This deliberately
replaces blind retries after an ambiguous local process launch.

Checkpoint files are retained with their lock files; deleting them while old
pending actions remain would remove duplicate-execution protection.

## Compatibility coverage

The existing Telegram test suite covers command parsing, allowlist and group
admission, reply/reaction classification, media cleanup and preservation,
voice checkpointing, cancellation, human callback authorization and durable
pending action decoding. Local-backend tests additionally cover completion
recovery, identity mismatch, interrupted admission and pre-submission cancellation.

Standalone command/group admission and the live bridge remain application
adapters around the shared queue and execution lifecycle; they are not replaced
by the connector's private application admission policy.
