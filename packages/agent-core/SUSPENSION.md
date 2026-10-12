# Durable child suspension

The normal `Agent.Subagents` registry supports `Suspended UTCTime Text`.
`agent-tools` exposes `suspend_until` to children. It registers the request,
yields after the current tool batch (without interrupting tool handlers),
then commits the suspension after the runner checkpoints its paired transcript,
and releases the shared execution slot. Suspended agents remain addressable.

## Host integration

1. Configure `setSubagentSuspensionPersistence` before exposing the tool.
   The storage callbacks run under the lifecycle lock; do not re-enter
   lifecycle-mutating registry APIs from them.
2. `persistSuspension` must durably checkpoint the deadline/reason and enough
   child identity/history to restore it. It runs after `runChildTurn` returns,
   so the child's transcript includes completed tool results. A failed write
   settles the turn as errored, never as suspended. The normal settled callback
   follows the durable commit; it is not the durability boundary.
3. `persistSuspensionResume` must atomically transition the durable record
   into recoverable runnable work, rather than merely delete its deadline.
   Bind each registered child's expected durable revision/owner lease in the
   callback's host-owned map. Compare-and-swap that revision and suspended
   state, increment the revision, and return `Left` if ownership changed.
   Subsequent checkpoint/settled writes must check the same current fence.
   The callback receives the exact `InterAgentMessage` which will start the
   resumed turn. Persist that message atomically with the runnable handoff
   (including scheduled wake messages), and replay it on recovery. General
   messages to already-running agents still require a host-owned durable inbox.
4. `persistSuspensionCancellation` must fence the durable record against
   stale settled writes. Storage must enforce single process ownership or
   leases when multiple hosts share a store.
5. On restart, restore the parent/child hierarchy and transcripts using
   `restoreSubagentAtWithCwdStatus` with `Suspended deadline reason`, then
   invoke `resumeDueSubagents registry now`. Invoke it periodically and after
   capacity becomes available. It also resumes suspensions with queued
   messages before their deadline. `sendInput` attempts immediate early
   admission; callers must retry admission failures without dropping input.
6. Use `stopSubagentRegistry` for process shutdown after stopping intake and
   checkpointing work. `closeSubagentRegistry` is permanent cancellation.

The registry serializes resume admission and honors its shared concurrency
cap, including nested children. It does not own a database, wall-clock
scheduler thread, or distributed lease service. The CLI can serialize and
display suspended status but does not configure a durable suspension store
or automatic wake scheduler by default.
