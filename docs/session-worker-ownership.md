# In-process session worker ownership

`Agent.Runtime.SessionOwner` owns background session-turn workers without
depending on CLI, TUI, filesystem layout, providers, or transport messages.
`Agent.CLI.Session.Threads` uses it in production and remains the compatibility
adapter for text responses and external session/activity locks.

The owner provides:

- typed, nonblocking admission failures (closed, same session busy, capacity);
- one running action per session identity;
- an explicit active-worker limit, including workers executing cleanup;
- immutable status snapshots and waits captured for one generation;
- cancellation that joins that generation without cancelling a later retry;
- close that rejects admission, suppresses completion notifications, and joins
  tracked workers before discarding their handles.

The adapter admits at most 64 active background turns per manager. There is no
pending queue: a rejected action never starts later. Terminal outcomes remain
available for repeated status queries and do not occupy an active slot.
This preserves the previous outcome-retention behavior; bounded terminal
retention is a separate policy decision.

The submitted action owns its persistence and cleanup ordering. Completion
notification runs before readiness for another turn and must be nonblocking
and must not call back into the owner. Cancellation and close must be initiated
outside the worker being cancelled. Cancellation remains cooperative Haskell
thread cancellation: an action or finalizer that masks forever can delay close.
Each generation owns one cancellation sender, shared by concurrent cancellation
requests and close. Interrupting a cancellation caller does not interrupt signal
delivery or cause another caller to send a second cancellation into cleanup.
Worker handles remain tracked until their `Async` has actually terminated.

## Remaining boundaries

This is a worker-lifecycle extraction, **not** a replacement for the complete
session engine. Foreground turn preparation, state commits, persistence
composition, provider/tool startup, and frontend rendering still require their
own capability boundary. Cross-process mutual exclusion continues to use the
existing session lock. The server still depends on `agent-cli`; moving the
native facade alone would not remove its underlying orchestration dependency.

The next composition change should extract shared startup/tool construction
from `runAgentWithRuntime`, retaining first-party dialect selection and
capability restrictions, before switching CLI and server to that implementation.
Moving or duplicating the current facade would hide rather than fix this
dependency.
