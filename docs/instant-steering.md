# Instant steering

New user guidance wakes the active model submission immediately. The loop keeps
the current task and tool scope, preserves complete response items, and submits
the new guidance with any real tool results. Partial streamed items are display
state only and are not promoted into conversation history.

## Codex source analysis

The reference is OpenAI Codex revision
`806d9732c974bc8a51b8317c1bd8985544fe627c`.

- [`core/src/session/turn.rs`](https://github.com/openai/codex/blob/806d9732c974bc8a51b8317c1bd8985544fe627c/codex-rs/core/src/session/turn.rs):
  `run_sampling_request` watches pending input using a per-request preemption
  token. `try_run_sampling_request` interrupts sampling and continues the same
  turn. Retry waits can also be interrupted. Completed tool calls are drained.
- [`codex-api/src/endpoint/responses_websocket.rs`](https://github.com/openai/codex/blob/806d9732c974bc8a51b8317c1bd8985544fe627c/codex-rs/codex-api/src/endpoint/responses_websocket.rs):
  Responses Lite sends `response.interrupt` after learning the response ID,
  with `mode: "discard_partial_items"`, then drains the response on the same
  socket. Ordinary streaming requests are cancelled instead.
- [`codex-api/src/sse/responses.rs`](https://github.com/openai/codex/blob/806d9732c974bc8a51b8317c1bd8985544fe627c/codex-rs/codex-api/src/sse/responses.rs):
  an interrupted incomplete response retains its response ID and usage and
  requests a follow-up. Other incomplete responses remain errors.
- [`core/tests/suite/pending_input.rs`](https://github.com/openai/codex/blob/806d9732c974bc8a51b8317c1bd8985544fe627c/codex-rs/core/tests/suite/pending_input.rs):
  tests cover socket reuse, retry interruption, and guidance arriving while
  tools drain.

## Harness implementation

`LoopConfig.loopWaitSteering` observes user guidance beyond the submitted,
unacknowledged queue prefix. Background completion notifications do not interrupt
sampling. Guidance remains queued until its response or recovered complete items
commit, so a failed request does not consume it.

The CLI reserves each submitted queue snapshot until acknowledgement. Dismissing
a background notification can only remove unsubmitted entries, keeping the
submitted prefix stable while newer guidance arrives.

The core loop races that notification against submission completion and explicit
cancellation. Providers may register a request-scoped steering interrupt through
`BackendCallbacks.onSteeringInterrupt`. The OpenAI Responses Lite WebSocket path
uses the native protocol; other paths cancel the submission and reconstruct the
next request from committed history and pending inputs. Native interrupt and
drain waits are bounded, with cancellation as a fallback.

Completed calls retain their actual results and existing asynchronous workers
remain owned by the enclosing tool scope. Explicit cancellation still ends the
task. Main sessions and subagents use the same steering mechanism.

Guidance arriving during a running tool waits for that tool's real result before
the next model request. Steering interrupts model generation; it does not undo
external side effects or terminate the current task.
