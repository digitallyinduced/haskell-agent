# Provider account failover and replay safety

Account health and permission to replay an action are separate decisions.
An authentication or quota error may justify replacing a credential, but not
repeating a request that already streamed output or admitted an async tool.

`Agent.Provider.runWithTokenProviderAttempt` consumes `ProviderAttemptFailure`,
which carries the original `ApiError` and an explicit `ReplaySafety`:

- `ReplaySafe`: account classification may authorize a replacement attempt.
- `ReplayUnsafe`: effects/output have crossed the action's replay boundary.
- `ReplayUnknown`: the action cannot establish that repetition is safe.

Only `ReplaySafe` permits failover. Unsafe and unknown failures return the
original error, without another credential checkout, even when a custom
account classifier recognizes that error. Unclassified failures still do not
rotate accounts, and the existing 64-attempt budget is unchanged.

## Streaming adapters

`runWithTokenProviderStreaming` latches output before delivering it to the
consumer. A later lifecycle event cannot clear that latch. Transports must
complete all callbacks before returning. Consumer exceptions, transport
exceptions, and cancellation propagate without being caught or replayed.

All three credentialed stateless Responses adapters use the existing
`streamOutputObserved` predicate. It includes visible output, completed
opaque checkpoints and completed tool items, including async tool admission.
The xAI and OpenRouter production backends use these adapters.
Lifecycle-only events still allow a rejected request to use another account.
The Gemini adapter treats every Gemini stream event as output: text,
reasoning, or a ready function call.

This adapter assumes classified account failures before output represent
request rejection. It is not an automatic proof that arbitrary remote effects
did not happen. Actions with uncertain effects must use explicit attempt
metadata and return `ReplayUnknown` or `ReplayUnsafe`.

## Compatibility and remaining boundaries

`runWithTokenProvider` and `runWithTokenProviderAfter` retain their historical
API and replay-safe-action precondition. They delegate to the explicit attempt
gate with `ReplaySafe`; they do not infer safety for arbitrary callbacks.
Existing non-streaming callers and connection-acquisition wrappers are not
silently given a different retry policy.

OpenAI's specialized backend already tracks raw/visible/tool output and async
admission, handles attempt-local display retraction, and guards its own
transport recovery. Those policies and its legacy replay-unsafe error markers
remain unchanged. This change does not migrate every retry layer to a new
failure type, implement a durable external-effect journal, or promise
exactly-once execution across crashes.

## Contract tests

- `agent-core/Agent.ProviderSpec`: explicit unsafe/unknown outcomes, custom
  classifiers, safe rejection, unclassified errors, bounded retries, streaming
  latching, consumer exceptions and cancellation.
- `agent-responses/Agent.Responses.LoopBackendSpec`: the same pre-output and
  post-output contract for all three checkpoint/history modes, including
  single async admission after a provider failure.
- `agent-gemini/Agent.Gemini.LoopBackendSpec`: safe pre-output failover and
  no replay after each native stream-event kind.

Run through the corresponding test-component `cabal repl` from `nix develop`,
then `:main` (or a focused Hspec `--match`). Require both a successful GHCi
module load and a nonzero test count with zero failures; GHCi's exit status
alone does not establish either.
