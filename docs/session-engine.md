# Shared session engine boundary

## Implemented: frontend-neutral turn lifecycle

`agent-cli-runtime` owns the lifecycle policy in `Agent.Runtime.*`:

- `Request`: typed native turn requests and validation, independent of
  `CliOptions`. Existing CLI exports remain compatibility reexports.
- `Compaction`: the installed automatic-compaction checkpoint.
- `TurnState`: preparation, conversation patches, interrupted-turn retention,
  response-chain invalidation, startup-context merging, and checkpoint rebasing.
- `TurnEngine`: one pure finalization decision over a prepared turn and the
  existing `Agent.Loop` execution result.

`Agent.CLI.Turn` consumes this decision rather than independently deriving
retention and recovery policy in each completion branch. The host still performs
UI updates, cleanup, and persistence at the existing boundaries. Provider
execution, approval dispatch, and exception handling have not been replaced.

Finalization preserves this precedence:

1. Explicit restart.
2. Cancellation.
3. Provider-unavailable recovery, only before a response commits.
4. Other failure.
5. Success.

`ModelItems` and `DisplayItems` are distinct opaque types at this boundary.
Incomplete assistant output can remain visible without becoming retry input.
Legacy persistence still uses raw response-item lists; unwrap projections only
at their corresponding storage fields. This is not yet a repository-wide
type-level separation of model history and display history.

The shared policy modules must not import CLI or TUI modules.
`scripts/check-package-boundaries.sh` checks this, as well as preventing the
runtime library from depending on the CLI or TUI packages.

## Remaining: move execution ownership out of the CLI

This extraction is a lifecycle kernel, not a complete headless session owner.
The server still calls the CLI's native runtime, which still lowers requests
into `CliOptions`. `SessionEnv` still combines runtime resources with terminal
state.

The next migration should:

1. Separate session resources from terminal rendering/input state, with explicit
   capabilities for persistence, provider execution, approvals, and events.
2. Introduce a runtime-owned session handle that serializes turns and owns
   cancellation and cleanup. Consume typed requests directly, without
   reconstructing CLI options.
3. Route CLI and server/native adapters through that owner, retaining the same
   `Agent.Loop` and storage formats.
4. Remove the server's `agent-cli` dependency only after fake-provider parity
   tests cover success, failed streaming, committed tools followed by failure,
   pending-approval cancellation, compaction/retry, resume, and shutdown.

Keep the existing process topology and private Swift application boundary.
Neither requires a rewrite to establish this dependency direction.

## Validation

The focused `Agent.Runtime.RequestSpec`, `TurnStateSpec`, and `TurnEngineSpec`
contain 17 passing examples, including a real loop with a scripted streaming
backend that fails after partial output. These run without terminal setup.

Package-boundary and whitespace checks pass. Independent diff review found no
policy regression. A multi-package Cabal GHCi check of runtime, CLI, and server
was blocked while compiling unchanged `Agent.OpenAI.Models.Types`: its
Template Haskell input `data/prompt.md` is absent in this checkout. Consequently
the changed downstream CLI/server integration has not yet been typechecked.
The focused tests do not establish full frontend parity.
