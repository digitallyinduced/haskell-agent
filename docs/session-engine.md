# Shared session engine boundary

## Typed turn persistence records

`Agent.Runtime.TurnRecord` carries the engine's opaque `ModelItems` and
`DisplayItems` projections through successful and interrupted turn persistence.
Only `Agent.Runtime.Session.TurnRecord.sessionTurnFromRecord`, a compatibility
adapter owned by `agent-runtime`, unwraps them into the existing `SessionTurn`
storage format. Display-only activity cannot be passed as canonical history.
The persisted schema is unchanged.

This is a data boundary, not a new persistence scheduler. The CLI host still
owns the existing timestamp/session creation, metadata append, slot publication,
fullscreen history, eviction, and title-update ordering. Extracting that IO
ownership remains a separate session-owner/composition step.

## Implemented: frontend-neutral turn lifecycle

`agent-runtime` owns the lifecycle policy in `Agent.Runtime.*`:

- `Request`: typed native turn requests and validation, independent of
  `CliOptions`. Existing CLI exports remain compatibility reexports.
- `StartupPolicy`: host-owned permissions for workspace instruction/skill
  context and host startup facilities. Native hooks carry this typed policy,
  not an arbitrary CLI-options transformation. The server selects the restricted
  preset for sandbox turns; the desktop bridge retains the host preset.
- `Compaction`: the installed automatic-compaction checkpoint.
- `Compaction.Provider`: provider compaction, occupancy accounting, and
  continuation bounding.
- `TurnState`: preparation, conversation patches, interrupted-turn retention,
  response-chain invalidation, startup-context merging, and checkpoint rebasing.
- `TurnEngine`: one pure finalization decision over a prepared turn and the
  existing `Agent.Loop` execution result.
- `TurnExecution`: executes a typed `PreparedExecution` with the real
  `Agent.Loop`, returns `ExecutedTurn`, and emits typed `ExceptionalTurn`
  rollback state before propagating an exception. No `CliOptions`, renderer,
  or terminal callback enters this boundary.

`Agent.CLI.Turn` consumes the executor and finalization decision rather than
calling the loop or independently deriving rollback policy. The host still
performs UI updates, auxiliary cleanup, and persistence at the existing
boundaries. Provider execution and approval dispatch continue to use the
unchanged loop capabilities. The exceptional-state sink is not a UI callback:
it commits the supplied patch and restores host-owned auxiliary state.

The extraction deliberately preserves the exception scope and ordering:
execute loop with exceptional rollback; read the compaction checkpoint outside
that handler; clear thinking and capture timing; consume the restart request;
finalize. A failure after loop execution must not retroactively roll it back.

### Native startup policy boundary

`Agent.Runtime.Startup.Model` resolves typed model startup inputs into model
identity, transport mapping, dialect, reasoning effort, and resume/context
invalidation decisions. Its resume input contains only the persisted fields
used by those decisions. Gateway selections remain authoritative; remembered
project dialects only apply to the matching provider, and changed custom-model
wire mappings invalidate the previous target using the existing rules.

`Agent.Runtime.Startup.Policy` owns approval defaults, native interaction-mode
approval, Claude bypass eligibility, and dialect effort normalization. A live
native interaction-mode callback always prevents provider-side Claude bypass,
including when the initial mode is Yolo. `Agent.Runtime.Startup.Gateway` owns
pure gateway catalog projection and model selection; saved aliases are hints,
not an authority for the provider assigned by the current gateway catalog.

The CLI translates `CliOptions`, native hooks, project settings, and session
metadata into these inputs. It retains credential/catalog IO, startup messages,
and provider client-option construction. This is not yet a shared session
startup entry point, and does not remove the server's CLI dependency.

`Agent.CLI.NativeRuntime` remains the legacy execution adapter. It translates
the native startup policy after preparing typed requests or legacy arguments.
Restricted startup pins the admitted cwd and disables worktrees, automatic
approval, startup input files, computer use, and code mode; supplied-context-only
startup disables AGENTS.md and skills. These are the previous server sandbox
restrictions, now enforced in the adapter rather than through server-owned
`CliOptions` mutation. Host policy preserves legacy desktop options unchanged.
Typed native turns independently retain their stricter request invariants.

This removes CLI options from the server startup contract, not the server's
dependency on CLI orchestration. Shared startup/session composition still needs
extraction before server and CLI can call the same frontend-neutral entry point.

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

## Bounded process-resource extraction

Background session workers now use a frontend-independent typed owner; see
[session worker ownership](session-worker-ownership.md) for admission,
cancellation, notification ordering, and the remaining composition boundary.

`agent-runtime` also owns `Agent.Runtime.NativeProcess` and
`Agent.Runtime.Session.Threads`. These frontend-neutral modules own process allocation,
the tracked cleanup worker, session-thread registration, MCP and network
recovery resources, and their close/restart operations. The server imports
process-resource operations directly from this package; CLI exports remain
compatibility facades. Existing acquisition rollback, worker unmasking,
cancel/join ownership, and shutdown order are preserved, not redesigned.

## Implemented: frontend-neutral provider scopes

`Agent.Runtime.Providers.withProviderRuntime` constructs OpenAI, xAI, Gemini,
OpenRouter, and Claude backends in `agent-runtime`. `ProviderConfig` carries
only the selected provider's configuration. `ProviderHost` supplies shared
compaction state and network recovery; account presentation, preferences, and
automatic-compaction installation remain explicit host operations.

The callback receives a `ProviderRuntime`: a neutral `SessionBackend`, context
window lookup, manual compaction, account-selection operations, and child
transport capabilities. These operations are valid only within that callback.
Persistent OpenAI connections, its account-switch worker, and Claude backend
resources retain their existing scoped lifetimes and exception behavior.
Disposable OpenAI side-call and child backends also belong to the runtime.

`Agent.Runtime.Compaction.Provider` owns provider summarization, transcript
installation, task-plan decoration, and occupancy policy. The lightweight
`Agent.Runtime.Compaction` checkpoint type remains separate so turn-state
modules do not depend on provider assembly. `Agent.Runtime.Session.History`
owns live-conversation operations and persisted model-history reconstruction.
`Agent.CLI.Session.History` now exposes only `hydrateUiHistory`, which projects
persisted turns into terminal UI state. Provider and compaction regression tests
live beside their runtime owner; package checks reject the retired CLI provider
and compaction namespaces.

This is a reusable provider scope, not a complete frontend-neutral startup
entry point. CLI startup still chooses the provider and assembles session,
tools, persistence, and presentation. Server and native adapters still use that
CLI startup path; this extraction does not remove their CLI dependency.

## Implemented: shared tool assembly and startup ownership

`Agent.Runtime.Tools.Dialects` owns Codex/Grok coding-tool assembly and
filtering. Frontends supply plan, secret, and image hooks; the shared module
does not import CLI options or terminal presentation. CLI and server consumers
use this module directly rather than a CLI facade.

`Agent.Runtime.Tools.Resources` owns session teardown domains, and
`Agent.Runtime.Tools.Startup` acquires the five tool domains concurrently with
context preload. Typed `Acquire` inputs retain each completed resource in its
domain; failed or cancelled startup joins sibling acquisitions before scopes
unwind. Shutdown order remains activities, code mode, session lock, computer
use, LSP, web fetch, MCP, coding tools, scratch storage.

This is a bounded extraction, not the complete session entry point. Concrete
host-specific tool groups, terminal hooks, and session launch
still live in CLI orchestration. Server still depends on `agent-cli` until
those remaining composition boundaries move. No new Cabal package is needed.

## Implemented: shared session resource preparation

`Agent.Runtime.Session.Preparation` owns deferred persistence creation and
resumed-session retargeting through a typed `PersistenceRequest`.
`Agent.Runtime.Session.Resources` restores task plans and image history,
allocates scratch storage, and owns temporary-directory leases and cleanup.
Partial acquisition failures and cancellation unwind the same resource scope.

CLI supplies resume notices, fullscreen history wiring, external-session tool
construction, and a repository worktree lease hook. CLI options are translated
at the adapter boundary; the runtime request contains no terminal or CLI types.
Stale-resource housekeeping remains in the host composition layer.

## Implemented: shared MCP startup

`Agent.Runtime.Mcp.Startup` maps harness configuration into MCP server
configuration and owns blocking/progressive fleet acquisition and lease cleanup.
Host hooks are cleared when startup fails or is cancelled and after lease release.
The CLI adapter retains progress rendering, pending notices, interactive
elicitation, integration endpoint discovery, and stale-resource housekeeping.
Protocol and transport implementations remain in `agent-mcp`; no new package is
introduced for this session-integration layer.

## Remaining: move session composition and ownership out of the CLI

### Conversation-state ownership

`Agent.Runtime.ConversationStore` now owns transcript residency, checkpoint
hydration, attachments, and revision-fenced provider continuation. The former
CLI module is only a compatibility reexport. Its concurrency and eviction tests
live in the runtime package.

`Agent.Runtime.SessionState` groups the conversation reference, pending startup
and Grok context, usage, last assistant, and installed compaction boundary.
The production host constructs it once, retaining the existing reference
lifetimes; `SessionEnv` and turn execution share that same state. Patch commits,
startup consumption, and consumed-context restoration now execute in the runtime,
without terminal dependencies. Transcript writes still invalidate continuation,
and restoration preserves concurrently refreshed context.

This is a mutable conversation owner, not a scheduler or an atomic transaction
across references. Persistence and frontend composition still belong to the host.

### Opaque session state

`SessionState` no longer exports its constructor or mutable fields. Turn
execution and frontend consumers use runtime operations for transcript
residency, continuation/attachment reads, usage accumulation, last-assistant
reads, compaction boundaries, and consumed prompt context. This keeps the
existing ordered patch protocol in the runtime rather than duplicating it in
frontends. Usage deltas remain individually atomic; this does not introduce
cross-field transactions or change the exception/commit ordering above.

`newSessionStateWith` remains a compatibility construction boundary: the host
supplies conversation, generated-startup-context, usage, and compaction
references whose lifetimes can span provider restarts. `restartSessionState`
preserves those references while allocating fresh per-run framing and
last-assistant state. The previous run must have stopped before it is rebuilt.

The one reference-returning migration seam, `borrowConversationRef`, is limited
to legacy attachment and model-selection helpers that still accept the host's
conversation slot. Ordinary transcript reads and turn execution do not use it.
Removing this seam requires migrating those helpers and their startup callers
together; encapsulation does not yet prevent a legacy owner from replacing the
conversation slot. Pending-state references are not otherwise exposed.

These extractions provide turn execution, process resources, and conversation
state, not a complete headless session owner. The dependency graph still includes
`agent-server -> agent-cli -> agent-runtime`. The server still calls
`Agent.CLI.NativeRuntime.runNativeTurn`, which lowers native requests into
`CliOptions` internally. The public native hook uses `nativeStartupPolicy`
rather than exposing those options.
Startup, tool composition, turn preparation, terminal state in `SessionEnv`,
normal persistence, and auxiliary rollback application remain in the CLI.
Server sandbox option restrictions remain on the existing path.

Removing that edge requires extracting those capabilities together; merely
moving `runNativeTurn` or renaming its options would hide the dependency rather
than invert it. This step intentionally stops before that larger migration.

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

## Conversation-state validation

The focused runtime suite passes in inherited Nix GHCi: 48 examples across
`ConversationSessionSpec`, `ConversationStoreSpec`, `TurnExecutionSpec`,
`TurnEngineSpec`, `TurnStateSpec`, and `RequestSpec`. This includes the moved
hydration/race tests and new state isolation, usage, context restoration,
failed-stream commit, and exceptional compaction rollback checks.

Package-aware GHCi loads runtime, CLI, and server together successfully:
305 modules, with no failed loads or compiler errors. Package-boundary and
whitespace checks pass. Regenerating both affected `package.nix` files produced
no dependency changes.

An additional focused CLI test attempt did not execute: GHC multi-library mode
does not support `:add`. The runtime tests above pass, but the prior CLI test
results below are not a rerun of this conversation-state change.

## Previous executor/process validation

Inherited Nix GHCi passes all 26 examples in `TurnExecutionSpec`,
`TurnEngineSpec`, `TurnStateSpec`, and `RequestSpec` (9 new execution examples).
The five `SessionThreadsSpec` examples also pass in inherited Nix GHCi:
duplicate-running rejection, cancel/join of multiple workers, post-close
rejection, completion/relaunch, and returned failure/retry.
Package-boundary and whitespace checks pass; `package.nix` was regenerated
with the inherited Nix `cabal2nix`.

`Agent.Runtime.TurnExecutionSpec` exercises the real loop against scripted
providers without terminal setup: baseline execution/event parity, provider
failure before and after committed tools, attempt-local display discard with
reused call IDs, pending-approval cancellation, compaction checkpoint/retry,
model-history resume, and exceptional rollback ordering. Resume here means
replaying the model projection in memory with the finalized previous-response
patch applied (including chain invalidation), not a persisted frontend round trip;
compaction uses an installed checkpoint, not a live summarization provider.

`Agent.Runtime.NativeProcessSpec` covers cleanup-worker ownership and session-worker
joining through the shared process handle: all three examples pass through
`cabal repl --offline agent-runtime:test:agent-runtime-test`.
These tests do not establish full CLI/server parity.

The server's `SupervisorSpec` and `ApplicationSpec` pass through its Cabal test
REPL: 56 examples, zero failures, covering supervisor cancellation/join,
retry/shutdown, failure fencing, and WAI admission.
The full runtime suite was also executed (`:main` in its test REPL):
221 examples, 20 failures. All 20 fail during managed PostgreSQL fixture setup
because the inherited temporary directory produces an overlong Unix-socket
path; the other 201, including all new tests, pass.
CLI `AgentSessionsSpec` was executed in its test REPL: 28 examples, 24 failures
at the same PostgreSQL setup guard, four passes. These database-backed tests
still need a rerun with an explicitly permitted shorter temporary directory.
They are not reported as passing, and no test or production guard was weakened.

Focused CLI `TurnSpec`, `NativeRuntimeSpec`, `RequestSpec`, and `CompactionSpec`
pass together in the CLI test REPL: 120 examples, zero failures. These cover
failed display/model separation, native lowering/resume, cancellation rollback,
compaction-hook ordering, and provider compaction failures; they do not replace
the blocked database-backed session tests.

Package-aware multi-library GHCi also loads CLI, runtime, and server together
successfully (288 modules). Local validation uses the inherited Nix toolchain:
`nix develop` itself still fails with a permission error inspecting
`~/.haskell-agent/tmp/sessions`. The initially missing pinned Hermes checkout
was fetched by normal Cabal resolution. The ignored OpenAI prompt/model data
files were installed using the flake shell hook's commands from the exact
flake-pinned Nix store assets, with hashes checked against the flake.
No access restriction was bypassed or dependency stub introduced.

Reproduce the package-aware load from the repository root in the Nix toolchain:

```sh
printf ':show modules\n:quit\n' | cabal repl --offline \
  agent-runtime:lib:agent-runtime \
  agent-cli:lib:agent-cli agent-server:lib:agent-server
```

For the database-backed rerun, first provide a permitted short `TMPDIR`, then
open `cabal repl --offline agent-runtime:test:agent-runtime-test`
and run `:main`. For CLI session integration, open
`cabal repl --offline agent-cli:test:agent-cli-test`, import
`Test.Hspec` and `Agent.CLI.AgentSessionsSpec` qualified, and run
`Test.Hspec.hspec Agent.CLI.AgentSessionsSpec.spec`.
