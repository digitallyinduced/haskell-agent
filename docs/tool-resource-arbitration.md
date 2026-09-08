# Shared tool-resource arbitration

`Agent.Tools.ResourceArbiter` is a cooperative, in-process service. The CLI
process and native process runtime each own one authority. Session tool
environments borrow it, including in-process background sessions; subagents
inherit it rather than allocating an independent authority. Separate runtime
handles and OS processes do not coordinate with one another.

## Implemented boundary

`withSharedToolResourceClaims` opts a handler into shared arbitration while
retaining its existing turn-local claims. Production consumers are `read_file`,
`list_dir`, `grep`, local image readers, and the Codex-dialect `apply_patch`
implementation. Their existing resolvers produce absolute allowed paths:
existing paths are canonicalized, while missing suffixes remain lexical beneath
a canonical existing ancestor. Root and child relative paths use their own
canonical workspace cwd; private temporary-path aliases resolve to the owning
session's actual temporary directory.
An entire claim set is acquired together; overlapping reads may run together,
and unrelated worktrees remain independent.

The arbiter preserves admission order between conflicting claims without
blocking unrelated claims behind them. Active claims plus waiting requests are
bounded (1,024 per production authority); capacity exhaustion fails the tool
visibly instead of retaining an unbounded waiter. Claim admission and removal
are exception-safe. Cancellation removes a waiting request or releases the
running handler's lease. Model-asynchronous tool calls hold the lease until the
actual handler completes, not merely until the provider continues its response.

Loop approval and initial filesystem-root resolution happen before acquiring resources.
Closing an authority wakes blocked requests and rejects further admission,
but does not revoke running leases. Existing scoped worker owners remain
responsible for cancellation and joining before releasing their other resources.
The arbiter itself allocates no worker threads.

No tool schemas, provider dialects, approval rules, or sandbox permissions are
changed. Handler wrapping preserves canonical argument decoding, progress
events, rich results, and asynchronous exception propagation.

## Deliberate limits and next steps

This is not a claim that all tool effects are serialized:

- Managed shell commands, `write_stdin`, and GHCi can return while underlying
  processes remain alive. They are **not** opted into handler-scoped leases.
  Their supervisors need an explicit lease transfer/completion contract before
  they can participate safely. Shell writes can therefore still race these
  filesystem tools.
- Desktop input, Git/worktree operations, MCP tools, sandbox replacements, and
  other handlers need appropriately scoped explicit claims and lifetime tests
  before adoption.
- `TurnSequential`/`ToolExclusive` retain their original turn-local meaning.
  Promoting these defaults to a global lock would deadlock a parent waiting for
  a child whose tool needs the same lock. Collaboration control operations do
  not claim arbitrary execution resources.
- Claims are advisory within the participating authority, not an OS security
  boundary. External editors, other processes, hard-link aliases, and filesystem
  mutations outside participating handlers are not fenced. Existing handler
  path authorization remains authoritative and is not bypassed by a lease.
  Missing-path lexical aliases and path topology changes between claim resolution
  and handler execution are not a physical-identity exclusion guarantee either.
- For isolated execution hosts, resources must refer to the actual host/path
  identity rather than a coincidentally equal path inside two sandboxes.
  The current opt-in wrappers are host tool implementations; replacement
  sandbox tools are not implicitly enrolled.

The next expansion should attach leases to managed-process ownership, including
yield, completion, cancellation, retained pipe descendants, and process shutdown.
Do not broaden the opt-in merely because a tool has turn-local resource claims.

## Validation

`Agent.Tools.ResourceArbiterSpec` covers two real loops sharing an authority,
blocking and model-asynchronous handler lifetime, cancellation while waiting,
read sharing, independent worktrees, cancellation of held leases, conflicting
request fairness, capacity rejection, close, and independent authorities.
The existing filesystem and dialect suites cover the opted-in handlers.

Development checks use GHCi:

```sh
cabal repl agent-core:lib:agent-core \
  agent-codex-dialect:lib:agent-codex-dialect \
  agent-cli-runtime:lib:agent-cli-runtime agent-cli:lib:agent-cli
cabal repl agent-core:test:agent-core-test
# :main --match "shared tool resource arbiter"
# :main --match grepTool
# hspec (ReadFileSpec.spec >> ListDirSpec.spec >> ShowImageSpec.spec >> ViewImageSpec.spec)
cabal repl agent-codex-dialect:test:agent-codex-dialect-test
# :main
cabal repl agent-cli-runtime:test:agent-cli-runtime-test
# :main --match "shared native process resources"
```

The production load checks all four changed libraries together. Run test
components separately: GHC 9.10 multi-unit GHCi does not support `:main`.
Fresh worktrees need the development shell's pinned upstream data-file setup
(`nix develop`) before loading the provider libraries.
