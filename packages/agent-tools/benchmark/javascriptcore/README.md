# macOS JavaScriptCore experiment

Standalone, trusted-input-only prototype. **Not wired into production code mode.**
It links Apple's system JavaScriptCore framework through Haskell's C FFI; no Bun
process is involved in the native path.

## How it works

One bound Haskell thread owns a reusable JSContextGroup and callback trampoline.
Each cell gets a fresh global context. An async-function wrapper permits `await`.
`tools.echo` creates a Promise, calls into Haskell, and queues protected resolver
references. Once evaluation returns, Haskell services the queue and resolves or
rejects each Promise through the C API. JavaScriptCore runs the continuation.
Payloads remain JSON, but there are no pipes, RPC envelopes or process switches.
Context cleanup releases pending references; the trampoline outlives the group.

## Reproduce

On macOS, from the repository root:

```sh
bash packages/agent-tools/benchmark/javascriptcore/run.sh
```

The script uses the existing Nix flake, builds the actual agent-tools library and
comparison executable with `-O2`, runs the semantic checks, and benchmarks twice
with `+RTS -N4`. Build artifacts and results go beneath `$TMPDIR`. The framework
comes from macOS, so its version is not pinned by Nix.

For interactive validation (not performance measurements):

```sh
mkdir -p "$TMPDIR/javascriptcore-ghci"
nix develop -c ghci -ignore-dot-ghci -framework JavaScriptCore \
  -stubdir "$TMPDIR/javascriptcore-ghci" \
  -ipackages/agent-tools/benchmark/javascriptcore \
  packages/agent-tools/benchmark/javascriptcore/JavaScriptCoreTests.hs
# :main
```

Tests cover output, awaited callbacks, queued Promise.all calls, delayed native
completion, Unicode and embedded NUL, fresh globals, JavaScript/native rejection,
syntax errors, unresolved Promises, native exceptions with pending siblings, and
recovery afterward. These are semantic smoke tests, not a security audit.

## Measurement scope

Comparison.hs runs the same source through the actual pooled Bun Host and this
prototype, checks/forces outputs, and uses an echo handler with JSON decoding and
encoding on both paths. Runtime startup is excluded; fresh cell/context setup,
execution, callbacks, output handling and cleanup are included. Each reported
number is the median of seven sample averages, each averaging 30 cells, after five
warmups per workload/backend. Backend order alternates between samples.

This is **not feature-equivalent** and not an end-to-end application measurement:
only two tool names are advertised to Bun and native defaults to echo/reject.
The native prototype has a fixed echo/reject catalog, not production tool dispatch.
Large tool catalogs, approvals, real network tools, database persistence, concurrent
cells and CPU/memory usage are not measured. Do not attribute the entire difference
to IPC or extrapolate directly to the application's reported 20 ms overhead.

## Results (2026-09-12)

Apple M3 Max, macOS 26.6.1 (25G76), system JavaScriptCore bundle version 21624,
GHC 9.10.3, Bun 1.4.0. Repository base: 434b6bcbc71afebc9636b9156635c41c2bc32f89.
All 14 semantic checks passed. Two independent benchmark runs:

| Calls/cell | Payload bytes | Bun ms/cell, runs 1 / 2 | JSC ms/cell, runs 1 / 2 |
|---:|---:|---:|---:|
| 0 | — | 0.1908 / 0.2366 | 0.0584 / 0.0769 |
| 1 | 16 | 0.2738 / 0.2679 | 0.0758 / 0.0864 |
| 10 | 16 | 0.7891 / 0.8621 | 0.1386 / 0.1413 |
| 100 | 16 | 6.0652 / 6.4684 | 0.6216 / 0.5931 |
| 1 | 4096 | 0.3095 / 0.2956 | 0.2593 / 0.2709 |
| 10 | 4096 | 1.0577 / 1.1085 | 1.6572 / 1.7069 |

Embedding is promising for small calls, but this prototype loses on repeated large
payloads. Its String/Text/JSON conversions remain unoptimized. These timings do not
reproduce a 20 ms warm empty-cell cost; trace the real application before deciding
which overhead to remove. Earlier exploratory runs showed substantial timing
variation, so treat these as local microbenchmarks, not latency guarantees.

## Before production

- Hard cancellation and memory limits: an infinite JS loop can hang this process;
  engine crashes/OOM are no longer isolated in a disposable Bun worker. Do not run
  arbitrary model-generated code here. Consider a small isolated JSC helper.
- Full host API: tool allowlists/dispatch, approval lifecycle, output limits,
  media, store/load, timers, yield/wait, exit and cancellation semantics.
- Module semantics: async-function wrapping is not ES-module evaluation and differs
  for imports/exports, return and top-level bindings.
- Thread-safe scheduling: runtime is single-owner and non-reentrant; queued native
  handlers are serviced serially, not concurrently.
- Validate resource lifetimes under cancellation and long-running workloads, engine
  version differences, signed app/JIT behavior and a non-macOS fallback.
