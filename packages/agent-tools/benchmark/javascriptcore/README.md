# macOS JavaScriptCore validation and experiments

This directory contains two distinct implementations and their validation tools:

- **Isolated worker:** `HostComparison.hs` and
  `worker-tests.mjs` exercise the native `agent-code-mode-worker`, including the
  real Haskell host for the former. The helper uses public system JavaScriptCore
  APIs in a separate process, fresh per-cell realms, and Acorn-backed module
  lowering. This is the backend selected by default on macOS.
  See the [native worker architecture and installation documentation](../../data/code-mode/javascriptcore/README.md).
- **Historical in-process prototype:** `JavaScriptCorePrototype.hs`,
  `Comparison.hs`, and `CompatibilityProbe.hs` retain the original bounded,
  trusted-input-only Haskell FFI experiment. It has no Bun process or IPC on its
  native path and is not the isolated worker implementation.

**macOS defaults to the isolated JavaScriptCore worker; Linux retains Bun.**
The rollout was explicitly approved with the measured warm-path regressions
below accepted as a tradeoff for faster cold execution and lower measured worker
memory. This is not a universal speedup or a claim that every performance
non-regression criterion passed. Set `AGENT_CODE_MODE_BACKEND=bun` to select Bun
explicitly; the host does not silently fall back after a native-worker failure.

## Validate the isolated worker

On macOS, from the repository root, build the installed helper and run its
availability check and worker fixtures:

```sh
validation_directory=$(mktemp -d "$TMPDIR/javascriptcore-validation.XXXXXX")
nix build .#agent-code-mode-worker --out-link "$validation_directory/native-worker"
"$validation_directory/native-worker/bin/agent-code-mode-worker" --check
bash packages/agent-tools/benchmark/javascriptcore/worker-tests.sh \
  "$validation_directory/native-worker/bin/agent-code-mode-worker"
```

The worker-test wrapper runs its fixture driver through `nix develop`. It also
accepts explicit `ACORN LOWER_MODULE WORKER_JS` paths after the executable for
development tests; the no-argument installed executable path above validates
resource discovery rather than relying on checkout-relative resources.

Benchmark both backends through the actual optimized Haskell host:

```sh
bash packages/agent-tools/benchmark/javascriptcore/host-comparison.sh
```

This script builds a GC-rooted Nix helper and an `-O2` `agent-tools` library plus
`HostComparison.hs`, then runs the comparison with `+RTS -N4`. It uses a minimal
Cabal project and offline dependency resolution inside `nix develop`, so cached
Cabal dependencies are required. Build products, the helper root, and results
remain in the printed artifact directory under `$TMPDIR`. Repeat measurements
before drawing conclusions. Preserve both improvements and regressions in the
acceptance record rather than reporting only cold-start results.

## Isolated-worker rollout measurements (2026-09-12)

The final actual-host comparison used optimized Haskell (`-O2`, `+RTS -N4`),
121 advertised tool names, checked/forced results, five warmup batches per
backend/workload, and seven alternating-order sample averages. Pool size zero
creates a process per cell; pool size two retains idle workers. Each sample
averages five batches without pooling or 30 with pooling. These measurements
include host protocol and tool dispatch, but not real network tools, approvals,
session persistence, or the application UI.

All times below are milliseconds per batch. A batch contains one cell except
the final row, which contains eight concurrent cells; **do not interpret that
row as single-cell latency**.

| Calls/cell | Payload bytes | Cells/batch | No pool: Bun / JSC | Pool 2: Bun / JSC |
|---:|---:|---:|---:|---:|
| 0 | — | 1 | 39.4092 / 28.2344 | 0.2794 / 0.4650 |
| 1 | 16 | 1 | 39.8408 / 29.6020 | 0.3644 / 0.8040 |
| 10 | 16 | 1 | 40.9758 / 29.7966 | 0.8096 / 1.1704 |
| 100 | 16 | 1 | 48.6198 / 35.2938 | 6.0826 / 5.4442 |
| 1 | 4096 | 1 | 40.1898 / 29.6222 | 0.4006 / 0.8745 |
| 10 | 4096 | 1 | 41.1386 / 31.3590 | 1.1019 / 1.5688 |
| 1 | 65536 | 1 | 40.8382 / 30.4830 | 0.6806 / 1.2536 |
| 10 | 65536 | 1 | 46.9124 / 36.3350 | 3.9008 / 5.4940 |
| 1 | 1048576 | 1 | 46.9406 / 38.5964 | 4.6175 / 6.9114 |
| 10 | 4096 | 8 | 83.8704 / 75.7814 | 76.7440 / 66.5836 |

The native worker improves every no-pool workload in this run, but is slower in
eight of the ten pooled workloads. For example, one warm 16-byte call rises
from 0.3644 to 0.8040 ms, and one warm 1 MiB call rises from 4.6175 to 6.9114 ms.
The 100-small-call and eight-concurrent-cell cases improve. These mixed results
were accepted for rollout, not characterized as removal of all code-mode overhead.

### Worker memory and CPU

`memory-comparison.py` measures fresh worker processes directly, separately from
the Haskell host benchmark. It runs three alternating repetitions with 121 tool
names, unique cell sources, no forced GC, and a 0.2-second post-workload idle
interval. The comparison invokes Bun directly without the host's `--smol` and
other startup flags; these measurements therefore do not establish exact
production-pool memory savings.

Reproduce after building the helper as above:

```sh
native_worker="$validation_directory/native-worker/bin/agent-code-mode-worker"
nix develop -c bash -c '
  python3 packages/agent-tools/benchmark/javascriptcore/memory-comparison.py \
    "$(command -v bun)" packages/agent-tools/data/code-mode/worker.mjs "$1"
' -- "$native_worker"
```

The corrected sample record reports the following medians. Memory is MiB
(bytes / 1,048,576); CPU is worker user + system seconds from kernel `wait4`
accounting, including process startup and shutdown, excluding the Python driver.
Idle-row CPU is **not** the CPU consumed solely during the idle observation.

| Workload | Post-workload RSS: Bun / JSC | Physical footprint: Bun / JSC | Peak RSS: Bun / JSC | Worker CPU seconds: Bun / JSC |
|---|---:|---:|---:|---:|
| Idle, no cells | 20.75 / 13.28 | 10.45 / 7.14 | 21.16 / 13.30 | 0.0286 / 0.0119 |
| 1,000 cells, one 16-byte call each | 50.55 / 39.52 | 27.45 / 13.48 | 50.81 / 39.53 | 0.2054 / 0.4389 |
| 100 cells, one 1 MiB call each | 74.11 / 36.19 | 20.33 / 13.77 | 74.34 / 36.20 | 0.3512 / 0.3999 |

RSS includes resident shared pages and must not be multiplied by worker count
to infer whole-application memory. Physical footprint is a distinct kernel
accounting measure, not allocated bytes. These tests do not measure allocation
volume or prove bounded memory over arbitrarily long sessions.

The native worker uses less measured memory here but more CPU on both active
workloads. Its 20 ms memory watchdog also increases idle interrupt wakeups:
128 versus Bun's 3 over each three-second idle observation. The Python driver's
`wall_seconds` includes driver work and is not an end-to-end latency benchmark.
The rollout accepts these CPU/wakeup and warm-latency costs; lower memory is not
evidence of a universal efficiency improvement.

Measurement sources: `javascriptcore-host-final-comparison.log` and
`javascriptcore-memory-corrected.jsonl` from the rollout validation. The tables
above preserve their reported values and derived medians rather than requiring
session-temporary artifacts to remain available.

## Historical prototype: how it works

One bound Haskell thread owns a reusable JSContextGroup and callback trampoline.
Each cell gets a fresh global context. An async-function wrapper permits `await`.
`tools.echo` creates a Promise, calls into Haskell, and queues protected resolver
references. Once evaluation returns, Haskell services the queue and resolves or
rejects each Promise through the C API. JavaScriptCore runs the continuation.
Payloads remain JSON, but there are no pipes, RPC envelopes or process switches.
Context cleanup releases pending references; the trampoline outlives the group.

## Reproduce the historical prototype

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

## Historical prototype measurement scope

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

## Historical prototype results (2026-09-12; not the isolated worker)

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

## Historical prototype compatibility findings

Run the bounded differential fixtures against the actual Bun worker and the
native prototype:

```sh
bash packages/agent-tools/benchmark/javascriptcore/compatibility.sh
```

The script builds only the small native probe through Nix, supervises each process
with a five-second deadline, and writes JSONL results beneath `$TMPDIR`. It exits
nonzero on any mismatch or baseline contract failure. It is separate from the
prototype smoke tests: a passing microbenchmark is not evidence of compatibility.
Only these trusted fixtures may be used with the unhardened prototype. This is a
small initial gate, not comprehensive conformance or security testing; errors are
compared as failure/success, not by message or exception type.

On 2026-09-12, Bun 1.4.0 versus system JavaScriptCore on macOS 26.6.1 produced
**11 differences in 15 fixtures**, with no baseline contract failures:

- The async wrapper changes top-level `this`, accepts top-level `return`, and
  rejects export declarations and `import.meta` that the module backend accepts.
- Direct/indirect `eval`, `Function`, and ordinary/async/generator/async-generator
  constructor aliases execute in the prototype but are rejected by Bun.
- Text, top-level await, top-level `typeof arguments`, and strict undeclared
  assignment agree in these fixtures.

Installed public JavaScriptCore headers expose script evaluation through
`JSEvaluateScript` (`JSBase.h`) and deferred Promises through
`JSObjectMakeDeferredPromise` (`JSObjectRef.h`), but no module evaluation API or
code-generation-disable setting. Exported SDK symbols such as
`JSGlobalContextSetEvalEnabled`, `JSContextGroupSetExecutionTimeLimit`, and
`JSScript` are not declared in the public headers; linker visibility is not a
supported API guarantee.

These probes originally changed no production configuration. Subsequent work now
adds host backend selection, the isolated helper, parser-backed lowering,
intrinsic restrictions, and Nix resource packaging; consult the native worker
documentation rather than treating the historical probe's missing features as
the current helper's implementation status.

The macOS default now uses the isolated implementation, **not this historical
prototype**. The findings above explain why direct substitution of the
in-process prototype was insufficient.
Regex replacement or removing only global `eval` and `Function` is insufficient.
The parser-backed implementation must be validated independently through the
worker fixtures and actual-host comparison commands above.

## Historical prototype limitations

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

For the isolated implementation, retain separate validation gates for semantic
compatibility, process termination and resource behavior, packaged-resource
discovery, supported macOS engine versions, and representative cold **and warm**
actual-host performance. The rollout accepts the documented performance
tradeoffs; it does not waive compatibility, isolation, or packaging validation.
