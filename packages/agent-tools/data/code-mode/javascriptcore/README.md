# JavaScriptCore code-mode worker

This macOS backend uses the system JavaScriptCore framework through
its public C API. It is not a Bun embedding and does not embed JavaScript in the
Haskell host process. It is the automatic default on macOS; Linux retains Bun.
The rollout explicitly accepts measured warm-latency and CPU regressions in
exchange for faster cold execution and lower measured worker memory. It is not
a universal performance improvement or a claim of complete security isolation.

## Process and context ownership

`cbits/javascriptcore-worker.mm` builds the `agent-code-mode-worker` executable.
The Haskell host launches and supervises it through the existing code-mode
worker protocol: newline-delimited JSON-RPC on stdin/stdout, with diagnostics on
stderr. Tool calls, responses, streamed content, notifications, yields, and
execution completion still cross this process boundary.

Each worker accepts one active cell at a time and can be reused by the host's
worker pool. A fresh JavaScript global context is created for each cell within
the worker's execution context group. Cell globals, tool promises, and timers
are not reused. A separate trusted parser context retains Acorn and the module
lowerer. Only lowered source text crosses from that context to the execution
context; parser objects are not exposed to user code.

One native owner thread enters JavaScriptCore. Protected native references keep
the private driver and its methods alive until cell retirement. The driver
receives tool responses and timer ticks from the native event loop; completion
retires outstanding request identifiers so late responses cannot resolve a
later cell's promises.

## Module execution

The public C API used here evaluates scripts, not ECMAScript modules.
`lower-module.js` therefore parses input with the pinned Acorn distribution in
module mode before lowering it to a strict async arrow expression:

- Module parsing checks syntax and module early errors, including invalid
  top-level return and invalid exports.
- Static imports and re-exports requiring dependencies are rejected.
- Local exports execute their declarations or expressions, but no module
  namespace is exposed.
- Module-level `this` becomes `undefined`, including through lexical arrows;
  ordinary functions and class members retain their own `this`.
- `import.meta` becomes a private lexical, null-prototype metadata object.
- Dynamic imports remain engine syntax. No module loader is supplied, so they
  cannot load dependencies. Leaving the syntax intact preserves argument
  evaluation rather than replacing it with an ordinary function call.
- The async arrow allows top-level `await` without introducing an ordinary
  function's `arguments` binding.

This is a single-module compatibility implementation, not a general module
loader. Differential tests against Bun are required when changing the lowerer.

## Helpers and restrictions

`worker.js` installs the code-mode helpers: `tools`, `ALL_TOOLS`, `text`, `image`,
`audio`, `generatedImage`, `notify`, `exit`, `yield_control`, `store`, `load`,
`setTimeout`, and `clearTimeout`. Tool proxies check the supplied tool catalog
and return promises correlated with host response identifiers. Stored values
are JSON-serializable copies; reads return copies. Timers use the native
monotonic clock and are serviced by the worker event loop, with at most 4,096
registered timers per cell.

Native send and clock callbacks are private factory arguments rather than
global properties. The helper factory captures the intrinsics it needs before
user code runs. Helper globals are non-writable and non-configurable.

The execution realm does not provide Node/Bun modules or direct network and
filesystem APIs. Blocked globals include `console`, `process`, `global`,
`require`, `module`, `Buffer`, `fs`, `net`, `http`, `https`, `child_process`,
`fetch`, `WebSocket`, `Atomics`, `SharedArrayBuffer`, `WebAssembly`, and
`setInterval`. `eval`, `Function`, and constructor paths through ordinary,
async, generator, and async-generator function prototypes are replaced with
throwing functions.

These restrictions are defense in depth, not a claim of a complete security
sandbox. JavaScriptCore remains native engine code, and separate address spaces
do not by themselves impose operating-system filesystem or network permissions.
Do not weaken process isolation on the assumption that fresh realms alone
contain engine faults or hostile code.

## Termination and resource limits

The Haskell parent owns cancellation and termination. CPU-bound JavaScript can
block the worker's event loop; a JavaScript timer is not an execution watchdog.
The parent must terminate the worker for enforced execution deadlines or
cancellation. An observation/yield deadline is not an execution deadline and
does not itself stop a running cell.

A separate native watchdog samples worker resident memory approximately every
20 ms and exits with status 75 when RSS exceeds 512 MiB. This is a sampled
process RSS threshold, not a hard heap or allocation limit: transient overshoot
depends on allocation rate and scheduling. The watchdog never accesses
JavaScript values.

Individual incoming and outgoing protocol lines are bounded at 16 MiB; an
oversized line terminates the worker with status 74. This transport bound is
separate from the host's source-size limit. It does not cap all intermediate
JavaScript allocations or aggregate output over a cell's lifetime.

## Installation and selection

Dependencies and builds are managed by the repository Nix flake.
`nix/javascriptcore-worker.nix` pins Acorn and installs this layout:

```text
bin/agent-code-mode-worker
share/agent-code-mode-worker/acorn.js
share/agent-code-mode-worker/lower-module.js
share/agent-code-mode-worker/worker.js
share/licenses/agent-code-mode-worker/acorn-LICENSE
```

With no arguments, the executable resolves resources relative to its own
resolved executable location, not the working directory. `--check` uses the
same installed resources and checks parser/bootstrap initialization and a
small lowered execution fixture before exiting. It is an availability check,
not the complete backend test suite. Development invocations can instead pass
three explicit paths:

```text
agent-code-mode-worker ACORN LOWER_MODULE WORKER_JS
```

An automatically configured macOS host selects this worker without an environment
override. To select it explicitly, optionally with a non-default executable:

```sh
export AGENT_CODE_MODE_BACKEND=javascriptcore
export AGENT_CODE_MODE_WORKER=/absolute/path/to/bin/agent-code-mode-worker
```

The executable override is optional when `agent-code-mode-worker` is available
on the host search path. `AGENT_CODE_MODE_BACKEND=bun` explicitly selects Bun,
including on macOS. An unset backend variable selects JavaScriptCore on macOS
and Bun on Linux. An invalid backend value is an error. Explicit Haskell
`BunBackend` or `JavaScriptCoreBackend` configuration overrides the backend
environment variable. Selection is resolved once per host so pool replenishment
cannot silently switch implementations; a missing or broken selected backend
does not fall back to another one.

## Rollout tradeoffs and ongoing validation

The macOS default switch was approved with known regressions. In the final
optimized actual-host comparison, a no-pool empty cell improved from 39.4092 ms
with Bun to 28.2344 ms, but a pooled empty cell increased from 0.2794 to
0.4650 ms. A pooled 1 MiB tool call increased from 4.6175 to 6.9114 ms.
Eight of ten pooled workloads regressed; the 100-small-call and eight-concurrent
cell cases improved. Cold-start improvements do not erase these costs.

Separate three-repetition worker-only measurements showed lower native-worker
RSS and physical footprint, but higher active-workload CPU and more idle
interrupt wakeups from the memory watchdog. Those memory tests invoke Bun
without the host's `--smol` flag and are not whole-application memory measurements.
See the [benchmark record](../../../benchmark/javascriptcore/README.md) for
all final workload results, methodology, reproduction commands, and the distinct
historical in-process prototype results.

Continue validating compatibility, isolation, resource lifetime, packaging, and
supported engine versions independently. Benchmark cold and warm execution,
tool round trips, catalog and payload sizes, and relevant resource behavior
over multiple samples. Report regressions alongside gains; the approved
tradeoff is not permission to claim universal speed or allocation improvements.
