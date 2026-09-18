# MicroHs Code Mode experiment

Standalone feasibility experiment. **Bun remains the production Code Mode
runtime. This is not an agent backend option and is not safe for untrusted
source.** No production packages or tool registrations are changed.

## Run

From the repository root:

```sh
nix develop .#microhs-code-mode
python3 -B experiments/microhs-code-mode/experiment.py
python3 -B -m unittest discover -s experiments/microhs-code-mode -v
python3 -B experiments/microhs-code-mode/experiment.py --benchmark --samples 7
```

For an untracked working copy of these new files, use
`nix develop "path:$PWD#microhs-code-mode"` so Nix includes them. The dedicated
shell provides pinned MicroHs, the same Bun version as the main flake, and
Python. It does not build the GHC agent. Set `TMPDIR` to an existing temporary
directory if your environment does not already supply it.

The package pins upstream revision `455782164e75998b140d869c1b7cdde0c8a21508`
and preloads its bundled `base.pkg`. No native executable is compiled per cell.

Pass a trusted Haskell source file as the positional argument to run another
cell. It must export `main`; for example:

```haskell
module Main(main) where
import CodeMode

main :: IO ()
main = runCell $ do
    result <- callTool "fixture.echo" (JsonObject [("answer", JsonNumber "42")])
    emit result
```

The host recognizes only `fixture.echo`, `fixture.numbers` (an object with an
integer `count` from 0 through 10000), and `fixture.denied` (always returns an
error). These are local test functions, not real harness tools. The denial
fixture tests error propagation, **not** the production approval interface.

## Architecture

`experiment.py` is a disposable Python test host using only the standard
library. The evaluated code and its tool-call library are Haskell. This avoids
adding experimental dependencies or runtime selection to the production host
before feasibility is established.

Each cell gets a fresh MicroHs process and a temporary working directory under
`TMPDIR`. The host writes `Cell.hs`, invokes `mhs -q -r`, reads a `ready` frame,
dispatches sequential `tool/call` requests, collects `content` frames, and
awaits the `cell-1` completion. Source loading/typechecking occurs before
`ready`. Protocol frames use newline-delimited JSON-RPC 2.0. Exceptions during
execution become error responses; compiler failures appear on the diagnostic
path. Previously emitted content is retained if execution fails.

This deliberately implements only a **subset** of Code Mode semantics. Unlike
the Bun worker, source arrives as a file rather than an `exec` request, and
`emit` sends a raw JSON value rather than a production content block. There
is no worker pool, parallel tool dispatch, yield/resume, persistent store,
image/audio output, dynamic tool declarations, or production host integration.
The small JSON codec preserves numeric lexemes rather than using floating-point
arithmetic; it is experimental, not a replacement for the production codec.

## Safety boundary

Only run reviewed, trusted source. Clearing environment variables, using a
temporary directory, and restricting the host dispatcher do **not** sandbox
Haskell IO or FFI. A cell can still access the filesystem or invoke runtime
primitives directly. The test host supplies no credentials, does not load MCP
configuration, and cannot dispatch arbitrary real tools. It enforces a wall
timeout and stdout/stderr byte limits and terminates/joins the worker process
group on completion, error, or cancellation. It does not impose a heap limit.

On Darwin, an exited but unreaped process group can report `EPERM` when
signalled. Cleanup briefly waits for reaping and retries the signal; it does
not silently ignore permission failures for live groups.

Before exposing this to a model with real tools, enforce OS-level isolation,
memory limits and restricted compiler/runtime capabilities, integrate the
existing host approval path, and test cancellation and protocol compatibility
against the production host. A restricted import list alone is insufficient.

## Comparison method

Recorded measurements and recommendation: [RESULTS.md](RESULTS.md).

The integer-sum benchmarks below are decoder/traversal stress tests, not a
representative assessment of Code Mode. `benchmark_workflows.py` adds synthetic
API-shaped workflows: dependent ID lookup, record filtering/projection, sparse
field extraction, and combining independent responses. It checks tool arguments
and final output, alternates backends, and reports raw samples with medians.

```sh
python3 -B experiments/microhs-code-mode/benchmark_workflows.py --samples 7
```

These workflows compare the persistent native-decoder MicroHs prototype with
the actual fresh Bun worker, not the production Bun idle pool. Zero-delay runs
measure local end-to-end overhead; simulated tool waits are reported separately
from total latency. They are not measurements of live services. The MicroHs
baseline still converts the entire native JSON document into Haskell values,
even for sparse extraction. No opaque-handle optimization is implemented here.
Independent calls are executed sequentially for equivalence with the prototype;
this does not measure Bun's parallel-call capability or production throughput.

The benchmark performs one identical logical operation in each language:
request an array of integers from a fake host tool, sum it, and emit the sum.
Every output is checked against the expected checksum, preventing a failed or
skipped computation from being reported as a fast sample. Workloads contain
10, 1000, and 10000 integers; 1000 is repeated to check stability. Each row
reports medians over the selected sample count.

- `microhs-source`: fresh process with source loading/typechecking.
- `microhs-cached`: fresh process with a disk compilation cache populated by
  one excluded preparation run. This is **not** a persistent warm interpreter.
- `bun-worker`: fresh process running the actual repository Bun worker with
  its normal runtime flags. It does not measure the production idle pool.

`ready_ms` measures launch through readiness; `elapsed_ms` measures launch
through completion including the host round trip, excluding source-file
creation and process cleanup. `cpu_ms` measures child user plus system CPU,
including cleanup, excluding the Python host. These are end-to-end prototype
measurements, not isolated language-runtime benchmarks. The Haskell prototype
uses its own JSON parser and arbitrary-precision integer sum; Bun uses native
JSON and JavaScript numbers (all measured sums are exactly representable).
GHCi timings are not used. Allocation, peak memory, persistent-worker latency,
and model task success rates are not measured by this benchmark.

## Persistent REPL experiment

`persistent.py` is a separate trusted-source host, not a production backend.
It keeps one MicroHs interpreter alive and loads `CodeMode` once. Cells are
single-line IO expressions (use explicit `do { ...; ... }` braces), rather than
complete modules. Do not share an interpreter
between trust domains: IO state and interpreter state can survive a cell.
The adapter wraps each expression in `runCell` and waits for the REPL prompt
after completion. Compile errors and caught runtime errors permit another cell;
timeouts, cancellation, or protocol errors discard the worker. Create a new
instance afterward; failed cells are never automatically replayed. Cells must
not manipulate protocol IO, fork children, or alter REPL state directly.

Python usage (with this experiment directory on the import path):

```python
from experiment import temporary_directory
from persistent import PersistentMicroHs

with temporary_directory() as directory:
    async with PersistentMicroHs(directory) as worker:
        first = await worker.execute('callTool "fixture.echo" (JsonNumber "42") >>= emit')
        second = await worker.execute('emit (JsonString "another cell, same interpreter")')
```

Run the comparison inside the same Nix shell:

```sh
python3 -B experiments/microhs-code-mode/benchmark_persistent.py --samples 7
```

This reports one cold REPL initialization separately, then seven samples per
workload in the same interpreter, including a repeated medium workload.
Fresh MicroHs and fresh Bun are rerun as controls. Warm-cell elapsed time
includes submission, typechecking/evaluation, host tool round trips, and
completion synchronization, but excludes interpreter initialization. This is
not a comparison against Bun's production idle pool. Raw elapsed samples are
included; per-cell CPU, allocations, and peak memory are not measured here.

## Native JSON FFI experiment

This variant retains the Python host and JSON-over-stdio protocol. It tests
calling yyjson from MicroHs through the C FFI, not embedding MicroHs into the
production GHC host or removing serialization. Only response decoding changes;
the JSON value representation, Haskell arithmetic, and output encoder remain
the same. `callToolWithDecoder` selects the response decoder per call.

The pinned MicroHs interpreter requires native functions to be registered in
its compiled FFI table. The separate `mhs-native-json` executable supplies those
registrations; the original `mhs` remains available as the baseline.

Inside the experiment's Nix shell:

```sh
MICROHS_NATIVE_INTERPRETER=mhs-native-json python3 -B -m unittest discover \
  -s experiments/microhs-code-mode -v
python3 -B experiments/microhs-code-mode/benchmark_native.py \
  --interpreter mhs-native-json --samples 7
```

The decoder comparison alternates execution order in one persistent interpreter
and checks every result. It reports median end-to-end elapsed milliseconds for
10, 1000, and 10000 integers, then repeats 1000. This includes conversion of the
native parse tree into Haskell values; it is not yyjson's parsing time alone.
Interpreter initialization is reported separately. No allocation, memory, or
per-cell CPU improvement is claimed. The same trusted-source restrictions as
the original REPL apply; an FFI is not a security boundary.
