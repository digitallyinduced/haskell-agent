# Initial results

Measured on 2026-09-12, Apple M3 Max, macOS 26.6.1, arm64. MicroHs
0.16.6.0 at the revision pinned in `nix/microhs.nix`, Bun 1.4.0, Python
3.14.7. Command: `python3 -B experiments/microhs-code-mode/experiment.py
--benchmark --samples 7` inside the dedicated Nix shell.

Seven samples per row, median milliseconds. Every sample checks its sum and
tool-call count. See README for timing boundaries and comparison caveats.

| Backend | Integers | Ready | Complete | Child CPU |
|---|---:|---:|---:|---:|
| MicroHs source | 10 | 616.530 | 618.334 | 622.025 |
| MicroHs disk cache | 10 | 9701.729 | 9703.299 | 9676.302 |
| Bun worker | 10 | 23.599 | 27.444 | 28.959 |
| MicroHs source | 1000 | 629.390 | 749.576 | 751.355 |
| MicroHs disk cache | 1000 | 9507.804 | 9627.212 | 9605.126 |
| Bun worker | 1000 | 21.608 | 24.955 | 27.533 |
| MicroHs source | 10000 | 626.352 | 5494.884 | 5476.559 |
| MicroHs disk cache | 10000 | 9540.103 | 14213.505 | 14158.799 |
| Bun worker | 10000 | 22.783 | 27.139 | 29.325 |
| MicroHs source (repeat) | 1000 | 617.588 | 741.049 | 740.204 |
| MicroHs disk cache (repeat) | 1000 | 9485.888 | 9605.305 | 9577.220 |
| Bun worker (repeat) | 1000 | 22.206 | 26.030 | 28.663 |

## Interpretation

The tool-call design is feasible: Haskell cells can invoke host functions,
consume their JSON responses, and emit results. Static type errors occur before
tool effects. This is not yet a viable drop-in replacement for Bun.

The current fresh-process prototype is substantially slower than the actual
Bun worker, even without measuring Bun's production idle pool. Source loading
accounts for most small-cell latency. The large-array case also exposes a
substantial execution/codec cost, which needs profiling before attributing it
to the evaluator. No memory or allocation advantage has been measured.

The tested `-C` disk-cache path is a regression, around 9.5–9.7 seconds to
readiness; it is not a startup optimization in this configuration. Investigate
cache serialization/loading and source invalidation before using it. A
persistent interpreter is measured separately below; precompiled support-package
optimization remains unmeasured.

Recommendation: retain this as a standalone experiment, leave Bun unchanged,
and address startup, JSON throughput, and OS isolation before real-tool
integration.

## Persistent REPL follow-up

Same machine and versions, using `benchmark_persistent.py --samples 7` in the
dedicated Nix shell. One interpreter loads CodeMode once and runs all warm
samples sequentially. Fresh MicroHs and Bun controls are rerun alongside each
workload. Every sample verifies the tool-call count and returned sum.

REPL initialization took **19270.802 ms** (one observation, not a median),
including imports and a successful probe cell. Warm timings exclude this cost
and include expression compilation, tool round trip, evaluation, and return to
the REPL prompt. Fresh timings include process startup. This does not compare
against pooled Bun, nor measure per-cell CPU, memory, or allocations.

Median complete latency in milliseconds, seven samples per entry:

| Integers | Warm MicroHs REPL | Fresh MicroHs | Fresh Bun worker |
|---|---:|---:|---:|
| 10 | 163.419 | 628.037 | 26.210 |
| 1000 | 285.620 | 737.681 | 26.288 |
| 10000 | 5564.009 | 5416.737 | 26.992 |
| 1000 (repeat) | 284.732 | 745.972 | 25.476 |

Persistence reduces small-cell latency about 3.8x and medium-cell latency
about 2.6x, excluding initialization. It does not improve the large-array
workload in this run. The small cell remains about 6.2x slower than even a
fresh Bun worker. The repeated medium workload returns to similar latency
after the large workload, but this is not a long-running memory test.

Conclusion: keeping a REPL works and amortizes some overhead, but does not
make this implementation competitive with Bun. Profile per-expression
compilation/linking and the JSON/evaluation path next; these timings alone
do not isolate their contributions. Keep production Bun unchanged.

The full suite passed: **14 tests**, including multiple cells in one process,
recovery after compile/runtime errors and denied calls, timeout replacement,
JSON/prompt framing, and output limits. The adapter is trusted-source only,
not a sandbox. Direct cancellation is not separately tested; repeated
cancellation during cleanup is not shielded.

## Native JSON FFI (2026-09-12)

`benchmark_native.py --interpreter mhs-native-json --samples 7` compares the
ReadP decoder with yyjson 0.12.0 through statically registered C FFI wrappers.
The pinned MicroHs C runtime and wrappers use the upstream optimized build
(`-O3`). Both decoders run in the same persistent interpreter, alternating
order each repetition, with identical tool replies, Json values, and Haskell
summation. Every sample checked its result and tool-call count.

Median complete warm-cell latency, milliseconds (seven samples each):

| Integers | ReadP | Native yyjson + conversion | Speedup |
|---|---:|---:|---:|
| 10 | 165.230 | 164.948 | 1.00x |
| 1000 | 286.798 | 213.228 | 1.35x |
| 10000 | 5458.743 | 741.146 | 7.37x |
| 1000 (repeat) | 287.376 | 211.259 | 1.36x |

Initialization was 19886.215 ms (one observation), excluded from warm timings.
Nix build/environment setup is excluded entirely. Measurements include cell
compilation, stdio transport, native-to-Haskell conversion, evaluation, and
completion synchronization, not just native parsing. No CPU, allocation, or
memory measurements were taken. The optional sanitizer check did not run.

Replacing this prototype's ReadP decoder removes most of the large-array
latency, but leaves small-cell latency unchanged. This is evidence against
the decoder implementation, not Haskell JSON libraries in general. Native
conversion and arithmetic still execute through MicroHs; remaining time has
not been profiled. Bun was not rerun in this comparison (the prior fresh Bun
large-array median was 26.992 ms), so this is not a new head-to-head Bun test.

Validation: the existing 14 tests passed after decoder parameterization; the
optional native test passed separately, covering valid/invalid JSON, exact
numeric lexemes, Unicode and embedded NUL, duplicate keys, nesting limits,
dependent tool calls, denied calls, and subsequent recovery. Production Bun
is unchanged; this remains a trusted-source experiment.

## Representative synthetic workflows

`benchmark_workflows.py` replaces the integer-sum stress test with customer-ID
chaining, filtering 100/1,000 customer records and projecting two fields,
extracting only a cursor and the first event ID from a 1,000-record response,
and combining three independent responses. The independent calls are **serial
in both backends**: this adapter does not measure production Bun's concurrent
tool-call capability. These are plausible workflow shapes, not captured
production traces or a claim about the frequency of particular workloads.

The comparison retains the current native decoder's **full Haskell JSON-tree
conversion**, including unused values. It does not benchmark opaque handles.
Both languages receive identical fixture responses; every tool name, argument,
call count, and final result is checked. Fixture construction is outside timing;
host serialization, transport, worker parsing, expression compilation/evaluation,
and output handling remain inside. MicroHs is warm; actual Bun workers start
fresh using the same production worker script as previous measurements.

Run with:

```sh
nix develop "path:$PWD#microhs-code-mode" -c \
  python3 -B experiments/microhs-code-mode/benchmark_workflows.py --samples 7
```

Each backend is sampled seven times with alternating execution order, at zero
and 100 ms simulated latency per call; the 100-record filter is repeated.
`simulated_wait_ms` measures actual time spent awaiting the fixture delay,
and `non_wait_ms` subtracts that measured duration from each sample (not merely
the requested delay). It is **not CPU time**. Raw measurements and medians are
printed as JSONL. No allocation or memory claims follow from this benchmark.

Seven-sample run, same machine and native build as above. MicroHs startup was
20,152.763 ms (one observation, excluded). All 168 timed executions validated.
Times below are median elapsed milliseconds:

| Workflow | Response bytes | MicroHs, zero delay | Bun, zero delay | MicroHs, 100 ms/call | Bun, 100 ms/call |
| --- | ---: | ---: | ---: | ---: | ---: |
| ID chain, two calls | 77 | 158.286 | 27.070 | 358.483 | 229.975 |
| Filter 100 | 17,330 | 197.622 | 26.920 | 302.447 | 128.188 |
| Filter 1,000 | 176,030 | 544.379 | 28.861 | 651.926 | 129.814 |
| Filter 100, repeat | 17,330 | 200.262 | 26.901 | 298.140 | 128.400 |
| Sparse extraction, 1,000 events | 159,924 | 437.619 | 28.237 | 535.032 | 130.908 |
| Three independent calls, serial | 42 | 163.969 | 26.786 | 468.120 | 331.198 |

After subtracting measured sleep time, delayed MicroHs medians were
156.623 / 201.379 / 550.855 / 197.067 / 433.961 / 165.233 ms respectively;
Bun medians were 27.969 / 27.106 / 28.866 / 27.307 / 29.828 / 28.753 ms.
These support a roughly 130 ms excess for small orchestration cells in this
prototype, with additional response-size-dependent cost. They do not identify
which compiler/runtime/conversion stage accounts for the gap.

The first smoke attempted the same sparse workflow with a description repeated
16 rather than two times per event (481,924 response bytes); MicroHs exited
during the exchange, producing a broken-pipe error in the host. The smaller
159,924-byte variant above passed. The larger failure is not included as a
latency sample. A separate `getLine >>= print . length` diagnostic, without JSON
parsing, passed with a 160,000-byte input but exited with `ERR: stack overflow`
at 482,000 bytes. This exposes a line-input/evaluation limitation independently
of JSON conversion; it is not an exact failure-threshold measurement. Do not
interpret the table as evidence that large responses are robust.

Validation: the 18-test regression suite passed. After adding async-handler
error cleanup and its regression test, all four workflow tests passed, including
error propagation through both backends and subsequent MicroHs cell recovery.

### Readiness boundary, second run

This run overlapped regression tests and diagnostics. Its timings are retained
for diagnostic context only, not as controlled performance evidence. Use the
first run above for the benchmark comparison.

A second seven-sample run captured `ready_ms` without changing workloads.
All 168 executions again validated. MicroHs startup was 19,903.753 ms.
For MicroHs this boundary is submission through receipt of `runCell`'s ready
frame, before fixture calls. For Bun it is process launch through worker
readiness, **before submitting source**; these readiness numbers therefore
describe different stages and must not be compared as compilation timings.

| Workflow, zero delay | MicroHs elapsed | MicroHs ready | Bun elapsed | Bun ready |
| --- | ---: | ---: | ---: | ---: |
| ID chain | 153.807 | 152.691 | 26.275 | 22.751 |
| Filter 100 | 195.242 | 164.645 | 26.255 | 22.852 |
| Filter 1,000 | 543.332 | 219.565 | 27.777 | 22.821 |
| Filter 100, repeat | 198.653 | 168.192 | 25.810 | 22.174 |
| Sparse 1,000 | 431.425 | 158.741 | 27.787 | 23.263 |
| Three serial calls | 163.029 | 161.772 | 26.478 | 23.002 |

The small ID-chain cell spends nearly all elapsed time before this MicroHs
ready boundary. That localizes the delay before tool orchestration, but does
not separate parsing, typechecking, linking, runtime preparation, or transport.
For delayed cells, MicroHs readiness medians remained 154.839 / 166.622 /
217.386 / 162.775 / 157.130 / 158.901 ms in the same workflow order.
The second run's complete raw results are retained as
`$TMPDIR/microhs-workflows-ready-results.jsonl`; the first run is
`$TMPDIR/microhs-workflows-results.jsonl`.
