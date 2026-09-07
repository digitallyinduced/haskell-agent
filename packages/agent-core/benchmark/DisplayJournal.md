# Display-journal updates

Build and run inside the repository Nix development shell:

```sh
nix develop
cabal build --offline agent-core:bench:display-journal-bench
bin=$(cabal list-bin agent-core:bench:display-journal-bench)
for mode in old new; do
  "$bin" "$mode" tools-failure 100 64 7
done
```

The benchmark compiles production `Agent.Loop.DisplayJournal`, a frozen copy of
the original list algorithm, and their shared types with the same `-O2` flags.
The baseline comes from commit `1d17af7e9`.

Arguments are implementation, workload, count, ASCII payload bytes, and sample
count. Each sample runs twenty independently salted fixtures. Fixtures and their
payloads are forced before measurement. Full output equality and 6,464
mixed-sequence prefix comparisons run outside measurement.
Run `"$bin" --verify` to run only those semantic comparisons. The mixed inputs
use 64 fixed seeds of a Word64 linear congruential generator, each producing
100 events, including duplicate starts/finishes and repeated discards.

The timed path uses the same strict `IORef` journal updates, attempt discard,
success clearing, and failed-output projection as the loop admission path.
Failure results are fully consumed, including their retained text/tool payloads;
success clears the journal without projecting it. This distinction matters:
the old list's lazy filtering can defer work until failed output is observed.
The benchmark does not include provider, event-pump, terminal, or storage time.
`LoopEvents.hs` provides a separate integrated loop streaming/failure workload.

CSV columns: implementation, workload, count, payload bytes, samples, median
elapsed milliseconds/operation, median CPU milliseconds/operation, median
allocated bytes/operation. RTS statistics are enabled by default (`-T`).
Collections bracket each sample; the final collection updates nursery allocation
statistics but is excluded from both clocks.

## Workloads

- `text-failure`, `text-success`: `count` adjacent text deltas.
- `text-after-tool-failure`, `text-after-tool-success`: one tool start followed
  by `count` adjacent deltas, guarding the text fast path in a tool-bearing turn.
- `tools-failure`, `tools-success`: `count` retained tool starts, sixteen rounds
  of alternating argument/metadata snapshots and output snapshots for every
  tool, then one finish per tool. Snapshot arguments and output differ in each
  round. Each tool has 34 total events.
- `retry-retract`: two copies of the tool workload separated by a restart, with
  the same call IDs reused; retract every second-attempt tool, append text,
  discard the current attempt, then append text again.

Use small tool counts (1, 4, 16), larger counts (100, 1,000), and text counts
(1,000, 10,000). Repeat representative cases to check stability, and include
larger payloads to separate indexing costs from text-copy costs.

## Final newest-first projection

The final implementation keeps cheap raw event/chunk admission and performs
one newest-first projection, using seen-call sets for update/output slots.
It avoids both per-event admission indexes and chronological reconstruction.
On macOS/aarch64, GHC 9.10.3, `-O2`, default RTS allocation area, seven samples
of twenty independently salted operations produced:

| Workload | Count | Bytes | Wall ms, old → new | CPU ms, old → new | Allocated bytes, old → new |
|---|---:|---:|---:|---:|---:|
| tools-failure | 1 | 64 | 0.00140 → 0.00070 | 0.00135 → 0.00070 | 8637 → 2365 |
| tools-success | 1 | 64 | 0.00025 → 0.00030 | 0.00025 → 0.00025 | 3365 → 1765 |
| tools-failure | 16 | 64 | 0.24545 → 0.01805 | 0.24590 → 0.01805 | 1439757 → 41005 |
| tools-success | 16 | 64 | 0.00325 → 0.00305 | 0.00320 → 0.00305 | 51725 → 26125 |
| tools-failure | 100 | 64 | 16.44030 → 0.22290 | 16.42930 → 0.22290 | 81168621 → 283981 |
| tools-success | 100 | 64 | 0.03810 → 0.04185 | 0.03800 → 0.04185 | 322541 → 162541 |
| text-failure | 1000 | 16 | 0.03450 → 0.03375 | 0.03445 → 0.03365 | 104405 → 88317 |
| text-success | 1000 | 16 | 0.00830 → 0.00780 | 0.00820 → 0.00775 | 64141 → 48141 |
| text-failure | 10000 | 16 | 0.46710 → 0.46525 | 0.46705 → 0.46490 | 1040405 → 880317 |
| text-success | 10000 | 16 | 0.16255 → 0.14655 | 0.16215 → 0.14630 | 640141 → 480141 |
| text-after-tool-failure | 1000 | 16 | 0.03660 → 0.03480 | 0.03655 → 0.03480 | 104621 → 88429 |
| text-after-tool-success | 1000 | 16 | 0.00820 → 0.00705 | 0.00820 → 0.00705 | 64181 → 48165 |
| retry-retract | 16 | 64 | 0.48800 → 0.05775 | 0.48795 → 0.05770 | 2891925 → 299269 |
| tools-failure (repeat) | 16 | 64 | 0.24835 → 0.01690 | 0.24800 → 0.01690 | 1439757 → 41005 |

The 100-tool failure workload is about 74× faster with 99.65% less allocation.
The one-tool failure allocation regression of the earlier prototype is gone.
Successful 100-tool timing is noisy: three additional new-then-old invocations
with eleven samples gave wall ms old → new of 0.05220 → 0.06865,
0.09015 → 0.09295, and 0.07600 → 0.06530 (CPU: 0.05215 → 0.06860,
0.09015 → 0.09115, 0.07595 → 0.06505). Allocation consistently fell from
322541 to 162541 bytes; these timing samples do not establish a success-path
speedup. Sub-microsecond differences in the smallest cases are not meaningful.

Reproduce the final matrix:

```sh
"$bin" --verify
for case in \
  "tools-failure 1 64" "tools-success 1 64" \
  "tools-failure 16 64" "tools-success 16 64" \
  "tools-failure 100 64" "tools-success 100 64" \
  "text-failure 1000 16" "text-success 1000 16" \
  "text-failure 10000 16" "text-success 10000 16" \
  "text-after-tool-failure 1000 16" "text-after-tool-success 1000 16" \
  "retry-retract 16 64" "tools-failure 16 64"; do
  for mode in old new; do "$bin" "$mode" $case 7; done
done
```

### Retained heap and lifetime boundary

`--retain old|new TOOLS SNAPSHOTS BYTES` streams individually forced, distinct
output snapshots round-robin directly into the journal. No input list remains
rooted. GC live bytes are measured after admission, after fully forcing output
while retaining the journal, and after clearing the journal while keeping output
and the empty reference alive. The last phase models releasing the journal root;
it does not require clearing the production journal during error finalization.

```sh
for tools in 1 16; do
  for snapshots in 100 1000; do
    for mode in old new; do
      "$bin" --retain "$mode" "$tools" "$snapshots" 65536
    done
  done
done
```

CSV columns: mode, `retained`, tools, total snapshots, bytes/snapshot, live bytes
after admission, live bytes after projection, live bytes after clear, checksum.
Checksums match for each old/new pair.

| Tools | Snapshots | Admission live bytes, old → new | Projected with journal rooted, old → new | After clear, old → new |
|---:|---:|---:|---:|---:|
| 1 | 100 | 6574440 → 6569624 | 101912 → 6570736 | 102896 → 71016 |
| 1 | 1000 | 65715256 → 65667240 | 101928 → 65668352 | 102912 → 71032 |
| 16 | 100 | 6582168 → 6577112 | 1090712 → 6578464 | 1090496 → 1058616 |
| 16 | 1000 | 65770248 → 65721992 | 1090728 → 65723344 | 1090512 → 1058632 |

There is **no claim of reduced snapshot retention before projection**: both
versions retain roughly the raw payload history here. The new journal retains
superseded snapshots while its raw root stays alive even after projection;
releasing that root leaves only the projected output. Production must preserve
the journal until late manager/event-pump failure handling finishes, because
those paths may reconstruct the execution result. These numbers are journal
diagnostics, not end-to-end loop memory or latency measurements.

## Rejected chronological-map projection prototype

The second prototype admitted raw events/chunks and normalized tool snapshots
into chronological maps only when projecting failed output. It passed the same
6,464 differential
prefixes. Seven samples of twenty operations, on the same machine/toolchain:

| Workload | Count | Bytes | Wall ms, old → deferred | Allocated bytes, old → deferred |
|---|---:|---:|---:|---:|
| tools-failure | 1 | 64 | 0.00140 → 0.00145 | 8637 → 13669 |
| tools-success | 1 | 64 | 0.00030 → 0.00030 | 3365 → 1765 |
| tools-failure | 16 | 64 | 0.26240 → 0.06345 | 1439757 → 381181 |
| tools-success | 16 | 64 | 0.00480 → 0.00385 | 51725 → 26125 |
| text-after-tool-failure | 1000 | 16 | 0.04245 → 0.03900 | 104621 → 88477 |
| text-after-tool-success | 1000 | 16 | 0.00945 → 0.00890 | 64181 → 48165 |

The success/text admission regressions were removed, but the one-tool failure
allocation increase remained; the final newest-first projection replaced it.
The following retained-heap results belong to that rejected prototype:

```sh
for tools in 1 16; do
  for snapshots in 100 1000; do
    for mode in old new; do
      "$bin" --retain "$mode" "$tools" "$snapshots" 65536
    done
  done
done
```

This separate-process diagnostic streams distinct, fully forced 64-KiB output
snapshots round-robin across the requested tools. No input list remains rooted.
The historical diagnostic collected after admission, then after forced projection while deliberately
keeping both the journal and its output alive. Its CSV columns were implementation,
`retained`, tool count, total snapshot count, bytes/snapshot, live bytes after
admission, live bytes after projection, and output checksum.

| Tools | Snapshots | Admission live bytes, old → deferred | Projected live bytes, old → deferred |
|---:|---:|---:|---:|
| 1 | 100 | 6574440 → 6569624 | 101912 → 6602616 |
| 1 | 1000 | 65715256 → 65667240 | 101928 → 65700232 |
| 16 | 100 | 6582168 → 6577112 | 1090712 → 6610344 |
| 16 | 1000 | 65770248 → 65721992 | 1090728 → 65755224 |

Both implementations retain roughly the entire raw snapshot history before
projection in this workload. Projection evaluates and releases the old lazy
filter chain, but the deferred prototype retains raw history if the journal
itself remains rooted. This is a diagnostic of that lifetime, not a claim that
the real loop retains its journal after returning failed output.

## Rejected eager-index prototype

The initial prototype promoted every tool-bearing attempt into strict `IntMap`
and call-ID indexes immediately. On macOS/aarch64, GHC 9.10.3, `-O2`, default RTS
allocation area, seven samples of twenty operations, it showed the following
old → prototype results:

| Workload | Count | Bytes | Wall ms | CPU ms | Allocated bytes |
|---|---:|---:|---:|---:|---:|
| tools-failure | 1 | 64 | 0.00155 → 0.00155 | 0.00155 → 0.00150 | 8445 → 14533 |
| tools-success | 1 | 64 | 0.00030 → 0.00120 | 0.00025 → 0.00115 | 3365 → 14125 |
| tools-failure | 16 | 64 | 0.26170 → 0.06655 | 0.26150 → 0.06655 | 1436685 → 435381 |
| tools-success | 16 | 64 | 0.00350 → 0.05930 | 0.00345 → 0.05935 | 51725 → 428853 |
| tools-failure | 100 | 64 | 16.49830 → 0.68720 | 16.48230 → 0.68350 | 81149421 → 3507221 |
| tools-success | 100 | 64 | 0.08620 → 0.65020 | 0.07780 → 0.64930 | 322541 → 3466421 |
| text-after-tool-failure | 1000 | 16 | 0.03715 → 0.04465 | 0.03730 → 0.04460 | 104493 → 240629 |
| text-after-tool-success | 1000 | 16 | 0.00950 → 0.01280 | 0.00955 → 0.01315 | 64181 → 200381 |

The failure-path improvement does not justify these success-path and
text-after-tool regressions. Eagerly building indexes shifts work from failed
projection into every successful turn; updating the text entry inside the map
also adds per-delta allocation. These prototype numbers are diagnostic, not a
claim about the final implementation.

## Integrated loop validation

The unchanged `LoopEvents.hs` driver was also built against baseline
`1d17af7e93e4760a4e4c25b584d080f0808b700f` and the final journal, using separate
Cabal projects with `optimization: 2`, GHC 9.10.3, and identical local dependency
sources. Unlike the direct journal driver, these runs include the event pump,
backend callback, and loop lifecycle. `streaming-failure` forces the retained
`executionUncommittedDisplayEvents`; `streaming` follows the successful path.
There is no provider/network request or UI rendering.

Each cell is old → new. Times are independently selected medians of seven runs,
in milliseconds; allocation is bytes. Sink delay is zero. The repeat reverses
the old/new execution order.

| Workload | Deltas | Run | CPU ms | Wall ms | Allocated bytes |
|---|---:|---|---:|---:|---:|
| streaming | 10000 | first | 2.190 → 2.477 | 2.192 → 2.477 | 6778704 → 6620008 |
| streaming | 10000 | repeat | 2.118 → 2.077 | 2.118 → 2.078 | 6777112 → 6617400 |
| streaming-failure | 10000 | first | 2.697 → 2.500 | 2.704 → 2.508 | 7272384 → 7112584 |
| streaming-failure | 10000 | repeat | 2.295 → 2.257 | 2.295 → 2.259 | 7272384 → 7113768 |
| streaming | 100000 | first | 33.830 → 33.490 | 34.261 → 33.510 | 67363272 → 65758064 |
| streaming | 100000 | repeat | 34.220 → 32.413 | 34.254 → 32.472 | 67361976 → 65755872 |
| streaming-failure | 100000 | first | 37.563 → 39.317 | 37.605 → 39.370 | 72358328 → 70751056 |
| streaming-failure | 100000 | repeat | 36.639 → 35.476 | 36.696 → 35.484 | 72358048 → 70752096 |

Integrated allocation consistently falls about 16 bytes per delta (2.2–2.4%).
Timing varies between runs: the first 10000-delta success and 100000-delta
failure runs regress, but neither regression reproduces in reverse-order
repeats. Treat integrated CPU as approximately neutral, not a claimed speedup.

To reproduce, build `agent-core:bench:loop-events-bench` in separate baseline
and candidate checkouts, with the same Nix development environment and
`--enable-optimization=2`, then run each resulting binary:

```sh
cabal build --offline --enable-optimization=2 agent-core:bench:loop-events-bench
BIN=$(cabal list-bin --enable-optimization=2 agent-core:bench:loop-events-bench)
for count in 10000 100000; do
    "$BIN" streaming "$count" 0 7 +RTS -T
    "$BIN" streaming-failure "$count" 0 7 +RTS -T
done
```

Final focused GHCi validation of `Agent.Loop.FailedDisplaySpec` and
`Agent.LoopSpec`: 107 examples, zero failures. Package-boundary checks,
`git diff --check`, and regenerated `package.nix` consistency also passed.
