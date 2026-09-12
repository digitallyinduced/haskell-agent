# Adjacent display-journal snapshot retention

This benchmark compares the actual `Agent.Loop.DisplayJournal` at `f37672e5`
with the working-tree implementation. It measures the journal downstream of
provider event projection, not Conduit decoding, network latency, UI rendering,
or end-to-end agent performance.

```sh
nix develop -c env TMPDIR="$TMPDIR" \
  python3 packages/agent-core/benchmark/journal-admission-build.py
nix develop -c env TMPDIR="$TMPDIR" \
  python3 packages/agent-core/benchmark/journal-admission-run.py
```

The build uses GHC `-O2 -threaded -rtsopts`, compiling both journal algorithms
and their shared dependencies with identical flags. The runner uses
`+RTS -T -N1`, eleven samples in alternating old/new order, both paths warmed,
and two complete rounds. The build also runs the existing `DisplayJournal.hs`
6,464 mixed-prefix comparisons against its independent historical list
baseline (duplicate starts/finishes, retries, retractions, and discards).
Each measured workload's full projected output is compared outside timing.

## Workloads and measurement boundaries

- `arguments`: 32/128/256 independently allocated argument snapshots, growing
  by 256 ASCII bytes per event (8/32/64 KiB final snapshots). Their repeated
  character varies by event to prevent payload reuse. This models cumulative
  preview size and ownership, not valid tool JSON or provider parsing.
- `interleaved`: the same payloads alternating between two call IDs; this
  deliberately exercises the limitation of adjacent-only replacement.
- `text`: 1,000/10,000 independent 16-byte text deltas, guarding ordinary
  text admission against snapshot-specific overhead.

Each payload is generated and its character checksum forced on arrival, then
admitted through strict IORef updates, as in loop bookkeeping. Both consumers
pay the same generation/checksum work. There is no retained input event list.
CPU and wall time cover generation and admission, including natural GC but
not explicit boundary collections or failed-history projection. Allocation
is sampled after a full collection to include the final nursery.

`live_bytes` is whole-process live heap **after admission and a full GC, before
projection**, not peak RSS or whole-agent memory. The construction frame has
returned and the journal is kept alive by an IORef-backed projection closure
used after collection. Final output is also checksummed. Raw samples are in
`$TMPDIR/journal-admission-bench/final-*.csv`.

## Measured results

Local optimized run, September 12, 2026. Each row gives the two independent
round medians, old → new; milliseconds unless marked bytes.

| Workload | CPU, round 1 / round 2 | Wall, round 1 / round 2 | Live bytes |
| --- | --- | --- | --- |
| Arguments, 32 snapshots | 0.0934→0.0937 / 0.0934→0.0937 | 0.0931→0.0933 / 0.0931→0.0933 | 264,376→132,936 |
| Arguments, 128 snapshots | 1.412→1.411 / 1.410→1.410 | 1.412→1.415 / 1.412→1.413 | 2,256,608→157,552 |
| Arguments, 256 snapshots | 7.028→5.793 / 6.944→5.839 | 6.996→5.796 / 6.922→5.843 | 8,582,888→190,328 |
| Interleaved, 256 snapshots | 6.215→6.375 / 6.182→6.213 | 6.190→6.355 / 6.154→6.192 | 8,583,208→8,583,208 |
| Text, 10,000 deltas | 0.456→0.440 / 0.455→0.439 | 0.457→0.441 / 0.455→0.438 | 1,004,496→1,004,496 |

The largest adjacent-snapshot workload retained **97.8% less live heap** and
used **15.9–17.6% less CPU**. Allocation was identical: 8,508,760 bytes for
256 snapshots and 2,801,368 bytes for 10,000 text deltas. Interleaving defeats
the optimization and measured 0.5–2.6% slower in these rounds; no universal
speedup is claimed. The tiny text improvement is not a structural benefit:
the text algorithm is unchanged, and alternative dispatch layouts moved
these small timings in either direction. Live-byte measurements can differ
by a few bytes between rounds due to harness state.

## Scope

Replacing obsolete adjacent snapshots does not stop producing those snapshots,
so allocated bytes need not decrease. It lets old payloads die earlier and
reduces GC work when the old journal would have promoted them.

This is deliberately not a globally indexed journal: matching only the head
keeps admission constant-time and preserves text/event ordering, restart
boundaries, retractions and ID reuse. Nonadjacent snapshots still use the
existing projection-time deduplication. Live delivery remains unchanged.
