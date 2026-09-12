# Connected memory: historical adjacent journal snapshot results

**Historical measurements of rejected candidate `80be75c3`.**
The PR now contains profiling, benchmarks, and regression coverage only:
the production journal optimization has been removed because of CPU and
allocation guardrail regressions. There is no new production memory change
and no new whole-process memory savings claim. The actual CLI measurements
below are retained as historical evidence, not results for the current PR.

Baseline: `ed2a95290`; candidate: `80be75c3`, adjacent-only snapshot replacement in
`Agent.Loop.DisplayJournal`, with both local helpers inlined. Linux,
GHC 9.10.3, Cabal optimization level 2, dynamically linked executables,
`+RTS -N4 -T -s`. Executables and all local shared libraries were frozen
separately before rebuilding. See [fixture instructions](ConnectedMemory.md).

These are actual CLI processes connected to a scripted loopback provider,
including tool execution, persistence and terminal rendering. They are not
live-model sessions or measurements of the complete process tree. PostgreSQL,
the fixture server, and tmux are excluded from CLI RSS.

## Ordinary read control

Three independent fresh-home runs per variant, baseline group then candidate
group (not alternating). Each run: 120×40 terminal, 10-second startup,
three user turns with four reads of the same 100-line synthetic file per turn,
4,096 bytes of final streamed text plus completion marker, three seconds idle
after each turn, then `/quit`. No argument padding or forced collections.
Background title requests receive plain text without advancing driven turns.

| Metric | Before median | After median |
|---|---:|---:|
| Peak process RSS, KiB | 268,232 | 269,792 |
| Maximum major-GC live bytes | 6,441,568 | 6,470,400 |
| Allocated bytes at last diagnostic sample | 5,632,517,096 | 5,566,404,352 |
| Mutator + GC CPU seconds at last sample | 2.292 | 2.160 |

Peak RSS samples: before 267,408 / 269,932 / 268,232 KiB;
after 270,920 / 269,792 / 267,884 KiB. The ranges overlap: **no ordinary-session
memory improvement established** (median RSS increased 0.58%). Timing and
allocation samples end before process exit and vary with background work;
the small differences are not a speedup claim.

## Streamed terminal-output comparison

Three alternating baseline/candidate pairs, fresh isolated homes, 30-second
startup, one turn with two real terminal commands. Each command emits 262,144
bytes in 200 flushed chunks over three seconds; final assistant text is 256
bytes plus completion marker. Three seconds idle, then `/quit`. Completion
sentinels, rendered final response, and graceful exit were checked in every run.

| Metric | Before median | After median |
|---|---:|---:|
| Peak process RSS, KiB | 270,808 | 269,488 |
| Maximum major-GC live bytes | 8,323,840 | 8,283,336 |
| Allocated bytes at last diagnostic sample | 17,762,847,400 | 17,726,850,216 |
| Mutator + GC CPU seconds at last sample | 8.094 | 8.082 |

Peak RSS samples: before 268,236 / 271,200 / 270,808 KiB;
after 270,104 / 269,488 / 268,772 KiB. Median reduction is only 0.49%,
with overlapping ranges: **no meaningful whole-process reduction established**.
The isolated worst-case journal improvement does not translate to this
full-CLI workload. These are shared-host measurements, not controlled CPU
benchmarks; two abandoned diagnostic CLI processes were discovered and cleaned
up afterward. They were excluded from per-PID memory, but may affect timing.

## Where ordinary memory goes

A separate `/proc/PID/smaps` snapshot during baseline startup measured
254,844 KiB RSS. File-named mappings accounted for 221,432 KiB (87%);
203,472 KiB was shared-clean. Total anonymous pages, including copy-on-write
pages inside file mappings, were 50,708 KiB. PSS was 130,582 KiB.
Do not add these overlapping categories or equate file-named mappings with
entirely shared memory. This dynamically linked build's RSS is dominated by
loaded code/data rather than the roughly 5–6 MB live Haskell heap.

One read-control allocation trace had already allocated 3.16 GB before the
first tool, out of approximately 5.6 GB for the session. This identifies
startup/idle allocation churn as a further profiling target, not a proven
function-level attribution. A smaller journal cannot remove mapped libraries.

## Historical component evidence and validation

The historical candidate showed about 99.3% less live memory for 1,000 consecutive cumulative snapshots
of a single call. Interleaved calls and text are controls, not equivalent wins.
That candidate retained only the latest adjacent superseding snapshot, preserving
text, restart, cross-call, retraction, and repeated-finish boundaries.

Its roughly 42% small-turn component CPU regression was not an accepted shipping
trade-off. Follow-up designs also failed guardrails, so no journal optimization
is being shipped. See
[DisplayJournalRetention.md](../../agent-core/benchmark/DisplayJournalRetention.md)
for the component experiments and their limitations. Those measurements must
not be substituted for a connected whole-CLI rerun.

13 visible-state examples and 21 failed-display/event-delivery examples passed;
the projection verifier passed 6,464 event prefixes. The optimized full CLI
build passed. Actual file-write smoke tests verified generated file contents,
rendered completion, graceful exit, and isolated database cleanup.

The ordinary 20% whole-process memory goal remains unmet. Component retention
savings must not be presented as an overall CLI reduction.
