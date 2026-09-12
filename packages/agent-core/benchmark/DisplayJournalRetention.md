# Cumulative display-journal retention

**Rejected optimization; benchmark retained.** Production `DisplayJournal.hs`
has been restored exactly to `ed2a95290`. This PR now adds profiling tools and
behavior/performance guardrails, not a production memory optimization. The
historical candidate below is `80be75c3`, available in this branch's history.
Its component memory savings are not present in the restored implementation.

`DisplayJournalRetention.hs` measures the journal while a response attempt is
still live, before failure projection or successful completion clears it.
It complements `DisplayJournal.hs`, whose ordinary successful/failed-turn
workloads guard against admission overhead.

## Method

Compare separately built executables from baseline `ed2a95290` and the rejected
candidate `80be75c3` (or a future redesign). **This baseline is
not `DisplayJournalBaseline.hs`**, the older implementation used by the existing
benchmark's `old` mode.

Build each revision with the same compiler and flags, keeping executables and
object directories separate. For example, from the repository root:

```sh
export BENCHROOT="$TMPDIR/journal-before" # use journal-after for the candidate
mkdir -p "$BENCHROOT"
nix develop -c ghc -O2 -threaded -rtsopts \
  -XGHC2021 -XDerivingStrategies -XBlockArguments -XOverloadedStrings \
  -XOverloadedRecordDot -XDuplicateRecordFields -XNoFieldSelectors -XLambdaCase \
  -ipackages/agent-core/src -ipackages/agent-json/src \
  -ipackages/agent-responses-types/src -outputdir "$BENCHROOT" \
  packages/agent-core/benchmark/DisplayJournalRetention.hs -o "$BENCHROOT/run"

"$BENCHROOT/run" arguments 1000 64 1 3 +RTS -N4 -T -s
"$BENCHROOT/run" output 1000 64 1 3 +RTS -N4 -T -s
"$BENCHROOT/run" mixed 1000 64 1 3 +RTS -N4 -T -s
"$BENCHROOT/run" arguments 500 64 4 3 +RTS -N4 -T -s
"$BENCHROOT/run" text 10000 16 1 3 +RTS -N4 -T -s
```

Arguments are workload, snapshots per call, growth bytes per snapshot, call
count, and sample count. `text` uses fixed-size chunks instead of cumulative
snapshots. Calls alternate round-robin; `mixed` alternates argument and output
updates for each call.

Each cumulative ASCII payload is allocated and forced immediately before
admission; there is no prebuilt input list retaining obsolete snapshots. A
stable pointer roots the unprojected journal across a major collection.
Admission live bytes therefore include the live journal and runtime overhead.
Projection is measured separately and fully checksummed. Payload construction
is included in admission allocations and CPU for both variants.

## Historical component results (rejected candidate)

GHC 9.10.3, `-O2`, `+RTS -N4 -T`, medians of three samples. Decimal MB.
These compare the frozen baseline with the rejected `80be75c3` candidate,
including both helper `INLINE` pragmas.

| Workload | Live MB before → after | Admission allocated MB before → after | Admission CPU ms before → after |
|---|---:|---:|---:|
| Arguments, 100 × 64 B, one call | 0.488 → 0.160 | 0.349 → 0.408 | 0.053 → 0.078 |
| Arguments, 500 × 64 B, one call | 8.226 → 0.190 | 8.138 → 8.430 | 2.513 → 2.416 |
| Arguments, 1,000 × 64 B, one call | 32.302 → 0.222 | 32.274 → 32.858 | 12.112 → 5.647 |
| Output, 1,000 × 64 B, one call | 32.322 → 0.218 | 32.322 → 32.850 | 12.339 → 6.022 |
| Mixed, 1,000 × 64 B, one call | 32.362 → 32.394 | 32.362 → 32.434 | 15.780 → 11.746 |
| Arguments, 500 × 64 B, four interleaved calls | 32.454 → 32.618 | 32.582 → 33.762 | 14.098 → 13.353 |
| Text, 10,000 × 16 B | 1.034 → 1.038 | 2.562 → 2.562 | 0.653 → 0.419 |

The final single-call snapshot is only 64,000 bytes; retaining every cumulative
snapshot previously kept about 32 MB alive. Adjacent replacement cuts measured
live bytes by approximately **99.3%** here. It does **not** eliminate producer
allocation of cumulative snapshots. Comparing call IDs also forces some
benchmark-generated identifier work earlier than the original admission path.

All 33 projection checksums matched across the eleven tested configurations
(arguments/output/mixed at 100/500/1,000 snapshots, four-call arguments, text).
The existing `DisplayJournal.hs --verify` also passed.

## Ordinary-workload guardrails and inlining

Use `new` mode in both separately frozen versions of `DisplayJournal.hs` to
compare current-before against current-after. Build it with the same command
above, substituting `DisplayJournal.hs`, adding
`-ipackages/agent-core/benchmark`, and choosing separate timing output directories.
With those executable paths assigned to `TIMING_BEFORE` and `TIMING_AFTER`:

```sh
"$TIMING_BEFORE" new tools-success 4 64 7 +RTS -N4 -T
"$TIMING_AFTER" new tools-success 4 64 7 +RTS -N4 -T
"$TIMING_AFTER" --verify
```

The initial candidate introduced selector/helper thunks: ordinary interleaved
tool workloads allocated an additional 408 bytes per call, roughly 25% on
successful turns. Explicitly inlining the two local admission helpers reduced
that to **56 additional bytes per turn**, independent of call count:

| Successful tool calls | Before bytes/turn | Initial candidate | Inlined candidate |
|---|---:|---:|---:|
| 1 | 1,769 | 2,177 | 1,825 |
| 4 | 6,641 | 8,273 | 6,697 |
| 16 | 26,129 | 32,657 | 26,185 |
| 100 | 162,545 | 203,345 | 162,601 |

Failed turns likewise add 56 bytes; the two-attempt retry/retraction workload
adds 112. All eight text controls (success/failure, with/without an initial tool,
1,000/10,000 chunks) have exactly unchanged allocations.

The inlined candidate independently passed verification and all 33 cumulative
checksums. Single-call 1,000-snapshot live bytes remained approximately
0.222 MB for arguments and 0.218 MB for output.

CPU microbenchmarks are noisy. Alternating before/initial-candidate processes
(three repetitions, 15 samples each) measured text controls essentially flat:
1,000-chunk CPU medians 0.008728 → 0.008524 ms, and
0.008668 → 0.008428 ms after a tool. Four-call tool turns added about
0.000582 ms; 1,000-call turns were approximately flat. The inlined sweep still
showed small absolute tool-admission overhead. Do not describe this change as
universally faster.

Fresh alternating processes confirmed the small-turn CPU cost rather than
explaining it away as noise: five repetitions of 31 samples at four successful
tool calls measured median CPU 0.001292 → 0.001834 ms (about 42%, but only
0.542 microseconds per turn). An independently compiled alternative with direct
duplicated argument-update pattern matches passed `--verify` but did not remove
the cost: a matched three-way sweep measured baseline 0.001272 ms, direct
0.001798 ms, and inlined helpers 0.001813 ms. Both alternatives allocated
6,697 bytes versus 6,641 before. The extra head inspection and ID comparison
are not free. This regression is not an acceptable shipping trade-off.

## CPU follow-up: restore the baseline

Further optimized, separately compiled experiments tried flat output nodes,
argument-update tags, a strict journal head, forced admission inlining, and
output-only replacement. None cleared all ordinary-workload guardrails.
Consequently **all production journal changes were removed**, rather than
trading a small-turn regression for a streaming/retraction regression.

For the output-only flat-node experiment, three alternating process pairs
(31 samples for tools, 15 for text, default 20 repetitions per sample) gave:

| Workload | Baseline CPU ms → experiment | Baseline bytes → experiment |
|---|---:|---:|
| Successful tools, 4 calls × 16 updates | 0.001300 → 0.001236 | 6,641 → 5,617 |
| Successful tools, 100 calls × 16 updates | 0.036770 → 0.034032 | 162,545 → 136,945 |
| Text success, 1,000 × 16 B | 0.008268 → 0.009585 | 48,145 → 48,145 |
| Text after tool success, 10,000 × 16 B | 0.127150 → 0.149774 | 480,169 → 480,169 |
| Retry/retract, 100 calls | 2.392683 → 2.278436 | 9,893,129 → 10,475,529 |

Thus the initial ~15% tool allocation win was rejected: text CPU and repeated
retraction allocation regressed. Reversing process order confirmed the small
text regression. Removing forced inlining and converting surviving output
nodes back to ordinary events during retraction each addressed only part of
the problem, not all guardrails.

The smallest alternative changed only the `ToolOutputUpdated` admission arm:
replace an adjacent same-ID output inside an ordinary `DisplayJournalEvent`,
with no new constructor or `INLINE` pragma. Three alternating pairs of 31
samples still measured four-call CPU 0.001307 → 0.001390 ms and 1,000-chunk
text CPU 0.008099 → 0.008466 ms; allocations were unchanged. It was also
rejected. These are component experiments, not whole-CLI CPU measurements.

Reproduce guardrails with separately compiled `DisplayJournal.hs` executables,
the flags above, and identical forced fixtures:

```sh
# Run inside nix develop, alternating BEFORE/AFTER for three process pairs.
"$TIMING_BEFORE" new tools-success 4 64 31 +RTS -N4 -T
"$TIMING_AFTER" new tools-success 4 64 31 +RTS -N4 -T
"$TIMING_AFTER" new tools-retract-once 4 64 31 +RTS -N4 -T
"$TIMING_AFTER" new retry-retract 100 64 31 +RTS -N4 -T
"$TIMING_AFTER" new text-success 1000 16 15 +RTS -N4 -T
"$TIMING_AFTER" new text-after-tool-success 10000 16 15 +RTS -N4 -T
```

Also cover tool counts 1/4/16/100, success/failure/retry, and all four text
controls at 1,000/10,000 chunks. `tools-retract-once` retains all existing calls
and retracts an unrelated ID, catching survivor-node reconstruction costs.
Optional final `REPETITIONS` defaults to 20; record any override with the
results, since CSV does not encode it. Increasing it changes fixture residency
and GC/cache behavior and must not replace the default workload guardrails.

## Limits

- The rejected candidate only replaced adjacent same-call snapshots of the same projection kind.
  Interleaved call IDs and alternating arguments/output remain negative controls.
- Text and restart boundaries are not crossed; prior finishes are never replaced.
- These numbers are **post-GC live heap and allocated bytes, not process RSS**.
  Measure RSS with an external process observer in independent runs.
- This is a generated component workload, not a whole-CLI connected-session
  measurement. It does not establish a 20% whole-application memory reduction.
