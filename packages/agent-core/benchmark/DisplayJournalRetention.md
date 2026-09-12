# Cumulative display-journal retention

`DisplayJournalRetention.hs` measures the journal while a response attempt is
still live, before failure projection or successful completion clears it.
It complements `DisplayJournal.hs`, whose ordinary successful/failed-turn
workloads guard against admission overhead.

## Method

Compare separately built executables from the current implementation before
adjacent snapshot replacement and the candidate after it. **This baseline is
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

## Component results

GHC 9.10.3, `-O2`, `+RTS -N4 -T`, medians of three samples. Decimal MB.
These compare the frozen current-before baseline with the final candidate,
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
6,697 bytes versus 6,641 before. Keep the shorter inlined implementation;
the extra head inspection and ID comparison are not free. This is a measured
component trade-off, not a claim of zero CPU regression.

## Limits

- Only adjacent same-call snapshots of the same projection kind are replaced.
  Interleaved call IDs and alternating arguments/output remain negative controls.
- Text and restart boundaries are not crossed; prior finishes are never replaced.
- These numbers are **post-GC live heap and allocated bytes, not process RSS**.
  Measure RSS with an external process observer in independent runs.
- This is a generated component workload, not a whole-CLI connected-session
  measurement. It does not establish a 20% whole-application memory reduction.
