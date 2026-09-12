# Transcript index benchmark

`TranscriptIndex.hs` compares the existing `Map BlockId value` representation
with `IntMap value` (a Patricia trie). The benchmark isolates index maintenance;
it does not measure terminal rendering, text layout, database reads, or total
application memory. Values are shared, already evaluated `Int` payloads.

## Workloads

- `positive`: increasing positive identifiers, as in the live transcript.
- `negative`: decreasing negative identifiers, as within one reconstructed page.
- `mixed`: alternating signs, exercising signed ordering and non-monotone input.
- `paged`: decreasing negative identifiers within 64-block fixture pages, with
  the pages reversed. This models older pages allocated later and prepended:
  the retained history is not globally ordered by identifier. Payload positions
  are reassigned to match the resulting transcript order.
- `sparse`: alternating signs with 104,729 between successive identifier
  magnitudes, exercising a non-dense trie. This is a stress shape rather than
  the allocator's usual consecutive identifiers.

Identifiers begin outside GHC's small-integer cache. Each shape measures:

- `rebuild`: the existing `fromList` construction, including its ascending-input
  optimization in `Map`, followed by complete result consumption.
- `append`: repeated individual inserts from an empty index, followed by complete
  result consumption.
- `lookup`: every resident identifier and three misses in an already built index.
- `retract`: filter an already built index by retained transcript position,
  keeping its first half, followed by result consumption.
- `lifecycle`: construction, up to twenty fresh insertions, navigation over the
  input identifiers, retention of the original first half, and reconstruction
  after removing one middle entry from those survivors. Fresh identifiers keep
  their signs and lie beyond the input's maximum magnitude; the untimed
  validation asserts they do not overwrite existing entries.
  This is a composite **index** workload, not a replay of the complete UI reducer.

The standalone `BlockId` has the same newtype representation as production.
`coerce` lets the baseline consume the fixture without a conversion-list cost.
No sorting or specialized ascending constructor is added to either candidate.
An `IORef` read per repetition and non-inlined workload entry points prevent a
repeated pure result from being shared across iterations. Checksums fully consume
the indexes. Ascending association lists and all paired workload checksums must
agree before a run is accepted.

Timing includes workload-triggered garbage collections, but excludes the explicit
collections surrounding each sample. The final collection refreshes RTS allocation
statistics to include the unfinished nursery. Seven samples alternate baseline
and candidate execution order; each output column reports its median independently.
The retained measurement is an incremental live-heap delta with a stable pointer
keeping the new index alive. It excludes shared input payloads and includes a
small measurement-bookkeeping overhead; it is not process RSS.

## Reproduction

Measured on Apple M3 Max, arm64 macOS, GHC 9.10.3, `-O2 -threaded`, one runtime
capability, default allocation area, RTS statistics enabled.

From the repository root:

```sh
mkdir -p "$TMPDIR/transcript-index-benchmark"
nix develop -c sh -c '
  directory="$1"
  ghc -O2 -Wall -rtsopts -threaded \
    packages/agent-tui/benchmark/TranscriptIndex.hs \
    -outputdir "$directory" -o "$directory/transcript-index-bench"
  for size in 10 100 1200 5000; do
    repetitions=$((1200000 / size))
    "$directory/transcript-index-bench" "$size" "$repetitions" 7 +RTS -T -N1
  done
  "$directory/transcript-index-bench" 1200 1000 7 +RTS -T -N1
' sh "$TMPDIR/transcript-index-benchmark"
```

The 1,200-entry case corresponds to the configured history-window block budget,
not an assertion that typical sessions fill it. The 5,000-entry case is a stress
case beyond that budget, not the ordinary retained window.
An optional fourth positional argument selects a single shape, for example
`1200 1000 7 paged +RTS -T -N1`.

## Results (2026-09-12)

At 1,200 entries, 1,000 repetitions per sample. Times are microseconds **per
repetition**, bytes are allocated **per repetition**. Each cell is
`Map / IntMap`. A lookup repetition visits all 1,200 keys plus three misses;
these are not individual-lookup timings.

| Identifier shape / operation | CPU µs | Elapsed µs | Allocated bytes |
|---|---:|---:|---:|
| Positive rebuild | 16.262 / 23.033 | 16.278 / 23.064 | 86,313 / 265,713 |
| Positive append | 102.903 / 22.522 | 104.418 / 22.650 | 810,673 / 265,713 |
| Positive lookup | 23.113 / 19.943 | 23.103 / 19.934 | 19,321 / 121 |
| Positive retract | 11.028 / 11.827 | 11.039 / 11.841 | 609 / 43,337 |
| Positive lifecycle | 84.545 / 94.319 | 85.145 / 94.375 | 365,369 / 637,817 |
| Negative rebuild | 98.109 / 25.373 | 98.278 / 25.521 | 810,673 / 313,393 |
| Negative append | 92.954 / 23.008 | 92.999 / 23.078 | 810,673 / 313,393 |
| Negative lookup | 21.643 / 20.882 | 21.655 / 20.934 | 19,321 / 121 |
| Negative retract | 10.952 / 11.850 | 10.959 / 11.848 | 609 / 43,337 |
| Negative lifecycle | 201.760 / 98.529 | 202.361 / 98.621 | 1,412,993 / 709,697 |
| Mixed rebuild | 97.882 / 26.775 | 98.238 / 26.788 | 781,873 / 289,673 |
| Mixed append | 96.158 / 26.213 | 96.407 / 26.255 | 781,873 / 289,673 |
| Mixed lookup | 22.604 / 19.963 | 22.633 / 19.963 | 19,321 / 121 |
| Mixed retract | 11.602 / 11.651 | 11.609 / 11.655 | 849 / 43,337 |
| Mixed lifecycle | 212.683 / 101.422 | 214.206 / 102.059 | 1,369,553 / 674,097 |

Lifecycle scaling, again `Map / IntMap` and per repetition:

| Entries | Repetitions | Positive CPU µs | Positive allocated bytes | Negative CPU µs | Negative allocated bytes |
|---:|---:|---:|---:|---:|---:|
| 10 | 120,000 | 1.146 / 0.829 | 8,000 / 6,416 | 1.259 / 0.857 | 9,640 / 6,776 |
| 100 | 12,000 | 7.364 / 6.675 | 42,728 / 48,816 | 12.084 / 6.926 | 92,864 / 54,416 |
| 1,200 | 1,000 | 84.545 / 94.319 | 365,369 / 637,817 | 201.760 / 98.529 | 1,412,993 / 709,697 |
| 5,000 | 240 | 419.179 / 465.508 | 1,478,429 / 2,945,781 | 1,138.175 / 477.854 | 6,939,989 / 3,209,221 |

The independent 1,200-entry repeat returned positive lifecycle CPU
83.044 / 92.566 µs and negative lifecycle 195.849 / 95.256 µs, with
identical allocation figures. The positive-rebuild regression persisted
(16.169 / 22.622 µs).

Retained incremental heap at 1,200 entries was:

| Shape | Map bytes | IntMap bytes |
|---|---:|---:|
| Positive | 58,896 | 78,056 |
| Negative | 78,080 | 78,056 |
| Mixed | 78,080 | 78,056 |

These figures do **not** establish a retained-memory reduction for the trie.
The practically equal negative/mixed measurements should not be presented as
a 24-byte improvement; that difference is below the useful precision of this
measurement.

### Page ordering and sparse identifiers

The same seven-sample 1,200-entry run includes `paged` and `sparse` shapes.
All checksum comparisons passed. Units and `Map / IntMap` convention match the
table above.

| Shape / operation | CPU µs | Elapsed µs | Allocated bytes |
|---|---:|---:|---:|
| Paged rebuild | 103.320 / 36.191 | 103.379 / 36.599 | 815,569 / 390,913 |
| Paged append | 103.009 / 34.869 | 103.101 / 34.886 | 815,569 / 390,913 |
| Paged lookup | 22.113 / 20.575 | 22.124 / 20.560 | 19,321 / 121 |
| Paged retract | 11.175 / 11.648 | 11.178 / 11.653 | 993 / 43,337 |
| Paged lifecycle | 216.558 / 112.249 | 217.259 / 112.709 | 1,418,993 / 816,457 |
| Sparse rebuild | 96.588 / 28.462 | 96.689 / 28.503 | 781,873 / 307,633 |
| Sparse append | 96.328 / 27.982 | 96.412 / 27.979 | 781,873 / 307,633 |
| Sparse lookup | 22.202 / 20.264 | 22.224 / 20.364 | 19,321 / 121 |
| Sparse retract | 11.873 / 11.794 | 11.867 / 11.806 | 849 / 43,337 |
| Sparse lifecycle | 207.704 / 102.799 | 208.111 / 102.871 | 1,369,553 / 702,617 |

Both additional shapes retained 78,080 / 78,056 bytes, again effectively equal.
The paged result is the stronger basis for a history-index candidate than the
single descending-run result: rebuild CPU drops 65% and allocation drops 52%;
the composite index workload improves 48% in CPU and 42% in allocation.
The independent repeat returned paged rebuild 101.970 / 34.538 µs and paged
lifecycle 214.073 / 110.623 µs, with identical allocations.

Paged lifecycle scaling:

| Entries | CPU µs (Map / IntMap) | Elapsed µs (Map / IntMap) | Allocated bytes (Map / IntMap) |
|---:|---:|---:|---:|
| 10 | 1.265 / 0.859 | 1.266 / 0.860 | 9,640 / 6,776 |
| 100 | 12.433 / 7.468 | 12.459 / 7.474 | 92,912 / 57,936 |
| 1,200 | 216.558 / 112.249 | 217.259 / 112.709 | 1,418,993 / 816,457 |
| 5,000 | 1,160.692 / 575.525 | 1,161.325 / 575.625 | 6,993,941 / 3,683,381 |

## Interpretation

Do not replace the live transcript index wholesale. Although incremental
insertion improves substantially, ascending `Map.fromList` construction is
already efficient. The positive composite workload regresses at 1,200 and
5,000 entries. `Map.filter` also shares unchanged subtrees in this workload;
the `IntMap` candidate allocates substantially more while retaining the first
half. Faster insertion alone would hide those costs.

The history-block lookup index remains a narrower candidate because its negative
identifiers descend within pages, missing the ascending `Map.fromList` fast path.
However, the measurements do not demonstrate a retained-memory improvement or
an application-level benefit. Keep this as a benchmark result rather than
shipping a broad transcript-index substitution. Any future history-only change
needs validation in the actual history-page maintenance path.

This benchmark does not establish that a trie is superior to every alternative.
An order-aware `Map` constructor could also improve construction if its ordering
and uniqueness preconditions can be established safely; history IDs are not
globally descending once pages are combined.
