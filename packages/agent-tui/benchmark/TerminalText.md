# Terminal text allocation benchmark

This preserves a **rejected optimization experiment**, not a production change.
`TerminalTextBaseline` copies the production grapheme-aware implementation;
`TerminalTextCandidate` returns the original `Text` for printable ASCII plus
structural newlines, retaining the original path for other inputs. Although
allocation falls dramatically, renderer retained heap and peak RSS regress
(see below). Production `displayTerminalText` remains unchanged.

## Reproduce

From the repository root:

```sh
cabal build agent-tui:bench:terminal-text-bench
# Or build directly inside the development environment:
export BENCHROOT="$TMPDIR/terminal-text-bench"
mkdir -p "$BENCHROOT"
nix develop -c bash -c '
  ghc -O2 -XOverloadedStrings -rtsopts -ipackages/agent-tui/src \
    -ipackages/agent-tui/benchmark -outputdir "$BENCHROOT" \
    packages/agent-tui/benchmark/TerminalText.hs \
    -o "$BENCHROOT/run" -package text -package vty -package deepseq \
    -package safe-exceptions
  for size in 80 1000 10000; do
    for scenario in ascii mixed unicode control late-unicode; do
      for mode in old new; do
        "$BENCHROOT/run" "$mode" "$scenario" "$size" 100 7 +RTS -T
      done
    done
  done
'
```

CSV columns: mode, scenario, body characters, input count, samples, median
elapsed milliseconds, median CPU milliseconds, median allocated bytes, median
post-GC live bytes. Input construction is excluded; fully evaluating output
and its checksum is included. Inputs differ per sample to avoid shared
evaluation. Both input and output are kept live through collection using a
bracketed stable pointer (rather than repeated checksums that GHC could share), modeling
source transcript text coexisting with displayed text. Live bytes include
benchmark/runtime overhead and are **not peak RSS or whole-CLI memory**.

`TerminalTextBaseline` preserves the old implementation and its local helpers:
moving helpers across module boundaries changes GHC optimization and can
confound allocation comparisons. Each invocation also checks fixture output
equivalence before measurement.

## Observed results

GHC 9.10.3, `-O2`, x86_64 Linux, 100 inputs, seven samples:

| ASCII body size | Allocation old → new (bytes) | Live old → new (bytes) | CPU old → new (ms) |
| ---: | ---: | ---: | ---: |
| 80 | 6,924,888 → 13,880 | 45,968 → 35,512 | 1.515 → 0.018 |
| 1,000 | 82,612,088 → 13,880 | 230,896 → 128,440 | 16.044 → 0.196 |
| 10,000 | 823,012,088 → 13,880 | 2,039,920 → 1,037,464 | 240.066 → 1.954 |

This removes over 99.8% of this ASCII operation's allocation, and 23–49% of
the benchmark's live source-plus-display heap. It does not establish a 20%
whole-application memory reduction.

At 1,000 characters, mixed/control/late-Unicode allocation was exactly
unchanged (81,734,488 / 81,515,288 / 82,924,088 bytes respectively).
The Unicode fixture allocated 69,841,688 → 69,410,488 bytes; the small
difference reflects optimized fallback code rather than the ASCII shortcut.

A previous reverse-order repeat with 15 samples (before replacing the
post-timing liveness checks with stable pointers) gave CPU milliseconds:

| Scenario | Old | New |
| --- | ---: | ---: |
| ASCII | 12.556 | 0.195 |
| Mixed | 12.706 | 12.707 |
| Unicode | 14.690 | 14.697 |
| Control | 12.677 | 13.106 |
| Late Unicode | 12.934 | 13.286 |

There is a small fallback CPU tradeoff (about 3% in the last two fixtures);
in particular a late non-ASCII character requires an extra prefix scan.
Timing is indicative, not a strict regression threshold.

The identity result shares the input's backing storage. If callers retain a
tiny slice of otherwise-dead large text, they may need an explicit copy at
that ownership boundary. The benchmark instead models an already-retained
source transcript, where sharing avoids duplicate storage.

Validation: the TextWidth Hspec suite passed all 14 examples, and 10,000
random strings produced identical baseline/current output. New regression
coverage includes printable-ASCII identity and controls, keycaps, and
combining characters after long ASCII prefixes.

## Full Markdown rendering cross-check

The existing `FullscreenMarkdown.hs` benchmark was also compiled twice with
`-O2`: once using `TextWidth.hs` from revision
`8a97d73c5a037b317a12f4b30e6ff5fa01cb8055`, once with the ASCII identity
shortcut. Both executables used renderer mode **`new`**, retaining the existing
streaming-cache optimization in both. Five samples, chunk size 64:

| Workload | Count | Allocation old → new (MB) | Max residency old → new (MB) | Peak RSS old → new (KiB) |
| --- | ---: | ---: | ---: | ---: |
| history-prose | 100 | 30.013 → 15.741 | 1.078 → 1.382 | 18,960 → 18,884 |
| history-prose | 1,000 | 287.028 → 144.460 | 11.400 → 13.144 | 38,832 → 46,732 |
| history-mixed | 100 | 155.296 → 110.659 | 9.974 → 10.267 | 39,512 → 39,900 |
| history-mixed | 500 | 774.414 → 551.297 | 44.001 → 47.367 | 109,428 → 114,300 |
| prose | 50 | 131.992 → 114.221 | 0.694 → 0.971 | 18,948 → 18,820 |
| mixed | 20 | 228.153 → 206.632 | 2.211 → 2.386 | 21,112 → 21,944 |
| resize | 20 | 233.691 → 207.244 | 2.291 → 2.425 | 20,524 → 21,016 |

**Reduced allocation did not translate into reduced peak memory in these
workloads.** The larger prose case used about 20% more peak RSS. Different GC
scheduling and backing-array sharing both require consideration; these
measurements alone do not establish the cause.

Maximum residency comes from `+RTS -T -s`; RSS from Linux `wait4` child
`ru_maxrss`. Both include the benchmark's preliminary frame-equivalence check
and input construction, unlike the timed allocation deltas. The five samples
share a process, so the peak values are one process-wide maximum, not medians.
Timing during these runs was noisy due to concurrent builds and is unsuitable
for precise percentage claims.

To reproduce the paired build without changing the working source, place each
version in a separate temporary `Agent/TUI/TextWidth.hs` and prepend its root
to the import path:

```sh
nix develop -c ghc -O2 -rtsopts -XGHC2021 -XDerivingStrategies \
  -XBlockArguments -XOverloadedStrings -XOverloadedRecordDot \
  -XDuplicateRecordFields -XNoFieldSelectors -XLambdaCase -XRecordWildCards \
  -i"$TEXTWIDTH_ROOT" -ipackages/agent-tui/src -ipackages/agent-core/src \
  -ipackages/agent-json/src -ipackages/agent-syntax/src \
  -ipackages/agent-responses-types/src -outputdir "$BUILD_ROOT" \
  packages/agent-tui/benchmark/FullscreenMarkdown.hs -o "$BUILD_ROOT/run"
"$BUILD_ROOT/run" new history-prose 1000 64 5 +RTS -T -s
```

Use separate build roots and verify the compiler log names the intended
override source. In a shared checkout, swapping the working source while
another benchmark builds can accidentally produce two baseline executables.

## Copying and explicit-retention diagnostic

A further isolated experiment compared the original implementation, the ASCII
identity shortcut, and an ASCII `Text.copy input` shortcut. The latter still
avoids grapheme segmentation but preserves the original ownership boundary.
No copying variant was applied to production source during this experiment.

A temporary copy of `FullscreenMarkdown.hs` changed `run` to return
`(checksum, finalBody, finalRenderState, finalPicture)` rather than only the
checksum. Each frame still consumed the actual display spans. The final tuple
was pinned using `bracket (newStablePtr result) freeStablePtr` across `performGC`;
the final column below is `gcdetails_live_bytes` after that GC. This retains the
actual rendered result and cache rather than estimating retention from sampled
maximum residency. Three samples, chunk size 64, same `-O2` build settings:

| Workload | Count | Live original | Live identity | Live copying |
| --- | ---: | ---: | ---: | ---: |
| history-prose | 100 | 282,560 | 350,656 | 282,560 |
| history-prose | 1,000 | 2,272,576 | 2,952,672 | 2,272,576 |
| history-mixed | 100 | 1,010,208 | 1,183,104 | 1,010,208 |
| history-mixed | 500 | 4,791,424 | 5,655,520 | 4,791,424 |
| prose | 50 | 540,824 | 609,016 | 540,824 |
| mixed | 20 | 1,163,928 | 1,228,440 | 1,163,928 |
| resize | 20 | 1,182,456 | 1,246,968 | 1,182,456 |

The identity shortcut therefore introduces a real retained-heap regression,
not merely different maximum-residency sampling. Copying eliminates that
regression exactly in all seven cases, consistent with backing-storage sharing
being responsible.

The unmodified full benchmark was also rerun for all three variants, with
three samples:

| Workload | Count | Peak RSS original / identity / copying (KiB) |
| --- | ---: | ---: |
| history-prose | 100 | 18,896 / 18,832 / 18,896 |
| history-prose | 1,000 | 38,920 / 46,648 / 44,680 |
| history-mixed | 100 | 39,508 / 39,832 / 33,416 |
| history-mixed | 500 | 109,488 / 114,248 / 110,216 |
| prose | 50 | 18,948 / 18,884 / 18,948 |
| mixed | 20 | 21,112 / 22,020 / 20,512 |
| resize | 20 | 21,408 / 20,868 / 20,448 |

Copying retains most of the allocation reduction: large prose
287,028,256 → 144,987,728 bytes and large mixed
774,381,312 → 552,072,016 bytes. However, large prose peak RSS still increases
about 15%, despite equal final retained heap.

**Recommendation:** reject the identity shortcut as a memory optimization.
The copying variant is an allocation optimization with no measured final-heap
benefit and an outstanding peak-RSS regression; it does not establish the
requested whole-application memory reduction either.
