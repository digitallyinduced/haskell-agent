# Transcript renderer memory experiment

Measured with GHC 9.10.3, Cabal `--enable-optimization=2
--enable-executable-dynamic`, RTS `-N4 -T -s`. These are generated
production-renderer workloads, not captured user sessions or a complete CLI
process. They do not establish a 20% real-world resident-memory reduction.

## Method

`TranscriptScrolling.hs` exercises the production history chunks and measured
viewport. It reports allocation, elapsed/CPU time, post-major-GC live bytes with
the final render state kept alive, and RTS memory in use. Setup/first render is
separate from 25 redraws. `compare-memory.py` alternates baseline/candidate in
fresh processes three times and records `wait4` peak RSS as well as raw RTS
output. Most workloads take five samples inside each process; the interaction
workload also takes one sample to expose initially cold fallback caches.

The interaction trace cycles through stable, selected, cleared, code-copy hover,
code-copy pressed, and cleared states, moving between chunks at a fixed terminal
size. Its target chunk is visible. The resize/scroll trace is measured separately.
`verify` emits actual styled text spans, widths, and extents for 25 frames; outputs
matched exactly between all three binaries for the static, resize/scroll, and
interaction workloads (100 blocks, 30 body lines). This is a render-equivalence
check, not a full application input/event-handler test.

For dynamically linked comparisons, copying the executable alone is insufficient.
Each variant was frozen with its local optimized shared libraries, a wrapper
setting `LD_LIBRARY_PATH`, and an `ldd` check showing no `dist-newstyle` references
or missing libraries. All variants used the same finalized benchmark harness and
terminal theme. An earlier empty-theme baseline was discarded and rebuilt.

## ASCII terminal-text fast path

This candidate was also deferred, and production terminal sanitation is unchanged.
The benchmark-only implementation and additional RSS/retention diagnostics are in
`packages/agent-tui/benchmark/TerminalText.md`.

Baseline uses grapheme rendering for all text; candidate returns printable ASCII
and newline-only text directly, retaining the existing Unicode/control fallback.
The table shows medians across three processes; MB means decimal megabytes.

| Workload | First-render allocation MB, old → new | First-render CPU ms, old → new | Warm live heap MB, old → new | Peak RSS reduction |
| --- | ---: | ---: | ---: | ---: |
| 100 blocks × 3 lines | 52.23 → 32.27 | 21.56 → 17.49 | 2.51 → 2.56 | 0.2% |
| 600 × 3 | 298.73 → 179.00 | 153.75 → 133.25 | 14.76 → 15.02 | -0.6% |
| 1200 × 3 | 594.71 → 355.24 | 335.21 → 241.83 | 29.48 → 30.00 | -0.2% |
| 600 × 3, resize/scroll | 298.78 → 179.04 | 161.15 → 145.59 | 10.63 → 10.88 | -0.3% |
| 100 × 30, resize/scroll | 279.35 → 138.70 | 104.63 → 73.23 | 4.84 → 5.13 | approximately 0% |
| 100 × 30 | 279.17 → 138.69 | 101.83 → 75.93 | 5.76 → 6.05 | -0.7% |
| 100 × 30, interaction | 279.18 → 138.69 | 100.86 → 74.28 | 6.06 → 6.39 | 0.5% |

First-render allocation falls 38–50%, but cached warm-redraw allocation is
essentially unchanged. First-interaction redraw allocation falls only 4.1%
(392.58 → 376.62 MB, single-sample processes). Live heap slightly increases and
RSS is effectively unchanged. Allocation throughput is not retained memory.

## Rejected candidate: omit nested caches inside cached transcript chunks

The experiment omitted `ConversationBlockCache` and fenced `CodeBlockCache`
entries while rendering an eligible complete outer `ConversationChunkCache`.
Partial, selected, hovered, running, and streaming chunks kept the normal block
renderer. Rendering and click/extent logic were otherwise unchanged.

The static 1200 × 3 workload saved only 0.40 MB live heap (29.99 → 29.59 MB),
about 1.3%, with 0.4% peak RSS reduction. Initial cache entries share rendered
images, so counting cache layers does not imply multiple independent payloads.

More importantly, selection/hover bypasses the outer cache. Without preexisting
inner results this renders a fresh chunk, while its original outer result remains
cached. This loses sharing and can retain both generations of images:

| 100 × 30 interaction, ASCII-only → cache candidate | Before | After |
| --- | ---: | ---: |
| Warm post-GC live heap | 6.39 MB | 11.06 MB |
| Cold interaction post-GC live heap | 6.39 MB | 11.05 MB |
| Cold 25-redraw allocation | 376.62 MB | 496.23 MB |
| Cold 25-redraw CPU | 139.86 ms | 210.46 ms |
| Cold process peak RSS change | | +7.3% |

Thus the candidate **was reverted**, despite exact visual equivalence. It
increased interactive retained heap approximately 73% and cold interaction CPU
approximately 50%. A future design would need coordinated cache eviction or
ownership/sharing changes, not merely fewer nested cache entries.

## Reproduction

Build each variant with the identical harness:

```sh
nix develop -c cabal build agent-cli:bench:transcript-scrolling-bench \
  --enable-optimization=2 --enable-executable-dynamic -j4
```

Freeze binaries and local shared libraries before rebuilding the next variant.
Then, using the frozen wrapper paths:

```sh
nix develop -c python3 packages/agent-cli/benchmark/compare-memory.py \
  /path/to/baseline/run /path/to/candidate/run "$TMPDIR/comparison.json" 3
/path/to/baseline/run verify history-measured-viewport-interaction 100 30 \
  > "$TMPDIR/baseline.verify"
/path/to/candidate/run verify history-measured-viewport-interaction 100 30 \
  > "$TMPDIR/candidate.verify"
cmp "$TMPDIR/baseline.verify" "$TMPDIR/candidate.verify"
```

Session artifacts were saved under the session temporary directory as
`transcript-ascii-corrected.json`, `transcript-cache-results.json`, and
`transcript-{baseline,ascii,cache}/` (frozen binaries/libraries and verify output).
Timing varies with machine contention; retained-heap and allocation comparisons
are the primary rejection evidence.
