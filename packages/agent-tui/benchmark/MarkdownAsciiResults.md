# Printable ASCII rendering benchmark

The candidate copies printable ASCII with `Text.copy`, avoiding grapheme
reconstruction without retaining the source buffer behind a rendered slice.
This replaces the original identity fast path after review identified its
ownership regression, already documented in [`TerminalText.md`](TerminalText.md).
The timings below are for the corrected copying implementation.

Measured on 2026-09-12 with GHC 9.10.3 on aarch64 macOS, inside `nix develop`.
The benchmark component uses `-O2`; allocation statistics use `+RTS -T`.
Results are medians of seven samples, with 64-character streaming chunks and
workload repetition counts of 5, 25, and 100.

The baseline is the saved benchmark executable built from master
`7e6cf643f7b12882117dc25c5807d619934f0a23`. The candidate adds only the
printable-ASCII guard to `displayTerminalText`: text consisting entirely of
characters from space through tilde is copied; other text uses
the existing grapheme-aware sanitization.

These timing executables were saved before the retained-heap diagnostic
refactor in `FullscreenMarkdown.hs`; both use the same original timing harness.
The baseline also includes the Unicode fixtures, with the original text helper.

Both executables use **streaming** mode. Comparing historical renderer modes
within the candidate executable would not isolate this change: they share
the modified text helper.

## Procedure

Build and save the baseline executable before applying the guard, then rebuild
the candidate with the same command:

```sh
nix develop -c cabal build --offline agent-tui:bench:fullscreen-markdown-bench
nix develop -c cabal list-bin agent-tui:bench:fullscreen-markdown-bench
```

Set `BASELINE` and `CANDIDATE` to the respective executable paths and run:

```sh
nix develop -c sh -c '
  for count in 5 25 100; do
    for scenario in prose prose-lines mixed table long-line; do
      "$BASELINE" streaming "$scenario" "$count" 64 7 +RTS -T
      "$CANDIDATE" streaming "$scenario" "$count" 64 7 +RTS -T
    done
  done
'
```

This measures the full Brick rendering path, not parser time or widget
construction alone. Each frame retains Brick's image cache, renders an
80-by-30 viewport, and consumes Vty display spans and clickable extents.
Input construction is outside the measured interval. The workload includes
the final non-streaming frame. The benchmark also checks per-frame display
and click-target equivalence against its historical section-cache renderer.

## Results

Each cell is **baseline → candidate**. Times are milliseconds; allocation is
bytes accumulated across all frames in one sample, not retained heap size.

| Scenario | Count | Elapsed ms | CPU ms | Allocated bytes |
| --- | ---: | ---: | ---: | ---: |
| prose | 5 | 1.280 → 1.081 | 1.280 → 1.077 | 9,492,144 → 7,743,408 |
| prose-lines | 5 | 1.727 → 1.271 | 1.727 → 1.271 | 12,664,096 → 8,900,320 |
| mixed | 5 | 7.501 → 6.535 | 7.509 → 6.550 | 46,417,240 → 41,635,016 |
| table | 5 | 1.126 → 1.004 | 1.126 → 1.006 | 7,983,080 → 7,045,920 |
| long-line | 5 | 1.711 → 1.357 | 1.711 → 1.341 | 12,228,648 → 8,423,632 |
| prose | 25 | 8.683 → 7.561 | 8.674 → 7.537 | 67,184,744 → 58,435,608 |
| prose-lines | 25 | 23.863 → 14.885 | 23.811 → 14.748 | 159,109,960 → 100,426,024 |
| mixed | 25 | 48.375 → 44.703 | 48.301 → 44.643 | 282,479,408 → 258,442,024 |
| table | 25 | 7.377 → 6.587 | 7.370 → 6.579 | 46,555,440 → 41,044,920 |
| long-line | 25 | 23.728 → 14.775 | 23.721 → 14.757 | 157,084,480 → 97,707,344 |
| prose | 100 | 46.085 → 40.373 | 46.014 → 40.196 | 350,065,736 → 314,760,296 |
| prose-lines | 100 | 321.568 → 191.620 | 321.019 → 191.202 | 1,876,583,512 → 1,016,108,080 |
| mixed | 100 | 333.364 → 317.798 | 332.970 → 317.196 | 1,787,271,336 → 1,691,537,352 |
| table | 100 | 85.143 → 73.646 | 84.853 → 73.542 | 393,368,952 → 336,381,720 |
| long-line | 100 | 379.203 → 219.822 | 378.096 → 219.664 | 1,925,557,328 → 1,054,709,344 |

## Repeat and Unicode fallback checks

An eleven-sample repeat, running the candidate first, measured `prose-lines 100`
at 313.839 → 182.705 ms elapsed, 313.476 → 182.691 ms CPU, and
1,876,583,512 → 1,016,108,080 allocated bytes. This confirms roughly 42% less
rendering time and 46% less allocation for that workload.

The following seven-sample checks use the same settings and both executables
include the new Unicode fixtures; the baseline has only the ASCII guard removed.
Run each executable with `streaming SCENARIO COUNT 64 7 +RTS -T`.

| Scenario | Count | Elapsed ms | CPU ms | Allocated bytes |
| --- | ---: | ---: | ---: | ---: |
| unicode-prose | 5 | 1.577 → 1.293 | 1.572 → 1.295 | 10,591,592 → 8,853,736 |
| unicode-prose | 20 | 13.345 → 11.085 | 13.347 → 11.064 | 88,227,648 → 69,982,592 |
| unicode-tail | 5 | 49.898 → 32.706 | 49.871 → 32.671 | 316,450,776 → 203,041,448 |
| unicode-tail | 20 | 696.118 → 504.058 | 694.904 → 503.574 | 4,525,057,240 → 3,030,715,032 |

`unicode-prose` includes CJK, combining accents, emoji ZWJ sequences and flags.
`unicode-tail` ends each long ASCII line with CJK, exercising a failed guard
after a long prefix. Both still contain printable ASCII spans that benefit from
the guard, especially after wrapping. These full-render results show no observed
regression for the fixtures; they do not imply the Unicode fallback itself is
faster or that its extra guard scan is free.

## Retained heap and peak RSS

The new `retained SCENARIO COUNT 64 3 +RTS -T` mode pins the final body,
Brick render state/image cache, and picture across a major GC. Both comparison
binaries include this diagnostic; the baseline disables only the ASCII guard.
Median total live heap bytes match exactly in all seven fixtures:

| Scenario | Count | Baseline bytes | Copy bytes |
| --- | ---: | ---: | ---: |
| history-prose | 100 | 307,680 | 307,680 |
| history-prose | 1000 | 2,542,496 | 2,542,496 |
| history-mixed | 100 | 1,035,280 | 1,035,280 |
| history-mixed | 500 | 4,925,296 | 4,925,296 |
| prose | 50 | 591,544 | 591,544 |
| mixed | 20 | 1,185,320 | 1,185,320 |
| resize | 20 | 1,203,688 | 1,203,688 |

Peak RSS is a separate metric. Three fresh process runs with macOS
`/usr/bin/time -l` around that retained command (including parity checks) gave
median RSS of 62,390,272 → 49,790,976 bytes for `history-prose 1000`, but
134,922,240 → 148,553,728 bytes for `history-mixed 500` (about 10% higher).
Thus copying fixes source-buffer retention, but these results do **not** establish
a general peak-memory improvement. The mixed-history peak remains a limitation;
allocation reductions must not be presented as reductions in process memory.

## Performance boundary

The main table uses ASCII-source Markdown workloads. No Unicode-path speedup is claimed:
non-ASCII and control-containing text still requires the general path.
The change removes unnecessary segmentation and reconstruction for printable
ASCII; it does not change streaming parser state or introduce image caches.
Completed lines within a growing prose section are still rendered again until
the section becomes stable, so this is a constant-factor improvement rather
than a change to that rendering complexity.

## Correctness validation

The focused `Agent.TUI.TextWidthSpec` and `Agent.TUI.MarkdownSpec` suites passed
in GHCi: 88 examples, zero failures. Coverage includes the entire printable
ASCII range, controls before and after ASCII, combining marks, keycaps, emoji
ZWJ sequences, and existing rendering/layout parity checks.

After adding the deterministic backing-array ownership regression test, the
text-width suite was rerun: 18 examples, zero failures. It verifies that a
seven-byte slice of a larger buffer renders into its own seven-byte array.

A live tmux smoke test streamed seven-character deltas through the fullscreen
TUI, including bold/link prose, fenced code, and a Unicode table. Resizing from
110×35 to 55×28 preserved wrapping and alignment; PageUp/PageDown navigated
history and returned to the final text correctly.
