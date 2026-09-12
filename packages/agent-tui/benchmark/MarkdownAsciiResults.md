# Printable ASCII rendering benchmark

Measured on 2026-09-12 with GHC 9.10.3 on aarch64 macOS, inside `nix develop`.
The benchmark component uses `-O2`; allocation statistics use `+RTS -T`.
Results are medians of seven samples, with 64-character streaming chunks and
workload repetition counts of 5, 25, and 100.

The baseline is the saved benchmark executable built from master
`7e6cf643f7b12882117dc25c5807d619934f0a23`. The candidate adds only the
printable-ASCII guard to `displayTerminalText`: text consisting entirely of
characters from space through tilde is returned unchanged; other text uses
the existing grapheme-aware sanitization.

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
| prose | 5 | 1.387 → 0.966 | 1.388 → 0.966 | 9,492,144 → 7,736,592 |
| prose-lines | 5 | 1.915 → 1.251 | 1.915 → 1.259 | 12,664,096 → 8,885,848 |
| mixed | 5 | 7.733 → 7.193 | 7.727 → 7.171 | 46,417,240 → 41,615,496 |
| table | 5 | 1.219 → 0.991 | 1.218 → 0.993 | 7,983,080 → 7,041,520 |
| long-line | 5 | 1.830 → 1.312 | 1.831 → 1.303 | 12,228,648 → 8,409,944 |
| prose | 25 | 8.763 → 7.620 | 8.761 → 7.628 | 67,184,744 → 58,401,504 |
| prose-lines | 25 | 25.303 → 15.269 | 25.276 → 15.247 | 159,109,960 → 100,200,768 |
| mixed | 25 | 51.543 → 45.032 | 51.550 → 45.006 | 282,479,408 → 258,345,376 |
| table | 25 | 7.882 → 6.621 | 7.866 → 6.617 | 46,555,440 → 41,018,256 |
| long-line | 25 | 25.973 → 15.430 | 25.926 → 15.441 | 157,084,480 → 97,495,888 |
| prose | 100 | 51.640 → 42.554 | 51.452 → 42.528 | 350,065,736 → 314,622,872 |
| prose-lines | 100 | 344.014 → 197.819 | 342.838 → 197.274 | 1,876,583,512 → 1,012,810,920 |
| mixed | 100 | 348.442 → 330.386 | 347.903 → 329.957 | 1,787,271,336 → 1,691,153,224 |
| table | 100 | 88.548 → 76.825 | 88.419 → 76.708 | 393,368,952 → 336,104,856 |
| long-line | 100 | 429.143 → 242.258 | 426.021 → 240.437 | 1,925,557,328 → 1,051,619,064 |

## Repeat and Unicode fallback checks

An eleven-sample repeat, running the candidate first, measured `prose-lines 100`
at 321.512 → 191.132 ms elapsed, 321.283 → 190.796 ms CPU, and
1,876,583,512 → 1,012,810,920 allocated bytes. This confirms roughly 41% less
rendering time and 46% less allocation for that workload.

The following eleven-sample checks use the same settings and both executables
include the new Unicode fixtures; the baseline has only the ASCII guard removed.
Run each executable with `streaming SCENARIO COUNT 64 11 +RTS -T`.

| Scenario | Count | Elapsed ms | CPU ms | Allocated bytes |
| --- | ---: | ---: | ---: | ---: |
| unicode-prose | 5 | 1.479 → 1.370 | 1.480 → 1.362 | 10,591,592 → 8,848,968 |
| unicode-prose | 20 | 13.692 → 11.274 | 13.691 → 11.261 | 88,227,648 → 69,932,704 |
| unicode-tail | 5 | 49.587 → 33.303 | 49.558 → 33.296 | 316,450,776 → 202,878,872 |
| unicode-tail | 20 | 700.619 → 503.677 | 698.833 → 502.191 | 4,525,057,240 → 3,028,550,184 |

`unicode-prose` includes CJK, combining accents, emoji ZWJ sequences and flags.
`unicode-tail` ends each long ASCII line with CJK, exercising a failed guard
after a long prefix. Both still contain printable ASCII spans that benefit from
the guard, especially after wrapping. These full-render results show no observed
regression for the fixtures; they do not imply the Unicode fallback itself is
faster or that its extra guard scan is free.

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

A live tmux smoke test streamed seven-character deltas through the fullscreen
TUI, including bold/link prose, fenced code, and a Unicode table. Resizing from
110×35 to 55×28 preserved wrapping and alignment; PageUp/PageDown navigated
history and returned to the final text correctly.
