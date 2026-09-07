# Measured viewport rendering

Status: root conversation integration locally validated. These are local
renderer benchmarks, not released UI or terminal-I/O measurements.

The adapter measures retained chunks, delegates scroll resolution to Brick,
and composes only intersecting results plus touching neighbors for border
joins. This still visits every chunk on each frame; it is not constant-time
virtualization or a persistent height index.

## Reproduce

```sh
nix develop -c cabal build --offline agent-cli:bench:transcript-scrolling-bench
B=$(nix develop -c cabal list-bin agent-cli:bench:transcript-scrolling-bench)
for n in 100 600 1200 1200; do
  "$B" history-chunk-cache "$n" 3 7 +RTS -T
  "$B" history-measured-viewport "$n" 3 7 +RTS -T
done
```

Same fixture, compiler settings, forcing, and timing method as
[HistoryRendering.md](HistoryRendering.md). Seven samples, median wall-time
sample, 25 warm redraws per sample, 100×32 viewport, no scrollbar. These are
stationary redraws, not terminal I/O.

## Initial results (2026-09-07, aarch64 macOS)

Warm totals, milliseconds / decimal MB allocated:

| Blocks | Retained chunk cache | Measured viewport |
|---:|---:|---:|
| 100 | 15.176 / 124.425 | 12.715 / 101.805 |
| 600 | 106.015 / 520.850 | 78.102 / 332.970 |
| 1200 | 206.503 / 991.565 | 150.497 / 607.369 |
| 1200 repeat | 206.269 / 991.565 | 146.573 / 607.375 |

Repeated 1200-block run: approximately 29% less wall time and 39% less
allocation. Setup plus first render: 182.822 → 172.127 ms and
639.580 → 612.248 MB.

## Scrolling and resize trace

```sh
for n in 100 600 1200 1200; do
  "$B" history-chunk-cache-trace "$n" 3 7 +RTS -T
  "$B" history-measured-viewport-trace "$n" 3 7 +RTS -T
done
```

Both trace modes use the same retained chunks, production right-hand scrollbar
renderer, and Brick visibility requests. Each 25-frame sample cycles requested
rows `0, 400, 1200, 80, 2400, 600, 3600, 160` and display regions
100×32, 76×24, 120×40, and 92×28. The benchmark forces rendered characters,
attributes, widths, and extent counts. The baseline preserves the production
`padLeftRight 2 (drawTranscript state)` hierarchy.

Warm trace totals, milliseconds / decimal MB allocated:

| Blocks | Retained chunk cache | Measured viewport |
|---:|---:|---:|
| 100 | 12.010 / 91.163 | 11.139 / 88.680 |
| 600 | 113.239 / 511.666 | 81.876 / 335.826 |
| 1200 | 223.479 / 1038.043 | 159.535 / 616.728 |
| 1200 repeat | 222.619 / 1038.349 | 150.843 / 616.728 |

The repeated 1200-block trace used about **32% less wall time and 41% less
allocation**. Setup plus first render was 194.547 → 173.844 ms and
659.198 → 612.292 MB.

Long-body trace (`100 30 7`): 79.870 → 66.526 ms and
390.607 → 319.399 MB warm; setup plus first render was 78.139 → 58.765 ms and
319.494 → 291.026 MB.

At the smallest trace size, setup plus first render was faster
(14.111 → 12.695 ms) but allocated 2.4% more (52.107 → 53.374 MB).

## Validation and scope

- Combined history, application, and measured-viewport suite: 173 examples,
  zero failures after a clean GHCi reload.
- Twelve differential cases compare actual characters/attributes, extents,
  cursor positions, and mouse dispatch with Brick's ordinary viewport,
  including narrow/empty viewports, scrollbar styling, and dynamic borders.
- Live fullscreen tmux smoke passed with a deterministic 600-block history:
  typing/cursor movement, page scrolling, resizing while scrolled, and returning
  to live output preserved the composer and right scrollbar. No provider calls
  were made.
- Only the root conversation path is integrated. Child-agent viewports and the
  empty welcome path remain unchanged.
- Stage-level profiling is deferred; measurement/cache lookup is still linear.
