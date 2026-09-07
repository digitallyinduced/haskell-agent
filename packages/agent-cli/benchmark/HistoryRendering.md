# Loaded-history rendering

The history setter now retains the per-turn coalesced display projection,
chunked across turns. Redraws reuse the existing 32-block image cache. Selected,
hovered, expanded, incomplete, and partial chunks retain the existing dynamic
rendering policy. Live transcript projection is unchanged.

## Reproduce

```sh
nix develop -c cabal build --offline agent-cli:bench:transcript-scrolling-bench
B=$(nix develop -c cabal list-bin agent-cli:bench:transcript-scrolling-bench)
for n in 100 600 1200 1200; do
  "$B" history-per-block "$n" 3 7 +RTS -T
  "$B" history-chunk-cache "$n" 3 7 +RTS -T
done
```

Measured on aarch64 macOS, GHC 9.10.3, Brick 2.9. Benchmark module uses
`-O2`; linked local libraries use Cabal's default `-O1`, identically for both
workloads. These are compiled results, not GHCi timings.

The fixture uses completed assistant blocks and every fifth block is a tool,
with Markdown bodies, ten blocks per history turn, and a 100×32 vertical
viewport with production horizontal padding. Both workloads call production
block rendering; the baseline preserves the old per-frame flatten/coalesce.
Each redraw constructs a fresh widget with a changing elapsed-time field and
carries Brick's render state forward. Picture display spans and extent counts
are forced. This measures rendering, not terminal I/O or the full event loop.

Seven samples; table reports the median wall-time sample and its CPU/allocation
measurements. Warm samples contain 25 redraws after an untimed cache warm-up.
Cold samples include history indexes/projection and one render with an empty
image cache; input blocks/runtime are prepared outside timing. GC runs before
each sample and after timing to flush nursery allocation counters.

## Results

Warm totals (milliseconds and decimal MB allocated per 25 redraws):

| Blocks | Old wall / CPU / MB | New wall / CPU / MB |
|---:|---:|---:|
| 100 | 31.173 / 31.142 / 210.416 | 27.634 / 27.615 / 193.131 |
| 600 | 147.352 / 147.106 / 694.032 | 115.802 / 115.717 / 589.562 |
| 1200 | 287.987 / 287.777 / 1278.144 | 212.507 / 212.354 / 1060.277 |
| 1200 repeat | 285.953 / 285.720 / 1278.140 | 212.165 / 211.897 / 1060.277 |

At 1200 blocks this is about **26% less redraw time and 17% less allocation**,
or 11.52 → 8.50 ms per redraw. Cost still grows with history size; chunk
caching does not eliminate Brick's extent/viewport bookkeeping.

Setup plus first render (one render per sample):

| Blocks | Old wall / CPU / MB | New wall / CPU / MB |
|---:|---:|---:|
| 100 | 14.474 / 14.449 / 57.647 | 14.641 / 14.621 / 57.668 |
| 600 | 85.845 / 85.667 / 324.629 | 86.569 / 86.400 / 323.350 |
| 1200 | 175.582 / 175.477 / 645.470 | 175.888 / 175.749 / 642.333 |
| 1200 repeat | 180.500 / 179.672 / 645.503 | 178.466 / 178.167 / 642.333 |

Cold cost is approximately unchanged, rather than hiding a substantial
first-render penalty behind the warm-cache improvement.

Longer-body check (`100 30 7`, including fenced code): warm old
100.971 ms / 100.893 CPU ms / 553.564 MB versus new
94.183 ms / 94.128 CPU ms / 514.257 MB. Cold old
62.771 ms / 62.725 CPU ms / 303.831 MB versus new
64.996 ms / 64.962 CPU ms / 303.852 MB: about 2.2 ms more first-render
time in this sample, with essentially identical allocation.
