# Cached transcript layout, 2026-10-16

Brick's `cached` combinator forwards the underlying widget's growth policies.
The viewport's size prepass therefore constructed Markdown widget trees for
every retained chunk, even when its rendered result was already cached. Stable
chunks now expose their existing `Greedy` width / `Fixed` height policies without
constructing those trees. `Fixed` means intrinsic content height; it does not
give every chunk a constant row count. Dynamic and incomplete chunks retain
their existing cache bypass.

The measured viewport also retains raw chunk results and clips their images to
establish geometry. Full cursor, extent, and border clipping happens only for
visible chunks and touching neighbors. The footer selects message, block, and
history-gap extents before clipping and translating them. Click targets still
receive Brick's normal clipping, independently of that footer filter. Combined
height retains Brick's 100,000-row release limit and decreasing child budget.

## Measurement method

The baseline is the implementation merged in
`6b6c6957807974d0ff73d81745fe8c3b53e8932d` (PR #1414). The benchmark retains
its viewport implementation in `MeasuredViewportBaseline.hs` and supplies the
original Brick `cached` combinator to the production transcript renderer.
`extent-baseline` combines both old behaviors. `sizing-only` and `metadata-only`
isolate each improvement; `production` includes both. Earlier `baseline` and
`live-baseline` modes remain available for the preceding changes.

Measurements use an Apple M3 Max, macOS 26.6.1, GHC 9.10.3, Cabal 3.16.1.0,
and Brick 2.9 inside `nix develop`. The driver uses `-O2`; the retained viewport
baseline explicitly uses `-O1`, matching the library build profile. Existing
per-module options are unchanged. Each number is the median of seven samples
with `GHCRTS=-M8G` and `+RTS -N4 -T`. Measurements ran sequentially without
concurrent builds or tests. The initial terminal is 100×32 with reduced motion.

Each synthetic turn has six blocks and 12 body lines per block. The benchmark
forces attributed output and extent metadata. Setup plus first frame is timed
separately. Post-scroll memory is measured after GC with the final app state
and render cache retained, without retaining previous frames. MB is decimal.
The workload, forcing, paging budgets, and scroll sequence are shared between
implementations; see [Conversation scrolling](ConversationScrolling.md).

## Results

All rows below measure 100 scroll steps. Live traces retain the complete live
conversation. History traces have 20,000 stored turns and at most 200 loaded
turns / 1,200 blocks. Paging loads 13 pages, evicts 880 turns, and visits the
same 43 scroll positions in both implementations.

| Trace | Turns | Wall ms, before → after | CPU ms, before → after | Allocated MB, before → after |
| --- | ---: | ---: | ---: | ---: |
| Live | 1 | 37.138 → 35.421 | 40.420 → 38.408 | 271.624 → 271.274 |
| Live | 80 | 447.935 → 65.466 | 525.711 → 71.048 | 1518.647 → 435.847 |
| Live | 320 | 1748.074 → 185.038 | 2178.856 → 194.510 | 5298.898 → 946.195 |
| Live | 1,000 | 5500.504 → 761.221 | 6871.317 → 938.579 | 16038.746 → 2449.501 |
| Local history | 20,000 | 957.353 → 118.162 | 1193.036 → 125.333 | 3437.576 → 745.212 |
| Paging | 20,000 | 4023.359 → 3334.577 | 4774.703 → 3869.339 | 13326.332 → 10983.200 |
| Resize history | 20,000 | 3148.852 → 2386.710 | 3743.100 → 2763.419 | 10527.547 → 8049.479 |
| Interaction history | 20,000 | 1451.352 → 234.896 | 1588.330 → 241.328 | 3598.501 → 945.456 |
| Live resize | 320 | 6081.566 → 4316.422 | 7057.323 → 4969.406 | 16627.513 → 12619.376 |
| Live interaction | 320 | 1853.054 → 253.638 | 2300.242 → 284.367 | 5450.739 → 1132.706 |

At 1,920 live blocks, the average step within the median trace falls from
17.48 to 1.85 ms (9.4× faster), with 82.1% less allocation. At 6,000 blocks,
it falls from 55.01 to 7.61 ms (7.2× faster). The single-turn case, which has
no full cacheable chunk, has essentially unchanged cost.

The full `drawApp` path was also measured against an archived binary built at
the merged baseline, using `application live 320 12 100 7 +RTS -N4 -T`:

| Stage | Wall ms, before → after | CPU ms, before → after | Allocated MB, before → after |
| --- | ---: | ---: | ---: |
| Setup + first frame | 445.960 → 436.119 | 533.447 → 511.649 | 1287.118 → 1281.884 |
| 100 scroll steps | 1750.248 → 209.742 | 2166.195 → 221.644 | 5317.997 → 965.403 |

Including application chrome, the average scroll step falls from 17.50 to
2.10 ms (8.3× faster), with 81.8% less allocation. Post-scroll live memory is
41.576 → 41.575 MB.

Neither change shifts scroll work into setup. Representative cold results:

| Trace | Setup wall ms, before → after | Setup CPU ms, before → after | Setup allocated MB, before → after | Post-scroll live MB, before → after |
| --- | ---: | ---: | ---: | ---: |
| Live 1 | 1.428 → 1.443 | 1.516 → 1.524 | 6.401 → 6.398 | 0.299 → 0.301 |
| Live 80 | 114.924 → 111.766 | 136.244 → 131.969 | 323.354 → 322.096 | 10.451 → 10.455 |
| Live 320 | 447.976 → 442.147 | 537.132 → 516.544 | 1286.742 → 1281.507 | 41.519 → 41.519 |
| Live 1,000 | 1368.203 → 1317.554 | 1599.780 → 1550.697 | 4017.176 → 4000.639 | 129.584 → 129.588 |
| Local history | 249.805 → 252.363 | 296.668 → 300.509 | 812.157 → 808.876 | 26.176 → 26.173 |

All ten traces have post-scroll retained-memory differences below 7 KiB.
Minor cold timing differences should be treated as variation. A reversed-order
repeat of live 320 measured production first: baseline → production is
1660.749 → 186.668 ms, with the same allocation as the first run. Cold wall
time in that repeat is 424.156 → 435.147 ms.

Both changes help independently (live 320, 100 steps):

| Renderer | Wall ms | CPU ms | Allocated MB |
| --- | ---: | ---: | ---: |
| `extent-baseline` | 1748.074 | 2178.856 | 5298.898 |
| `sizing-only` | 402.396 | 499.380 | 1453.146 |
| `metadata-only` | 1497.336 | 1805.750 | 4791.947 |
| `production` | 185.038 | 194.510 | 946.195 |

## Reproduce and verify

```sh
nix develop
cabal build --offline agent-cli:bench:conversation-scrolling-bench
scroll_benchmark=$(cabal list-bin agent-cli:bench:conversation-scrolling-bench)
export GHCRTS=-M8G
for turns in 1 80 320 1000; do
  for renderer in extent-baseline production; do
    "$scroll_benchmark" "$renderer" live "$turns" 12 100 7 +RTS -N4 -T
  done
done
for renderer in extent-baseline sizing-only metadata-only production; do
  "$scroll_benchmark" "$renderer" live 320 12 100 7 +RTS -N4 -T
done
for trace in local paging resize interaction; do
  for renderer in extent-baseline production; do
    "$scroll_benchmark" "$renderer" "$trace" 20000 12 100 7 +RTS -N4 -T
  done
done
for trace in live-resize live-interaction; do
  for renderer in extent-baseline production; do
    "$scroll_benchmark" "$renderer" "$trace" 320 12 100 7 +RTS -N4 -T
  done
done
```

Use the differential-output commands in `ConversationScrolling.md`, replacing
`baseline` with `extent-baseline`, for all seven traces. Verification passed
with 57 steps, 12 body lines, 320 live turns or 20,000 stored turns: attributed
terminal cells, ordered visible hit targets, footer, and viewport geometry
match exactly. Both isolated optimizations also match on `live-interaction`.
Full application output matches the archived merged binary on `live`,
`live-resize`, and `live-interaction` (12 comparisons total).

The focused viewport, transcript, scroll-policy, action-rendering, cache,
footer, and scrollbar suites pass 60 Hspec examples through GHCi. Tests cover
growth policies for all 11 stable block kinds, footer extent clipping outside
the measurement context, and visible controls after scrolling and resizing.
A live tmux run of `"$scroll_benchmark" smoke-live 320 12` exercised PageUp,
wheel input, PageDown, 100×32 → 76×24 → 100×32 resizing, editable draft
retention, End to resume following, and normal `q` exit.

## Remaining boundaries

Scrolling still visits every loaded chunk and scans message extents for the
footer. It remains sensitive to retained conversation size and content.
Growing partial chunks and dynamic selections render without the stable cache;
a single large active message can remain expensive. Resize and page replacement
invalidate caches and retain substantial cold layout cost, explaining their
smaller gains. This change adds no retention limit to the active conversation.

These measurements cover offline rendering. Database latency, asynchronous page
delivery, model work, terminal output, and extra production input-handler frames
are outside the timed interval.
