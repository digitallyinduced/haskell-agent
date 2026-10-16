# Conversation scrolling

The latest comparison against the merged chunk renderer is documented in
[Cached transcript layout](CachedTranscriptLayout.md). It retains the previous
viewport and eager cache-sizing behavior as `extent-baseline`.

`conversation-scrolling-bench` generates only the requested pages of a synthetic
conversation for history traces. Increasing the total conversation size does not
retain the full history fixture in memory. Live traces instead retain the whole
current conversation, as an uninterrupted turn does. It uses the production page merge, block-ID remapping,
eviction budgets, transcript renderer, Brick scroll requests, scrollbar, and
scrolled-away footer. `application` renders the full application chrome too.

The history source currently fetches 80 turns per page from PostgreSQL. The UI
normally retains at most 200 turns, 1,200 blocks, or 8 MiB of textual history.
A single oversized turn and the active live turn can exceed these limits.
Offloading bounds the loaded history; rendering that loaded window can still
produce substantial temporary allocation on every scroll frame.

## Reproduce

Run the build and measurements inside the development shell:

```sh
nix develop
cabal build --offline agent-cli:bench:conversation-scrolling-bench
B=$(cabal list-bin agent-cli:bench:conversation-scrolling-bench)
for turns in 80 2000 20000; do
  "$B" baseline local "$turns" 3 100 7 +RTS -N4 -T
  "$B" production local "$turns" 3 100 7 +RTS -N4 -T
done
for trace in local paging resize interaction; do
  "$B" baseline "$trace" 20000 12 100 7 +RTS -N4 -T
  "$B" production "$trace" 20000 12 100 7 +RTS -N4 -T
done
```

Arguments are renderer, trace, total turns, body lines per block,
scroll steps, and samples. Each turn has six blocks mixing user messages,
assistant Markdown, inspection output, and tool output. Local scrolling moves
three rows per step, reversing every 25 steps. The paging trace accepts another
80-turn page every eight steps, reverses direction, preserves the previous edge
block, invalidates Brick's cache, and evicts the opposite edge. Resize traces
cycle through 100×32, 76×24, 120×40, and 92×28 terminals.
The interaction trace selects and deselects messages, then hovers and presses
their code-copy controls, exercising the dynamic chunk cache bypass.

The benchmark reports medians of elapsed time, CPU time, allocated bytes, and
retained live bytes after GC. Setup plus first render is reported separately
from the scroll interval, so cache work moved into setup remains visible.
Rendered characters, attributes, and extents are forced. Timed samples retain
only the current app state and render cache, without retaining old pictures.
Counters and invariants check that scrolling moved, paging evicted old turns,
cursor order remained contiguous, and the loaded history stayed within budget.

This is an offline rendering benchmark: database query latency, asynchronous
page delivery, provider work, and terminal output latency are outside its
measurements. The scripted event handler issues Brick scroll requests directly;
production mouse handlers can request additional reflow/image-sync frames.
A page is injected into the same UI merge/eviction path that
accepts a completed database load. Real database paging can be measured with
the separate `agent-store` session history paging benchmark.

The saturated local window contains 200 turns / 1,200 blocks even when the
synthetic store has 20,000 turns. Larger totals exercise cursor and gap handling;
they do not increase the number of rendered blocks. The viewport still measures
the loaded chunks and examines their extents for the footer. Its remaining cost
depends on the loaded window and message contents. This benchmark does not prove
that PostgreSQL queries, including the generation's total-turn count, have
constant latency as stored history grows.

## Check rendering and interactions

The retained `baseline` applies horizontal padding outside each cached chunk,
as the original root pane did. `production` caches that horizontal layout with
the chunk and now also defers cached widget construction and offscreen metadata
clipping. Both use the same transcript contents. Cross-chunk
vertical spacing remains outside the cache because paging can change the
previous block. Padding is part of the cache name; selection and hover still
bypass the affected chunk cache.

Compare exact attributed terminal cells, ordered hit-test extents (including
links and code-copy controls), scrollbars, footer, and viewport geometry:

```sh
mkdir -p "$TMPDIR/conversation-scroll-check"
for trace in local paging resize interaction; do
  "$B" verify baseline "$trace" 20000 12 57 1 +RTS -N4 -T \
    > "$TMPDIR/conversation-scroll-check/baseline.out" \
    2> "$TMPDIR/conversation-scroll-check/baseline.extents"
  "$B" verify production "$trace" 20000 12 57 1 +RTS -N4 -T \
    > "$TMPDIR/conversation-scroll-check/production.out" \
    2> "$TMPDIR/conversation-scroll-check/production.extents"
  diff -u "$TMPDIR/conversation-scroll-check/baseline.out" \
    "$TMPDIR/conversation-scroll-check/production.out"
  diff -u "$TMPDIR/conversation-scroll-check/baseline.extents" \
    "$TMPDIR/conversation-scroll-check/production.extents"
done
```

For a real terminal smoke test, run `"$B" smoke 20000 12` in tmux. This uses
the production fullscreen event handlers with an offline fixture runtime.
Scroll, resize the terminal, and press `q` to exit.

## Long live conversations

The `live`, `live-resize`, and `live-interaction` traces put all six blocks per
turn in `appUi`, without a paged history window. They cover the growing active
conversation, which does not have the history window's retention budget.
`live-baseline` preserves the former live renderer: combine the entire live
conversation, then apply horizontal padding, before viewport cropping.
`production` passes individually padded cached chunks to the measured viewport,
which composes only chunks intersecting the visible rows.

```sh
for turns in 1 80 320 1000; do
  "$B" live-baseline live "$turns" 12 100 7 +RTS -N4 -T
  "$B" production live "$turns" 12 100 7 +RTS -N4 -T
done
for trace in live live-resize live-interaction; do
  "$B" verify live-baseline "$trace" 320 12 57 1 +RTS -N4 -T \
    > "$TMPDIR/conversation-scroll-check/live-baseline.out" \
    2> "$TMPDIR/conversation-scroll-check/live-baseline.extents"
  "$B" verify production "$trace" 320 12 57 1 +RTS -N4 -T \
    > "$TMPDIR/conversation-scroll-check/live-production.out" \
    2> "$TMPDIR/conversation-scroll-check/live-production.extents"
  diff -u "$TMPDIR/conversation-scroll-check/live-baseline.out" \
    "$TMPDIR/conversation-scroll-check/live-production.out"
  diff -u "$TMPDIR/conversation-scroll-check/live-baseline.extents" \
    "$TMPDIR/conversation-scroll-check/live-production.extents"
done
```

Use `"$B" application live 320 12 100 7 +RTS -N4 -T` to include the full
application chrome, and `"$B" smoke-live 320 12` for a live tmux check with the
production input handlers. Setup and first-frame measurements include cold
chunk layout; steady-state results do not hide that cost.

The live viewport still measures each retained chunk and examines message
extents for its footer. This change reduces composition and padding work; it
does not impose a retention budget on the active turn or make scrolling
independent of its size.

Validation for the live chunk change compares exact output and ordered extents
for all seven traces above. The measured viewport, transcript cache, scroll
policy, compact action layout, historical selection/expansion, message footer,
and scrollbar checks pass 57 Hspec examples through GHCi. The action rendering
fixtures use the root transcript chunk path, including the boundary from a
historical action to the first live message. The message footer fixture scrolls
through 65 live messages across cached chunks and resumes at the latest message.
A real tmux run with 1,920 live blocks exercises PageUp, wheel input, PageDown,
100×32 → 76×24 → 100×32 resizing, editable draft retention, and normal `q` exit.

## Measurements, 2026-10-15

Measured on an Apple M3 Max with macOS 26.6.1, GHC 9.10.3 and Cabal 3.16.1.0,
inside `nix develop`. The benchmark component uses `-O2 -threaded -rtsopts`;
dependencies use the repository's default Cabal `-O1` profile and existing
per-module options (including existing `-O0` TUI modules). Every row is a median
of seven samples with `+RTS -N4 -T` and `GHCRTS=-M8G`; measurements ran sequentially.
The initial terminal size is 100×32. Allocation below uses decimal MB.

For `local 20000 12 100 7`, both renderers retain 200 turns / 1,200 blocks and
visit 26 distinct scroll positions:

| Stage | Baseline wall ms | Production wall ms | Baseline CPU ms | Production CPU ms | Baseline allocated MB | Production allocated MB |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Setup + first frame | 278.170 | 264.644 | 328.074 | 314.042 | 811.992 | 811.987 |
| 100 scroll steps | 1904.404 | 1091.656 | 2413.598 | 1342.012 | 5486.794 | 3440.444 |

This reduces scroll wall time by 42.7%, CPU time by 44.4%, and allocation by
37.3%. The mean cost per step within the median trace is 19.04 → 10.92 ms.
Retained live bytes after scrolling increase from 25,210,888 to 26,174,232
(+963,344 bytes, about 0.92 MiB) because padded chunk results are cached.
Setup allocation is unchanged: the improvement removes repeated horizontal
extent transformations without deferring more rendering into startup.

Additional 100-step traces, shown as baseline → production:

| Trace | Stored turns | Body lines | Wall ms | CPU ms | Allocated MB |
| --- | ---: | ---: | ---: | ---: | ---: |
| Local | 80 | 3 | 268.131 → 170.502 | 291.143 → 179.887 | 1313.072 → 837.295 |
| Local | 2,000 | 3 | 1011.006 → 573.991 | 1269.673 → 684.947 | 2950.684 → 1778.961 |
| Local | 20,000 | 3 | 1016.977 → 543.281 | 1276.048 → 657.632 | 2952.124 → 1780.401 |
| Paging | 20,000 | 12 | 4950.926 → 4303.523 | 5944.193 → 5073.852 | 15082.133 → 13327.723 |
| Resize | 20,000 | 12 | 4166.642 → 3363.942 | 5054.726 → 3995.982 | 12421.438 → 10530.512 |
| Interaction | 20,000 | 12 | 1881.930 → 1097.658 | 2409.109 → 1363.358 | 5621.908 → 3601.376 |

The 80-turn case holds 480 blocks. Both larger local cases hold the same
200-turn / 1,200-block window and have nearly identical allocation despite a
tenfold increase in stored turns. Their post-scroll live memory is also stable:
about 17.13 → 17.60 MB in either larger case. The paging trace loads 13 pages,
evicts 880 turns, visits 43 viewport positions and never exceeds 200 loaded
turns / 1,200 blocks. It ends with 25.14 → 25.99 MB live. Paging and resizing
still rebuild chunk caches, so their relative gains are smaller than ordinary
scrolling.

Repeating `local 20000 12 100 7` with production measured first gives
1806.283 → 1049.872 ms wall, 2317.601 → 1296.214 ms CPU, and the same
5486.794 → 3440.444 MB allocation. That is a 41.9% wall-time reduction;
the gain survives reversed measurement order. Setup wall time in this repeat
is 262.809 → 272.611 ms, with unchanged allocation, so small startup timing
differences should be treated as measurement variation.

The full `drawApp` path was also measured using a saved binary linked against
the pre-change library and the candidate binary, with the same local fixture
and scroll sequence (`application local 20000 12 100 7 +RTS -N4 -T`):

| Stage | Before wall ms | After wall ms | Before CPU ms | After CPU ms | Before allocated MB | After allocated MB |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Setup + first frame | 273.899 | 288.078 | 323.023 | 337.832 | 812.108 | 812.331 |
| 100 scroll steps | 1896.698 | 1206.084 | 2406.260 | 1459.039 | 5505.407 | 3459.419 |

Including the application chrome, scroll time falls by 36.4%, CPU time by
39.4%, and allocation by 37.2%. Post-scroll live memory is 25.35 → 26.23 MB.
These are offline render costs; input-to-display latency still includes the
terminal and the production event-handler behavior described above.

The exact differential checks above passed for all four traces with 57 steps,
20,000 turns and 12 body lines. The measured-viewport and history Hspec suites
passed all 64 examples. A real tmux smoke check exercised PageUp, PageDown,
100×32 → 76×24 → 100×32 resize, the message footer and the prompt, then exited
normally with `q`.

## Live conversation measurements, 2026-10-16

Measured on an Apple M3 Max with macOS 26.6.1 and GHC 9.10.3 inside
`nix develop`. The benchmark uses `-O2 -threaded -rtsopts`; dependencies use
the default Cabal `-O1` profile and existing per-module options. Each result
is the median of seven samples, with `GHCRTS=-M8G`, `+RTS -N4 -T`, 12 body
lines per block, a 100×32 terminal, and 100 scroll steps. Measurements run
sequentially. Allocation below uses decimal MB.

The live traces have no historical window: all blocks remain in the current
UI state. `live-baseline` reproduces the previous combined live image;
`production` retains its chunk boundaries through viewport composition.

| Live blocks | Before wall ms | After wall ms | Before CPU ms | After CPU ms | Before allocated MB | After allocated MB |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| 480 | 725.466 | 391.399 | 883.190 | 473.263 | 3000.952 | 1518.615 |
| 1,920 | 3323.471 | 1785.103 | 4104.255 | 2203.990 | 11303.149 | 5298.795 |
| 6,000 | 11049.967 | 5424.778 | 13937.251 | 6877.141 | 34847.849 | 16038.438 |

At 1,920 blocks, mean cost per step within the median trace falls from
33.23 to 17.85 ms (46.3%), with 53.1% less allocation. At 6,000 blocks it
falls from 110.50 to 54.25 ms (50.9%), with 54.0% less allocation. Each trace
visits 26 distinct viewport positions.

Cold setup and first render still measure every chunk. At 6,000 blocks their
wall times are 1373.795 → 1373.938 ms, CPU times 1609.190 → 1608.616 ms,
and allocations 4122.210 → 4017.173 MB. The scroll improvement does not move
work into setup. Post-scroll retained memory is 129.307 → 129.582 MB, an
increase of about 0.26 MiB for cached horizontal layout. At 1,920 blocks,
setup wall time is 439.720 → 461.059 ms and allocation 1320.166 → 1286.741 MB;
small startup timing differences should not be interpreted as a speedup.

Resizing the 1,920-block live transcript every twelve steps gives
7147.575 → 6386.287 ms wall, 8507.008 → 7190.159 ms CPU, and
22458.933 → 16627.409 MB allocated across 100 steps. Its improvement is
smaller because changed terminal dimensions rebuild the chunk caches.

Selecting, deselecting, hovering, and pressing code-copy controls across chunks
gives 3091.346 → 1934.859 ms wall, 3881.215 → 2334.509 ms CPU, and
10350.378 → 5450.635 MB allocated. Both renderers visit 18 viewport positions.

Repeating the 1,920-block local measurement with production first gives
3722.076 → 1757.561 ms wall and 4368.746 → 2179.928 ms CPU, with unchanged
allocation. The wall-time improvement remains 46–53% across the two runs.
Setup wall times in this repeat are 475.732 → 459.236 ms; setup allocation
remains lower in production, with no consistent startup timing regression.

The full `application live 320 12 100 7` path takes 1934.567 ms wall,
2322.547 ms CPU, and allocates 5317.303 MB for the scroll trace. This is a
candidate-only integration measurement, including application chrome; the
before/after comparisons above isolate the root conversation pane.

The six-block case has no full chunk to cache. Its first run gave
35.976 → 44.637 ms for 100 steps. Two repeats with production measured first
gave 41.319 → 41.986 ms and 39.436 → 39.309 ms, with CPU times
44.675 → 45.637 ms and 43.023 → 42.504 ms. Allocation is effectively unchanged
at 271.562 → 271.615 MB (0.02%). Short-transcript timings vary around
0.4 ms per step; the repeats show no consistent regression or improvement.
