# Fullscreen streaming Markdown benchmark

The original section-cache comparison is retained below. For the subsequent
retained-parser comparison, use `current` versus `incremental` and see
[Retained parser](#retained-parser).

`FullscreenMarkdown.hs` retains the pre-optimization production path as
`baseline`: strict `Text` append followed by whole-message Markdown rendering,
with Brick's existing closed-code-body cache. `optimized` additionally caches
completed prose sections. Both call the same underlying Markdown, layout, and
syntax-support code. Completed/history messages use the original renderer:
section caches are only useful while further appends are possible.

## Reproduce

From the repository root:

```sh
nix develop
cabal build --offline agent-tui:bench:fullscreen-markdown-bench
bin=$(cabal list-bin agent-tui:bench:fullscreen-markdown-bench)
"$bin" old mixed 50 64 7 +RTS -T
"$bin" new mixed 50 64 7 +RTS -T
```

Arguments are implementation, workload, count, chunk size in characters, and
sample count. Output is CSV:

```text
implementation,workload,count,chunk_chars,samples,median_wall_ms,median_cpu_ms,median_allocated_bytes
```

The benchmark source and its renderer/support modules are compiled with `-O2`;
shared dependency libraries use the normal Cabal optimization settings. It
runs single-threaded with the default allocation area. Inputs are built and
forced before measurement. A fresh input is read through an IORef inside the
timed region, with a sample-specific heading and a `NOINLINE` consumer to avoid
sharing previously evaluated results.

Each delta appends the strict message body and renders an actual Brick frame
through `renderFinal`, retaining `RenderState` between frames. The viewport is
80×30 and follows a trailing visible marker, so the complete growing message
is laid out while the visible tail is converted into Vty display spans. The
consumer forces span text, attributes, and click extents. This includes cache
construction, parsing, layout, image composition, viewport work, and display
span conversion—not just widget construction or a checksum of source text.
Each streaming workload also renders the completed message once after its last
delta, using the original renderer and retaining the existing code cache.
Thus the final full render is included rather than shifting work out of the
measurement.
GC before each sample is outside timing; GC after timing accounts for the
unfinished nursery in allocation totals. Results are medians.

Before measurement, every old/new frame is checked for equal explicit text
and attributes and equal click extents. Span segmentation is normalized:
different image compositions may split identical text into different spans.
The check reads actual text because `SpanOp`'s `Show` instance omits it.

## Workloads

- `prose`: repeated blank-line-separated paragraphs containing bold, inline
  code, and a link.
- `prose-lines`: the same prose without blank separators, exercising the
  uncached growing section.
- `fence` / `open-fence`: one growing code fence, with/without a closing fence.
- `table` / `open-table`: one growing table, with/without following prose.
- `mixed`: repeated groups of one prose paragraph, a three-line code fence,
  and a three-row table.
- `resize`: mixed, alternating viewport width 80/40 every 50 frames and
  clearing render state as a resize invalidates cached layouts.
- `history-prose` / `history-mixed`: a cold, already-completed message rendered
  once. These ignore the chunk-size argument.

All source workloads are ASCII. Syntax highlighting is disabled in both paths;
closed-code caches and link click targets are enabled.

## Streaming results

GHC 9.10.3, Intel i9-9900K, seven samples per case, including the final
completion render. Allocation is decimal MB. Run each row using the command
above with its workload, count and chunk size.

| Workload | Count | Chunk | Wall old → new (ms) | CPU old → new (ms) | Allocated old → new (MB) |
| --- | ---: | ---: | --- | --- | --- |
| prose | 20 | 64 | 20.752 → 7.417 | 20.689 → 7.390 | 105.341 → 44.667 |
| prose | 50 | 64 | 115.139 → 20.975 | 114.734 → 20.921 | 534.055 → 131.992 |
| prose | 100 | 64 | 432.509 → 47.428 | 431.077 → 47.244 | 1910.340 → 298.162 |
| mixed | 20 | 64 | 385.585 → 46.929 | 384.613 → 46.801 | 1347.121 → 228.169 |
| mixed | 50 | 64 | 2622.406 → 149.306 | 2613.812 → 148.917 | 7796.406 → 719.116 |
| prose-lines | 50 | 64 | 111.443 → 109.335 | 111.143 → 109.063 | 545.848 → 543.401 |
| fence | 50 | 64 | 9.930 → 9.394 | 9.905 → 9.370 | 58.072 → 56.938 |
| open-fence | 50 | 64 | 10.045 → 9.470 | 10.020 → 9.445 | 59.194 → 58.113 |
| table | 50 | 64 | 48.237 → 44.789 | 48.127 → 44.679 | 203.759 → 190.348 |
| open-table | 50 | 64 | 45.223 → 43.892 | 45.108 → 43.788 | 188.381 → 187.445 |
| resize | 50 | 64 | 2484.609 → 200.429 | 2478.281 → 200.102 | 7828.209 → 859.181 |
| prose | 50 | 16 | 438.547 → 62.856 | 437.358 → 62.493 | 2050.115 → 433.812 |
| mixed | 50 | 16 | 9719.336 → 453.186 | 9694.422 → 452.007 | 30848.678 → 2440.931 |
| prose | 50 | 256 | 32.028 → 10.719 | 31.960 → 10.695 | 151.057 → 55.947 |
| mixed | 50 | 256 | 646.209 → 73.620 | 644.435 → 73.401 | 2049.664 → 288.685 |
| mixed (repeat) | 50 | 64 | 2461.051 → 148.458 | 2454.735 → 148.066 | 7796.406 → 719.116 |

The repeated mixed case retains a 16.6× CPU speedup and 90.8% allocation
reduction. The 100-paragraph prose case improves CPU 9.1× and allocation 84.4%.

## Verification

The Markdown GHCi suite passed 66 examples, including every-character
prefix comparisons with warm caches, Unicode, links, tables, fences, clipping,
resize invalidation, and a test proving cached render bodies are not evaluated.
The CLI and TUI loaded together in GHCi (262 modules).

A real fullscreen tmux smoke streamed a 12-section response with headings,
paragraphs, links, a table, and Haskell code. Resizing 100 → 64 → 100 columns
and scrolling with Page Up/Page Down preserved the content and wrapping;
the response reached its final marker and returned to an interactive prompt.

## Scope

This optimization removes repeated rendering of completed prose sections,
including completed tables. It does not make the complete update path linear:
strict body append, contextual fence parsing, section discovery, and image
composition still revisit the growing message. Growing tables, open code
fences, and prose without blank separators remain largely unchanged.

These are renderer CPU/allocation measurements, not provider latency or an
end-to-end application throughput claim.

### Cold completed-message boundary

An initial version applied section caching to every message. That regressed
cold rendering of 50 prose paragraphs from 3.361 to 3.749 ms CPU (+11.5%) and
15.757 to 16.084 MB allocated (+2.1%). That application of the optimization was
removed: only `BlockStreaming` assistant messages use the new path.

With the production dispatch, cold history uses the same renderer as before.
GHC 9.10.3, Intel i9-9900K, medians of 31 samples:

| Cold workload | CPU old → new (ms) | Allocated old → new (MB) |
| --- | --- | --- |
| 50 prose paragraphs | 3.254 → 3.247 | 15.757 → 15.757 |
| 50 mixed groups | 24.410 → 24.694 | 78.138 → 78.138 |

There is no benefit when a response arrives in one large delta and completes
immediately: the speculative streaming cache has no later delta to amortize
its construction. Including the final render, 50 prose paragraphs delivered
in one delta measured 6.878 → 7.076 ms CPU and 31.513 → 31.841 MB allocated.
The mixed equivalent measured 47.791 → 48.504 ms and 146.146 → 146.890 MB.
The change targets incremental streaming, not this two-frame boundary case.

### Cache-lifecycle scope

Production now evicts a terminating streaming message's prose cache entries
through the CLI event loop, preserving its closed-code entries. The renderer
and benchmark source are unchanged: the timings above include the final
completed-message render, but exclude that targeted event-loop cleanup.
The harness retains its prose entries until the sample ends, so these results
do not measure production cache residency after completion.

## Retained parser

`current` is the existing streaming-section-cache renderer (also available as
the historical `new` mode). `incremental` additionally retains
`FenceStreamState`, feeds each new delta inside the measured interval, and
renders its parsed sections. Both retain the strict body append and perform
the same final completed-message render. Cold history bypasses the parser.
The measurement therefore includes parser maintenance rather than moving
that work outside the timed region.

```sh
nix develop
cabal build --offline agent-tui:bench:fullscreen-markdown-bench
bin=$(cabal list-bin agent-tui:bench:fullscreen-markdown-bench)
"$bin" current mixed 100 64 7 +RTS -T
"$bin" incremental mixed 100 64 7 +RTS -T
```

The incremental mode compares every frame with the existing section-cache
renderer before measurement, including normalized display text, attributes,
and click extents. The benchmark's parser, renderer, and support modules use
`-O2`; shared dependency libraries use Cabal's normal optimization settings.
The original `old` and `new` modes remain available.

### Measurements, 2026-09-12

GHC 9.10.3, Apple M3 Max (arm64 macOS), single-threaded RTS with `+RTS -T`,
default allocation area, seven samples per row. Time covers all frames,
including completion; allocation is decimal MB.

| Workload | Count | Chunk | Wall current → incremental (ms) | CPU current → incremental (ms) | Allocated current → incremental (MB) |
| --- | ---: | ---: | --- | --- | --- |
| prose | 20 | 64 | 5.981 → 5.715 | 5.976 → 5.696 | 44.665 → 44.346 |
| prose | 50 | 64 | 17.551 → 17.636 | 17.527 → 17.571 | 131.987 → 130.015 |
| prose | 100 | 64 | 41.656 → 43.949 | 41.628 → 43.759 | 298.150 → 290.413 |
| prose | 200 | 64 | 108.097 → 93.691 | 107.713 → 93.662 | 719.611 → 688.701 |
| mixed | 50 | 64 | 128.041 → 115.699 | 127.752 → 115.640 | 719.094 → 664.796 |
| mixed | 100 | 64 | 381.505 → 335.819 | 379.783 → 335.283 | 1947.231 → 1729.814 |
| prose-lines | 50 | 64 | 86.942 → 86.585 | 86.849 → 86.514 | 543.395 → 542.492 |
| fence | 100 | 64 | 28.643 → 28.926 | 28.611 → 28.891 | 186.993 → 184.940 |
| open-fence | 100 | 64 | 30.257 → 29.397 | 30.217 → 29.384 | 191.361 → 189.400 |
| table | 100 | 64 | 113.322 → 114.826 | 112.837 → 114.611 | 617.845 → 616.585 |
| open-table | 100 | 64 | 115.089 → 112.165 | 114.499 → 112.063 | 614.829 → 613.633 |
| resize | 50 | 64 | 165.758 → 155.165 | 165.556 → 154.723 | 859.159 → 804.829 |
| mixed | 50 | 16 | 381.735 → 347.581 | 381.236 → 347.086 | 2440.845 → 2223.372 |
| mixed | 50 | 256 | 59.804 → 55.980 | 59.752 → 55.949 | 288.679 → 275.052 |
| mixed (repeat) | 50 | 64 | 124.231 → 112.191 | 123.943 → 112.118 | 719.094 → 664.796 |
| history-prose | 50 | 64 | 2.515 → 2.489 | 2.515 → 2.483 | 15.754 → 15.754 |
| history-mixed | 50 | 64 | 18.660 → 18.729 | 18.649 → 18.702 | 78.135 → 78.135 |

The mixed 50-group repeat retains a 9.5% CPU reduction and 7.6% allocation
reduction. The 100-group mixed case reduces CPU 11.7% and allocation 11.2%.
This is an incremental improvement over section caching, not the much larger
historical gain from introducing section caching itself.

The small apparent regressions were repeated in reverse implementation order
with 21 samples. The initial 100-paragraph slowdown did not reproduce; a
growing table remains effectively unchanged. Sample-specific headings have
different digit lengths in the 21-sample run, so allocation totals are not
identical to the seven-sample run.

| Repeated workload | Count | Chunk | Wall current → incremental (ms) | CPU current → incremental (ms) | Allocated current → incremental (MB) |
| --- | ---: | ---: | --- | --- | --- |
| prose | 100 | 64 | 39.375 → 38.301 | 39.361 → 38.281 | 297.649 → 289.913 |
| fence | 100 | 64 | 28.475 → 27.805 | 28.453 → 27.801 | 186.946 → 184.894 |
| table | 100 | 64 | 111.675 → 111.497 | 111.346 → 111.397 | 616.883 → 615.623 |
| prose | 50 | 64 | 16.808 → 16.719 | 16.799 → 16.709 | 131.739 → 129.768 |
| mixed | 100 | 64 | 358.745 → 317.702 | 358.045 → 317.055 | 1947.665 → 1730.266 |
| prose, single delta | 50 | 100000 | 5.317 → 5.300 | 5.318 → 5.306 | 31.840 → 31.816 |
| mixed, single delta | 50 | 100000 | 36.632 → 36.900 | 36.606 → 36.856 | 146.890 → 146.739 |

Every incremental run passed per-frame equivalence. The representative
100-group mixed improvement remains 11.4% CPU and 11.2% allocation on repeat.
No speedup is claimed for growing tables, small prose messages, cold history,
or the single-delta boundary. Their CPU differences are small and inconsistent
between runs.

This change removes rediscovery of completed fences and prose-section
boundaries. It does not make total streaming work linear: strict body append,
unfinished-section parsing/layout, open-code-body flattening, and Brick image
composition still revisit growing state. The benchmark includes those costs
and excludes event-loop cache retirement, terminal I/O, and syntax highlighting
just as the original comparison does.
