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

## Retained Markdown syntax

`streaming` retains block and inline syntax using `MarkdownStreamState`.
Compare it with `incremental`, the fence-only retained parser from PR #1278,
not the older `current` section-cache implementation. All historical modes
remain runnable. Both implementations append the strict message body, feed
the new delta inside the measured interval, render every frame through Brick,
and perform the final non-streaming render. Before measurement, every frame
is checked against the independent section-cache renderer for display text,
attributes, and click extents.

```sh
nix develop
cabal build --offline agent-tui:bench:fullscreen-markdown-bench
bin=$(cabal list-bin agent-tui:bench:fullscreen-markdown-bench)
for mode in incremental streaming; do
  "$bin" "$mode" prose-lines 100 64 7 +RTS -T
  "$bin" "$mode" mixed 100 64 7 +RTS -T
  "$bin" "$mode" long-line 100 64 7 +RTS -T
  "$bin" "$mode" incomplete 100 64 7 +RTS -T
done
```

`prose-lines` has completed prose lines without blank paragraph boundaries.
`long-line` repeats the same styled prose on one line. `incomplete` keeps an
unclosed emphasis/link prefix before a growing line containing code spans:
this tests the ambiguous-suffix boundary rather than only complete syntax.
Counts represent repeated workload units; chunk sizes are Unicode characters.

`unmatched-code` and `unmatched-link` retain an unclosed inline code span or
link destination. `baseline-inline` and `streaming-inline` are parser-only
diagnostics using these same input deltas, retaining the strict body append
and forcing a checksum of the entire syntax tree after each delta. They check
every prefix against `parseInline` before measurement. These modes omit Brick
layout and cannot establish a fullscreen performance improvement.

```sh
for mode in baseline-inline streaming-inline; do
  "$bin" "$mode" long-line 100 64 7 +RTS -T
  "$bin" "$mode" unmatched-code 100 64 7 +RTS -T
  "$bin" "$mode" unmatched-link 100 64 7 +RTS -T
done
```

Streaming snapshots retain literal fallback for unresolved syntax. An
ambiguous suffix may grow without bound and still requires reparsing in the
snapshot; resumable feed state does not make total per-frame work linear.

### Retained-syntax measurements

#### Final validation

Same machine, GHC 9.10.3 and `-O2` settings as below; count 100,
64-character deltas, 21 samples (`prose`: 31), `+RTS -T`.
The first four baselines use the saved executable from `eab8603f9` to avoid
shared-renderer baseline drift. The two newly added workloads use the retained
`incremental` mode in the current executable. Every invocation passed the
per-frame picture/style/click-extent equivalence check.

| Workload | Wall baseline → streaming ms | CPU ms | Allocated MB |
| --- | --- | --- | --- |
| prose | 39.059 → 39.446 | 38.977 → 39.376 | 289.913 → 290.684 |
| prose-lines | 334.944 → 305.146 | 334.040 → 304.453 | 1942.843 → 1745.893 |
| mixed | 328.451 → 324.783 | 326.804 → 324.169 | 1730.266 → 1698.266 |
| table | 112.123 → 84.699 | 111.998 → 84.591 | 615.623 → 393.293 |
| long-line | 377.901 → 350.799 | 377.288 → 350.240 | 1926.716 → 1782.735 |
| incomplete | 42.496 → 28.336 | 42.475 → 28.320 | 376.868 → 191.140 |

Continuous prose uses 8.9% less CPU and 10.1% less allocation; tables use
24.5% less CPU and 36.1% less allocation. The incomplete-markup fixture uses
33.3% less CPU and 49.3% less allocation. Mixed content is effectively flat
in CPU, with 1.8% less allocation. Short-paragraph CPU is about 1% higher:
a reversed-order repeat measured baseline 38.894 ms versus streaming 39.252 ms.
The always-on short-inline scanner was redesigned to use batch parsing up to
128 characters, but retained block bookkeeping still has a small cost here.
No short-paragraph or universal no-regression claim is made.

Run the final workload checks with the executable setup above:

```sh
for scenario in prose prose-lines mixed table long-line incomplete; do
  for mode in incremental streaming; do
    "$bin" "$mode" "$scenario" 100 64 21 +RTS -T
  done
done
```

For an independent pre-change comparison, build the benchmark at `eab8603f9`
and substitute that executable for `incremental` on the first four workloads.
It does not contain the `long-line` or `incomplete` scenarios.

#### Earlier implementation matrix

The matrix below records the initial retained-syntax implementation, before
the final line-boundary reuse, ordinary-prose classification fast path, and
batch fallback for inline bodies of at most 128 characters.
See the final validation measurements above for the final implementation.

Measured on Apple M3 Max/macOS arm64, GHC 9.10.3, benchmark source modules
compiled with `-O2`, default single-thread RTS and `+RTS -T`. Values are medians
of seven samples; allocation is decimal MB. Every measured invocation passed
per-frame equivalence. Arrows below mean `incremental` → `streaming`.

| Workload | Count | Chunk | Wall ms | CPU ms | Allocated MB |
| --- | ---: | ---: | --- | --- | --- |
| prose-lines | 50 | 64 | 84.321 → 75.739 | 84.160 → 75.697 | 545.071 → 494.085 |
| prose-lines | 100 | 64 | 313.519 → 282.306 | 313.285 → 282.030 | 1953.380 → 1745.974 |
| prose-lines | 200 | 64 | 1204.561 → 1097.227 | 1203.494 → 1096.521 | 7210.924 → 6381.328 |
| long-line | 50 | 64 | 89.415 → 82.331 | 89.382 → 82.263 | 536.251 → 500.571 |
| long-line | 100 | 64 | 360.264 → 328.410 | 358.983 → 328.519 | 1927.189 → 1783.086 |
| long-line | 200 | 64 | 1542.112 → 1345.816 | 1538.568 → 1343.593 | 7090.156 → 6515.806 |
| incomplete | 50 | 64 | 10.924 → 7.744 | 10.920 → 7.743 | 103.815 → 55.992 |
| incomplete | 100 | 64 | 40.947 → 27.692 | 40.970 → 27.621 | 377.097 → 190.438 |
| incomplete | 200 | 64 | 155.441 → 101.926 | 155.094 → 101.970 | 1336.197 → 637.899 |
| table | 50 | 64 | 32.552 → 24.726 | 32.560 → 24.728 | 191.924 → 130.524 |
| table | 100 | 64 | 106.152 → 79.446 | 106.179 → 79.446 | 623.395 → 394.360 |
| table | 200 | 64 | 414.956 → 311.894 | 414.089 → 311.217 | 2223.239 → 1339.749 |
| mixed | 50 | 64 | 110.534 → 108.319 | 110.306 → 108.247 | 666.624 → 646.548 |
| mixed | 100 | 64 | 312.356 → 303.641 | 311.897 → 303.213 | 1733.480 → 1698.171 |
| mixed | 200 | 64 | 1090.264 → 1102.583 | 1089.308 → 1101.596 | 5057.506 → 5008.271 |
| mixed | 50 | 16 | 326.345 → 323.434 | 326.233 → 323.118 | 2226.845 → 2175.469 |
| mixed | 50 | 256 | 54.669 → 54.108 | 54.615 → 54.068 | 276.460 → 264.041 |
| mixed, single delta | 50 | 100000 | 35.408 → 34.323 | 35.420 → 34.327 | 148.015 → 137.870 |
| open-table | 100 | 64 | 107.445 → 81.793 | 107.467 → 81.748 | 620.438 → 391.393 |
| resize | 100 | 64 | 455.271 → 442.004 | 454.392 → 436.445 | 2291.299 → 2094.601 |

The main benefit is retained syntax in growing prose and tables, not a large
additional speedup for already-cached mixed messages. The initial implementation
regressed incomplete syntax and long lines; avoiding repeated block-prefix scans
and reusing literal-fallback syntax removed those large regressions.

Boundary and representative workloads were repeated with 21 samples, reversing
implementation order except the 200-group mixed repeat:

| Workload | Count | Chunk | Wall ms | CPU ms | Allocated MB |
| --- | ---: | ---: | --- | --- | --- |
| unmatched-code | 100 | 64 | 10.272 → 10.671 | 10.238 → 10.618 | 67.803 → 67.882 |
| unmatched-link | 100 | 64 | 20.675 → 20.554 | 20.657 → 20.534 | 186.227 → 186.236 |
| prose | 100 | 64 | 37.857 → 38.513 | 37.681 → 38.494 | 290.392 → 290.412 |
| prose | 5 | 64 | 1.206 → 1.354 | 1.207 → 1.347 | 8.802 → 8.759 |
| fence | 100 | 64 | 27.906 → 27.751 | 27.899 → 27.740 | 184.906 → 184.955 |
| open-fence | 100 | 64 | 28.732 → 28.192 | 28.624 → 28.167 | 189.363 → 189.414 |
| history-mixed | 100 | 64 | 38.257 → 37.731 | 38.198 → 37.723 | 156.579 → 156.579 |
| mixed | 100 | 64 | 313.333 → 307.255 | 313.259 → 306.531 | 1733.938 → 1698.514 |
| mixed | 200 | 64 | 1088.515 → 1075.271 | 1087.332 → 1074.837 | 5057.604 → 5008.369 |

The mixed-200 apparent slowdown reversed on repeat. Small prose and unmatched
code still show overhead, so these measurements do not support a universal
no-regression claim. The five-paragraph difference is 0.140 ms per complete
stream, not per frame.

Three further interleaved subprocess pairs on unchanged sources did not
consistently reproduce the small-boundary slowdowns: prose-5/64/51 CPU was
1.286 → 1.285, 1.272 → 1.321, and 1.271 → 1.275 ms; unmatched-code-100/64/21
was 10.587 → 10.372, 10.592 → 10.622, and 10.402 → 10.280 ms.
These sub-millisecond differences are sensitive to process/run conditions;
no speedup is claimed for these boundaries.

A saved executable built from PR #1278 independently checked baseline drift.
For mixed-100/64/21 it measured wall 317.523 ms, CPU 313.470 ms and 1730.266 MB,
versus streaming wall 310.250 ms, CPU 309.687 ms and 1698.514 MB. Its prose-lines
and table allocation totals were 1943.327 and 616.585 MB (100/64/7), slightly
below the in-tree baseline; the major prose/table gains remain.

Parser-only diagnostics (100/64/7) illustrate why parser timings alone are
insufficient:

| Workload | Wall baseline → streaming ms | CPU ms | Allocated MB |
| --- | --- | --- | --- |
| long-line | 27.048 → 2.152 | 27.038 → 2.163 | 155.719 → 12.234 |
| incomplete | 13.470 → 1.558 | 13.457 → 1.558 | 207.310 → 20.528 |
| prose-lines | 16.973 → 2.460 | 16.975 → 2.466 | 168.021 → 15.881 |
| unmatched-code | 0.112 → 0.087 | 0.112 → 0.088 | 0.721 → 0.699 |
| unmatched-link | 0.321 → 0.331 | 0.321 → 0.331 | 3.143 → 3.123 |

Separate `100 64 1 +RTS -T -s` subprocesses reported maximum residency:
prose-lines 1.276 → 1.433 MB, mixed 16.707 → 20.255 MB, long-line
1.767 → 1.809 MB. **These include the untimed equivalence pass**, which runs
both the oracle and selected renderer; they are whole-harness peaks, not
isolated parser-state sizes. Reduced allocation does not imply lower residency:
retained syntax occupies additional live memory.

Reproduce the matrix with the build and `bin` setup above:

```sh
for count in 50 100 200; do
  for scenario in prose-lines mixed long-line incomplete table; do
    for mode in incremental streaming; do
      "$bin" "$mode" "$scenario" "$count" 64 7 +RTS -T
    done
  done
done
for scenario in unmatched-code unmatched-link prose fence open-fence history-mixed mixed; do
  for mode in streaming incremental; do
    "$bin" "$mode" "$scenario" 100 64 21 +RTS -T
  done
done
for scenario in prose-lines mixed long-line; do
  for mode in incremental streaming; do
    "$bin" "$mode" "$scenario" 100 64 1 +RTS -T -s
  done
done
```
