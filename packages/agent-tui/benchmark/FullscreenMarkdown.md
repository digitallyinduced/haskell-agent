# Fullscreen streaming Markdown benchmark

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
