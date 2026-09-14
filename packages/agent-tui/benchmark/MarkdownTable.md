# Markdown table parser benchmark

`MarkdownTable.hs` measures isolated row splitting and complete cold Markdown
rendering. Preserve the exact baseline using its recorded Git object, not a
second implementation rewritten to resemble the old algorithm.

## Reproduce

From the repository root, enter `nix develop`, then:

```sh
root="$TMPDIR/table-bench"
mkdir -p "$root/old/Agent/TUI/Markdown" "$root/old-obj" "$root/new-obj"
git show 434b6bcbc71afebc9636b9156635c41c2bc32f89:packages/agent-tui/src/Agent/TUI/Markdown/Block.hs \
  > "$root/old/Agent/TUI/Markdown/Block.hs"
for version in old new; do
  ghc -O2 -rtsopts -XGHC2021 -XOverloadedStrings \
    -XLambdaCase -XBlockArguments -XDerivingStrategies -XRecordWildCards \
    -XDuplicateRecordFields -XOverloadedRecordDot -XNoFieldSelectors \
    -i"$root/$version" -ipackages/agent-tui/src \
    -ipackages/agent-syntax/src -ipackages/agent-core/src \
    -ipackages/agent-json/src -ipackages/agent-responses-types/src -ipackages/agent-tools/src \
    -outputdir "$root/$version-obj" \
    packages/agent-tui/benchmark/MarkdownTable.hs -o "$root/$version-bin"
done
"$root/old-bin" split mixed 100 1000 7 +RTS -T
"$root/new-bin" split mixed 100 1000 7 +RTS -T
"$root/old-bin" render mixed 100 20 7 +RTS -T
"$root/new-bin" render mixed 100 20 7 +RTS -T
```

Only `Block.hs` is overridden; all rendering and support code is otherwise
identical and compiled with `-O2`. No production source is overwritten.
Arguments: stage, kind, size, repetitions per sample, samples.
CSV columns: stage, kind, size, repetitions, samples, median elapsed ms,
median CPU ms, median allocated bytes, checksum.

`split` grows a row by repeating a cell fragment `size` times; `render`
grows a complete two-column table to `size` body rows, with a heading and
following Markdown paragraph. Kinds include ordinary ASCII, inline formatting
and different-width backtick spans, Unicode including combining and astral
characters, and escaped pipes/backslashes. The escaped workload deliberately
includes an unescaped pipe after a paired backslash.
The `prose` kind is a non-table rendering control: a heading followed by
`size` paragraphs containing ordinary words, bold, code, and Japanese text.

Each sample uses unique, pre-forced input, an IORef read in the timed region,
and a `NOINLINE` consumer. Splitting forces every resulting cell character;
rendering parses the entire message, performs Brick layout/image composition
in an 80×30 tail-following viewport, and consumes actual Vty span text,
attributes, and click extents. It does not merely force widget WHNF.
Syntax highlighting and caches are disabled. Rendering is completed-message
rendering, not streaming cache performance.

GC before timing is excluded. GC after timing accounts for the unfinished
nursery in allocation totals. Default single-threaded RTS/allocation area.
Compare checksums between the separately compiled binaries as a workload
sanity check; the correctness suite provides stronger semantic checks.

Peak RSS must be collected from separate processes (on macOS,
`/usr/bin/time -l "$root/new-bin" render mixed 1000 20 1 +RTS -T`;
on Linux use `/usr/bin/time -v`). Repeat each version and take the median.
RSS includes the executable, runtime, inputs, and rendering; it is not an
allocation count or a GC live-heap estimate.

## Results

Apple M3 Max, macOS, Nix GHC 9.10.3, settings above. Seven samples per
invocation; allocations below are decimal MB. All old/new checksums matched.
`split` uses 100 repetitions; `render` uses 20. Sizes 10/100/1000 were
run for every kind, sequentially, with old/new adjacent.

| Stage/kind | Size | Elapsed old → new (ms) | CPU old → new (ms) | Allocated old → new (MB) |
| --- | ---: | --- | --- | --- |
| split/plain | 10 | 0.196 → 0.046 | 0.196 → 0.046 | 2.094 → 0.125 |
| split/plain | 100 | 1.977 → 0.368 | 1.978 → 0.367 | 18.657 → 0.260 |
| split/plain | 1000 | 22.847 → 3.591 | 22.826 → 3.591 | 183.529 → 1.610 |
| split/mixed | 10 | 1.632 → 0.355 | 1.619 → 0.356 | 5.478 → 2.594 |
| split/mixed | 100 | 128.145 → 3.592 | 128.008 → 3.490 | 126.556 → 24.896 |
| split/mixed | 1000 | 8946.497 → 45.269 | 8927.939 → 45.172 | 8264.894 → 247.916 |
| split/unicode | 10 | 0.220 → 0.066 | 0.221 → 0.067 | 2.027 → 0.135 |
| split/unicode | 100 | 2.186 → 0.438 | 2.200 → 0.438 | 17.868 → 0.360 |
| split/unicode | 1000 | 24.878 → 4.491 | 24.774 → 4.491 | 174.808 → 2.610 |
| split/escaped | 10 | 0.718 → 0.306 | 0.718 → 0.305 | 3.523 → 2.820 |
| split/escaped | 100 | 39.419 → 3.450 | 39.259 → 3.449 | 56.169 → 27.156 |
| split/escaped | 1000 | 3128.399 → 44.845 | 3123.387 → 44.774 | 2886.478 → 270.516 |
| render/plain | 10 | 11.734 → 12.160 | 11.702 → 12.156 | 77.494 → 76.384 |
| render/plain | 100 | 100.091 → 97.347 | 100.014 → 97.291 | 487.183 → 476.842 |
| render/plain | 1000 | 1057.269 → 1052.240 | 1056.299 → 1051.277 | 4577.598 → 4474.955 |
| render/mixed | 10 | 17.233 → 16.934 | 17.223 → 16.918 | 106.131 → 104.908 |
| render/mixed | 100 | 149.724 → 147.213 | 149.616 → 146.849 | 687.726 → 676.265 |
| render/mixed | 1000 | 1509.193 → 1491.047 | 1503.759 → 1488.999 | 6480.729 → 6366.886 |
| render/unicode | 10 | 12.313 → 12.610 | 12.068 → 12.454 | 75.866 → 74.801 |
| render/unicode | 100 | 101.756 → 101.458 | 101.670 → 101.369 | 470.885 → 460.992 |
| render/unicode | 1000 | 1074.639 → 1056.525 | 1071.066 → 1051.105 | 4414.600 → 4316.437 |
| render/escaped | 10 | 10.593 → 10.565 | 10.600 → 10.546 | 68.615 → 67.987 |
| render/escaped | 100 | 82.151 → 82.585 | 82.128 → 82.529 | 398.438 → 392.929 |
| render/escaped | 1000 | 918.848 → 920.093 | 917.599 → 918.314 | 3690.139 → 3635.816 |

The large isolated speedups are **not** whole-render speedups. Ordinary
short-cell tables spend most of their time in the unchanged inline renderer
and Brick layout. Their allocation reductions are only about 1–2%, and
timing differences are small enough to require repeat measurements.

### Stability and apparent regressions

Short isolated rows were also checked at size 1, 10,000 repetitions and seven
samples (old/new adjacent). Checksums matched; there was no short-row slowdown.

| Split kind | Elapsed old → new (ms) | CPU old → new (ms) | Allocated old → new (MB) |
| --- | --- | --- | --- |
| plain | 4.220 → 1.601 | 4.215 → 1.601 | 45.521 → 11.041 |
| mixed | 9.276 → 4.463 | 9.281 → 4.470 | 72.481 → 36.241 |
| unicode | 4.385 → 1.941 | 4.381 → 1.937 | 44.321 → 11.121 |
| escaped | 6.474 → 4.684 | 6.469 → 4.677 | 58.401 → 38.561 |

The initial short plain/Unicode renders appeared 2–4% slower. Retesting all
four kinds at 10/100 rows with 100 repetitions, seven samples, and three fresh
invocations per version (new then old, adjacent) did not reproduce that
regression. The table is the median of those three invocation medians.

| Render kind | Rows | Elapsed old → new (ms) | CPU old → new (ms) | Allocated old → new (MB) |
| --- | ---: | --- | --- | --- |
| plain | 10 | 59.632 → 58.455 | 59.290 → 58.098 | 387.467 → 381.914 |
| plain | 100 | 478.589 → 476.835 | 477.620 → 474.790 | 2435.911 → 2384.206 |
| unicode | 10 | 61.442 → 61.414 | 61.353 → 61.385 | 379.327 → 373.998 |
| unicode | 100 | 505.852 → 504.824 | 505.199 → 503.995 | 2354.420 → 2304.956 |
| escaped | 10 | 53.141 → 52.278 | 52.978 → 52.230 | 343.068 → 339.932 |
| escaped | 100 | 412.313 → 411.379 | 411.296 → 410.505 | 1992.187 → 1964.642 |
| mixed | 10 | 84.018 → 83.576 | 83.902 → 83.388 | 530.648 → 524.536 |
| mixed | 100 | 732.434 → 723.863 | 730.718 → 722.556 | 3438.626 → 3381.321 |

No material repeatable full-render regression was observed. Do not claim a
meaningful full-render speedup for Unicode or escaped short-cell tables:
these measurements are effectively flat. The supported benefit is faster,
lower-allocation splitting, particularly eliminating repeated suffix conversion
to Text during backtick lookahead, without moving cost into rendering. Suffix
searches remain; this is not a worst-case linear-time parser.

### Non-table control

`render prose SIZE 20 7 +RTS -T`, three fresh invocations per version,
old/new adjacent. Medians of invocation medians; all checksums matched.

| Paragraphs | Elapsed old → new (ms) | CPU old → new (ms) | Allocated old → new (MB) |
| ---: | --- | --- | --- |
| 10 | 7.808 → 7.784 | 7.801 → 7.743 | 54.980 → 53.957 |
| 100 | 73.625 → 71.514 | 73.617 → 71.463 | 394.658 → 384.621 |
| 1000 | 758.593 → 751.993 | 757.602 → 751.232 | 3767.561 → 3667.380 |

No repeatable regression: the first 100-paragraph pair was 68.238 → 71.257
ms, but the next two were 73.934 → 71.514 and 73.625 → 71.771 ms.
Treat small timing differences conservatively, not as a reliable prose speedup.

### Peak RSS

Five fresh processes per version, alternating old/new, running
`render mixed 1000 20 1 +RTS -T` under macOS `/usr/bin/time -l`:
median peak RSS **63,422,464 → 62,390,272 bytes** (60.48 → 59.50 MiB).
Old range 63,422,464–63,455,232 bytes; all five new values were 62,390,272.
This modest whole-process change is consistent with the small full-render
allocation reduction, not the much larger isolated long-row improvement.

For isolated splitting, three fresh processes per version, alternating
old/new, running `split mixed 1000 100 1 +RTS -T` gave median peak RSS
**35,586,048 → 28,262,400 bytes** (33.94 → 26.95 MiB). All old values
were 35,586,048; new range 28,262,400–28,295,168 bytes.

## Validation and limitations

- Block, inline, and Markdown renderer suites: 92 examples, zero failures.
- Differential comparison with the original parser: 166,501 generated rows,
  zero mismatches, including 3,000 longer rows.
- Direct source CLI Markdown rendering in tmux passed at widths 100 and 24,
  including escaped pipes, code spans, Unicode, alignment, and wrapping.
- Full-agent smoke testing was blocked by the existing GStreamer/pkg-config
  dependency failure. The source renderer was tested directly instead.
- Pathological unmatched-backtick performance was not measured separately.
- This change reduces temporary parsing overhead; it does not establish a
  reduction in idle agent memory or retained conversation size.
