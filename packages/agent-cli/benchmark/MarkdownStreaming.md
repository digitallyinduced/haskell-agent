# Streaming Markdown benchmark

The retained `MarkdownStreamBaseline.hs` implements the strict append and
full-buffer rescan behavior from
`89102f95873de78ccf23142c847105bce9ac4777`. The benchmark compares it with the
production `Agent.CLI.Render.MarkdownStream`, compiling both and their shared
ANSI Markdown renderer with `-O2` in the same component.

## Reproduce

From the repository root:

```sh
nix develop
cabal build --offline agent-cli:bench:markdown-streaming-bench
bin=$(cabal list-bin agent-cli:bench:markdown-streaming-bench)

for count in 100 500 1000 2000; do
  for scenario in fence table longline prose; do
    for mode in old new; do
      "$bin" "$mode" "$scenario" "$count" 16 7 +RTS -T
    done
  done
done

for chunk in 1 16 64 256 65536; do
  for scenario in fence table mixed; do
    for mode in old new; do
      "$bin" "$mode" "$scenario" 100 "$chunk" 7 +RTS -T
    done
  done
done

# Repeat representative cases in the opposite old/new order. Include blocks
# that must be rendered during final flush, not during a feed operation.
for scenario in fence table open-fence open-table prose mixed; do
  for mode in new old; do
    "$bin" "$mode" "$scenario" 1000 16 7 +RTS -T
  done
done
```

Arguments are implementation, scenario, body count, chunk characters, and
sample count. CSV columns are those five arguments followed by median wall
milliseconds, CPU milliseconds, and allocated bytes per complete stream.

Fixtures are ASCII, so chunk characters equal UTF-8 bytes. Fences contain
58-byte generated-code lines; tables contain two columns, inline formatting,
and 28-byte body rows (including newlines); prose contains emphasis, inline code,
and a link.
`longline` measures one fenced line containing COUNT 16-character units.
`mixed` measures COUNT groups, each containing one prose line, a five-line
fence, and a five-row table. Width is 100 terminal cells.

All input text and the chunk list are forced before timing. Every feed output
and the final flush output are consumed by a character checksum. The benchmark
therefore includes final concatenation, Markdown parsing, table layout, and
ANSI generation; it does not merely move that work outside measurement.
An IORef read and sample-specific inputs prevent sharing a previously evaluated
result. Each sample starts after a GC. Allocation is read after another GC,
outside the timing interval, to include the unfinished nursery. Exact output
equality is checked before measurement, **including output for each delta**,
so deferring output to a later feed cannot masquerade as an improvement.

These are scrollback renderer CPU/allocation measurements, not model throughput
or terminal I/O timings. The optimization does not change fullscreen rendering.

## Results (2026-09-09)

Intel Core i9-9900K, x86_64 Linux, GHC 9.10.3, inside `nix develop`.
The benchmark component and its local modules use `-O2`; the unchanged
`agent-tui` dependency uses Cabal's `-O1` default. Single-capability,
non-threaded RTS with `+RTS -T`, default allocation area. Seven samples per
entry. MB below means decimal allocated bytes / 1,000,000.

At 16-byte deltas, including final rendering:

| Workload | Count | Wall ms old → new | CPU ms old → new | Allocated MB old → new |
|---|---:|---:|---:|---:|
| Fence | 100 | 3.325 → 0.938 | 3.311 → 0.942 | 17.619 → 5.546 |
| Fence | 500 | 65.344 → 4.899 | 65.253 → 4.889 | 329.343 → 27.556 |
| Fence | 1,000 | 268.066 → 11.646 | 267.170 → 11.365 | 1,261.867 → 55.063 |
| Fence | 2,000 | 1,056.379 → 24.288 | 1,054.181 → 24.238 | 4,937.944 → 110.087 |
| Table | 100 | 6.033 → 1.945 | 6.021 → 1.940 | 45.492 → 11.685 |
| Table | 500 | 112.830 → 9.812 | 112.671 → 9.795 | 910.850 → 57.922 |
| Table | 1,000 | 448.619 → 20.337 | 447.802 → 20.302 | 3,531.322 → 115.718 |
| Table | 2,000 | 1,855.535 → 42.312 | 1,852.606 → 42.236 | 13,901.517 → 231.311 |
| Long fenced line | 500 | 3.401 → 1.088 | 3.395 → 1.086 | 9.226 → 7.114 |
| Long fenced line | 1,000 | 12.165 → 2.367 | 12.142 → 2.364 | 22.434 → 14.210 |
| Long fenced line | 2,000 | 41.191 → 5.062 | 41.111 → 5.050 | 60.850 → 28.402 |
| Prose | 500 | 18.074 → 18.135 | 18.036 → 18.090 | 104.228 → 104.227 |
| Prose | 1,000 | 35.559 → 35.469 | 35.476 → 35.373 | 208.445 → 208.444 |
| Prose | 2,000 | 72.130 → 73.051 | 71.941 → 72.866 | 416.853 → 416.853 |
| Mixed groups | 100 | 24.487 → 22.903 | 24.422 → 22.826 | 128.694 → 117.998 |
| Mixed groups | 1,000 | 234.472 → 214.943 | 233.875 → 214.113 | 1,286.778 → 1,179.851 |
| Open fence (flush) | 1,000 | 258.230 → 11.211 | 257.870 → 11.190 | 1,261.541 → 54.908 |
| Open table (flush) | 1,000 | 448.060 → 20.298 | 447.455 → 20.267 | 3,534.875 → 115.448 |

The 1,000-row fence reduces CPU by 23.5× and allocation by 95.6%; the table
reduces CPU by 22.1× and allocation by 96.7%. Doubling from 1,000 to 2,000
rows approximately quadruples baseline work/allocation but doubles the new
implementation. Ordinary prose is unchanged (timing varies by about 1%).
Small-block mixed output benefits more modestly: about 8% less CPU/allocation.

Repeating the 1,000-row cases with reversed process order produced fence CPU
261.196 → 11.301 ms and table CPU 448.681 → 20.270 ms; allocation was identical
to the first run. Prose repeated at 35.470 → 35.712 ms with unchanged allocation.

Chunk-size boundary checks, COUNT=100:

| Workload | Delta bytes | Wall ms old → new | CPU ms old → new | Allocated MB old → new |
|---|---:|---:|---:|---:|
| Fence | 1 | 44.224 → 1.345 | 43.913 → 1.324 | 200.527 → 7.618 |
| Fence | 64 | 1.466 → 0.973 | 1.461 → 0.972 | 8.441 → 5.442 |
| Fence | 256 | 1.053 → 0.890 | 1.051 → 0.883 | 6.158 → 5.411 |
| Table | 1 | 68.124 → 2.091 | 68.013 → 2.086 | 556.350 → 12.649 |
| Table | 64 | 3.048 → 1.948 | 3.045 → 1.943 | 20.236 → 11.636 |
| Table | 256 | 2.177 → 1.849 | 2.169 → 1.844 | 13.928 → 11.624 |
| Table | 65,536 | 1.979 → 1.871 | 1.977 → 1.865 | 11.671 → 11.620 |
| Mixed groups | 1 | 55.765 → 28.131 | 55.605 → 28.067 | 330.377 → 163.040 |
| Mixed groups | 64 | 21.911 → 20.933 | 21.818 → 20.868 | 118.268 → 115.244 |
| Mixed groups | 256 | 20.996 → 20.545 | 20.939 → 20.496 | 115.356 → 113.814 |
| Mixed groups | 65,536 | 51.116 → 27.645 | 50.977 → 27.527 | 273.537 → 150.205 |

Large deltas containing many small blocks also benefit because finishing a
fence/table no longer splits and concatenates all following blocks merely to
recover the unconsumed remainder.

### Single-shot fence boundary

A complete fence arriving in one delta has no repeated-scan cost to remove.
The initial 100-row / 65,536-byte-delta run was 0.879 → 0.919 ms CPU
(5.410 → 5.403 MB), so this boundary was repeated with 31 samples per process,
three independent runs, at both 100 and 1,000 rows:

```sh
for count in 100 1000; do
  for repeat in 1 2 3; do
    for mode in new old; do
      "$bin" "$mode" fence "$count" 65536 31 +RTS -T
    done
  done
done
```

| Count | Repeat | Wall ms old → new | CPU ms old → new | Allocated MB old → new |
|---:|---:|---:|---:|---:|
| 100 | 1 | 0.885 → 0.899 | 0.883 → 0.898 | 5.411 → 5.404 |
| 100 | 2 | 0.882 → 0.895 | 0.880 → 0.893 | 5.411 → 5.404 |
| 100 | 3 | 0.873 → 0.869 | 0.871 → 0.868 | 5.411 → 5.404 |
| 1,000 | 1 | 11.556 → 11.553 | 11.537 → 11.534 | 53.746 → 53.686 |
| 1,000 | 2 | 11.710 → 11.595 | 11.689 → 11.570 | 53.746 → 53.686 |
| 1,000 | 3 | 11.501 → 11.626 | 11.478 → 11.605 | 53.746 → 53.686 |

No speedup is claimed for this boundary: timings overlap and allocation is
effectively unchanged. The substantial gains require a block to span multiple
deltas, or a delta to contain multiple blocks.

### Tiny blocks

One- and five-row blocks were also checked at 16- and 256-byte deltas with
101 samples per process (same command, COUNT=1 or 5, CHUNK_CHARS=16 or 256,
SAMPLES=101). Allocation decreases in every case; larger deltas leave timings
approximately neutral at these sizes.

| Workload | Rows | Delta bytes | Wall ms old → new | CPU ms old → new | Allocated bytes old → new |
|---|---:|---:|---:|---:|---:|
| Fence | 1 | 16 | 0.0224 → 0.0216 | 0.0228 → 0.0220 | 97,352 → 95,200 |
| Fence | 1 | 256 | 0.0210 → 0.0207 | 0.0213 → 0.0211 | 94,120 → 92,520 |
| Fence | 5 | 16 | 0.0585 → 0.0525 | 0.0588 → 0.0529 | 342,712 → 314,000 |
| Fence | 5 | 256 | 0.0518 → 0.0517 | 0.0522 → 0.0521 | 312,440 → 307,720 |
| Table | 1 | 16 | 0.0620 → 0.0616 | 0.0625 → 0.0621 | 246,200 → 243,216 |
| Table | 1 | 256 | 0.0608 → 0.0595 | 0.0613 → 0.0600 | 244,680 → 241,288 |
| Table | 5 | 16 | 0.1447 → 0.1376 | 0.1452 → 0.1381 | 778,952 → 705,584 |
| Table | 5 | 256 | 0.1372 → 0.1362 | 0.1377 → 0.1366 | 706,376 → 701,080 |

Repeating the five-row / 256-byte cases in reverse order produced fence CPU
0.0531 → 0.0514 ms and table CPU 0.1343 → 0.1364 ms, with identical allocation
to the first run. These sub-millisecond differences should not be interpreted
as a reliable speedup or regression.
