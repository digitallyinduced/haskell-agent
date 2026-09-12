# Git output representation benchmark

`GitOutput.hs` compares the original Git output conversion pipeline with strict
`Text` retention. Both pipelines start with the same fully constructed strict
`ByteString` and perform lenient UTF-8 decoding inside the measured interval:

- `original-output`: decode, unpack into a strict `String` field, pack at the
  consumer boundary, and force a character checksum.
- `text-output`: decode into a strict `Text` field and force the same checksum.
- `original-render` and `text-render`: additionally run the production
  `displayTerminalText` sanitizer before computing the checksum.

The capture and consume functions are `NOINLINE` to preserve the producer and
consumer separation across `runSafeGit`'s IO boundary. Input is read from an
`IORef` for every sample so the transformed output cannot be shared across
samples. Each sample performs a major GC before measurement and another after
the timed interval to flush allocation counters. The reported allocation
includes this bookkeeping; elapsed and CPU times exclude the explicit GCs.

Each fixture unit contains an added diff line with Latin text, Japanese text,
a ZWJ emoji, and a tab. A single invalid UTF-8 byte and newline terminate the
complete fixture. The byte count is reported explicitly. Seven samples are
aggregated by median, over three sizes and a repeated intermediate size.
After measurement, the benchmark checks complete old/new `Text` equality both
before and after sanitization, in addition to verifying checksum stability
across samples.

## Reproduction

Run from the repository root:

```sh
mkdir -p "$TMPDIR/git-output-benchmark"
nix develop -c ghc -O2 -Wall -threaded -rtsopts \
  -XGHC2021 -XBlockArguments -XOverloadedStrings -XOverloadedRecordDot \
  -ipackages/agent-tui/src \
  -outputdir "$TMPDIR/git-output-benchmark/objects" \
  packages/agent-cli/benchmark/GitOutput.hs \
  -o "$TMPDIR/git-output-benchmark/benchmark"
nix develop -c sh -c '
  for n in 1000 10000 100000 10000; do
    for operation in original-output text-output original-render text-render; do
      "$1" "$operation" "$n" 7 +RTS -T -N4
    done
  done
' sh "$TMPDIR/git-output-benchmark/benchmark"
```

CSV columns: operation, fixture units, input bytes, samples, median elapsed
milliseconds, median CPU milliseconds, median allocated bytes, checksum.

## Results

Measured 2026-09-12 on macOS ARM64, GHC 9.10.3, `-O2`, `+RTS -T -N4`.
Old and new checksums matched for every corresponding workload.

| Pipeline | Input bytes | Old elapsed ms | Text elapsed ms | Old CPU ms | Text CPU ms | Old allocated bytes | Text allocated bytes |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| Output | 46,002 | 0.421 | 0.157 | 0.430 | 0.162 | 2,555,264 | 47,912 |
| Output + sanitizer | 46,002 | 6.798 | 7.195 | 7.227 | 7.563 | 28,844,936 | 26,337,552 |
| Output | 460,002 | 4.179 | 1.544 | 4.494 | 1.562 | 25,270,816 | 461,912 |
| Output + sanitizer | 460,002 | 72.093 | 68.065 | 76.004 | 71.465 | 288,152,488 | 263,343,552 |
| Output | 4,600,002 | 42.153 | 15.087 | 44.810 | 15.258 | 258,979,520 | 4,601,912 |
| Output + sanitizer | 4,600,002 | 717.581 | 686.012 | 753.581 | 717.236 | 2,887,781,192 | 2,633,403,552 |
| Output (repeat) | 460,002 | 4.074 | 1.617 | 4.324 | 1.625 | 25,270,816 | 461,912 |
| Output + sanitizer (repeat) | 460,002 | 69.313 | 69.069 | 73.195 | 72.556 | 288,152,488 | 263,343,552 |

The output-only pipeline allocates approximately 98% less. Including the
existing sanitizer, the reduction is approximately 9%; the sanitizer remains
the dominant allocation source. Render timings are variable, particularly
for small inputs, so this is not evidence for a consistent rendering speedup.

The initial small rendering case was slower with Text. A 31-sample repeat in
both execution orders did not reproduce that regression:

```sh
nix develop -c sh -c '
  for operation in text-render original-render original-render text-render; do
    "$1" "$operation" 1000 31 +RTS -T -N4
  done
' sh "$TMPDIR/git-output-benchmark/benchmark"
```

| Execution order | Operation | Median elapsed ms | Median CPU ms |
| ---: | --- | ---: | ---: |
| 1 | text-render | 5.826 | 6.186 |
| 2 | original-render | 5.911 | 6.266 |
| 3 | original-render | 6.369 | 6.782 |
| 4 | text-render | 6.191 | 6.501 |

Allocation counts were identical to the corresponding seven-sample results.

## Scope

This measures decoding, representation conversion and downstream consumption,
not just construction of a lazy list head. The rendering variants include the
existing grapheme-based sanitizer, which has its own allocations unchanged by
the Git output migration.

It does not measure Git subprocess execution, pipe reads, concurrent stderr,
path splitting, terminal painting, whole-agent physical footprint, or retained
heap. In particular, allocation reductions must not be described as equivalent
reductions in total process memory. Other content distributions may have
different costs.

An initial fixture put an invalid byte after every 46-byte line. Its unchanged
lenient decoder dominated both pipelines: `text-output` median elapsed time was
2.708 ms at 48 KB and 251.546 ms at 480 KB. This densely malformed case is not
representative of ordinary UTF-8 Git diffs and obscures conversion costs; the
recorded representative results instead use one malformed suffix. This change
does not address the decoder's behavior on densely malformed input.
