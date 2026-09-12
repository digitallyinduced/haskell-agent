# Clipboard subprocess text capture

This benchmark imports the production `readClipboardProcessText` helper and
compares it with `readProcessWithExitCode` followed by `Text.pack`. Both methods
capture, decode, and fully force **both** stdout and stderr. The benchmark also
packs the baseline's short diagnostic stream, so both return the same tuple.
It does not touch the user's clipboard or start any clipboard utility.

## Reproduce

From the repository root:

```sh
mkdir -p "$TMPDIR/clipboard-output-build"
nix develop -c ghc -O2 -threaded -rtsopts -XBlockArguments -Wall \
  -ipackages/agent-cli/src \
  -outputdir "$TMPDIR/clipboard-output-build" \
  packages/agent-cli/benchmark/ClipboardOutput.hs \
  -o "$TMPDIR/clipboard-output-build/clipboard-output"
nix develop -c "$TMPDIR/clipboard-output-build/clipboard-output" \
  suite 21 +RTS -N4 -T -RTS
```

The standalone optimized build avoids compiling the unrelated CLI libraries.
`-N4` matches the CLI's default capability count; the allocation area is the
RTS default. The benchmark sets the Haskell locale encoding to UTF-8 so the
baseline's locale-sensitive decoding is deterministic.

Each sample launches real `/bin/sh` and `/bin/cat` subprocesses over private
fixture files. Stdout is exactly 1 KiB, 64 KiB, 1 MiB, or 8 MiB of ASCII or
mixed 1/2/3/4-byte UTF-8 characters, including CR/LF. Stderr is a short Unicode
diagnostic. Fixture construction/writing and warmup are outside measurement.
The 1 MiB cases repeat at the end; method order alternates within each case.
All warmup and measured outputs are compared exactly against the fixture,
including exit code and both streams, outside the measured interval.

Elapsed time includes process startup, pipe IO, decoding, forcing the complete
strict output, and automatic GC. CPU time and allocated bytes are for the
parent Haskell process, not the fixture subprocesses. A GC before each sample
resets the heap; a GC after the timing interval flushes RTS allocation counters.
The CSV reports independent medians, not a single selected sample.

These are capture-stage measurements, **not end-to-end paste latency**:
clipboard provider delays, sanitization, prompt insertion and rendering are
not included.

## Peak memory (separate fresh processes)

Generate fixtures once, then start a new process for each method and sample.
Do not measure the `suite` process's high-water mark: that would mix fixture
generation, retained expected output, warmups, and both implementations.

```sh
nix develop -c bash -c '
  set -eu
  bin="$1"
  dir=$(mktemp -d "$TMPDIR/clipboard-output-peak.XXXXXX")
  trap '\''rm -rf "$dir"'\'' EXIT
  for kind in ascii unicode; do
    for size in 1048576 8388608; do
      "$bin" fixture "$kind" "$size" "$dir/out" "$dir/err" +RTS -T
      for sample in 1 2 3 4 5 6 7; do
        for method in old new; do
          echo "$kind $size $sample $method"
          /usr/bin/time -l "$bin" peak "$method" "$dir/out" "$dir/err" \
            +RTS -N4 -T -RTS
        done
      done
    done
  done
' _ "$TMPDIR/clipboard-output-build/clipboard-output"
```

The command uses macOS `time -l` (maximum RSS in bytes); on Linux use
`/usr/bin/time -v` (maximum RSS in KiB). `peak` additionally prints RTS
`max_live_bytes` (maximum sampled live heap at major GC) and
`max_mem_in_use_bytes` (maximum RTS-managed memory). These are distinct from
OS RSS and from cumulative allocated bytes. It forces both returned texts and
keeps them live across a major GC, without loading a reference fixture into
the parent. Equality is verified in `suite`; the isolated mode prints lengths.

## Reference measurements

2026-09-12, Apple M3 Max, arm64 macOS 26.6.1, Nix GHC 9.10.3,
`text-2.1.3`, `process-1.6.26.1`, build/RTS settings above. Each cell is
**old → new**, allocation in decimal MB (1,000,000 bytes). Stderr is 36 bytes.
Twenty-one samples per method:

| Stdout | Bytes | Elapsed ms | Parent CPU ms | Allocated MB |
| --- | ---: | ---: | ---: | ---: |
| ASCII | 1,024 | 10.192 → 9.827 | 0.723 → 0.793 | 0.215 → 0.174 |
| ASCII | 65,536 | 10.365 → 10.214 | 1.256 → 0.948 | 3.077 → 0.322 |
| ASCII | 1,048,576 | 42.858 → 13.469 | 35.444 → 3.637 | 46.635 → 2.524 |
| ASCII | 8,388,608 | 328.115 → 36.969 | 324.307 → 26.824 | 371.869 → 19.124 |
| Unicode | 1,024 | 10.685 → 11.369 | 0.735 → 0.765 | 0.198 → 0.179 |
| Unicode | 65,536 | 10.769 → 10.743 | 1.265 → 0.939 | 1.760 → 0.352 |
| Unicode | 1,048,576 | 25.911 → 13.163 | 18.861 → 3.584 | 25.554 → 2.982 |
| Unicode | 8,388,608 | 183.187 → 34.260 | 177.177 → 24.807 | 203.246 → 22.716 |
| ASCII repeat | 1,048,576 | 42.533 → 12.756 | 35.700 → 3.623 | 46.635 → 2.524 |
| Unicode repeat | 1,048,576 | 26.349 → 13.555 | 18.845 → 3.778 | 25.558 → 2.986 |

At 1–8 MiB the helper reduces allocation by approximately 88–95%, parent CPU
by 81–92%, and elapsed time by 49–89% in this subprocess fixture. Both paths
scale approximately linearly in allocation; this removes the large `String`
intermediate, not a quadratic algorithm.

Small payloads are a performance boundary, not a claimed latency win.
The extra scoped-reader coordination costs roughly 0.03–0.07 ms parent CPU at
1 KiB in this run. Their elapsed times are dominated by subprocess startup:
an initial seven-sample run had Unicode 1 KiB at 10.259 → 9.845 ms, while the
21-sample run had 10.685 → 11.369 ms. At 64 KiB the initial run had
12.449 → 13.688 ms ASCII and 10.773 → 11.752 ms Unicode; the longer repeat
above was effectively flat, while CPU and allocation improved in both runs.
Do not interpret these sub-millisecond-to-millisecond wall-time variations
as a reliable small-paste speedup.

Seven independent fresh-process peak-memory samples per method, median
high-water marks in **MiB** (1,048,576 bytes), old → new:

| Stdout | Bytes | OS maximum RSS | RTS maximum live heap | RTS maximum memory in use |
| --- | ---: | ---: | ---: | ---: |
| ASCII | 1,048,576 | 64.609 → 34.672 | 16.781 → 1.106 | 53 → 23 |
| ASCII | 8,388,608 | 338.609 → 60.734 | 149.618 → 8.101 | 327 → 49 |
| Unicode | 1,048,576 | 54.594 → 36.703 | 8.856 → 1.106 | 43 → 25 |
| Unicode | 8,388,608 | 213.594 → 58.703 | 89.813 → 8.106 | 202 → 47 |

The output remains proportional to clipboard size: this is not a bounded-memory
streaming API. Its improvement is avoiding the transient linked-list character
representation while retaining the same decoded text.
