# JSON preview scanner benchmark

This comparison changes only `Agent.JsonText` between builds. Both executables
run `JsonDecoding.hs` through `newStreamEventToLoopEvents`, including bounded
argument accumulation, partial JSON decoding, preview comparison and JSON
encoding of emitted tool arguments. It does not measure network input, terminal
rendering, idle memory, or retained conversation memory.

The existing `tool-shell-baseline` mode compares an older batching design and
must **not** be used as the baseline for this scanner change. Run `tool-shell`
on both builds instead.

Correctness validation of the measured candidate passed 12 focused tests and
matched the original public partial-JSON decoder exactly on 112,347 generated
cases. Its `Agent.JsonText` source SHA-256 is
`8f90b98675d02b92cb91d129239b5b2d691f31765c10ff189579f11d241ec3db`.

## Workloads and measurement

- `tool-shell`: ASCII `x` characters.
- `tool-shell-escaped`: repeating decoded quote, backslash and tab characters.
- `tool-shell-unicode`: repeating `café世界🙂`.

The body size counts decoded Unicode characters. JSON encoding happens outside
measurement; escaped bodies have twice as many encoded body characters.
Delta size counts encoded Unicode characters, not UTF-8 bytes. Every stream
includes an added call, argument deltas, and a final complete-arguments event.
Normal production preview limits and batching remain active.

The harness creates a fresh projector for each repetition, consumes emitted
events and strict textual payloads into a checksum, performs GC before each
sample, and flushes the nursery after timing for allocated-byte accounting.
Reported wall time, CPU time and allocated bytes are independent medians.
The harness's maximum-live-byte column is process-wide and is not an isolated
retained-heap measurement.

## Reproduction

Run from the repository root. The immutable baseline module comes from
`10913d1374bd09ed2972f3237859223885b99fcb`.

```sh
directory="$TMPDIR/json-preview-benchmark"
mkdir -p "$directory/baseline/Agent"
git show 10913d1374bd09ed2972f3237859223885b99fcb:packages/agent-core/src/Agent/JsonText.hs \
  > "$directory/baseline/Agent/JsonText.hs"

for implementation in old new; do
  if [ "$implementation" = old ]; then
    source_directory="$directory/baseline"
  else
    source_directory=packages/agent-core/src
  fi
  nix develop -c ghc -O2 -Wall -rtsopts \
    -XGHC2021 -XDerivingStrategies -XOverloadedStrings \
    -XOverloadedRecordDot -XDuplicateRecordFields -XNoFieldSelectors \
    -XLambdaCase -XBlockArguments -XRecordWildCards \
    -XTypeApplications -XPackageImports \
    -i"$source_directory" \
    $(find packages -maxdepth 3 -path '*/src' -type d | sed 's/^/-i/') \
    -outputdir "$directory/$implementation-objects" \
    packages/agent-responses/benchmark/JsonDecoding.hs \
    -o "$directory/$implementation"
done
```

Use separate object directories so the compiler does not reuse the other
implementation of `Agent.JsonText`.

```sh
nix develop -c sh -c '
  for size in 16 128 1024 4096 16384 1024; do
    for mode in tool-shell tool-shell-escaped tool-shell-unicode; do
      for implementation in old new; do
        printf "%s," "$implementation"
        "$1/$implementation" "$mode" "$size" 16 100 7 +RTS -T
      done
    done
  done
' sh "$directory"
```

The tiny-input confirmation uses:

```sh
nix develop -c sh -c '
  for implementation in old new new old; do
    printf "%s," "$implementation"
    "$1/$implementation" partial-field-escaped 16 10000 7 +RTS -T
  done
' sh "$directory"
```

The final 1,024-character run repeats the representative middle size.
The production shell preview retains at most 4,096 encoded characters,
publishes eagerly through 128 characters and then in 64-character increments.
Consequently the larger-input measurements include processing discarded deltas,
not unlimited rescanning of a growing command.

## Results

Measured 2026-09-12, Apple M3 Max, macOS 26.6.1, GHC 9.10.3,
`-O2`, single-capability runtime with `+RTS -T`. Candidate module SHA-256:
`8f90b98675d02b92cb91d129239b5b2d691f31765c10ff189579f11d241ec3db`.
Each row covers 100 complete stream projections, seven samples.
All paired output checksums matched.

| Body | Characters | Old wall/CPU ms | Text wall/CPU ms | Old allocated B | Text allocated B |
| --- | ---: | ---: | ---: | ---: | ---: |
| ASCII | 16 | 0.548/0.549 | 0.579/0.579 | 3856448 | 3798848 |
| Escaped | 16 | 0.972/0.973 | 0.968/0.969 | 5949168 | 5729968 |
| Unicode | 16 | 0.576/0.576 | 0.551/0.552 | 3868904 | 3811304 |
| ASCII | 128 | 3.525/3.526 | 3.067/3.068 | 22603832 | 16709432 |
| Escaped | 128 | 5.721/5.723 | 5.233/5.234 | 33361488 | 25098288 |
| Unicode | 128 | 4.010/4.006 | 3.407/3.407 | 23397376 | 17365376 |
| ASCII | 1024 | 21.806/21.785 | 12.516/12.511 | 170206080 | 62001280 |
| Escaped | 1024 | 87.672/87.228 | 66.905/66.904 | 542019184 | 232027184 |
| Unicode | 1024 | 28.382/28.315 | 18.838/18.800 | 184351152 | 73114352 |
| ASCII | 4096 | 223.746/223.586 | 91.981/91.906 | 1995071184 | 317903184 |
| Escaped | 4096 | 327.258/327.051 | 256.715/256.140 | 2046971984 | 789507984 |
| Unicode | 4096 | 321.555/320.812 | 165.049/164.744 | 2344164720 | 618817520 |
| ASCII | 16384 | 247.089/246.602 | 115.283/115.219 | 2172719184 | 495551184 |
| Escaped | 16384 | 391.725/390.468 | 302.068/301.423 | 2402224784 | 1144760784 |
| Unicode | 16384 | 360.887/360.097 | 198.159/197.659 | 2521812720 | 796465520 |
| ASCII, repeat | 1024 | 22.404/22.416 | 13.347/13.357 | 170206080 | 62001280 |
| Escaped, repeat | 1024 | 91.924/91.855 | 69.845/69.785 | 542019184 | 232027184 |
| Unicode, repeat | 1024 | 27.753/27.753 | 18.884/18.864 | 184351152 | 73114352 |

At 1,024 decoded characters, allocation falls 57–64% across the three
workloads and elapsed time falls 24–43%; the repeated measurements preserve
that direction. These are temporary allocation savings, not equivalent
reductions in physical footprint. The complete-JSON decoder path is unchanged.

### Small-input boundary

The 16-character samples were too short for a reliable speed claim. Repeating
them with 10,000 projections per sample and reversing executable order showed
no consistent elapsed-time regression or improvement (approximately ±2%):

| Body | Order | Old wall/CPU ms | Text wall/CPU ms | Old allocated B | Text allocated B |
| --- | --- | ---: | ---: | ---: | ---: |
| ASCII | old, new | 69.082/68.971 | 67.623/67.536 | 385349232 | 379589232 |
| Escaped | old, new | 110.276/110.170 | 111.896/111.799 | 594682368 | 572762368 |
| Unicode | old, new | 72.714/72.666 | 74.159/73.988 | 386777144 | 381017144 |
| ASCII | new, old | 71.303/71.200 | 71.295/71.264 | 385349232 | 379589232 |
| Escaped | new, old | 108.160/107.608 | 107.223/106.717 | 594682368 | 572762368 |
| Unicode | new, old | 70.069/70.035 | 68.620/68.542 | 386777144 | 381017144 |

```sh
nix develop -c sh -c '
  for order in "old new" "new old"; do
    for mode in tool-shell tool-shell-escaped tool-shell-unicode; do
      for implementation in $order; do
        printf "%s," "$implementation"
        "$1/$implementation" "$mode" 16 16 10000 7 +RTS -T
      done
    done
  done
' sh "$directory"
```

Allocation still falls slightly at this size, but fixed stream bookkeeping and
JSON decoding dominate. Do not advertise a small-command speedup.

## Uncapped public-helper boundary

`partial-field`, `partial-field-escaped` and `partial-field-unicode` additionally
exercise the entire public `jsonTextFieldPartial` function without the shell
projector's 4,096-character cap. They use the same decoded body patterns, with
the closing JSON quote/object omitted. Each fixture is checked against its
expected decoded body before timing. Each timed repetition reads its input
through an IORef and folds every returned character into the checksum, avoiding
pure-result sharing. This includes the failed complete-object decode and the
fallback field scan, incomplete-escape handling, UTF-8 encoding and final string
decode. It is not a full TUI renderer or diff-layout benchmark.

Same machine/build/runtime, 100 helper calls per sample, seven samples:

| Body | Characters | Old wall/CPU ms | Text wall/CPU ms | Old allocated B | Text allocated B |
| --- | ---: | ---: | ---: | ---: | ---: |
| ASCII | 16 | 0.218/0.219 | 0.194/0.201 | 1110224 | 908624 |
| Escaped | 16 | 0.264/0.265 | 0.275/0.276 | 1313552 | 1026352 |
| Unicode | 16 | 0.229/0.230 | 0.202/0.202 | 1123152 | 921552 |
| ASCII | 1024 | 1.725/1.721 | 0.576/0.576 | 14415472 | 1713072 |
| Escaped | 1024 | 4.824/4.766 | 3.311/3.270 | 27831960 | 8719160 |
| Unicode | 1024 | 2.155/2.155 | 0.861/0.862 | 15761560 | 2647960 |
| ASCII | 16384 | 20.963/20.913 | 6.005/6.006 | 217176144 | 14003344 |
| Escaped | 16384 | 66.017/65.504 | 46.926/45.493 | 431808144 | 125968944 |
| Unicode | 16384 | 29.031/27.695 | 10.570/10.458 | 238708944 | 28980944 |
| ASCII | 65536 | 84.969/84.384 | 23.073/23.053 | 865985744 | 53324944 |
| Escaped | 65536 | 271.490/269.647 | 178.241/177.878 | 1724508944 | 501162544 |
| Unicode | 65536 | 111.623/111.266 | 40.531/40.463 | 952118544 | 113241744 |
| ASCII, repeat | 1024 | 1.554/1.554 | 0.580/0.580 | 14415472 | 1713072 |
| Escaped, repeat | 1024 | 4.259/4.264 | 3.361/3.352 | 27831960 | 8719160 |
| Unicode, repeat | 1024 | 2.049/2.047 | 0.822/0.823 | 15761560 | 2647960 |

The initial 16-character escaped sample was too short to establish its apparent
4% slowdown. Repeating that case with 10,000 calls per sample, seven samples,
and reversed executable order gave:

| Order | Old wall/CPU ms | Text wall/CPU ms | Old allocated B | Text allocated B |
| --- | ---: | ---: | ---: | ---: |
| old, new | 32.729/32.707 | 31.288/31.154 | 131108256 | 102388256 |
| new, old | 32.121/31.959 | 30.600/30.566 | 131108256 | 102388256 |

This longer tiny-input check shows 4–5% lower elapsed time and 22% lower
allocation, not a reproducible regression. All paired checksums matched.

Rebuild both executables using the commands above, then:

```sh
nix develop -c sh -c '
  for size in 16 1024 16384 65536 1024; do
    for mode in partial-field partial-field-escaped partial-field-unicode; do
      for implementation in old new; do
        printf "%s," "$implementation"
        "$1/$implementation" "$mode" "$size" 100 7 +RTS -T || exit
      done
    done
  done
' sh "$directory"
```
