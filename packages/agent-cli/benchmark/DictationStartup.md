# Dictation startup benchmark

Run from the repository root inside the Nix development shell:

```sh
out=$(mktemp -d "$TMPDIR/dictation-bench.XXXXXX")
ghc -O2 -threaded -XGHC2021 -XBlockArguments -XLambdaCase \
  -ipackages/agent-cli/src -outputdir "$out" \
  packages/agent-cli/benchmark/DictationStartup.hs -o "$out/bench"
"$out/bench"
```

This isolates serial provider-then-capture startup versus `withBufferedCapture`.
Fixtures use a simulated 20 ms microphone startup, 200/1000 ms provider setup,
and 10/100 chunks of 4800 PCM bytes. Each result is the median of five runs;
all runs verify the complete byte count. No microphone or network is used.

GHC 9.10.3, optimized macOS run:

| Setup | Chunks | Serial ready | Buffered ready | Serial total | Buffered total |
| --- | --- | --- | --- | --- | --- |
| 200 ms | 10 | 222.09 ms | 21.07 ms | 222.09 ms | 201.16 ms |
| 200 ms | 100 | 221.95 ms | 20.80 ms | 221.96 ms | 201.56 ms |
| 1000 ms | 10 | 1022.37 ms | 20.99 ms | 1022.37 ms | 1001.32 ms |
| 1000 ms | 100 | 1022.35 ms | 20.99 ms | 1022.35 ms | 1001.51 ms |

Repeating the 200 ms case at the end gave 221.58–222.11 ms serial readiness
versus 20.82–21.05 ms buffered readiness.

These are scheduling measurements, not end-to-end transcription timings.
A separate local FFmpeg AVFoundation probe delivered first PCM at 729 ms.
Adding `-probesize 32 -analyzeduration 1` yielded 835 ms, so those flags were
not changed. Device startup remains; provider setup no longer precedes it.
