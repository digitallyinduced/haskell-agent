# Real session loading benchmark

This benchmark measures canonical PostgreSQL session loading against an
existing coding session. It reports only the session key, a checksum, timing,
and Haskell allocation; it does not print transcript contents.

```console
nix develop -c cabal build --offline \
  agent-store:bench:real-session-load-bench
bin=$(nix develop -c cabal list-bin \
  agent-store:bench:real-session-load-bench)
"$bin" per-item "$HOME/.haskell-agent" active SESSION_KEY 11 +RTS -T
"$bin" adaptive "$HOME/.haskell-agent" active SESSION_KEY 11 +RTS -T
"$bin" adaptive "$HOME/.haskell-agent" active-prompt SESSION_KEY 11 +RTS -T
```

Use `active` to measure the inference context loaded when resuming a session,
starting at its latest replacement/reset checkpoint. Use `active-prompt` to
include the bounded latest-prompt-epoch lookup performed by a cache-stable
resume, while retaining `active` as its same-binary baseline. Use `full` to
load every persisted turn. The `per-item` implementation is the executable
baseline that issues the original child-row point reads; `adaptive` selects
the production implementation.

Inputs are read from the local managed PostgreSQL store. The store must already
contain the selected session. The first load is discarded as a warm-up; the
reported sample is the median of the requested measured loads. The checksum
forces every typed response-item field.

## 2026-08-27 results

Apple M3 Max, 36 GiB RAM, GHC 9.10.3, optimized Cabal build. Three real coding
sessions covered a small message-only context and two tool-heavy contexts:

| Context | Active turns | Items | Messages | Reasoning | Tool calls/outputs |
|---|---:|---:|---:|---:|---:|
| Small | 1 | 5 | 4 | 0 | 0 / 0 |
| Tool-heavy A | 4 | 210 | 58 | 39 | 54 / 54 |
| Tool-heavy B | 6 | 435 | 37 | 93 | 145 / 145 |

Before this change, each response item loaded its normalized child row with a
separate Hasql statement. The optimized path batches messages, reasoning,
reasoning summaries/content parts, function calls, and function-call outputs
once per turn when a child kind has more than eight rows. Smaller groups retain
the lower-latency indexed point-read path.

| Context | Implementation | Median wall | Median CPU | Allocation |
|---|:---|---:|---:|---:|
| Small | Per-item | 0.555 ms | 0.348 ms | 400,768 B |
| Small | Adaptive batching | 0.552 ms | 0.344 ms | 400,768 B |
| Tool-heavy A | Per-item | 13.935 ms | 9.362 ms | 7,374,344 B |
| Tool-heavy A | Adaptive batching | 5.271 ms | 3.530 ms | 3,704,656 B |
| Tool-heavy B | Per-item | 25.336 ms | 17.295 ms | 13,347,816 B |
| Tool-heavy B | Adaptive batching | 10.463 ms | 7.117 ms | 6,998,584 B |

The final same-binary comparison alternated implementations for three
11-sample rounds on each tool-heavy context. Tool-heavy allocation fell by
47.6–49.8%, elapsed time by 58.7–62.2%, and CPU time by 58.9–62.3%. The small
workload was repeated for five 31-sample rounds and stayed effectively flat.
Allocation was byte-for-byte stable across each implementation's rounds, and
all checksums matched.
