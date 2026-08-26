# Real session loading benchmark

This benchmark measures canonical PostgreSQL session loading against an
existing coding session. It reports only the session key, a checksum, timing,
and Haskell allocation; it does not print transcript contents.

```console
nix develop -c cabal build --offline \
  agent-store:bench:real-session-load-bench
bin=$(nix develop -c cabal list-bin \
  agent-store:bench:real-session-load-bench)
"$bin" "$HOME/.haskell-agent" active SESSION_KEY 11 +RTS -T
```

Use `active` to measure the inference context loaded when resuming a session,
starting at its latest replacement/reset checkpoint. Use `full` to load every
persisted turn.

Inputs are read from the local managed PostgreSQL store. The store must already
contain the selected session. The first load is discarded as a warm-up; the
reported sample is the median of the requested measured loads. The checksum
forces every typed response-item field.

## 2026-08-26 results

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
| Small | Per-item | 0.431 ms | 0.274 ms | 373,888 B |
| Small | Adaptive batching | 0.436 ms | 0.275 ms | 373,808 B |
| Tool-heavy A | Per-item | 10.362 ms | 7.546 ms | 6,863,936 B |
| Tool-heavy A | Adaptive batching | 4.221 ms | 3.077 ms | 3,532,376 B |
| Tool-heavy B | Per-item | 20.128 ms | 14.733 ms | 12,754,032 B |
| Tool-heavy B | Adaptive batching | 8.738 ms | 6.436 ms | 6,801,312 B |

The final comparison alternated old and new optimized binaries for three
21-sample rounds. Tool-heavy allocation fell by 46.7–48.5%, elapsed time by
56.6–59.3%, and CPU time by 56.3–59.2%. The small workload stayed effectively
flat. Allocation was byte-for-byte stable across each implementation's rounds,
and all checksums matched.
