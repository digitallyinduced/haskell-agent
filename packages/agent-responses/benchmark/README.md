# Responses hot-path benchmark

This benchmark measures:

- complete SSE framing plus per-event typed decoding,
- the common output-text delta path using one reusable Hermes session,
- full portable-versus-Hermes event decoding and stream assembly,
- direct `ResponseCreateParams` encoding,
- representative Aeson baselines.

Inputs are generated outside timed regions. SSE inputs are split into 4 KiB
HTTP chunks. The full-stream fixture contains three lifecycle snapshots with
two opaque namespace-tool payloads totalling 32 KiB in each snapshot, plus
output-item lifecycle events and a 50/30/10/10 mix of reasoning-summary,
output-text, function-argument, and custom-tool deltas.
Stream workloads replay the fixture 100 times per sample and report per-replay
results. Run optimized benchmarks with:

```console
nix develop -c cabal build --enable-optimization=2 \
  agent-responses:bench:responses-hot-path
bin=$(nix develop -c cabal list-bin --enable-optimization=2 \
  agent-responses:bench:responses-hot-path)
"$bin" direct-sse 2000 1024 9 +RTS -T
"$bin" aeson-sse 2000 1024 9 +RTS -T
"$bin" portable-stream 160 16 7 +RTS -T
"$bin" hermes-stream 160 16 7 +RTS -T
```

The Aeson SSE baseline parses the same six fields into a typed record. The
Aeson request baseline follows the removed Responses instance's `toJSON` path:
it first builds an `Aeson.Value` object and then encodes it. The stream
workloads decode the same current `ResponseStreamEvent` representation and run
the same assembly/checksum path, so they are the equivalent before/after
comparison for replacing the portable WebSocket decoder with Hermes.

## 2026-08-27 baseline

Apple M3 Max, GHC 9.10.3, `-O2`, median of seven stream samples and nine
samples for the other workloads:

| Workload | Count x payload | Wall | CPU | Haskell allocation |
|:---|---:|---:|---:|---:|
| Portable typed stream | 160 x 16 B + lifecycle | 1.45 ms | 1.45 ms | 18.6 MB |
| Hermes typed stream | 160 x 16 B + lifecycle | 0.414 ms | 0.414 ms | 2.35 MB |
| Portable typed stream | 160 x 1 KiB + lifecycle | 2.24 ms | 2.24 ms | 33.2 MB |
| Hermes typed stream | 160 x 1 KiB + lifecycle | 0.492 ms | 0.492 ms | 3.99 MB |
| Portable typed stream | 1,000 x 16 B + lifecycle | 5.11 ms | 5.11 ms | 47.2 MB |
| Hermes typed stream | 1,000 x 16 B + lifecycle | 2.51 ms | 2.51 ms | 13.0 MB |
| Portable typed stream | 1,000 x 1 KiB + lifecycle | 12.9 ms | 12.9 ms | 185.5 MB |
| Hermes typed stream | 1,000 x 1 KiB + lifecycle | 6.27 ms | 6.27 ms | 69.7 MB |
| Direct Hermes SSE | 10,000 x 16 B delta | 21.7 ms | 21.7 ms | 97.2 MB |
| Aeson typed SSE | 10,000 x 16 B delta | 10.6 ms | 10.6 ms | 75.7 MB |
| Direct Hermes SSE | 2,000 x 1 KiB delta | 5.15 ms | 5.15 ms | 27.1 MB |
| Aeson typed SSE | 2,000 x 1 KiB delta | 4.96 ms | 4.95 ms | 21.8 MB |
| Direct Hermes SSE | 500 x 64 KiB delta | 15.7 ms | 15.7 ms | 172.9 MB |
| Aeson typed SSE | 500 x 64 KiB delta | 43.8 ms | 43.8 ms | 169.6 MB |
| Direct request | 2 MiB input | 4.18 ms | 4.19 ms | 0 B reported |
| Aeson `Value` request | 2 MiB input | 2.68 ms | 2.68 ms | 6.3 MB |

The full typed-stream workload is the production migration boundary: Hermes
avoids portable reparsing of discriminated objects and raw lifecycle snapshots,
making the real-world-shaped 160-delta case about 3.5x faster with 7.9x lower
Haskell allocation. Isolated tiny SSE deltas still favor Aeson because they pay
one simdjson document setup per event; Hermes crosses over on larger documents.
That boundary is recorded rather than hidden.

RTS allocation does not include simdjson's C++ allocations.
