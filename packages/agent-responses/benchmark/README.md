# Responses hot-path benchmark

This benchmark measures:

- complete SSE framing plus per-event typed decoding,
- the common output-text delta path using one reusable Hermes session,
- direct `ResponseCreateParams` encoding,
- representative Aeson baselines.

Inputs are generated outside timed regions and split into 4 KiB HTTP chunks.
Samples vary the request body to prevent result sharing. Run with:

```console
nix develop -c cabal build agent-responses:bench:responses-hot-path
bin=$(nix develop -c cabal list-bin agent-responses:bench:responses-hot-path)
"$bin" direct-sse 2000 1024 9 +RTS -T
"$bin" aeson-sse 2000 1024 9 +RTS -T
```

The Aeson SSE baseline parses the same six fields into a typed record. The
Aeson request baseline follows the removed Responses instance's `toJSON` path:
it first builds an `Aeson.Value` object and then encodes it.

## 2026-08-27 baseline

Apple M3 Max, GHC 9.10.3, `-O2`, median of nine samples:

| Workload | Count × payload | Wall | Haskell allocation |
|:---|---:|---:|---:|
| Direct Hermes SSE | 10,000 × 16 B delta | 17.85 ms | 63.3 MB |
| Aeson typed SSE | 10,000 × 16 B delta | 10.25 ms | 79.9 MB |
| Direct Hermes SSE | 2,000 × 1 KiB delta | 3.48 ms | 17.9 MB |
| Aeson typed SSE | 2,000 × 1 KiB delta | 4.31 ms | 21.8 MB |
| Direct Hermes SSE | 2,000 × 1 KiB + raw extension | 4.89 ms | 22.8 MB |
| Aeson typed SSE | 2,000 × 1 KiB + raw extension | 4.93 ms | 26.3 MB |
| Direct request | 2 MiB input | 2.17 ms | 0 B reported |
| Aeson `Value` request | 2 MiB input | 2.34 ms | 6.3 MB |

The no-DOM implementation materially reduces Haskell allocation and wins the
large-event SSE and request cases, including opaque extension retention. Tiny
deltas remain slower due to one simdjson document
setup per event; this is recorded rather than hidden. Provider migration can
proceed because realistic larger payloads improve and memory is lower, while a
future Hermes batch-document API can address tiny-frame setup cost.

RTS allocation does not include simdjson's C++ allocations.
