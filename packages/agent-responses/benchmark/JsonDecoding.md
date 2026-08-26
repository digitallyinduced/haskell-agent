# Responses JSON decoding benchmark

This benchmark measures the UTF-8 round trip removed from the Responses SSE
decoder. Inputs are constructed outside the measured interval, vary by sequence
number to prevent sharing, and decoded results are checksummed.

```console
nix develop -c cabal build agent-responses:bench:responses-json-bench
bin=$(nix develop -c cabal list-bin agent-responses:bench:responses-json-bench)
"$bin" utf8-round-trip 100000 16 9 +RTS -T
"$bin" direct-bytes 100000 16 9 +RTS -T
```

The workloads decode the same canonical `ResponseStreamEvent`. The baseline
models the old `ByteString -> Text -> ByteString` path; `direct-bytes` models
the new path after the SSE block has already been UTF-8 validated.

## 2026-08-26 results

Apple M3 Max, 36 GiB RAM, GHC 9.10.3, `-O2`:

| Events | Delta | Workload | Median wall | Haskell allocation |
|---:|---:|:---|---:|---:|
| 100,000 | 16 B | UTF-8 round trip | 92.801 ms | 692,842,944 B |
| 100,000 | 16 B | Direct bytes | 90.164 ms | 646,174,224 B |
| 20,000 | 1,024 B | UTF-8 round trip | 44.647 ms | 196,555,528 B |
| 20,000 | 1,024 B | Direct bytes | 41.946 ms | 147,915,832 B |
| 500 | 65,536 B | UTF-8 round trip | 41.038 ms | 99,793,008 B |
| 500 | 65,536 B | Direct bytes | 39.171 ms | 32,204,752 B |

Direct byte decoding is 3–6% faster here. Its allocation reduction grows from
7% for tiny deltas to 68% for 64 KiB deltas.

Hermes 0.8.0.0 was also evaluated with a reusable environment and a
semantics-preserving hybrid decoder. On a representative mixed SSE stream it
regressed from 3.712 ms / 25,599,736 Haskell-allocated bytes with Aeson to
6.305 ms / 34,441,224 bytes. At the larger workload it regressed from
21.174 ms / 136,385,344 bytes to 53.469 ms / 172,133,160 bytes. The runtime
Hermes integration was therefore rejected rather than shipped.
