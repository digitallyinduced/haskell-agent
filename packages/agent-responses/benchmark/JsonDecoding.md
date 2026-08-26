# Responses JSON decoding benchmark

This benchmark measures the UTF-8 round trip removed from the Responses SSE
decoder. Inputs are constructed outside the measured interval, vary by sequence
number to prevent sharing, and decoded results are checksummed.

```console
nix develop -c cabal build agent-responses:bench:responses-json-bench
bin=$(nix develop -c cabal list-bin agent-responses:bench:responses-json-bench)
"$bin" utf8-round-trip 100000 16 9 +RTS -T
"$bin" direct-bytes 100000 16 9 +RTS -T
"$bin" coding-utf8-round-trip 100000 16 9 +RTS -T
"$bin" coding-direct-bytes 100000 16 9 +RTS -T
```

The workloads decode the same canonical `ResponseStreamEvent`. The baseline
models the old `ByteString -> Text -> ByteString` path; `direct-bytes` models
the new path after the SSE block has already been UTF-8 validated.
The `coding-*` workloads replay the event mix in a normal coding turn: 50%
reasoning-summary deltas, 30% assistant-text deltas, 10% function-call
argument deltas, and 10% custom-tool input such as `apply_patch`.

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

## Mixed coding-turn results

Apple M3 Max, 36 GiB RAM, GHC 9.10.3, `-O2`, 2026-08-27:

| Events | Delta | Workload | Median wall | Median CPU | Haskell allocation |
|---:|---:|:---|---:|---:|---:|
| 100,000 | 16 B | UTF-8 round trip | 101.383 ms | 101.202 ms | 674,449,896 B |
| 100,000 | 16 B | Direct bytes | 97.889 ms | 97.795 ms | 627,408,664 B |
| 20,000 | 1,024 B | UTF-8 round trip | 45.908 ms | 45.949 ms | 191,375,808 B |
| 20,000 | 1,024 B | Direct bytes | 42.790 ms | 42.803 ms | 142,534,096 B |
| 500 | 65,536 B | UTF-8 round trip | 39.944 ms | 39.941 ms | 99,713,024 B |
| 500 | 65,536 B | Direct bytes | 38.497 ms | 38.526 ms | 32,167,264 B |

For the mixed coding stream, direct decoding reduces allocation by 7.0% for
token-sized events, 25.5% for 1 KiB tool-argument chunks, and 67.7% for large
events. It is 3.4–6.8% faster.

Hermes 0.8.0.0 was also evaluated with a reusable environment and a
semantics-preserving hybrid decoder. On a representative mixed SSE stream it
regressed from 3.712 ms / 25,599,736 Haskell-allocated bytes with Aeson to
6.305 ms / 34,441,224 bytes. At the larger workload it regressed from
21.174 ms / 136,385,344 bytes to 53.469 ms / 172,133,160 bytes. The runtime
Hermes integration was therefore rejected rather than shipped.
