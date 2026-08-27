# Direct JSON codec benchmark

This benchmark compares the no-DOM portable and Hermes decoders with Aeson,
and the exact-size direct encoder with Jsonifier and Aeson. Inputs are
constructed outside the measured interval, vary by identifier to prevent
sharing, and decoded results are checksummed.

```console
nix develop -c cabal build agent-json-hermes:bench:direct-json-bench
bin=$(nix develop -c cabal list-bin agent-json-hermes:bench:direct-json-bench)
"$bin" hermes-decode 100000 32 7 +RTS -T
"$bin" jsonifier-encode 100000 32 7 +RTS -T
```

Arguments are workload, record count, bytes in the body field, and samples.
RTS allocation excludes simdjson's C++ allocations.

## 2026-08-26 baseline

Apple M3 Max, 36 GiB RAM, GHC 9.10.3, `-O2`, seven samples:

| Records | Body | Workload | Median wall | Haskell allocation |
|---:|---:|:---|---:|---:|
| 100,000 | 32 B | portable decode | 117.158 ms | 1,067,516,600 B |
| 100,000 | 32 B | Hermes decode | 91.020 ms | 450,902,312 B |
| 100,000 | 32 B | Aeson decode | 58.369 ms | 400,425,472 B |
| 20,000 | 1 KiB | portable decode | 89.518 ms | 1,023,296,856 B |
| 20,000 | 1 KiB | Hermes decode | 46.059 ms | 107,723,104 B |
| 20,000 | 1 KiB | Aeson decode | 58.400 ms | 96,385,584 B |
| 1,000 | 64 KiB | portable decode | 207.104 ms | 2,695,093,856 B |
| 1,000 | 64 KiB | Hermes decode | 87.627 ms | 67,128,392 B |
| 1,000 | 64 KiB | Aeson decode | 150.850 ms | 66,666,376 B |
| 100,000 | 32 B | direct encode | 26.304 ms | 186,826,696 B |
| 100,000 | 32 B | Aeson encode | 32.366 ms | 671,592,552 B |
| 20,000 | 1 KiB | direct encode | 21.540 ms | 57,354,152 B |
| 20,000 | 1 KiB | Aeson encode | 25.062 ms | 151,284,008 B |
| 1,000 | 64 KiB | direct encode | 51.180 ms | 64,698,968 B |
| 1,000 | 64 KiB | Aeson encode | 57.938 ms | 141,169,152 B |

The direct encoder numbers above include the public codec interpreter. Its
private Jsonifier write-plan primitive measured 10.653 ms / 49,727,736 bytes
for the tiny case before that interpreter overhead.

An experimental bytestring `Builder` encoder measured 125.324 ms for the tiny
case and 3,615.572 ms for the 64 KiB case, so it was rejected. The retained
direct encoder wraps Jsonifier's private exact-size write program; no generic
JSON value is exposed or constructed.

Hermes wins for larger typed records but remains slower for tiny objects in
this generic codec interpreter. The production Responses path now uses the
dependent object-fold and complete raw-value extension proposed upstream in
`velveteer/hermes#33`; this table predates that focused event specialization.
