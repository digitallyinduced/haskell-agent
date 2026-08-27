# Buffered OpenAI JSON decoding benchmark

This benchmark compares the former
`ByteString -> Text -> ByteString -> Aeson` path with direct decoding from the
original buffered HTTP response bytes. It covers ordinary Responses JSON and
buffered SSE compatibility responses, plus the `/responses/compact` envelope.

Each sample uses a distinct prebuilt payload set so GHC cannot share pure
decoder results between samples. The decoded response id, message id, and full
assistant text contribute to the checksum. Old and direct checksums are
compared before measurement. Wall time, CPU time, and Haskell allocation use
independent medians.

Build and run:

```sh
nix develop -c cabal build --offline agent-openai:bench:openai-json-decoding-bench
bin=$(nix develop -c cabal list-bin agent-openai:bench:openai-json-decoding-bench)
"$bin" http-utf8-round-trip 10000 64 9 +RTS -T
"$bin" http-direct-bytes 10000 64 9 +RTS -T
"$bin" http-sse-utf8-round-trip 10000 64 9 +RTS -T
"$bin" http-sse-direct-bytes 10000 64 9 +RTS -T
"$bin" compact-utf8-round-trip 10000 64 9 +RTS -T
"$bin" compact-direct-bytes 10000 64 9 +RTS -T
```

## 2026-08-27 results

Apple M3 Max, 36 GiB RAM, GHC 9.10.3, `-O2`, nine samples:

| Path | Responses | Text | Workload | Median wall | Median CPU | Haskell allocation |
|:---|---:|---:|:---|---:|---:|---:|
| JSON | 10,000 | 16 B | UTF-8 round trip | 42.071 ms | 41.991 ms | 281,545,512 B |
| JSON | 10,000 | 16 B | Direct bytes | 40.870 ms | 40.768 ms | 274,893,344 B |
| JSON | 2,000 | 1 KiB | UTF-8 round trip | 11.796 ms | 11.771 ms | 58,557,600 B |
| JSON | 2,000 | 1 KiB | Direct bytes | 11.437 ms | 11.413 ms | 57,058,888 B |
| JSON | 100 | 64 KiB | UTF-8 round trip | 10.897 ms | 10.884 ms | 19,180,552 B |
| JSON | 100 | 64 KiB | Direct bytes | 10.495 ms | 10.441 ms | 5,934,776 B |
| SSE | 10,000 | 16 B | UTF-8 round trip | 57.632 ms | 57.583 ms | 364,587,816 B |
| SSE | 10,000 | 16 B | Direct bytes | 57.277 ms | 57.047 ms | 354,978,200 B |
| SSE | 2,000 | 1 KiB | UTF-8 round trip | 16.394 ms | 16.382 ms | 81,619,088 B |
| SSE | 2,000 | 1 KiB | Direct bytes | 16.101 ms | 16.088 ms | 75,455,536 B |
| SSE | 100 | 64 KiB | UTF-8 round trip | 15.037 ms | 15.011 ms | 32,633,368 B |
| SSE | 100 | 64 KiB | Direct bytes | 14.870 ms | 14.875 ms | 19,825,080 B |
| Compact | 10,000 | 16 B | UTF-8 round trip | 16.203 ms | 16.190 ms | 131,272,496 B |
| Compact | 10,000 | 16 B | Direct bytes | 15.402 ms | 15.378 ms | 127,436,264 B |
| Compact | 2,000 | 1 KiB | UTF-8 round trip | 6.449 ms | 6.441 ms | 30,796,616 B |
| Compact | 2,000 | 1 KiB | Direct bytes | 6.141 ms | 6.119 ms | 24,501,304 B |
| Compact | 100 | 64 KiB | UTF-8 round trip | 10.553 ms | 10.544 ms | 17,919,016 B |
| Compact | 100 | 64 KiB | Direct bytes | 9.953 ms | 9.930 ms | 5,013,208 B |

Direct decoding allocated less in every workload. The allocation reduction
grew to 69% for large ordinary JSON responses, 39% for large buffered SSE
responses, and 72% for large compact responses. Median wall time also improved
in every case. A repeated token-sized run reproduced the allocation totals;
direct decoding was 0.2% faster for JSON, 1.9% for SSE, and 4.7% for compact
responses.
