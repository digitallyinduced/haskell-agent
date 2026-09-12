# Tool-output storage benchmark

Build the library and benchmark with optimization:

```sh
nix develop -c cabal build --offline agent-tools:bench:artifact-storage-bench --enable-optimization=2
binary=$(nix develop -c cabal list-bin agent-tools:bench:artifact-storage-bench --enable-optimization=2)
"$binary" 9 +RTS -T
"$binary" 9 +RTS -T
```

`ArtifactStorage.hs` compares the disk baseline (`toolOutputMemoryCap = 0`)
with the default bounded resident store. It uses identical single-line ASCII
responses of 64 KiB, 256 KiB, 1 MiB, and 2 MiB. The last six bytes contain the
search pattern. A 2 MiB response exceeds the resident per-artifact limit and
exercises disk fallback.

Each measurement includes artifact creation, resident copying or disk writing,
native tool dispatch, UTF-8 decoding, retrieval, JSON result serialization, and
a forced result checksum. Input construction and fresh environment allocation
are outside the measured interval. Temporary storage is removed after each
sample. Memory and disk results are checked for equality before measurement.

The benchmark reports medians of elapsed time, CPU time, and allocated bytes.
GC runs before each sample and after the timed operation to update allocation
statistics. The additional final GC is excluded from elapsed and CPU times.
Run it twice to check stability. This is an end-to-end storage/query comparison,
not a claim that resident storage minimizes allocations: retaining a private
copy deliberately trades bounded resident memory for avoiding filesystem work.
The filesystem baseline uses the operating system's ordinary cache, without
attempting to simulate cold storage or sandbox process-launch overhead.

## Measurement on 2026-09-09

macOS aarch64, GHC 9.10.3, library and benchmark `-O2`, nine samples per
combination, default RTS allocation settings and `+RTS -T`. Results below
are the final execution including cryptographically random artifact IDs,
following two earlier executions before that ID change. Each time is
milliseconds and allocation is bytes.

| KiB | Operation | Disk elapsed / CPU | Resident-eligible elapsed / CPU | Disk allocation | Resident-eligible allocation |
|---:|---|---:|---:|---:|---:|
| 64 | Read first page | 0.235 / 0.234 | 0.038 / 0.038 | 290696 | 208176 |
| 256 | Read first page | 0.311 / 0.269 | 0.049 / 0.050 | 290824 | 601616 |
| 1024 | Read first page | 0.853 / 0.482 | 0.199 / 0.199 | 291632 | 2177832 |
| 2048 | Read first page | 2.673 / 0.690 | 1.320 / 0.580 | 290664 | 290536 |
| 64 | Search final occurrence | 0.276 / 0.276 | 0.085 / 0.086 | 399296 | 182192 |
| 256 | Search final occurrence | 0.575 / 0.549 | 0.257 / 0.258 | 816400 | 587376 |
| 1024 | Search final occurrence | 1.832 / 1.450 | 1.178 / 1.180 | 2457840 | 2192952 |
| 2048 | Search final occurrence | 3.347 / 2.720 | 2.809 / 2.661 | 4643368 | 4643240 |

Both earlier executions also showed lower resident latency up to 1 MiB.
The final execution has noticeably more filesystem elapsed-time variation;
CPU time at 1 MiB improves from 0.482 to 0.199 ms for reads and from
1.450 to 1.180 ms for search. The 2 MiB control uses disk in both modes:
its search was 5–7% slower in the earlier executions and faster in the
final execution. This variability is not evidence of a fallback speedup.

The resident implementation regressed short-page allocation above 64 KiB:
resident storage copies the complete response, and its single-chunk decoder
also decodes the complete response before taking the page. These measurements
must not be described as a general allocation improvement.
