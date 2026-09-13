# Artifact occurrence retrieval benchmark

## Purpose and performance boundary

This benchmark records the cost of replacing matching-line prefixes with
paginated occurrence contexts and original Unicode coordinates. The baseline
and replacement receive identical files but **do not produce equivalent
output**: the baseline can hide a late match beyond its line preview, whereas
the replacement returns that occurrence. These measurements are not a blanket
search speedup claim.

The baseline specializes the previous byte-streaming search to a single line:
32KiB reads, a retained pattern overlap, and a 16KiB prefix. Its case-insensitive
counterpart uses lazy line scanning and Unicode case folding, including the
scan required to establish whether another matching line exists.

The replacement searches bounded 32K-code-point windows with pattern overlap.
It returns the first occurrence with up to 128 characters of context on each
side and a continuation cursor. Early matches can stop before EOF. Late
matches still require a full scan; decoded coordinates, context and JSON
metadata introduce modest additional allocation compared with line prefixes.

The benchmark includes file reads, decoding, search, encoding and forced output
checksums. File creation is outside measurement. It reports medians of seven
samples, with GC before each sample and after the measured interval to make
allocation counters current. Both elapsed and CPU time exclude the final GC.
Files are 64KiB, 256KiB, 1MiB and 8MiB, with one `needle` at the beginning or
end of an otherwise single-line ASCII document. A second 1MiB run checks
measurement stability. The separate residency mode searches a supplied file
case-insensitively without constructing its contents in memory.

## Commands

From the repository root:

```sh
nix develop -c cabal build --offline agent-tools:bench:artifact-retrieval-bench
binary=$(nix develop -c cabal list-bin agent-tools:bench:artifact-retrieval-bench)
nix develop -c "$binary" +RTS -T
```

The benchmark stanza compiles the retrieval module directly with `-O2` rather
than depending on the optimization setting of an already-built library.

The measurements below used the equivalent standalone build in the Nix
development shell, GHC 9.10.3, macOS aarch64, on 2026-09-09
(paths below updated for the package extraction):

```sh
mkdir -p "$TMPDIR/artifact-retrieval-build"
ARTIFACT_BENCHMARK_BUILD="$TMPDIR/artifact-retrieval-build" \
  nix develop -c sh -c '
    ghc -O2 -rtsopts -package-env - -ipackages/agent-tools/src \
      -outputdir "$ARTIFACT_BENCHMARK_BUILD" \
      packages/agent-tools/benchmark/ArtifactRetrieval.hs \
      -o "$ARTIFACT_BENCHMARK_BUILD/benchmark" &&
    "$ARTIFACT_BENCHMARK_BUILD/benchmark" +RTS -T
  '
```

`-package-env -` selects the Nix development-shell packages without combining
them with a separate Cabal package environment.

## Recorded results

Each cell is **elapsed ms / CPU ms / allocated bytes**.

| Size | Match | Baseline sensitive | Occurrences sensitive |
|---|---|---|---|
| 64KiB | Early | 0.044 / 0.043 / 121968 | 0.028 / 0.027 / 108680 |
| 64KiB | Late | 0.100 / 0.100 / 153552 | 0.087 / 0.086 / 245904 |
| 256KiB | Early | 0.063 / 0.063 / 320232 | 0.032 / 0.038 / 108680 |
| 256KiB | Late | 0.285 / 0.285 / 552024 | 0.267 / 0.267 / 660800 |
| 1MiB | Early | 0.123 / 0.118 / 1122568 | 0.026 / 0.025 / 108680 |
| 1MiB | Late | 1.157 / 1.150 / 2144128 | 1.088 / 1.087 / 2298296 |
| 8MiB | Early | 0.918 / 0.917 / 8575496 | 0.028 / 0.027 / 108680 |
| 8MiB | Late | 9.294 / 9.236 / 16982624 | 8.741 / 8.709 / 17584672 |
| 1MiB repeated | Early | 0.135 / 0.134 / 1119888 | 0.026 / 0.026 / 112440 |
| 1MiB repeated | Late | 1.577 / 1.432 / 2144312 | 1.193 / 1.160 / 2298296 |

| Size | Match | Baseline folded | Occurrences folded |
|---|---|---|---|
| 64KiB | Early | 0.103 / 0.103 / 271760 | 0.087 / 0.086 / 208448 |
| 64KiB | Late | 0.209 / 0.209 / 308952 | 0.204 / 0.203 / 312120 |
| 256KiB | Early | 0.136 / 0.135 / 676528 | 0.090 / 0.089 / 208448 |
| 256KiB | Late | 0.720 / 0.720 / 903816 | 0.716 / 0.716 / 925736 |
| 1MiB | Early | 0.310 / 0.302 / 2275760 | 0.091 / 0.090 / 208448 |
| 1MiB | Late | 2.949 / 2.947 / 3300296 | 2.943 / 2.941 / 3356672 |
| 8MiB | Early | 2.151 / 2.151 / 17243080 | 0.090 / 0.090 / 208448 |
| 8MiB | Late | 23.955 / 23.408 / 25654472 | 24.250 / 23.755 / 26051072 |
| 1MiB repeated | Early | 0.286 / 0.258 / 2275760 | 0.102 / 0.102 / 208448 |
| 1MiB repeated | Late | 3.356 / 3.057 / 3298632 | 2.867 / 2.866 / 3356928 |

Early searches benefit from stopping at the requested occurrence count. Late
search timing is similar, with run-to-run variation. At 8MiB, additional
allocation is approximately 3.5% sensitive and 1.5% folded. The small-input
sensitive case has proportionally greater fixed result overhead. These are
explicit costs of the richer retrieval functionality, not an allocation
optimization.

## Bounded residency

Create a 64MiB document followed by the sole match:

```sh
python3 - <<'PY'
import os
with open(os.path.join(os.environ["TMPDIR"], "artifact-residency-input"), "wb") as output:
    for _ in range(1024):
        output.write(b"x" * 65536)
    output.write(b"needle")
PY
nix develop -c "$binary" residency "$TMPDIR/artifact-residency-input" \
  +RTS -T -M8m -A256k -s
```

The recorded folded search completed with a 271-byte result, 157792 bytes
maximum residency and 2MiB total memory in use. Total allocation was
207724136 bytes and total elapsed time was 0.189s. This verifies bounded live
memory, not constant total allocation. The small nursery is intentional:
`-M8m` alone exhausted the default allocation configuration despite low live
residency.
