# Production CLI package-split benchmark

This benchmark measures extracting production-unrelated modules from
`agent-cli` into two Cabal packages:

- `agent-repository` owns repository review, delivery, and process-security
  code;
- `agent-native-bridge` owns browser/MCP/resource administration and the
  Darwin foreign-library bridge.

The Linux production CLI no longer compiles six modules (5,747 source lines).
The Darwin-only FFI modules moved with their bridge but were not part of the
Linux baseline compilation.

## Method

Benchmark base revision: `db9d4f05d4a9d17a5dc27d89fd3dd1561f68a5d2`

Host: Intel Core i9-9900K, 16 logical CPUs, 64 GiB RAM, Linux 6.18.38

Toolchain: GHC 9.10.3, Cabal 3.16.1.0, Nix 2.34.7

The measured artifact is the real optimized production derivation:

```text
nix build .#agent-cli-static --no-link --print-build-logs --rebuild \
  --max-jobs 1 --cores 4
```

Each revision was built once to prime its new derivation, followed by three
forced rebuilds with the same cached dependency closure. This measures a fresh
production target build, not a cold rebuild of all transitive Nix
dependencies. No other Nix builds ran concurrently.
No garbage collection ran between stages; `/nix/store` had 57 GiB free during
the baseline samples and 35 GiB free during the package-split samples.

GNU `time` wraps the Nix client, so its wall clock covers the daemon build but
its maximum RSS is only the client process. Raw samples are in
[`build-time-benchmark-package-split-static.tsv`](build-time-benchmark-package-split-static.tsv).

## Results

| Metric | Baseline | Package split | Change |
| --- | ---: | ---: | ---: |
| Forced rebuild median | 64.97 s | 56.56 s | **-8.41 s (-12.9%)** |
| `agent-cli` library modules | 212 | 206 | -6 (-2.8%) |
| Compile-log entries | 215 | 209 | -6 |
| Installed executable output | 67,222,313 bytes | 67,222,313 bytes | unchanged |
| Installed data output | 24,079 bytes | 24,079 bytes | unchanged |

Samples were 58.27, 64.97, and 73.37 seconds before the split and 53.27,
56.56, and 58.31 seconds after it. The unchanged installed sizes are expected:
the executable did not import the extracted modules, but Cabal previously had
to compile every module in the library.

## Boundary and correctness checks

- The recursive static CLI derivation graph contains neither extracted package
  nor `agent-runtime-daemon`.
- `scripts/check-package-boundaries.sh` prevents the moved modules or package
  dependencies from leaking back into `agent-cli`.
- The Nix repository check passes 53 examples, the bridge check passes 41,
  and the remaining CLI check passes 1,570.
- `cabal check` passes for all three affected packages.
- Cabal source archives were generated for both new packages; the bridge
  archive contains its public C header.
- Linux flake outputs and checks evaluate successfully. The pinned nixpkgs
  cannot cross-evaluate the complete Darwin graph on this Linux host because
  an existing Haskell import-from-derivation requires an
  `aarch64-darwin` builder. The Darwin dependency set was therefore also
  checked with `cabal2nix --system aarch64-darwin`, and the native binary-cache
  job now builds both the bridge check and wrapped bridge artifact.
