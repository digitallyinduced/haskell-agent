# Build-time benchmark

This document records the baseline for the package-boundary refactor.  The
benchmark is implemented by
`scripts/benchmark-build-times.sh`; it uses optimized Cabal builds, separate
fresh build directories for fresh-local-package samples, and separately
primed build directories for incremental scenarios. External dependencies use
the same Cabal store in both worktrees. Marker edits and timestamps are
restored automatically.

## Baseline

Revision: `d0df7e20249aca5ee86f91a1e9aac145e1dce949`

Host: Apple M3 Max (14 cores), macOS 26.6.1, 2026-08-31

Toolchain: GHC 9.10.3, Cabal 3.16.1.0, Nix 2.34.8

Build settings: Cabal `-O1`, `--jobs=8`
Commands:

```text
nix develop -c ./scripts/benchmark-build-times.sh 3 baseline cold
nix develop -c ./scripts/benchmark-build-times.sh 3 refactored cold
nix develop -c ./scripts/benchmark-build-times.sh 7 baseline incremental
nix develop -c ./scripts/benchmark-build-times.sh 7 refactored incremental
nix develop -c ./scripts/benchmark-nix-source-invalidation.sh 3 baseline
nix develop -c ./scripts/benchmark-nix-source-invalidation.sh 3 refactored
```

Both revisions use GHC 9.10.3, Cabal 3.16.1.0, `-O1`, and the same machine.
Results below are medians. The fresh-build runs use three samples and the
short incremental runs use seven. Raw samples are checked in next to this
document:

- [`build-time-benchmark-baseline-fresh.tsv`](build-time-benchmark-baseline-fresh.tsv)
- [`build-time-benchmark-refactored-fresh.tsv`](build-time-benchmark-refactored-fresh.tsv)
- [`build-time-benchmark-baseline-incremental.tsv`](build-time-benchmark-baseline-incremental.tsv)
- [`build-time-benchmark-refactored-incremental.tsv`](build-time-benchmark-refactored-incremental.tsv)
- [`build-time-benchmark-nix-invalidation.tsv`](build-time-benchmark-nix-invalidation.tsv)

## Workloads

- fresh optimized local-package build of `agent-cli:lib:agent-cli`;
- fresh optimized local-package build of `agent-telegram:lib:agent-telegram`;
- warm CLI-only marker edit followed by Telegram build;
- warm CLI-only marker edit followed by CLI build;
- warm shared-runtime marker edit followed by each consumer;
- production Nix output-path evaluation after a test-only source edit.

The primary success criterion is that a CLI-only edit no longer rebuilds
`agent-cli` when building Telegram.

The marker is a comment-only source change. It intentionally measures package
source invalidation and component rebuild scheduling without conflating the
result with an API/ABI change. Shared-runtime edits use the same kind of marker
to show the cost that both consumers must still pay.

## Results

| Workload | Baseline median | Refactored median | Elapsed change |
| --- | ---: | ---: | ---: |
| Fresh local-package `agent-cli` | 327.70 s | 312.64 s | **-4.6%** |
| Fresh local-package `agent-telegram` | 328.31 s | 248.87 s | **-24.2%** |
| CLI-only edit → Telegram | 5.19 s | 0.41 s | **-92.1%** |
| CLI-only edit → CLI | 3.28 s | 4.49 s | +36.9% |
| Shared-runtime edit → Telegram | 8.08 s | 4.76 s | **-41.1%** |
| Shared-runtime edit → CLI | 5.36 s | 5.99 s | +11.8% |
| Test-only edit changes production Nix output | yes | no | **pass** |

CPU measurements support the cross-frontend result: fresh Telegram user time
fell from 407.52 s to 247.53 s (-39.3%), and CLI-only edit → Telegram user
time fell from 3.36 s to 0.29 s (-91.4%). A fresh CLI build remained neutral
in user time (377.11 s to 380.96 s).

The component evidence is more important than sub-second timing noise:
after the refactor, all seven CLI-only edit → Telegram samples compiled zero
lines and reported `agent-cli-built=false`, `runtime-built=false`, and
`telegram-built=false`. Before the refactor, both the CLI and Telegram
components were rebuilt.

There is a deliberate boundary cost on the other side. An edit to the shared
runtime now crosses two Cabal components when rebuilding the CLI, adding
0.63 s median in this run, and ordinary CLI-only rebuilds paid about 1.21 s
more Cabal/link overhead. That tradeoff is why this result does **not** justify
splitting additional CLI-local directories into packages. The package
boundary is useful specifically because Telegram and future non-terminal
frontends can omit the 189-module terminal library.

Concurrent unrelated Nix bundle builds were active on the host. Wall-clock
samples therefore have more variance than CPU samples (notably the third
fresh CLI sample), while component-rebuild assertions are deterministic.
