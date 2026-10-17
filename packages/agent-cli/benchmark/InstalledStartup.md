# Installed CLI startup comparison

Measured on 2026-10-16 after the compiled-launcher change, comparing installed
revision `9886c75` with previous revision `3b3b4ed`. Both are optimized Nix
production executables built with GHC 9.10.3 and compiled RTS defaults
`-N4 -M8G`. The machine was an Apple M3 Max, 14 logical CPUs, 36 GiB RAM, arm64
macOS. The launcher change did not change application initialization code.

The Haskell measurement programs were typechecked with GHCi and built with
`-O2 -threaded -rtsopts` in the existing Nix development shell. This is latency
evidence; allocation and GC improvements are not claimed.

## Launcher and application initialization

The version probe rotated executable order, discarded one warmup, and took
seven measured samples per executable. Median milliseconds:

| Invocation | Previous | Installed |
| --- | ---: | ---: |
| Packaged `--version` | 203.1 | 59.5 |
| Underlying executable `--version` | 52.6 | 49.6 |

One early installed-launcher observation took 1.32 seconds. Repeated warm
launches did not consistently reproduce that delay; its cause was not isolated.

Interactive measurements used fresh 120×40 tmux sessions, existing configuration
and credentials, warm storage, normal computer-use defaults, and
`--fullscreen --yolo`. They typed an unsubmitted probe and exited without a
model turn. Each scenario has three measured alternating samples per executable
after one discarded warmup each. The inherited PATH was 9,614 bytes, compared
with 18,432 bytes in the earlier launcher-fixture measurements.

Median launch-to-ready seconds:

| Scenario | Previous | Installed |
| --- | ---: | ---: |
| Plain launch | 1.338 | 1.318 |
| Resume empty diagnostic session | 1.470 | 1.095 |
| Resume inactive four-turn session | 1.103 | 1.130 |
| Create managed worktree and fetch upstream | 3.565 | 4.222 |

Median launch-to-observed-probe milliseconds:

| Scenario | Previous | Installed |
| --- | ---: | ---: |
| Plain launch | 382 | 328 |
| Resume empty diagnostic session | 474 | 353 |
| Resume inactive four-turn session | 320 | 166 |
| Create managed worktree and fetch upstream | 682 | 405 |

The launcher and editable composer improved. Normal full readiness was
effectively unchanged in these samples. Worktree readiness was slower, but the
small sample and variable network/storage work do not isolate a launcher
regression. These results do not support a general claim that total startup is
32% faster.

## Limits and remaining delays

- Worktree Git fetches took about 1.6–2.8 seconds. One installed-build sample
  spent 840 ms opening storage, versus 12–119 ms in the measured previous-build
  samples. Both builds exhibited variable worktree creation times.
- Host load averages were initially about 62–77 on 14 CPUs. During the
  57-second worktree/request measurement window, `vm_stat` recorded 71,766
  swapped-in pages and 135,436 swapped-out pages at 16,384 bytes per page:
  approximately 1.10 GiB in and 2.07 GiB out. Concurrent load and active paging
  limit timing stability; no attribution to a particular process was established.
- First-response latency could not be compared successfully. Both builds
  rejected the configured `gpt-6.1-sol` model as unsupported when using Codex
  with a ChatGPT account. Each was checked once, without changing model
  configuration. Both also reported existing MCP OAuth refresh failures.
- External times include tmux creation/capture/polling overhead. They are
  paired observations, not terminal-emulator presentation times, and are not
  directly comparable with the earlier direct-PTY measurements.
- The earlier launcher-fixture results used `--no-computer-use` and a longer
  development-shell PATH. Preserve that workload as evidence of launcher
  behavior, while using representative account settings for total readiness.

The paired tmux probe is retained as `scripts/BenchmarkCliStartup.hs`; its
[procedure](Startup.md#paired-tmux-comparison) documents the measurement
boundaries and commands. Repeat representative workloads under stable host
conditions before attributing small differences to application changes.
