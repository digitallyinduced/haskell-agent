# Repository subprocess descriptor-limit benchmark

On Linux, `process-1.6.26.1` implements `close_fds=True` by scanning descriptor
numbers up to `sysconf(_SC_OPEN_MAX)`. Repository integration tests spawn many
isolated Git processes, so a large soft descriptor limit makes them expensive.
The check-mode package now caps that limit at 4096 in `preCheck`, inside the
Nix builder. Production/development packages and `close_fds` are unchanged.
Already smaller soft limits and the hard limit are preserved.

## Reproduce

From the repository root on an x86_64 Linux builder with a hard limit of at least
524288:

```sh
nix build --impure --no-link -L --file nix/benchmarks/repository-fd-limit.nix \
  --argstr root "git+file://$PWD"
nix build .#checks.x86_64-linux.agent-repository --no-link -L
```

The benchmark uses the actual check derivation, its compiled test binary and
fixtures (GHC 9.10.3, process 1.6.26.1, check-mode optimization disabled to match
CI). Only the soft descriptor limit varies. Three interleaved samples cover
snapshot/diff parsing and the longer stage/unstage/restore/commit workflow.
Each selection must run exactly one passing example. The entire suite runs
afterward at 4096. This diagnostic loop is not part of normal CI.

For another measurement of an already built derivation, add `--rebuild` to the
first command. Ensure dependencies are cached first; do not interpret dependency
downloads or compilation as test runtime. Avoid concurrent builds while timing.

## Local results (2026-09-12)

Hspec elapsed seconds, median of three samples:

| Soft descriptor limit | Snapshot/diff | Mutation workflow |
| ---: | ---: | ---: |
| 524288 (comparison baseline) | 7.0427 | 31.2677 |
| 65536 | 1.3088 | 5.7349 |
| 4096 (mitigation) | 0.5275 | 2.2600 |

The representative workflows improved about 13.4x and 13.8x. Some samples
overlapped another local validation build, so these are diagnostic rather than
isolated-machine precision measurements. All three samples show the same large
effect. The normal patched Nix check passed all 68 examples in 32.3298 seconds
(33 seconds for checkPhase). The unmodified local builder inherited 1048576,
not the explicitly selected 524288 comparison baseline.

A separate `-O2 -threaded` launch microbenchmark in `nix develop` corroborated
the cause: median time for ten isolated `true` launches was 0.49872 seconds at
524288 versus 0.00979 seconds at 1024 (five samples each). Tracing fifty launches
at 1024 recorded 50200 failed close calls and no `close_range` calls.

The previous hosted CI repository suite took 2496.3426 seconds in
[job 103447425379](https://github.com/digitallyinduced/haskell-agent/actions/runs/34655674394/job/103447425379).
That is a different machine/run, not a controlled before/after comparison.
Hosted CI improvement still needs verification after publishing the change.

Do not apply the test limit to production or disable `close_fds`: production
may legitimately hold descriptors above 4096. A general runtime fix would need
an upstream efficient close-range implementation with portable fallbacks.
