# Model picker latency

Startup catalog selection is measured separately below; neither measurement
represents complete process launch time.

This benchmark exercises the production `modelChoiceWithEffort` path with a
successful cached gateway catalog and controlled transport latency. It observes
the first nonempty picker event and forces every row label, detail, and effort
indicator. It separately measures complete catalog/usage refresh publication,
then dismisses the picker and joins its workers outside the measured interval.

`blocking-baseline` retains the removed synchronous model refresh and usage
barrier: four concurrent usage requests, with a two-second timeout for the
entire group. It then uses the same production picker for equivalent row
construction and publication. The baseline forces every formatted usage
summary and the production picker reads those snapshots from its usage cache.
The background transports of that final shared picker invocation are blocked,
avoiding a second baseline refresh.

`cached-first` invokes the production picker without that barrier. Model and
usage transports have exactly the same injected latency as the baseline.
Catalog construction, cache seeding, runtime construction, and a garbage
collection occur before each measured sample.

The first-list metric stops before waiting for refresh completion. The
complete-refresh metric includes catalog publication and all model usage
annotations, forcing every intermediate update. It does not measure terminal
drawing or PostgreSQL startup/hydration. This is a latency benchmark, not an
allocation-reduction claim. Workloads must finish all usage requests within the
production two-second deadline; the documented dimensions satisfy that bound.

Build and run from the repository root:

```sh
nix develop -c cabal build --offline agent-cli:bench:model-picker-latency-bench
bin=$(nix develop -c cabal list-bin agent-cli:bench:model-picker-latency-bench)
for models in 4 16 64; do
    for workload in blocking-baseline cached-first; do
        nix develop -c "$bin" "$workload" "$models" 100 7
    done
done
for repetition in 1 2; do
    for workload in blocking-baseline cached-first; do
        nix develop -c "$bin" "$workload" 16 250 7
    done
done
```

The component uses `-O2 -threaded -rtsopts` and reports median elapsed and CPU
milliseconds. Production modules retain their repository build settings,
including module-local optimization overrides.

## Initial measurement and rejected update strategy

Measured on 2026-09-08, macOS arm64, GHC 9.10.3, Cabal library profile `-O1`,
benchmark component `-O2 -threaded -rtsopts`, default single RTS capability.
Seven samples per row, medians in milliseconds:

| Models | Transport delay | Workload | First list | First-list CPU | Complete refresh | Complete CPU |
|---:|---:|---|---:|---:|---:|---:|
| 4 | 100 | Blocking baseline | 202.398 | 0.679 | 202.398 | 0.679 |
| 4 | 100 | Cached, per-model updates | 0.138 | 0.117 | 102.011 | 0.701 |
| 16 | 100 | Blocking baseline | 505.261 | 0.893 | 505.261 | 0.893 |
| 16 | 100 | Cached, per-model updates | 0.147 | 0.149 | 405.503 | 3.567 |
| 64 | 100 | Blocking baseline | 1719.035 | 3.090 | 1719.035 | 3.090 |
| 64 | 100 | Cached, per-model updates | 0.351 | 0.356 | 1642.506 | 61.489 |
| 16 | 250 | Blocking baseline | 1254.995 | 1.362 | 1254.995 | 1.362 |
| 16 | 250 | Cached, per-model updates | 0.113 | 0.114 | 1005.824 | 3.941 |
| 16 | 250 | Blocking baseline, repeat | 1253.697 | 1.427 | 1253.697 | 1.427 |
| 16 | 250 | Cached, per-model updates, repeat | 0.110 | 0.113 | 1005.774 | 3.854 |

Cached-first presentation removed the network barrier, but publishing a complete
row snapshot for every usage response increased total CPU sharply with catalog
size. That per-model publication strategy was removed: usage results are now
published in a batch without delaying the initial model list.

## Batched usage publication

The same commands, build settings, machine, sample counts, and workload
dimensions were repeated after replacing per-model publication with a single
usage batch. The completion observer now requires the catalog update and a
complete usage update rather than one update per model.

| Models | Transport delay | Workload | First list | First-list CPU | Complete refresh | Complete CPU |
|---:|---:|---|---:|---:|---:|---:|
| 4 | 100 | Blocking baseline | 201.579 | 0.409 | 201.579 | 0.409 |
| 4 | 100 | Cached, batched updates | 0.061 | 0.057 | 101.273 | 0.388 |
| 16 | 100 | Blocking baseline | 504.714 | 0.894 | 504.714 | 0.894 |
| 16 | 100 | Cached, batched updates | 0.141 | 0.143 | 404.302 | 1.149 |
| 64 | 100 | Blocking baseline | 1715.990 | 3.013 | 1715.990 | 3.013 |
| 64 | 100 | Cached, batched updates | 0.296 | 0.299 | 1615.436 | 3.949 |
| 16 | 250 | Blocking baseline | 1255.469 | 1.457 | 1255.469 | 1.457 |
| 16 | 250 | Cached, batched updates | 0.140 | 0.136 | 1004.046 | 1.468 |
| 16 | 250 | Blocking baseline, repeat | 1254.833 | 1.388 | 1254.833 | 1.388 |
| 16 | 250 | Cached, batched updates, repeat | 0.132 | 0.137 | 1003.928 | 1.683 |

The representative 16-model, 250-ms transport workload publishes usable models
in 0.14 ms rather than 1.26 seconds. Complete refresh also finishes earlier,
because catalog and usage requests overlap. The two representative repetitions
agree closely on elapsed time.

This does not claim lower total CPU. Publishing the initial cached view and two
updated views has measurable overhead compared with publishing one final view:
at 64 models, complete CPU is 3.95 ms versus 3.01 ms. Batching removes the
rejected quadratic publication behavior (61.49 ms at 64 models); the remaining
cost is the additional bounded number of snapshots required for live refresh.
No network requests were removed or deferred beyond the complete-refresh
measurement.

## Startup catalog selection

The `startup-baseline` workload reproduces synchronous catalog refresh followed
by the production gateway model-option selection. `startup-cached` invokes
`withGatewayModelsForStartup` with a seeded cache, using the same production
selector. `startup-cold` invokes that helper without a cache. Every workload
selects the same explicit alias and forces the complete selected model target.
The reported `first-list` columns mean **selected startup target readiness** for
these workloads, not picker publication or terminal readiness.

The refreshed catalog reverses its original order so the benchmark can detect
actual cache publication. After target readiness, the observer waits for a
transport-completion signal, then cooperatively yields until the changed cache
is visible, forces every catalog identifier, and records complete elapsed/CPU
time. It does not poll during the injected network wait. The background worker
is cancelled and joined outside the timed interval when its scope closes.
Catalog construction, successful cache seeding, and GC occur before timing;
PostgreSQL hydration, authentication, repository discovery, and terminal setup
are excluded. This measures removal of the catalog network barrier, not a
reduction in complete-refresh work, allocation, or total CLI startup duration.

Use the optimized build and executable lookup above, then:

```sh
for models in 4 16 64; do
    for workload in startup-baseline startup-cached startup-cold; do
        nix develop -c "$bin" "$workload" "$models" 100 7
    done
done
for repetition in 1 2; do
    for workload in startup-baseline startup-cached startup-cold; do
        nix develop -c "$bin" "$workload" 16 250 7
    done
done
```
