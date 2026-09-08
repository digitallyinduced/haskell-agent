# History chart restoration benchmark

This benchmark separates history event-handler latency from total chart work.
It uses distinct 2,000-point numeric line charts, native previews (Ghostty), and
empty preview caches. Input construction and runtime setup precede measurement.
Every requested chart must be queued/retained; a checksum forces every PNG byte.

* `synchronous`: current history reset/remapping plus the removed synchronous
  restore loop from `98ae13c`, using the same rasterizer and cache budgets.
* `event`: production reset, select, and enqueue, without starting the worker.
* `worker-total`: production reset/enqueue, scoped worker, mailbox delivery, and
  application of every prepared preview. Results are collected before applying
  them, rather than interleaved with terminal drawing.

The last boundary includes all rendering work, not just work shifted off the
event thread. Database reads, Brick reflow, Kitty transmission, terminal drawing,
and competing UI events are excluded. This measures responsiveness, not a
claim of increased rendering throughput.

## Reproduction

```sh
nix develop --command cabal build agent-cli:bench:history-chart-restoration-bench --offline
nix develop --command sh -c '
  bench=$(cabal list-bin agent-cli:bench:history-chart-restoration-bench)
  for charts in 1 8 64; do
    for workload in synchronous event worker-total; do
      "$bench" "$workload" "$charts" 2000 3 +RTS -N1 -T
    done
  done
'
```

The benchmark executable uses `-O2`; libraries use normal project optimization,
including the existing explicit `-O0` on the App/History orchestration modules.
Do not use GHCi timings. Output columns are workload, charts, points, samples,
median wall milliseconds, median process CPU milliseconds, and median allocated
bytes. GC occurs before and after samples for complete nursery accounting; the
final GC is excluded from wall/CPU timing. Allocation is cumulative, not peak
memory residency.

## Recorded results

Apple M3 Max, macOS 26.6.1, GHC 9.10.3, `+RTS -N1 -T`, three fresh samples per
row. The executable was directly compiled with `ghc -O2 -threaded -rtsopts`
against the current Cabal-registered libraries, using the same source and
dependencies as the stanza above.

| Charts | Boundary | Wall ms | CPU ms | Allocated bytes |
|---:|---|---:|---:|---:|
| 1 | synchronous | 98.463 | 95.127 | 394094376 |
| 1 | event | 0.011 | 0.012 | 6152 |
| 1 | worker-total | 105.661 | 103.667 | 405426480 |
| 8 | synchronous | 877.255 | 845.732 | 3152698184 |
| 8 | event | 0.067 | 0.057 | 29808 |
| 8 | worker-total | 870.913 | 850.913 | 3243203784 |
| 64 | synchronous | 6826.110 | 6593.407 | 25222768168 |
| 64 | event | 0.184 | 0.184 | 260800 |
| 64 | worker-total | 6593.216 | 6561.716 | 25952134336 |

Reset/enqueue no longer blocks on seconds of rasterization. Total work remains
similar, with about 2.9% more allocation at 64 charts. Small timing differences
between total-work rows should not be interpreted as a throughput improvement.
