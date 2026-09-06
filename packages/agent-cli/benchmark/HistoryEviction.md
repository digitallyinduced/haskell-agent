# History eviction

`HistoryEvictionBaseline.hs` preserves the original page-merge implementation.
The benchmark compiles it and production `Agent.CLI.TUI.History` together with
`-O2`, avoiding interpreted timings or different optimization settings.

```sh
nix develop
cabal build --offline agent-cli:bench:history-eviction-bench
bin=$(cabal list-bin agent-cli:bench:history-eviction-bench)
for mode in old new; do
  "$bin" "$mode" page 120 512 7
done
```

Arguments: implementation, scenario, turn count, ASCII body bytes per block,
sample count. Each sample merges 20 independently constructed pages, alternating
older/newer directions (append uses newer only). Every turn has four blocks.

- `append`: one incoming turn into a full window of `count` turns.
- `page`: 30 incoming turns into a full window of `count` turns.
- `turns`, `blocks`, `bytes`: bulk pages of `count` turns with up to 120 existing
  turns; the named budget retains the larger of the initial window and one
  quarter of the merged window.
- `no-eviction`: the bulk workload with all budgets large enough to retain it.

Inputs and existing indexes are forced outside timing. Outputs are consumed
identically, including both indexes and block text. Each invocation first
checks full baseline/production result equality using separate fixtures.
CSV columns are implementation, scenario, count, body bytes, samples, median
elapsed milliseconds/operation, median CPU milliseconds/operation, and median
allocated bytes/operation. RTS statistics are enabled by the benchmark stanza.
Explicit collections bracket samples; the trailing collection updates allocation
statistics but is outside the clocks. This is an in-memory merge benchmark,
not an end-to-end terminal or storage benchmark.

The optimization uses temporary budget accounting and builds indexes only after
trimming. No persistent cached sizes are added to the public record. Anchor
protection, duplicate precedence, paging flags, and the soft-budget exception
for a single oversized turn remain unchanged. Sorting/deduplication, final
index construction, and scanning retained text still cost work.

## Measured results

macOS/aarch64, GHC 9.10.3 from the repository Nix shell, benchmark `-O2`,
default RTS allocation area, `-T`, seven samples of twenty operations.
Run each row below with the command above, substituting scenario/count/body.
Values are old → new; allocation is bytes per operation.

| Scenario | Count | Body | Elapsed ms | CPU ms | Allocated bytes |
|---|---:|---:|---:|---:|---:|
| no-eviction | 120 | 512 | 0.20050 → 0.17245 | 0.20035 → 0.17250 | 594947 → 543035 |
| page | 120 | 512 | 0.59810 → 0.09765 | 0.59660 → 0.09770 | 4365011 → 284019 |
| append | 120 | 512 | 0.10685 → 0.09405 | 0.10685 → 0.09410 | 380831 → 233263 |
| turns | 500 | 512 | 22.28490 → 0.25885 | 22.27685 → 0.25930 | 181320723 → 1294171 |
| blocks | 500 | 512 | 26.97060 → 0.25590 | 26.41180 → 0.25565 | 205152151 → 1299987 |
| bytes | 500 | 512 | 45.20150 → 0.44950 | 45.16595 → 0.44820 | 240530459 → 1429819 |
| bytes | 1000 | 4096 | 474.67370 → 4.35615 | 473.93645 → 4.35265 | 782743807 → 2765023 |
| no-eviction (repeat) | 120 | 512 | 0.18805 → 0.16605 | 0.18810 → 0.16610 | 594947 → 543035 |
| page (repeat) | 120 | 512 | 0.55550 → 0.09395 | 0.55495 → 0.09400 | 4365011 → 284019 |

Normal 30-turn paging is about 6× faster with 93.5% less allocation in this
fixture. Bulk eviction is a stress case, not a claim about ordinary UI latency.
An initial staged-only implementation slightly increased no-eviction allocation;
the final version retains a no-eviction fast path and uses strict folds for
loaded totals instead of building intermediate mapped sequences. No-eviction
allocation now falls by 8.7%, with elapsed/CPU improvements on both runs.
