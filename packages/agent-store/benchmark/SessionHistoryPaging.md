# Session history paging benchmark

This benchmark exercises real PostgreSQL session reads and retains equivalent
`rowList` and `rowVector` decoder workloads. It reports median elapsed time,
CPU time, and allocated bytes after one warm-up query.

Build and run:

```sh
nix develop -c cabal build --offline \
  agent-store:bench:session-history-paging-bench
bin=$(nix develop -c cabal list-bin \
  agent-store:bench:session-history-paging-bench)
TMPDIR=/tmp "$bin" 1000,5000,10000 4096 5 +RTS -T
TMPDIR=/tmp "$bin" 10000 4096 11 +RTS -T
TMPDIR=/tmp "$bin" 10000 16 11 +RTS -T
```

`TMPDIR=/tmp` keeps the managed PostgreSQL Unix-socket path below PostgreSQL's
length limit. The benchmark component uses `-O2`; the `agent-store` library was
built with Cabal's optimized `-O1` build profile. Workloads use 4,096 assistant
payload bytes per turn and retain 80 active turns.

## Direct row decoder comparison

The benchmark has two direct decoder comparisons. `list-rows` and
`vector-rows` checksum the same 10,000 ordered
`(turn_index, assistant_text)` rows. The `*-rows-retained` variants inspect
only the first already-decoded row, avoiding a full consumer fold.

Hasql 2.0.1.0 does not implement `rowVector` through a list either. Its result
decoder allocates a mutable vector at the exact PostgreSQL row count, writes
each decoded row into it, and freezes it. `rowList` uses a separate strict
right fold.

These are 11-sample medians:

| payload bytes | decoder | elapsed ms | CPU ms | allocated bytes |
|---:|:---|---:|---:|---:|
| 4,096 | `rowList` | 62.254 | 49.419 | 92,972,408 |
| 4,096 | `rowVector` | 65.157 | 49.926 | 92,812,528 |
| 16 | `rowList` | 16.357 | 14.684 | 11,295,192 |
| 16 | `rowVector` | 16.826 | 14.998 | 11,135,368 |

With the representative 4 KiB payload, `rowVector` allocated 0.17% less in
the initial folded comparison. The roughly 160 KB difference across 10,000
rows is the expected avoided list-node overhead; row payload decoding
dominates the total. Timing varied between runs and changed direction when
the full consumer fold was removed, so the direct decoder microbenchmark
alone does not justify either container.

## Vector-native session assembly

The investigation also found quadratic ordered grouping in `loadSessions`.
`Map.fromListWith (flip (++))` repeatedly appended a singleton turn to an
existing per-session list.

An intermediate linear-List implementation prepended each singleton and
reversed each completed group once. The final implementation instead retains
Vectors throughout the store read path:

- Hasql statements decode metadata and turns with `rowVector`;
- contiguous per-session rows are grouped as Vector slices;
- stored sessions and pages expose Vector turn collections;
- page `take`, `reverse`, first, and last operations are Vector-native.

The CLI currently converts at its existing List API boundary. The benchmark's
`full-list-boundary` workload models that conversion explicitly.

Alternating builds of the linear-List commit and the Vector implementation,
using 7-sample medians:

| turns | implementation | elapsed ms | CPU ms | allocated bytes |
|---:|:---|---:|---:|---:|
| 1,000 | linear List | 53.708 | 42.420 | 25,994,880 |
| 1,000 | Vector | 44.418 | 37.119 | 25,940,328 |
| 5,000 | linear List | 282.887 | 228.157 | 129,835,952 |
| 5,000 | Vector | 245.680 | 197.245 | 129,463,584 |
| 10,000 | linear List | 555.487 | 451.436 | 259,667,040 |
| 10,000 | Vector | 496.722 | 393.155 | 258,886,312 |
| 10,000 | Vector plus List boundary | 498.635 | 393.487 | 258,886,624 |

The Vector path is 10.6–17.3% faster elapsed and 12.5–13.6% faster on CPU
across these sizes. At 10,000 turns the existing CLI List boundary retains a
10.2% elapsed and 12.8% CPU improvement over the linear-List store.

Compared with the original quadratic implementation:

| turns | before elapsed ms | after elapsed ms | before allocated | after allocated |
|---:|---:|---:|---:|---:|
| 1,000 | 54.426 | 44.418 | 53,886,960 | 25,940,328 |
| 5,000 | 383.395 | 245.680 | 1,225,494,776 | 129,463,584 |
| 10,000 | 1,053.146 | 496.722 | 4,709,280,832 | 258,886,312 |

At 10,000 turns this reduces elapsed time by 52.8% and allocation by 94.5%
while preserving turn order.
