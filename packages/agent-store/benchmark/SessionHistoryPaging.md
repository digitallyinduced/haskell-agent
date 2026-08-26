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

Both workloads select and checksum the same 10,000 ordered
`(turn_index, assistant_text)` rows. The benchmark consumes `rowVector`
directly with `Vector.foldl'`; it never converts it to a list.

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

With the representative 4 KiB payload, `rowVector` allocated 0.17% less but
took 4.66% more elapsed time and 1.03% more CPU time. The roughly 160 KB
allocation difference across 10,000 rows is the expected avoided list-node
overhead; row payload decoding dominates the total. With 16-byte payloads,
Vector saved 1.42% allocation but remained 2.87% slower elapsed and 2.14%
slower on CPU. The production decoder migration was therefore reverted.

## Session assembly improvement

The investigation also found quadratic ordered grouping in `loadSessions`.
`Map.fromListWith (flip (++))` repeatedly appended a singleton turn to an
existing per-session list. The replacement prepends each singleton with
`Map.fromListWith (++)` and reverses each completed group once.

Before/after medians for the full-transcript workload:

| turns | before elapsed ms | after elapsed ms | before allocated | after allocated |
|---:|---:|---:|---:|---:|
| 1,000 | 54.426 | 52.362 | 53,886,960 | 25,994,928 |
| 5,000 | 383.395 | 271.686 | 1,225,494,776 | 129,839,960 |
| 10,000 | 1,053.146 | 571.631 | 4,709,280,832 | 259,667,352 |

At 10,000 turns this reduces elapsed time by 45.7% and allocation by 94.5%
while preserving turn order and the existing list-based API.
