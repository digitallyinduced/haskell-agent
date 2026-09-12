# Command completion: radix-trie experiment

## Scope

The live minimal and fullscreen editors use `slashMenuForCatalog`, which performs
fuzzy subsequence matching and ranking. The older
`slashCompletionCandidatesWithCatalog` prefix API has no production callers.
Consequently, a prefix-only benchmark is not evidence of a live UI improvement.

The candidate implementation uses a compressed radix trie to identify command
groups whose canonical name or alias contains the query as a subsequence. It
then applies the existing fuzzy scorer and stable catalog-order tie breaker.
It does not replace fuzzy matching with prefix-only matching. Empty queries keep
the existing enumeration path. The catalog index is shared across lookups and
replaced when skills change.

## Reproduction

Run from the repository root. Dependencies come exclusively from the Nix flake.
The direct optimized build below compiles the actual completion modules without
building the unrelated CLI runtime and provider libraries:

```sh
mkdir -p "$TMPDIR/command-completion-benchmark"
nix develop -c ghc -O2 -rtsopts -threaded -Wall \
  -XGHC2021 -XOverloadedStrings -XOverloadedRecordDot \
  -XNoFieldSelectors -XDuplicateRecordFields -XBlockArguments \
  -XLambdaCase -XDerivingStrategies \
  -ipackages/agent-cli/src -ipackages/agent-core/src \
  -ipackages/agent-json/src \
  packages/agent-cli/benchmark/CommandCompletion.hs \
  -outputdir "$TMPDIR/command-completion-benchmark" \
  -o "$TMPDIR/command-completion-benchmark/command-completion-bench"

nix develop -c "$TMPDIR/command-completion-benchmark/command-completion-bench" \
  menu 0 10000 7 +RTS -T -N1
```

The Cabal component is `agent-cli:bench:command-completion-bench`; it also builds
the completion source modules with `-O2`. Positional arguments are workload mode,
additional skill count, queries per warm sample, and sample count.

- `menu`: actual live menu, synthetic `skill-NNNNNN` names sharing a long prefix.
- `menu-distributed`: actual live menu, bundled skill names followed by generated
  names across ten descriptive namespaces.
- `prefix`: legacy prefix helper experiment; includes both the original
  algorithm and a cached normalized-list control, so normalization savings are
  not incorrectly attributed entirely to the trie.

Use 0, 10, 100, and 1000 skills, both menu shapes, and repeat a representative
case. The benchmark reports mixed, broad, selective, and non-prefix subsequence
query distributions separately.

## Measurement method

- Fixtures and query lists are generated and forced outside lookup timing.
- The baseline preserves the old menu scan; both versions use the same unchanged
  fuzzy scorer. The indexed version calls production `commandMenu` directly.
- Every output row's text lengths, match positions, flags, and replacement range
  contribute to a forced checksum. Equality validation compares complete menus
  before timing, including order and duplicate rows.
- Runtime inputs are read from IORefs; query actions recompute their result on
  every call rather than repeatedly forcing one shared thunk.
- Warm measurements alternate baseline/indexed ordering. Results are medians.
- CPU time uses `getCPUTime`; elapsed time uses `getMonotonicTimeNSec`.
- Allocation uses `GHC.Stats.allocated_bytes`. GC before and after the timed
  interval captures the final nursery; GC caused by the workload remains timed.
  Common measurement overhead is included in allocation figures.
- Construction is measured separately and fully forced. Cold-session rows also
  include creation of a fresh catalog and its lazily demanded index, for 1, 10,
  and 100 queries, to expose initialization costs.
- Additional retained heap is the post-major-GC live-byte difference with source
  data still live and a fresh index held through a scoped stable pointer. This is
  incremental Haskell heap, not process RSS or total application memory.
- Terminal rendering, input handling, database access, and model calls are not
  timed. Results describe completion computation, not whole-application CPU use.

## Behavior validation

The real minimal `readInlineEditor` was loaded through GHCi in `nix develop`
and exercised in tmux after reloading the changed command modules:

- `/mo`, Tab, Enter returned `/model `.
- Non-prefix fuzzy query `/mdl`, Tab, Enter also returned `/model `.
- `/zzzz` showed no suggestion; Tab, Enter preserved `/zzzz`.

This checks the actual minimal editor and completion integration, not a mock
Haskeline callback. It does not constitute a full application startup or a
fullscreen renderer smoke test. No model request was made.

## Results (2026-09-12)

Apple M3 Max, macOS 26.6.1, GHC 9.10.3, `-O2 -threaded`,
`+RTS -T -N1`, default allocation area. Seven samples per measurement;
10,000 warm queries per sample for 0/10/100 skills, 2,000 for 1,000 skills.
The fixture contains 59 always-available commands and 79 canonical/alias names
before adding skills. All menu equivalence validations passed.

Full CPU, elapsed-time, allocation, and checksum measurements are retained in
[CommandCompletionResults.csv](CommandCompletionResults.csv).

### Mixed-query warm lookup

Values are per query (sample medians divided by the number of queries). Allocation
is cumulative allocation per operation, not simultaneously resident memory.

| Skill shape / count | CPU µs, old → trie | CPU reduction | Allocated KiB, old → trie | Allocation reduction | Added retained KiB |
|---|---:|---:|---:|---:|---:|
| menu-0 | 8.74 → 5.23 | 40.2% | 82.46 → 54.60 | 33.8% | 32.08 |
| menu-10 | 12.02 → 6.60 | 45.1% | 104.31 → 61.43 | 41.1% | 39.30 |
| menu-100 | 32.72 → 13.91 | 57.5% | 299.94 → 115.65 | 61.4% | 101.76 |
| menu-1000 | 238.70 → 85.99 | 64.0% | 2245.29 → 657.81 | 70.7% | 750.05 |
| menu-distributed-10 | 12.28 → 7.31 | 40.4% | 103.68 → 69.84 | 32.6% | 39.65 |
| menu-distributed-100 | 41.06 → 17.00 | 58.6% | 344.03 → 137.98 | 59.9% | 102.04 |
| menu-distributed-1000 | 334.54 → 111.08 | 66.8% | 2768.37 → 652.71 | 76.4% | 755.20 |
| menu-0-repeat | 9.17 → 5.50 | 40.0% | 82.46 → 54.60 | 33.8% | 32.08 |

The built-in mixed case was repeated after the larger workloads: both versions
were about 5% slower in absolute CPU time, while the relative reduction remained
40% and allocated bytes were identical.

### Query-shape boundary

| Catalog | Broad CPU reduction | Selective CPU reduction | Non-prefix subsequence CPU reduction |
|---|---:|---:|---:|
| menu-0 | 22.9% | 54.1% | 51.1% |
| menu-10 | 22.3% | 59.7% | 56.2% |
| menu-100 | 29.5% | 79.1% | 70.7% |
| menu-1000 | 34.7% | 87.4% | 75.8% |
| menu-distributed-10 | 19.9% | 53.5% | 53.3% |
| menu-distributed-100 | 28.3% | 78.3% | 71.1% |
| menu-distributed-1000 | 32.0% | 90.5% | 76.2% |

All tested warm distributions improved in CPU and allocation. Subsequence lookup
is **not** an O(query-length) operation like exact prefix lookup: it may still
traverse most trie edges. Broad matches must still produce and score all matching
rows. Larger catalogs with unrelated names can therefore retain linear work.

### Construction and amortization

| Catalog | Fully forced index CPU ms | Construction allocation KiB | Cold 100-query session CPU ms, old → trie |
|---|---:|---:|---:|
| menu-0 | 0.027 | 164.11 | 0.918 → 0.727 |
| menu-10 | 0.035 | 200.06 | 1.222 → 0.785 |
| menu-100 | 0.092 | 579.03 | 3.147 → 1.104 |
| menu-1000 | 1.184 | 5207.41 | 20.251 → 5.391 |
| menu-distributed-10 | 0.032 | 194.17 | 1.231 → 0.917 |
| menu-distributed-100 | 0.083 | 477.23 | 4.555 → 2.608 |
| menu-distributed-1000 | 0.825 | 4011.19 | 40.939 → 19.455 |

**Tradeoff:** a single cold query is slower. For the built-in catalog it took
0.011 → 0.031 ms; with 1,000 distributed skills, 0.405 → 1.173 ms.
The 10-query typing session approximately broke even for the smallest distributed
catalog and improved for the other tested catalogs. Every 100-query cold session
used less CPU and allocated less memory, including index creation.

The index is retained for the lifetime of its catalog: approximately 32 KiB for
built-ins and 755 KiB with 1,000 distributed skills. This is a deliberate
CPU/allocation-versus-retained-memory tradeoff, not a total-memory reduction.
Catalog construction and equality do not force the new derived fields, so
sessions that never request completion do not pay the full index construction
cost. Construction itself is forced once demanded to avoid retaining temporary
construction tuples.

Decision: retain the lazy, shared trie for live completion. Do not claim a
first-query improvement or a whole-application memory reduction. Keep the
baseline, cold-session workload, and broad/subsequence distributions for future
regression measurements.

## Validation

`Agent.CLI.CommandSpec` passed in GHCi: 88 examples, 0 failures, including
equivalence properties and lazy catalog equality. Live minimal-editor checks
in tmux verified prefix completion (`/mo`), fuzzy completion (`/mdl`), and a
no-match query (`/zzzz`). The benchmark also checks exact menu equivalence
before timing each fixture. The full application test suite was not run.
