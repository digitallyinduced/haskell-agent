# macOS memory investigation — 2026-09-12

## Method and limits

Measurements used the installed optimized Nix CLI (`7shn2y7g5njgqmr6ynhcz81zqdl0x80r`),
ARM64 macOS 26.6.1, default `-N4 -M8G`, in separate temporary tmux sessions.
The workload was minimal interactive startup followed by short model replies,
with no tool calls. Filesystem skills, AGENTS.md, and computer use were disabled.
This is not a large-transcript or MCP-heavy workload and does not establish the
retained heap of the user's older, already-running binaries.

Built-in `+RTS -T -S<file>` supplied GC observations; a separate `-hT -i1`
run supplied closure-type profiles. Heap profiling perturbs collection behaviour.
macOS physical footprint came from `vmmap -summary`, not summed RSS or virtual size.

## Observations

| Checkpoint | Live heap after major GC | Physical footprint |
| --- | ---: | ---: |
| Fresh baseline, before first prompt | about 25 MiB | 149.8 MiB |
| Baseline after a short turn and idle | about 21.8 MiB | 84.4 MiB |
| Baseline after another short turn and idle | about 21.8 MiB | 84.7 MiB |

The settled baseline had about 62 MiB of dirty/resident anonymous VM, versus
about 21.8 MiB live heap. Copying-GC space, allocation areas, unused capacity,
and page-return behaviour must therefore be distinguished from live data.
Native allocations, writable library data, and runtime metadata add further overhead.
The 1 TiB GHC virtual reservation was unallocated; the 8 GiB setting is a limit,
not a per-process allocation.

A separate trial with `--disable-delayed-os-memory-return` did **not** improve
footprint: it settled at 181.6 MiB with roughly the same 21.8 MiB live heap.
Its anonymous VM included 83.3 MiB reported swapped out. These single runs were
not matched for all timing and system-pressure effects, so do not infer a
general regression or adopt that flag as a fix from this experiment.

## History retention and remaining leads

An idle closure profile totalled about 20.9 MiB, including 14.4 MiB of list
cons cells. Byte arrays were about 1.6 MiB; ASN.1 decoding structures also
contributed, consistent with certificate-related state. Closure types alone
do not identify ownership of generic lists or byte arrays.

The inline editor (`src/Agent/CLI/Input.hs`, `readHistory` and `editorLoop`)
retains Haskeline history containing `String` values while waiting for input.
It also creates a lazy `map Text.pack` view, which does not release the
original history needed by the save path.

An isolated optimized Haskeline harness confirmed the cost of this representation.
It fully traversed the history and retained it in an IORef read after major GC;
only numeric counts were printed. Across three runs, 8,897 entries containing
530,427 characters occupied 12,991,400 live bytes versus a blank-process median
of 48,720 bytes: **12.34 MiB additional live heap** from a roughly 540 KB file.
RTS heap capacity was 55 MiB versus 21 MiB in the blank harness; this capacity
delta is not a prediction of application physical-footprint savings.

A subsequent paired three-run comparison (8,898 entries / 530,437 characters;
another process had appended one entry) retained only fully evaluated strict
`Text` entries instead. Median total live bytes fell from 12,991,648 to
1,316,616: **11.13 MiB less live heap, about 90% of this representation's cost**.
RTS heap capacity fell from 55 to 30 MiB in the isolated harness.
The editor now retains compact history and reads/writes Text directly without
deleting on-disk history. See [the implementation benchmark](benchmark/CommandHistory.md).
End-to-end CLI footprint savings have not yet been measured.

Converting after `readHistory` does not remove parsing allocation: cumulative
allocation was about 92 MB for original history versus 94.4 MB for Text-only.
The baseline CLI allocated roughly 22 GiB cumulatively during the short test;
that is allocation churn, not simultaneous residency. A single history read
does not explain it. Attribution of that churn remains open.

Existing transcript eviction and major collection at turn completion are
already implemented. The short-run results do not show substantial live-heap
growth across turns, but cannot rule out leaks under other workloads.
MCP fleet/catalog retention and output-artifact caches remain separate leads
for larger sessions; their contribution was not isolated here.

## Diagnostic implementation

See [HEAP_DIAGNOSTICS.md](HEAP_DIAGNOSTICS.md) for the new opt-in numeric CSV
sampler. Normal operation is unchanged when the variable is absent.
Validation passed GHCi typechecking and an optimized standalone harness covering
disabled mode, missing RTS statistics, invalid forced-GC settings, periodic/final
samples, forced major collections, and append/header behaviour.

The initial offline application build lacked a `hermes` source checkout.
After fetching it, all 251 CLI library modules loaded successfully in Cabal GHCi.
The new executable integration has not been deployed. Production memory
measurements above use the existing executable, not the new sampler.
