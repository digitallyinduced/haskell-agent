# Bounded tool-preview saturation benchmark

Reproduce from the repository root with an existing Cabal build plan and
registered `agent-core`, `agent-json`, and `agent-responses-types` libraries:

```sh
nix develop -c env TMPDIR="$TMPDIR" \
  python3 packages/agent-responses/benchmark/preview-saturation.py
```

The script extracts the unchanged production preview module from `f37672e5`
as `BaselinePreview`, compiles it and the working-tree module together with
GHC `-O2 -threaded -rtsopts`, and runs with `+RTS -T -N1`.
Shared dependencies are the same registered Cabal libraries for both variants.
The temporary package environment retains Cabal's package database declarations
but not its whole-project exposed package list. Explicit dependency IDs come
from the Cabal plan, avoiding accidental selection of different Nix versions.

This measures the actual `toolArgumentStreamStep`, including identity
resolution, bounded preview accumulation, shell partial-JSON parsing, event
publication, runaway counters, and sparse item-done recovery. It is **not**
an HTTP/WebSocket, response-assembly, tool execution, renderer, or whole-agent
memory benchmark. No peak-residency claim follows from its allocation numbers.

Each input is one function call, a JSON `{"command":"..."}` argument split into
N 16-character ASCII deltas plus opening/closing fragments, and a sparse done
item. `read_file` selects generic raw previews; its synthetic command field is
not a schema-valid read_file argument and no tool is dispatched.
`shell_command` exercises actual live command preview parsing. Input is decoded
and forced outside timing. Every emitted event's full shown representation
is compared between old/new outside timing; inside timing preview bodies are
forced with a character checksum, and warning/activity events are forced too.
The IORef state transition models the production projection callback.

Raw preview capacity is 65,536 characters; shell capacity is 4,096.
N=100/1,000/10,000/50,000 distinguishes below-cap, crossing-cap and saturated
behavior. Seven samples alternate old/new order; each sample batches
20/10/3/1 replays respectively. The 10,000-delta cases are repeated.
Reported CPU/wall/allocation are independent medians normalized per replay.
Explicit GCs before/after timing flush RTS allocation accounting; GCs during
replay remain timed. Raw CSVs are under `$TMPDIR/preview-saturation/run-*.csv`.
Machine: Linux x86_64, Core i9-9900K, GHC 9.10.3; no CPU isolation.

## Initial matrix

CPU ms / wall ms / allocated bytes, old → fast-path candidate:

| Tool / deltas | CPU | Wall | Bytes |
|---|---|---|---|
| raw / 100 | 0.0403 → 0.0414 | 0.0404 → 0.0416 | 193,080 → 193,080 |
| raw / 1,000 | 0.6159 → 0.6357 | 0.6185 → 0.6379 | 2,283,369 → 2,283,369 |
| raw / 10,000 | 8.999 → 8.866 | 9.027 → 8.884 | 24,943,813 → 22,251,133 |
| raw / 50,000 | 21.393 → 25.007 | 21.597 → 25.066 | 88,105,840 → 67,173,160 |
| shell / 100 | 0.5811 → 0.5839 | 0.5828 → 0.5859 | 3,583,211 → 3,583,211 |
| shell / 1,000 | 3.066 → 3.060 | 3.077 → 3.070 | 21,155,889 → 20,816,169 |
| shell / 10,000 | 5.895 → 5.574 | 5.915 → 5.591 | 41,052,928 → 36,609,208 |
| shell / 50,000 | 18.477 → 16.890 | 18.513 → 16.936 | 129,525,448 → 106,841,728 |
| raw / 10,000 repeat | 8.966 → 8.794 | 8.991 → 8.814 | 24,943,813 → 22,251,133 |
| shell / 10,000 repeat | 5.976 → 5.749 | 5.996 → 5.767 | 41,052,928 → 36,609,208 |

Repeating the whole matrix gave raw/50,000 CPU 21.384 → 18.797 ms
(wall 21.421 → 18.830) with the same allocation, so the initial large-case
slowdown was not stable. However raw/1,000 remained slower, 0.6247 → 0.6368
ms (wall 0.6259 → 0.6380), with unchanged allocation. The early saturation
guard was therefore redesigned to run only after the existing publish test.

## Retained implementation

The saturation guard now runs on the non-publishing branch. Two full matrices
showed no below-cap timing regression, with unchanged below-cap allocation.
The second matrix is reproduced here (CPU ms / wall ms / bytes, old → new):

| Tool / deltas | CPU | Wall | Bytes |
|---|---|---|---|
| raw / 100 | 0.0409 → 0.0404 | 0.0410 → 0.0405 | 193,080 → 193,080 |
| raw / 1,000 | 0.8340 → 0.8187 | 0.8361 → 0.8203 | 2,283,369 → 2,283,369 |
| raw / 10,000 | 12.515 → 12.136 | 12.539 → 12.165 | 24,943,813 → 23,715,573 |
| raw / 50,000 | 24.653 → 23.212 | 24.709 → 23.269 | 88,105,840 → 78,557,600 |
| shell / 100 | 0.6382 → 0.6242 | 0.6396 → 0.6258 | 3,583,211 → 3,583,211 |
| shell / 1,000 | 3.354 → 3.250 | 3.363 → 3.264 | 21,155,889 → 21,000,929 |
| shell / 10,000 | 6.166 → 5.842 | 6.181 → 5.857 | 41,052,928 → 39,025,968 |
| shell / 50,000 | 19.265 → 17.627 | 19.304 → 17.668 | 129,525,448 → 119,178,488 |
| raw / 10,000 repeat | 12.539 → 12.002 | 12.564 → 12.031 | 24,943,813 → 23,715,573 |
| shell / 10,000 repeat | 6.185 → 5.883 | 6.202 → 5.908 | 41,052,928 → 39,025,968 |

Across both matrices, saturated 10,000/50,000-delta cases used approximately
3–9% less CPU, and 5–11% fewer allocated bytes. This is a small projection
optimization, not evidence of faster network/token delivery. Absolute timings
changed with machine load; only paired old/new runs should be compared.
Below-cap improvements should be treated as noise/code-layout effects, not
an intentional optimization. All event-equivalence checks passed.
