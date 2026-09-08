# Worktree recovery preparation benchmark

`WorktreeCollection.hs` compares the old full-snapshot collection path against
clean-checkout recovery using existing Git objects. It keeps the old path as a
baseline. Each fixture is a linked checkout with a committed feature merged into
its primary checkout, containing the requested number of 1,024-byte tracked
files.

The measured old path runs snapshot preflight, snapshot creation (which captures
the checkout twice), and final snapshot verification (a third capture). The new
path runs the five clean inspections used by collection, including the inspection
inside recovery preservation. Both paths prepare recovery without removing the
fixture. Fixture construction and destruction are outside measurement.

This is **not an end-to-end GC benchmark**: saved-session discovery, leases,
incorporation proofs, directory size estimation, and checkout removal are not
measured. Git subprocess time is included in elapsed time. CPU and allocation
columns cover the Haskell parent only, not Git subprocess CPU or allocation.

Each sample forces a checksum of the returned recovery descriptor. GC runs before
each sample and after the elapsed/CPU interval so the allocation count includes
the last partial nursery. Sample order alternates between implementations; each
column reports its median.

## Build

From the repository root, compile the narrow dependency graph with optimization
inside the repository development shell:

```sh
export BENCHMARK_OUTPUT="$TMPDIR/worktree-collection-build"
mkdir -p "$BENCHMARK_OUTPUT"
nix develop -c ghc \
  -O2 -threaded -rtsopts \
  -XGHC2021 -XBlockArguments -XOverloadedStrings \
  -XOverloadedRecordDot -XNoFieldSelectors -XLambdaCase \
  -ipackages/agent-cli/src -ipackages/agent-core/src \
  -outputdir "$BENCHMARK_OUTPUT" \
  packages/agent-cli/benchmark/WorktreeCollection.hs \
  -o "$BENCHMARK_OUTPUT/worktree-collection-bench"
```

## Run

Use a controlled environment for both implementations. A development shell's
large build environment otherwise becomes part of every `getEnvironment` and
subprocess invocation, disproportionately inflating allocation in the old
one-process-per-file implementation. The Git executable still comes from Nix.

```sh
nix develop -c sh -c '
  env -i \
    PATH="$(dirname "$(command -v git)"):/usr/bin:/bin" \
    TMPDIR="$TMPDIR" HOME="$HOME" LANG=en_US.UTF-8 \
    "$BENCHMARK_OUTPUT/worktree-collection-bench" +RTS -T
'
```

With no positional arguments, the benchmark runs 100, 1,000, and 3,000 files with
three samples each. Pass `FILES SAMPLES` before `+RTS` for a particular case.
Repeat at least one case in a separate invocation to check stability.

## Recorded results (2026-09-08)

Apple Silicon macOS, GHC 9.10.3, `-O2 -threaded -rtsopts`, one RTS capability,
`+RTS -T`, controlled environment as above. All rows are medians of three
samples. Allocated bytes and CPU are **parent-process measurements**.

| Files | Implementation | Elapsed ms | Parent CPU ms | Parent allocated bytes |
| ---: | --- | ---: | ---: | ---: |
| 100 | Old snapshot | 9,668.189 | 688.825 | 241,265,440 |
| 100 | New clean | 608.060 | 40.928 | 44,661,584 |
| 1,000 | Old snapshot | 98,508.686 | 6,700.591 | 2,301,874,480 |
| 1,000 | New clean | 940.249 | 94.700 | 282,592,184 |
| 3,000 | New clean | 1,696.219 | 206.774 | 823,954,736 |
| 100, repeat | Old snapshot | 10,004.516 | 668.768 | 241,266,072 |
| 100, repeat | New clean | 636.439 | 42.366 | 44,498,808 |
| 1,000, repeat | New clean | 949.621 | 95.615 | 282,632,936 |
| 100, final FETCH_HEAD support | Old snapshot | 10,276.352 | 687.667 | 241,266,840 |
| 100, final FETCH_HEAD support | New clean | 632.616 | 43.874 | 44,630,040 |

The initial mixed matrix completed the 100- and 1,000-file baselines before its
tool session became unavailable during the 3,000-file case. No 3,000-file old-path
result is claimed. After final safety checks were added to the clean
implementation, it was recompiled and measured separately using:

```text
new-clean 100 3
new-clean 1000 3
new-clean 3000 3
new-clean 1000 3
```

The final 100-file repetition used the mixed `100 3` invocation with alternating
order and the final implementation. It confirms approximately 15.7× lower
recovery-preparation elapsed time and 81.6% less parent allocation. At 1,000
files, the completed baseline versus final clean measurement is approximately
105× faster with 87.7% less parent allocation. These are not whole-GC speedups.
The final 1,000-file clean repetitions differ by about 1% in elapsed time.

After adding optional `FETCH_HEAD` preservation and extracting ignored-directory
validation, the final implementation was rebuilt and the alternating `100 3`
case repeated once more. It measured 10.276 seconds versus 0.633 seconds (16.2×),
with 81.5% less parent allocation. That fixture has no `FETCH_HEAD`, so it covers
the added absence check rather than the cost of preserving populated fetch
metadata.

The remaining clean implementation still allocates proportionally to tracked
file count because its safety checks enumerate the index and attributes several
times. Large repository histories, ignored-directory size estimation, and
checkout removal remain separate performance boundaries not assessed here.
