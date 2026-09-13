# Command history: String versus Text

Measured 2026-09-12 on ARM64 macOS, Nix GHC 9.10.3, optimized `-O2`,
`+RTS -T -N4`. Synthetic newest-first entries contain an integer followed by
60 ASCII characters. Each row is the median of five samples. Fixture creation
and copying are outside measurement; reads and appends use the real history code.

| Entries | Operation | Elapsed ms | CPU ms | Allocated bytes | Retained live bytes |
| ---: | --- | ---: | ---: | ---: | ---: |
| 1,000 | Original read | 3.718 | 3.759 | 11,248,720 | 1,658,872 |
| 1,000 | Text read | 0.349 | 0.370 | 386,168 | 112,344 |
| 9,000 | Original read | 52.391 | 55.413 | 102,488,032 | 15,185,056 |
| 9,000 | Text read | 1.912 | 1.909 | 2,318,792 | 1,080,344 |
| 18,000 | Original read | 111.547 | 119.387 | 206,478,784 | 30,669,032 |
| 18,000 | Text read | 4.092 | 4.126 | 4,510,328 | 2,177,344 |
| 1,000 | Original append | 5.315 | 5.480 | 14,561,312 | — |
| 1,000 | Text append | 0.495 | 0.492 | 616,520 | — |
| 9,000 | Original append | 52.438 | 59.953 | 131,945,040 | — |
| 9,000 | Text append | 2.910 | 2.516 | 4,165,144 | — |
| 18,000 | Original append | 119.979 | 130.918 | 265,797,488 | — |
| 18,000 | Text append | 6.821 | 5.284 | 8,190,816 | — |

A second independent 9,000-entry run produced 51.375/1.930 ms original/Text
reads and 55.082/2.744 ms appends, with identical retained read heap.
After the final caller-controlled whitespace adjustment, a fresh build and
repeat inside `nix develop` confirmed 50.633/1.951 ms reads and 55.266/2.809 ms
appends at 9,000 entries (five-sample medians). Read allocation was
102,488,544/2,342,776 bytes and retained heap remained identical. A second
repeat gave 52.040/1.942 ms reads and 54.877/2.704 ms appends. The 1,000- and
18,000-entry repeats also improved both CPU time and allocation.
At that size, retained heap drops 92.9% (14.48 to 1.03 MiB), and read allocation
drops 97.7%. This is not a measurement of whole-application physical footprint.

Original read retains both Haskeline's String history and a fully traversed Text
view, modeling history navigation rather than untouched idle startup (where the
old Text view was lazy). New read includes file-lock overhead. Both append
variants reread under the private lock and rewrite the same file; this models
the old fullscreen append, not the inline editor's former stale-snapshot save.
Live bytes are major-GC deltas with results retained through an IORef;
append returns no history, so its small negative GC deltas are not useful.
Timing excludes the explicit post-operation major collection; allocation deltas
include it. OS caches and concurrent system load can affect timing.

## Reproduce

From the repository root, using session temporary storage:

```sh
mkdir -p "$TMPDIR/command-history"
nix develop -c ghc -O2 -Wall -threaded -rtsopts \
  -XGHC2021 -XBlockArguments -XOverloadedStrings -XOverloadedRecordDot \
  -XNoFieldSelectors -XDuplicateRecordFields -XLambdaCase \
  -ipackages/agent-cli/src -ipackages/agent-cli-runtime/src -ipackages/agent-core/src \
  -outputdir "$TMPDIR/command-history/objects" \
  packages/agent-cli/benchmark/CommandHistory.hs \
  -o "$TMPDIR/command-history/benchmark"
for n in 1000 9000 18000 9000; do
  for op in original-read compact-read original-append compact-append; do
    nix develop -c "$TMPDIR/command-history/benchmark" "$op" "$n" 60 5 +RTS -T -N4
  done
done
```

The Cabal benchmark is registered as `command-history-bench`. Direct compilation
was initially used because the offline Cabal build lacked a hermes checkout.
Six focused GHCi tests pass: Haskeline format compatibility, malformed UTF-8,
byte-identical writes, missing files/caller-controlled whitespace, duplicates, and concurrent appends
with private permissions. After fetching the dependency, all 251 CLI library
modules loaded successfully with `nix develop -c cabal repl agent-cli:lib:agent-cli`.
The source-loaded inline editor was exercised in tmux: Unicode submission and
Up-arrow recall both preserved `history café`. Full-agent startup in the isolated
test home was blocked by macOS's PostgreSQL Unix-socket path length limit.
