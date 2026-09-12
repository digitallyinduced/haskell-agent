# Viewport preview retention diagnostic

`ViewportRetention.hs` exercises the actual `AgentViewportRuntime` cache, with
synthetic completed-child transcripts. It is not a whole-CLI RSS benchmark.

```sh
nix develop -c cabal run agent-cli:bench:viewport-retention-bench -- \
  full-single 8 50 65536 5 +RTS -T -N4
```

Arguments: mode, children, messages per child, payload bytes per message, samples.
CSV columns: mode, children, messages, payload bytes, median wall milliseconds,
median CPU milliseconds, median allocated bytes, median post-GC live bytes,
checksum summed across samples. Timings include the final collections; source
construction is outside the measured interval.

- `full` consumes all preview titles/details, matching fullscreen mailbox size
  accounting. `visible` consumes only the first two steps.
- `full-single` / `visible-single` use single-word preview lines. This matters:
  `Text.unwords . Text.words` can return a slice for a singleton word, retaining
  the entire original message array.
- Sources are forced before measurement, held in a mutable map, and cleared
  after consuming the snapshot. Returned snapshots are not retained.
- A bracketed `StablePtr` keeps the runtime/cache alive across two final major
  collections. This prevents a misleading result from collecting the cache too.

## Ownership fix and measured results

The runtime now copies cached preview titles/details at their ownership boundary.
No transcript content is dropped. There is no speculative eager-list traversal.

GHC 9.10.3, optimized dynamic build, five-sample medians, eight children and fifty
messages each. Baseline and fixed binaries use the same StablePtr harness and
dependencies; only `AgentViewport/Runtime.hs` differs.

The measured run used this direct build after Cabal had built the dependencies
in `dist-newstyle`. `BASELINE` must identify the revision before the ownership
fix. All build artifacts and the baseline source override remain temporary:

```sh
export VIEWPORT_BUILD="$TMPDIR/viewport-retention-build"
mkdir -p "$VIEWPORT_BUILD/baseline-src/Agent/CLI/AgentViewport"
git show "$BASELINE:packages/agent-cli/src/Agent/CLI/AgentViewport/Runtime.hs" \
  > "$VIEWPORT_BUILD/baseline-src/Agent/CLI/AgentViewport/Runtime.hs"
export VIEWPORT_PACKAGE_FLAGS="$(jq -r \
  '."install-plan"[] | select(.id == "agent-cli-0.1.0.0-inplace") |
   .depends[] | "-package-id " + .' dist-newstyle/cache/plan.json)"
nix develop -c bash -c '
  flags="-O2 -rtsopts -threaded -dynamic -XGHC2021 -XBlockArguments
    -XOverloadedStrings -XOverloadedRecordDot -XDuplicateRecordFields
    -XNoFieldSelectors -XLambdaCase -XRecordWildCards -XNamedFieldPuns
    -XTypeApplications -XScopedTypeVariables -hide-all-packages
    -package-db $HOME/.local/state/cabal/store/ghc-9.10.3-394c/package.db
    -package-db dist-newstyle/packagedb/ghc-9.10.3"
  for version in before after; do
    override=""
    if [ "$version" = before ]; then
      override="-i$VIEWPORT_BUILD/baseline-src"
    fi
    ghc --make $flags $VIEWPORT_PACKAGE_FLAGS $override \
      -ipackages/agent-cli/src -outputdir "$VIEWPORT_BUILD" \
      -o "$VIEWPORT_BUILD/$version" \
      packages/agent-cli/benchmark/ViewportRetention.hs || exit
  done
  for caps in 1 4; do
    for mode in full full-single; do
      for bytes in 4096 65536 1048576; do
        for version in before after; do
          echo "$version N$caps"
          "$VIEWPORT_BUILD/$version" "$mode" 8 50 "$bytes" 5 +RTS -T -N$caps
        done
      done
    done
  done
'
```

Adjust the compiler-specific package database paths for another compiler.
Exporting `VIEWPORT_BUILD` before entering Nix is intentional: the development
shell can change `TMPDIR`. Dynamic linking avoids this environment's static
linker's argument-list limit.

| Mode | Capabilities | Payload | Before live bytes | After live bytes |
|---|---:|---:|---:|---:|
| full-single | 1 | 4 KiB | 92,320 | 27,040 |
| full-single | 1 | 64 KiB | 1,075,360 | 27,040 |
| full-single | 1 | 1 MiB | 16,804,000 | 27,040 |
| full-single | 4 | 4 KiB | 325,512 | 57,120 |
| full-single | 4 | 64 KiB | 4,257,672 | 63,672 |
| full-single | 4 | 1 MiB | 16,835,448 | 57,120 |
| full | 1 | all three sizes | 27,520 | 27,520 |
| full | 4 | 4 KiB | 57,616 | 59,184 |
| full | 4 | 64 KiB | 55,144 | 55,120 |
| full | 4 | 1 MiB | 58,936 | 55,144 |

At one capability, single-word allocation rose from 78,312 to 82,768 bytes
(5.7%); multiword allocation rose from 94,016 to 100,144 bytes (6.5%).
Checksums match before/after (1,200 single-word; 3,760 multiword).
Single-word one-capability wall medians were 0.530→0.589 ms, 1.459→1.628 ms,
and 14.575→14.355 ms. These short runs do not establish a latency improvement.

Four-capability retention is scheduling-sensitive: earlier versions of the
harness keeping the cache alive through a final reset rather than a StablePtr
showed additional whole-transcript retention, sometimes worse after the fix.
A final bounded repeat of the identical StablePtr harness at 64 KiB also showed
this variability: `-N4` measured 4,262,648→3,337,976 live bytes, whereas `-N1`
reproduced 1,075,360→27,040 exactly. The cause was not established. Do not infer
that every byte of the four-capability difference is directly attributable to
Text slices, or that the fix removes all concurrent retention.

The controlled one-capability results establish the slice ownership problem.
They **do not establish a 20% reduction in real-world whole-CLI memory**.
That requires replaying representative sessions, retaining the live application
state, and measuring process RSS and heap residency with normal runtime settings.
