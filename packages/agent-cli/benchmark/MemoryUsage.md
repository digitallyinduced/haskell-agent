# agent-cli memory investigation

Memory measurements must distinguish bytes allocated (GC traffic), live heap
after collection, RTS heap capacity, and operating-system peak RSS. Reducing one
does not necessarily reduce the others. These fixtures exercise production
code with synthetic coding transcripts and read-only saved-session replays,
not a connected end-to-end model session.

## Outcome

**The 20% target is met for large-draft offline session replays, not generally
for the connected CLI.** Compact undo preserves 200 undo steps without keeping
200 complete drafts. Three alternating process runs per version show peak RSS
falling 24.0–24.2% when editing 256 KiB drafts and 48.5% for 1 MiB drafts, with
the full current UI and resident conversation retained. No-edit and short-prompt
controls are essentially unchanged. Allocation traffic is unchanged and process
CPU is about 0.7–1.8% higher for those large-draft cases. See
[SessionReplay.md](SessionReplay.md) for methodology, results and undo tradeoffs.

The other production change copies cached agent preview text at its ownership
boundary. In the controlled single-capability, eight-child, 64 KiB-message
fixture, post-GC live heap falls from 1,075,360 to 27,040 bytes; normal multiword
previews are unchanged. See [ViewportRetention.md](ViewportRetention.md) for
allocation costs and the variability observed with four capabilities.

The global ASCII shortcut is retained only as a benchmark experiment: it cuts
first-render allocations but increases retained heap and can increase peak RSS.
Removing nested transcript caches was also rejected: interaction live heap rose
about 73%, with cold redraw allocation up 32%. See the
[renderer experiment](../../../docs/agent-cli-transcript-memory-experiment.md).

Validation: viewport runtime Hspec 4/4; terminal text Hspec 14/14; composer
Hspec 60/60; compact undo Hspec 10/10. Optimized benchmark builds passed.
Saved-session replay uses normal `-N4` and retains full UI/conversation state;
170 styled frames matched exactly, and 200-step undo restoration checks passed.
A final live tmux CLI smoke covered Unicode input, middle edit, Ctrl-_ undo,
draft clearing, local `/help`, and `/quit`, without submitting a prompt. Emoji
displayed as a replacement glyph in the capture; emoji glyph support was not
verified. The full repository test suite was not run.
A broader connected-service measurement remains necessary before claiming a
general whole-CLI saving.

## Hotspots

* **Composer undo retention:** retaining complete snapshots multiplies large
  pasted drafts by up to 200. Compact inverse splices remove this duplication
  without reducing undo depth. Editing/rendering still allocates heavily.
* **Agent preview ownership:** a short, single-word `Text` preview can share the
  backing array of a large source message even after a child transcript becomes
  cold. Copying the cached title/detail gives the preview its own small buffer.
  See `ViewportRetention.hs` and its accompanying report.
* **Terminal sanitation:** `displayTerminalText` segments text into graphemes and
  concatenates sanitized clusters. Ordinary ASCII takes this expensive path
  unnecessarily. Allocation savings must be checked against retained slices and
  changed GC behavior; see `../../agent-tui/benchmark/TerminalText.md`.
* **Transcript rendering:** cached Brick results retain border maps, image trees,
  and unevaluated closures. Caching both chunks and their constituent blocks
  is a candidate for reduction, but removing inner caches can penalize the first
  selection or hover that bypasses a chunk cache.

An optimized baseline heap-type profile of
`history-measured-viewport-trace 600 30 3 +RTS -N4 -T -hT -i0.02`
sampled a peak of 39,738,416 bytes. Major categories at that sample:

| Heap category | Bytes |
| --- | ---: |
| `THUNK_2_0` | 8,496,672 |
| `THUNK` | 7,294,576 |
| Brick `Edges` | 4,709,840 |
| Brick `BorderMap` | 2,661,192 |
| `THUNK_1_0` | 2,129,640 |
| Vty `VertJoin` | 2,115,160 |
| `ARR_WORDS` | 1,523,408 |
| Vty `HorizText` | 1,414,880 |

This is a sampled heap-type breakdown, not allocation-site attribution or RSS.
Profiling changes GC behavior; use unprofiled runs for comparisons.
This exploratory profile used the earlier empty attribute map; final comparative
renderer runs used identical terminal-theme settings on both variants.

## Reproduction and safeguards

Build baseline and candidate with identical settings:

```sh
nix develop -c cabal build agent-cli:bench:transcript-scrolling-bench \
  --enable-optimization=2 --enable-executable-dynamic -j4
```

Freeze both the executable **and its locally built shared libraries** before
changing source. A copied executable whose libraries still point into
`dist-newstyle` is not a frozen baseline. Verify with `ldd` and use separate
`LD_LIBRARY_PATH` wrappers for the two versions.

```sh
nix develop -c python3 packages/agent-cli/benchmark/compare-memory.py \
  /path/to/baseline/run /path/to/candidate/run "$TMPDIR/memory-results.json" 5
```

The Linux runner alternates independent processes and preserves CPU/wall time,
peak RSS, benchmark output, and RTS statistics. The renderer benchmark pins its
final render state across GC so retained cache memory is counted. Compare the
same current-renderer workload on both revisions, not historical `old`/`new`
algorithms already present before this change. Its `verify` mode emits actual
rendered text, attributes, dimensions, and extents for output-equivalence checks.

Fixtures include short/long histories, code-heavy bodies, resizing/scrolling,
and selection/hover transitions, including a fresh-process interaction sample.
Whole-process peak RSS includes fixture construction; it is not idle CLI RSS.

## Final transcript-cache experiment decision

**Rejected and reverted:** omitting inner block/code caches inside completed
cached chunks. Three alternating process repeats across eight workloads used
identical finalized themed harnesses and frozen local dynamic libraries.
Styled text, dimensions, and extents matched exactly for static, resize/scroll,
and selection/code-copy hover traces.

Static savings were small (1200 blocks × 3 lines: live heap 29.99 → 29.59 MB).
The 100 × 30-line cold interaction trace instead increased retained heap
6.39 → 11.05 MB (+73%), peak RSS 7.3%, allocation 376.62 → 496.23 MB,
and 25-redraw CPU 139.86 → 210.46 ms. Interaction fallback renders new inner
results while the outer cached image remains retained, losing existing sharing.
Production block/chunk caching is unchanged.

The separately isolated ASCII fast path reduced first-render allocation 38–50%
in these transcript fixtures, but did not reduce RSS and slightly increased
retained heap. This is not evidence of a 20% whole-CLI memory reduction.
The ASCII production change was also deferred after broader renderer testing.

See the [full transcript experiment report](../../../docs/agent-cli-transcript-memory-experiment.md)
for tables, method, verification, and reproduction instructions. The exploratory
heap-type profile above used the earlier empty attribute map; it identifies heap
categories but is not the finalized themed before/after comparison.
