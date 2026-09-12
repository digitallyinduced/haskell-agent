# Full-state session replay

`SessionReplay.hs` measures the offline CLI UI and conversation ownership, not
the entire connected CLI service stack. It retains the current `AppState`,
Brick render state/caches, and `ConversationStore` through a stable pointer to
an IORef. Previous frames and the decoded `SessionTurn` list are not extra roots.

Saved JSONL is decoded read-only with the production decoder and projected with
`sessionHistoryPage`/`resetHistoryPage`, including normal history budgets and
the most recent generation. No provider requests, MCP services, database writes,
or external messages are issued. Synthetic fixtures use distinct coding turns,
tool output, fenced code, and Unicode. `resident` keeps active conversation
context; `idle` keeps only a reload checkpoint, like a cold conversation.

## Reproduction

Build both revisions with the same optimized settings:

```sh
nix develop -c cabal build agent-cli:bench:session-replay-bench \
  --enable-optimization=2 --enable-executable-dynamic -j4
```

Freeze the executable **and all locally built shared libraries** before building
the other revision. Copying just a dynamically linked executable can silently
compare both binaries against the candidate library.

Example arguments (append `+RTS -N4 -T -s -RTS`):

```text
fixture static resident 40 10 1
fixture resize idle 40 10 1
file static resident /path/to/private-transcript.jsonl 1
file paste-1048576 resident /path/to/private-transcript.jsonl 1
file paste-middle-262144 resident /path/to/private-transcript.jsonl 1
```

Pass argument arrays in a JSON file to the process-isolated Linux runner:

```sh
nix develop -c python3 packages/agent-cli/benchmark/compare-session-memory.py \
  "$BASELINE/run" "$CANDIDATE/run" "$WORKLOADS" "$RESULTS" 5
```

It alternates old/new order, uses `-N4`, records each process's peak RSS with
`wait4`, and preserves CPU, elapsed time, allocation, live heap, and RTS capacity.
Use one internal sample per process to avoid earlier samples affecting peak RSS.
Do not compare an isolated undo percentage with whole-process memory.

Paste traces seed a diagnostic draft without clipboard/file/image loading, then
send 200 printable keys through the production composer handler, redrawing every
10 keys. They keep the complete 200-entry undo history during the final 25 frames.
`paste-middle-BYTES` edits at the midpoint rather than the end. This models editing
large pasted diagnostics; it is not evidence that typical short prompts save as
much memory. Verification additionally undoes all 200 keys and checks the exact
original text and cursor, outside the memory comparison.

Composer events run through Brick's public startup hook with an inert Vty,
stopping before its terminal loop. This adds the same short-lived input-thread
adapter overhead to both versions; it does not measure physical terminal input.
The `resize` trace changes rendering dimensions, not scroll positions. Use the
separate transcript-scrolling benchmark for actual viewport scrolling.

The optional `verify` prefix prints attributed screen characters and extents for
equivalence checking. **Use synthetic fixtures only for public verification
logs:** verification of private transcripts exposes their contents.

## Saved-session results (2026-09-12)

Three alternating fresh-process runs per version, GHC 9.10.3, optimized dynamic
builds, `+RTS -N4 -T -s`, identical harness and independently frozen libraries.
The only production difference in this comparison is compact undo; both versions
already include the preview ownership fix. Private saved inputs contain 66 turns
(6,880,071 bytes, large) and two turns (2,200,967 bytes, medium). Draft editing is
a scripted diagnostic-paste scenario, not a recorded user's keystrokes.
MB below means decimal megabytes. RSS includes process startup and setup.

| Workload | Peak RSS old → new, MB | RSS reduction | Final live heap old → new, MB |
| --- | ---: | ---: | ---: |
| Large, resident, no edits | 268.56 → 268.79 | -0.1% | 27.71 → 27.71 |
| Large, idle, resize | 246.44 → 247.78 | -0.5% | 17.96 → 17.96 |
| Medium, resident, no edits | 202.83 → 203.19 | -0.2% | 8.04 → 8.04 |
| Large, 1 KiB draft + 200 edits | 268.01 → 267.11 | 0.3% | 27.96 → 27.72 |
| Large, 64 KiB draft + 200 edits | 294.75 → 269.21 | 8.7% | 40.93 → 27.85 |
| Large, 256 KiB draft + 200 end edits | 359.39 → 273.09 | **24.0%** | 80.46 → 28.25 |
| Large, 256 KiB draft + 200 middle edits | 361.17 → 273.86 | **24.2%** | 80.46 → 28.25 |
| Large, 1 MiB draft + 200 edits | 546.66 → 281.54 | **48.5%** | 238.56 → 29.82 |

The 256 KiB end-edit RSS ranges were 359.16–360.56 MB old versus
270.61–273.72 MB new; middle-edit ranges were 360.44–362.39 versus
273.10–274.13 MB. For 1 MiB: 545.51–549.54 versus 277.82–283.77 MB.
Thus the large-draft reductions are well outside run-to-run variation.

Total process CPU for those three cases was respectively 5.55 → 5.65 s,
3.58 → 3.60 s, and 17.57 → 17.76 s (about 0.7–1.8% higher). Total allocation
was effectively unchanged: 32.8005 → 32.8009 GB, 17.9293 → 17.9298 GB,
and 122.7453 → 122.7458 GB. This is a **retention improvement**, not a fix
for the substantial allocation traffic in editing/rendering large drafts.

**The 20% target is met for these large-draft offline replay cases only.**
Short prompts and no-edit controls do not meet it. Provider workers, connected
MCP services and the database runtime are absent, so a general 20% reduction
for connected CLI sessions remains unproven.

Verification used synthetic static, resize, interaction, 1 KiB end-edit and
256 KiB middle-edit fixtures: attributed characters, dimensions and extents
matched exactly, and both editing traces restored the original draft/cursor
after all 200 real composer undo events.

## Implementation and component measurements

Previously, `appUndo` retained up to 200 complete draft snapshots. After a large
paste, each subsequent insertion allocates another almost-identical draft and
keeps it alive. The replacement keeps one absolute snapshot at the head and
copied inverse-splice fragments for older entries. The 200-step limit and cursor
semantics are unchanged. An absolute head is important: history navigation and
other draft replacements can occur without recording undo entries.

Splices use UTF-8 byte offsets internally, adjusted to code-point boundaries;
editor cursors remain character offsets. Bulk comparisons avoid scanning long
unchanged regions through allocating character iterators. The chain spine and
copied fragments are evaluated promptly so old drafts are actually released.

Focused `-O2 -N4` measurements (three process samples, 200 edits) found:

| Draft / edits | Push CPU old → new | Retained heap old → new |
| --- | ---: | ---: |
| 1 MiB ASCII, append | 110.11 → 61.95 ms | 210.86 → 2.19 MB |
| 1 MiB ASCII, middle | 138.13 → 91.92 ms | 210.86 → 2.19 MB |
| 4 MiB Unicode, append | 623.41 → 430.79 ms | 843.15 → 8.48 MB |

These are **component tests, not whole-session results**. Push allocations were
essentially unchanged (about 0.51 MB additional over 200 edits). Undoing a middle
edit now reconstructs a draft instead of selecting an existing full snapshot:
the 200-undo test allocated 208.81 MB versus 1.23 MB and took 32.62 versus
26.07 ms. Ten regression tests cover exhaustive short Unicode pairs, sliced
backing arrays, partial code-point matches, chunk boundaries, history limits,
and branching after undo.

Final validation also passed all 60 existing composer Hspec examples and exact
comparison of 170 styled replay frames. Live tmux testing exercised Unicode
input, a middle edit and Ctrl-_ undo, clearing the draft, `/help`, and `/quit`
without submitting a prompt. Accented Latin, Greek, and CJK rendered correctly;
emoji appeared as a replacement glyph, so emoji glyph support is unverified.
The full repository test suite was not run.
