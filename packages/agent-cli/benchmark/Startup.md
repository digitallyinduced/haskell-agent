# Interactive startup latency

Use `scripts/benchmark-startup.py` from `nix develop` with **compiled, optimized**
before/after binaries. The script launches a 120×40 PTY, never submits a model
request, records each sample as JSON, and reports medians after discarded warmups.
Do not substitute GHCi timings or compare an unrelated installed version.

It measures:

- `first_frame_ms`: launch to the fullscreen renderer restoring the visible
  cursor after its first frame.
- `editable_ms`: launch to rendering `startup-probe`, which is injected after the
  first frame once terminal echo is disabled. This is an observed upper bound on
  input readiness, including processing/rendering the probe, not just drawing an
  empty editor.
- `ready_ms`: launch to displaying the existing `startup: … ready …` diagnostic.
- `internal_ready_ms`: the diagnostic's own elapsed time, excluding process
  loading and early initialization before its timer starts.

External measurements use a monotonic clock immediately before process creation.
The renderer must be `--fullscreen`; it does not use the inline editor's
synchronized-output boundary. Timings are PTY-observed, not terminal-emulator
pixel presentation times. Inspect raw captures when renderer changes invalidate
the cursor/diagnostic markers; a missing milestone fails the run.

## Procedure

1. Save the base revision and build its executable with:

   ```
   nix develop -c cabal build --offline -O2 agent-cli:exe:agent-cli
   nix develop -c cabal list-bin -O2 agent-cli:exe:agent-cli
   ```

   Keep that executable and its data files available, then build the changed
   revision with the same options. If using separate worktrees, ensure dependency
   versions, RTS options and data-file lookup are equivalent.

2. Create a **dedicated, short-path HOME** with mode 0700. Do not copy real OAuth
   tokens or benchmark against your daily session database. A dummy Gemini key
   and an explicit model suffice for a no-request startup. Initialize this HOME
   once before measuring warm startup; PostgreSQL cold provisioning is a separate
   workload. Run from `nix develop` so PostgreSQL and other runtime tools are
   available.

   ```
   export GOOGLE_API_KEY=startup-benchmark
   python3 scripts/benchmark-startup.py \
     --home "$BENCH_HOME" --cwd "$PROJECT" \
     --samples 7 --warmups 1 --output-dir "$RESULTS/before" \
     -- "$BEFORE" --provider gemini --model gemini-2.5-flash \
       --fullscreen --no-computer-use --yolo
   ```

   Repeat with `$AFTER`, identical HOME/project/options/environment, then repeat
   the before case to check stability. The script terminates and joins the agent
   after the milestones; it never sends Enter. A separately daemonized managed
   PostgreSQL instance intentionally remains warm between samples. Stop it with
   `HOME="$BENCH_HOME" "$AFTER" storage stop` when done.
   To measure restarting an already-provisioned database, add `--cold-store`:
   this invokes that binary's `storage stop` against the dedicated HOME before
   each sample, outside the timed interval. It does not delete/reinitialize data.

3. Cover more than one workload: empty directory, representative repository with
   skills/instructions, and a resumed session with representative history sizes.
   Measure `--worktree` separately because creating a checkout is real extra
   work. Do not mix those results with ordinary no-worktree launches.

4. Keep the raw sample JSON and record hardware, GHC version, optimization/RTS
   settings, revision identities, workload dimensions and medians. Avoid running
   builds concurrently with timed samples. For allocation/GC optimizations, add
   allocation measurements rather than treating these latency-only results as
   allocation evidence.

5. Separately smoke-test the changed executable in **tmux**. Confirm that text
   typed during loading survives into the idle composer, resize/redraw remains
   sound, and startup failure/cancellation restores the terminal. A PTY
   benchmark alone is not the required tmux visual check.

## Early-shell change: local measurement

Baseline `d2df3d9f66fe080eb0c1bffb20bafcb216e4835d` versus the early-shell
`Flow.hs` / workspace-timing `Initialized.hs` changes, built from the same
source archive with GHC 9.10.3, Cabal `-O2`, unchanged compiled RTS defaults and
`GHCRTS` unset. Apple M3 Max, 36 GiB RAM, arm64, macOS 26.6.1. Seven measured
samples after one warmup per case; before → after → before-repeat order.
The after cases were subsequently repeated with the corrected ready parser;
those corrected runs are shown below.
Workload: this repository, default skills enabled, dedicated pre-provisioned
HOME, dummy Gemini credentials, fullscreen, `--no-computer-use --yolo`, no turn.

Median launch-to-observation times, milliseconds:

| Store state | Binary | First frame | Editable | Rendered ready |
| --- | --- | ---: | ---: | ---: |
| Running | Before | 36.5 | 59.9 | 117.7 |
| Running | After | 31.0 | 52.1 | 127.1 |
| Running | Before repeat | 39.1 | 70.7 | 125.6 |
| Restarted each launch | Before | 193.3 | 211.9 | 273.8 |
| Restarted each launch | After | 29.6 | 51.0 | 269.2 |
| Restarted each launch | Before repeat | 194.1 | 219.8 | 275.5 |

The clear improvement is an editable UI while storage starts: about **161 ms /
76% less waiting** in the database-restart case. Total readiness did not
meaningfully improve. Warm startup's rendered-ready median increased by 9 ms
versus the first baseline (2 ms versus the repeat), so do not claim a
total-startup speedup from these results. The original parser matched an
intermediate “workspace scopes ready” checkpoint, invalidating both its
rendered-ready and internal-ready values. The corrected runs require the
standalone ready checkpoint; internal-ready medians were 47 ms warm and
191 ms with the database restarted.
These are local no-request measurements, not first-install provisioning,
authenticated production-account, resumed-history, or Grok Build comparisons.

Modified-binary tmux checks also passed: text entered during “Opening session
storage…” survived into the idle composer and a 120×40 → 100×30 resize.
An unknown resume UUID displayed the startup-failure dialog; selecting Exit
restored canonical input and echo. Double Ctrl-C during “Opening session
storage…” also exited and restored those terminal settings. No turn was sent.
