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

## Compiled Nix launcher

The shell launcher generated by `makeWrapper` repeatedly performs Bash pattern
substitutions over `PATH`. A long development environment can therefore delay
even `--version`, before Haskell's startup timer begins. The CLI package now uses
`makeBinaryWrapper`, with one prefix operation per directory to preserve search
order and duplicate removal. The pinned helper also receives an empty-value
correction: an explicitly empty search path must not gain a trailing separator
that searches the current directory.

`nix/benchmarks/cli-wrapper.nix` applies both launchers to the **same existing
optimized executable**, using the production `nix/cli-wrapper.nix` function.
It checks resource defaults and overrides, empty/unset search paths, duplicate
directories, argument forwarding, output and exit status. The shell's inert
`/no-such-path` default is omitted by the compiled launcher when PATH is unset.
It does not rebuild the CLI or send a model request.

The fixture takes `pkgs`, `executable`, `syntaxDirectory`, `postgresBin`,
`runtimeTools` (package roots), `gstreamerPlugins` (package roots), and optional
`appleSessionTitle` / `mediaHelper` package roots. Copy those resource locations
from the baseline installed launcher into a JSON configuration file. The
`executable` must be its underlying `.agent-cli-wrapped`, not the shell launcher.

```sh
export CLI_LAUNCHER_CONFIGURATION="$TMPDIR/launcher-configuration.json"
comparison=$(nix build --impure --no-link --print-out-paths --expr '
  let
    project = builtins.getFlake (toString ./.);
    pkgs = project.inputs.nixpkgs.legacyPackages.${builtins.currentSystem};
    configuration = builtins.fromJSON
      (builtins.readFile (builtins.getEnv "CLI_LAUNCHER_CONFIGURATION"));
  in import ./nix/benchmarks/cli-wrapper.nix (configuration // { inherit pkgs; })
')
mkdir -p "$TMPDIR/cli-launcher-benchmark"
nix develop -c ghc -O2 -threaded -rtsopts scripts/BenchmarkCliLauncher.hs \
  -outputdir "$TMPDIR/cli-launcher-benchmark" \
  -o "$TMPDIR/cli-launcher-benchmark/benchmark-cli-launcher"
nix develop -c "$TMPDIR/cli-launcher-benchmark/benchmark-cli-launcher" "$comparison" \
  --samples 7 --warmups 1 --output "$TMPDIR/launcher-samples.json"
```

`scripts/BenchmarkCliLauncher.hs` also runs the fixture's environment and argument
checks with `--check CONFIGURATION`. The benchmark alternates shell/native order,
verifies identical version output,
and covers 1, 64, 128 and 256 synthetic Nix-style search directories, the inherited
environment, then a repeated 256-directory case. For interactive measurements,
use `scripts/benchmark-startup.py` with `$comparison/shell/bin/agent-cli` and
`$comparison/native/bin/agent-cli`, identical options, and alternating launches.
Resolve output paths before entering `nix develop`, which can replace `TMPDIR`.

Local measurements on 2026-10-16: Apple M3 Max, arm64 macOS 26.6.1, installed
revision `3b3b4ed`, optimized Nix production executable, GHC 9.10.3, unchanged
`-N4 -M8G` defaults, `GHCRTS=-M8G`. Seven samples per launcher after one warmup,
with no concurrent build. Median process-to-`--version` completion, milliseconds:

| PATH bytes | Shell | Compiled |
| ---: | ---: | ---: |
| 63 | 47.9 | 44.3 |
| 4,095 | 75.1 | 43.3 |
| 8,191 | 144.6 | 46.4 |
| 16,383 | 419.9 | 45.8 |
| 18,432 (inherited) | 494.6 | 46.7 |
| 16,383 (repeat) | 416.6 | 44.7 |

Interactive measurements used the inherited 18,432-byte PATH, existing
credentials and warm store, default skills, fullscreen, `--no-computer-use
--yolo`, no worktree creation or submitted turn. Seven alternating samples
after one warmup per launcher and workload. Median PTY observations, milliseconds:

| Workload | Launcher | First frame | Editable | Ready |
| --- | --- | ---: | ---: | ---: |
| This repository | Shell | 487.6 | 509.3 | 1,323.0 |
| This repository | Compiled | 47.0 | 71.8 | 897.9 |
| Empty directory | Shell | 490.0 | 513.2 | 1,299.9 |
| Empty directory | Compiled | 43.0 | 64.5 | 818.5 |

In this long-PATH, computer-use-disabled workload, repository startup reaches an
editable composer 437 ms sooner (86%) and full readiness 425 ms sooner (32%).
These percentages do not describe ordinary installed-CLI startup; the
[installed-build comparison](InstalledStartup.md) measures that separately.
The internal ready timer is essentially unchanged
(808 ms shell / 837 ms compiled in the repository), as expected for a change
before Haskell main. Network timing still contributes variation. The effect is
largest in long-path development environments; the short-path version workload
improves by about 4 ms. No allocation improvement is claimed.

A real tmux check confirmed that text entered before readiness survives into
the composer and a 120×40 → 100×30 resize, followed by exit status 0. A separate
comparison exposed an existing terminal-restoration failure: both launchers
leave canonical input disabled after Ctrl-D, even when it was enabled before
launch; echo is restored. Both run the same unmodified executable. That failure
is outside this launcher change and is not counted as a passing cleanup check.
The macOS CLI and Linux static CLI package definitions also evaluate successfully;
the Linux executable was not built or run on this macOS host.

## Paired tmux comparison

`scripts/BenchmarkCliStartup.hs` compares two optimized executables in alternating
order, through fresh 120×40 tmux terminals. It writes each sample and the final
medians to `OUTPUT/samples.jsonl`, retaining the final plain pane for each launch.
Warmup samples have negative indices and are excluded from medians. It never
sends Enter; prompt and managed-turn file options are rejected.

Use an unused output directory and a short, unused socket path. Resolve paths
before entering `nix develop`, which can replace `TMPDIR`:

```sh
export STARTUP_BENCHMARK_ROOT="$TMPDIR/startup-comparison"
export STARTUP_BENCHMARK_SOCKET="$TMPDIR/startup"
mkdir -p "$STARTUP_BENCHMARK_ROOT/build"
nix develop -c ghc -O2 -threaded -rtsopts -Wall scripts/BenchmarkCliStartup.hs \
  -outputdir "$STARTUP_BENCHMARK_ROOT/build" \
  -o "$STARTUP_BENCHMARK_ROOT/build/benchmark-cli-startup"
GOOGLE_API_KEY=startup-benchmark nix develop -c \
  "$STARTUP_BENCHMARK_ROOT/build/benchmark-cli-startup" \
  "$BEFORE" "$AFTER" "$STARTUP_BENCHMARK_SOCKET" "$PROJECT" \
  "$STARTUP_BENCHMARK_ROOT/results" --home "$BENCH_HOME" \
  --samples 7 --warmups 1 --timeout 45 -- \
  --provider gemini --model gemini-2.5-flash --no-computer-use --yolo
```

Use the dedicated, pre-provisioned HOME described above. `--home` also redirects
the XDG configuration, data, cache and state directories. To compare ordinary
installed-account startup, explicitly select `--current-home` instead and omit
the dummy credentials and provider/model overrides. Keep computer-use settings
representative of the scenario being measured. `--fullscreen` is always added.

The runner observes the editor's `❯` marker, injects unsubmitted `startup-probe`
text, and requires the standalone `startup: … · ready …` diagnostic. Its
`first_frame_ms` is the first observed editor marker, rather than the direct-PTY
script's cursor-restoration boundary. All external times include tmux process,
capture and polling overhead. Compare paired measurements from the same runner;
do not interpret differences between runners as application regressions.

Each sample clears the probe and requests exit with Ctrl-D, allowing three
seconds for normal shutdown before closing its private tmux session/server.
Retained captures may include session text: inspect them before publishing.
`--current-home` creates normal session metadata. Passing `--worktree` creates
real managed checkouts and may fetch upstream; remove those diagnostic checkouts
through the normal managed-worktree workflow after verifying their identity and
cleanliness. The benchmark does not delete session records or worktrees.

Run plain startup, representative resumed histories and worktree creation as
separate workloads, with fresh output directories. Record revision identities,
PATH size, model/provider, computer-use settings, store state and host load.
First model response latency requires a separate successful request; this runner
measures initialization only.
