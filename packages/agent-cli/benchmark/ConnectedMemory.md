# Connected CLI memory fixture

`connected-memory-fixture.py` runs a bounded loopback Responses server and
prepares a fresh isolated CLI home. `connected-memory-run.py` drives the actual
optimized CLI through tmux, sampling its process RSS during startup, tool rounds,
streaming text, idle, and graceful exit. No external model/provider is contacted.
This is a full-runtime synthetic fixture, **not evidence about every connected
real-world session**. MCP/provider inference and non-fixture integrations are
not represented.

Build the executable with identical optimized settings for baseline/candidate.
Freeze the executable **and local shared libraries** before rebuilding.
Run the scripts with Python 3.9+ inside `nix develop` so required tool/data paths
are available. `TMPDIR` must be set; all generated artifacts stay beneath it.
The server takes a **new** run directory and rejects existing directories.
It also rejects roots too long for PostgreSQL and observation Unix sockets.
On Linux use `--proc-home` when `TMPDIR` is long. This opens the TMPDIR-owned
directory and uses `/proc/<fixture-pid>/fd/<fd>` as a short alias: no scratch
directory is created outside TMPDIR. The fixture owns the fd until shutdown,
stops only its exact isolated PostgreSQL cluster with `pg_ctl`, then closes it.
Keep the fixture alive through CLI/resume; the alias is not a durable home after
the fixture exits. Without Linux procfs, use a short tool-provided TMPDIR.
When driving from another Nix shell, use the original fixture `TMPDIR`, not
the new invocation's different temporary directory.

```sh
# For a Cabal build without installed data files, from the repository root:
export agent_runtime_datadir="$PWD/packages/agent-runtime"
export agent_cli_datadir="$PWD/packages/agent-cli"
export agent_tui_datadir="$PWD/packages/agent-tui"
python3 packages/agent-cli/benchmark/connected-memory-fixture.py \
  --root "$TMPDIR/cm1" --cli /absolute/path/to/frozen/run \
  --rounds 4 --argument-padding 0 --text-bytes 4096 \
  --postgres-port 55439 --proc-home
```

Keep this server running; after `ready.json` exists, in another command:

```sh
python3 packages/agent-cli/benchmark/connected-memory-run.py \
  --root "$TMPDIR/cm1" --turns 5 --idle-seconds 10
```

The fixture exposes `/status` plus atomic `status.json` numeric observations.
Each user prompt `MEMORY_TURN_N` produces the configured number of read-only
`read_file` calls against the generated `fixture.txt`, followed by streamed
assistant text ending `MEMORY_DONE_N`. Progress is derived from completed call
IDs in the request, so a transport retry does not silently advance work. The
runner checks server completion; inspect pane captures and persisted transcript
as well to verify successful tools and final UI state.

For substantive generated file content rather than whitespace stress, add
`--tool shell-write --write-lines 100`. Each round streams a `run_terminal_cmd`
argument containing a literal, quoted heredoc of synthetic symbol declarations,
writes `fixture-edit-<turn>-<round>.txt` only in the fresh fixture cwd, and returns
its byte count. This explicit mode enables `--yolo` in the isolated CLI wrapper;
no command or path is derived from incoming prompt content. `--write-lines`
is bounded at 20,000. Inspect final generated files and successful tool outcomes,
not only the server's final response marker. Both read-only and generated-write
cases remain scripted fixtures, not evidence of general live-provider savings.

For streamed terminal output, use fixture flags `--tool shell-output --rounds 2
--output-bytes 262144 --output-chunks 200 --output-duration-ms 3000
--text-bytes 256`. Each real `run_terminal_cmd` invocation runs the fixture's
absolute Python interpreter, flushes synthetic output over three seconds, and
writes a completion sentinel in the isolated cwd. The provider checks that
sentinel before advancing. This mode enables isolated `--yolo`; no shell command
is derived from incoming prompts. Output is bounded at 1 MiB per call. Use runner
flags `--turns 1 --startup-seconds 30 --idle-seconds 3`; short startup waits can
race database initialization. This is a reproducible output-heavy workload,
not a claim about the frequency of large outputs in actual usage.

For a manual smoke, launch `cm1/run-cli` in an owned tmux session and submit
`MEMORY_TURN_1`, then `/quit`. `run-cli --resume SESSION_ID` preserves the same
isolated home. To automate a resumed session while the fixture remains alive:

```sh
python3 packages/agent-cli/benchmark/connected-memory-run.py \
  --root "$TMPDIR/cm1" --resume SESSION_ID --start-turn 6 --turns 5
```

`heap.csv` appends across processes, including PID/timestamps; per-drive RSS CSV
and pane captures live in `drive-memory-*`. `cli-stderr.log` is replaced at each
CLI launch, so preserve it before resuming if comparing final RTS summaries.
Use a separate fresh directory per independent sample. Stop the owned fixture
server with SIGTERM after the CLI exits; it also stops after `--max-seconds`.
The wrapper uses `env -i` with an allowlist of tool/library/data paths and an
isolated `HOME`, not user credentials, gateway settings, skills, or MCP config.
Never point this harness at real user data or copy credential files into it.

## Measurement controls

* Default launch is `+RTS -N4 -T -s`; `heap.csv` uses the executable's existing
  `HeapDiagnostics` hook. Normal `last_gc_*` samples may be stale or describe
  minor collections. They are not necessarily current major-GC residency.
* Use a separate fixture with `--force-gc` to collect before each observation,
  and another with `--heap-profile` for `-hT -i0.1`. Both perturb GC behaviour.
  Do not compare their timing/RSS against an uninstrumented baseline.
* `rss.csv` observes only the CLI PID from the diagnostics file: `VmRSS` and
  `VmHWM` in KiB, sampled every 250 ms. It does not claim total process-tree
  memory or count the independent Python fixture.
* The isolated home may start private PostgreSQL. Choose an unused
  `--postgres-port` and retain/report server lifecycle separately. The fixture
  stops its exact cluster on exit; stop the CLI first so its shutdown does not
  race database teardown. `postgres-stop.log` records that cleanup.
* Test unpadded normal arguments first. `--argument-padding 262144` adds harmless
  JSON whitespace to the `read_file` arguments to isolate accumulation costs.
  This is an explicit stress control, **not a representative normal tool call**.
* Repeat baseline/candidate alternately at least three times; compare identical
  turn/round counts, chunk sizes/delays, terminal size, input/output bytes, build
  flags, startup/idle intervals, and PostgreSQL cold/warm status.
* Defaults cap runtime at 30 minutes, request body at 64 MiB, requests at 1000,
  and each connection read/write timeout at 15 seconds. Invalid prompts fail
  rather than invoking any fallback model.

### Streaming terminal output

Use fixture flags `--tool shell-output --output-bytes 262144
--output-chunks 200 --output-duration-ms 3000` to run a bounded Python command
through the real `run_terminal_cmd` tool. It flushes 256 KiB over approximately
three seconds, followed by a unique completion sentinel. The fixture requires
that sentinel in the returned tool output before completing the driven turn.
This mode uses the isolated fixture's `--yolo` launch, like `shell-write`.
Python must be available in the allowlisted launch `PATH`.

The runner also requires the final `MEMORY_DONE_N` and `✓ Finished` to render.
Use `--startup-seconds 30` when initializing a cold PostgreSQL cluster; sending
input while session storage is still opening is not a valid workload.
Title requests and background transcript consumers return plain text, are
logged with `auxiliary: true`, and never advance `completed_turn`.
