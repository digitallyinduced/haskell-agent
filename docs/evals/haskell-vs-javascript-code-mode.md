# Haskell versus JavaScript code mode

For the separate write-workflow experiment and controlled compile-before-effects
tests, see [audited fulfillment](code-mode-fulfillment.md). Its results are not
pooled with the read-only evaluations below.

## Follow-up: environment guidance — 2026-09-27

The unchanged 80-trial evaluation completed at 08:51 UTC. Both backends passed
**40/40**. Haskell customer/order joins improved from **1/10 to 10/10**, and
CSV summaries from **8/10 to 10/10**; the other two families remained 10/10.

| Task | Haskell baseline correct | Haskell follow-up correct | Haskell baseline median s | Haskell follow-up median s | JavaScript follow-up median s |
|---|---:|---:|---:|---:|---:|
| Customer/order join | 1/10 | 10/10 | 119.20 | 17.36 | 8.10 |
| CSV summary | 8/10 | 10/10 | 39.23 | 15.38 | 10.03 |
| Paginated analytics | 10/10 | 10/10 | 38.33 | 7.38 | 6.56 |
| Recursive log audit | 10/10 | 10/10 | 46.28 | 11.17 | 8.18 |

Medians include every attempt, startup, model requests, compiler repair, and
cleanup. Overall Haskell median fell from 44.66 to 11.50 seconds; JavaScript
was 8.26 before and 8.34 seconds afterward. Haskell repair attempts fell from
227 to 17 and failed cells from 98 to 3. All follow-up trials completed with
unchanged fixtures and complete recorded usage. All 40 paired outcomes were
correct for both backends. JavaScript remained faster in every task median.

The intervention supplies shared main/repair guidance for JSON traversal and
record fields, plus qualified standard imports. This is **not a prompt-only
ablation**. Tasks, models, ordering, and limits were unchanged, and source
hashes remained unchanged during collection. These tasks informed the fix:
this is development-set evidence, not held-out reliability or universal
language superiority. Each 10/10 result has a descriptive Wilson 95% interval
of approximately 72%–100%.

Validation: 81 focused tests, 80 no-model reference executions, and both
callback-limit termination checks passed before collection. Run command:

```sh
nix develop -c python3 scripts/evaluate-code-mode.py --results-dir "$TMPDIR/code-mode-guidance-evaluation" --trials 10
```

Artifacts: [all trial results and resource counts](data/code-mode-guidance-results.json),
[source manifest](data/code-mode-guidance-manifest.json),
[preflight results](data/code-mode-guidance-preflight-results.json), and
[preflight manifest](data/code-mode-guidance-preflight-manifest.json).
The earlier baseline remains below, separate from the follow-up.

**Corrected behavioral evaluation:** JavaScript passed **40/40**, Haskell
**29/40** with compiler repair enabled. The original twelve-trial evaluation
below is invalid as a reliability comparison and is not pooled with these results.
The runtime measurements are a separate experiment.

## Corrected model evaluation — 2026-09-27

Following the [predeclared protocol](code-mode-evaluation-protocol.md), we ran
four tasks with ten paired fixture variants per backend: **80 trials**, with
alternating backend order and fresh hosts. Both used `gpt-6-sol`, low reasoning;
Haskell additionally used the production `gpt-6-luna` low compiler-repair path.
Typed outputs and learned return hints were enabled in the current implementation.
This compares configured products, not language syntax in isolation.

Fixtures comprised a 121-integer CSV summary, recursive logs for three services,
four pages of 25 analytics values, and a join of 18 customers with 90 orders.
These are **local workflow fixtures, not live MCP services**. Directory listings
now return structured arrays matching their descriptions. Before model calls,
reference solutions passed all 80 task executions on the actual hosts, and both
callback-limit termination tests passed.

Each trial allowed eight main-model turns, 128 nested callbacks, and 180 seconds
including host startup and repair; cleanup can extend that deadline. Success
required exact final text, at least one `exec` and nested call, and unchanged
fixture bytes. All 80 fixtures remained unchanged. No trials were discarded or
selectively retried. The manifest confirms sources remained unchanged during
the run (07:21–08:12 UTC). The driver used GHCi inside `nix develop`; these are
end-to-end development-runtime observations, not optimized host microbenchmarks.

| Task | JS correct | Haskell correct | JS median seconds | Haskell median seconds |
|---|---:|---:|---:|---:|
| CSV summary | 10/10 | 8/10 | 9.91 | 39.23 |
| Log tree | 10/10 | 10/10 | 8.26 | 46.28 |
| Paginated analytics | 10/10 | 10/10 | 6.22 | 38.33 |
| Customer/order join | 10/10 | 1/10 | 9.87 | 119.20 |

Medians include failures, model inference, repair, startup and cleanup; they are
not time-to-success. Haskell success-only medians were 34.13, 46.28, 38.33 and
119.02 seconds respectively (the last represents just one success). Across the
40 pairs, both passed 29, JavaScript alone passed 11, Haskell alone passed zero,
and neither passed zero. Ten samples per task still give wide Wilson 95% intervals:
10/10 is 72–100%, 8/10 is 49–94%, and 1/10 is 2–40%. The variants share task
templates; these intervals do not establish reliability across arbitrary work.

### Failures and repair cost

Haskell made 227 repair attempts and returned 98 failed cells; JavaScript
returned none. Nine join trials and one CSV trial exhausted the main-turn limit.
The other failed CSV trial ended with an inability response after a GHCi control
pipe closure. Inspected traces show unavailable module references and attempts
to call record selectors suppressed by `NoFieldSelectors`; repairs did not
consistently resolve these. The turn-limited CSV trial eventually computed the
answer but had no remaining main turn to deliver it, so it remains a failure.

Per-trial median input/output tokens (input includes cached tokens):

| Task/backend | Main | Repair | Combined |
|---|---:|---:|---:|
| CSV / JS | 5323 / 153.5 | 0 / 0 | 5323 / 153.5 |
| Logs / JS | 5412.5 / 219.5 | 0 / 0 | 5412.5 / 219.5 |
| Pagination / JS | 5258 / 91 | 0 / 0 | 5258 / 91 |
| Join / JS | 5403 / 202 | 0 / 0 | 5403 / 202 |
| Logs / Haskell | 11696.5 / 633.5 | 7489 / 897.5 | 19185.5 / 1517 |
| Pagination / Haskell | 12049 / 321 | 5422.5 / 465 | 17681.5 / 779.5 |

CSV and join Haskell groups contain trials flagged `incompleteUsage`; their
full-group token medians are conservatively **unknown**, not zero. Recorded
partial usage is retained. Combined medians are calculated per trial, not by
adding marginal medians. Token counts across two models are not dollar costs.

**Conclusion:** the current JavaScript configuration performed better on this
suite. The corrected log-tree task offers no Haskell reliability advantage.
Haskell's join handling and repair behavior need improvement before arguing for
a default switch. This is not evidence that Haskell intrinsically cannot work,
nor an ablation proving whether typed outputs or repair help.

Artifacts: [all 80 records](data/code-mode-behavioral-valid-results.json),
[run manifest](data/code-mode-behavioral-valid-manifest.json),
[preflight manifest](data/code-mode-behavioral-preflight-manifest.json), and
[preflight results](data/code-mode-behavioral-preflight-results.json).

Reproduce with fresh output directories (the second command makes paid calls):

```sh
nix develop -c python3 scripts/evaluate-code-mode.py \
  --results-dir "$TMPDIR/code-mode-preflight" --preflight
nix develop -c python3 scripts/evaluate-code-mode.py \
  --results-dir "$TMPDIR/code-mode-evaluation" --trials 10
python3 scripts/summarize-code-mode-evaluation.py \
  "$TMPDIR/code-mode-evaluation" --expected-trials 10
```

Runtime baseline recorded **2026-09-27**, before the typed-output implementation.
The retained numbers describe that pre-output-types baseline, not later changes.

## Scope

This comparison separates runtime overhead for equivalent hand-written cells from
model-generated task completion. The runtime suite uses the actual GHCi and Bun
hosts, not standalone language microbenchmarks.

Runtime workloads:

- Repository import audit: concurrently read 8 or 32 Haskell source files and
  count import declarations.
- Parallel analytics: fetch 8 or 32 fixture reports, each containing 1,000 rows
  with 25 ms simulated service latency, then filter and aggregate.
- Dependent pagination: retrieve 4 or 16 sequential pages, each containing 500
  rows with 10 ms simulated service latency, then filter and aggregate.

Both languages use identical callbacks and validate the final aggregate and
callback count against an independent oracle. API fixtures remove network
variability; these are **not live MCP service benchmarks**. The tool schema is a
single object with a required integer `index` field, not a production-sized
catalog of nested records.

The baseline uses one generated Haskell tool binding. A sensitivity run pads
the catalog to 50 tools with the same one-field schema; only `query` is invoked.
This is not a heterogeneous production MCP catalog. Responses remain `Value`
and are decoded explicitly in Haskell. These results precede typed-output work.

## Runtime methodology

The benchmark harness and local libraries are compiled with `-O2`. GHCi remains
the Haskell execution backend because that is the implementation being tested.
Five samples per case alternate backend order. A second complete suite checks
repeatability. Warm cases retain hosts after one unmeasured execution. Sources
and expectations are forced before measurements; filesystem caches are warm.

Warm JavaScript retains its default two-worker pool. Cold **first-result**
starts before host creation and ends after the first validated result, before
cleanup. It includes Haskell host readiness and cell loading, but excludes
the application's separate `prepareHaskellBindings` publication preflight.
Cold **lifecycle** additionally includes host shutdown; do not confuse it with
user-visible first-result latency. Binding source generation is outside timing.

CPU time and allocated bytes are **parent-process-only**: they include callback
handling and serialization, but exclude the Bun/GHCi child processes. They are
not total runtime CPU or memory comparisons.

Environment: Apple M3 Max (arm64), macOS 26.6.1, GHC 9.10.3, Bun 1.4.0.

## Runtime results

Median elapsed milliseconds, five samples per backend/case, one-tool catalog:

| Workload | Size | Warm JS | Warm Haskell | Cold first-result JS | Cold first-result Haskell |
|---|---:|---:|---:|---:|---:|
| Repository import audit | 8 files | 5.235 | 56.384 | 32.891 | 424.957 |
| Repository import audit | 32 files | 8.776 | 59.556 | 34.248 | 427.783 |
| Parallel analytics fixture | 8 reports | 27.786 | 85.067 | 59.018 | 452.638 |
| Parallel analytics fixture | 32 reports | 30.815 | 99.097 | 61.105 | 463.633 |
| Dependent pagination fixture | 4 pages | 45.057 | 102.019 | 75.244 | 463.938 |
| Dependent pagination fixture | 16 pages | 178.192 | 237.577 | 209.297 | 600.862 |

Here GHCi adds approximately **51–68 ms per warm cell** and **389–403 ms
to a cold first result** versus Bun. These are implementation overheads, not
evidence that Haskell arithmetic or model task completion is intrinsically slower.

With 50 simple tool schemas, warm medians change as follows:

| Workload | Size | JS | Haskell |
|---|---:|---:|---:|
| Repository import audit | 8 files | 5.731 | 227.191 |
| Repository import audit | 32 files | 8.891 | 225.110 |
| Parallel analytics fixture | 8 reports | 27.878 | 255.124 |
| Parallel analytics fixture | 32 reports | 30.759 | 273.034 |
| Dependent pagination fixture | 4 pages | 45.314 | 266.193 |
| Dependent pagination fixture | 16 pages | 180.333 | 394.883 |

Catalog size materially affects GHCi loading overhead: the warm difference
is now approximately **215–242 ms**. Caching unchanged generated bindings is
a candidate for separate investigation, not an optimization included here.

All 600 formal timed executions and 60 warmups across the five retained suites
validated exact output text and callback counts. Expected text was `92`/`465`
for repository sizes 8/32, `10665333`/`170661333` for analytics sizes 8/32,
and `666333`/`10665333` for pagination sizes 4/16. Repository callbacks really
read local source files; expectations were read before timing, warming caches.
The first 8/32 files contained 95,582/485,473 bytes in this baseline.

### Raw data and repeatability

CSV rows retain every sample, not just medians, plus parent CPU/allocation.
No timed samples were discarded.

- [First-result baseline](data/code-mode-comparison-first-result.csv):
  `cold` means cold first-result, excluding cleanup.
- [First-result 50-tool catalog](data/code-mode-comparison-first-result-catalog.csv):
  same timing boundary.
- [Initial lifecycle baseline](data/code-mode-comparison-results.csv) and
  [complete repeat](data/code-mode-comparison-repeat.csv):
  `cold` means full lifecycle including cleanup; catalog size is implicitly 1.
- [Lifecycle 50-tool catalog](data/code-mode-comparison-catalog.csv):
  `cold` includes cleanup.

The original lifecycle baseline/repeat warm medians agreed in magnitude:
repository 8 files, JS 5.221/5.177 ms and Haskell 53.397/55.552 ms;
pagination 16 pages, JS 180.828/179.523 ms and Haskell 245.419/236.774 ms.
Lifecycle cold results were higher (for example 713.289 ms Haskell versus
52.696 ms JS for 8 files); the first-result rerun above deliberately removes
shutdown from that metric. These are separate runs, not paired cleanup estimates.

The measured baseline was built successfully with `-O2`. After measurements,
argument validation and CSV line buffering were tightened; a final rebuild
was initially blocked by concurrent `agent-core` changes. The retained harness
subsequently passed a current-source GHCi load/typecheck. Product development
continued after the frozen results, so future runs are not this exact baseline.

## Reproduction

Run from the repository root:

```sh
nix develop -c cabal build --offline --enable-optimization=2 \
  agent-tools:bench:code-mode-comparison-bench

nix develop -c sh -c '
  binary=$(cabal list-bin --enable-optimization=2 agent-tools:bench:code-mode-comparison-bench)
  env -u GHCRTS "$binary" 5 1 +RTS -T -N2
'
```

Use `5 50` for catalog sensitivity. Samples must be odd and at least 3.
The current source explicitly names `cold-first-result`, `cold-lifecycle`,
and `warm`; the historical CSV labels are explained above.

The retained source is
`packages/agent-tools/benchmark/CodeModeComparison.hs`.

## Interpretation boundaries

- Runtime measurements exclude model inference, token costs, and repair turns.
- Warm GHCi means a retained process, not a precompiled cell cache. The current
  implementation loads/typechecks cell modules on each invocation.
- Fixture latency and payload sizes are explicit; results should not be
  extrapolated to arbitrary remote-service latency or larger catalogs.
- A small model evaluation is a smoke test, not evidence of general reliability
  or a statistically established language advantage.

## Invalid historical model evaluation (pre-return-type baseline)

Twelve trials used `gpt-6-sol`, low reasoning, the actual code-mode hosts,
three trials per backend/task, no compiler-repair side model, an eight-turn
limit and a 180-second timeout. Tasks read allowlisted local fixtures: summarize
a single CSV row containing 12 integers, and recursively count errors/warnings in service logs. These
are small workflow examples, not live MCP or production repository evaluations.
All-sample medians below include failed attempts; they are not time-to-success.
Elapsed time includes the cold host, model calls and teardown, but excludes
evaluation-driver GHCi loading and authentication.

Backend order alternated between trials/tasks. Each sample used a fresh toolset
and separate fixture directory; success required exact final text plus at least
one `exec` and nested-tool call. The twelve fixture file manifests remained
identical after execution. Timeout handling closed the bracketed toolset before
the next sample. These are unsandboxed hosts, not an isolation benchmark.

| Task | Backend | Correct | Median seconds | Median input tokens | Median output tokens |
|---|---|---:|---:|---:|---:|
| CSV summary | JavaScript | 3/3 | 5.91 | 4864 | 132 |
| CSV summary | Haskell | 2/3 | 26.68 | 38955 | 942 |
| Log tree (confounded) | JavaScript | 0/3 | 180.23 | 58252 | 380 |
| Log tree (confounded) | Haskell | 2/3 | 24.97 | 29126 | 929 |

Input tokens include cached tokens. In the CSV trials Haskell had 11 failed
cells, eight with compiler diagnostics. Haskell log-tree trials had eight
failed cells, four with compiler diagnostics. JavaScript's hanging cells did
not produce failed-cell results.

**The log-tree comparison is not a fair reliability result:** the fixture
description said “Returns JSON array of names” while the tool returned a
JSON-encoded array as a raw string, not a structured array.
JavaScript iterated characters and recursively revisited `/`, issuing
110,508–115,908 local callbacks per trial before timeout. This harness defect
confounds the comparison; it does not establish a Haskell reliability advantage.
The CSV sample is also far too small to generalize from.

[All twelve compact trial records](data/code-mode-behavioral-results.json)
retain successes, failures, timeouts and token/cache counts. Large traces are
not checked in. The runner is `scripts/CodeModeEvaluation.hs`, launched with:

```sh
nix develop -c python3 scripts/evaluate-code-mode.py \
  --results-dir "$TMPDIR/code-mode-behavioral" --trials 3
python3 scripts/summarize-code-mode-evaluation.py "$TMPDIR/code-mode-behavioral"
```

This is the historical invocation, not a reproduction of the old implementation
using the current runner. Running the current runner makes paid model requests
using configured authentication unless `--preflight` or `--typecheck` is selected.
These retained measurements precede typed outputs and learned return hints;
they do not measure the benefit of that implementation.
