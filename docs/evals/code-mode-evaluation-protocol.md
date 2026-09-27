# Code-mode behavioral evaluation protocol

This replacement protocol was specified before collecting its model results.
It supersedes the defective twelve-trial behavioral comparison, not the separate
runtime microbenchmark. No general language reliability claim is implied.

## Comparison

- Main model: `gpt-6-sol`, low reasoning effort, for both backends.
- Current JavaScript and Haskell code-mode implementations.
- Haskell compiler repair: production `gpt-6-luna`, low reasoning effort.
  Count repair requests, elapsed time, and provider-reported tokens separately
  and include them in total task resource use.
- Four task families: CSV summary, recursive log audit, paginated analytics,
  and record joining. Ten deterministic fixture variants per family, paired
  across backends: 80 total model-task trials.
- Fresh conversations, toolsets, and fixture directories per trial; alternate
  backend order. No selective retries or removal of unsuccessful trials.
- Identical underlying data, tool behavior, and task requirements. Language
  declarations and supported compiler-repair behavior intentionally differ:
  this compares the configured products, not language syntax in isolation.

## Admission checks

Before paid model trials, execute no-model checks through both actual code-mode
backends. Verify text versus structured-array/object return contracts, run
known-correct task cells against independent expected answers, and deliberately
exceed the callback limit to verify prompt cancellation. Tool descriptions must
match the values visible inside each runtime. Record source provenance.

The fixture APIs are local, read-only simulations of workflows; they are not
live MCP service measurements. The hosts are unsandboxed. Filesystem allowlists
protect the fixture callbacks, not arbitrary code inside a runtime.

## Outcomes

Use exact requested final answers and required tool use as correctness gates.
Report every trial, including incorrect answers, compiler failures, transport
failures, exhausted budgets, and timeouts. Enforce equal main-turn, callback,
and elapsed-time limits: eight main-model turns, 128 dispatched nested callbacks,
and 180 seconds per trial (including startup and repair; cleanup may extend the
observed wall time). Any harness defect found during collection invalidates
the affected comparison; fix it and retain its records separately rather than
silently replacing unsuccessful samples.

Report per task and backend:

- Correct answers / attempts, with descriptive 95% Wilson intervals.
- Paired outcomes: both correct, only JavaScript correct, only Haskell correct,
  neither correct.
- Median elapsed time for all attempts and successful attempts separately.
- Main and repair token counts, including cached input separately; no invented
  monetary pricing. Record incomplete accounting if an interrupted provider
  request does not return usage.
- Main model turns, execution calls, failed cells, repair attempts, and nested
  callback counts.

Ten trials per task still yield wide uncertainty intervals. Results characterize
these four workflows, model settings, fixture sizes, and the measured revision;
they cannot establish universal superiority or isolate the causal benefit of
typed outputs or repair without additional ablations.
# Follow-up: environment guidance

The next run retains the same four tasks, ten paired variants, models, ordering,
and execution limits as the corrected baseline. The intervention is improved
Haskell instructions for both the main and repair models, plus standard-library
imports needed for the documented JSON operations. It adds no custom JSON API
or task-specific solution examples. Consequently this is a **prompt and import
environment** comparison, not an isolated prompt-only ablation. Preserve the
baseline and report all follow-up trials, including regressions. These tasks
have informed the intervention, so improvements are development-set results,
not held-out generalization evidence.
