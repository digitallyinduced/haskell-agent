# Haskell programmatic tool-calling benchmark

This benchmark compares otherwise identical real-model agent sessions:

- `direct`: `run_haskell_program` and its prompt guidance are disabled.
- `optional-haskell`: the model chooses between direct and programmatic calls.
- `forced-haskell`: the task must use one outer `run_haskell_program` call.
- `forced-shell`: Haskell is disabled and the task must use one shell call.

The default run uses three generated, locally scored tasks:

1. `privacy-canary` reads a secret through a nested tool call but should expose
   only its length to the model-visible transcript.
2. `fanout-reduce` reads four shards and computes an aggregate.
3. `simple-control` reads one value and adds one, checking that orchestration
   overhead is not hidden by only testing favorable workloads.

## Run

```console
nix develop
agent="$(cabal list-bin agent-cli:exe:agent-cli)"
cabal run agent-benchmark -- \
  --agent "$agent" \
  --provider openai \
  --model gpt-5.6-sol \
  --effort medium \
  --repetitions 3 \
  --arms direct,optional-haskell,forced-haskell \
  --tasks privacy-canary,fanout-reduce,simple-control
```

Use a fixed provider, model, effort, task set, and repetition count when
comparing revisions. The runner rotates arm order within each task/repetition
to reduce ordering bias.

## Outputs

Each run directory contains:

- `runs.jsonl`: machine-readable metrics for every session.
- `summary.md`: per-run table and aggregate means by arm.
- `logs/`: captured stdout and stderr from each agent invocation.
- `logs/.../tool-events.jsonl`: audited top-level and nested tool starts,
  including names and arguments but not tool outputs.
- `fixtures/`: the exact generated task inputs.

The metrics include strict result correctness, exit status, wall time,
provider-reported input/output/cached tokens, top-level tool names, visible
tool-output bytes, whether Haskell was used, and whether the privacy canary
appeared in a top-level tool result.

Forced-Haskell runs pass tool-adherence checks only when the event log contains
the task's expected nested shell commands. This prevents an outer
`run_haskell_program` call that reads files directly through unrestricted
Haskell IO from being mistaken for programmatic tool calling.

Nested tool calls intentionally do not enter the model transcript. Therefore,
the visible-output metric directly measures the context reduction provided by
programmatic filtering, while correctness prevents a small output from being
mistaken for a successful run.

## Interpreting a pilot

A useful result should show all of the following:

- Similar or better correctness for the Haskell arm.
- Lower visible tool-output volume on fan-out/filtering tasks.
- No privacy-canary exposure for the forced-Haskell arm.
- Acceptable token and latency overhead.
- Little or no advantage on `simple-control`.

One repetition is only a smoke test. Use multiple repetitions and inspect
failures before drawing conclusions because model tool selection and generated
code are stochastic.
