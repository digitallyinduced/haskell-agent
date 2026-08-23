# GHCi-only vs bash-only vs combined eval

This behavioral eval compares three user-facing configurations:

- **ghci-only**: the default, with `run_ghci` and no explicit shell tool.
- **bash-only**: the provider shell tool with `run_ghci` disabled.
- **ghci-plus-bash**: `--bash`, which adds the provider shell tool while
  retaining `run_ghci`.

It uses the same user task prompt in all variants. The complete product
configurations necessarily differ because their tool schemas and matching
system-prompt guidance differ.

The suite uses three isolated, deterministically graded tasks:

1. Aggregate a CSV into an exact report.
2. Implement and verify a specification-driven C11 command-line program.
3. Recursively audit a directory tree.

Each one-shot run is saved as a normal agent session. The evaluator reports:

- grader pass/fail;
- wall-clock duration;
- provider-reported input, output, and cached tokens;
- ordered tool calls;
- stdout/stderr logs and the final workspace.

## Run

Build the current agent and evaluator:

```console
nix develop -c cabal build \
  agent-cli:exe:agent-cli \
  agent-cli:exe:eval-ghci-vs-bash
```

Locate both executables and run one trial per task/configuration:

```console
agent_bin=$(nix develop -c cabal list-bin agent-cli:exe:agent-cli)
eval_bin=$(nix develop -c cabal list-bin agent-cli:exe:eval-ghci-vs-bash)

"$eval_bin" \
  --agent-bin "$agent_bin" \
  --results-dir "eval-results/ghci-vs-bash-$(date +%Y%m%d-%H%M%S)" \
  --trials 1 \
  --timeout-seconds 180 \
  -- --provider openai --model gpt-5.6-sol
```

Increase `--trials` for a less noisy comparison. Run order alternates between
the configurations across tasks and trials. Results are written to
`results.json` and `summary.md`.

Use `--task NAME` or
`--mode ghci-only|bash-only|ghci-plus-bash` to rerun a subset.
Each run has a configurable timeout so a stalled provider response does not
block the suite indefinitely. Results directories must be new or empty; this
prevents stale logs from a previous full or filtered run.

The eval makes real provider requests and may incur usage charges.

## C task spot check

After replacing the Haskell task, the C task was run once per mode on
August 23, 2026 with OpenAI `gpt-5.6-sol`, low reasoning effort, and a
180-second timeout:

| mode | strict result | wall time | input tokens | output tokens |
|---|---:|---:|---:|---:|
| ghci-only | pass | 48.65 s | 50,999 | 1,727 |
| bash-only | pass | 118.07 s | 60,557 | 2,050 |
| ghci-plus-bash | timeout | 180.15 s | unavailable | unavailable |

Both successful modes produced C that compiled under strict C11 warning flags
and passed all grader cases. GHCi-only invoked the C compiler and executable
through `System.Process` from `run_ghci`; bash-only used `shell_command`.

The bash-only wall time includes a 58-second provider-credential retry, so the
raw latency difference is not attributable to tool choice. The combined run
wrote a program that passed the external grader, but the agent did not finish
its verification turn before the timeout. A combined-mode retry behaved the
same way. Because the evaluator requires both a passing artifact and a clean
agent exit, both combined runs count as failures.

This is still only a spot check. It demonstrates that the GHCi-only
configuration can complete and verify a non-Haskell programming task, but it
does not establish a stable ranking between the modes.

## Superseded pilot comparison

The first pilot, run on August 23, 2026 with OpenAI `gpt-5.6-sol`, used a
Haskell repair task in the second slot. That task gave `run_ghci` an
unfair domain-specific advantage, so it has been replaced by the C11 program
task above. These numbers are retained only as historical context and are not
results for the current suite.

The superseded pilot used low reasoning effort, one trial per
task/configuration, and a 60-second per-run timeout:

| task | ghci-only | bash-only | ghci-plus-bash |
|---|---:|---:|---:|
| data-summary | pass, 16.35 s | pass, 24.74 s | pass, 21.14 s |
| Haskell fix (removed) | pass, 51.97 s | timeout; retry passed, 52.17 s | pass, 50.10 s |
| tree-audit | pass, 38.24 s | provider rejection; retry timed out at 120 s | timeout; retry passed, 39.83 s |

First-attempt grader pass rates were:

- **ghci-only: 3/3**
- **bash-only: 1/3**
- **ghci-plus-bash: 2/3**

For the two tasks completed by every mode, GHCi-only was fastest on the data
summary, while all three modes were close on the removed Haskell repair.
Bash-only never completed the tree audit, including a retry with a 120-second
timeout.

Using the successful retry for the combined tree audit, the two modes that
completed all three tasks totaled:

| mode | wall time | input tokens | output tokens |
|---|---:|---:|---:|
| ghci-only | 106.56 s | 161,932 | 2,889 |
| ghci-plus-bash | 111.07 s | 179,812 | 3,230 |

GHCi-only therefore used **4.1% less wall time**, **9.9% fewer input tokens**,
and **10.6% fewer output tokens** than the combined mode, while also completing
all tasks without a retry.

The current suite needs to be rerun before drawing conclusions. In particular,
the old 3/3 result for GHCi-only included the removed Haskell-specific task.
Use at least three trials per task with a longer timeout before treating
latency, token, or reliability deltas as stable.
