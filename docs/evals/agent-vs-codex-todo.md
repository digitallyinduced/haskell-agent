# agent-cli vs Codex: real-world Haskell todo app

This eval gives `agent-cli` and Codex the same model, reasoning effort, prompt,
empty Git workspace, automatic approval policy, and timeout. The task is to
build a GHC 9.10 HTTP todo server with a Nix flake and in-memory `MVar`
persistence.

This compares the two product configurations, not identical tool schemas or
system prompts. Codex runs ephemerally with user config and exec-policy rules
disabled; both runners still use their own built-in coding-agent contracts.

The revised evaluator disables agent-cli subagents and its GHCi tool, enables
its explicit Bash tool, and requires both runners to personally build and
exercise the application before finishing. This prevents unreported
child-model usage and gives both runners a shell-based execution path on hosts
without `ghci` in the ambient `PATH`.

The evaluator records:

- deterministic grader pass/fail;
- coding-agent wall-clock duration;
- provider-reported input, output, and cached-input tokens;
- stdout/stderr logs;
- completed workspaces;
- the `agent-cli` session transcript and Codex thread id, when available.

Wall time stops when the coding agent exits. Building and exercising the
finished application happens afterward and is deliberately excluded from the
agent-time comparison.

## Run

Build the current `agent-cli` and evaluator:

```console
nix develop -c cabal build \
  agent-cli:exe:agent-cli \
  agent-cli:exe:eval-agent-vs-codex-todo
```

Locate the executables and run at least three trials:

```console
agent_bin=$(nix develop -c cabal list-bin agent-cli:exe:agent-cli)
eval_bin=$(nix develop -c cabal list-bin agent-cli:exe:eval-agent-vs-codex-todo)

"$eval_bin" \
  --agent-bin "$agent_bin" \
  --codex-bin "$(command -v codex)" \
  --model gpt-5.6-sol \
  --effort medium \
  --trials 3 \
  --timeout-seconds 900 \
  --results-dir "eval-results/agent-vs-codex-todo-$(date +%Y%m%d-%H%M%S)"
```

Run order alternates by trial to reduce ordering bias. Use
`--runner agent-cli` or `--runner codex` for a focused smoke test. Results
directories must be new or empty.

The eval makes real model requests and may incur usage charges.

## Grading

The external grader verifies that:

1. `nix develop -c ghc --numeric-version` reports GHC 9.10.
2. Haskell source uses `MVar`.
3. `PORT=<isolated-port> nix run` starts an HTTP server.
4. `GET /tasks` initially returns an empty JSON array.
5. Two `POST /tasks` requests return HTTP 201 with exact task fields and
   produce persistent in-memory state.
6. `DELETE /tasks/:id` returns HTTP 204 and the following `GET` reflects the
   deletion.

The grader uses a fresh process and workspace for every run. A solution only
counts as successful when both the agent exits cleanly and every grader check
passes.

## Results: August 24, 2026

The results below are from the original run. Transcript analysis found that
agent-cli could not launch its registered GHCi runner, performed only static
verification, and delegated review to a `gpt-5.6-luna` child whose usage was
not included in the root-session totals. Codex built and exercised its
applications itself. These numbers are therefore retained as historical data,
not as an apples-to-apples efficiency conclusion. The revised run uses
`--no-subagents --no-ghci --bash` and requires recorded self-verification.

The suite was run on the x86-64 Linux host `office-builder` with:

- model `gpt-5.6-sol`;
- medium reasoning effort;
- three trials per runner;
- a 900-second timeout per run;
- `agent-cli 0.1.0.0`;
- `codex-cli 0.144.4`.

All six runs exited cleanly and passed every external grader check.

### Medians

| runner | passed | agent seconds | input tokens | cached input | output tokens |
|---|---:|---:|---:|---:|---:|
| agent-cli | 3/3 | 193.69 | 182,194 | 118,272 | 7,473 |
| Codex | 3/3 | 235.73 | 760,465 | 672,000 | 6,661 |

Compared with Codex, the `agent-cli` medians were:

- 17.8% less wall-clock time;
- 76.0% fewer reported input tokens;
- 82.4% fewer cached-input tokens;
- 12.2% more output tokens.

The reported input-token difference is dominated by cached context. Across all
three trials, subtracting cached input left 184,726 uncached input tokens for
`agent-cli` and 234,122 for Codex, so `agent-cli` used 21.1% fewer uncached
input tokens.

### Individual runs

| trial | runner | pass | agent seconds | grade seconds | input | cached | output |
|---:|---|---:|---:|---:|---:|---:|---:|
| 1 | agent-cli | yes | 214.00 | 36.11 | 227,898 | 144,256 | 7,599 |
| 1 | Codex | yes | 257.56 | 37.35 | 1,142,833 | 1,030,400 | 7,641 |
| 2 | Codex | yes | 235.73 | 18.27 | 760,465 | 672,000 | 6,661 |
| 2 | agent-cli | yes | 170.45 | 33.38 | 153,002 | 118,272 | 6,935 |
| 3 | agent-cli | yes | 193.69 | 87.25 | 182,194 | 115,840 | 7,473 |
| 3 | Codex | yes | 161.94 | 11.37 | 470,472 | 437,248 | 5,813 |

Run order alternated as designed. Codex was fastest in trial 3, while
`agent-cli` was faster in trials 1 and 2 and had the lower median.

### Aggregate totals

| runner | agent seconds | input | cached input | uncached input | output |
|---|---:|---:|---:|---:|---:|
| agent-cli | 578.14 | 563,094 | 378,368 | 184,726 | 22,007 |
| Codex | 655.23 | 2,373,770 | 2,139,648 | 234,122 | 20,115 |

These results compare complete product configurations. The runners use the
same task prompt, model, effort, approval policy, and isolated workspace, but
their system prompts, tool contracts, execution strategies, context caching,
and installed CLI versions differ.
