# agent-cli vs Codex: real-world Haskell todo app

This eval gives `agent-cli` and Codex the same model, reasoning effort, prompt,
workspace fixture, automatic approval policy, and timeout. The task is to
build a GHC 9.10 HTTP todo server with a Nix flake and in-memory `MVar`
persistence.

This compares the two product configurations, not identical tool schemas or
system prompts. Codex runs ephemerally with user config and exec-policy rules
disabled; both runners still use their own built-in coding-agent contracts.

The evaluator has three runners:

- `agent-cli`: disables subagents and GHCi, enables the explicit Bash tool, and
  requires the root agent to implement and verify the application itself;
- `agent-cli-rlm`: gives the root only the GHCi tool plus `rlmQuery`,
  `rlmQueryMany`, and `rlmCode` helpers backed by in-process subagents;
- `codex`: runs Codex exec with matching model, effort, approval, and task
  constraints.

RLM worker usage is included in the root session totals. Read-only workers may
run concurrently; coding workers are serialized to avoid overlapping edits.
The packaged `agent-cli` puts GHC on `PATH`, because RLM mode requires the
persistent GHCi coordinator.

Every workspace starts with the same pinned `flake.nix` and `flake.lock`. The
fixture exposes GHC 9.10 plus Aeson, WAI, Warp, and http-types, and runs
`app/Main.hs`. The evaluator builds the fixture and development environment
before starting any timed agent run. Agents are told not to change either
flake file, and the grader requires both files to remain byte-for-byte
identical. This removes dependency selection and first-use Nix compilation
from the timed comparison while leaving the Haskell implementation task open.

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

Run order alternates by trial to reduce ordering bias. Use `--runner
agent-cli`, `--runner agent-cli-rlm`, or `--runner codex` for a focused smoke
test. Results directories must be new or empty.

The eval makes real model requests and may incur usage charges.

## Grading

The external grader verifies that:

1. The provided `flake.nix` and `flake.lock` are unchanged.
2. `nix develop -c ghc --numeric-version` reports GHC 9.10.
3. Haskell source uses `MVar`.
4. `PORT=<isolated-port> nix run` starts an HTTP server.
5. `GET /tasks` initially returns an empty JSON array.
6. Two `POST /tasks` requests return HTTP 201 with exact task fields and
   produce persistent in-memory state.
7. `DELETE /tasks/:id` returns HTTP 204 and the following `GET` reflects the
   deletion.

The grader uses a fresh process and workspace for every run. A solution only
counts as successful when both the agent exits cleanly and every grader check
passes.

## Results: August 24, 2026

### In-process RLM prototype: `gpt-5.6-terra`

A one-trial smoke comparison was run on `office-builder` with medium effort
and a 1,200-second timeout. All three corrected runners passed their own
verification and the independent grader.

| runner | pass | seconds | input | uncached | cached | output |
|---|---:|---:|---:|---:|---:|---:|
| agent-cli | yes | 206.61 | 371,798 | 28,886 | 342,912 | 6,545 |
| agent-cli RLM, warm Nix cache | yes | 248.30 | 155,518 | 49,790 | 105,728 | 9,342 |
| Codex | yes | 713.20 | 1,886,187 | 111,851 | 1,774,336 | 9,637 |

Against direct agent-cli in this sample, RLM used 58.2% fewer total input
tokens and 69.2% fewer cached-input tokens, but 72.4% more uncached input,
42.7% more output, and 20.2% more time. Against Codex, RLM used 91.8% fewer
total input tokens, 94.0% fewer cached tokens, 55.5% fewer uncached tokens,
3.1% fewer output tokens, and 65.2% less time.

The first corrected RLM run took 835.26 seconds with 333,778 input tokens
(245,504 cached), 9,133 output tokens, and still passed. Its generated flake
selected a Nix package set that was not warm on the host. The first GHC version
check exhausted the command timeout while Nix built dependencies, then the
retry succeeded. After warming that exact package set, the second RLM run took
248.30 seconds. This makes the timing result unsuitable as a controlled
performance conclusion: model choices changed the generated flake, and
first-use Nix closure cost dominated both the cold RLM run and the Codex run.
The token reduction is less sensitive to that host-cache artifact, but this is
still only one successful trial per configuration and needs repetition.

Transcript inspection explains the token shape. The RLM root made one
read-only inspection call, delegated the initial implementation to a coding
worker, inspected the result locally, and used a second coding worker for a
focused API correction. The root then performed the required Nix and HTTP
checks itself. This kept large implementation contexts inside short-lived
workers instead of replaying them through every root turn, substantially
reducing cached-context amplification. It also added uncached prompts and
worker responses, explaining why uncached input and output increased relative
to direct agent-cli.

### Revised self-verifying run: `gpt-5.6-sol`

The revised suite was run on `office-builder` with the same model, medium
effort, three trials, and 900-second per-run timeout. agent-cli used
`--no-subagents --no-ghci --bash`; both runners had to record successful GHC,
build, `nix run`, and HTTP CRUD verification. A run only passed when it was
self-verified, did not delegate, exited successfully, and passed the independent
grader.

| runner | passed | median successful seconds | median input | median uncached | median output | median cached |
|---|---:|---:|---:|---:|---:|---:|
| agent-cli | 2/3 | 232.20 | 383,787 | 48,491 | 7,136 | 335,296 |
| Codex | 3/3 | 213.94 | 657,318 | 51,364 | 6,835 | 610,816 |

Among successful runs, agent-cli used 41.6% fewer total input tokens and 45.1%
fewer cached-input tokens. The uncached-input advantage was much smaller at
5.6%, and agent-cli used 4.4% more output tokens. Codex had the faster median by
8.5% and completed all three trials.

| trial | runner | pass | seconds | input | uncached | cached | output |
|---:|---|---:|---:|---:|---:|---:|---:|
| 1 | agent-cli | yes | 175.95 | 251,329 | 42,177 | 209,152 | 5,571 |
| 1 | Codex | yes | 213.94 | 657,318 | 46,502 | 610,816 | 7,459 |
| 2 | Codex | yes | 207.32 | 547,104 | 58,400 | 488,704 | 6,835 |
| 2 | agent-cli | yes | 288.45 | 516,245 | 54,805 | 461,440 | 8,702 |
| 3 | agent-cli | no | 900.11 | n/a | n/a | n/a | n/a |
| 3 | Codex | yes | 221.13 | 666,020 | 51,364 | 614,656 | 6,822 |

Every completed run self-verified without delegation and passed the external
GHC 9.10, `MVar`, and HTTP CRUD checks. agent-cli trial 3 stalled immediately
after creating its plan, produced no project files, and reached the 900-second
timeout. This revised result supports a narrower conclusion than the original:
agent-cli still has materially lower cached-context amplification, but it was
not faster or more reliable in this sample.

### Revised self-verifying run: `gpt-5.6-terra`

The same corrected suite was also run on `office-builder` with
`gpt-5.6-terra`, medium effort, three trials, and the same 900-second timeout.
All six runs self-verified without delegation and passed the independent
grader.

| runner | passed | median successful seconds | median input | median uncached | median output | median cached |
|---|---:|---:|---:|---:|---:|---:|
| agent-cli | 3/3 | 348.18 | 507,968 | 53,952 | 11,088 | 454,016 |
| Codex | 3/3 | 240.18 | 579,451 | 55,675 | 5,482 | 523,776 |

agent-cli used 12.3% fewer total input tokens, 13.3% fewer cached-input
tokens, and 3.1% fewer uncached-input tokens. It used 102.3% more output
tokens and took 45.0% longer at the median. Unlike the `gpt-5.6-sol` run,
agent-cli completed all three trials.

| trial | runner | pass | seconds | input | uncached | cached | output |
|---:|---|---:|---:|---:|---:|---:|---:|
| 1 | agent-cli | yes | 350.99 | 717,087 | 65,311 | 651,776 | 11,088 |
| 1 | Codex | yes | 240.18 | 579,451 | 55,675 | 523,776 | 5,482 |
| 2 | Codex | yes | 285.41 | 948,267 | 60,971 | 887,296 | 8,260 |
| 2 | agent-cli | yes | 348.18 | 507,968 | 53,952 | 454,016 | 11,702 |
| 3 | agent-cli | yes | 208.74 | 373,138 | 46,994 | 326,144 | 7,311 |
| 3 | Codex | yes | 141.61 | 359,581 | 28,317 | 331,264 | 4,781 |

### Original run

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
