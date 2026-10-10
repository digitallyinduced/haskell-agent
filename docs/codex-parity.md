# Codex wire-contract parity audit

Audited 2026-08-27 against the `openai-codex` reference checkout in
`~/.haskell-agent/reference/openai-codex` (codex-rs, commit `4f39251`,
2026-08-22). Motivated by runaway tool-argument generation on `gpt-5.6-sol`
(see PR #641): the same model degenerates far more often under this harness
than under Codex CLI, so the question is where our request distribution
deviates from the one the model was trained with.

## What already matches

Verified field-by-field between `Agent.CLI.Request.requestParams` /
`Agent.OpenAI.WebSocketClient.buildWsPayloadWithOptions` and codex-rs
`ModelClientSession::build_responses_request` (`core/src/client.rs:867-957`):

| Field | Codex | This harness |
| --- | --- | --- |
| `store` | `false` always | `false` ✓ |
| `stream` | `true` always | `true` ✓ |
| `tool_choice` | `"auto"` | `"auto"` ✓ |
| `include` | `["reasoning.encrypted_content"]` | same ✓ |
| `reasoning.summary` | `auto` (when supported) | `auto` for OpenAI ✓ |
| `reasoning.context` | `all_turns` on Responses Lite only | same ✓ |
| `parallel_tool_calls` | `false` on Lite, else `true` | same ✓ |
| `temperature` / `top_p` / `max_output_tokens` | never sent | never sent ✓ |
| Lite prefix | tools as `additional_tools` + developer base-instructions message, function/custom tools wrapped in a `namespace` spec named `functions` | same shape ✓ (`responsesLitePrefix`, `responsesLiteToolValues`) |
| Lite `text.verbosity` | `low` (sol catalog `default_verbosity: low`) | `low` ✓ |
| Lite image `detail` | stripped | stripped ✓ |
| AGENTS.md wrapper | `# AGENTS.md instructions for <dir>` … `<INSTRUCTIONS>…</INSTRUCTIONS>` | same ✓ |
| WS envelope | flat request + `type: response.create`, `previous_response_id` | same ✓ |

## Drift, by impact

### 1. Tool modality for `gpt-5.6-*` (high)

The bundled model catalog (`codex-rs/models-manager/models.json`) declares for
`gpt-5.6-sol`:

```
shell_type            = unified_exec
tool_mode             = code_mode_only
apply_patch_tool_type = freeform
default_verbosity     = low
default_reasoning_level = low
```

Under Codex, sol runs in **code mode only**: it receives a freeform `exec`
tool (Lark grammar, JavaScript source) plus `wait`, and the direct function
tools are hidden — the model orchestrates tool calls by writing JS, not by
emitting JSON function calls. This harness gives it direct function tools
(`shell_command`, `read_file`, `grep`, …). That is the largest distribution
gap and the most plausible driver of the elevated
runaway-function-argument rate: the model barely samples raw JSON tool-call
argument streams in its native harness.

Options, in increasing effort:
- keep direct tools and rely on the #641 guards (current state);
- prefer non-code-mode models (`gpt-5.4`, `gpt-5.5` have
  `tool_mode = default`) for the default configuration;
- implement code mode (freeform `exec` + a JS orchestration runtime) —
  a dedicated project.

### 2. Exec tool shape (high, coupled to 1)

Current Codex has **no `shell` or `shell_command` tool at all**. The exec
surface is `exec_command` + `write_stdin`
(`core/src/tools/handlers/shell_spec.rs`):

```
exec_command { cmd (required), workdir, tty, yield_time_ms,
               max_output_tokens, shell?, login?,
               sandbox_permissions, justification, prefix_rule }
write_stdin  { session_id (required), chars, yield_time_ms, max_output_tokens }
```

Ours is `shell_command { command (required), workdir, timeout_ms,
yield_time_ms }` plus a separate `write_stdin` — an earlier Codex generation
(`shell_command` survives in codex-rs only as a serde alias and a reserved
name). Differences the model has to bridge on every call: tool name,
`cmd` vs `command`, `timeout_ms` (Codex has no such parameter — only
`yield_time_ms`), missing `tty` / `max_output_tokens`.

Recommendation: migrate the Codex dialect to `exec_command` semantics
(rename, `cmd`, drop `timeout_ms` in favor of `yield_time_ms`). Touches
approval allowlists, resource claims, renderers, and persisted transcripts,
so it needs its own PR and migration story.

### 3. Base instructions (medium-high)

Codex no longer ships static prompt files; the `instructions` field comes
from the model catalog's per-model `instructions_template`
(`gpt-5.6-sol`: 17,730 chars; fetched live from the backend `/models`
endpoint, bundled fallback in `models-manager/models.json`). Our
`Agent.Codex.Dialect.Prompt.codexSystemPrompt` is a short custom prompt.
Deliberate in part (our tool surface differs), but it means every turn runs
against instructions the model never saw in training.

Recommendation: consider sourcing the per-model template (bundled or from
`/models`) and layering harness-specific deltas on top, instead of a
from-scratch prompt.

### 4. Context items (medium)

Codex sends `<environment_context>` (cwd, shell, current date, timezone,
sandbox details) as a *user* message and permission instructions as a
*developer* message wrapped in `<permissions instructions>` (space, not
underscore). We fold cwd and date into the system prompt instead. Low risk,
but easy to converge and removes another train/serve mismatch.

### 5. `prompt_cache_key` (low, quick win)

Codex always sends one (the session id, or
`{internal_source}:{parent_thread_id}` for subagents) — improves server-side
cache affinity. We send none. `ResponseCreateParams.promptCacheKey` already
exists and `sanitizeCodexRequest` passes it through; plumbing the session key
into the three `requestParams` call sites is a small change.

### 6. Reasoning default for sol (low)

Catalog `default_reasoning_level` for `gpt-5.6-sol` is `low`; our default
session effort is `medium`. Not a correctness issue, but worth knowing when
comparing behavior against Codex.

## Intentional divergences

- **Runaway guards.** Codex has *no* client-side repetition or
  argument-volume guard (audited: only output truncation caps). Our
  argument-stream activity, 100k-char warnings, and 300k-char abort (#641)
  are deliberate additions; keep them.
- **Approval parameters.** Codex always advertises `sandbox_permissions` /
  `justification` / `prefix_rule` on `exec_command`. Our approval model
  differs by design.
- **Harness tools** (`read_file`, `grep`, `list_dir`, `run_ghci`,
  `update_plan` variants, collaboration tools) — deliberate product surface.

## Non-issue verified during this audit

The 220 KB runaway sample's `member_<uuid>/task_<uuid>/…` path is **not** a
path this harness ever created: `~/.haskell-agent/worktrees/<repo>/` is flat
(date-hash directories only) and no `member_`/`task_` directories exist. The
model hallucinated the nesting, borrowing multi-agent task-path vocabulary.
No worktree-layout change is needed.
