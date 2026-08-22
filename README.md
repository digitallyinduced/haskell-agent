# haskell-agent

A universal coding-agent harness written in Haskell.

## Quick start

With [Nix](https://nixos.org/download/) installed, run the agent directly from
GitHub:

```console
nix run github:digitallyinduced/haskell-agent
```

## Packages

- `agent-cli` is the command-line entry point (`-p` for one-shot, otherwise a REPL).
- `agent-syntax` provides renderer-independent syntax loading, language
  resolution, tokenization, and semantic token classes.
- `agent-tui` provides retained fullscreen presentation state, Markdown
  rendering, themes, and terminal presentation of syntax spans.
- `agent-core` provides provider-neutral credentials, common
  errors, tool dispatch, and transport utilities under the `Agent.*` namespace.
- `agent-responses` provides the canonical Responses wire model, codecs, error
  normalization, response merging, and provider-neutral loop adapters.
- `agent-openai` provides the OpenAI/ChatGPT Responses transports,
  authentication, and streaming under the `Agent.OpenAI.*` module namespace.
- `agent-xai` provides Grok request mapping, OAuth login, HTTP SSE transport,
  and stateful sessions under the `Agent.XAI.*` module namespace.
- `agent-openrouter` provides OpenRouter static API-key auth, HTTP SSE
  transport, and a local-transcript loop backend under the
  `Agent.OpenRouter.*` module namespace.

## Development

All compiler and package dependencies come from the pinned Nix flake.
The flake also fetches Skylighting's complete XML syntax-definition set and
configures it for development, tests, and packaged `agent-cli` executables; the
generated grammar data is not vendored in this repository.

Each package has a checked-in `package.nix` generated with `cabal2nix` (no IFD).
After changing a `.cabal` file, regenerate that package's Nix expression:

```console
(cd packages/agent-core && cabal2nix . > package.nix)
(cd packages/agent-responses && cabal2nix . > package.nix)
(cd packages/agent-openai && cabal2nix . > package.nix)
(cd packages/agent-xai && cabal2nix . > package.nix)
(cd packages/agent-openrouter && cabal2nix . > package.nix)
(cd packages/agent-syntax && cabal2nix . > package.nix)
(cd packages/agent-tui && cabal2nix . > package.nix)
(cd packages/agent-cli && cabal2nix . > package.nix)
```

```console
nix develop
cabal build all
cabal test all
```

```console
nix develop
cabal run agent-cli -- --help
cabal run agent-cli -- -p "list the files here"
```

From `nix develop`, `repl` opens `cabal repl lib:agent-cli` (via expect) and
starts the agent with a GHCi `:cmd` loop. Development REPL sessions default to
OpenAI `gpt-5.6-sol` with `--yolo`. On first open it also passes `--worktree`
when the cwd is not already under `~/.haskell-agent/worktrees`. Inside the agent
REPL, `:reload` returns to GHCi, reloads modules, and resumes the same session
automatically through that REPL's GHCi continuation. Concurrent development
REPLs therefore keep independent reload state. `:q` exits the agent back to
`ghci>`.

Without `-p` / `--prompt-file` the CLI starts a REPL. Credentials come from
`~/.grok/auth.json` / `GROK_ACCESS_TOKEN` (xAI), `~/.codex/auth.json` /
`CODEX_ACCESS_TOKEN` (OpenAI), or `OPENROUTER_API_KEY` (OpenRouter).
`--provider` overrides auto-detection.

## Haskell programmatic tool calling

Every provider exposes `run_haskell_program` alongside its direct coding
tools. It executes a Haskell expression, usually a `do` block, in a dedicated
fresh GHCi process. Programs can invoke registered harness tools:

```haskell
do
  matches <- callTool "shell_command" (object
    [ "command" .= ("rg TODO packages" :: String)
    , "workdir" .= ("." :: String)
    ])
  emitText (Text.unlines (take 20 (Text.lines matches)))
  pure ()
```

Nested calls use the same approvals, plan-mode restrictions, cancellation,
and handlers as direct model tool calls. Their raw results stay inside GHCi;
only stdout selected by the program becomes the outer tool result and enters
model history. Each outer invocation has isolated Haskell bindings, preventing
one approved program from poisoning the helper environment used by a later
program. Tool names and argument schemas follow the active provider's
advertised surface. `callTool` returns the same formatted result as a direct
call, including metadata such as shell exit-status lines.

Independent nested calls can use the preimported `Concurrently` applicative:

```haskell
do
  (readme, agents) <- runConcurrently $ (,)
    <$> Concurrently
      (callTool "read_file" (object ["target_file" .= ("README.md" :: Text)]))
    <*> Concurrently
      (callTool "read_file" (object ["target_file" .= ("AGENTS.md" :: Text)]))
  emitText (Text.pack (show (Text.length readme + Text.length agents)))
  pure ()
```

The harness still applies each tool's execution policy: read-only tools marked
parallel-safe overlap, while stateful tools remain serialized. Only use
`Concurrently` for independent calls because serialized calls submitted this
way do not have a defined execution order.

The GHCi process is not OS-sandboxed. Arbitrary Haskell IO can bypass
`callTool`, so every `run_haskell_program` call requires approval and the tool
is unavailable while Plan Mode is active. Pass `--no-haskell-program` to omit
the tool and its prompt guidance, including from spawned subagents.

### Benchmarking

`agent-benchmark` runs paired real-model sessions against generated fixtures.
It compares direct tools, optional Haskell orchestration, forced Haskell
orchestration, and an optional forced-shell control. The runner records exact
answer correctness, wall time, provider-reported tokens, top-level tool calls,
visible tool-output bytes, and privacy-canary exposure:

```console
nix develop
agent="$(cabal list-bin agent-cli:exe:agent-cli)"
cabal run agent-benchmark -- \
  --agent "$agent" \
  --provider openai \
  --model gpt-5.6-sol \
  --effort medium \
  --repetitions 3
```

Results are written to a timestamped directory under `benchmark-results/` as
`runs.jsonl`, per-run logs, generated fixtures, and a Markdown summary. Arm
order rotates for each task and repetition. The default tasks include a
privacy canary, a multi-file fan-out/reduce workload, and a simple negative
control where programmatic orchestration should not help.

OpenAI sessions compact automatically at the selected model's default context
threshold. Pass `--compact-threshold N` to override it in estimated tokens, for
example `--model gpt-5.6-luna --compact-threshold 120000`.

Terminal animation is controlled independently of color with
`--motion full|reduced|off`. `reduced` keeps stable semantic glyphs with
coarser elapsed-time updates; `off` removes cosmetic animation and retains only
one-second semantic timer refreshes. Terminal-native indeterminate progress is
also disabled outside full-motion mode.

Ghostty receives native progress, notifications, semantic turn boundaries,
working-directory updates, inline images, synchronized picker redraws, and
terminal clipboard support. See `docs/ghostty.md`; run `/terminal` inside the
CLI to inspect the detected capabilities.

## Skills

The CLI discovers reusable Agent Skills from `SKILL.md` files. It scans
`.agents/skills`, `.grok/skills`, and `.codex/skills` in each directory from
the repository root to the current working directory, plus the matching
directories under the user home. Repository skills take priority over user
skills for bare invocation names; colliding definitions remain available
through qualified names shown by `/skills`.

Each skill is a directory whose `SKILL.md` starts with YAML frontmatter:

```markdown
---
name: commit
description: Create a well-formed commit. Use when the user asks to commit changes.
---

Review the diff, run relevant checks, and create the commit.
```

Skill names appear in the interactive slash menu. Invoke one with
`/commit optional arguments`, mention one in a prompt as `$commit`, or let the
model select it from its description. `/skills` lists the active catalog and
`/skills reload` rescans disk. Use `--no-skills` to disable discovery and
invocation for a session.

Skill scripts, references, and assets remain relative to the skill directory
and are loaded only when needed. Skill-specific model overrides and
`allowed-tools` auto-approval are parsed for compatibility but are not applied;
normal model selection, permissions, and plan-mode restrictions remain in
force.

Build and run the CLI directly with Nix:

```console
nix flake check
nix run .
```

The imported OpenAI package also retains the headless ChatGPT login executable:

```console
nix run .#agent-openai-login -- --output ~/.codex/auth.json
```
