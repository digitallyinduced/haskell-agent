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
