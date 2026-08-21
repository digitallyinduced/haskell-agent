# haskell-agent

A universal coding-agent harness written in Haskell.

## Packages

- `agent-cli` is the command-line entry point (`-p` for one-shot, otherwise a REPL).
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

Each package has a checked-in `package.nix` generated with `cabal2nix` (no IFD).
After changing a `.cabal` file, regenerate that package's Nix expression:

```console
(cd packages/agent-core && cabal2nix . > package.nix)
(cd packages/agent-responses && cabal2nix . > package.nix)
(cd packages/agent-openai && cabal2nix . > package.nix)
(cd packages/agent-xai && cabal2nix . > package.nix)
(cd packages/agent-openrouter && cabal2nix . > package.nix)
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
REPL, `:reload` writes `~/.haskell-agent/dev-resume`, returns to GHCi, reloads
modules, and resumes the same session automatically. `:q` exits the agent back
to `ghci>`.

Without `-p` / `--prompt-file` the CLI starts a REPL. Credentials come from
`~/.grok/auth.json` / `GROK_ACCESS_TOKEN` (xAI), `~/.codex/auth.json` /
`CODEX_ACCESS_TOKEN` (OpenAI), or `OPENROUTER_API_KEY` (OpenRouter).
`--provider` overrides auto-detection.

Ghostty receives native progress, notifications, semantic turn boundaries,
working-directory updates, inline images, synchronized picker redraws, and
terminal clipboard support. See `docs/ghostty.md`; run `/terminal` inside the
CLI to inspect the detected capabilities.

Build and run the CLI directly with Nix:

```console
nix flake check
nix run .
```

The imported OpenAI package also retains the headless ChatGPT login executable:

```console
nix run .#agent-openai-login -- --output ~/.codex/auth.json
```
