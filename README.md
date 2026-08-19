# haskell-agent

A universal coding-agent harness written in Haskell.

## Packages

- `agent-cli` is the command-line entry point (`-p` for one-shot, otherwise a REPL).
- `agent-core` provides provider-neutral credentials, broker failover, common
  errors, tool dispatch, and transport utilities under the `Agent.*` namespace.
- `agent-openai` provides the OpenAI/ChatGPT Responses transports, authentication,
  streaming, and tool-call types under the `Agent.OpenAI.*` module namespace.
- `agent-xai` provides Grok request mapping, OAuth login, HTTP SSE transport,
  and stateful sessions under the `Agent.XAI.*` module namespace.

## Development

All compiler and package dependencies come from the pinned Nix flake.

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

Without `-p` / `--prompt-file` the CLI starts a REPL. Credentials come from
`~/.grok/auth.json` / `GROK_ACCESS_TOKEN` (xAI) or `~/.codex/auth.json` /
`CODEX_ACCESS_TOKEN` (OpenAI). `--provider` overrides auto-detection.

Build and run the CLI directly with Nix:

```console
nix flake check
nix run .
```

The imported OpenAI package also retains the headless ChatGPT login executable:

```console
nix run .#agent-openai-login -- --output ~/.codex/auth.json
```
