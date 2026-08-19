# haskell-agent

A universal coding-agent harness written in Haskell.

## Packages

- `agent-cli` is the command-line entry point.
- `agent-core` provides provider-neutral credentials, broker failover, common
  errors, tool dispatch, and transport utilities under the `Agent.*` namespace.
- `agent-openai` provides the OpenAI/ChatGPT Responses transports, authentication,
  streaming, and tool-call types under the `Agent.OpenAI.*` module namespace.
- `agent-xai` provides Grok request mapping, OAuth login, HTTP SSE transport,
  and stateful sessions under the `Agent.XAI.*` module namespace.
- `agent-openrouter` provides OpenRouter static API-key auth, HTTP SSE
  transport, and a local-transcript loop backend under the
  `Agent.OpenRouter.*` module namespace.

## Development

All compiler and package dependencies come from the pinned Nix flake.

```console
nix develop
cabal build all
cabal test all
```

Build and run the CLI directly with Nix:

```console
nix flake check
nix run .
```

The imported OpenAI package also retains the headless ChatGPT login executable:

```console
nix run .#agent-openai-login -- --output ~/.codex/auth.json
```
