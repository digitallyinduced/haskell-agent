# agent-core

Provider-neutral infrastructure shared by the harness transports:

- `Agent.Error` defines the common transport and provider error channel.
- `Agent.Provider` owns credentials, account-failure feedback, and failover.
- `Agent.Dialect` provides the stable identity and static vocabulary used to
  select model-facing contracts separately from provider transport. Concrete
  Codex and Grok Build implementations live in `agent-codex-dialect` and
  `agent-grok-build-dialect`.
- `Agent.Loop` runs the provider-neutral tool-calling agent loop. Transport
  adapters live in `agent-openai` (`Agent.OpenAI.LoopBackend`), `agent-xai`
  (`Agent.XAI.LoopBackend`), and `agent-openrouter`
  (`Agent.OpenRouter.LoopBackend`).
- `Agent.ToolArgs` parses model-supplied JSON tool arguments.
- `Agent.ToolDSL` owns JSON Schema fragments for function-tool parameters.
- `Agent.ToolDispatch` decodes and runs provider-neutral application tools.
- `Agent.Tools.Types`, `Agent.Tools.IO`, `Agent.Tools.Ghci`, and related
  modules provide dialect-neutral execution primitives. Concrete tool names,
  schemas, prompts, and resource composition belong to dialect packages.
- `Agent.Transport.WebSocket` owns reusable WebSocket sessions, ping/pong
  handling, STM-scoped request ownership, serialized writes, bounded receive
  buffering, and provider-neutral failure classification. Interrupted or
  unfinished exchanges poison the session so abandoned frames cannot leak
  into its successor.

This package does not contain OpenAI, ChatGPT, xAI, or OpenRouter transport logic.
