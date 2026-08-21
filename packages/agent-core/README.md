# agent-core

Provider-neutral infrastructure shared by the harness transports:

- `Agent.Error` defines the common transport and provider error channel.
- `Agent.Provider` owns credentials, account-failure feedback, and failover.
- `Agent.Broker` obtains provider credentials from the central broker.
- `Agent.Loop` runs the provider-neutral tool-calling agent loop. Transport
  adapters live in `agent-openai` (`Agent.OpenAI.LoopBackend`), `agent-xai`
  (`Agent.XAI.LoopBackend`), and `agent-openrouter`
  (`Agent.OpenRouter.LoopBackend`).
- `Agent.ToolArgs` parses model-supplied JSON tool arguments.
- `Agent.ToolDSL` owns JSON Schema fragments for function-tool parameters.
- `Agent.ToolDispatch` decodes and runs provider-neutral application tools.
- `Agent.Tools` / `Agent.Tools.Grok` register grok-build coding tools
  (`read_file`, `grep`, `list_dir`, `search_replace`, `run_terminal_cmd`).
- `Agent.Tools.Codex` registers Codex tools (`shell_command`, `apply_patch`,
  `update_plan`). `apply_patch` is the Codex freeform patch language.
- `Agent.Tools.IO` owns path confinement, file IO, and timed shell commands.
- `Agent.Transport.WebSocket` owns reusable WebSocket sessions, ping/pong
  handling, STM-scoped request ownership, serialized writes, bounded receive
  buffering, and provider-neutral failure classification. Interrupted or
  unfinished exchanges poison the session so abandoned frames cannot leak
  into its successor.

This package does not contain OpenAI, ChatGPT, xAI, or OpenRouter transport logic.
