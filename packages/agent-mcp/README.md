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
- `Agent.Tools.Secret` provides scoped, owner-only temporary secret files for
  trusted host prompts. It keeps secret values out of model-visible tool
  arguments and results, but does not sandbox same-user processes from the
  returned path.
- `Agent.Transport.WebSocket` owns reusable WebSocket sessions, ping/pong
  handling, STM-scoped request ownership, serialized writes, bounded receive
  buffering, and provider-neutral failure classification. Interrupted or
  unfinished exchanges poison the session so abandoned frames cannot leak
  into its successor.

This package does not contain OpenAI, ChatGPT, xAI, or OpenRouter transport logic.

# Automatic artifact resources

Hosts can set `mcpHostArtifactDirectory` to a process-owned private directory.
The caller owns its cleanup and authorizes the directory for consuming sessions.
A tool result can request bounded materialization with:

```json
{"type":"resource_link","uri":"opaque:reference","name":"report.pdf","size":123,
 "_meta":{"dev.haskell-agent/artifact":true}}
```

The client reads this URI using `resources/read` on the same MCP connection
(including its authentication), never as a URL. The response must contain exactly
one matching resource with a base64 `blob`, no text, and the exact declared byte
length. Files are uniquely named, owner-only, and atomically published. Failed
batches remove files already produced. At most eight artifacts and 16 MiB decoded
bytes are accepted per tool response; the existing 16 MiB transport response limit
also applies to base64 resource responses. Ordinary untagged resource links are
not fetched automatically. No artifacts are fetched from failed tool results.
