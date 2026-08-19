# agent-core

Provider-neutral infrastructure shared by the harness transports:

- `Agent.Error` defines the common transport and provider error channel.
- `Agent.Provider` owns credentials, account-failure feedback, and failover.
- `Agent.Broker` obtains provider credentials from the central broker.
- `Agent.Loop` runs the provider-neutral tool-calling agent loop.
- `Agent.ToolArgs` parses model-supplied JSON tool arguments.
- `Agent.ToolDSL` owns JSON Schema fragments for function-tool parameters.
- `Agent.ToolDispatch` decodes and runs provider-neutral application tools.
- `Agent.Transport.WebSocket` owns reusable WebSocket sessions, ping/pong
  handling, serialized writes, bounded receive buffering, and provider-neutral
  failure classification.

This package does not contain OpenAI, ChatGPT, or xAI transport logic.
