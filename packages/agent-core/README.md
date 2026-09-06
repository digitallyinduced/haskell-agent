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
  (`Agent.XAI.LoopBackend`), `agent-openrouter`
  (`Agent.OpenRouter.LoopBackend`), and `agent-gemini`
  (`Agent.Gemini.LoopBackend`).
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

## Loop implementation boundaries

`Agent.Loop` remains the public facade. Its private implementation modules are:

- `Loop.Input`: turn inputs, attachments, and prompt-image normalization.
- `Loop.TokenUsage`: token accounting and generation-rate estimates.
- `Loop.Output`: completed responses and the live event protocol.
- `Loop.Backend`: provider callbacks and immutable checkpoint contracts.
- `Loop.DisplayJournal`: bounded live-event projection and display-only history
  of uncommitted attempts. This history must never enter backend/model state.
- `Loop.EventPump`: generic bounded, single-consumer event delivery.
- `Loop.Internal`: response progression and scoped tool-worker ownership.

Keep cancellation, commit ordering, and worker lifetimes together in the
execution module; the leaf modules do not own worker lifetimes.

## Backend middleware

`Backend` represents one provider/model submission. The corresponding
middleware type is deliberately just a function:

```haskell
type BackendMiddleware = Backend -> Backend
```

Middleware therefore composes with ordinary `(.)`, uses `id` as its empty
value, and needs no framework-specific combinator:

```haskell
middleware :: BackendMiddleware
middleware =
    addPendingInputs
        . compactContext
        . recoverConnections

backend :: Backend
backend = middleware providerBackend
```

The leftmost middleware is outermost: it sees the request first and the
result last. A `BackendMiddleware` wraps only the provider step, including its
streamed events and returned tool calls. Tool approval and execution happen
later in `Agent.Loop`, outside this boundary. Consequently, retry middleware
can replay a provider submission without replaying tools that the host has
already completed.

This package does not contain OpenAI, ChatGPT, xAI, OpenRouter, or Gemini
transport logic.
