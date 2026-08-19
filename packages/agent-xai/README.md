# agent-xai

xAI-specific transport package for the agent harness.

- Projects canonical `ResponseCreateParams` values into the Grok subscription
  proxy dialect while keeping the request typed.
- Decodes SSE into the canonical typed `ResponseStreamEvent` union and assembles
  the terminal `Response`.
- Implements xAI device authorization, token refresh, and account-id derivation.

`Agent.XAI.LoopBackend` implements the provider-neutral `Backend` used by
`Agent.Loop`. The proxy does not store transcripts, so the adapter keeps a
local item list and resends it on each turn.

The package contains no agent loop, context trimming, retry policy, or
credential failover. Those concerns belong to the harness around the provider
client.

OAuth login options require the application's public client id at runtime;
`agent-xai` does not embed one in its source.

```haskell
import Agent.OpenAI.Responses.Types
import Agent.XAI

request = defaultResponseCreateParams
    { model = Just "grok-4.5"
    , input = Just (ResponseInputText "Hello")
    }

main = createResponse credential request >>= print
```

`agent-xai` depends on `agent-openai` for the canonical Responses wire model,
OpenAI-compatible error decoding, typed stream events, and response merging.
Provider credentials and common errors come from `agent-core`.
`agent-openai` does not depend on `agent-xai`.
