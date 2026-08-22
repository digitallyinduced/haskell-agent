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

The package contains no agent loop, context trimming, or credential failover.
Those concerns belong to the harness around the provider client. The HTTP
client does retry short-lived capacity / overload failures (30s delay, a few
attempts) before returning them to the loop.

OAuth login options require the application's public client id at runtime;
`agent-xai` does not embed one in its source.

```haskell
import Agent.Responses.Types
import Agent.XAI

request = defaultResponseCreateParams
    { model = Just "grok-4.6"
    , input = Just (ResponseInputText "Hello")
    }

main = createResponse credential request >>= print
```

`agent-xai` depends on `agent-responses` for the canonical Responses wire
model, OpenAI-compatible error decoding, typed stream events, response
merging, and stateless loop adaptation. Provider credentials and common
errors come from `agent-core`.
