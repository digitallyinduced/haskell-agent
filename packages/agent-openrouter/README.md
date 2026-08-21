# agent-openrouter

OpenRouter transport package for the agent harness.

- Projects canonical `ResponseCreateParams` values onto OpenRouter's
  OpenAI-compatible Responses dialect while keeping the request typed.
- Forces `store = false` and omits `previous_response_id` — OpenRouter's
  Responses API is stateless and rejects server-side transcripts.
- Decodes SSE incrementally into the canonical typed `ResponseStreamEvent`
  union, delivers callbacks while the response is still arriving, and
  assembles the terminal `Response`.
- Authenticates with a static OpenRouter API key.

`Agent.OpenRouter.LoopBackend` implements the provider-neutral `Backend`
used by `Agent.Loop`. Because the host does not store transcripts, the
adapter keeps a local item list and resends it on each turn.

The package contains no agent loop, context trimming, retry policy, or
credential failover. Those concerns belong to the harness around the
provider client.

```haskell
import Agent.OpenAI.Responses.Types
import Agent.OpenRouter
import Agent.Provider (Credential(..), Provider(..))

credential = Credential
    { accessToken = openRouterApiKey
    , accountId = ""
    , leaseId = Nothing
    , provider = OpenRouterProvider
    }

request = defaultResponseCreateParams
    { model = Just "openai/gpt-5.1"
    , input = Just (ResponseInputText "Hello")
    }

main = createResponse credential request >>= print
```

`agent-openrouter` depends on `agent-openai` for the canonical Responses
wire model, error decoding, typed stream events, and response merging.
Provider credentials and common errors come from `agent-core`.
`agent-openai` does not depend on `agent-openrouter`.
