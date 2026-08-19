# agent-xai

XAI-specific transport package for the agent harness.

- Maps canonical `Agent.OpenAI.Responses.Types.ResponseCreateParams` values to the Grok
  subscription proxy dialect.
- Parses HTTP SSE responses back into canonical `Agent.OpenAI.Responses.Types.Response`
  values.
- Provides stateful transcript replay for `previous_response_id` semantics.
- Implements XAI device-code login, token refresh, and account-id derivation.

OAuth login options require the application's public client id at runtime;
`agent-xai` does not embed one in its source.

```haskell
import Agent.OpenAI.Responses.Types
import Agent.XAI.Grok

request = defaultResponseCreateParams
    { model = Just "grok-4.5"
    , input = Just (ResponseInputText "Hello")
    }

main = createGrokMessage credential request >>= print
```

`agent-xai` depends on `agent-openai` for the canonical Responses wire model,
OpenAI-compatible error decoding, and response merging. Provider credentials
and common errors come from `agent-core`; transcript trimming is private to
`agent-xai`. `agent-openai` does not depend on `agent-xai`.
