# agent-gemini

Native Google Gemini transports for Code Assist and the AI Studio
GenerateContent API.

- Supports browser-based Google OAuth and Gemini Code Assist without an API
  key. Selecting a Gemini model from `/model` starts sign-in when needed.
- Also supports `GOOGLE_API_KEY`, with `GEMINI_API_KEY` as a fallback, for
  Google AI Studio API billing.
- Projects the provider-neutral Responses request model to native
  `streamGenerateContent` requests.
- Streams text, thoughts, thought signatures, function calls, and usage into
  the shared agent loop.
- Maps the portable hosted-search tool to Gemini's native Google Search and
  adapts freeform harness tools such as `apply_patch` to function calls.
- Preserves Gemini thought signatures across tool-call continuations.

The CLI ships `gemini-3.7-flash` as the default, plus
`gemini-3.1-pro-preview` and `gemini-3.5-flash-lite`. Each model has a
1,048,576-token input context in the bundled catalog.

```console
/model
```

OAuth credentials are stored in haskell-agent's private managed credential
store and refreshed automatically. API keys are supplied through the
`x-goog-api-key` request header and are never written to the model catalog.
