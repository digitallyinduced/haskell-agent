# Changelog for `agent-openai`

## Unreleased

- Add `Agent.OpenAI.LoopBackend`: map `TurnInput` / `TurnOutput` onto the
  Responses WebSocket transport, including `function_call` and
  `custom_tool_call` (Codex `apply_patch`).
- Remove embedded OAuth client identifiers. Login and token refresh now receive
  the public client id from application runtime configuration.
- Replace the streaming event discriminator/property bag with typed lifecycle,
  output-item, and error constructors plus a lossless fallback for other and
  future events. The WebSocket receiver now decodes each frame once and
  pattern-matches on `ResponseStreamEvent`.
- Move provider credentials, common errors, tool dispatch,
  JSON tool-argument parsing, and the reusable WebSocket session pump to
  `agent-core`; move Grok transcript trimming to `agent-xai`. `agent-openai`
  now contains only OpenAI wire, auth, and transport concerns.
- Isolate successful HTTP/SSE response-body decoding and failed-response
  normalization in `Agent.OpenAI.Http`.
- Add `createCodexMessageWithProviderAt` so the REST client can POST to any
  OpenAI-compatible Responses base URL, and `staticBearerProvider` for a
  brokerless service API key. Empty `accountId` credentials omit the
  ChatGPT-only `chatgpt-account-id` header. Successful JSON bodies are
  accepted in addition to SSE `response.completed` streams.
- Replace the package-specific request/response hierarchy with one lossless,
  wire-aligned OpenAI Responses API model shared by REST and WebSocket
  transports. Unknown object fields and union discriminators round-trip.
- Retry transient overload/server responses centrally for non-streaming
  WebSocket requests, while leaving dead-socket recovery to reconnect callers.
- Classify `service_unavailable_error` as a typed retryable provider failure.
- Normalize HTTP-200 `status: failed` responses into typed errors and retry
  transient overload/server failures with bounded exponential backoff.
- Add a small `TokenProvider` boundary shared by credential sources, plus
  provider-based REST and WebSocket entry points.
- Share account-failure classification and retry orchestration between REST
  and WebSocket transports, including replay-safe in-band WebSocket failover.
- Redact bearer and lease tokens from `Credential` debug output and guard local
  authentication recovery against repeated refresh-token rotation.
- Add opt-in WebSocket `context_management` with a server-side compaction
  threshold for long-running Responses conversations.
- Bound the WebSocket inbound frame queue and log per-response stream stats.
- Move noisy WebSocket request payload send logs to debug level.
- Added a typed `PreviousResponseNotFound` error classification for missing
  `previous_response_id` failures.
- Added client-initiated WebSocket pings in `withCodexWs`.

## 0.1.0.0 — 2026-04-23

Initial release. Extracted from the belege.ai application:

- `Agent.OpenAI.Responses.Types` — request/response types, tool definitions, computer-use actions.
- `Agent.OpenAI.Error` — `ApiError` / `ErrorType` and classification helpers.
- `Agent.OpenAI.Auth` — in-memory multi-account pool with round-robin, cooldown, JWT exp parsing, and a pure HTTP OAuth refresh. Persistence is a plain callback.
- `Agent.OpenAI.Client` — REST client with SSE parsing and rate-limit failover.
- `Agent.OpenAI.WebSocketClient` — WebSocket send/receive for streaming responses.
- `Agent.OpenAI.ToolDSL` — minimal `PropertySchema` builder for function-tool parameters.
