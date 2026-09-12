# agent-responses

Provider-neutral protocol support for Responses-compatible model transports.

- canonical types supplied by `agent-responses-types`
- JSON codecs
- bounded incremental SSE framing shared by provider transports
- streamed response merging
- loop adapters for stateless Responses transports

## Streaming pipeline

The shared Responses HTTP/SSE path uses Conduit:

```text
timeout-aware body reader → decodeSseC → assembleResponseC
```

`consumeResponsesSse` composes these stages. The HTTP transport keeps its
`withResponse` bracket around the whole pipeline; exceptions and cancellation
unwind that scope. No background worker or extra queue is introduced. The sink
delivers events in wire order before applying the existing assembly transition
and stops requesting input at a terminal event.

The decoder retains the existing SSE limits, UTF-8 validation, malformed-JSON
skip policy, and final undelimited-event handling on normal EOF. Validation
remains per input chunk: a hard decoding error prevents delivery of events from
that chunk, even if it also contains a terminal event.

Tool approval/execution, retry policy, recovery journals, checkpoint commits,
and UI buffering remain outside this pipeline. WebSocket and other provider
transports are unchanged.
