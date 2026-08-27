# agent-responses

Provider-neutral protocol support for Responses-compatible model transports.

- canonical types supplied by `agent-responses-types`
- direct typed JSON codecs with no generic JSON tree
- bounded incremental SSE framing shared by provider transports
- streamed response merging
- loop adapters for stateless Responses transports

SSE transports allocate one scoped, single-threaded Hermes decoder session per
response stream. Unknown fields are retained as validated `RawJson` bytes.
