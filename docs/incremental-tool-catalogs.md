# Incremental Responses Lite tool catalogs

Responses Lite templates advertise their tools as `additional_tools` input items.
The OpenAI backend now commits these declarations alongside the conversation
and keeps a provider-owned catalog snapshot. Subsequent requests append only
new or changed definitions, a single namespace-update explanation per batch,
and explicit removal or namespace-instruction notices. Unchanged definitions
and base instructions are not appended again.

The implementation follows Codex's
[immutable top-level tool state](https://github.com/openai/codex/blob/6326163b/codex-rs/core/src/context/world_state/top_level_tools.rs)
and [batched namespace notices](https://github.com/openai/codex/commit/0b755b19).
Catalog state is explicit checkpoint metadata, not reconstructed from hidden
transcript messages. Notices are real model-facing context; their local
attribution is stripped from wire requests.

## Lifecycle and compatibility

- The capability boundary is a known Responses Lite model **and** a typed
  `additional_tools` request template. Ordinary Responses and other providers
  do not send incremental catalog updates.
- Catalog candidates become authoritative only with the successful backend
  snapshot. Failed/discarded submissions do not mutate shared catalog state.
  Native interrupted responses retain their accepted request context.
- Full replay preserves the historical sequence of declarations and updates.
  Clearing only a connection continuation does not reset catalog state.
- Missing/invalid metadata (including old sessions), model changes, and
  replaced/compacted history rebase onto the current complete catalog and
  discard the old continuation. Compaction requests use the complete current
  catalog rather than obsolete historical definitions.
- Other backends omit Lite-only model context and clear its provider state.
  User messages, completed tool calls, and results remain portable.
- Session metadata is optional for compatibility with older persisted data.

Regression tests cover catalog diffs, unchanged continuations, append-only
updates, failed submission, native interruption, reconnect replay, legacy
snapshots, compaction, and switching back to ordinary Responses. No measured
latency or cache-hit improvement is claimed.
