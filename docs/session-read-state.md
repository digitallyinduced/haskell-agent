# Durable session read state

The authenticated server API exposes one shared read receipt per session:

* `GET /v1/sessions/:id/read-state` returns `revision` (opaque string),
  `unread` (boolean), and `first_unread_turn` (nullable zero-based turn index).
* `PATCH /v1/sessions/:id/read-state` accepts
  `{"expected_revision":"…","unread":false}` to acknowledge the displayed
  snapshot, or `unread:true` to mark the whole conversation unread.
* A stale revision returns HTTP 409 with the current snapshot in error details.
  Clients must refresh, not automatically retry the acknowledgement.

Live appended conversation turns mark the session unread atomically with their
history commit. Each new result changes the revision, including when an earlier
result is already unread. Replacing metadata, compaction, history reads, imports,
and forks do not manufacture new unread results. Read receipts do not alter the
session activity timestamp. A manually unread conversation can have no turns.

State lives in PostgreSQL and is shared by clients within the existing session
authorization boundary. Merely fetching history does not acknowledge it: clients
should acknowledge only a snapshot they actually displayed. The observation
socket remains read-only. Native/CLI unread indicators are not added by this API
change.

`GET /v1/sessions` also includes a `read_states` object keyed by the IDs in that
page, loaded in one batch query. Clients can discover unread sessions without
issuing a separate request per row. A session deleted during page loading may
have no receipt in this map.

The Haskell `agent-server-client` exposes `getAgentServerSessionReadState` and
`updateAgentServerSessionReadState` with typed receipt/request records. Pass the
displayed receipt's revision to the update; HTTP 409 remains an explicit client
error and is not retried.

This follows the revision-checked receipt design in
[Codex PR 52337](https://github.com/openai/codex/pull/52337), using existing
session persistence and authorization rather than a separate SQLite table.
