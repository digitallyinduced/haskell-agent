# PostgreSQL connector stores

New applications can use the packaged inbox and outbox without implementing
polling checkpoints, deduplication, claims, chunk receipts or uncertain-send
recovery themselves:

```haskell
let inbox = postgreSQLInboxStore pollingConnection inboundConnection "my-bot"
    outbox = PostgreSQLStore deliveryConnection "my-bot"
    deliveries = postgreSQLDeliveryStore outbox withCurrentBinding decodeStoredPart
    connector = TelegramConnector
        { connectorClient = telegramClient
        , connectorInbox = inbox
        , connectorApplication = application
        , connectorExecutions = executions
        , connectorBackend = backend
        , connectorDeliveries = deliveries
        , connectorProgress = progress
        , connectorReportFailure = reportComponentFailure
        }
runTelegramConnector connector
```

Run `initializePostgreSQLStore` once from a database migration. All connector
tables have RLS enabled without browser policies: use a private server database
role. The namespace identifies one bot's durable state, not an authorization
boundary or a substitute for tenant ownership checks.

Each connection above must be **distinct and dedicated**. The connector runs
polling, inbound processing and outbound delivery concurrently.

## Application responsibilities

`application :: Connection -> ConversationApplication binding` authenticates
the Telegram owner against your current binding and
projects admitted events into your conversation and durable execution records.
Its database publication must use the supplied transactional `Connection`
(the dedicated `inboundConnection`): inbox acknowledgement
and publication then commit together. Use `withConversationTransaction` for
standalone publication, or `withConversationLock` within an existing transaction
(including an inbox handler), for all channels submitting work to one conversation.
A busy lock must retain or durably queue the work, not silently acknowledge it.
Retain a stable execution
identity when reconciling an uncertain backend submission.

`executions` adapts your durable session/execution records. The selected
`SessionBackend` may be local or server-backed; neither inbox nor outbox tables
contain model credentials.

Publish replies with `enqueueDelivery` inside the same transaction as the
assistant message. Pass a stable delivery identifier, current binding revision,
chat identifier and the **already prepared, immutable JSON parts**.
`decodeStoredPart` converts one stored part into a connector delivery attempt
without changing its destination, formatting or boundaries.

`withCurrentBinding` has type:

```haskell
forall result. DeliveryRecord -> IO result -> IO (Maybe result)
```

It acquires your owner/binding lock, verifies that the saved binding revision
and chat are still current, and executes the supplied action while holding that
lock. Return `Nothing` when revoked. Unlink must acquire the same lock and invoke
`revokeBinding` in its transaction. Never authorize solely from a delivery ID.

The adapter commits the `sending` claim before invoking this scope, then
atomically records the receipt and advances its part index. If transmission or
receipt publication is uncertain, it does **not** retry automatically. A process
crash leaves `sending` durable for operator reconciliation, not replay.

## Human controls

Use cryptographically random callback tokens. `registerCallback` associates a
token with the exact owner, chat, binding revision, session, turn and pending
request, plus expiry. Under your current binding/request lock, `consumeCallback`
checks every scope component and consumes the token once.

Consume and publish the durable human-response intent in the **same database
transaction**. Only a separate recoverable backend action submits it remotely.
Rolling publication back also rolls consumption back. Never consume a token
and immediately make an unrecorded remote approval request.

## Existing applications

Keep existing tables behind `InboxStore`, `ExecutionStore` and `DeliveryStore`
adapters when adopting the connector. Do not copy pending rows into a second
queue, reset polling offsets, recreate backend execution IDs, re-split existing
delivery content or turn uncertain sends back into pending sends.
