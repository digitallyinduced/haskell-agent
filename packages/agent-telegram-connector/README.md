# Telegram session connector

`runTelegramConnector` owns supervised polling, durable inbox processing,
session execution reconciliation, outgoing delivery, and progress scheduling.
The private-conversation policy rejects group, bot, and mismatched-sender
updates before application admission. Each worker needs its own database
connection.

Supply a `SessionBackend`, persistence adapters, and a transaction-scoped
`ConversationApplication`. The application factory receives the inbox
transaction context, so authorization, message publication and execution intent
can commit together. Application adapters still own account linking, document
storage, transcription policy, and the meaning of admitted events. This is not
a claim that all application-specific event logic disappears.

For an application with existing durable stores, the service entry point is:

```haskell
runBot telegram server inbox application executions deliveries progress report =
    runTelegramConnector TelegramConnector
        { connectorClient = telegram
        , connectorInbox = inbox
        , connectorApplication = application
        , connectorExecutions = executions
        , connectorBackend = serverSessionBackend server 16000
        , connectorDeliveries = deliveries
        , connectorProgress = progress
        , connectorReportFailure = report
        }
```

Here `application` constructs the authorization/publication adapter from the
transaction context, not from a connection captured before the transaction.
The stores implement the documented atomic claims; they are not replacement
polling loops. `PostgreSQL` supplies inbox persistence for new installations;
existing applications can retain their own message/execution tables.

## Composing a PostgreSQL application

The following composition sketch names the application policies explicitly.
`authorize`, `linkAccount`, and `capabilities` operate on the connection passed
by the inbox factory; `executions` is the application's durable execution
adapter. Open distinct polling, inbox, delivery, execution and progress
connections before starting the service.

```haskell
import Agent.Telegram.Connector
import Agent.Telegram.Connector.Input
import Agent.Telegram.Connector.PostgreSQL
import Agent.Telegram.Connector.Server

runApplication telegram server pollingDb inboxDb deliveryDb
        executions authorize linkAccount capabilities
        authorizeDelivery decodePart progress report =
    runTelegramConnector TelegramConnector
        { connectorClient = telegram
        , connectorInbox = postgreSQLInboxStore pollingDb inboxDb "my-bot"
        , connectorApplication = \transaction -> ConversationApplication
            { authorizeConversation = authorize transaction
            , linkConversation = linkAccount transaction
            , recordConversationEvent = \binding _updateId ->
                processConversationInput (capabilities transaction binding)
            }
        , connectorExecutions = executions
        , connectorBackend = serverSessionBackend server 16000
        , connectorDeliveries = postgreSQLDeliveryStore
            (PostgreSQLStore deliveryDb "my-bot")
            authorizeDelivery decodePart
        , connectorProgress = progress
        , connectorReportFailure = report
        }
```

`capabilities transaction binding` returns `InputCapabilities`: the application
supplies duplicate lookup, transcription, durable media storage, message plus
execution-intent publication, notices, linking, cancellation, edits, reactions
and callbacks. The shared input pipeline selects the right capability, skips
duplicate media work, validates prepared text and publishes it. It never
starts a second agent turn for an edit or reaction by itself. Publication must
use `transaction`, so a failed inbox update rolls back its message and intent
together.

`authorizeDelivery` must lock and recheck the current binding revision while
running its supplied send action; `decodePart` decodes previously persisted
chunk payloads rather than resegmenting text. Call
`initializePostgreSQLStore` explicitly during deployment, not every startup.
If Telegram is not configured or identity verification fails, the application
can instead call `runSessionConnector executions (serverSessionBackend server
16000) report` to keep web-originated executions running.

`Server.serverSessionBackend` reconciles remote turns by stable client request
ID and persisted turn ID. Commit session creation before submission. Never
replace an existing request ID after an uncertain response.

Control adapters validate owner/chat/binding revision, exact
session/execution/turn, callback message and expiry. They must lock the current
binding and token, validate and commit one-use consumption before remote IO.
The shared session backend rechecks the exact pending human request.
Uncertain responses do not authorize replay.

Delivery adapters persist chunk boundaries and commit a sending claim before
the single transport attempt. Only positive receipts acknowledge a part;
uncertain sends are not automatically retried.

The standalone Telegram application shares the ordered conversation queue,
session lifecycle, and media normalization. Its group allowlists, commands,
drafts and local human-request bridge remain standalone-specific adapters.

The PostgreSQL tests start an isolated temporary cluster and require `initdb`
and `pg_ctl` on `PATH`; they never use an application database.
