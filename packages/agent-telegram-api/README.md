# Telegram application components

`agent-telegram-api` provides the same Telegram transport, wire decoders,
Markdown rendering, text segmentation, reaction descriptions, and scoped typing
notifications used by `agent-telegram`, without depending on the agent runtime,
provider credentials, local sessions, or database implementation.

Applications retain ownership of account authorization, durable update offsets,
message identifiers, conversation mapping, and delivery intent. Import
`Agent.Telegram.Types.Wire` for `telegramUpdateDecoder`; decoding an update does
not authorize its sender. Validate the sender and chat before processing content.

Use `Agent.Telegram.Progress.withTelegramProgressUsing` around an active operation,
supplying `sendTypingAction` and the appropriate draft operation. Its child thread
is cancelled and joined when the operation exits.

Use `telegramRequestOnce` when a durable outbox owns retries. The legacy
`telegramRequest` and high-level delivery functions perform bounded retries.
An error without an error code indicates an uncertain result: do not automatically
repeat a non-idempotent send. Error codes 500 and above can also be uncertain.
The success value is the raw response body, not a decoded method result. Callers
must validate the envelope and required result fields before acknowledging a
delivery; malformed or incomplete responses are uncertain, not safe to retry.
For an application-owned outbox, render with `markdownToTelegramHtml` and send
through the single-attempt operation; only use a plain-text fallback after a
definitive Telegram parse rejection, not after a transport failure.

`withTelegramVoiceTranscript` accepts acquisition, release and transcription
callbacks. It checks duration and declared size before acquisition, brackets the
resource lifetime, and rejects empty transcripts. Acquisition must enforce the
actual byte limit and remove partial files if it fails. The transcription callback
must honor the application's provider or organization-gateway configuration;
the library never selects ambient credentials.

The library does not start polling, create agent sessions, or provide application
authorization. Running `agent-telegram` alongside another update consumer for the
same bot is not an embedding mechanism.
