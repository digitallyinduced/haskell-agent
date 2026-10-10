# Asynchronous clarification

The Codex dialect exposes `request_user_input_async`:

```json
{"questions":[{"title":"Which color should I use?","options":["Blue","Red"]},{"title":"Any accessibility requirements?"}]}
```

The tool publishes the questions and returns `{"accepted":true}` without
waiting for an answer. The user replies in the ordinary chat composer. During
an active turn, that reply uses the existing steering path; after the turn
finishes, it starts an ordinary new turn. There is no polling tool, pending
answer worker, special reply identifier, or exclusive ownership of stdin.

Use it when useful independent work remains. `ask_user_question` is unchanged
and remains appropriate when progress must wait for an answer. Neither tool
replaces approval for a protected action.

Interactive CLI sessions display the questions in the transcript and send the
existing attention notification. Native hosts receive assistant text through
their loop-event callback. Questions and suggested answers also remain in the
persisted tool-call arguments. Transcript replay reconstructs readable question
titles and suggestions from those arguments; the immediate display notice is
not a separate synthetic provider message. Unattended/background and one-shot terminal
hosts return an unavailable error rather than silently accepting a question
that cannot be answered there. Embeddings constructing tools directly can
install a nonblocking delivery callback with `setAsyncQuestionDelivery`.

This follows Codex's
[`request_user_input_async` handler at 806d9732](https://github.com/openai/codex/blob/806d9732/codex-rs/core/src/tools/handlers/request_user_input_async.rs):
publish immediately, acknowledge delivery, and accept a later ordinary user
message. Our terminal presents suggestions as text rather than a blocking
multiple-choice overlay.
