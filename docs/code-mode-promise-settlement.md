# Code-mode promise settlement

JavaScript code-mode cells provide `as_settled` and `stream_settled` for
processing independent operations in settlement order rather than waiting for
every operation to finish.

```javascript
const calls = new Map([
  ["first", tools.read_file({ target_file: "first.txt" })],
  ["second", tools.read_file({ target_file: "second.txt" })],
]);

await stream_settled(calls, result => {
  if (result.status === "fulfilled") {
    text({ source: result.index, value: result.value });
  } else {
    text({ source: result.index, error: String(result.reason) });
  }
  yield_control();
});
```

`as_settled(inputs)` returns an async iterator. Inputs may be a synchronous
iterable of promises, thenables, or ordinary values. Results have one of these
shapes:

- `{ index, status: "fulfilled", value }`
- `{ index, status: "rejected", reason }`

For a `Map`, `index` is the original key; for other iterables it is the zero-based
position. Already settled inputs are observed in iteration order. Settlements
that arrive while the consumer is occupied remain queued in observation order.
Each input is observed once.

`stream_settled(inputs, emit)` consumes that iterator and awaits each callback
before invoking the next. An input rejection becomes a result; a callback
failure rejects `stream_settled`. Empty input completes without a callback.

Neither helper emits model-visible output automatically. Use `text` to append
output and `yield_control` to expose partial output immediately; the cell
continues running and can be observed through `wait`.

Breaking iteration, returning the iterator, or a callback failure releases the
settlement queue. It does not cancel underlying tool operations. Queues are also
closed when the cell finishes; existing cell cancellation and tool-lifetime
rules remain unchanged. The helpers do not persist across cells.

The API follows the Codex
[promise-settlement helpers](https://github.com/openai/codex/commit/28a264fbc766a59f2b550b8318f88e4a4b8dffe1)
and uses cell-local JavaScript state. In addition, explicit iterator return
closes a queue even before its first `next()` or while `next()` is waiting.
