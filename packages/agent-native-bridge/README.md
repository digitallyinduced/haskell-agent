# Native bridge

The C ABI is declared in `include/HaskellAgentBridge.h`. Its implementation
modules under `ffi/Agent/CLI/MacOS` are private; moving an endpoint between them
must preserve exported C symbols, callback argument order, and buffer lifetime.

Repository boundaries:

- `RepositoryChecks` owns check-handle publication, callback adaptation,
  cancellation, and destruction as one lifecycle.
- `RepositoryDeliveryBridge` owns delivery-status, push, and pull-request
  callbacks, including each operation's single terminal-callback gate.
- `RepositoryInput` copies bounded required UTF-8 fields before asynchronous
  work begins.
- `RepositoryWorkers` remains the shared worker registry and callback-thread
  boundary. Splitting endpoint modules must not create separate registries.

The `Bridge` module still contains local review (snapshot/diff/mutation)
endpoints and engine/session orchestration. Repository process, locking,
snapshot, and confirmation policies belong to `agent-repository`, not the FFI
adapters.
