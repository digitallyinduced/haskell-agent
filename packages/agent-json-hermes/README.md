# agent-json-hermes

Hermes/simdjson backend for `agent-json-codec`.

Sessions are mutable and single-threaded. Allocate one session per concurrent
stream with `withDecoderSession`.

This package currently pins the small Hermes extension proposed upstream in
[velveteer/hermes#33](https://github.com/velveteer/hermes/pull/33). It adds a
stateful dependent object fold and complete `raw_json()` access, allowing typed
fields and opaque nested extensions to be decoded in one forward pass.
