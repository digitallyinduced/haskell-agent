# agent-json-hermes

Hermes/simdjson backend for `agent-json-codec`.

Sessions are mutable and single-threaded. Allocate one session per concurrent
stream with `withDecoderSession`.

Hermes 0.8 exposes only `raw_json_token()`, which is not a complete nested
array or object. Codecs that retain `RawJson` therefore use the portable direct
backend until Hermes exposes a complete dependent object fold/raw-value API.
