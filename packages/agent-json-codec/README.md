# agent-json-codec

Direct JSON codecs for the agent harness.

The public API intentionally has no generic JSON value or object type. Encoders
write domain values directly to bytes, while decoders consume JSON bytes
directly into domain values. Unknown forward-compatible fields can be retained
as opaque, validated `RawJson` values.

```haskell
encode :: Encoder a -> a -> ByteString
decode :: Decoder a -> ByteString -> Either DecodeError a
```

`Encoder` is an abstract exact-size write program backed by Jsonifier. Its
internal write plan is a size-and-poke program, not a semantic JSON value, and
is never exposed.
`RawJson` constructors are private; the unchecked raw writer can therefore
only receive bytes previously accepted by `validateRawJson` or captured by a
decoder.

The portable decoder is a strict `ByteString` cursor parser. It validates the
complete input, supports scalar roots, tracks paths, limits nesting, implements
last-key-wins duplicate behavior, and constructs only the requested domain
type. Object codecs declare known fields plus one explicit unknown-field
decoder.

The optional C++/simdjson backend lives in the separate
`agent-json-hermes` package so this package remains portable.
