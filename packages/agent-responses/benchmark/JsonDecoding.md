# Responses JSON regression benchmark

This is a compact regression benchmark for the JSON paths that ship in
`agent-responses`. It is not a parser shoot-out.

The `stream` mode decodes a representative coding turn with Hermes, using one
reusable `withResponseStreamEventDecoder` session. It applies every decoded
event to the production typed stream assembler and forces a checksum of the
terminal `Response`. The stream includes `output_item` added/done events,
created/in-progress/completed lifecycle snapshots, large namespace and schema
payloads (including unknown fields), and this delta mix:

* 50% reasoning-summary text
* 30% assistant output text
* 10% function-call arguments
* 10% custom-tool input

The optional `request` mode measures the production Aeson request encoder. Its
input starts from `defaultResponseCreateParams` and contains typed custom,
function, and namespace tools.

Build with optimisation and enable allocation statistics:

```console
nix develop -c cabal build -O2 agent-responses:bench:responses-json-bench
bin=$(nix develop -c cabal list-bin -O2 \
  agent-responses:bench:responses-json-bench)
"$bin" stream 2000 64 9 +RTS -T
"$bin" request 10000 9 +RTS -T
```

Each CSV row is:

```text
mode,count,delta-bytes,samples,median-wall-ms,median-cpu-ms,median-Haskell-allocated-bytes,checksum
```

Inputs are built and forced before stream timing. Every sample performs a GC;
reported allocation is the difference in RTS `allocated_bytes`.

## Representative result

Apple M3 Max, 36 GiB RAM, macOS 26.6.1, GHC 9.10.3, `-O2`,
2026-08-27:

| Mode | Count | Delta | Median wall | Median CPU | Median Haskell allocation |
|:---|---:|---:|---:|---:|---:|
| Hermes stream decode + assembly | 2,000 | 64 B | 2.920 ms | 2.921 ms | 10,268,232 B |
| Aeson request encoding | 10,000 | — | 38.519 ms | 38.357 ms | 272,704,656 B |

These are regression reference points, not cross-mode comparisons. Machine
load, compiler, and dependency changes can move them.

## Historical context

The old Aeson measurements belong only to the PR history that motivated the
Hermes migration; they are not retained as a live comparison. For traceability,
that PR recorded Aeson baselines of 3.712 ms / 25,599,736 B and 21.174 ms /
136,385,344 B for its two prototype workloads. Those numbers are historical
only: the payloads and measured code path differ from this final benchmark.
Hermes is the production decoder and this benchmark guards the shipped codec
and typed assembly path.
