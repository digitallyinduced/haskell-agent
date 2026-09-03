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
function, and namespace tools with realistic JSON schemas and a custom grammar.

The `tool-shell` and `tool-json` modes measure the production live tool-argument
projector. They split a fixed-size argument into configurable deltas and replay
the complete added/delta/done sequence through a fresh
`newStreamEventToLoopEvents` projector for every repetition. `tool-shell`
exercises semantic shell-command previews; `tool-json` is the generic safe JSON
control workload.

Build with optimisation and enable allocation statistics:

```console
nix develop -c cabal build -O2 agent-responses:bench:responses-json-bench
bin=$(nix develop -c cabal list-bin -O2 \
  agent-responses:bench:responses-json-bench)
"$bin" stream 160 16 9 +RTS -T
"$bin" stream 160 1024 9 +RTS -T
"$bin" stream 1000 16 9 +RTS -T
"$bin" stream 1000 1024 9 +RTS -T
"$bin" request 10000 9 +RTS -T
"$bin" tool-shell 4096 1 3 9 +RTS -T
"$bin" tool-json 65536 1 3 9 +RTS -T
```

Each CSV row is:

```text
mode,count,delta-bytes,samples,median-wall-ms,median-cpu-ms,median-Haskell-allocated-bytes,median-maximum-live-bytes,checksum
```

Inputs are built and forced before stream timing. Every sample performs a GC
before timing, then another after the clocks stop so that sub-nursery workloads
are reflected in RTS `allocated_bytes`.

For the tool modes, `count` is the argument-body size and the mode suffix records
the number of complete projector repetitions (for example, `tool-shell-x3`).

## Representative result

Apple M3 Max, 36 GiB RAM, macOS 26.6.1, GHC 9.10.3, `-O2`,
2026-08-27:

| Mode | Count | Delta | Median wall | Median CPU | Median Haskell allocation |
|:---|---:|---:|---:|---:|---:|
| Hermes stream decode + assembly | 160 | 16 B | 0.219 ms | 0.219 ms | 951,272 B |
| Hermes stream decode + assembly | 160 | 1 KiB | 0.466 ms | 0.467 ms | 1,545,992 B |
| Hermes stream decode + assembly | 1,000 | 16 B | 1.263 ms | 1.258 ms | 5,410,096 B |
| Hermes stream decode + assembly | 1,000 | 1 KiB | 3.211 ms | 3.205 ms | 17,608,760 B |
| Aeson realistic request encoding | 10,000 | — | 131.377 ms | 131.293 ms | 717,878,040 B |

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
