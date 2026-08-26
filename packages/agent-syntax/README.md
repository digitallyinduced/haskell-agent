# agent-syntax

Renderer-independent syntax highlighting for the universal agent harness.

## Syntax loading benchmark

`syntax-loading-bench` retains the previous eager-loading path as a baseline
and compares it with initialization plus on-demand grammar loading. Each sample
loads from `AGENT_SYNTAX_DIR`, highlights a generated source block with every
requested grammar to force the result, and performs a major GC. The
`on-demand-released` workload clears the runtime-style cache reference before
that collection; the other workloads retain it as baselines. The benchmark
reports median wall time, CPU time, allocated bytes, and live bytes.

Build the optimized benchmark and run equivalent workloads with:

```sh
nix develop
cabal build --offline --enable-optimization=2 \
  agent-syntax:bench:syntax-loading-bench
bin=$(cabal list-bin agent-syntax:bench:syntax-loading-bench)

"$bin" eager none 50 7
"$bin" on-demand none 50 7
"$bin" eager haskell 50 7
"$bin" on-demand haskell 50 7
"$bin" on-demand-released haskell 50 7
"$bin" eager haskell,javascript,python,typescript,json,xml,bash 500 7
"$bin" on-demand haskell,javascript,python,typescript,json,xml,bash 500 7
"$bin" on-demand-released haskell,javascript,python,typescript,json,xml,bash 500 7
"$bin" eager haskell 5000 7
"$bin" on-demand haskell 5000 7
```

Output columns are workload, requested language count, source line count,
sample count, elapsed milliseconds, CPU milliseconds, allocated bytes, and
live bytes. RTS statistics are enabled by the benchmark executable.

The package owns:

- loading KDE XML syntax definitions through Skylighting
- Markdown fence language and file-extension resolution
- bounded tokenization with exact source preservation
- a stable semantic token-class model for renderers

Syntax definitions are supplied at runtime through `AGENT_SYNTAX_DIR`. The
interactive path uses `newSyntaxHighlighter` to index filenames without parsing
their XML, then `loadSyntaxLanguage` parses only a requested definition and its
transitive grammar dependencies. The eager `loadSyntaxHighlighter` API remains
available for batch callers.

The repository's Nix flake fetches the pinned upstream definitions and
configures the variable for tests, development shells, and packaged
executables.

Terminal colors, Brick attributes, Markdown layout, and widget caching remain
in `agent-tui`.
