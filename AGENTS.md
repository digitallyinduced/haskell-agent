# about

coding harnesses are going to be the primary interface for humans to work with the computer.
we are building the independent agent harness that works with any llm model.

the agent harness will provide acess to latest frontier models and open source models

we will support cli, native macos desktop, windows, ios, android and web.

while we are starting out as a coding harness, we plan to expand the harness to deal with all kinds of digital work.

# architecture

we are using haskell and ghc as the primary runtime system for the agent.
type safety and the approach of functional program maps well to the problem space. monads and haskels concurrency system seem well suited for agent harnesses that need to deal with many concurrent agents.

we follow the tool defintions that are used by the first party lab harnesses. e.g. for oai we use the tool defintions that codex provides out of the box, for grok we use the tool definitions that grok build provides out of the box. This way


# ghci

use ghci instead of compiling the code. E.g. instead of nix flake check start a ghci and load in the necessary modules. This is way faster than doing a full compile.

From `nix develop`, run `repl` to open the agent under GHCi. It defaults to
OpenAI `gpt-5.6-sol` with `--yolo`. On first open it passes `--worktree` when
the cwd is not already under `~/.haskell-agent/worktrees`. Agent `:reload`
returns to GHCi, reloads modules, and resumes the previous session.

## development feedback loop

Prefer `cabal repl` over `cabal run` when iterating on the agent itself. Do not rebuild the binary between UI/logic tweaks; reload in GHCi instead.

### recommended: multi-package repl

Load every library you may edit so `:r` recompiles across package boundaries (`agent-cli`, `agent-core`, providers, …):

`cabal.project` sets `multi-repl: True`, so multiple library targets share one GHCi session by default:

```
nix develop
cabal repl \
  agent-cli:lib:agent-cli \
  agent-cli-runtime:lib:agent-cli-runtime \
  agent-telegram:lib:agent-telegram \
  agent-core:lib:agent-core \
  agent-connectivity:lib:agent-connectivity \
  agent-process:lib:agent-process \
  agent-codex-dialect:lib:agent-codex-dialect \
  agent-grok-build-dialect:lib:agent-grok-build-dialect \
  agent-tui:lib:agent-tui \
  agent-responses:lib:agent-responses \
  agent-openai:lib:agent-openai \
  agent-xai:lib:agent-xai \
  agent-openrouter:lib:agent-openrouter \
  agent-gemini:lib:agent-gemini \
  claude-agent-sdk-haskell:lib:claude-agent-sdk-haskell \
  agent-claude:lib:agent-claude
```

In GHCi:

```
import System.Environment (withArgs)
withArgs ["--worktree"] run
```

After code changes (any of those packages), stop the running agent, reload, and start again:

```
:q
:r
withArgs ["--worktree"] run
```

Name **library** components explicitly (`pkg:lib:pkg`). Bare package names also pull in tests and executables and load far more modules than you need.

### lighter: `agent-cli` only

If you are only editing `packages/agent-cli/src`:

```
cabal repl agent-cli
```

Same `withArgs ... run` / `:q` / `:r` loop. Dependency packages are
linked as built libraries here, so changes in `agent-core` / `agent-tui` /
providers need a repl restart or the multi-package command above.

### CLI UI changes

When changing the CLI UI (prompt, colors, chrome, keybindings, paste, approval prompts, markdown rendering, status lines, etc.):

- Always exercise the live agent in **tmux** before opening a PR.
- Prefer `tmux new-session` / `tmux send-keys` / `tmux capture-pane` so the TTY path is real (raw mode, Esc cancel, haskeline, washes).
- Do not rely only on unit tests for visual/TTY behavior; confirm the prompt and interactions look right in the captured pane.
- Only open the PR after that tmux smoke check passes.

### pitfalls

- Exit the agent (`:q` or Ctrl-D) before `:r`; a live stdin/WebSocket session blocks GHCi.
- `repl` / `devMain` creates a new worktree on first open when not already under `~/.haskell-agent/worktrees`. `:reload` resumes the same session (and cwd). Manual `run` still needs `withArgs ["--worktree"]` (or `--resume` / `--cwd`) if you want a worktree.
- The development entry point restores GHCi's original cwd when the agent exits, so a later fresh run does not silently reuse the previous worktree.
- `cabal repl agent-cli:exe:agent-cli` + `:main` looks convenient but only interprets `Main.hs` and does **not** reload library source changes.
- Keep the automatic `repl` launcher single-component. With GHC 9.10,
  Cabal's multi-home-unit mode does not support the GHCi `:module` and `:cmd`
  commands that its automatic reload/resume loop uses. Use the manual
  multi-package workflow above when editing dependency packages.
- Use `ghcid` for typecheck-on-save; keep `cabal repl` + `withArgs ... run` for running the live agent.
- Prefer `repl` when you want automatic `:reload` + session resume instead of the manual `:q` / `:r` / `run` loop.

### memory / RTS heap cap

`nix develop` does not impose a heap ceiling on every development command. The
`repl` wrapper defaults `GHCRTS` to `-M8G`, which protects the machine from an
unbounded long-running GHCi/agent process while preserving the RTS allocation
area default. The compiled `agent-cli` executable defaults to `-N4 -M8G`; four
capabilities cover concurrent agent work without multiplying the RTS
allocation area by every host core. Both defaults are overridable because
`-rtsopts` is enabled. Set `GHCRTS` explicitly to override the wrapper:

```
GHCRTS='-M16G' repl
GHCRTS='-M4G' cabal repl agent-cli
cabal run agent-cli -- +RTS -N8 -M16G -RTS
```

Avoid large `-A` defaults: the allocation area is per capability, so
`-N14 -A64m` consumes roughly 896 MiB before meaningful application data.

## Nix package maintenance

Each package has a checked-in `package.nix` generated with `cabal2nix`; the
flake does not use import-from-derivation. After changing a package's Cabal
file, regenerate its expression from the repository root, for example:

```
(cd packages/claude-agent-sdk-haskell && cabal2nix . > package.nix)
(cd packages/agent-claude && cabal2nix . > package.nix)
```

# performance

Performance improvements must be supported by a benchmark that proves the
claimed improvement before opening a PR.

- Benchmark the old and new implementations with equivalent, representative
  workloads. Keep a baseline workload in the benchmark so future changes can
  be compared against the behavior being replaced.
- Use an optimized build rather than interpreted GHCi timings. Run benchmarks
  inside `nix develop`, force the result so GHC cannot optimize the work away,
  and enable RTS statistics when measuring allocation.
- Measure both elapsed/CPU time and allocated bytes when the change is intended
  to reduce allocation or GC pressure. A speedup that shifts work or allocation
  to another stage is not sufficient.
- Run multiple samples and report a robust aggregate such as the median. Test
  multiple input sizes to distinguish constant/linear behavior from quadratic
  behavior, and repeat at least one representative case to check measurement
  stability.
- Record the benchmark command, GHC/build settings, workload dimensions, and
  before/after results in the PR. Keep the benchmark in the repository when it
  is useful for detecting regressions.
- If benchmarking shows that part of the proposed optimization regresses, do
  not ship that part. Revert it or redesign it, and document the remaining
  performance boundary.

## streaming text benchmark

The streaming allocation benchmark is
`packages/agent-core/benchmark/StreamingText.hs`. It is built with `-O2` and
uses `GHC.Stats` with `+RTS -T` to measure allocated bytes. It constructs input
outside the measured interval, forces a checksum of the output to prevent
dead-code elimination, performs a GC before each sample, and reports the median
CPU time and allocation across the requested sample count.

Build and locate it with:

```
nix develop -c cabal build --offline agent-core:bench:streaming-text-bench
bin=$(nix develop -c cabal list-bin agent-core:bench:streaming-text-bench)
```

Compare strict repeated append, chunked accumulation, removed-buffer overhead,
the `text-builder` alternative, and the fullscreen baseline with:

```
"$bin" old-accumulate       10000 16 7 +RTS -T
"$bin" new-accumulate       10000 16 7 +RTS -T
"$bin" old-io-ref           10000 16 7 +RTS -T
"$bin" new-io-ref           10000 16 7 +RTS -T
"$bin" text-builder-io-ref   10000 16 7 +RTS -T
"$bin" no-duplicate-buffer  10000 16 7 +RTS -T
"$bin" fullscreen-baseline  10000 16 7 +RTS -T
```

The positional arguments are workload, chunk count, ASCII bytes per chunk, and
sample count. Run several chunk counts and sizes; the benchmark used for the
streaming-buffer change covered 1,000, 2,000, 5,000, and 10,000 16-byte chunks,
plus fixed-total-size cases with larger chunks. Alternative-buffer comparisons
also covered 20,000, 50,000, and 100,000 chunks. The `*-io-ref` workloads model
the actual renderer update path; keep those when evaluating alternative buffer
implementations. An attempted fullscreen chunk-buffer change was benchmarked
separately and reverted because flattening the complete body for every Markdown
render made it slower and allocated more than the strict-`Text` baseline.

# haskell
- Prefer Control.Exception.Safe over Control.Exception
- Never use bare `Control.Concurrent.Async.async`. Prefer structured
  concurrency (`withAsync`, `race`, `concurrently`, etc.) so child lifetimes
  are scoped. If work must outlive the current call, track its lifecycle and
  completion explicitly, and cancel and join it when its owner closes.
