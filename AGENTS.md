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
  agent-core:lib:agent-core \
  agent-responses:lib:agent-responses \
  agent-openai:lib:agent-openai \
  agent-xai:lib:agent-xai \
  agent-openrouter:lib:agent-openrouter
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

Same `withArgs ... run` / `:q` / `:r` loop. Dependency packages are linked as built libraries here, so changes in `agent-core` / providers need a repl restart or the multi-package command above.

### CLI UI changes

When changing the CLI UI (prompt, colors, chrome, keybindings, paste, approval prompts, markdown rendering, status lines, etc.):

- Always exercise the live agent in **tmux** before opening a PR.
- Prefer `tmux new-session` / `tmux send-keys` / `tmux capture-pane` so the TTY path is real (raw mode, Esc cancel, haskeline, washes).
- Do not rely only on unit tests for visual/TTY behavior; confirm the prompt and interactions look right in the captured pane.
- Only open the PR after that tmux smoke check passes.

### pitfalls

- Exit the agent (`:q` or Ctrl-D) before `:r`; a live stdin/WebSocket session blocks GHCi.
- `repl` / `devMain` creates a new worktree on first open when not already under `~/.haskell-agent/worktrees`. `:reload` resumes the same session (and cwd). Manual `run` still needs `withArgs ["--worktree"]` (or `--resume` / `--cwd`) if you want a worktree.
- `run` calls `setCurrentDirectory`, so later runs in the same GHCi process inherit that cwd.
- `cabal repl agent-cli:exe:agent-cli` + `:main` looks convenient but only interprets `Main.hs` and does **not** reload library source changes.
- Use `ghcid` for typecheck-on-save; keep `cabal repl` + `withArgs ... run` for running the live agent.
- Prefer `repl` when you want automatic `:reload` + session resume instead of the manual `:q` / `:r` / `run` loop.

### memory / RTS heap cap

`nix develop` and the `repl` wrapper default `GHCRTS` to `-M8G -A64m` so the agent/GHCi process dies at an 8 GiB heap instead of OOMing the whole machine. The `agent-cli` executable is built with the same `-M8G -A64m` RTS defaults (overridable via `+RTS` because `-rtsopts` is enabled). Override when needed:

```
GHCRTS='-M16G -A64m' repl
GHCRTS='-M4G -A32m' cabal repl agent-cli
cabal run agent-cli -- +RTS -M16G -RTS
```


# haskell
- Prefer Control.Exception.Safe over Control.Exception
- Never use bare `Control.Concurrent.Async.async`. Prefer structured
  concurrency (`withAsync`, `race`, `concurrently`, etc.) so child lifetimes
  are scoped. If work must outlive the current call, track its lifecycle and
  completion explicitly, and cancel and join it when its owner closes.
