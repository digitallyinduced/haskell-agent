# Ghostty integration

The CLI detects Ghostty through `TERM_PROGRAM=ghostty` and enables terminal
features only when the relevant output handle is a TTY. Escape sequences are
wrapped for tmux passthrough when `TMUX` is set.

## Integrated features

- Kitty graphics previews for pasted images.
- OSC 7 working-directory reporting, including agent worktrees.
- OSC 8 links for Markdown URLs and absolute tool file paths.
- OSC 9 desktop notifications for long turns, failures, approvals, and
  completed subagents.
- OSC 9;4 native progress while the model is working.
- OSC 52 clipboard commands:
  - `/copy` or `/copy-last`
  - `/copy-code [N]`
  - `/copy-diff`
  - `/copy-path`
  - `/copy-session`
- OSC 133 semantic prompt and command markers.
- DEC synchronized output for interactive picker redraws.
- Kitty keyboard protocol inside raw-mode pickers. The mode is pushed on entry
  and popped on exit, so Haskeline and the parent shell retain legacy input.
- `/terminal` (alias `/ghostty`) shows the detected capabilities.

Ghostty may require clipboard-write permission in its configuration for OSC 52.
tmux may likewise require escape-sequence passthrough to be enabled.

## Recommended quick-terminal profile

The harness works well as Ghostty's global quick terminal. Add a binding like
the following to the user's Ghostty configuration, adjusting the key chord to
avoid conflicts:

```ini
keybind = global:cmd+grave=toggle_quick_terminal
quick-terminal-position = top
quick-terminal-autohide = true
```

Start `agent-cli` from the shell initialization used by that surface, or create
a small shell alias dedicated to the desired project.

## Subagent panes

Ghostty's macOS AppleScript API can create windows, tabs, and splits. A future
native dashboard can use that API to attach read-only views to the harness's
subagent event stream. It should not start independent copies of an agent for
every pane: the registry in the main process remains the source of truth.

For the current CLI, `scripts/ghostty-agent-layout [PROJECT_DIR]` creates a
four-pane Ghostty workspace containing the agent, a shell, Git status, and an
agent-snapshot pane. It requires Ghostty 1.3 or newer and macOS Automation
permission for the invoking application.

The portable implementation should expose a structured event stream first.
AppleScript should then be a macOS/Ghostty adapter over that stream rather than
being coupled to core orchestration.

## libghostty

`libghostty` is a candidate terminal engine for future native clients. Keep it
behind a C FFI package and pin the exact Ghostty revision through the Nix flake;
its API should not leak into `agent-core`. The CLI does not link it because a
terminal emulator is unnecessary when the harness already runs inside a PTY.
