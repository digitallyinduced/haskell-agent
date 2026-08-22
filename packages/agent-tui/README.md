# agent-tui

Retained terminal UI building blocks for the universal agent harness.

This package owns presentation state and reusable Brick/Vty rendering:

- `Agent.TUI.Model` — renderer-independent UI state and reducer
- `Agent.TUI.Markdown` — fullscreen Markdown widgets
- `Agent.TUI.Theme` — semantic Brick attributes and themes
- `Agent.TUI.Presentation` — plain tool-call presentation helpers

The CLI-specific runtime adapter remains in `agent-cli` as
`Agent.CLI.TUI.App`. It coordinates clipboard access, command completion,
permissions, interrupts, history, and agent selection while depending on this
package for retained state and rendering.
