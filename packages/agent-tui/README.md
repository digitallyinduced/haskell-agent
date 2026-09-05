# agent-tui

Retained terminal UI building blocks for the universal agent harness.

This package owns presentation state and reusable Brick/Vty rendering:

- `Agent.TUI.Model` — renderer-independent UI state and reducer
- `Agent.TUI.Markdown` — fullscreen Markdown widgets
- `Agent.TUI.Theme` — semantic Brick attributes and themes
- `Agent.TUI.Presentation` — plain tool-call presentation helpers

Renderer-independent syntax loading and tokenization live in `agent-syntax`.

`Agent.TUI.Model` keeps the public API and turn-level reducer. Its private
modules separate block creation (`Block`), inspection bursts (`Inspection`),
block navigation (`Selection`), background terminal ownership (`Shell`), and
tool-result presentation/legacy transcript adaptation (`ToolResult`), alongside
the existing `State`, `Edit`, `Timing`, and `Types` modules. Turn finalization
and response retraction stay together in the reducer because they update
several of these lifecycles at once. Private modules do not import the public
facade.

The CLI-specific runtime adapter remains in `agent-cli` as
`Agent.CLI.TUI.App`. It coordinates clipboard access, command completion,
permissions, interrupts, history, and agent selection while depending on this
package for retained state and rendering.
