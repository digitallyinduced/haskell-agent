# agent-syntax

Renderer-independent syntax highlighting for the universal agent harness.

The package owns:

- loading KDE XML syntax definitions through Skylighting
- Markdown fence language and file-extension resolution
- bounded tokenization with exact source preservation
- a stable semantic token-class model for renderers

Syntax definitions are supplied at runtime through `AGENT_SYNTAX_DIR`. The
repository's Nix flake fetches the pinned upstream definitions and configures
the variable for tests, development shells, and packaged executables.

Terminal colors, Brick attributes, Markdown layout, and widget caching remain
in `agent-tui`.
