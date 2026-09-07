# 0.1.0.0 — 2026-09-01

## Features

- **MCP catalog commands** list, enable, and disable configured servers with `agent-cli mcp list`, `enable`, and `disable`.
- **Release notes** are now available from `/changelog` and the start screen.
- **Secret requests** now notify the terminal when the agent needs sensitive input.
- **Plan questions** now accept custom replies in addition to predefined choices.
- **Cross-tool resume** can continue recent Codex, Claude Code, Cursor, and Grok Build sessions with `/resume-codex`, `/resume-claude`, `/resume-cursor`, and `/resume-grok`.
- **Skill installer** copies Agent Skills into `~/.haskell-agent/skills` or the project's `.haskell-agent/skills` directory. The harness discovers those product-home trees in addition to `.agents/skills`.

## Bug Fixes

- **File edit diffs** remain visible after the edit finishes.
