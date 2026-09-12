# Application text representation audit

2026-09-12. Static review of production Haskell under `packages`, including
`String`, `[Char]`, and `Text.unpack`. This identifies candidates, not measured
performance improvements outside the command-history benchmark.

## Prioritized follow-up

| Priority | Module | Finding and acceptance criteria |
| --- | --- | --- |
| High | `agent-cli/src/Agent/CLI/GitDiff.hs` | `GitCommandOutput` stores complete stdout/stderr as `String`. `readHandleStrict` decodes bytes to Text then unpacks; consumers repack. Retain Text directly, converting individual paths only at process boundaries. Benchmark large diffs and preserve NUL-separated paths and invalid-UTF-8 policy. |
| High | `agent-tui/src/Agent/TUI/TextWidth.hs` | `graphemeClusters` unpacks entire input, constructs character lists, then packs clusters; width checks also unpack. Benchmark Text traversal/slices in actual rendering and cursor workloads; preserve combining marks, flags, ZWJ, modifiers and terminal-width compatibility. |
| Medium | `agent-cli/src/Agent/CLI/Clipboard/{MacOS,Linux}.hs` | Process capture passes potentially large clipboard content through String. Prefer byte capture with explicit decoding and Text results. Preserve subprocess cleanup, errors, and paste behavior. |
| Medium | `agent-tui/src/Agent/TUI/Markdown/Block.hs` | `splitTableRow` unpacks each row and constructs reversed character lists. Benchmark a Text scanner/builder; preserve escaped pipes and code spans. |
| Medium | `agent-store/src/Agent/Store/Postgres/Custom/Sql.hs` | `splitTopLevelStatements` unpacks complete SQL batches and constructs character-list statements. Separate parser change with equivalence tests for dollar quotes, escaping and nested comments. |
| Lower | `agent-grok-build-dialect/src/Agent/GrokBuild/Dialect/Shell.hs` | Quote and ampersand scanning unpacks command text. Security-sensitive: preserve parser decisions before considering performance. |
| Lower | `agent-mcp/src/Agent/MCP/Types.hs` | Arguments/environment retained as `[String]` and `[(String,String)]`. Text storage could defer conversion until process creation, but no significant memory contribution has been established. |

## Boundaries that should remain explicit

- `Aeson.String` already contains Text; it is not a linked-list String.
- File paths need `FilePath` or `OsPath` semantics, not a mechanical Text substitution.
- Process arguments/environment, `Show`/`Read`, `fail`, command-line parsers,
  URI/WebSocket and FFI APIs may require String. Convert at those boundaries.
- Small error messages, short terminal escape sequences, tiny character sets,
  and base URLs are lower priority than large retained content.
- Strict Text is the application-text default; ByteString is appropriate for
  encoded/protocol or binary data. This convention is recorded in `AGENTS.md`.

The history replacement is implemented separately. The candidates above are
not changed in this audit; each needs focused compatibility tests and an
optimized benchmark before making a performance claim.
