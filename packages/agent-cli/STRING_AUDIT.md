# Application text representation audit

2026-09-12. Static review of production Haskell under `packages`, including
`String`, `[Char]`, and `Text.unpack`. Remaining candidates are unmeasured;
implemented changes link to their own benchmarks below.

## Implemented follow-up

GitDiff now retains stdout/stderr as strict `Text`, including `/review` error
handling. NUL splitting operates on Text and converts individual paths to
`FilePath` at the process boundary. UTF-8 decoding remains lenient. See
`benchmark/GitOutput.md` for the isolated representation benchmark; this is
not a measurement of whole-process memory or Git execution time.

Markdown table rows now scan strict Text chunks rather than unpacking rows and
packing each backtick-lookahead suffix. Completed cells are copied to avoid
retaining a large source row through a small cell. Escape parity, Unicode
whitespace, and the existing closing-backtick run rule are unchanged. See
`../agent-tui/benchmark/MarkdownTable.md` for measurements and validation.
This targets temporary parsing allocation, not retained conversation size.

Partial JSON string previews now scan strict Text for quotes and backslash
parity rather than constructing character lists. JSON decoding and incomplete
escape handling are unchanged. At 1,024 decoded characters, complete streaming
preview benchmarks allocate 57–64% less and take 24–43% less elapsed time across
ASCII, Unicode and escape-heavy inputs. Tiny-input timing is inconclusive, with
no consistent regression in repeated runs. See
`../agent-responses/benchmark/JsonPreview.md`. These are temporary allocation
savings, not measurements of idle or retained conversation memory.

## Prioritized follow-up

| Priority | Module | Finding and acceptance criteria |
| --- | --- | --- |
| High | `agent-tui/src/Agent/TUI/TextWidth.hs` | `graphemeClusters` unpacks entire input, constructs character lists, then packs clusters; width checks also unpack. Benchmark Text traversal/slices in actual rendering and cursor workloads; preserve combining marks, flags, ZWJ, modifiers and terminal-width compatibility. |
| Medium | `agent-cli/src/Agent/CLI/Clipboard/{MacOS,Linux}.hs` | Process capture passes potentially large clipboard content through String. Prefer byte capture with explicit decoding and Text results. Preserve subprocess cleanup, errors, and paste behavior. |
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

Further review identified these compatibility boundaries:

- TextWidth predicates can migrate before segmentation. Preserve exactly two
  regional indicators for flags and character-count cursor offsets, not UTF-8
  byte offsets. Benchmark complete rendering/cursor consumers, not only helpers.
- Markdown table scanning preserves escape parity and accepts closing backtick
  runs at least as long as the opening run. Repeated suffix searches remain for
  unmatched runs; the Text replacement is not a worst-case linear-time parser.
- Clipboard byte capture must not silently change locale-based strict decoding
  into lenient UTF-8. Preserve backend fallback order and first-error selection.

The history replacement is implemented separately. The remaining candidates above are
not changed in this audit; each needs focused compatibility tests and an
optimized benchmark before making a performance claim.
