# agent-tools

Concrete local tools for the agent harness: filesystem and shell operations,
GHCi, code-mode workers, planning, artifacts, images, charts, and subagent tools.
Provider dialects compose these implementations into their model-facing tools.

This package depends on `agent-core`, never the reverse. Generic tool contracts
(`Agent.Tools.Types`), scheduling, resource arbitration, and output-memory
accounting remain in core because the loop needs them without concrete tools.

The code-mode JavaScript worker and GHCi support module are Cabal data files owned by this package.
Tool-specific tests and allocation benchmarks live here alongside their
implementations.

## Experimental Haskell code mode

JavaScript remains the default. Select GHCi with
`agent-cli --code-mode --code-mode-backend haskell`.
The public tools remain `exec` and `wait`; `exec` takes a complete `IO ()`
expression, not an interactive GHCi command. Generated `Tools` bindings describe
common input schemas using records; unsupported shapes use Aeson's `Value`.
Declared output schemas generate result records and decoders. These bindings
return `ToolResult a`: `.decodedResult` is `Either Text a`, and `.rawResult`
retains the original JSON payload even if decoding fails. Decoding never repeats
the tool call. Unsupported schema shapes remain `Value`; this is structural
decoding, not a complete JSON Schema validator. Follow the current `exec`
declaration for generated types and helper signatures.

Tools without declared output schemas keep returning `Value`. Bounded observed
return-shape hints describe successful structured responses without turning a
sample into a type contract. These hints are session-local, not persisted across
restarts; fields may disappear or change type. They retain property names and
types, not scalar response values. Property names themselves can contain data,
so these hints are not an anonymization mechanism.

Build the optional, flake-pinned compiler environment without adding GHC to the
default CLI closure:

```sh
nix build .#code-mode-ghci
export HASKELL_AGENT_GHCI="$PWD/result/bin/ghci"
```

This backend is **not sandboxed**: ordinary IO has ambient process permissions.
Nested calls still use the normal tool dispatcher and approval policy. Only one
cell may be active; observe or terminate it with `wait` before another `exec`.
Local bindings do not persist between cells. Termination or worker failure does
not roll back IO, and the runtime never replays a failed cell automatically.
Repeat the backend selection when resuming in a new CLI process.

## Reading local documents

`read_file` retains numbered, bounded line windows for text. PDF documents and
supported images are recognized by their content, including files stored without
an extension. They are returned as native model content rather than binary text
or instructions to run a converter. The same allowed-root policy applies to all
formats; attachments are limited to 20 MiB and loaded whole, so line-range and
page-selection arguments are rejected for them.

PDF processing requires a provider/model accepting native file inputs. The tool
does not claim to have extracted text or verified facts: it supplies the original
bytes for model inspection. Local PDF rendering and OCR are not prerequisites.
