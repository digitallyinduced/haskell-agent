# agent-tools

Concrete local tools for the agent harness: filesystem and shell operations,
GHCi, code-mode workers, planning, artifacts, images, charts, and subagent tools.
Provider dialects compose these implementations into their model-facing tools.

This package depends on `agent-core`, never the reverse. Generic tool contracts
(`Agent.Tools.Types`), scheduling, resource arbitration, and output-memory
accounting remain in core because the loop needs them without concrete tools.

The code-mode JavaScript worker is a Cabal data file owned by this package.
Tool-specific tests and allocation benchmarks live here alongside their
implementations.

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
