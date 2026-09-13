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
