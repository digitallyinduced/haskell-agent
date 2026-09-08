# agent-codex-dialect

The Codex model-facing contract for the universal agent harness: prompts,
tool schemas and handlers, child-agent conventions, and project-instruction
formatting. Provider transport and authentication remain in `agent-openai`.

`shell_command` defaults to the session sandbox. A command blocked by isolation
may request `"sandbox_permissions":"require_escalated"` with a nonblank
`justification`. The host must freshly approve that exact invocation; a model
request alone never authorizes execution. On macOS this omits the outer
Seatbelt wrapper while preserving the session `TMPDIR` and process lifecycle,
allowing tools such as Swift Package Manager to apply their own sandbox.

Escalation is not remembered. Nonempty `write_stdin` input to an escalated
process also requires fresh approval, except an exact Ctrl-C cancellation.
Reading output does not grant permission to send later input. Ordinary
library dispatch fails closed when escalation has not been authorized.
