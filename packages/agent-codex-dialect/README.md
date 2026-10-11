# agent-codex-dialect

The Codex model-facing contract for the universal agent harness: prompts,
tool schemas and handlers, child-agent conventions, and project-instruction
formatting. Provider transport and authentication remain in `agent-openai`.

`shell_command` defaults to the session sandbox. A command blocked by isolation
may request `"sandbox_permissions":"require_escalated"` with a nonblank
`justification`. The host must authorize that exact invocation: full access
(`--yolo`) auto-approves it; otherwise fresh user approval is required. A model
request alone never authorizes execution. On macOS this omits the outer
Seatbelt wrapper while preserving the session `TMPDIR` and process lifecycle,
allowing tools such as Swift Package Manager to apply their own sandbox.

Escalation is not remembered. Nonempty `write_stdin` input to an escalated
process follows the same approval policy, except an exact Ctrl-C cancellation.
Reading output does not grant permission to send later input. Ordinary
library dispatch fails closed when escalation has not been authorized.

`apply_patch` preserves existing LF, CRLF, and CR line endings, including
mixed endings on unchanged and context lines. Inserted and replaced lines use
the first line-ending style in the original file (LF when none exists).
Updates retain the original presence or absence of a final newline; new files
use LF with a final newline. The same rules apply when moving a file.
This follows Codex's line-ending-preserving source representation
([upstream change](https://github.com/openai/codex/pull/51203)), while retaining
this harness's existing behavior for unterminated final lines.
