# Terminal lifecycle status

Interactive CLI sessions with terminal stdout report OSC 7501 status:

```text
ESC ] 7501 ; state=idle:app=haskell-agent ESC \
ESC ] 7501 ; state=working:app=haskell-agent ESC \
ESC ] 7501 ; state=blocked:app=haskell-agent ESC \
ESC ] 7501 ; state=clear ESC \
```

`idle` means the foreground session is ready for another request. `working`
covers foreground model turns and their tools. `blocked` means a human-input
wait is outstanding (including permission requests and questions). Overlapping
waits are counted, so resolving one request does not clear another.

Status is cleared on ordinary or exceptional exit and during external-program,
fullscreen suspension and update-exec handoffs. Returning from a handoff
restores the current status. Forced process termination cannot run cleanup.
One-shot and redirected-output invocations emit no status sequences.

The protocol contains only fixed application and lifecycle tokens: no prompts,
tool arguments, filenames or generated content. Unsupported terminals ignore
the sequence; display behavior is determined by the terminal or multiplexer.
This does not change the existing window-title or notification behavior.

The wire format follows [Codex's OSC 7501 implementation](https://github.com/openai/codex/blob/4bad6d78e9b50f9fa8bd941f1db012ed491ad2da/codex-rs/tui/src/terminal_program_status.rs).
