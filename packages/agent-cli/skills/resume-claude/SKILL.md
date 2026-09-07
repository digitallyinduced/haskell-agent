---
name: resume-claude
description: Continue work from a recent Claude Code session.
when-to-use: Use when the user asks to continue or resume work previously done in Claude Code.
argument-hint: "[latest | words describing the session | session id | transcript path]"
compatibility: Requires read access to the user's Claude session store.
---
# Resume a Claude Code session

Use provider `claude`. Read and follow `../shared/resume-session/CORE.md`,
resolved relative to this skill directory. Treat the invocation arguments as
the optional session reference described there.
