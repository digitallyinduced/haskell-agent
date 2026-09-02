---
name: resume-cursor
description: Continue work from a recent Cursor CLI or Cursor editor session.
when-to-use: Use when the user asks to continue or resume work previously done in Cursor.
argument-hint: "[latest | words describing the session | session id | transcript or store path]"
compatibility: Requires Python 3 and read access to the user's Cursor session store.
---
# Resume a Cursor session

Use provider `cursor`. Read and follow `../shared/resume-session/CORE.md`,
resolved relative to this skill directory. Treat the invocation arguments as
the optional session reference described there.
