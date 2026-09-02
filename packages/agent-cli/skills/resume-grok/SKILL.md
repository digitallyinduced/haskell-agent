---
name: resume-grok
description: Continue work from a recent Grok Build session.
when-to-use: Use when the user asks to continue or resume work previously done in Grok Build.
argument-hint: "[latest | words describing the session | session id | session path]"
compatibility: Requires Python 3 and read access to the user's Grok Build session store.
---
# Resume a Grok Build session

Use provider `grok`. Read and follow `../shared/resume-session/CORE.md`,
resolved relative to this skill directory. Treat the invocation arguments as
the optional session reference described there.
