# Session Nix environments

`set_environment` accepts **plain Nix source**, without JSON or Markdown:

```nix
{ pkgs }:
pkgs.mkShell {
  packages = [ pkgs.python312 pkgs.ffmpeg ];
}
```

The harness supplies `pkgs` from a managed flake pinned to nixpkgs revision
`afe3d8ac4395617bdcdac9f188ac8717a062e014`. Nix with flake support must be
available to the harness. The repository's own flake is not edited.

The experimental tool is available on the Codex and Grok coding tool surfaces,
including code-only mode. It is not exposed for Claude Code's SDK-owned shell.
Disabling shell tools also disables `set_environment`.

Each call writes a new revision under the session temporary directory, then
runs `nix develop` to realize a profile and exercise its shell hook. Build output
is streamed; the build has a ten-minute timeout and supports cancellation.
Only a successful build activates the candidate. A failed or cancelled build
leaves the active environment unchanged.

Subsequent shell commands enter the activated profile automatically. The complete
expression is replaced, rather than merged with the previous expression.
Existing processes keep their original environment, and the harness process's
environment is never changed. Other sessions are unaffected.

The parent and its in-process child agents share the session's active profile.
Environment changes are serialized across their tool instances; a successful
change affects subsequent shell commands from every agent in that session.

Successful activation is recorded atomically in `nix-environment/active.json`.
Resuming the same session restores its profile while the session temporary
directory and Nix profile remain available. Revision directories retain the
expression, flake, lock file, and profile; an earlier expression can be submitted
again to restore its package selection.

This is executable configuration, not a security boundary: derivations and shell
hooks can execute code. Normal mutation approval and sandbox restrictions apply.
The tool does not silently escalate when a build is blocked by the sandbox.
Shell hooks run when entering the environment, including for later commands.
