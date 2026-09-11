# Per-command shell sandbox approval

Shell commands remain sandboxed by default. On macOS the session's Seatbelt
wrapper can conflict with Swift Package Manager's own manifest sandbox:

```
sandbox-exec: sandbox_apply: Operation not permitted
```

This occurs before compilation and is not an Xcode signing error. Preserve the
failed command's output, then request a separately approved invocation:

```json
{
  "command": "xcodebuild -resolvePackageDependencies -project MyApp.xcodeproj",
  "sandbox_permissions": "require_escalated",
  "justification": "SwiftPM cannot create its manifest sandbox inside the session sandbox."
}
```

`shell_command` and Grok's `run_terminal_cmd` accept this harness extension.
Grok still requires its normal `description` argument. Omitted permission or
`use_default` keeps existing behavior; invalid values and missing or blank
escalation justifications are rejected.

## Approval and scope

- Every escalated invocation requires fresh confirmation, including with
  `--yolo` or remembered tool approval. There is no permanent escalation choice.
- Plan/read-only restrictions still apply. Child agents and hosts without a
  suitable fresh-confirmation channel must deny the request.
  Interactive line and fullscreen CLIs and native hosts support this
  confirmation. The fullscreen dialog defaults to Deny and offers only
  Allow once or Deny. Managed-turn routes still fail closed until their
  approval protocol supports the same warning and once-only semantics.
- The trusted host dispatch supplies an opaque one-use authorization, separate
  from model arguments. It expires when dispatch finishes. Ordinary direct
  dispatch and all existing process APIs remain sandboxed.
- Only the approved process launch omits the harness's macOS wrapper. Session
  `TMPDIR`, output capture, timeout and cancellation remain in effect. This is
  not root access and does not bypass macOS TCC or signing requirements.
- Nonempty input to an escalated Codex process needs another fresh approval.
  Empty output inspection and an exact Ctrl-C cancellation do not grant further
  execution authority.
- Escalated Grok commands use the turn cwd and do not source or update the
  persistent terminal environment/cwd. Include any necessary `cd` in the
  approved command itself.

The host approval callback is a trusted boundary: accepting a fresh-required
call asserts actual fresh user confirmation, not an automatic policy decision.
An approved command can execute project code and spawn descendants; approval
is not a guarantee that a build script is harmless.

## macOS acceptance check

Run this check from a newly started, updated harness. A GHCi harness launched
inside an older sandbox inherits that sandbox and cannot remove it.

1. Create a dependency-free Swift package under the session's `$TMPDIR`.
2. Run `swift package --package-path "$TMPDIR/<package>" dump-package` with
   default permissions and retain the result.
3. If it fails with the nested-sandbox error, repeat the exact command with
   `require_escalated` and a justification. Verify the approval shows the
   command, working directory, reason and loss-of-isolation warning.
4. Deny once and verify no command starts. Request again and approve once;
   manifest evaluation should succeed.
5. Run a new default command and verify it remains sandboxed.
6. Retry the affected app's dependency resolution, then an unsigned build.
   Treat unrelated dependency/compiler failures separately. Do not install,
   launch, publish or notarize the app as part of this check.

When running from a Nix development environment, check both `DEVELOPER_DIR`
and `SDKROOT`: they may select a Nix SDK incompatible with the installed Swift
compiler. For an Xcode installation at the standard path, use these same
explicit selections in both the default and approved command:

```sh
DEVELOPER_DIR=/Applications/Xcode.app/Contents/Developer \
SDKROOT="$(env -u SDKROOT DEVELOPER_DIR=/Applications/Xcode.app/Contents/Developer /usr/bin/xcrun --sdk macosx --show-sdk-path)" \
/usr/bin/xcrun swift package --package-path "$TMPDIR/<package>" dump-package
```
