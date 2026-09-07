---
name: wait-for-ci
description: Wait for continuous-integration checks to finish and respond to their final result instead of ending while checks are pending.
when-to-use: Apply after a push or pull-request update when CI checks are running, queued, or expected and the task depends on their result.
argument-hint: "[pull request, branch, commit, or run]"
user-invocable: true
---

# Wait for CI

Keep the current turn active while relevant CI is pending. A status such as
"waiting for CI" is progress, not a final result.

## Watch the right revision

1. Identify the pull request, branch, commit, or workflow run relevant to the
   task.
2. Record the exact head commit being tested. After any new push, discard
   results for the previous head and follow the replacement run.
3. Include every required check. Do not treat one successful job as overall
   success while other required checks are queued or running.

## Wait

Prefer the CI provider's native watch operation when available. For GitHub,
`gh pr checks <pr> --watch --interval 30` is usually appropriate. Otherwise
poll status at a moderate interval, normally 30 to 60 seconds. Use long,
bounded waits rather than rapid repeated requests.

Immediately after a push, a provider may temporarily report no checks. When
checks are expected, treat that as pending registration and keep polling; it is
not success.

Do not end the turn merely because checks are still pending. Continue watching
until one of these conditions occurs:

- all relevant checks reach a successful terminal state;
- a check fails, is cancelled, or requires action;
- CI cannot start or cannot be observed because of a concrete external block;
- the user asks to stop; or
- a deadline specified by the user is reached.

If checks remain queued unusually long, inspect the run once for an actionable
cause, then keep waiting when the queue is merely slow. Do not restart, rerun,
cancel, merge, or otherwise mutate remote state unless the original task
authorizes it.

## Act on the result

- **Success:** continue with any remaining requested work. If CI was the final
  gate, report the successful checks and exact tested commit.
- **Failure:** inspect the failing job and logs. If a fix is within the original
  task, implement and validate it, push the fix when authorized, and wait for
  the new head's CI. Otherwise report the failure and the concrete next action.
- **Cancelled, blocked, or inaccessible:** report the terminal state or
  external block with the run or check details. Do not describe pending CI as
  completed.

Before finishing, refresh status once and verify that the reported result still
belongs to the current head commit.
