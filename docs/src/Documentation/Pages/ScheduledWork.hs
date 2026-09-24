{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.ScheduledWork (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/guides/scheduled-work/"
    , pageTitle = "Grok tasks, goals, and scheduled work"
    , pageDescription = "Supervise background tasks, set bounded goals, schedule recurring prompts, and run named research workflows."
    , pageGroup = "Using the agent"
    , pageBody = [hsx|
        <p>This page describes the Grok Build dialect. These tool names are not universal.
        Scheduler, goal and workflow tools are root-session capabilities rather than tools
        offered recursively to every child. Check the active catalog before requesting them.</p>
        <h2 id="background-commands">Background commands</h2>
        <p><code>run_terminal_cmd</code> requires <code>command</code> and <code>description</code>.
        Optional <code>timeout</code> controls the command deadline; <code>background: true</code>
        returns a task identifier. Shell escalation uses <code>sandbox_permissions</code> and
        <code>justification</code>, independently of the background setting.</p>
        <pre><code class="language-json">{"{\"command\":\"git status --short\",\"description\":\"Inspect the checkout without changing it\",\"background\":false}" :: Text}</code></pre>
        <p>A background task is still running after its initial tool call returns. Completion is
        reported automatically; use the tools below for one bounded wait or an inspection, not
        a loop of polling calls. An exit code, not the mere creation of a task, establishes completion.</p>
        <table><thead><tr><th>Tool</th><th>Inputs</th><th>Behavior</th></tr></thead><tbody>
            <tr><td><code>get_task_output</code></td><td><code>task_ids</code> array; optional <code>timeout_ms</code></td><td>Omitted/0 timeout gives a snapshot; positive timeout waits for completion, capped at 600,000 ms</td></tr>
            <tr><td><code>wait_tasks</code></td><td><code>task_ids</code>, <code>mode</code> of <code>wait_any</code> or <code>wait_all</code>; optional <code>timeout_ms</code></td><td>Wait for first/all completion, capped at 600,000 ms</td></tr>
            <tr><td><code>kill_task</code></td><td>Required <code>task_id</code></td><td>Stops the addressed background work; it does not reverse completed file or service changes</td></tr>
        </tbody></table>
        <pre><code class="language-json">{"{\"task_ids\":[\"REPLACE_WITH_RETURNED_TASK_ID\"],\"timeout_ms\":1000}" :: Text}</code></pre>
        <p>Do not substitute a persisted conversation ID for a Grok task ID. If a task is missing,
        inspect the original launch result and current session rather than starting duplicate work.</p>
        <h2 id="typed-subtasks">Typed subtasks</h2>
        <p><code>task</code> requires <code>prompt</code> and <code>description</code>. Optional
        <code>subagent_type</code> selects <code>general-purpose</code>, <code>explore</code> or
        <code>plan</code>; types can restrict the child's tool set. Additional fields are
        <code>run_in_background</code>, <code>resume_from</code>, <code>cwd</code>, <code>model</code>,
        and <code>isolation</code> (<code>none</code> or <code>worktree</code>).</p>
        <pre><code class="language-json">{"{\"prompt\":\"Locate the configuration parser and report its file path; do not edit files.\",\"description\":\"Find the configuration entry point\",\"subagent_type\":\"explore\",\"run_in_background\":true,\"isolation\":\"none\"}" :: Text}</code></pre>
        <p>The task description should name an independently verifiable result. A worktree isolates
        files, not external credentials or remote effects. Model selection must satisfy the host
        policy. Inspect output, review changes and run checks before integrating a child's result.</p>
        <h2 id="monitors">Event monitors</h2>
        <p><code>monitor</code> runs a <code>command</code> with a required <code>description</code>.
        Each stdout line becomes an event; process exit ends the watch. Print meaningful state
        changes and use line-buffered pipelines to avoid delayed events. The optional
        <code>timeout_ms</code> defaults to and is capped at 36,000,000 (10 hours).
        <code>persistent: true</code> instead watches for the session lifetime.</p>
        <p>Record the returned task identifier and stop it with the available task-control tool when
        finished. A monitor is an executing program: it needs the same command review as an ordinary
        shell invocation. It is not a durable service that survives the session.</p>
        <h2 id="goals">Bounded autonomous goals</h2>
        <pre><code>/goal Investigate the failing test and propose a minimal correction --budget 10</code></pre>
        <p>Use <code>/goal status</code> to inspect progress, <code>/goal pause</code> to pause,
        <code>/goal resume</code> to continue and <code>/goal clear</code> to remove the objective.
        Choose a bounded objective; do not interpret a budget as permission to perform
        unrelated changes. The positive integer budget is an advisory token budget, not money
        or an enforced spending cap. This runtime stores goal state in memory, not a durable
        scheduler; do not assume restarting restores it. Check status before resuming work.</p>
        <p>The model's <code>update_goal</code> accepts optional <code>message</code>,
        <code>completed</code> and <code>blocked_reason</code>. Completion ends goal mode.
        A blocked reason pauses it; the model should report repeated failed attempts rather than
        loop indefinitely. Updating an absent or inactive goal is rejected. A model's completion
        report must still be checked against the actual requested result.</p>
        <h2 id="recurring-prompts">Recurring prompts</h2>
        <p><code>scheduler_create</code> creates a recurring task with <code>interval</code> and
        <code>prompt</code>. Intervals include <code>60s</code>, <code>5m</code>, <code>2h</code>
        and <code>1d</code>; minimum 60 seconds. At most 50 schedules exist and they expire after
        seven days. <code>fire_immediately</code> defaults to false.</p>
        <pre><code class="language-json">{"{\"interval\":\"5m\",\"prompt\":\"Inspect the explicitly agreed build status and report only changes. Do not modify files or publish anything.\",\"fire_immediately\":false}" :: Text}</code></pre>
        <p>Supply an existing <code>task_id</code> to update its interval or prompt in place;
        omitted fields remain unchanged. <code>scheduler_list</code> inspects schedules and
        <code>scheduler_delete</code> removes the selected schedule using its required
        <code>id</code> field. Listing takes no arguments and reports identifiers, prompts,
        intervals and next-fire times. If deletion reports an unknown identifier, list again
        instead of guessing. Deletion stops future fires, not already-completed effects.</p>
        <p><strong>This host supports session-only, detached child-agent fires.</strong>
        <code>durable: true</code>, <code>foreground: true</code> and one-shot scheduling are
        rejected. Closing the session is not a way to keep scheduled work running. Deleting a
        schedule is not evidence that effects from earlier fires were undone; inspect active tasks
        and relevant external state before declaring cleanup complete.</p>
        <h2 id="named-workflows">Named research workflow</h2>
        <p>The current host provides <code>deep-research</code>. The <code>workflow</code> tool
        accepts <code>name</code>, <code>args</code> (a string or an object with
        <code>query</code> or <code>objective</code>) and <code>validate_only</code>.</p>
        <pre><code class="language-json">{"{\"name\":\"deep-research\",\"args\":{\"query\":\"Compare the public compatibility requirements of the two proposed libraries\"},\"validate_only\":true}" :: Text}</code></pre>
        <p>Validate before launching; then explicitly request execution with
        <code>validate_only: false</code>. Use <code>/workflow runs</code> to inspect runs or
        <code>/workflow deep-research QUESTION</code> to launch from the prompt.</p>
        <p>Although present in the dialect schema, custom <code>agent_budget</code>, inline
        <code>script</code>, <code>script_path</code> and <code>resume_from_run_id</code> are
        unsupported by this host. This is not a general Rhai execution service. Research results
        require source review; availability of a workflow does not establish the reliability of
        every external source it reads.</p>
    |]
    }
