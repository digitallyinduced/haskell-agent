{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.AgentLifecycle (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/guides/agent-lifecycle/"
    , pageTitle = "Agent lifecycle"
    , pageDescription = "Choose an agent's workspace and context, monitor its work, interrupt it safely, and integrate its result."
    , pageGroup = "Using the agent"
    , pageBody = [hsx|
        <p>A subagent is a separate conversation carrying out a delegated task. It can
        inspect files, use tools, and return findings while the coordinating agent
        continues working. A separate conversation is not automatically a separate
        checkout, a separate security boundary, or an independent background service.</p>
        <h2 id="choose-the-kind-of-work">Choose the kind of work</h2>
        <table>
            <thead><tr><th>Need</th><th>Use</th><th>Important boundary</th></tr></thead>
            <tbody>
                <tr><td>Independent research in the same checkout</td><td>A shared-workspace subagent</td><td>All agents see the same files; ask for no edits when appropriate.</td></tr>
                <tr><td>Independent implementation changes</td><td>A subagent in a dedicated Git worktree</td><td>Changes require explicit review and integration.</td></tr>
                <tr><td>A conversation you will direct yourself</td><td><code>/fork</code>, optionally with <code>--worktree</code></td><td>This creates a peer session, rather than delegating a bounded child task.</td></tr>
                <tr><td>A reusable procedure</td><td>A <a href="/customization/skills/">skill</a></td><td>Instructions are not themselves a running agent.</td></tr>
            </tbody>
        </table>
        <p>Tell the coordinating agent your intended outcome and isolation requirement.
        Tool names below explain what you may see in the transcript; they are not
        slash commands to enter into the terminal. The available collaboration
        interface depends on the active model's dialect.</p>
        <h2 id="define-an-assignment">Define a bounded assignment</h2>
        <p>Specify the deliverable, allowed changes, files owned, verification, and
        when to stop. For example:</p>
        <pre><code>{"Create a subagent named parser_review in the current checkout.\nIt must inspect parser code and tests without editing files or running shell commands.\nReport the three most important missing test cases with file references.\nDo not implement fixes. Continue reviewing the public API yourself." :: Text}</code></pre>
        <p>In the collaboration interface, spawning returns a canonical name such as
        <code>/root/parser_review</code>. Nested children extend their parent's path.
        Keep this name when requesting follow-up work so the coordinator can target
        the existing conversation instead of spawning a replacement.</p>
        <p>A read-only assignment is an instruction, not a filesystem lock.
        <a href="/security/approvals/">Approval and sandbox controls</a> still matter.</p>
        <h2 id="workspace-and-shared-state">Workspace and shared state</h2>
        <table>
            <thead><tr><th>State</th><th>Behavior</th><th>Practical consequence</th></tr></thead>
            <tbody>
                <tr><td>Conversation</td><td>Each child retains its own transcript after initialization.</td><td>The parent's later conversation is not continuously copied into the child; send relevant updates explicitly.</td></tr>
                <tr><td>Shared-workspace files</td><td>Normal spawning uses the caller's working directory.</td><td>Two edits to the same file can conflict even though their conversations are separate.</td></tr>
                <tr><td>Worktree files</td><td>Worktree spawning creates a dedicated checkout owned by that child.</td><td>The child does not see arbitrary uncommitted changes in the parent checkout. Confirm its base before depending on recent work.</td></tr>
                <tr><td>Approval and plan controls</td><td>The CLI child runtime uses the session's shared approval policy, allowed-root state, and plan-mode hooks.</td><td>Delegation is not a way around a denied operation or plan-mode editing restriction.</td></tr>
                <tr><td>MCP</td><td>The child runtime receives the session's MCP tools.</td><td>A worktree does not provide a separate external service account or database. External mutations can still affect the same resources.</td></tr>
                <tr><td>Host resources</td><td>Children use the same host and configured services.</td><td>Separate checkouts do not isolate TCP ports, installed dependencies, or provider rate limits.</td></tr>
            </tbody>
        </table>
        <p>For shared-checkout editing, assign non-overlapping file ownership and one
        integration owner. For separate checkouts, specify how the changes will be
        transferred before starting. See <a href="/guides/parallel-agents/">worktree
        base selection and retention</a>.</p>
        <h2 id="context-and-model-selection">Context and model selection</h2>
        <p>The collaboration spawn tools accept a task message and optional history,
        model, and reasoning settings:</p>
        <table>
            <thead><tr><th>Setting</th><th>Meaning</th><th>Constraint</th></tr></thead>
            <tbody>
                <tr><td><code>fork_turns</code> omitted or <code>"all"</code></td><td>Fork the available parent conversation history.</td><td>Inherits the parent model and reasoning effort; explicit overrides are rejected.</td></tr>
                <tr><td><code>fork_turns: "none"</code></td><td>Do not copy the parent transcript.</td><td>The task message must provide the necessary background, paths, and constraints.</td></tr>
                <tr><td><code>fork_turns: "3"</code></td><td>Copy a bounded recent history using a positive integer string.</td><td>Older decisions may be missing; summarize those in the assignment.</td></tr>
                <tr><td><code>model</code></td><td>Select a different allowed child model.</td><td>Requires no-history or bounded-history spawning and must satisfy the host's model policy.</td></tr>
                <tr><td><code>reasoning_effort</code></td><td>Request a child reasoning setting.</td><td>Requires no-history or bounded-history spawning; supported values depend on the selected model.</td></tr>
            </tbody>
        </table>
        <p>Ask for a self-contained assignment when switching models. “Use another
        model” alone does not establish which files, acceptance criteria, or previous
        decisions it should know. If an organization rejects a child model, select
        an allowed model rather than retrying with an invented alias.</p>
        <h2 id="monitor-and-limit-concurrency">Monitor and limit concurrency</h2>
        <pre><code>{"/agents\n/agents limit\n/agents limit 3" :: Text}</code></pre>
        <p>The first command opens agent browsing; the others inspect or change the
        concurrent subagent cap. You can also set it at startup with
        <code>agent-cli --max-concurrent-agents 3</code>. Lowering the cap does not
        interrupt existing work: active agents retain their slots, and the new
        limit applies to subsequent spawn or follow-up admission.</p>
        <p>The collaboration listing distinguishes these states:</p>
        <table>
            <thead><tr><th>State</th><th>Interpretation</th></tr></thead>
            <tbody>
                <tr><td><code>pending_init</code></td><td>Accepted work has not yet entered its running phase.</td></tr>
                <tr><td><code>running</code></td><td>The current task is active; it may be generating, using a tool, or waiting.</td></tr>
                <tr><td><code>completed</code></td><td>The turn finished; inspect its final report and evidence.</td></tr>
                <tr><td><code>errored</code></td><td>The turn failed; inspect the error before retrying.</td></tr>
                <tr><td><code>interrupted</code></td><td>The turn was stopped, not necessarily rolled back.</td></tr>
                <tr><td><code>shutdown</code></td><td>The agent has been closed.</td></tr>
            </tbody>
        </table>
        <p>Completion notifications reach the coordinator. It can wait for an update
        instead of repeatedly listing agents. A timeout while waiting is not itself
        proof that the child failed.</p>
        <p><code>wait_agent</code> accepts optional <code>timeout_ms</code> (default 30,000).
        <code>list_agents</code> accepts optional <code>path_prefix</code>, without a trailing
        slash, to restrict the tree. Use returned canonical task paths when names are ambiguous.
        Neither operation starts a fresh child turn.</p>
        <pre><code class="language-json">{"{\"path_prefix\":\"/root/parser_review\"}" :: Text}</code></pre>
        <p>These short-lived children are not independent durable conversations. For explicit
        cross-session creation, reading, messaging and waiting, see
        <a href="/guides/structured-memory/#independent-sessions">persisted sessions</a>.
        Grok's typed <code>task</code> interface is documented under
        <a href="/guides/scheduled-work/">scheduled work</a>.</p>
        <h2 id="messages-and-follow-up">Messages versus follow-up tasks</h2>
        <p><code>send_message</code> delivers information without starting a new turn
        when the target is idle. <code>followup_task</code> sends an assignment and
        starts a turn for an idle child; for a running child, it is delivered at
        message boundaries. These are different from creating another agent.</p>
        <pre><code>{"Send parser_review this clarification: focus on malformed nested input.\nWhen its review is complete, give that same agent a follow-up task:\nrank the proposed tests by regression risk, without editing files." :: Text}</code></pre>
        <p>If a completed agent does not answer an informational message, ask the
        coordinator to give it a follow-up task. Do not assume every message causes
        fresh model execution.</p>
        <h2 id="interrupt-and-close">Interrupt, resume, and close</h2>
        <p>Ask the coordinator to interrupt a named child when its current work must
        stop. <code>interrupt_agent</code> requests cancellation and returns the
        previous status; that response is not a new completion report. The agent
        remains available for messages and follow-up tasks.</p>
        <pre><code>{"Interrupt parser_review. Check its resulting status and any files it changed.\nDo not restart it until you have reported what completed and what remains uncertain." :: Text}</code></pre>
        <p>Cancellation does not undo edits or an external action that already
        completed. Inspect the diff, process state, and external service before
        retrying a mutation. Aborting a root turn also interrupts pending or running
        child work owned by that turn and discards its queued work; do not treat
        children as independent services that will necessarily survive cancellation.</p>
        <p>Closing is stronger than interrupting: registry shutdown closes the agent
        and its descendants and releases owned resources. A child-owned worktree
        is scheduled for cleanup when its owner closes. Preserve and integrate
        required changes before closure rather than using that worktree as permanent
        storage. Exact close controls depend on the available dialect and host.</p>
        <h2 id="verify-and-integrate">Verify completion and integrate results</h2>
        <ol>
            <li>Read the final report and distinguish completed work from suggestions.</li>
            <li>For research, verify important claims against cited files or tool output.</li>
            <li>For implementation, inspect the exact checkout, changed files, and diff.</li>
            <li>Request the precise test commands, exit results, and untested boundaries.</li>
            <li>For a separate worktree, explicitly transfer the reviewed change into the
            intended branch. Completion does not automatically merge it.</li>
            <li>Run integration checks against the combined result, not only each
            child's isolated change.</li>
        </ol>
        <p>The <a href="/tutorials/parallel-changes/">parallel-changes tutorial</a>
        demonstrates an explicit review and integration workflow. Keep the
        coordinating agent responsible for the final result rather than treating
        several successful reports as proof that the combined application works.</p>
        <h2 id="lifecycle-troubleshooting">Troubleshooting</h2>
        <table>
            <thead><tr><th>Symptom</th><th>Next action</th></tr></thead>
            <tbody>
                <tr><td>Spawn rejected by concurrency policy</td><td>Inspect active agents and wait for completion or interrupt unnecessary work. Increase the cap only when the host and provider can support it.</td></tr>
                <tr><td>Child cannot find the parent's new file</td><td>Check whether it has a separate worktree and which base it uses. Supply or integrate the required change explicitly.</td></tr>
                <tr><td>Conflicting edits</td><td>Stop overlapping writers, inspect the shared diff, and assign a single integration owner.</td></tr>
                <tr><td>Model override rejected</td><td>Check allowed model IDs and use <code>"none"</code> or a positive integer for <code>fork_turns</code>, not <code>"all"</code>.</td></tr>
                <tr><td>Message sent but no answer</td><td>An idle target needs a follow-up task, not only an informational message.</td></tr>
                <tr><td>Completed report but no changes in the parent</td><td>Inspect the child worktree and integration plan. A report is not a merge.</td></tr>
                <tr><td>Interrupted operation has an uncertain outcome</td><td>Inspect local or remote state before retrying. Do not equate cancellation with rollback.</td></tr>
            </tbody>
        </table>
    |]
    }
