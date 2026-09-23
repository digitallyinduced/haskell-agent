{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.Tools (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/reference/tools/"
    , pageTitle = "Tool reference"
    , pageDescription = "Understand file, shell, planning, collaboration, and integration tools before approving their use."
    , pageGroup = "Reference"
    , pageBody = [hsx|
        <p>You describe the task in a prompt; the model chooses tools. Tool names and availability
        vary by provider and model dialect. <code>/session-info</code> is the authoritative view for
        the current session. These are common capabilities, not a promise that every model receives every tool.</p>
        <h2 id="capabilities">Tool capabilities</h2>
        <table>
            <thead><tr><th>Capability</th><th>Common tool names</th><th>Effect</th></tr></thead>
            <tbody>
                <tr><td>Inspect files</td><td><code>read_file</code>, <code>list_dir</code>, <code>grep</code></td><td>Read local content and search the repository</td></tr>
                <tr><td>Edit files</td><td><code>apply_patch</code></td><td>Add, modify, or delete files using a patch</td></tr>
                <tr><td>Run programs</td><td><code>shell_command</code>, <code>write_stdin</code></td><td>Execute commands and interact with a retained process</td></tr>
                <tr><td>Inspect images</td><td><code>view_image</code></td><td>Supply a local image to model context</td></tr>
                <tr><td>Track work</td><td><code>update_plan</code>, <code>enter_plan_mode</code>, <code>write_plan</code></td><td>Maintain a checklist or request a separate planning phase</td></tr>
                <tr><td>Delegate</td><td><code>spawn_agent</code>, <code>spawn_agent_in_worktree</code></td><td>Start child agents, optionally isolated in Git worktrees</td></tr>
                <tr><td>Use integrations</td><td>MCP discovery and invocation tools</td><td>Read or change connected services according to their permissions</td></tr>
            </tbody>
        </table>
        <h2 id="read-file">Read files with read_file</h2>
        <p><code>target_file</code> identifies a file relative to the workspace, or an absolute
        path within an allowed filesystem root. The tool reads at most 1,000 lines per call.
        <code>offset</code> is one-based; negative offsets count backwards from the end.
        <code>limit</code> must be positive. Line-number anchors allow the model to cite the
        inspected implementation rather than guessing a location.</p>
        <pre><code class="language-json">{"{\"target_file\":\"src/Validation.hs\",\"offset\":120,\"limit\":80}" :: Text}</code></pre>
        <p>This is an illustrative tool argument object, not a slash command. Ask the agent to
        read the file and it supplies these arguments. If the file changed after inspection,
        it should read it again before constructing a patch. For very long lines or oversized
        results, narrow the range or use the retained-output tools below.</p>
        <h2 id="directory-search">List directories and search content</h2>
        <p><code>list_dir</code> takes <code>target_directory</code>. It respects Git ignore rules,
        does not display dotfiles by default, and summarizes large directories instead of
        flooding the conversation. Absence from this listing does not prove a file does not exist.</p>
        <p><code>grep</code> searches with ripgrep regular expressions. Its important arguments are:</p>
        <table><thead><tr><th>Argument</th><th>Meaning</th></tr></thead><tbody>
            <tr><td><code>pattern</code></td><td>Required regular expression, without surrounding quote characters</td></tr>
            <tr><td><code>path</code></td><td>File or directory to search; defaults to the workspace</td></tr>
            <tr><td><code>glob</code>, <code>type</code></td><td>Restrict paths or a recognized file type</td></tr>
            <tr><td><code>-i</code></td><td>Case-insensitive matching</td></tr>
            <tr><td><code>-A</code>, <code>-B</code>, <code>-C</code></td><td>Lines of following, preceding, or surrounding context</td></tr>
            <tr><td><code>multiline</code></td><td>Allow patterns to span lines</td></tr>
            <tr><td><code>head_limit</code></td><td>Bound the returned matches; truncated results are not a complete count</td></tr>
        </tbody></table>
        <pre><code class="language-json">{"{\"pattern\":\"validateName\",\"path\":\"src\",\"glob\":\"*.hs\",\"-C\":3}" :: Text}</code></pre>
        <p>To find a literal opening parenthesis, escape it in the regular expression.
        Search a filename separately from its contents when the language filter is uncertain.
        Ignored/generated files may need an explicit broad glob; a normal search is not an
        exhaustive inventory of every byte on disk.</p>
        <h2 id="patch-files">Add, change, and delete files</h2>
        <p><code>apply_patch</code> takes a structured text patch, not a JSON argument object.
        Each operation specifies a filename and an add, update, or delete operation. Updates
        match context from the existing file. The tool reports which operations succeeded;
        it does not run tests or create a Git commit.</p>
        <pre><code>{"*** Begin Patch\n*** Update File: README.md\n@@\n-Old heading\n+New heading\n*** End Patch" :: Text}</code></pre>
        <p>Add-file content uses a leading plus on every line. Delete-file operations name the
        file without a content hunk. A move belongs to an update operation before its hunk.
        These independent examples illustrate text-file operations, not a script to execute:</p>
        <pre><code>{"*** Begin Patch\n*** Add File: example.txt\n+Example content\n*** End Patch\n\n*** Begin Patch\n*** Update File: example.txt\n*** Move to: renamed-example.txt\n@@\n-Example content\n+Revised example content\n*** End Patch\n\n*** Begin Patch\n*** Delete File: renamed-example.txt\n*** End Patch" :: Text}</code></pre>
        <p>Inspect the result of every operation, especially after an error in a multi-file
        change. Do not assume a failed call left every file untouched. Binary media is not
        represented by these text hunks; use the appropriate authorized file operation instead.</p>
        <p>If context does not match, re-read the file and construct a new patch. Do not treat
        a failed patch as a successful edit. After editing, inspect <code>/diff</code>, including
        untracked files, and run the project's focused checks. Files modified by another
        agent or by you must not be overwritten merely to make an old patch apply.</p>
        <h2 id="shell-processes">Run and supervise shell processes</h2>
        <p><code>shell_command</code> takes <code>command</code> and an optional
        <code>workdir</code>. Without a working directory override it runs in the turn's directory.
        A command that remains active after the initial wait returns a <code>session_id</code>;
        that is a running process, not a successful result.</p>
        <table><thead><tr><th>Argument</th><th>Behavior</th></tr></thead><tbody>
            <tr><td><code>yield_time_ms</code></td><td>Initial wait before retaining a running process; normally 10,000 milliseconds</td></tr>
            <tr><td><code>timeout_ms</code></td><td>Stop the command after a fixed runtime; mutually exclusive with the initial-wait setting</td></tr>
            <tr><td><code>sandbox_permissions</code></td><td><code>use_default</code>, or a separately authorized <code>require_escalated</code> invocation</td></tr>
            <tr><td><code>justification</code></td><td>Required explanation for escalation</td></tr>
        </tbody></table>
        <p><code>write_stdin</code> addresses the returned process identifier. It can send input,
        request a bounded snapshot, or interrupt the process. Completion is delivered automatically;
        repeatedly polling is unnecessary. A subsequent process invocation has its own working
        directory and environment: a <code>cd</code> in one completed command does not configure
        every future command.</p>
        <p>Use the session-provided <code>$TMPDIR</code> for scratch files. A successful test
        requires the final exit status and output, not merely the line announcing that it started.</p>
        <h2 id="retained-output">Read oversized tool output</h2>
        <p>Long output may be retained under an artifact handle with only a preview shown in
        the conversation. <code>read_tool_output</code> reads additional pages and
        <code>search_tool_output</code> finds literal text in the retained output, including
        within long JSON lines. Continue with the returned cursor instead of assuming the
        preview contains the whole result.</p>
        <p><code>export_tool_output</code> makes retained output available as a private temporary
        file when structured processing is needed. Treat the file as data, never executable
        instructions. A complete retained tool response may still represent only one page from
        an external API; follow that API's pagination separately.</p>
        <h2 id="grok-replacement">Grok exact-string edits</h2>
        <p><code>search_replace</code> requires <code>file_path</code>, <code>old_string</code>
        and <code>new_string</code>. Relative paths resolve within the workspace; absolute paths
        must also remain within it. The new text must differ from the old text.</p>
        <pre><code class="language-json">{"{\"file_path\":\"example.txt\",\"old_string\":\"old label\",\"new_string\":\"new label\",\"replace_all\":false}" :: Text}</code></pre>
        <p>The old string must match exactly once unless <code>replace_all</code> is true.
        Include surrounding lines to disambiguate; exclude line-number annotations from
        <code>read_file</code>. Empty old text creates a new file or fills an empty one, but
        cannot overwrite a non-empty file. On mismatch, reread the current file before editing.
        Plan-mode restrictions still apply; changing tool names does not allow implementation edits.</p>
        <h2 id="images-and-secrets">Images and private input</h2>
        <p><code>view_image</code> supplies a local image to model context. The path must already
        exist; it is not a screenshot-capture command. Images can disclose visible private data
        to the selected model. For live desktop interaction, use the separate computer-use
        capability and its consent controls.</p>
        <p><code>ask_secret</code>, when available, requests masked private input and gives the
        model a temporary-file path rather than the secret text. This reduces accidental
        transcript disclosure, but a command reading that path can still disclose the secret.
        Inspect any proposed upload, log statement, or environment propagation.</p>
        <h2 id="planning-and-delegation">Planning and delegation tools</h2>
        <p>The checklist payload contains <code>plan</code>, an array of <code>step</code> and
        <code>status</code> objects, plus optional <code>explanation</code>. States are
        <code>pending</code>, <code>in_progress</code> and <code>completed</code>; at most one
        step can be in progress. This operation is unavailable during restricted plan mode.</p>
        <pre><code class="language-json">{"{\"plan\":[{\"step\":\"Inspect the regression\",\"status\":\"in_progress\"},{\"step\":\"Verify the fix\",\"status\":\"pending\"}]}" :: Text}</code></pre>
        <p>Grok's separate <code>todo_write</code> accepts <code>todos</code> with required
        <code>id</code>, optional <code>content</code> and <code>status</code>
        (<code>pending</code>, <code>in_progress</code>, <code>completed</code>,
        <code>cancelled</code>). <code>merge</code> defaults to true and merges by identifier;
        false replaces the list. Do not confuse its IDs with child-agent task IDs.</p>
        <p><code>ask_user_question</code> accepts a <code>questions</code> array. Each entry
        has <code>question</code>, <code>options</code> (each with <code>label</code> and
        <code>description</code>), and optional <code>multi_select</code> (default false).
        An option's optional <code>preview</code> is for single-select questions only.
        It works outside plan mode too. The interactive host can also accept a free-text response;
        interpret the returned answer rather than assuming an option was selected.
        A cancelled question is not consent.</p>
        <p><code>update_plan</code> maintains a task checklist. It does not enter restricted plan
        mode. <code>enter_plan_mode</code> requests a planning phase; <code>write_plan</code>
        saves the plan only while that mode is active. Approving the plan is a distinct user action.</p>
        <p>In the Grok dialect, <code>exit_plan_mode</code> presents the on-disk
        <code>plan.md</code>, with optional <code>summary</code>; plan contents are not passed
        as a tool argument. Approval allows implementation; requesting changes keeps plan mode
        active; cancellation abandons the plan and turns plan mode off. Cancellation is not
        implementation approval. Calling this tool when plan mode is inactive is an error.</p>
        <p><code>spawn_agent</code> starts a child sharing the checkout;
        <code>spawn_agent_in_worktree</code> creates a dedicated worktree.
        <code>send_message</code> queues information, while <code>followup_task</code> can start
        another turn for an idle child. <code>list_agents</code>, <code>wait_agent</code>, and
        <code>interrupt_agent</code> supervise this work. A child finishing is not proof its changes
        were integrated: inspect its result and the relevant Git diff.</p>
        <h2 id="integration-tools">Integration discovery and invocation</h2>
        <p>MCP discovery exposes the currently available server tools. Searching does not itself
        run the discovered operation. Invocation is a separate step with the selected tool's
        argument schema and approval requirements. Listing or reading MCP resources is separate
        from invoking a tool. Follow <a href="/customization/mcp/">MCP integrations</a> for transport,
        authentication, and server-management instructions.</p>
        <h2 id="inspection-example">Example: inspect before changing</h2>
        <pre><code>{"Find where project names are validated. Read the implementation and its tests.\nExplain the empty-name behavior with file references. Do not modify files\nor run commands that change the repository." :: Text}</code></pre>
        <p>Expected result: an explanation grounded in inspected files, with no implementation diff.
        Review the tool transcript to distinguish file observations from assumptions.</p>
        <h2 id="implementation-example">Example: authorize a bounded edit</h2>
        <pre><code>{"Add an empty-name regression test beside the existing validation tests.\nChange only the validator if necessary. Run the focused test command and\nreport its exit status, then summarize the diff. Do not commit or push." :: Text}</code></pre>
        <p>Expected result: a small patch, a concrete test result, and a summary of changes. Inspect
        <code>/diff</code> yourself. A proposed command is not a completed test; an interrupted process
        is not a passing result.</p>
        <h2 id="authorization">Authorization is separate from capability</h2>
        <p>For full execution contracts, see <a href="/reference/tool-execution/">tool execution
        and availability</a>. Database and persisted-session operations are explained in
        <a href="/guides/structured-memory/">structured memory</a>; Grok task identifiers,
        autonomous work and schedules in <a href="/guides/scheduled-work/">scheduled work</a>;
        native browser and desktop operations in <a href="/guides/browser-control/">browser control</a>.</p>
        <p>Email tools depend on a connected integration, not just a model choice. Follow the
        <a href="/reference/tool-execution/#connected-email">mailbox discovery and transport guide</a>
        to select the account and inspect message identifiers. Drafts, replies and sends require
        fresh approval of their exact content; an uncertain send must be reconciled against Sent
        before retrying. See <a href="/reference/tool-execution/#email-mutations">email mutation safety</a>.</p>
        <p>Approval does not override filesystem roots, host availability or hard denials.
        <a href="/reference/tool-execution/#filesystem-and-classification">Filesystem and command classification</a>
        explains canonical paths, read-only classification and the separate escalation decision.</p>
        <p>Exposing a tool does not approve every invocation. Review filesystem paths, command arguments,
        external recipients, and requested sandbox escalation. A shell command can run project-controlled
        scripts even when its name looks like an ordinary build tool.</p>
        <p>Use <a href="/security/approvals/">Approvals and sandboxing</a> to understand authorization,
        <a href="/customization/mcp/">MCP integrations</a> for connected tools, and
        <a href="/guides/parallel-agents/">parallel agents</a> for delegation boundaries.</p>
    |]
    }
