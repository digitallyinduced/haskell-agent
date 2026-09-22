{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.Sessions (page) where

import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/guides/sessions/"
    , pageTitle = "Sessions"
    , pageDescription = "Resume, search, name, export, and compact persistent conversations."
    , pageGroup = "Using the agent"
    , pageBody = [hsx|
        <p>A session is a saved conversation with its working-directory and model state.
        Interactive work is persisted so you can return to it after leaving the agent.
        The harness uses local PostgreSQL-backed storage; a saved session is not an
        off-machine backup of your project.</p>
        <h2 id="resume-previous-work">Resume previous work</h2>
        <p>Inside the agent:</p>
        <pre><code>/resume</code></pre>
        <p>This opens the session picker in a terminal. If you already have an identifier:</p>
        <pre><code class="language-sh">agent-cli --resume SESSION_ID</code></pre>
        <p>Use <code>/session</code> to print the current identifier or <code>/copy-session</code> to copy it.
        <code>/search &lt;query&gt;</code> searches previous conversations and lets you resume a match.
        <code>/home</code> returns to the session picker.</p>
        <h2 id="give-a-session-a-useful-title">Give a session a useful title</h2>
        <p>A descriptive title makes the picker useful when several conversations
        concern the same repository.</p>
        <pre><code>/rename Project name validation</code></pre>
        <p><code>/rename --auto</code> restores automatic naming. The title model is independent of
        the coding model: <code>/title-model</code> selects it and <code>/title-model --auto</code> restores
        automatic selection. On supported Macs, automatic naming uses on-device Apple
        Intelligence when available and falls back when it cannot be used.</p>
        <p><code>/title</code> is an alias for <code>/rename</code>. A manual title
        may contain at most 100 characters; use a short distinguishing description
        rather than pasting the task specification.</p>
        <h2 id="example-return-to-a-task">Example: return to an unfinished task</h2>
        <ol>
            <li>Run <code>/rename Project name validation</code>, then <code>/session</code>.
            Save the displayed identifier somewhere private.</li>
            <li>Ask: <code>Summarize the current findings and remaining checks. Do not make further changes.</code></li>
            <li>Exit with <code>/quit</code>. Later run <code>agent-cli --resume SESSION_ID</code>
            in your shell, replacing the placeholder with the saved identifier.</li>
            <li>Run <code>/session</code> again and confirm it matches before continuing.</li>
        </ol>
        <p><strong>Expected result:</strong> the saved conversation is available in the resumed
        session. Inspect the current files before relying on an earlier test result:
        other tools or people may have changed the checkout while you were away.
        If you lost the identifier, use <code>/resume</code> and look for the title rather
        than starting over with <code>/delete</code>.</p>
        <h2 id="start-over-without-confusing-the-operations">Start over without confusing the operations</h2>
        <table>
            <thead><tr><th>Command</th><th>Effect</th></tr></thead>
            <tbody>
                <tr><td><code>/new</code></td><td>Start a fresh persisted session identifier</td></tr>
                <tr><td><code>/clear</code></td><td>Reset the live conversation while retaining the session identifier</td></tr>
                <tr><td><code>/delete</code></td><td>Delete the current session and start fresh</td></tr>
                <tr><td><code>/fork</code></td><td>Create a peer session from the current chat</td></tr>
            </tbody>
        </table>
        <p>Do not use <code>/delete</code> when you merely want to leave a session; use <code>/quit</code>.
        These are conversation operations, not a general undo for filesystem changes.</p>
        <h2 id="rewind-and-delete">Rewind safely, or permanently delete</h2>
        <p>Run <code>/rewind</code> (alias <code>/undo</code>) to select an earlier user prompt.
        The conversation is restored to immediately before that prompt, later turns are
        removed, and the chosen prompt becomes an editable draft. It is not sent
        automatically. The confirmation defaults to cancel.
        <strong>Files are unchanged.</strong> Inspect <code>/diff</code>
        before resubmitting: repeating an edit or an external mutation can duplicate work.</p>
        <p>For example, rewind an overly broad request, narrow the returned draft to one
        file, inspect existing changes, then send it. If you only want to reuse wording,
        choose <code>/history</code> or <code>/edit-prompt</code> instead of discarding turns.</p>
        <p><code>/delete</code> asks for confirmation and defaults to cancellation.
        Confirming permanently removes the current transcript and session-local artifacts,
        then starts a new session. Export anything you need before confirming.
        This is not an archive or a reversible way to close a tab. It does not revert
        project files or undo actions at external services.</p>
        <h2 id="resume-troubleshooting">When resuming does not work</h2>
        <p>Use <code>agent-cli sessions list</code> to check the identifier and the account's
        local session store. Do not substitute a title for an identifier. If a managed
        checkout was collected, resuming can restore it; if the original shared repository
        or registry is missing, restore those from your backups rather than creating an
        unrelated repository at the same path. An existing directory blocks restoration
        to prevent overwriting it. See <a href="/guides/parallel-agents/#restore-a-checkout">worktree recovery</a>.</p>
        <h2 id="afk-handoff">Keep a session in tmux</h2>
        <p><code>/afk</code> starts a detached local tmux session using the current agent
        executable. It waits for the original session to release its run lock before
        resuming. Install <code>tmux</code> first; use the exact
        <code>tmux attach -t NAME</code> command printed on success to reconnect.</p>
        <pre><code>/afk build-host:/absolute/path/to/project</code></pre>
        <p>Remote handoff requires working SSH access, <code>tar</code>, remote
        <code>agent-cli</code>, and remote <code>tmux</code>. The destination project directory
        must already exist and contain the files needed for the task. Handoff transfers
        the conversation and session artifacts, <strong>not the project checkout or
        provider credentials</strong>. Authorize the remote agent separately. The
        destination is <code>HOST:PATH</code>, not a URL.</p>
        <p>Session artifacts can contain private data. Use only a trusted destination.
        After success, use the printed SSH/tmux attach command. If upload, import, or
        tmux startup fails, read the reported stage, inspect whether a remote session
        already exists, and reconnect rather than blindly repeating the transfer.
        Handoff is multi-step, not an atomic migration; a failed attempt may leave staged
        artifacts or an imported session on the host.</p>
        <h2 id="manage-long-conversations">Manage long conversations</h2>
        <p><code>/context</code> shows context-window usage and estimates. <code>/compact</code> summarizes
        history to free model context:</p>
        <pre><code>/compact Preserve the API decisions, modified files, and remaining test failures.</code></pre>
        <p>Compaction behavior depends on the provider. Custom portable models need a
        configured context-window limit so the harness can bound the summary request;
        it does not guess this limit. A summary can omit details, so keep durable
        requirements in project files rather than only in a long conversation.</p>
        <h2 id="session-browser">Browse and search saved conversations</h2>
        <p><code>/resume</code> without an ID and <code>/home</code> (alias
        <code>/welcome</code>) open the same session browser. Opening it does not itself
        start a fresh conversation or clear saved history. Cancel to keep the current
        session; selecting the already-active ID reports that you are already on it.
        A failed load or rejected organization-identity boundary leaves you in the
        current conversation. Copy important unsent draft text before deliberately
        switching sessions rather than treating drafts as exported history.</p>
        <table><thead><tr><th>Fullscreen browser key</th><th>Action</th></tr></thead><tbody>
            <tr><td>Up / Down; Page Up / Page Down</td><td>Move one entry or ten entries.</td></tr>
            <tr><td>Enter / Escape</td><td>Resume the selected entry / cancel the browser.</td></tr>
            <tr><td><code>e</code></td><td>Expand the selected conversation preview.</td></tr>
            <tr><td><code>f</code></td><td>Cycle the source filter.</td></tr>
            <tr><td><code>d</code>, then <code>y</code></td><td>Delete a selected saved session after confirmation. The active session cannot be deleted here. Use <code>n</code> or Escape to cancel.</td></tr>
            <tr><td><code>/</code> or other printable text</td><td>Enter search; Enter runs the search, Ctrl+U clears its query, Escape leaves search mode.</td></tr>
        </tbody></table>
        <pre><code>/search parser regression</code></pre>
        <p>This searches indexed conversation turns within the current local/organization
        boundary, rather than searching repository files. Select the desired result to
        resume it, then inspect <code>/session-info</code> before continuing. The query
        requests up to 100 search results; it is not an unbounded export. No matches
        reports <code>no conversations matched</code> and keeps the current session.
        Use <code>/find</code> instead to search only the active saved transcript.</p>
        <h2 id="inspect-session-state">Interpret session information</h2>
        <p><code>/context</code> reports the active model, used tokens, known window and
        free space, plus an estimated breakdown for instructions, messages, tool schemas
        and other overhead. A line such as <code>Used: 12000 tokens (estimated)</code>
        is an illustrative local estimate, not a provider bill. The label
        <code>provider reported</code> is used only while that occupancy snapshot still
        matches committed history; otherwise the serialized request is estimated again.
        Breakdown entries remain estimates even when the headline is provider-reported.
        An unknown model window yields unknown capacity/free-space fields rather than
        an invented limit.</p>
        <p><code>/session-info</code>, <code>/status</code> and <code>/info</code> report
        the session ID, persistence state, optional title, provider, connection, model,
        dialect, effort, working directory, shell mode, token usage and registered tool
        names. <code>pending</code> means an ID is reserved but persistence is not yet
        active; <code>active</code> means the session has a persisted handle;
        <code>not_persisted</code> means persistence is disabled. Check
        <code>connection</code> as well as <code>provider</code> before sending confidential
        work: the transport name alone does not identify the endpoint.</p>
        <p><code>/clear</code> retains this identity but records a transcript-reset marker
        and resets the current provider conversation, task plan, token counters and recap
        metadata. It is not secure erasure of historical storage. If the persistent write
        fails, the command reports <code>could not clear conversation</code> and does not
        proceed with the normal in-memory reset. Resolve the storage error rather than
        assuming the conversation was cleared.</p>
        <h2 id="export-a-conversation">Export a conversation</h2>
        <pre><code>/export /absolute/path/to/conversation.md</code></pre>
        <p>Without a path, <code>/export</code> offers copying Markdown or saving a file.
        The suggested filename is <code>agent-session-&lt;session-id&gt;.md</code>.
        Relative paths are resolved against the current working directory. Saving never
        overwrites an existing file: choose a new filename when it reports a collision.
        A failed write reports the destination and error; fix the directory or permissions
        before retrying. Export requires an active persisted session and exports its
        visible transcript, not a portable session-import archive.</p>
        <p>Review exports before sharing: prompts, tool results, and source excerpts may
        contain confidential material.</p>
        <p>For a one-shot command, opt into persistence:</p>
        <pre><code class="language-sh">agent-cli --save-session -p "Review the current changes without modifying files."</code></pre>
    |]
    }
