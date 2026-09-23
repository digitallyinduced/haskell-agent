{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.Commands (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/reference/commands/"
    , pageTitle = "Command reference"
    , pageDescription = "Common launch options and interactive commands for Haskell Agent."
    , pageGroup = "Reference"
    , pageBody = [hsx|
        <p>Run <code>agent-cli --help</code> for launch options. Inside a session, <code>/help</code> lists
        commands and <code>/help NAME</code> describes one. Availability can depend on the active
        provider, dialect, and enabled tools.</p>
        <h2 id="launch-options">Launch options</h2>
        <table>
            <thead><tr><th>Option</th><th>Purpose</th></tr></thead>
            <tbody>
                <tr><td><code>--cwd DIR</code></td><td>Set the working directory for tools</td></tr>
                <tr><td><code>--worktree</code></td><td>Create a managed Git worktree</td></tr>
                <tr><td><code>--provider NAME</code></td><td>Select <code>openai</code>, <code>xai</code>, <code>openrouter</code>, <code>gemini</code>, or <code>claude-code</code></td></tr>
                <tr><td><code>--model NAME</code></td><td>Override the saved last model</td></tr>
                <tr><td><code>--resume ID</code></td><td>Resume a persisted session</td></tr>
                <tr><td><code>-p TEXT</code></td><td>Run one prompt and exit</td></tr>
                <tr><td><code>--prompt-file FILE</code></td><td>Read a one-shot prompt from a file</td></tr>
                <tr><td><code>--save-session</code></td><td>Persist a one-shot run</td></tr>
                <tr><td><code>--max-turns N</code></td><td>Limit model turns</td></tr>
                <tr><td><code>--max-concurrent-agents N</code></td><td>Set the concurrent subagent cap</td></tr>
                <tr><td><code>--fullscreen</code></td><td>Use the retained full-screen interface</td></tr>
                <tr><td><code>--minimal</code></td><td>Use terminal-native append-only output</td></tr>
                <tr><td><code>--yolo</code></td><td>Auto-approve ordinary tools</td></tr>
                <tr><td><code>--no-yolo</code></td><td>Deny mutations when no TTY can request approval</td></tr>
                <tr><td><code>--no-computer-use</code></td><td>Hide local desktop control</td></tr>
                <tr><td><code>--ghci</code></td><td>Enable the optional persistent GHCi tool</td></tr>
                <tr><td><code>--no-bash</code></td><td>Disable shell execution tools</td></tr>
                <tr><td><code>--no-agents-md</code></td><td>Skip project-instruction discovery</td></tr>
                <tr><td><code>--no-skills</code></td><td>Disable filesystem skill discovery</td></tr>
            </tbody>
        </table>
        <h2 id="launch-defaults">Launch defaults and additional controls</h2>
        <p>Shell execution, project-instruction discovery, and filesystem skills are enabled
        by default; persistent GHCi is disabled. Desktop control defaults to enabled only for
        interactive, non-one-shot runs with a TTY, where supported. A prompt supplied with
        <code>-p</code> does not enable desktop control by default, even from a terminal.
        Provider and model selection can inherit saved configuration, so
        specify both when a repeatable automation depends on a particular backend.</p>
        <table><thead><tr><th>Option</th><th>Meaning</th></tr></thead><tbody>
            <tr><td><code>--effort LEVEL</code></td><td>Set reasoning effort; valid levels depend on the selected model</td></tr>
            <tr><td><code>--compact-threshold N</code></td><td>Positive automatic-compaction threshold in tokens</td></tr>
            <tr><td><code>--show-raw-reasoning</code></td><td>Display raw OpenAI reasoning when available</td></tr>
            <tr><td><code>--motion full|reduced|off</code></td><td>Animation policy; default is full</td></tr>
            <tr><td><code>--bash</code>, <code>--no-ghci</code></td><td>Explicitly enable shell execution or disable persistent GHCi</td></tr>
            <tr><td><code>--agents-md</code>, <code>--skills</code></td><td>Explicitly enable instruction or skill discovery</td></tr>
            <tr><td><code>--computer-use</code></td><td>Explicitly enable supported desktop tools; approval is still separate</td></tr>
            <tr><td><code>--code-mode</code></td><td>Enable JavaScript tool orchestration when the model catalog does not select a tool mode</td></tr>
            <tr><td><code>--no-code-mode</code></td><td>Disable full code mode even when selected by the catalog</td></tr>
        </tbody></table>
        <p>Code mode is a model-facing tool orchestration capability, not the documentation
        renderer or a JavaScript application requirement. Internal managed-turn flags are
        intended for harness components rather than ordinary shell automation.</p>
        <h2 id="argument-contract">Argument validation and diagnostics</h2>
        <p><code>--prompt TEXT</code> is the long form of <code>-p TEXT</code>. Choose exactly
        one prompt source: <code>-p</code>, <code>--prompt-file</code>, or the internal
        <code>--managed-turn-file</code>. Do not combine <code>--resume</code> and
        <code>--worktree</code>: resume reopens an existing session, whereas worktree creates
        a new checkout. Repeated run options are applied in order; the last assignment wins.</p>
        <p>Turn, concurrency, and compaction limits require positive integers, not zero,
        fractions, or an unlimited sentinel. The default model-turn cap is 2000;
        choose a smaller explicit <code>--max-turns</code> for a bounded review.
        Correct a parser error before retrying;
        changing the prompt cannot repair an invalid option. Use
        <code>agent-cli --version</code> in bug reports and <code>agent-cli --help</code>
        (also <code>-h</code>) to compare your installed executable with this reference.</p>
        <p><code>--managed-turn-file FILE</code> supplies an internal managed-turn request;
        it is not a text prompt file. <code>--managed-deny-mutations</code> is an internal
        worker policy flag rather than a replacement for public <code>--no-yolo</code>
        automation. Do not construct internal requests by copying a transcript export.</p>
        <h2 id="managed-turn-payload">Managed integration turn payload</h2>
        <p><code>--managed-turn-file FILE</code> is an internal integration interface, not the
        ordinary text-file prompt option. The version-1 JSON object requires <code>text</code>.
        Omitted <code>version</code> defaults to 1; other versions are rejected.
        <code>images</code> and <code>files</code> default to empty arrays. Each media object
        requires <code>path</code> and <code>mime</code>, with optional <code>name</code>.
        Paths identify readable files in the launched process's filesystem, not remote URLs.
        Their bytes are loaded as attachments; the declared MIME type is supplied to the model.</p>
        <pre><code class="language-json">{"{\"version\":1,\"text\":\"Describe the attached screenshot without modifying anything.\",\"images\":[{\"path\":\"/absolute/path/screenshot.png\",\"mime\":\"image/png\"}],\"files\":[]}" :: Text}</code></pre>
        <p>Optional <code>bridge_directory</code> is the integration's bridge path.
        Optional <code>context</code> requires string <code>gateway</code> and integer
        <code>chat_id</code> and <code>user_id</code>; integer <code>message_thread_id</code>
        and <code>reply_to_message_id</code> are optional. These describe integration context,
        not permission to impersonate a user. Producers must preserve the real authorized context.
        Keep request files and attachments private. A missing request file, malformed JSON or
        unsupported version fails loading; fix the producer payload instead of retrying a
        remote action blindly. Use <code>--prompt-file</code> for normal text-only automation.</p>
        <h2 id="one-shot-automation">Run a bounded one-shot task</h2>
        <pre><code class="language-sh">{"agent-cli --cwd /absolute/path/to/project --no-yolo --no-computer-use \\\n  --max-turns 6 --save-session \\\n  -p 'Read the README and report the documented test command. Do not edit files.'" :: Text}</code></pre>
        <p>Replace the project path. <code>-p</code> runs one prompt and exits rather than opening
        the normal composer. <code>--save-session</code> retains the conversation for later
        inspection; without it, ordinary one-shot runs are not persisted as sessions.
        <code>--no-yolo</code> prevents a headless process from silently approving mutations.
        It may therefore prevent a requested build or test that writes files.</p>
        <p>For a longer specification, use <code>--prompt-file review-request.txt</code>.
        Keep secrets out of both prompt files and shell arguments. <code>--max-turns</code>
        bounds model turns, not elapsed seconds or monetary cost. Inspect the final output:
        reaching a limit is not proof that the requested task completed.</p>
        <p>Capture stdout and stderr separately: assistant text is streamed to stdout;
        activity, errors and resume hints can appear on stderr. The run command has no
        structured-output flag: do not parse this stream as the JSON returned by the
        administrative <code>sessions</code> commands. For retained structured inspection,
        save the session and use <code>sessions show ID --json</code> afterward.</p>
        <p>A failed foreground one-shot turn exits unsuccessfully. A completed turn exits
        normally, but cancellation can also follow the normal quit path: exit status zero
        alone does not prove completion or successful tests. Have automation check the
        requested result and explicit test evidence as well as process status. Use an
        external timeout for a wall-clock limit, and treat interrupted output as incomplete.
        Before rerunning, inspect persisted history and external state; a retry is not an
        exactly-once transaction and may repeat effects already performed.</p>
        <h2 id="storage-subcommands">Manage local storage</h2>
        <p>These commands manage the local PostgreSQL cluster, not an external database.
        Use the packaged PostgreSQL 18 binaries, run as the owning user, and keep its state
        directory writable with sufficient free disk space. <code>start</code> initializes
        a missing cluster, starts a stopped cluster, ensures the database and applies
        migrations. <code>migrate</code> also starts storage if necessary; it is not a
        PostgreSQL major-version upgrade.</p>
        <table><thead><tr><th>Command</th><th>Operation</th></tr></thead><tbody>
            <tr><td><code>agent-cli storage status</code></td><td>Inspect managed PostgreSQL status</td></tr>
            <tr><td><code>agent-cli storage start</code></td><td>Start the local database</td></tr>
            <tr><td><code>agent-cli storage doctor</code></td><td>Check storage health</td></tr>
            <tr><td><code>agent-cli storage migrate</code></td><td>Apply storage migrations</td></tr>
            <tr><td><code>agent-cli storage stop</code></td><td>Stop the managed database; do not run while another agent needs it</td></tr>
        </tbody></table>
        <h3 id="storage-diagnostics">Interpret storage diagnostics</h3>
        <table><thead><tr><th>Output or failure</th><th>Next step</th></tr></thead><tbody>
            <tr><td><code>managed PostgreSQL is not initialized</code></td><td>On first use, run storage start. If prior data is expected, check the selected state directory before initializing a different one.</td></tr>
            <tr><td><code>managed PostgreSQL is stopped</code></td><td>Run storage start, then storage doctor.</td></tr>
            <tr><td><code>managed PostgreSQL is running</code></td><td>This is process status; doctor additionally opens and closes a database connection pool.</td></tr>
            <tr><td><code>managed PostgreSQL is healthy; socket: ...</code></td><td>Doctor connected successfully. The actual socket directory replaces the ellipsis; this is not a complete data-integrity check.</td></tr>
            <tr><td>Startup or connection failure</td><td>Inspect the managed cluster's postgres.log, executable availability, ownership, free space and socket directory. Do not delete lock or PID files from a running server.</td></tr>
            <tr><td>Existing cluster has another major version</td><td>Preserve it and plan a PostgreSQL pg_upgrade or logical export/import with compatible tooling. Do not rewrite PG_VERSION or treat storage migrate as that upgrade.</td></tr>
        </tbody></table>
        <p>Successful start reports <code>managed PostgreSQL is running and migrations are up to date</code>;
        successful migrate reports <code>managed PostgreSQL migrations are up to date</code>.
        The default socket port number is 55432, but TCP listening is disabled. A normal
        unrelated TCP service on that port is therefore not the first explanation for failure.
        Check the actual Unix socket and server identity before stopping another process.
        The log is <code>postgres.log</code> beside the cluster's <code>data</code> and
        <code>run</code> directories.</p>
        <h3 id="storage-migration-recovery">Before migration and after failure</h3>
        <p>Arrange a maintenance window and stop agent clients that write to this store.
        Make a verified PostgreSQL backup and preserve the associated agent configuration
        and filesystem artifacts. A plain copy of a live data directory is not a reliable
        backup; use PostgreSQL-aware backup tooling or a confirmed stopped-cluster copy.
        Keep the original backup separate from the migration target.</p>
        <p>Run <code>agent-cli storage migrate</code>, check its result, then run
        <code>agent-cli storage doctor</code> and inspect a known session. If migration fails,
        retain the exact error and postgres.log, fix the reported cause and retry only with
        the intended compatible build. There is no CLI rollback command. Do not edit migration
        bookkeeping or delete the cluster to silence an error. If recovery requires restoring,
        stop clients and the cluster, preserve the failed state for diagnosis and restore a
        verified compatible backup under the original owner before restarting.</p>
        <h2 id="session-subcommands">Inspect sessions from scripts</h2>
        <pre><code class="language-sh">{"agent-cli sessions list --json\nagent-cli sessions show SESSION_ID --json --limit 50\nagent-cli sessions show SESSION_ID --json --before TURN_INDEX --limit 50\nagent-cli sessions wait SESSION_ID" :: Text}</code></pre>
        <p>Substitute an identifier from the list. <code>sessions</code> without a subcommand lists
        sessions in human-readable form. <code>--json</code> selects machine-readable output.
        For transcript pagination, <code>--before</code> requests older turns than the supplied
        cursor and <code>--limit</code> bounds turns, with a maximum of 500. Use returned cursor
        information rather than assuming a page is the complete transcript.
        <code>wait</code> waits for the session's lifetime lock to be released; it does
        not submit a new task or report model-task success. It exits silently once the
        lock is inactive. An invalid identifier or missing session directory fails;
        an unlocked existing directory returns immediately. This is useful for handoff,
        not as a task-result API.</p>
        <p><code>agent-cli sessions import [--cwd DIR]</code> reads the harness's transferred-session
        JSON format from standard input and prints the imported session identifier.
        It is not a general Markdown-file importer, and a transcript listing is not necessarily
        a transferable session document.
        Interactive <code>/export</code> produces a readable Markdown record instead.</p>
        <h3 id="session-transfer">Import a session transfer</h3>
        <p><code>agent-cli sessions import --cwd /absolute/project &lt; transfer.json</code>
        reads one raw transfer object from stdin and prints its imported session ID.
        The supported producer used by the product is remote <code>/afk</code>, which
        serializes the active session and pipes it into this command over SSH. Prefer that
        handoff for moving work; neither <code>/export</code> Markdown nor
        <code>sessions show --json</code> is this transfer representation.</p>
        <table><thead><tr><th>Object</th><th>Contract</th></tr></thead><tbody>
            <tr><td>Top level</td><td>Required <code>meta</code> and <code>turns</code>; optional <code>currentTaskPlan</code></td></tr>
            <tr><td>Metadata</td><td>Required <code>version</code>, <code>id</code>, <code>createdAt</code>, <code>updatedAt</code>, <code>provider</code>, <code>model</code>, <code>cwd</code>, <code>effort</code>, <code>title</code>. Connection, dialect and provider continuation fields must retain the source's values and compatible types.</td></tr>
            <tr><td>Turns</td><td>Each requires <code>at</code>, <code>userText</code> and canonical <code>items</code>. Optional assistant/error/response ID, display-only items, usage, telemetry and transcript effect preserve execution history.</td></tr>
            <tr><td>Task plan</td><td><code>explanation</code> and <code>plan</code> items with <code>step</code> and <code>status</code></td></tr>
        </tbody></table>
        <p>This is a version-coupled runtime format, not a promise that arbitrary third-party
        chat JSON can be imported. Preserve canonical response items from the producer rather
        than fabricating them from visible assistant text. An import preserves the source ID;
        it fails instead of overwriting an existing session. <code>--cwd</code> overrides
        the stored directory, but does not copy that directory or credentials. Invalid JSON
        reports <code>invalid transferred session</code>; storage or duplicate-ID failures
        must be resolved before retrying. Inspect <code>sessions show ID</code> after success
        before resuming. Do not delete an existing conversation merely to bypass a collision.</p>
        <h3 id="session-json">JSON shapes and pagination</h3>
        <p><code>sessions list --json</code> writes an array of summaries to stdout;
        warnings go to stderr. Summary fields are <code>id</code>, <code>title</code>,
        <code>updatedAt</code>, <code>provider</code>, <code>model</code>, <code>effort</code>,
        <code>cwd</code>, <code>isRunning</code>, <code>isLocked</code>, and
        <code>isArchived</code>. Running and locked are distinct observations, not proof
        that a task succeeded.</p>
        <p>The list includes archived entries but excludes deleted sessions. It is not
        filtered to the current working directory. Ordering is newest
        <code>updatedAt</code> first, with session key as the tie-breaker. Corrupt or
        incompatible metadata produces stderr warnings and is omitted from the array;
        therefore an empty array plus warnings is not proof that storage contains no records.
        Apply your own explicit project/archive filter when scripting rather than taking
        the first entry as the current conversation.</p>
        <p><code>sessions show ID --json</code> without pagination returns
        <code>meta</code> and all <code>turns</code>. Each turn contains
        <code>userText</code>, <code>assistantText</code>, <code>error</code>, and
        <code>toolEvents</code>. This is a display-oriented transcript, not a complete
        provider request dump or the transfer format accepted by <code>import</code>.</p>
        <ol>
            <li>Request <code>sessions show ID --json --limit 50</code>.</li>
            <li>Read <code>page.hasOlder</code>. When true, find the smallest
            <code>index</code> in the returned <code>turns</code>.</li>
            <li>Request <code>sessions show ID --json --before INDEX --limit 50</code>
            with that index. The cursor is exclusive.</li>
            <li>Continue until <code>hasOlder</code> is false, preserving turn indexes
            when assembling chronological output.</li>
        </ol>
        <p>Paginated output adds each turn's <code>index</code> and a <code>page</code>
        object with <code>generationStart</code>, <code>totalTurns</code>,
        <code>hasOlder</code>, and <code>hasNewer</code>. Do not invent a
        <code>nextCursor</code> field. Pagination requires <code>--json</code>;
        <code>--limit</code> must be between 1 and 500; the cursor is a nonnegative
        integer. Supplying <code>--before</code>
        without a limit uses 50. An unknown session or storage failure is an error,
        not an empty successful transcript. Keep stderr separate from JSON and check
        process success before parsing output.</p>
        <h2 id="mcp-subcommands">Manage MCP connections outside the TUI</h2>
        <h3 id="mcp-oauth-identity">OAuth scopes and logout identity</h3>
        <p>Repeat <code>--scope SCOPE</code> on <code>agent-cli mcp login URL</code> for step-up
        authorization. The requested set combines the selected challenge/resource/configuration
        scope set with previously granted scopes and the additional flags, without duplicates.
        Previous grants and client registration are reused only when issuer and resource match.
        <code>offline_access</code> is requested only when advertised by the authorization server.
        Login starts authorization; a scope flag does not itself grant access. Review consent and
        verify the intended account with a harmless read after reconnecting.</p>
        <p>Credential files under <code>~/.haskell-agent/credentials/mcp/</code> are keyed from
        the exact endpoint string. Use the same URL spelling, including its query, for login and
        logout. <code>agent-cli mcp logout URL</code> removes that local credential file if present.
        It neither removes the configured server nor calls a provider revocation endpoint.
        A separately running process can retain in-memory credentials: disable/restart its
        MCP runtime to end that connection, and use the service's account controls if you need
        to revoke the grant remotely. Do not assume logout undoes prior tool effects.</p>
        <h3 id="mcp-server-prompts">Submit a server-provided prompt</h3>
        <pre><code>/mcp prompt SERVER PROMPT_NAME topic=compatibility</code></pre>
        <p>Replace both names with a prompt actually advertised by your connected server.
        <code>/mcps</code> aliases <code>/mcp</code>. Arguments are string key/value tokens
        split at the first equals sign; a token without equals has an empty value. Follow
        the server's argument names and required fields; do not assume shell quoting or JSON
        objects are interpreted as typed arguments. A missing server or prompt error is
        displayed and returns to the prompt without starting a model turn.</p>
        <p>A successful response is rendered and submitted as an expanded user turn to the
        current model, not merely inserted into an editable draft. Use only trusted server
        prompts and inspect the subsequent tool requests under the normal approval policy.
        The visible command label identifies the server and prompt; it is not a complete
        record of every argument or proof that the generated instructions are safe.</p>
        <p><code>agent-cli mcp list --json</code> writes a JSON array to stdout, with one
        object per configured server (including disabled entries). Fields are
        <code>name</code>, boolean <code>enabled</code>, <code>transport</code>,
        nullable <code>url</code>, <code>command</code>, string-array <code>args</code>,
        nullable <code>cwd</code> and string-array <code>envKeys</code>. Environment
        values are omitted. This is saved configuration, not a live connection-health probe.
        An empty catalog is <code>[]</code>; malformed configuration is an error, not
        an empty successful catalog. Check process success before parsing and select by
        <code>name</code>, not array position. URLs and command arguments may still contain
        sensitive information, so inspect output before publishing it.</p>
        <table><thead><tr><th>Command</th><th>Operation</th></tr></thead><tbody>
            <tr><td><code>agent-cli mcp list [--json]</code></td><td>List configured servers</td></tr>
            <tr><td><code>agent-cli mcp add NAME [-t stdio|http] COMMAND_OR_URL [ARGS]</code></td><td>Add a server; an HTTP(S) URL implies HTTP unless transport is explicit</td></tr>
            <tr><td><code>agent-cli mcp enable NAME</code></td><td>Enable a configured server</td></tr>
            <tr><td><code>agent-cli mcp disable NAME</code></td><td>Disable without removing its configuration</td></tr>
            <tr><td><code>agent-cli mcp login URL [--scope SCOPE]</code></td><td>Authorize with OAuth PKCE; repeat the scope option to request multiple scopes</td></tr>
            <tr><td><code>agent-cli mcp logout URL</code></td><td>Remove saved OAuth credentials for the endpoint</td></tr>
        </tbody></table>
        <p>Use <code>--</code> before a local server command when its arguments would otherwise be
        parsed as agent options. See <a href="/customization/mcp/">MCP integrations</a> for examples
        and how to refresh the runtime after changing configuration externally.</p>
        <h2 id="gateway-and-maintenance">Gateway and worktree maintenance</h2>
        <p><code>agent-cli login</code> opens provider credential management without starting a
        normal conversation. Inside the agent, <code>/login</code> (alias <code>/accounts</code>)
        opens account management. Follow <a href="/getting-started/authentication/">authentication</a>
        for account setup and credential-source details.</p>
        <p><code>agent-cli gateway connect --url HTTPS_URL</code> connects to a gateway;
        <code>gateway status</code> inspects it and <code>gateway disconnect</code> disconnects it.
        Changing gateway credentials changes the provider-routing trust boundary; do not assume
        an old conversation continues unchanged across that transition.</p>
        <pre><code class="language-sh">agent-cli worktree gc --dry-run --inactivity-days 30</code></pre>
        <p>This simulates managed-worktree adoption and collection eligibility, reporting reasons
        and estimated bytes without collecting. Review this report before deliberately running
        the same command without <code>--dry-run</code>. The inactivity argument must be a positive
        number of days. See <a href="/guides/parallel-agents/">worktrees</a> before removing checkouts.</p>
        <table><thead><tr><th>Command</th><th>Operation</th></tr></thead><tbody>
            <tr><td><code>agent-cli worktree enroll PATH</code></td><td>Explicitly enroll an existing checkout in automatic collection</td></tr>
            <tr><td><code>agent-cli worktree protect PATH</code></td><td>Protect an enrolled checkout from collection</td></tr>
            <tr><td><code>agent-cli worktree unprotect PATH</code></td><td>Permit inactivity-based collection again</td></tr>
            <tr><td><code>agent-cli worktree restore PATH</code></td><td>Restore a collected checkout without overwriting an existing path</td></tr>
        </tbody></table>
        <p>Without an explicit inactivity override, collection uses the configured policy
        (one day by default). Unmerged work does not expire merely because it is old.
        Enrollment is a deliberate change to retention behavior, not an inspection command.</p>
        <h2 id="conversation-and-work">Conversation and work</h2>
        <table>
            <thead><tr><th>Command</th><th>Purpose</th></tr></thead>
            <tbody>
                <tr><td><code>/plan [description]</code></td><td>Enter plan mode</td></tr>
                <tr><td><code>/view-plan</code></td><td>Display the saved plan</td></tr>
                <tr><td><code>/steer &lt;prompt&gt;</code></td><td>Guide the current turn</td></tr>
                <tr><td><code>/queue [prompt]</code></td><td>Queue a follow-up or list pending prompts</td></tr>
                <tr><td><code>/diff</code></td><td>Inspect Git changes, including untracked files</td></tr>
                <tr><td><code>/review [instructions]</code></td><td>Request a review</td></tr>
                <tr><td><code>/retry</code></td><td>Retry the last failed turn exactly</td></tr>
                <tr><td><code>/agents [limit [N]]</code></td><td>Browse agents or inspect/set the concurrency cap</td></tr>
                <tr><td><code>/worktree</code></td><td>Start a fresh session in a new worktree</td></tr>
                <tr><td><code>/fork [--worktree|--no-worktree] [directive]</code></td><td>Create a peer conversation</td></tr>
            </tbody>
        </table>
        <h2 id="sessions">Sessions</h2>
        <table>
            <thead><tr><th>Command</th><th>Purpose</th></tr></thead>
            <tbody>
                <tr><td><code>/resume [ID]</code></td><td>Select or resume a saved session</td></tr>
                <tr><td><code>/search &lt;query&gt;</code></td><td>Search past conversations</td></tr>
                <tr><td><code>/find [text]</code></td><td>Search the current conversation</td></tr>
                <tr><td><code>/session</code></td><td>Print the session identifier</td></tr>
                <tr><td><code>/session-info</code></td><td>Display model, tool, and context information</td></tr>
                <tr><td><code>/rename &lt;title&gt;</code></td><td>Name the session</td></tr>
                <tr><td><code>/export [path]</code></td><td>Export the conversation as Markdown</td></tr>
                <tr><td><code>/compact [focus]</code></td><td>Summarize history to free context</td></tr>
                <tr><td><code>/context</code></td><td>Inspect context usage</td></tr>
                <tr><td><code>/new</code></td><td>Create a fresh session identifier</td></tr>
                <tr><td><code>/clear</code></td><td>Reset the live conversation under the same identifier</td></tr>
                <tr><td><code>/delete</code></td><td>Delete the current session and start fresh</td></tr>
                <tr><td><code>/quit</code></td><td>Exit</td></tr>
            </tbody>
        </table>
        <h2 id="configuration-and-tools">Configuration and tools</h2>
        <table>
            <thead><tr><th>Command</th><th>Purpose</th></tr></thead>
            <tbody>
                <tr><td><code>/model [name]</code></td><td>Select a model</td></tr>
                <tr><td><code>/effort [level]</code></td><td>Inspect or set reasoning effort</td></tr>
                <tr><td><code>/title-model [name|--auto]</code></td><td>Select automatic session naming</td></tr>
                <tr><td><code>/login</code></td><td>Log in or manage provider accounts</td></tr>
                <tr><td><code>/usage</code></td><td>Inspect account usage and reset times</td></tr>
                <tr><td><code>/reload-auth</code></td><td>Reload credentials</td></tr>
                <tr><td><code>/meta &lt;request&gt;</code></td><td>Preview and apply a configuration request</td></tr>
                <tr><td><code>/permissions</code></td><td>Select tool approval policy</td></tr>
                <tr><td><code>/always-approve</code> (alias <code>/yolo</code>)</td><td>Toggle persistent project auto-approval</td></tr>
                <tr><td><code>/shell [ghci|bash|both|none]</code></td><td>Select available shell tools</td></tr>
                <tr><td><code>/computer-use [on|off]</code></td><td>Control desktop-tool availability</td></tr>
                <tr><td><code>/mcp</code></td><td>Manage MCP servers</td></tr>
                <tr><td><code>/skills [reload]</code></td><td>List or rediscover skills</td></tr>
                <tr><td><code>/theme [name]</code></td><td>Select a terminal theme</td></tr>
                <tr><td><code>/mouse [on|off]</code></td><td>Control fullscreen mouse capture</td></tr>
                <tr><td><code>/terminal</code></td><td>Inspect detected terminal capabilities</td></tr>
            </tbody>
        </table>
        <h2 id="clipboard-and-navigation">Clipboard, attachments, and navigation</h2>
        <table><thead><tr><th>Command</th><th>Operation</th></tr></thead><tbody>
            <tr><td><code>/init</code></td><td>Create an AGENTS.md contributor guide</td></tr>
            <tr><td><code>/history</code></td><td>Search prompt history and reuse a prompt</td></tr>
            <tr><td><code>/transcript</code> (alias <code>/log</code>)</td><td>Open the session transcript in a pager</td></tr>
            <tr><td><code>/edit-prompt</code></td><td>Edit a prompt draft without submitting it</td></tr>
            <tr><td><code>/recap</code></td><td>Summarize the session so far</td></tr>
            <tr><td><code>/paste [--send] [TEXT]</code></td><td>Attach a clipboard image and optional caption; send immediately only with the flag</td></tr>
            <tr><td><code>/attachments</code>, <code>/clear-attachments</code></td><td>Inspect or clear queued images</td></tr>
            <tr><td><code>/copy [N] [PATH]</code></td><td>Copy an assistant response to the clipboard or a file</td></tr>
            <tr><td><code>/copy-code [N]</code>, <code>/copy-diff</code></td><td>Copy a code block or the last diff</td></tr>
            <tr><td><code>/copy-path</code>, <code>/copy-session</code></td><td>Copy the worktree path or session identifier</td></tr>
            <tr><td><code>/rewind</code> (alias <code>/undo</code>)</td><td>Restore conversation before a chosen prompt and return that prompt to the draft; files are unchanged</td></tr>
            <tr><td><code>/home</code></td><td>Return to the session picker</td></tr>
            <tr><td><code>/desktop</code></td><td>Open the persisted conversation in the macOS application</td></tr>
            <tr><td><code>/afk [HOST:PATH]</code></td><td>Move the session into tmux locally or over SSH</td></tr>
            <tr><td><code>/btw QUESTION</code></td><td>Ask a side question without changing or persisting the main conversation</td></tr>
            <tr><td><code>/changelog</code></td><td>Read release notes</td></tr>
            <tr><td><code>/update-and-restart</code></td><td>Install the latest agent and resume the session</td></tr>
        </tbody></table>
        <h2 id="conditional-commands">Provider and capability-specific commands</h2>
        <p><code>/fast</code> applies to the Codex dialect. The Grok Build dialect exposes the
        following commands only when the corresponding tool is available:</p>
        <table><thead><tr><th>Command</th><th>Purpose</th></tr></thead><tbody>
            <tr><td><code>/loop [interval] PROMPT</code></td><td>Run a recurring prompt</td></tr>
            <tr><td><code>/goal OBJECTIVE [--budget N]</code></td><td>Set an autonomous goal; also accepts <code>status</code>, <code>pause</code>, <code>resume</code>, and <code>clear</code></td></tr>
            <tr><td><code>/workflow runs</code>, <code>/workflow NAME [INPUT]</code></td><td>List workflow runs or launch a named workflow</td></tr>
            <tr><td><code>/deep-research QUERY</code></td><td>Run bounded background research and produce a cited report</td></tr>
        </tbody></table>
        <p><code>/voice</code> starts a voice call and requires ChatGPT sign-in. This is distinct
        from dictating a text prompt. <code>/mcp prompt SERVER NAME [key=value…]</code> runs an
        MCP server prompt. <code>/codemod</code> enables code mode for the current session.
        When a command is absent, check <code>/help</code> and <code>/session-info</code>
        rather than assuming every provider exposes the same capabilities.</p>
        <h2 id="local-command-boundaries">Local command outcomes</h2>
        <p><code>/btw What does this error mean?</code> makes a single side request using
        a snapshot of the current conversation. No client tools run; an attempted tool
        call is an error, not a background investigation. Its answer does not become a
        normal persisted main-conversation turn. It still makes a provider request and
        can incur usage. Cancellation, empty answers and provider failures are reported
        for the side question without turning it into a new main task.</p>
        <p><code>/shell none</code> removes shell tools from the active tool set;
        <code>/shell both</code> enables both GHCi and Bash. The other modes are
        <code>ghci</code> and <code>bash</code>; <code>/shell</code> alone reports the
        selection. Disabling GHCi suspends that runtime. This changes tool availability,
        not the safety policy, and is not a guarantee that every previously launched
        external Bash process has been terminated.</p>
        <p>For quieter terminal output, launch <code>agent-cli --motion reduced</code>
        or <code>agent-cli --motion off</code>. Full mode animates indicators; reduced
        and off use static indicator frames and disable native progress animation.
        Reduced mode retains a faster refresh cadence than off. Neither disables
        task execution or hides task-state updates.</p>
        <p><code>--prompt-file</code> reads a text file using the process's text encoding
        and strips leading/trailing whitespace, just as a literal prompt is trimmed.
        Keep prompts in UTF-8 and use a UTF-8 locale. A missing, unreadable or undecodable
        file is an input error, not a fallback to an empty prompt. It cannot be combined
        with <code>-p</code>/<code>--prompt</code> or <code>--managed-turn-file</code>.</p>
        <p>The automatic interface is fullscreen only when both stdin and stdout are
        terminals, the launch is not one-shot, and <code>--minimal</code> is absent.
        <code>--fullscreen</code> does not override the one-shot or terminal requirements.
        Use <code>--minimal</code> when integrating with terminal scrollback or a recorder.</p>
        <p><code>/fast</code> toggles the Codex request service tier between
        <code>priority</code> and the normal default. It requires both the appropriate
        command catalog and matching active-model metadata advertising that tier.
        Otherwise it reports that fast mode is unavailable. Priority service can have
        different provider pricing or usage treatment; it is not a free local speed
        setting. Toggle again to return to the default tier.</p>
        <p><code>/codemod</code> (alias <code>/code-mode</code>) saves/resumes the current
        persisted session through a runtime restart with code mode enabled. It is an
        enable command, not an on/off toggle or a global preference. Use
        <code>--no-code-mode</code> on a later launch to explicitly disable code mode.
        The restart keeps the session identity; availability still depends on the
        selected provider's supported tools.</p>
        <p>Invalid arguments and commands unavailable in the active provider's command
        catalog are rejected with a command error; they are not silently submitted as a
        coding prompt. Use <code>/help NAME</code> and correct the syntax before retrying.</p>
        <p><code>/view-plan</code> (also <code>/show-plan</code> and <code>/plan-view</code>)
        displays the saved Markdown plan for the current session. If no nonempty plan
        exists it reports <code>No saved plan is available for this session.</code>
        Viewing a plan does not create or approve one.</p>
        <p><code>/recap</code> generates a catch-up summary, whereas <code>/compact</code>
        changes the working context to reduce its size. Requesting a recap is not a
        substitute for compaction and should not be expected to lower context usage.</p>
        <p><code>/retry</code> retries the retained failed turn rather than your current
        draft; with none retained it reports <code>No failed turn is available to retry.</code>
        Before retrying after an uncertain tool result, inspect the actual file or remote
        resource. A failed model turn does not mean earlier external effects were undone.</p>
        <h2 id="initialize-and-update">Initialize a guide and update the CLI</h2>
        <p><code>/init</code> checks for <code>AGENTS.md</code> in the current directory.
        An existing entry is left unchanged. Otherwise it asks the model to generate a
        concise “Repository Guidelines” document covering repository structure, build/test
        commands, style, tests and contribution conventions. This is generated advice,
        not a fixed template or proof the commands work. Review the resulting diff and
        verify project commands before committing it.</p>
        <p><code>/update-and-restart</code> requires a persisted session. The current
        updater specifically removes the <code>haskell-agent</code> Nix profile entry,
        adds <code>github:digitallyinduced/haskell-agent</code> with flake configuration
        accepted, and executes <code>agent-cli --resume SESSION_ID</code> from PATH.
        It is not a generic updater for arbitrary package-manager or source installations.
        Note your session ID first and ensure Nix and the intended CLI are on PATH.</p>
        <p>If updating throws an error, the running runtime reports
        <code>update failed</code> and attempts to resume with its existing implementation.
        Profile removal and installation are not atomic: a failed install can leave the
        profile entry absent. Repair installation before exiting that process. The
        newly executed command receives the resume ID, not every original transient
        launch flag; reapply required explicit overrides when launching manually.</p>
        <h2 id="command-aliases">Command aliases</h2>
        <p>Aliases use the same arguments and availability rules as their canonical
        command; they do not enable otherwise unavailable capabilities.</p>
        <table><thead><tr><th>Alias</th><th>Canonical command</th></tr></thead><tbody>
            <tr><td><code>/m</code></td><td><code>/model</code></td></tr>
            <tr><td><code>/t</code></td><td><code>/theme</code></td></tr>
            <tr><td><code>/show-plan</code>, <code>/plan-view</code></td><td><code>/view-plan</code></td></tr>
            <tr><td><code>/log</code></td><td><code>/transcript</code></td></tr>
            <tr><td><code>/configure</code></td><td><code>/meta</code></td></tr>
            <tr><td><code>/summarize</code></td><td><code>/recap</code></td></tr>
            <tr><td><code>/status</code>, <code>/info</code></td><td><code>/session-info</code></td></tr>
            <tr><td><code>/title</code></td><td><code>/rename</code></td></tr>
            <tr><td><code>/accounts</code></td><td><code>/login</code></td></tr>
            <tr><td><code>/welcome</code></td><td><code>/home</code></td></tr>
            <tr><td><code>/undo</code></td><td><code>/rewind</code></td></tr>
            <tr><td><code>/copy-last</code></td><td><code>/copy</code></td></tr>
            <tr><td><code>/ghostty</code></td><td><code>/terminal</code></td></tr>
            <tr><td><code>/a</code></td><td><code>/agents</code></td></tr>
            <tr><td><code>/mcps</code></td><td><code>/mcp</code></td></tr>
            <tr><td><code>/code-mode</code></td><td><code>/codemod</code></td></tr>
            <tr><td><code>/yolo</code></td><td><code>/always-approve</code></td></tr>
            <tr><td><code>/exit</code></td><td><code>/quit</code></td></tr>
        </tbody></table>
        <h2 id="recurring-work">Recurring prompts are detached work</h2>
        <pre><code>/loop 5m In /absolute/path/to/project, inspect the status of PR 123. Report a short status, make no changes, and stop this check.</code></pre>
        <p>This example requires the scheduler capability. Replace both the project and PR.
        The agent creates a schedule; each fire runs in a detached background subagent,
        not with the whole parent conversation. Include paths, identifiers, status commands,
        success conditions, and stop conditions in the stored prompt. A single fire should
        finish, not poll indefinitely.</p>
        <p>Intervals use <code>s</code>, <code>m</code>, <code>h</code>, or <code>d</code>,
        with a minimum of 60 seconds. Without a cadence the agent should ask rather than
        assume one. Creation requests an immediate first fire. Check the confirmation for
        cadence, stop condition, seven-day expiry, and <code>task_id</code>.
        The parent/user owns cancellation: ask the parent to cancel that task by its
        identifier through the scheduler. A detached child cannot modify its schedule.
        Merely exiting a child or receiving one successful check is not cancellation.</p>
        <h2 id="computer-control">Enable computer control for a session</h2>
        <pre><code>{"/computer-use\n/computer-use on\n/computer-use off" :: Text}</code></pre>
        <p>The argument-free command reports the current setting. Before enabling it, confirm
        that the active provider and platform support computer use. An unavailable combination
        reports that limitation rather than making control available. Enabling changes the
        session's tool availability; it does not grant consent to control the desktop.</p>
        <p>Changing this setting clears the remembered computer-tool approval. After enabling,
        expect a fresh approval before control. Disable it when finished, and check the reported
        state. Turning it off is not an undo operation for clicks, typed text, files or messages
        produced earlier. See <a href="/security/approvals/">approvals and sandboxing</a>
        before using a desktop with sensitive windows or accounts open.</p>
        <h2 id="goals-and-workflows">Goals, workflows, and research</h2>
        <pre><code>{"/goal Investigate the failing parser test and fix only its cause.\n/goal status\n/goal pause\n/goal resume\n/goal clear" :: Text}</code></pre>
        <p>A goal tracks an objective and progress; it is not permission to ignore approval
        rules. Pause before changing direction, inspect status, and clear an obsolete goal.
        The optional positive integer <code>--budget N</code> is an advisory token budget,
        not an enforced spending cap or the CLI's model-turn limit. Goal state is held in
        memory, so check status rather than assuming a restart restores it. Request evidence
        for completion rather than treating a status label as a successful test.</p>
        <p>Omitting the budget leaves it unset. Setting a new objective replaces the current
        goal and starts a fresh progress list. Pause accepts an active goal; resume accepts
        a user-paused or blocked goal, not a completed one. A missing goal or incompatible
        state produces an error without inventing a new objective. If blocked, resolve the
        reported cause before resuming. Completion ends goal mode; clear removes the
        objective, not work already performed. See
        <a href="/guides/scheduled-work/#goals">goal lifecycle and tool updates</a>.</p>
        <pre><code>{"/workflow runs\n/workflow deep-research Compare the documented migration paths for our database.\n/deep-research Compare the documented migration paths for our database." :: Text}</code></pre>
        <p><code>/workflow NAME INPUT</code> launches the named workflow with the input as
        its query; <code>/deep-research</code> is the deep-research workflow shortcut.
        Look for a real workflow-tool result and inspect <code>/workflow runs</code>.
        A prose answer alone is not evidence that a background workflow launched.
        If a workflow name or option is rejected, inspect the error rather than assuming
        the agent silently ran an equivalent procedure. Read the generated report and
        verify its citations before relying on it.</p>
        <h2 id="workflow-lifecycle">Workflow results and lifecycle</h2>
        <p>The supported named workflow is <code>deep-research</code>, with a nonempty
        research query. Slash-command text becomes the query, not a set of shell flags.
        See <a href="/guides/scheduled-work/#named-workflows">the workflow tool contract</a>
        for structured input and validation-only calls; arbitrary scripts, agent budgets,
        and resume-from-run options are not supported.</p>
        <p>A successful launch returns immediately with a tracked run and child agent.
        Completion is delivered through the subagent mechanism. The research report is
        the child agent's result, not a guaranteed file at a fixed path. Ask to save a
        reviewed report explicitly if you need a repository artifact. The research
        instruction requests primary sources, cross-checking, uncertainty and a concise
        cited report; it does not guarantee that every claim is correct.</p>
        <p><code>/workflow runs</code> lists the display name, <code>wf_N</code> run ID,
        objective, start time and child-derived status: pending, active, complete,
        failed, interrupted, closed or unknown. An empty list means no tracked runs
        in this runtime. Unknown means the child cannot be found, not that research
        completed. This in-memory run index is not a durable workflow scheduler or
        a resumable job queue across application restarts.</p>
        <p>Workflow management operations such as cancel or resume return
        <code>workflow_management_unsupported</code>; they must not be treated as
        successful cancellation. To stop a running research child, request the ordinary
        agent interruption operation for that child and verify its resulting status.
        Interrupting a foreground response is not proof that a background child stopped.
        Neither interruption nor closing a child reverses tool effects already performed.
        Inspect a failed run's child output before launching another potentially duplicate
        investigation.</p>
    |]
    }
