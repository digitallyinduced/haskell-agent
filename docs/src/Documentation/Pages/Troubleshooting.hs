{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.Troubleshooting (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/troubleshooting/"
    , pageTitle = "Troubleshooting"
    , pageDescription = "Diagnose installation, authentication, tool, worktree, and terminal problems."
    , pageGroup = "Reference"
    , pageBody = [hsx|
        <p>Start with <code>agent-cli --version</code>, then record the exact operation and error.
        Inside a session, <code>/session-info</code> helps identify the active model and tools.
        Do not include credentials or private source code in a public bug report.</p>
        <h2 id="diagnostic-sequence">Collect a baseline before changing settings</h2>
        <pre><code class="language-sh">{"command -v agent-cli\nagent-cli --version\nagent-cli storage status\nagent-cli storage doctor\nagent-cli mcp list" :: Text}</code></pre>
        <p>The first two commands distinguish an old executable earlier on <code>PATH</code>
        from a failed upgrade. Storage diagnostics separate local startup failures from remote
        model failures. The MCP list reports saved configuration, not proof that every server
        is currently connected.</p>
        <p>In the affected conversation, record <code>/session</code>,
        <code>/session-info</code>, and <code>/terminal</code>. Reproduce with one small request
        in a fresh session before clearing or deleting the failing session. Retain the original
        session for investigation; deletion is not a diagnostic procedure.</p>
        <h2 id="specific-error-messages">Recognize local error messages</h2>
        <table><thead><tr><th>Message or prefix</th><th>Meaning and next action</th></tr></thead><tbody>
            <tr><td><code>session not found:</code></td><td>The requested session directory is unavailable. Copy an exact identifier from <code>agent-cli sessions list</code>; check that you are using the same user account and storage environment.</td></tr>
            <tr><td><code>invalid transferred session:</code></td><td>Session import could not decode stdin. Supply the harness's transfer format, not Markdown from <code>/export</code> or arbitrary transcript JSON. Preserve the decoder detail after the prefix.</td></tr>
            <tr><td><code>MCP server NAME already exists</code></td><td>Add would duplicate a catalog entry. Inspect <code>agent-cli mcp list</code>; enable the existing entry or deliberately choose another name rather than repeatedly adding it.</td></tr>
            <tr><td><code>TRANSPORT must be stdio or http</code></td><td>The MCP transport option is invalid. Use <code>--transport stdio</code> for a local process or <code>--transport http</code> for a remote endpoint.</td></tr>
            <tr><td><code>--max-turns expects a positive integer</code></td><td>The option is a count, not a duration. Supply a positive whole number such as <code>6</code>, not <code>0</code> or <code>30s</code>.</td></tr>
            <tr><td><code>limit must be a positive integer</code></td><td>A file-read request supplied an invalid line limit. Ask the agent to retry with a positive limit and a valid offset, not wider filesystem access.</td></tr>
        </tbody></table>
        <h2 id="storage-and-log-locations">Storage and log locations</h2>
        <table><thead><tr><th>Location</th><th>Purpose</th></tr></thead><tbody>
            <tr><td><code>~/.haskell-agent/config.json</code></td><td>Runtime configuration, including MCP connections; may contain secrets</td></tr>
            <tr><td><code>~/.haskell-agent/models.json</code></td><td>Custom model and connection catalog</td></tr>
            <tr><td><code>&lt;project&gt;/.haskell-agent/settings.json</code></td><td>Saved project preferences, including auto-approval</td></tr>
            <tr><td><code>~/.haskell-agent/postgres/postgres.log</code></td><td>Managed PostgreSQL startup and server diagnostics</td></tr>
            <tr><td><code>~/.haskell-agent/postgres/data</code></td><td>Database files; do not remove to resolve a login or rendering issue</td></tr>
            <tr><td><code>&lt;session-directory&gt;/agent.log</code></td><td>Background-agent output when a background agent was started</td></tr>
        </tbody></table>
        <p>Use storage diagnostics to identify the actual state location in the current
        environment. Do not assume every execution mode writes one universal application log.
        An interactive <code>/export</code> or <code>agent-cli sessions show SESSION_ID --json</code>
        provides conversation evidence, but can include private tool output.</p>
        <h2 id="the-installed-version-did-not-change">The installed version did not change</h2>
        <p><code>nix profile add</code> does not upgrade an existing installation. Use:</p>
        <pre><code class="language-sh">{"nix profile list\nnix profile upgrade --refresh --accept-flake-config haskell-agent" :: Text}</code></pre>
        <p>Use the actual profile entry name if it differs. Check which executable your
        shell selects if you have more than one installation.</p>
        <h2 id="local-storage-fails-to-start">Local storage fails to start</h2>
        <p>Before opening sessions, run:</p>
        <pre><code class="language-sh">{"agent-cli storage start\nagent-cli storage doctor" :: Text}</code></pre>
        <p>Preserve the diagnostic output. Do not delete storage directories as a first
        response: they can contain saved conversations and durable state.</p>
        <p>The managed cluster uses port 55432 by default and a private Unix-socket directory.
        <code>AGENT_POSTGRES_PORT</code> overrides the port and <code>AGENT_POSTGRES_BIN</code>
        overrides the directory containing PostgreSQL executables. A custom executable
        directory must contain a compatible PostgreSQL installation; mixing server versions
        with an existing data directory is not a repair strategy.</p>
        <ol>
            <li>If startup cannot locate PostgreSQL tools, check whether you are running the
            Nix-packaged executable and whether <code>AGENT_POSTGRES_BIN</code> was overridden.</li>
            <li>If connection or socket errors persist, compare <code>storage status</code>
            with the PostgreSQL log. Check that different shells are using the same storage
            environment and port.</li>
            <li>For schema errors, preserve a backup before running
            <code>agent-cli storage migrate</code>, then repeat <code>storage doctor</code>.
            Doctor checks connectivity, not an exhaustive schema comparison.</li>
            <li>Before any stop/restart, finish other sessions that depend on this database.
            Preserve the original error and log excerpt if recovery fails.</li>
        </ol>
        <h2 id="storage-diagnostic-output">Interpret storage diagnostics</h2>
        <p>The following are exact success/status messages, not a transcript from your machine:</p>
        <table><thead><tr><th>Command/output</th><th>Interpretation and action</th></tr></thead><tbody>
            <tr><td><code>storage status</code>: <code>managed PostgreSQL is not initialized</code></td><td>No initialized managed cluster. Verify the intended state directory and user before using <code>storage start</code>.</td></tr>
            <tr><td><code>storage status</code>: <code>managed PostgreSQL is stopped</code></td><td>Cluster exists but is not running. Inspect the log, then start it after checking the executable version.</td></tr>
            <tr><td><code>storage status</code>: <code>managed PostgreSQL is running</code></td><td>The managed process is running; use doctor to test a database connection.</td></tr>
            <tr><td><code>storage start</code>: <code>managed PostgreSQL is running and migrations are up to date</code></td><td>Start also initializes storage and applies migrations when needed. It is not a read-only health check.</td></tr>
            <tr><td><code>storage doctor</code>: <code>managed PostgreSQL is healthy; socket: </code> followed by a directory</td><td>Status and opening the connection pool succeeded. This does not prove backup integrity, model connectivity or every stored record's correctness.</td></tr>
            <tr><td>Doctor reports not initialized/stopped</td><td>The error explicitly recommends <code>agent-cli storage start</code>. Follow the backup precaution below for existing data.</td></tr>
            <tr><td>Process running but doctor fails</td><td>Compare socket directory, port, user permissions and database errors. Do not create a second cluster or delete the original to make the check green.</td></tr>
        </tbody></table>
        <p>The managed configuration disables TCP listening. Its port selects a Unix-socket
        filename, so an unrelated TCP listener on 55432 alone is not a conflict. For socket/lock
        conflicts inspect the actual server owner and log before stopping anything; never remove
        a live server's socket or PID file. The packaged PostgreSQL tools, a writable state
        directory, sufficient disk space and a server version compatible with the data directory
        are prerequisites. Startup errors identify commands such as <code>initdb</code> or
        <code>pg_ctl start</code>; preserve their stderr and <code>postgres.log</code>.</p>
        <h2 id="storage-migration-recovery">Back up before schema maintenance</h2>
        <ol>
            <li>Finish dependent sessions and stop application writers. Record the CLI and
            PostgreSQL versions, actual state/socket directory and port. Keep the prior executable.</li>
            <li>For a running database, create a logical backup using compatible PostgreSQL tools.
            Store it in a private persistent backup directory, not a conversation attachment.</li>
            <li>Verify the dump is readable and rehearse restoration into an isolated cluster before
            relying on it. If storage cannot start, retain a stopped-cluster snapshot and seek
            version-specific recovery; do not copy a running data directory as an ordinary backup.</li>
            <li>Run <code>agent-cli storage migrate</code>. Success is
            <code>managed PostgreSQL migrations are up to date</code>. This command also starts
            storage if necessary; there is no documented downgrade command.</li>
            <li>On failure, keep writers stopped, preserve the complete error and log, and correct
            the diagnosed prerequisite before retrying. Do not alter migration records manually.
            Restore only into an isolated destination first; verify it with the matching executable
            before deliberately replacing production state.</li>
        </ol>
        <p>Example for the default running cluster; replace paths and port with your actual values.
        Create and secure <code>BACKUP_DIR</code> first. These commands are a procedure, not an
        executed backup record:</p>
        <pre><code class="language-sh">{"pg_dump -h \"$HOME/.haskell-agent/postgres/run\" -p 55432 -U ha_owner -d haskell_agent -Fc -f \"$BACKUP_DIR/haskell-agent.dump\"\npg_restore --list \"$BACKUP_DIR/haskell-agent.dump\"\nagent-cli storage migrate\nagent-cli storage doctor" :: Text}</code></pre>
        <p>Listing a dump verifies its catalog can be read, not that a complete restore succeeds.
        A logical database dump does not include all cluster roles, configuration or non-database
        session files. Preserve those separately and use the
        <a href="/guides/deployment/#restore-checklist">restore checklist</a>.</p>
        <h2 id="authentication-fails-or-the-wrong-account-is-used">Authentication fails or the wrong account is used</h2>
        <ol>
            <li>Inspect <code>/session-info</code> and <code>/usage</code>.</li>
            <li>Open <code>/login</code> to check the provider connection.</li>
            <li>Use <code>/reload-auth</code> after changing credentials outside the agent.</li>
            <li>Select the intended model again with <code>/model</code>.</li>
        </ol>
        <p>For Claude Code, verify <code>claude auth login</code> completed. For a custom endpoint,
        check the named environment variable and the configured base URL. Never paste
        an API key into the conversation to diagnose it.</p>
        <h2 id="provider-error-categories">Distinguish provider error categories</h2>
        <table><thead><tr><th>Symptom</th><th>Check</th><th>Recovery</th></tr></thead><tbody>
            <tr><td>HTTP 401 or expired credentials</td><td>Selected connection and credential source</td><td>Re-authenticate that provider; reload supported credentials and retry one small request</td></tr>
            <tr><td>HTTP 403 or access denied</td><td>Account entitlement and organization policy</td><td>Use an authorized model/account; extra shell permissions do not grant provider access</td></tr>
            <tr><td>HTTP 404 or model not found</td><td>Exact model identifier, connection base URL, and API protocol</td><td>Correct the catalog or endpoint; verify with the provider's model listing</td></tr>
            <tr><td>HTTP 429</td><td>Usage limits, reset time, and API credit</td><td>Wait for reset or deliberately change accounts/models; do not repeatedly submit the same request</td></tr>
            <tr><td>Connection refused, DNS, or TLS failure</td><td>Server process, host/port, VPN/proxy, and certificate trust</td><td>Restore endpoint connectivity; do not disable certificate verification as a default fix</td></tr>
            <tr><td>Context capacity or compaction error</td><td>Model's configured context window and conversation size</td><td>Correct the catalog limit, compact where supported, or begin a smaller fresh task</td></tr>
        </tbody></table>
        <p>The exact message is provider-dependent. For custom models, a reachable HTTP endpoint
        can still implement the wrong protocol. A chat-only endpoint is not automatically a
        Responses API endpoint. Check <a href="/customization/models/">custom model configuration</a>
        before changing authentication.</p>
        <h2 id="the-agent-is-waiting-after-a-usage-limit">The agent is waiting after a usage limit</h2>
        <p>Interactive sessions can wait for an account's reset time when another
        configured account cannot take over. <code>/usage</code> displays available usage
        information. Press <code>Esc</code> to cancel the wait, or deliberately select another
        available model. Subscription and API-credit billing are different.</p>
        <h2 id="a-tool-is-missing-or-denied">A tool is missing or denied</h2>
        <p>Check whether the active model supports the tool and whether it is enabled.
        <code>/shell</code> reports shell selection, <code>/computer-use</code> controls desktop capability,
        and <code>/mcp</code> displays external server status.</p>
        <p>A denial can be an approval-policy decision, a plan-mode restriction, an
        operating-system permission, or a sandbox restriction. Read the error rather
        than enabling unrestricted execution indiscriminately.</p>
        <p>If the optional GHCi tool cannot find <code>ghci</code>, start with a Nix-provided GHC:</p>
        <pre><code class="language-sh">nix shell nixpkgs#ghc -c agent-cli --ghci</code></pre>
        <h2 id="an-mcp-server-is-unavailable">An MCP server is unavailable</h2>
        <p>Use <code>/mcp</code> to inspect the server. Verify the command exists or the HTTP endpoint
        is reachable. Use <code>i</code> to re-authorize HTTP and <code>r</code> to restart connections.
        Interactive startup is progressive, so a server may still be connecting when
        the prompt first appears.</p>
        <p>If a mutation might already have succeeded remotely, inspect its outcome before
        retrying it.</p>
        <ol>
            <li><strong>Local command not found:</strong> run the configured executable's version
            or help command in the same environment. Prefer a Nix-provided command rather than
            relying on a GUI application's different <code>PATH</code>.</li>
            <li><strong>Local process starts but initialization fails:</strong> confirm it speaks
            MCP over stdio, not an HTTP service; stdout must be reserved for protocol messages.
            Put diagnostic logging on stderr.</li>
            <li><strong>Remote authorization repeats:</strong> verify the exact endpoint URL and
            requested OAuth scopes. Re-authorize with <code>agent-cli mcp login URL</code>
            or the manager's <code>i</code> action.</li>
            <li><strong>Connected but tool absent:</strong> inspect that server's tool list and
            enabled state. Some models discover tools on demand; ask for the specific operation.</li>
            <li><strong>Configuration changed but old behavior remains:</strong> restart the MCP
            runtime with <code>/mcp</code>, then <code>r</code>, and inspect its new status.</li>
        </ol>
        <h2 id="worktree-creation-fails">Worktree creation fails</h2>
        <p>Managed worktrees fetch the selected remote by default. Confirm Git access,
        remote configuration, and network connectivity. A fetch failure intentionally
        stops creation rather than using stale state.</p>
        <p>If you intend to work without fetching, configure
        <code>worktree.fetchLatestUpstream</code> as described in
        <a href="/guides/parallel-agents/">worktrees</a>. Do not assume a new worktree includes
        uncommitted files from your current checkout.</p>
        <pre><code class="language-sh">{"git status --short\ngit remote -v\ngit branch --show-current\ngit worktree list" :: Text}</code></pre>
        <p>These commands show local changes, configured remotes, the current branch, and registered
        worktrees without deleting anything. Redact embedded credentials from remote URLs before
        sharing output. A clean original checkout and a managed worktree are different directories;
        use <code>/copy-path</code> to verify which one the agent is editing.</p>
        <h2 id="text-selection-or-terminal-rendering-is-unexpected">Text selection or terminal rendering is unexpected</h2>
        <p>Use <code>/mouse off</code> for native text selection. Run <code>/terminal</code> to inspect terminal
        capabilities. If fullscreen rendering is unsuitable, try:</p>
        <pre><code class="language-sh">agent-cli --minimal</code></pre>
        <p>When reporting a terminal issue, include the terminal application, operating
        system, selected rendering mode, and a redacted screenshot.</p>
        <p>If paste produces an image attachment rather than text, inspect <code>/attachments</code>
        and clear unwanted images with <code>/clear-attachments</code> before submitting.
        If a shortcut is intercepted by the terminal, use the equivalent slash command and
        consult <a href="/reference/keybindings/">keybindings</a>. Clipboard image support on Linux
        depends on the relevant Wayland/X11 clipboard utilities.</p>
        <h2 id="skills-not-discovered">A skill is not discovered</h2>
        <ol>
            <li>Confirm the filename is <code>SKILL.md</code> inside the skill's directory, not
            a loose Markdown file elsewhere.</li>
            <li>Check the name and description front matter against the
            <a href="/customization/skills/">skill reference</a>, including directory/name agreement.</li>
            <li>Run <code>/skills reload</code>, then <code>/skills</code>, from the intended project.</li>
            <li>Check that the session was not started with <code>--no-skills</code>. For duplicate
            names, inspect discovery precedence rather than assuming the last edited file wins.</li>
        </ol>
        <h2 id="compaction-refuses-a-custom-model">Compaction refuses a custom model</h2>
        <p>Set <code>context_window</code> in the <a href="/customization/models/">model catalog</a> to the
        server's documented limit. The harness refuses to guess a portable model's
        context capacity.</p>
        <h2 id="prepare-a-useful-report">Prepare a useful report</h2>
        <p>Reduce the issue to one reproducible operation and include:</p>
        <ul>
            <li>The CLI version, operating system, and terminal application.</li>
            <li>The exact command or sequence of actions, with secrets removed.</li>
            <li>What you expected, what happened, and the complete relevant error.</li>
            <li>The selected provider and rendering mode, and whether the issue occurs in a fresh session.</li>
        </ul>
        <p>For example: “In fullscreen mode, after <code>/mouse off</code>, native selection
        still does not work in this terminal; minimal mode works.” This is more
        actionable than “the terminal is broken.” Review screenshots and transcripts
        for credentials, personal data, and private code before sharing them.</p>
    |]
    }
