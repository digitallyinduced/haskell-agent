{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.Configuration (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/reference/configuration/"
    , pageTitle = "Configuration reference"
    , pageDescription = "Find machine configuration, project instructions, model connections, and explicit launch overrides."
    , pageGroup = "Reference"
    , pageBody = [hsx|
        <h2 id="choose-the-right-place">Choose the right place</h2>
        <table>
            <thead><tr><th>Location</th><th>Purpose</th></tr></thead>
            <tbody>
                <tr><td><code>~/.haskell-agent/config.json</code></td><td>Machine-wide harness configuration: theme, MCP, web fetching, LSP, worktrees, and agent concurrency</td></tr>
                <tr><td><code>~/.haskell-agent/models.json</code></td><td>Additional model definitions and endpoint connections</td></tr>
                <tr><td><code>&lt;checkout&gt;/.haskell-agent/settings.json</code></td><td>Remembered checkout choices and approval policy; see <a href="/reference/persisted-settings/">persisted settings</a></td></tr>
                <tr><td><code>~/.haskell-agent/settings.json</code></td><td>User model, title-model and terminal preferences</td></tr>
                <tr><td>Process environment</td><td>Credentials and explicit transport/helper overrides; see <a href="/reference/environment/">environment variables</a></td></tr>
                <tr><td><code>AGENTS.md</code></td><td>Repository instructions for the model, not a JSON settings file</td></tr>
                <tr><td>Launch arguments</td><td>Explicit choices for this invocation, such as model, working directory, and interface</td></tr>
            </tbody>
        </table>
        <p><code>version</code> must be the integer <code>1</code>. Boolean settings take JSON
        <code>true</code> or <code>false</code>, not quoted strings. Both <code>inactivityDays</code>
        and an explicitly configured <code>maxConcurrentAgents</code> must be integers of at least one.
        Omitting the concurrency limit leaves it unspecified at machine scope; it does not override
        a project or launch setting.</p>
        <h2 id="configuration-precedence">Configuration boundaries and precedence</h2>
        <p>Do not treat every configuration source as one merged JSON tree.
        <code>config.json</code> configures the harness, <code>models.json</code> defines model connections,
        and <code>AGENTS.md</code> supplies model instructions. Putting a JSON option in
        <code>AGENTS.md</code> does not configure the runtime.</p>
        <p>For concurrent agents, the explicit CLI limit wins over the project limit, which wins over
        the machine limit. For model selection and connection definitions, use the
        <a href="/customization/models/">model guide</a>. For instruction discovery, use
        <a href="/guides/projects/">project instructions</a>.</p>
        <h2 id="web-fetch-settings">Web-fetch settings</h2>
        <p>These settings control the harness's client-side web-fetch capability. They are not
        a general firewall for shell commands, MCP servers, or provider-hosted browsing.</p>
        <table>
            <thead><tr><th>Field under webFetch</th><th>Type / default</th><th>Constraints</th></tr></thead>
            <tbody>
                <tr><td><code>enabled</code></td><td>Boolean / false</td><td>Must be enabled before requests can run</td></tr>
                <tr><td><code>allowedDomains</code></td><td>String array / empty</td><td>An empty list denies all requests; entries must not be blank</td></tr>
                <tr><td><code>timeoutSeconds</code></td><td>Integer / 60</td><td>1–300 seconds</td></tr>
                <tr><td><code>maxContentBytes</code></td><td>Integer / 10485760</td><td>1–52428800 bytes; maximum fetched content</td></tr>
                <tr><td><code>maxInlineBytes</code></td><td>Integer / 100000</td><td>1–1048576 bytes and no greater than maxContentBytes; inline result limit</td></tr>
            </tbody>
        </table>
        <p>Merge this section into your existing configuration to permit a specific documentation host:</p>
        <pre><code class="language-json" data-config-schema="harness">{webFetchExample}</code></pre>
        <p>Restart the session after a manual edit and request an actual fetch of a page at
        <code>https://haskell.org/</code>. Inspect the tool result rather than accepting a response
        generated from model knowledge. A different host is not authorized merely because it belongs
        to the same organization. Keep the allowed host list narrow.</p>
        <h2 id="language-server-settings">Language-server settings</h2>
        <p><code>lsp.enabled</code> defaults to false and <code>lsp.servers</code> defaults to an empty
        object. Each key under <code>servers</code> names one local language-server process.
        Install the executable through your project's Nix development environment before enabling it.</p>
        <table>
            <thead><tr><th>Server field</th><th>Type / default</th><th>Purpose</th></tr></thead>
            <tbody>
                <tr><td><code>command</code></td><td>Required string</td><td>Executable to launch; must not be blank</td></tr>
                <tr><td><code>args</code></td><td>String array / empty</td><td>Separate arguments; not a shell command string</td></tr>
                <tr><td><code>env</code></td><td>String-to-string object / empty</td><td>Environment overrides; values are redacted from diagnostics</td></tr>
                <tr><td><code>extensionToLanguage</code></td><td>Required nonempty string-to-string object</td><td>Map filename extensions to language identifiers; neither keys nor values may be blank</td></tr>
                <tr><td><code>initializationOptions</code></td><td>Optional JSON</td><td>Server-specific initialization payload</td></tr>
                <tr><td><code>settings</code></td><td>Optional JSON</td><td>Server-specific workspace settings</td></tr>
                <tr><td><code>workspaceFolder</code></td><td>Optional string</td><td>Explicit workspace folder</td></tr>
                <tr><td><code>startupTimeoutMilliseconds</code></td><td>Integer / 15000</td><td>Startup deadline; 1–120000 milliseconds</td></tr>
                <tr><td><code>shutdownTimeoutMilliseconds</code></td><td>Integer / 5000</td><td>Shutdown deadline; 1–120000 milliseconds</td></tr>
            </tbody>
        </table>
        <p>Only <code>stdio</code> transport is supported. Do not copy configurations that require
        <code>restartOnCrash: true</code> or <code>maxRestarts</code>: the loader rejects them.
        Server settings depend on the language server, not the agent.</p>
        <h3 id="lsp-workspace-folder">Language server workspace folder</h3>
        <p>Omitted <code>workspaceFolder</code> uses the active workspace. A relative
        value such as <code>packages/api</code> is resolved beneath that workspace,
        not beneath <code>~/.haskell-agent</code>. The folder must already exist.
        Both paths are canonicalized, and a folder outside the active workspace
        is rejected, including a symlink that escapes it.</p>
        <p>Each configured server receives one resolved root URI and a one-element
        workspace-folder list. This field is a string, not a multi-root array.
        Configure separate named server entries when different contained subdirectories
        need separate roots. An <code>invalid workspaceFolder</code> error means check
        existence/path permissions; an “inside the active workspace” error means
        choose a contained directory rather than relaxing unrelated tool approvals.</p>
        <h2 id="mcp-settings">MCP settings</h2>
        <p><code>mcpServers</code> maps names to local commands or remote URLs. Each entry must configure
        exactly one of those transports. The <a href="/customization/mcp/#server-field-reference">MCP field reference</a>
        covers timeouts, OAuth, environment values, protocol selection, and optional capabilities.</p>
        <p>For guided changes, use <code>/meta</code>. The Meta Console validates and previews supported
        configuration changes before applying them. Preserve unrelated settings when editing files manually.</p>
        <h2 id="mcp-lifecycle">MCP identity, credentials and lifecycle</h2>
        <p>Remote <code>connectionId</code> values must be nonempty, at most 128
        characters and contain only lowercase ASCII letters, digits and hyphens.
        They identify connections, not display labels. Preserve generated identities;
        do not copy an identity from another connection to reuse its credentials.</p>
        <p>If <code>connectionCredentials</code> is absent, it defaults to whether
        <code>connectionId</code> is present. Explicit <code>false</code> retains
        CLI-configured credentials; <code>true</code> selects managed connection
        credentials. Automatic migration of a legacy remote entry generates an
        identity and generation, sets this flag to <code>false</code> and fills a
        missing display name from its catalog label. Migration therefore does not
        silently move legacy authorization into the protected store.</p>
        <p><code>connectionGeneration</code> distinguishes lifecycle revisions.
        Managed authorization and enable/disable operations replace it; callbacks
        for an older generation must not restore a replaced or disabled connection.
        Use the management interface to authorize, disable or remove connections,
        rather than manually restoring an old generation. A failed migration
        requires repairing private configuration write access and retrying, not
        deleting all credentials. Refresh a stale management preview before saving.</p>
        <p>For local servers, omitted <code>cwd</code> uses the workspace passed to
        MCP startup. An explicit value is passed to the process launcher unchanged;
        a relative value is relative to the agent process working directory, not
        the directory containing <code>config.json</code>. Prefer an absolute path
        when a server must start in a fixed directory.</p>
        <h2 id="mcp-sampling">MCP sampling limits</h2>
        <p>Enabling <code>sampling</code> allows a server to request an isolated
        one-shot completion using the active model/backend. Requests need at least
        one message and support only <code>user</code>/<code>assistant</code> roles
        with text strings, text-content objects or arrays of text content.
        Images and other content types are rejected. The system prompt and optional
        temperature are forwarded; the requested maximum output token count is
        clamped to at least one and remains subject to provider limits.</p>
        <p>The main conversation is not shared: previous response state is cleared,
        tools and parallel tool calls are disabled, and provider storage is
        requested off where supported. A generated tool call, empty response or
        provider error rejects the request. Context/model preference hints do not
        switch the active model or import workspace context. Usage belongs to the
        active model's configured credential/billing route, not a free allowance
        supplied by the MCP server. Leave sampling disabled for untrusted servers.</p>
        <h2 id="machine-settings">Machine settings</h2>
        <p>The file is JSON, with quoted keys and no comments. Missing fields use the defaults below.
        A missing or blank file uses the default configuration. A malformed or semantically invalid file
        produces an error; do not assume that an invalid value silently falls back to its default.</p>
        <table>
            <thead><tr><th>Field</th><th>Default</th><th>Meaning</th></tr></thead>
            <tbody>
                <tr><td><code>version</code></td><td><code>1</code></td><td>Configuration schema version</td></tr>
                <tr><td><code>theme</code></td><td><code>"midnight"</code></td><td>String: <code>auto</code>, <code>midnight</code>, <code>daylight</code>, <code>tokyonight</code>, <code>rosepine-moon</code>, or <code>oscura-midnight</code></td></tr>
                <tr><td><code>mcpInitStrategy</code></td><td><code>"auto"</code></td><td>MCP initialization strategy; also accepts <code>"progressive"</code> and <code>"blocking"</code></td></tr>
                <tr><td><code>mcpServers</code></td><td>Empty object</td><td>Named local or remote MCP server configurations</td></tr>
                <tr><td><code>webFetch.enabled</code></td><td><code>false</code></td><td>Enable client-side URL fetching; the domain allowlist still applies</td></tr>
                <tr><td><code>webFetch.allowedDomains</code></td><td>Empty list</td><td>Allowed domains; an empty list denies requests</td></tr>
                <tr><td><code>lsp.enabled</code></td><td><code>false</code></td><td>Enable configured language-server support</td></tr>
                <tr><td><code>worktree.fetchLatestUpstream</code></td><td><code>true</code></td><td>Fetch upstream when preparing managed worktrees</td></tr>
                <tr><td><code>worktree.inactivityDays</code></td><td><code>1</code></td><td>Inactivity threshold used by worktree management</td></tr>
                <tr><td><code>maxConcurrentAgents</code></td><td>Unset</td><td>Machine-level concurrent-agent limit</td></tr>
            </tbody>
        </table>
        <h2 id="worktree-policy">Worktree policy, offline operation, and inactivity</h2>
        <p>The machine-wide <code>worktree</code> object contains two fields.
        <code>fetchLatestUpstream</code> is a boolean, defaulting to <code>true</code>;
        <code>inactivityDays</code> is a positive integer, defaulting to <code>1</code>.
        For offline creation and a seven-day inactivity threshold, merge this into
        <code>~/.haskell-agent/config.json</code>, preserving unrelated fields:</p>
        <pre><code class="language-json" data-config-schema="harness">{"{\n  \"version\": 1,\n  \"worktree\": {\n    \"fetchLatestUpstream\": false,\n    \"inactivityDays\": 7\n  }\n}" :: Text}</code></pre>
        <p>With fetching disabled, new managed worktrees start from local
        <code>HEAD</code>. With fetching enabled, the current branch's configured
        remote wins, followed by <code>upstream</code>, <code>origin</code>, then a sole
        remaining remote. If no remote is selected, creation uses local
        <code>HEAD</code>. Otherwise it fetches the remote default branch into an
        isolated temporary ref: it does not merge into your checkout or update
        normal remote-tracking refs.</p>
        <p>A cached remote default branch is reused. If that branch no longer
        exists, the agent discovers the remote default again; authentication,
        connectivity, or other fetch errors stop creation rather than silently
        using stale commits. For deliberately offline work, set fetching to
        <code>false</code>; do not interpret a failed fetch as a successful refresh.
        If a remote changes its default but keeps the old branch, explicitly fetch
        the new remote-tracking branch and update Git's remote HEAD with
        <code>git remote set-head REMOTE --auto</code>.</p>
        <p>Interactive and subagent worktree creation reload the current machine
        policy for each creation. Initial CLI creation and background cleanup use
        the validated startup snapshot; restart the CLI to apply manual changes
        to that snapshot. Changes do not rewrite existing worktree branches.</p>
        <p>Inactivity means elapsed 24-hour days since the later of the worktree's
        recorded activity and saved-session activity, not its creation age.
        Lease acquisition/release and explicit enrollment or recovery update
        recorded activity. The threshold never authorizes deletion on its own:
        active leases, protected/recovery-state checkouts, uncertain ownership,
        dirty files, and work not proven incorporated prevent collection.</p>
        <p>Background maintenance runs asynchronously during session resource
        setup, not as an exact expiry timer. Each pass has bounded time and
        removal-attempt budgets, so an eligible checkout may survive until a
        later pass. A failure to establish activity evidence retains work rather
        than assuming inactivity. Use the
        <a href="/guides/parallel-agents/">worktree lifecycle guide</a>
        to inspect eligibility, preview manual cleanup, and protect or recover
        work before changing retention settings.</p>
        <h2 id="limit-concurrent-agents">Example: limit concurrent agents</h2>
        <pre><code>/meta set the concurrent agent limit to 3</code></pre>
        <p>Inspect the proposed scope and value before confirming. An explicit launch limit takes
        precedence over project settings, which take precedence over the machine setting:</p>
        <pre><code class="language-sh">agent-cli --max-concurrent-agents 2</code></pre>
        <p>For a new configuration file, this minimal example sets only the machine limit. Do not replace
        an existing file with it; merge the field instead.</p>
        <pre><code class="language-json" data-config-schema="harness">{"{\n  \"version\": 1,\n  \"maxConcurrentAgents\": 3\n}" :: Text}</code></pre>
        <h2 id="verify-a-change">Verify a change</h2>
        <p>Harness-managed machine updates use a process-shared lock and atomic replacement.
        The configuration directory and file are written with private permissions.
        Preserve <code>config.lock</code> and <code>config.revision-key</code> alongside the file;
        the revision key supports detecting changes to the saved configuration.</p>
        <p>Loading an older remote MCP entry can assign and persist a missing connection
        identity. A read can therefore require write access during migration. If it fails,
        check ownership and permissions rather than inventing IDs. A stale configuration
        preview should be refreshed and reviewed again, not forced over another process's edit.</p>
        <ol>
            <li>Save a private backup of the existing file. Configuration can contain credentials;
            do not paste it into issue reports or commit it to a repository.</li>
            <li>Change one section, preserving unrelated keys. Prefer <code>/meta</code> for a validated
            preview instead of editing a large file blindly.</li>
            <li>Restart after manually changing startup settings. For an external MCP catalog edit,
            <code>/mcp</code> followed by <code>r</code> reloads the runtime without discarding the session.</li>
            <li>Exercise the capability you changed: a fetch, a language-server operation, an MCP
            read, or a small delegated task. A file that parses is not proof that its external service works.</li>
        </ol>
        <p>Use <code>/session-info</code> to inspect the active session and <code>/model</code> to check model
        selection. Restart the CLI when testing manually edited startup settings. If a configuration fails
        to load, restore your saved copy rather than deleting credentials or widening permissions.</p>
        <h2 id="configuration-errors">Configuration errors</h2>
        <table>
            <thead><tr><th>Error or symptom</th><th>Correction</th></tr></thead>
            <tbody>
                <tr><td><code>Unsupported harness config version</code></td><td>Use version 1; do not copy another product's schema</td></tr>
                <tr><td><code>must configure exactly one of url or command</code></td><td>Keep only the HTTP URL or local executable for that MCP entry</td></tr>
                <tr><td><code>maxInlineBytes must not exceed maxContentBytes</code></td><td>Reduce the inline limit or increase the content limit within its documented bounds</td></tr>
                <tr><td><code>LSP transport is unsupported</code></td><td>Configure a stdio language server, not an HTTP endpoint</td></tr>
                <tr><td>Change appears to have no effect</td><td>Check the file location, restart the affected runtime, and inspect project/launch overrides before changing more values</td></tr>
            </tbody>
        </table>
        <p>Continue with <a href="/customization/models/">custom model configuration</a>,
        <a href="/customization/mcp/">MCP integrations</a>, or <a href="/guides/projects/">project instructions</a>.</p>
    |]
    }

webFetchExample :: Text
webFetchExample = "{\n  \"version\": 1,\n  \"webFetch\": {\n    \"enabled\": true,\n    \"allowedDomains\": [\"haskell.org\"],\n    \"timeoutSeconds\": 30,\n    \"maxContentBytes\": 1048576,\n    \"maxInlineBytes\": 50000\n  }\n}"
