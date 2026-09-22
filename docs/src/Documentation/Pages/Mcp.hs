{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.Mcp (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/customization/mcp/"
    , pageTitle = "MCP integrations"
    , pageDescription = "Connect local and remote Model Context Protocol servers and manage their permissions."
    , pageGroup = "Customization"
    , pageBody = [hsx|
        <p>MCP servers provide additional tools, resources, and prompts. Haskell Agent
        can connect to local stdio processes and remote HTTP servers.</p>
        <p>For a complete first connection, follow
        <a href="/tutorials/connect-mcp/">Connect and verify an MCP server</a>.
        It uses Sentry's hosted endpoint and checks a read-only result before requesting any mutation.</p>
        <h2 id="add-a-server-interactively">Add a server interactively</h2>
        <p>Open:</p>
        <pre><code>/mcp</code></pre>
        <p>Use <code>a</code> to add a remote URL or local command. For an HTTP server, the manager
        starts OAuth when required. You can also ask Meta Console to add an endpoint:</p>
        <pre><code>/meta add the MCP server at https://example.com/mcp</code></pre>
        <p>Replace that example URL with your server's actual endpoint and review the
        configuration preview.</p>
        <h2 id="manage-connections">Manage connections</h2>
        <p>In the MCP manager:</p>
        <table>
            <thead><tr><th>Key</th><th>Action</th></tr></thead>
            <tbody>
                <tr><td>Arrow keys or <code>j</code> / <code>k</code></td><td>Select a server</td></tr>
                <tr><td><code>Enter</code></td><td>Inspect its tools</td></tr>
                <tr><td><code>i</code></td><td>Re-authorize an HTTP server</td></tr>
                <tr><td><code>Space</code></td><td>Enable or disable</td></tr>
                <tr><td><code>x</code>, then <code>y</code></td><td>Remove</td></tr>
                <tr><td><code>r</code></td><td>Restart the MCP runtime</td></tr>
            </tbody>
        </table>
        <p>Saved interactive changes restart the runtime without discarding the session.
        Environment values are not displayed.</p>
        <h2 id="configure-from-the-command-line">Configure from the command line</h2>
        <pre><code class="language-sh">{"agent-cli mcp list\nagent-cli mcp list --json\nagent-cli mcp add sentry --transport http https://mcp.sentry.dev/mcp\nagent-cli mcp disable sentry\nagent-cli mcp enable sentry" :: Text}</code></pre>
        <p>These commands manage <code>~/.haskell-agent/config.json</code>. A running session picks up
        external catalog changes when you restart its MCP runtime with <code>/mcp</code>, then <code>r</code>.</p>
        <h3 id="catalog-json">Read the catalog as JSON</h3>
        <p><code>agent-cli mcp list --json</code> writes a JSON array, including disabled entries.
        Each object has <code>name</code> (string), <code>enabled</code> (boolean),
        <code>transport</code> (string), <code>url</code> (string or null),
        <code>command</code> (string), <code>args</code> (string array),
        <code>cwd</code> (string or null), and <code>envKeys</code> (string array).
        Environment values are intentionally omitted. This is saved configuration, not a live
        connectivity or authentication check; an empty array means no configured entries.</p>
        <pre><code class="language-sh">{"agent-cli mcp list --json | jq -r '.[] | select(.enabled) | .name'" :: Text}</code></pre>
        <p>The example needs <code>jq</code>. In scripts, check the command's exit status before
        parsing stdout; a configuration-read error is not an empty catalog. Do not execute command
        or URL strings from the output as shell code.</p>
        <p>A local server can be managed through Nix:</p>
        <pre><code class="language-sh">agent-cli mcp add project-tools -- nix run /absolute/path/to/server</code></pre>
        <p>The referenced flake must run a stdio MCP server. The agent does not supply an
        implementation for this example.</p>
        <h2 id="configuration-examples">Configuration examples</h2>
        <p>Merge entries into <code>mcpServers</code> in <code>~/.haskell-agent/config.json</code>.
        This example illustrates both transports. Replace the local flake path and remote URL with
        servers you operate or trust; neither placeholder is a bundled server.</p>
        <pre><code class="language-json" data-config-schema="harness">{configurationExample}</code></pre>
        <p><code>command</code> is an executable, and <code>args</code> contains separate arguments.
        Shell operators are not interpreted automatically. A stdio server must reserve stdout for
        protocol messages and write diagnostic output to stderr. Run the local command in your Nix
        environment first to check that the executable exists; a protocol server waiting on stdin is
        not itself a failure.</p>
        <h2 id="server-field-reference">Server field reference</h2>
        <table>
            <thead><tr><th>Field</th><th>Type / default</th><th>Behavior</th></tr></thead>
            <tbody>
                <tr><td><code>enabled</code></td><td>Boolean / true</td><td>Disabled entries remain configured but are not started</td></tr>
                <tr><td><code>url</code></td><td>Optional string</td><td>Remote Streamable HTTP endpoint; mutually exclusive with command</td></tr>
                <tr><td><code>command</code></td><td>String / empty</td><td>Local executable; required when url is absent</td></tr>
                <tr><td><code>args</code></td><td>String array / empty</td><td>Arguments for the local executable</td></tr>
                <tr><td><code>cwd</code></td><td>Optional string</td><td>Working directory for a local process</td></tr>
                <tr><td><code>env</code></td><td>String-to-string object / empty</td><td>Server environment; values are hidden in manager/list diagnostics, not encrypted by putting them in JSON</td></tr>
                <tr><td><code>startupTimeoutSeconds</code></td><td>Positive integer / 30</td><td>Startup timeout; allow additional time for a first Nix build</td></tr>
                <tr><td><code>requestTimeoutSeconds</code></td><td>Positive integer / 60</td><td>Idle request timeout; progress can extend the wait</td></tr>
                <tr><td><code>protocol</code></td><td>String / auto</td><td>auto, modern, or legacy; see negotiation below</td></tr>
                <tr><td><code>roots</code></td><td>Boolean / false</td><td>Permit workspace-root requests when supported by the host</td></tr>
                <tr><td><code>sampling</code></td><td>Boolean / false</td><td>Permit isolated model-generation requests when supported by the host</td></tr>
                <tr><td><code>logLevel</code></td><td>Optional string</td><td>debug, info, notice, warning, error, critical, alert, emergency; absent leaves server logging unchanged</td></tr>
                <tr><td><code>oauth</code></td><td>Optional object</td><td>Remote-only client registration and scopes; see authentication below</td></tr>
                <tr><td><code>displayName</code></td><td>Optional nonblank string</td><td>Human-readable connection name</td></tr>
            </tbody>
        </table>
        <p>Managed remote entries can also contain <code>connectionId</code>,
        <code>connectionCredentials</code>, and <code>connectionGeneration</code>. These identify the
        protected credential selection and lifecycle. Preserve them when editing an existing entry;
        do not duplicate connection identities or manufacture them for a new server.</p>
        <h2 id="remote-authentication">Remote authentication and OAuth</h2>
        <ol>
            <li>Add the HTTP endpoint through <code>/mcp</code>, then follow its authorization flow.</li>
            <li>Verify the browser's service and requested scopes before granting access.</li>
            <li>Return to the manager and inspect the server's tools. Test a read-only operation
            against a resource you can independently identify.</li>
            <li>Use <code>i</code> on the selected HTTP server to re-authorize after revoking access
            or changing the required account.</li>
        </ol>
        <p>For a pre-registered OAuth application, the optional <code>oauth</code> object accepts
        <code>clientId</code>, <code>clientSecret</code>, <code>clientIdMetadataUrl</code>, and
        <code>scopes</code>. The first three are optional strings; scopes is a string array, empty
        by default. A client secret requires a client ID. A metadata URL must use HTTPS and include
        a path; scope entries must not be blank. Use the values issued by the server operator,
        not arbitrary identifiers.</p>
        <p>For example, merge this named server into your existing configuration rather than
        replacing unrelated settings:</p>
        <pre><code class="language-json" data-config-schema="harness">{oauthExample}</code></pre>
        <p>For services using a supplied bearer token, <code>env.MCP_ACCESS_TOKEN</code> supplies the
        token. Legacy token-file integrations can use <code>env.MCP_OAUTH_TOKEN_FILE</code> pointing
        to a private JSON record with <code>client_id</code>, <code>token_endpoint</code>,
        <code>access_token</code>, <code>refresh_token</code>, and <code>expires_at</code>.
        Keep these files private and outside version control. Prefer the interactive managed flow
        instead of copying credentials between files.</p>
        <p>On a token-file request returning HTTP 401, the client locks and re-reads the record,
        refreshes the token, atomically saves rotated credentials with private permissions, and
        retries once. Failed refresh is an error, not an unbounded authorization loop.
        HTTP 401/403 challenges can indicate missing scopes as well as an invalid token.</p>
        <h2 id="startup-and-tool-discovery">Startup and tool discovery</h2>
        <p>By default, interactive sessions start servers progressively so you can use the
        prompt while connections initialize. One-shot commands wait for initialization.
        A connecting or unavailable server cannot yet provide usable tools.</p>
        <p>Some providers discover MCP tools on demand rather than placing every tool in
        every model request. Ask for the operation you need; the model can search the
        connected tool catalog. Enabled server connections are shared with subagents,
        but discovery state can be session-local.</p>
        <h2 id="tool-payloads">Discovery and invocation payloads</h2>
        <table><thead><tr><th>Dialect</th><th>Discovery</th><th>Invocation</th></tr></thead><tbody>
            <tr><td>Codex</td><td><code>tool_search</code>: required <code>query</code>, optional <code>limit</code> (default 8)</td><td>Matching declarations become available on the next model request; call the discovered tool directly</td></tr>
            <tr><td>Generic</td><td><code>mcp_search</code>: optional <code>query</code>, <code>server</code>, <code>limit</code> (1–50)</td><td><code>mcp_call</code>: required <code>name</code> in qualified <code>server__tool</code> form; optional object <code>arguments</code></td></tr>
            <tr><td>Grok</td><td><code>search_tool</code>: required <code>query</code>, optional <code>limit</code> (default 5, range 1–255)</td><td><code>use_tool</code>: required <code>tool_name</code> and object <code>tool_input</code></td></tr>
        </tbody></table>
        <pre><code class="language-json">{"{\"query\":\"list projects\",\"server\":\"YOUR_CONNECTED_SERVER\",\"limit\":5}" :: Text}</code></pre>
        <p>This example is for generic discovery, not invocation. Read the returned schema and
        use its real qualified name; do not assume a tool called <code>list_projects</code>
        exists. A partial catalog means some servers may still be connecting. Recheck status
        before searching again; restarting a runtime can change its catalog.</p>
        <p>Resource listing is separate: <code>mcp_list_resources</code> accepts optional
        <code>server</code> and otherwise queries all connected resource-capable servers.
        <code>mcp_read_resource</code> requires <code>server</code> and <code>uri</code>
        returned by that listing or a <code>resource_link</code>. A template must be instantiated
        with valid values before reading; a URI is not necessarily an HTTP URL.</p>
        <pre><code class="language-json">{"{\"server\":\"YOUR_CONNECTED_SERVER\",\"uri\":\"REPLACE_WITH_RETURNED_RESOURCE_URI\"}" :: Text}</code></pre>
        <p>Resource content may be text or encoded binary data; treat it as untrusted source
        material. For an uncertain mutation result, inspect the affected resource before retrying:
        a transport error does not prove that a send, creation or update did not happen.</p>
        <h2 id="protocol-negotiation">Protocol negotiation</h2>
        <p><code>auto</code> first probes <code>server/discover</code> for the modern
        <code>2026-07-28</code> protocol. A failed probe or no answer within five seconds falls back
        to legacy <code>initialize</code>, requesting <code>2025-11-25</code>. The selected protocol
        era is remembered across reconnects. Use <code>legacy</code> to skip the probe for a known
        older server, or <code>modern</code> to require discovery to succeed.</p>
        <p>Do not change protocol selection to fix an expired token. Authentication failures and
        protocol incompatibility need different remedies.</p>
        <h2 id="cli-oauth-identity">CLI OAuth scopes and logout</h2>
        <pre><code class="language-sh">{"agent-cli mcp login https://example.com/mcp --scope read --scope write\nagent-cli mcp logout https://example.com/mcp" :: Text}</code></pre>
        <p>Replace the example endpoint and scopes with values required by your service.
        Repeat <code>--scope</code> for additional scopes. Login performs authorization;
        requested scopes combine the first nonempty source (server challenge, resource metadata,
        then configured scopes), previously granted scopes for a matching issuer/resource, and
        your additional scopes, without duplicates. <code>offline_access</code> is included only
        when the authorization server advertises it. Requesting a scope does not guarantee that
        the account is authorized for it; inspect the consent screen and verify a read-only operation.</p>
        <p>The CLI credential file under <code>~/.haskell-agent/credentials/mcp/</code> is keyed by
        the exact URL string, including its query. Use the same URL spelling for login, configured
        startup and logout. Configuration OAuth lookup can normalize equivalent endpoint spelling,
        but preserves the query; it is not a reason to assume two spellings share a credential file.
        A changed issuer or resource prevents reuse of the old registration and granted scopes.
        Managed hosts can instead bind credentials to an immutable connection identity.</p>
        <p><code>logout</code> removes that URL's local credential file if present; an absent file
        is a no-op. It does not remove the server configuration, revoke the grant at the provider,
        remove an explicitly supplied bearer token, or terminate another process's authenticated
        connection. Disable or restart the running MCP runtime separately, and use the provider's
        account controls if remote grant revocation is required.</p>
        <h2 id="prompts-and-resources">Prompts and resources</h2>
        <p>MCP is not limited to tools. A server may publish prompt templates and addressable resources.
        Invoke a template using the server and prompt names from its catalog:</p>
        <pre><code>/mcp prompt server-name prompt-name argument=value</code></pre>
        <p><code>/mcps</code> is an alias for <code>/mcp</code>, including prompt invocation.
        With no arguments either opens the manager. For a server whose catalog declares a
        <code>review</code> prompt with a <code>target</code> argument, an invocation would be:</p>
        <pre><code>/mcp prompt project-tools review target=src</code></pre>
        <p>This is a catalog-dependent example, not a bundled prompt. Each argument token is split
        at its first <code>=</code>; the remainder is a string value, and a token without
        <code>=</code> supplies an empty value. Use the exact declared names, avoid duplicate keys,
        and do not pass secrets. The server validates its required arguments. A missing server,
        unsupported prompt or failed request displays an error without submitting an expanded turn.</p>
        <p>On success the returned messages are rendered and submitted as the next model turn,
        rather than merely previewed in the editor. Review whether you trust the server before
        invoking a prompt; its text can request actions but does not grant tool authorization.</p>
        <p>The resolved messages become the next turn. Resources are available through
        <code>mcp_list_resources</code> and <code>mcp_read_resource</code>; request a resource's actual
        URI rather than guessing a filesystem path. Tool results may contain resource links that
        require a separate read.</p>
        <p>Server instructions are supplied to the model. In progressive mode they arrive when the
        connections settle. Server-published skills can appear alongside local skills and are fetched
        and integrity-checked when used.</p>
        <h2 id="progress-and-user-input">Progress, cancellation, and user input</h2>
        <p><code>requestTimeoutSeconds</code> measures inactivity. Progress notifications extend the
        wait, up to ten times the configured value, and appear in running-tool output. A timed-out
        stdio request receives a cancellation notification; an HTTP response stream is closed.
        Cancellation does not prove that a remote mutation was undone.</p>
        <p>Servers may request a form or an authorization URL during a call. Interactive forms are
        validated and reviewed before submission; URL requests show the destination and require
        explicit consent before opening a browser. Decline and cancel remain available.
        Non-interactive runs do not advertise elicitation support.</p>
        <p>When servers announce changed tools, the manager and discovery catalog refresh.
        A changed deferred tool schema or approval declaration requires rediscovery. An older
        exposed handler will reject changed definitions instead of silently using the new contract.</p>
        <h2 id="permissions">Permissions</h2>
        <p>Before requesting a mutation, verify the connection with a small inspection:</p>
        <ol>
            <li>Open <code>/mcp</code>, select the server, and press <code>Enter</code> to inspect its tools.</li>
            <li>Choose an available read-only operation and ask the agent to perform it
            against a resource you are authorized to access.</li>
            <li>Check that the result names the intended service and resource.</li>
        </ol>
        <p><strong>Expected result:</strong> a tool result from that server, not merely a
        description of what the agent could do. If the tool is missing, inspect the
        connection status and available catalog before changing permissions.</p>
        <p>Tools explicitly marked read-only by the server avoid generic mutation
        approval. Other tools are treated as mutations under the session's approval
        policy. A server's read-only annotation is its claim, not proof that its
        implementation is harmless; connect only servers you trust.</p>
        <p>Roots and sampling are disabled by default. Roots can disclose the workspace
        location. Sampling lets a server request generation from your configured
        model. Enable these per server only when needed.</p>
        <p>If authorization or startup fails, inspect the server in <code>/mcp</code>, check its
        endpoint or executable, and re-authorize or restart rather than repeatedly
        retrying a mutation with an uncertain outcome.</p>
        <h2 id="diagnose-a-connection">Diagnose a connection</h2>
        <table>
            <thead><tr><th>Symptom</th><th>Check</th><th>Recovery</th></tr></thead>
            <tbody>
                <tr><td>Server absent</td><td><code>agent-cli mcp list</code>, spelling, enabled flag</td><td>Add or enable the intended entry; restart with /mcp then r</td></tr>
                <tr><td>Local startup timeout</td><td>Executable availability, Nix build output, stdout protocol discipline</td><td>Complete installation first; increase startup timeout only for a genuinely slow startup</td></tr>
                <tr><td>HTTP 401 or 403</td><td>Account, expired grant, requested scopes, endpoint</td><td>Re-authorize with i; verify read access before retrying a write</td></tr>
                <tr><td>Connected but tool unavailable</td><td>Tool catalog, deferred discovery, changed schema</td><td>Inspect with Enter and request discovery again</td></tr>
                <tr><td>Request times out</td><td>Service health and progress output</td><td>Inspect remote state before repeating a mutation; do not blindly lengthen every timeout</td></tr>
                <tr><td>Roots or sampling unavailable</td><td>Per-server setting and host support</td><td>Enable only the capability the trusted server actually needs</td></tr>
            </tbody>
        </table>
        <h2 id="current-limitations">Current limitations</h2>
        <p>MCP image and audio blocks are described to the model, but their bytes are not forwarded.
        Task identifiers are not persisted across restarts. Icon metadata is retained but is not
        rendered by the terminal interface.</p>
    |]
    }

configurationExample :: Text
configurationExample = "{\n  \"version\": 1,\n  \"mcpInitStrategy\": \"auto\",\n  \"mcpServers\": {\n    \"project-tools\": {\n      \"command\": \"nix\",\n      \"args\": [\"run\", \"/absolute/path/to/server\"],\n      \"startupTimeoutSeconds\": 120,\n      \"requestTimeoutSeconds\": 60,\n      \"protocol\": \"auto\",\n      \"roots\": false,\n      \"sampling\": false,\n      \"logLevel\": \"warning\"\n    },\n    \"remote-tools\": {\n      \"url\": \"https://example.com/mcp\"\n    }\n  }\n}"

oauthExample :: Text
oauthExample = "{\n  \"version\": 1,\n  \"mcpServers\": {\n    \"remote-tools\": {\n      \"url\": \"https://example.com/mcp\",\n      \"oauth\": {\n        \"clientId\": \"your-registered-client-id\",\n        \"scopes\": [\"read\"]\n      }\n    }\n  }\n}"
