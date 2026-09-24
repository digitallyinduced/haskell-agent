{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.McpTutorial (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/tutorials/connect-mcp/"
    , pageTitle = "Connect and verify an MCP server"
    , pageDescription = "Connect Sentry, authorize the correct account, verify a read-only result, and recover safely from connection failures."
    , pageGroup = "Tutorials"
    , pageBody = [hsx|
        <p>This walkthrough connects the hosted Sentry MCP endpoint and verifies access without
        changing an issue. You will finish with a saved connection, an inspected tool catalog, and
        a read-only result that you can compare against the Sentry web interface.</p>
        <h2 id="prerequisites">Prerequisites</h2>
        <ul>
            <li>A working agent session with a configured model provider.</li>
            <li>A Sentry account with access to an organization and project you are authorized to inspect.</li>
            <li>A browser for authorization and network access to <code>https://mcp.sentry.dev/mcp</code>.</li>
        </ul>
        <p>This uses an external service. Its available tools and account requirements can change.
        Connecting it does not give the agent permissions beyond the account and scopes you authorize.
        Do not use a production administrator account merely to make the example work.</p>
        <h2 id="inspect-existing-connections">1. Inspect existing connections</h2>
        <pre><code class="language-sh">agent-cli mcp list</code></pre>
        <p>Look for an existing Sentry entry before adding another. If the label <code>sentry</code>
        is already in use, inspect that connection in <code>/mcp</code>; do not overwrite it blindly.
        The command lists configured servers without printing environment values.</p>
        <h2 id="add-sentry">2. Add the hosted endpoint</h2>
        <p>If no corresponding connection exists, run:</p>
        <pre><code class="language-sh">{"agent-cli mcp add sentry --transport http https://mcp.sentry.dev/mcp\nagent-cli mcp list" :: Text}</code></pre>
        <p>The second command should now list <code>sentry</code>. This verifies that configuration
        was saved, not that authorization succeeded. The entry lives in
        <code>~/.haskell-agent/config.json</code>.</p>
        <p>Alternatively, open <code>/mcp</code> in the agent, press <code>a</code>, and enter the
        same URL. The interactive add flow starts OAuth when required and restarts the MCP runtime
        after a saved change.</p>
        <h2 id="authorize-account">3. Authorize the intended account</h2>
        <ol>
            <li>Open an interactive agent session and enter <code>/mcp</code>.</li>
            <li>If the session was running when you changed configuration externally, press
            <code>r</code> to restart its MCP runtime.</li>
            <li>Select the Sentry connection with the arrow keys. Press <code>i</code> to authorize
            or re-authorize the HTTP server.</li>
            <li>Check the service, signed-in account, organization access, and requested scopes in
            the browser. Grant only access you intend the agent to use.</li>
            <li>Return to the agent and inspect the connection state.</li>
        </ol>
        <p>If you cancel authorization, the saved connection can remain present but unusable.
        A listed server is not proof of a valid grant. Never paste an OAuth callback URL or token
        into a public issue report.</p>
        <h2 id="inspect-tool-catalog">4. Inspect the actual tool catalog</h2>
        <p>With Sentry selected in <code>/mcp</code>, press <code>Enter</code> to inspect its tools.
        Choose an operation whose description explicitly reads or lists data. Do not assume that
        a remembered tool name still exists: the service can update its catalog.</p>
        <p>In deferred-discovery sessions, a connected tool may not yet be visible to the model.
        Request discovery explicitly:</p>
        <pre><code>Discover the Sentry tools that can list the organizations or projects accessible to my account. Do not perform any mutation.</code></pre>
        <p>Expected result: discovery of relevant tools from the Sentry connection, not a generic
        explanation of the Sentry API. If no tool appears, return to the manager and check the
        connection before retrying the prompt.</p>
        <h2 id="verify-read-only-access">5. Verify a read-only result</h2>
        <pre><code>Use the connected Sentry server to list the organizations or projects I can access. Report their identifiers and names. Do not create, update, resolve, assign, or delete anything.</code></pre>
        <p>Inspect the tool call and its result. If an approval is requested, read its operation and
        arguments; cancel if it performs a mutation. A server's read-only annotation is a declaration
        by that server, not an independent guarantee.</p>
        <ol>
            <li>Confirm the result came from the Sentry connection.</li>
            <li>Open the Sentry web interface yourself and compare an organization or project identifier.</li>
            <li>If the result is empty, verify that the authorized account has access before treating
            it as a connection failure.</li>
        </ol>
        <p>You now have stronger evidence than “connected”: the configured account can perform an
        authorized read and return recognizable data.</p>
        <h2 id="investigate-one-issue">6. Investigate one issue without changing it</h2>
        <p>Choose an issue you are permitted to read and substitute its actual identifier:</p>
        <pre><code>Read Sentry issue PROJECT-123 using the connected server. Summarize its error, affected release, and available stack-trace evidence. Do not resolve, assign, comment on, or otherwise modify it. Tell me if a requested field is unavailable.</code></pre>
        <p>Compare the returned evidence against the issue page. Treat a proposed code fix as a
        hypothesis until it is checked against the repository and reproduced in a test. An MCP
        connection supplies external context; it does not prove the cause of an application bug.</p>
        <h2 id="recover-from-failures">Recover from failures</h2>
        <table>
            <thead><tr><th>Failure</th><th>Next action</th></tr></thead>
            <tbody>
                <tr><td>Entry missing in a running session</td><td>Verify agent-cli mcp list, then /mcp and r to reload the catalog</td></tr>
                <tr><td>HTTP 401 or expired grant</td><td>Re-authorize the selected HTTP server with i and verify the browser account</td></tr>
                <tr><td>HTTP 403 or missing organization</td><td>Check Sentry account membership and requested scopes; do not broaden access without understanding the missing permission</td></tr>
                <tr><td>Connected but expected tool absent</td><td>Inspect the live catalog and repeat discovery; do not invent a tool name</td></tr>
                <tr><td>Read times out</td><td>Check service/network health and try a smaller read once; retain the error for diagnosis</td></tr>
                <tr><td>Mutation outcome uncertain</td><td>Inspect the remote issue state before any retry; a timeout does not mean the operation did not execute</td></tr>
            </tbody>
        </table>
        <h2 id="disable-or-remove">Disable or remove the connection</h2>
        <pre><code class="language-sh">agent-cli mcp disable sentry</code></pre>
        <p>Restart the running MCP runtime with <code>/mcp</code>, then <code>r</code>, to pick up
        the external edit. To remove the saved entry interactively, select it and press
        <code>x</code>, then <code>y</code>. Disabling or removing a local connection is not the
        same as revoking the service-side OAuth grant; revoke that through Sentry's account controls
        when access is no longer wanted.</p>
        <p>Continue with the <a href="/customization/mcp/">MCP integration reference</a> for local
        stdio servers, complete configuration fields, protocol selection, resources, prompts, and
        optional client capabilities.</p>
    |]
    }
