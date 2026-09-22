{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.Providers (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/reference/providers/"
    , pageTitle = "Provider reference"
    , pageDescription = "Select provider transports, distinguish account and API billing, and verify the active connection."
    , pageGroup = "Reference"
    , pageBody = [hsx|
        <p>A provider supplies authentication and transport. A model selects the model identifier and
        its tool dialect. Changing one does not guarantee that the other provider exposes the same tools,
        context limit, or billing method.</p>
        <h2 id="provider-identifiers">Provider identifiers</h2>
        <table>
            <thead><tr><th>Identifier</th><th>Connection</th><th>Before starting</th></tr></thead>
            <tbody>
                <tr><td><code>openai</code></td><td>OpenAI</td><td>Connect a supported account or API credential through the login interface</td></tr>
                <tr><td><code>xai</code></td><td>xAI / Grok</td><td>Connect a supported Grok account or API credential</td></tr>
                <tr><td><code>openrouter</code></td><td>OpenRouter</td><td>Connect credentials; <code>OPENROUTER_API_KEY</code> is recognized</td></tr>
                <tr><td><code>gemini</code></td><td>Google Gemini</td><td>Use Google sign-in, or <code>GOOGLE_API_KEY</code> / <code>GEMINI_API_KEY</code> for API billing</td></tr>
                <tr><td><code>claude-code</code></td><td>Claude Code SDK</td><td>Install Claude Code, run <code>claude auth login</code>, and select the provider explicitly</td></tr>
            </tbody>
        </table>
        <p>Without <code>--provider</code>, the CLI detects a provider from available authentication.
        Claude Code requires explicit selection. Use the current <code>/model</code> catalog rather
        than assuming that a model name offered by one provider works with another.</p>
        <h2 id="openai">OpenAI / ChatGPT</h2>
        <ol>
            <li>Open <code>/login</code> and connect the OpenAI account using the offered login flow.</li>
            <li>For API billing, add the API credential through the account interface rather than placing it in a prompt.</li>
            <li>Start <code>agent-cli --provider openai</code>, select an OpenAI model, and inspect <code>/session-info</code>.</li>
        </ol>
        <p>The coding credential loader also reads <code>CODEX_ACCESS_TOKEN</code>,
        <code>CODEX_AUTH_JSON</code>, and <code>~/.codex/auth.json</code>. <code>CODEX_HOME</code>
        changes the directory containing that file. These are credential sources, not model catalog
        settings. A plain access token cannot provide the same refresh information as a complete OAuth login.</p>
        <p>Enabled managed accounts are combined with external accounts; managed accounts suppress
        duplicates with the same account identity. When subscription accounts exist, the automatic
        OpenAI pool uses subscription accounts rather than mixing them with API-billed accounts.
        Do not assume that exporting <code>OPENAI_API_KEY</code> selects coding API billing: that
        variable is also used by separate functionality such as dictation. Use the login interface
        to configure coding credentials explicitly.</p>
        <h2 id="xai">xAI / Grok</h2>
        <ol>
            <li>Open <code>/login</code> and connect your Grok account, or use <code>/meta connect my Grok account</code>.</li>
            <li>Complete the browser flow and return to the terminal.</li>
            <li>Start <code>agent-cli --provider xai</code> and choose a Grok model from <code>/model</code>.</li>
        </ol>
        <p>External credentials are also discovered from <code>GROK_AUTH_JSON</code>,
        <code>GROK_ACCESS_TOKEN</code>, and <code>~/.grok/auth.json</code>.
        Managed credentials are considered before external sources. Use a complete login when
        refresh is required; do not repeatedly reuse an expired bearer token.</p>
        <h2 id="openrouter">OpenRouter</h2>
        <p>Create a key in your OpenRouter account, then either add it through <code>/login</code>
        or supply <code>OPENROUTER_API_KEY</code> in the environment of the process that starts
        Haskell Agent. A managed credential takes precedence over the external environment credential
        when no specific external account was selected.</p>
        <pre><code class="language-sh">agent-cli --provider openrouter</code></pre>
        <p>Select the exact OpenRouter model identifier from <code>/model</code>. To add another
        OpenRouter model, invoke <code>$add-model</code> with its identifier. OpenRouter has its own
        API billing and model availability; a direct-provider subscription does not fund this route.</p>
        <h2 id="gemini">Google Gemini</h2>
        <p>Choose a Gemini entry in <code>/model</code> to initiate Google sign-in when no credential
        exists. For AI Studio API billing, supply <code>GOOGLE_API_KEY</code> or
        <code>GEMINI_API_KEY</code> before starting. Among those environment variables,
        <code>GOOGLE_API_KEY</code> takes precedence. Managed Gemini credentials are considered
        first, so an existing managed connection can explain why adding an environment variable
        did not change the account.</p>
        <h2 id="meta-model-api">Meta Model API</h2>
        <p>The shipped catalog also includes a direct Meta connection. Supply <code>MODEL_API_KEY</code>
        to the process and select <code>agent-cli --model muse-spark-1.2</code>.
        The separate <code>meta/muse-spark-1.2</code> catalog entry uses OpenRouter instead.
        Select by model identifier; do not infer a new <code>--provider</code> value from a connection name.</p>
        <h2 id="claude-code">Claude Code SDK</h2>
        <p>Authenticate the installed Claude Code executable with <code>claude auth login</code>,
        then run <code>agent-cli --provider claude-code --model sonnet</code>.
        This provider is never chosen by automatic credential detection. Its SDK tools and
        authentication differ from the Responses connections; a custom Responses URL does not
        configure Claude Code.</p>
        <h2 id="detection-order">Automatic provider detection</h2>
        <p>When no provider is selected and credential detection is used, the order is
        OpenAI, xAI, OpenRouter, then Gemini. This checks available authentication, not the cheapest
        provider or the fastest model. An explicit <code>--provider</code> avoids ambiguity.
        A custom catalog model selects its own connection instead of adding a provider to this order.</p>
        <h2 id="explicit-provider">Example: choose and verify Gemini</h2>
        <pre><code class="language-sh">agent-cli --provider gemini</code></pre>
        <p>Complete Google sign-in if prompted, select a Gemini model, then inspect the session:</p>
        <pre><code>{"/model\n/session-info" :: Text}</code></pre>
        <p>Expected result: the session reports your selected model and its available tools. If login
        fails, use <code>/login</code> to inspect connections. Do not paste credentials into a normal prompt.</p>
        <h2 id="connection-diagnostics">Connection diagnostics</h2>
        <table>
            <thead><tr><th>Symptom</th><th>Check</th></tr></thead>
            <tbody>
                <tr><td>No credentials found</td><td>Connect the intended provider with <code>/login</code>. Environment variables must be present in the launching process, not another terminal.</td></tr>
                <tr><td>Unexpected account or billing</td><td>Inspect the login dashboard and managed accounts before changing keys; explicit provider selection does not itself select an API billing account.</td></tr>
                <tr><td>401 / expired token</td><td>Reauthorize the selected account. After changing an external credential file, use <code>/reload-auth</code>.</td></tr>
                <tr><td>Model unavailable</td><td>Confirm the exact catalog identifier and that the selected account has access; similarly named direct and OpenRouter models use different connections.</td></tr>
                <tr><td>429 / usage exhausted</td><td>Inspect <code>/usage</code> and the provider reset time. Reauthentication does not replenish quota.</td></tr>
            </tbody>
        </table>
        <p>For credential and direct-transport overrides, use the
        <a href="/reference/environment/">environment reference</a>. Those variables
        do not override every managed account or gateway routing decision.</p>
        <h2 id="multiple-accounts">Multiple accounts and remembered selection</h2>
        <p>Provider-local usage ranking is available for direct OpenAI, xAI and OpenRouter
        accounts. It is not used for gateway authentication, Gemini or Claude Code.
        Among candidates with a known positive remaining capacity, a usable remembered
        account wins. Otherwise the candidate with greatest capacity wins; ties keep
        discovery order. An account with unknown capacity is not treated as verified
        available by this ranking procedure.</p>
        <p>Remembered account identities live in checkout
        <code>lastAccounts</code>, separate from credentials. Clearing that preference
        does not disconnect an account. See
        <a href="/reference/persisted-settings/#account-records">account records</a>
        before changing saved selection data.</p>
        <p>For OpenAI, enabled managed accounts are considered first, followed by external
        token, environment JSON and auth-file sources. Managed account identities suppress
        matching external duplicates; the remaining list is deduplicated. An existing
        subscription pool is not silently mixed with API-billed credentials.</p>
        <h2 id="account-recovery">Example: diagnose the wrong account</h2>
        <ol>
            <li>Open <code>/login</code> and identify the provider, account and enabled state.
            Do not assume <code>--provider</code> pins one account.</li>
            <li>Use <code>/usage</code> to inspect quota before assigning work. A remembered
            account is useful only while it remains usable.</li>
            <li>If testing a different managed account, disable the unintended managed
            connection through the login interface; do not delete all credentials.</li>
            <li>After replacing an external authentication file, run <code>/reload-auth</code>.
            Restart after changing the launching process's environment.</li>
            <li>Inspect <code>/session-info</code>, then send a small non-mutating request.
            Check the reported route and account rather than asking the model to identify itself.</li>
        </ol>
        <table><thead><tr><th>Failure</th><th>Recovery</th></tr></thead><tbody>
            <tr><td>Browser login cancelled</td><td>Return to the account interface and start the intended connection again; cancellation is not a successful authorization.</td></tr>
            <tr><td>Expired static bearer token</td><td>Replace the token or use a complete login with refresh information; reload the matching source.</td></tr>
            <tr><td>Revoked OAuth grant</td><td>Reconnect the account and review the provider's account/scopes before continuing.</td></tr>
            <tr><td>Quota exhausted or cooldown</td><td>Inspect reset/capacity information; waiting or choosing another eligible account is different from reauthentication.</td></tr>
            <tr><td>Unknown capacity</td><td>Inspect usage/service errors; unknown is not positive remaining quota.</td></tr>
            <tr><td>Duplicate-looking account</td><td>Inspect managed versus external sources and account identities; do not assume identical labels mean different billing accounts.</td></tr>
        </tbody></table>
        <h2 id="external-credential-formats">External credential document formats</h2>
        <p><code>CODEX_AUTH_JSON</code> and the Codex auth file accept an object with
        string <code>access_token</code> and <code>account_id</code>, plus optional
        string <code>refresh_token</code> and <code>id_token</code>. The same object
        may be nested under <code>tokens</code>, which takes precedence over flat fields.
        A nonempty array of these documents is accepted, but only its first entry is
        selected: it is not a way to add multiple accounts. Every array element must
        decode. Missing required fields, an empty array or malformed JSON reject
        that source. Use managed connections to register multiple accounts.</p>
        <p><code>GROK_AUTH_JSON</code> and the Grok auth file accept a flat object or
        a one-level nested object with <code>key</code> or <code>access_token</code>
        strings; <code>key</code> wins when both exist. A valid flat object wins over
        nested objects. Avoid multiple nested token objects: this is one credential,
        not an account list. Optional fields are <code>refresh_token</code>,
        <code>id_token</code>, <code>expires_at</code>, <code>expires_in</code>,
        <code>oidc_client_id</code>, <code>principal_type</code>,
        <code>principal_id</code> and <code>email</code>. All are strings except
        <code>expires_in</code> (integer seconds) and <code>expires_at</code>
        (ISO timestamp or numeric Unix seconds). Expiry uses <code>expires_at</code>,
        then load time plus <code>expires_in</code>, then the access token's JWT expiry.
        A valid environment JSON credential wins over <code>GROK_ACCESS_TOKEN</code>;
        otherwise the token is tried. The file is discovered separately.</p>
        <p>These formats describe interoperability with credential-producing programs,
        not templates to paste into chat. Keep the original private file and its
        permissions; never put tokens in project configuration or example prompts.</p>
        <h2 id="credential-refresh">Token rotation and recovery outcomes</h2>
        <p>OpenAI and Grok refresh under locks and re-read the credential source to
        avoid rotating stale tokens concurrently. OpenAI rejects a refresh that
        changes account identity. Managed OAuth credentials write back to the protected
        store; external file credentials write back to their original file.
        Environment JSON refresh is in-memory only: it does not update the parent
        shell or survive a new process. Static bearer credentials cannot refresh.</p>
        <p>Grok retains the old refresh token when the provider does not return a
        replacement. File rotation preserves the supported nested token container
        and surrounding profile fields. If persistence fails, the refresh reports
        an error rather than presenting the rotation as successfully saved. Restore
        access to the original private store/file, then reload or reconnect; do not
        solve this by making a token file world-readable.</p>
        <p>OpenAI authentication rejection triggers immediate refresh recovery.
        If it remains broken, the account's authentication cooldown is 60 seconds.
        Rate-limit cooldown is separate: successful authentication refresh does not
        erase quota exhaustion. When all accounts are unavailable, inspect the
        reported retry/reset time. The CLI can briefly count down before retrying;
        <code>Esc</code> cancels that wait. Reconnecting does not reset provider quota.</p>
        <h2 id="gemini-eligibility">Gemini subscription eligibility and endpoint failures</h2>
        <ol>
            <li>Confirm whether you intend API-key access or Google subscription
            sign-in. Managed credentials are consulted before environment keys.</li>
            <li>For subscription sign-in, finish Google authorization and any validation
            presented by Code Assist. An OAuth token alone is not proof of model access.</li>
            <li>Code Assist loads the current tier and project; without a current tier,
            it selects an allowed default tier and runs onboarding. Free-tier onboarding
            does not send a configured project. Other tiers can require one.</li>
            <li>If onboarding is ineligible, unfinished or missing a required project,
            resolve the Google account/project entitlement with your administrator.
            Repeatedly changing model names or endpoint URLs does not grant eligibility.</li>
            <li>After successful setup, inspect the active account/model and make a small
            request. API-key authentication failures concern the direct Gemini endpoint;
            subscription setup failures concern Code Assist. Use the endpoint-specific
            error to distinguish authorization, onboarding and model access.</li>
        </ol>
        <p>The harness reports provider eligibility and onboarding failures rather
        than granting organization permissions. If changing to an API key instead,
        review the separate API billing route before reconnecting.</p>
        <h2 id="gateway-credentials">Organization gateway credentials</h2>
        <p>A saved gateway credential takes precedence over local provider discovery.
        The credential's base URL and WebSocket URL must use the same origin;
        mismatched or invalid credentials fail rather than silently becoming direct
        requests. The gateway bearer is not a direct OpenAI, xAI or Claude API key.</p>
        <p>With a gateway active, no explicit provider or <code>openai</code> uses
        gateway routing; <code>xai</code> and <code>claude-code</code> use their gateway
        routes. Other explicit providers require disconnecting the gateway first.
        Inspect the gateway account label/origin, not just the model's display label.
        Local-account operations and dictation use their separate credential boundaries.</p>
        <p>Disconnect the gateway through the login account interface and restart
        the agent to apply the route change immediately. Disconnection invalidates
        local credential ownership and removes the saved gateway credential;
        it is not a promise that the remote server revoked every issued bearer.
        For compromise or organization-wide revocation, use the gateway administrator's
        server-side revocation procedure. For an expired/revoked gateway credential,
        reconnect to the intended organization rather than exporting direct-provider
        keys and assuming they replace the gateway.</p>
        <h3 id="gateway-command-lifecycle">Connect and disconnect a gateway from the shell</h3>
        <pre><code class="language-sh">{"agent-cli gateway connect --url https://gateway.example.com\nagent-cli gateway status" :: Text}</code></pre>
        <p>Replace the example with your organization's gateway. The URL must have
        a host, no embedded username/password, query or fragment, and use HTTPS.
        HTTP is accepted only for localhost or loopback addresses. Connect prints
        a verification URL and device code, attempts to open the browser, then waits
        for authorization. If opening the browser fails, visit the printed URL
        yourself and enter the code. Only enter it on the expected gateway.</p>
        <p>After authorization and validation of advertised MCP servers, success
        prints <code>Gateway connection saved.</code>. The credential lives in
        <code>~/.haskell-agent/credentials/gateway.json</code>; do not paste or commit
        it. The connection also installs gateway MCP entries and invalidates MCP
        runtimes. A failed authorization or save is not a successful connection:
        inspect the error, verify the URL and local file permissions, and restart
        authorization if the code expired or was denied.</p>
        <p>Status prints <code>Connected to</code> followed by the saved base URL,
        then <code>Responses WebSocket:</code> and its saved endpoint. This inspects
        local credentials, not server health or token validity. <code>Not connected
        to a gateway.</code> means no saved credential; malformed/unreadable credentials
        produce an error rather than this disconnected result.</p>
        <pre><code class="language-sh">{"agent-cli gateway disconnect\nagent-cli gateway status\nagent-cli --provider openrouter" :: Text}</code></pre>
        <p>Successful disconnect prints <code>Gateway connection removed.</code>,
        removes the local gateway credential and gateway MCP entries, and invalidates
        MCP runtimes. Status should then report disconnected. Start a new direct-provider
        session with separately configured credentials, as in the OpenRouter example.
        Disconnect does not revoke the server-side token or manufacture direct-provider
        credentials; contact the administrator for revocation.</p>
        <h2 id="claude-diagnostics">Claude Code executable diagnostics</h2>
        <p>The harness uses a nonblank, trimmed <code>CLAUDE_CODE_EXECUTABLE</code>
        override, otherwise searches <code>PATH</code> for <code>claude</code>.
        It probes <code>claude auth status --json</code>. “Not found” means install
        the executable or fix the override; launch failure means inspect executable
        permissions/path; nonzero status or unreadable JSON means run that diagnostic
        directly and repair the Claude installation/login. Do not substitute an
        arbitrary script that prints a success value.</p>
        <p>Claude Code is an SDK subprocess integration, not a native Responses
        transport. Native provider fallback excludes it, and OpenAI/xAI/OpenRouter
        account usage ranking does not select its credentials. Its own login,
        supported SDK behavior and subscription policy remain authoritative.</p>
        <h2 id="billing">Account usage and billing</h2>
        <h3 id="usage-output">Read usage, pacing and reset information</h3>
        <pre><code>/usage</code></pre>
        <p>For direct OpenAI accounts this fetches ChatGPT Codex usage windows.
        Each account block can show its short account identifier, plan, remaining
        percentage, window duration and reset countdown/time. A line such as
        <code>20% reserve</code> means 80% of that reported window is used, not that
        20% of your monetary balance remains. At zero reserve the wording becomes
        <code>exhausted until reset in</code>. <code>pacing until</code> is the local
        account cooldown, which can differ from the provider's reset time.</p>
        <p><code>couldn't load usage:</code> reports a fetch error; <code>no rate-limit
        windows</code> means the response supplied none. Neither means unlimited
        usage or zero remaining quota. Run <code>/usage</code> again for a new
        snapshot if an earlier display is stale; provider reporting itself can lag.</p>
        <p>Gateway sessions report that usage is organization-managed. Claude Code
        reports its account/subscription and directs you to <code>claude /status</code>
        for live limits. xAI, OpenRouter and Gemini do not expose account usage through
        this command's ChatGPT window API. Missing OpenAI credentials or an empty pool
        are reported explicitly; connect an account instead of treating missing data
        as available capacity.</p>
        <p>A subscription login and an API key can use different quotas and billing systems.
        Check <code>/usage</code> before a long task. Provider account cooldowns can delay work;
        changing a model is not proof that a rate limit or billing restriction has been removed.</p>
        <p>For Claude Code, check the provider's current SDK subscription policy linked in
        <a href="/getting-started/authentication/#claude-code">Authentication</a>. Technical integration
        does not override provider terms.</p>
        <h2 id="custom-connections">Custom endpoints</h2>
        <p>Configure custom Responses-compatible connections in <code>~/.haskell-agent/models.json</code>.
        These are model-catalog connections, not additional built-in values for <code>--provider</code>.
        Select the configured model with <code>--model</code>.</p>
        <p>The <a href="/customization/models/">Models guide</a> includes a complete JSON example and
        explains endpoint compatibility, secret environment variables, and context limits.</p>
    |]
    }
