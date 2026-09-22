{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.Authentication (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/getting-started/authentication/"
    , pageTitle = "Authentication"
    , pageDescription = "Connect supported provider accounts and understand model selection and billing."
    , pageGroup = "Start here"
    , pageBody = [hsx|
        <p>Haskell Agent supports OpenAI, xAI, OpenRouter, Gemini, and Claude Code.
        Available models and billing depend on the credential and provider you select.
        A provider subscription and an API key are not interchangeable billing methods.</p>
        <h2 id="connect-interactively">Connect interactively</h2>
        <p>Start <code>agent-cli</code>, then open:</p>
        <pre><code>/login</code></pre>
        <figure>
            <a href="/login-dashboard.svg"><img src="/login-dashboard.svg" alt="Login dashboard with zero connected credentials; press a to add a provider account or g for gateway login" width="1120" height="160" /></a>
            <figcaption>Actual standalone <code>agent-cli login</code> output from build <code>9a20f72</code>, captured without connected accounts and rendered as SVG. The fullscreen login view uses different labels. Open the image for full size.</figcaption>
        </figure>
        <p>Press <code>a</code> to open the provider chooser. Select the intended provider
        before proceeding to its authentication method. Press <code>Esc</code> to return
        without connecting an account.</p>
        <figure>
            <a href="/provider-chooser.svg"><img src="/provider-chooser.svg" alt="Provider chooser with OpenAI, xAI, OpenRouter, Gemini and Claude Code; arrow keys select and Enter continues" width="760" height="240" /></a>
            <figcaption>Provider chooser from the same captured build. Fonts and colors are normalized; newer builds may differ. <a href="/interactive-terminal-captures.txt">Capture provenance and accessible transcripts.</a></figcaption>
        </figure>
        <p>Use the login/account interface to manage provider connections. Then open
        <code>/model</code> and choose the model for your work. You can also request a configuration
        change without adding it to the coding conversation:</p>
        <pre><code>/meta connect my Grok account</code></pre>
        <p>Meta Console previews supported configuration changes before applying them.
        Enter secrets only in the host's masked prompts, not in ordinary chat messages.</p>
        <h2 id="account-dashboard">Understand the account dashboard</h2>
        <p>Each account row identifies the provider, account label and identity, whether its source
        is managed or external, whether it is enabled, its billing mode, and available usage information.
        Select an account to inspect its details rather than relying on a model's description of its identity.</p>
        <table>
            <thead><tr><th>Action</th><th>Effect</th></tr></thead>
            <tbody>
                <tr><td>Refresh usage</td><td>Fetch current provider limits for the account. It does not reset the provider's quota.</td></tr>
                <tr><td>Import credential</td><td>Copy an eligible external credential into the managed store; the external source remains a separate file or environment setting.</td></tr>
                <tr><td>Disable credential</td><td>Keep the managed credential stored, but exclude it from selection.</td></tr>
                <tr><td>Enable credential</td><td>Make a stored managed credential available again.</td></tr>
                <tr><td>Disconnect credential</td><td>Delete the managed copy. This is not a provider-side revocation and does not remove an external credential file.</td></tr>
            </tbody>
        </table>
        <p>External credentials cannot be enabled or deleted as if they were managed records.
        Change their originating environment or file, or import them to use managed-account controls.
        A credential may remain discoverable after disconnecting its managed copy if the external
        source still exists.</p>
        <h2 id="account-walkthrough">Connect, verify, and retire an account</h2>
        <ol>
            <li>Open <code>/login</code> and add the intended provider connection. Complete browser
            authorization or the masked credential prompt.</li>
            <li>Inspect the resulting account row. Confirm the account identity and billing method
            before sending work.</li>
            <li>Select a model belonging to that provider with <code>/model</code>, then run
            <code>/session-info</code> and a short generation-only request.</li>
            <li>Use <code>/usage</code> to inspect available account usage; provider reporting can be unavailable.</li>
            <li>When retiring a managed account, disable it first if you want a reversible change.
            Disconnect it only when you intend to delete the stored copy.</li>
        </ol>
        <p>If a secret was exposed, revoke it at the provider as well. Deleting a local copy does
        not invalidate a copied token. Do not include auth files, environment values, or credential
        store contents in a bug report.</p>
        <h2 id="select-a-provider-explicitly">Select a provider explicitly</h2>
        <p>To verify your connection, select a model, run <code>/session-info</code>,
        then send a small request:</p>
        <pre><code>Reply with one sentence confirming you received this message. Do not use tools.</code></pre>
        <p><strong>Expected result:</strong> the session details identify the intended model
        and the request receives a response. This checks basic generation, not every
        tool capability. A model's claim about its own identity is not a substitute
        for checking the session details. If authentication fails, return to
        <code>/login</code>; do not paste a credential into the conversation.</p>
        <pre><code class="language-sh">{"agent-cli --provider openai\nagent-cli --provider xai\nagent-cli --provider openrouter\nagent-cli --provider gemini" :: Text}</code></pre>
        <p>Provider credential detection checks OpenAI, xAI, OpenRouter, then Gemini.
        Claude Code is selected explicitly rather than through automatic detection.
        See the <a href="/reference/providers/">provider reference</a> for each provider's
        credential sources and account-selection rules.</p>
        <h2 id="gemini">Gemini</h2>
        <p>Choose a Gemini model in <code>/model</code>. When no Gemini account is connected, the CLI
        opens Google sign-in in your browser and stores the OAuth credential in its
        managed credential store. This account flow does not require an API key.</p>
        <p>For Google AI Studio API billing, supply <code>GOOGLE_API_KEY</code> in the process
        environment (<code>GEMINI_API_KEY</code> is also supported), then select the provider:</p>
        <pre><code class="language-sh">agent-cli --provider gemini</code></pre>
        <h2 id="claude-code">Claude Code</h2>
        <p>Install and authenticate Claude Code first:</p>
        <pre><code class="language-sh">{"claude auth login\nagent-cli --provider claude-code --model sonnet" :: Text}</code></pre>
        <p>The harness uses the Claude Code SDK integration, including Claude's built-in
        tools and additional harness tools. Provider subscription usage rules still
        apply. Consult Anthropic's current
        <a href="https://support.claude.com/en/articles/15036540-use-the-claude-agent-sdk-with-your-claude-plan">SDK subscription policy</a>;
        technical compatibility is not a substitute for required provider approval.</p>
        <h2 id="check-credentials-and-usage">Check credentials and usage</h2>
        <table>
            <thead><tr><th>Command</th><th>Purpose</th></tr></thead>
            <tbody>
                <tr><td><code>/session-info</code></td><td>Inspect the active model, tools, and session details</td></tr>
                <tr><td><code>/usage</code></td><td>Show connected-account usage and reset times</td></tr>
                <tr><td><code>/reload-auth</code></td><td>Re-read provider credentials after an external change</td></tr>
                <tr><td><code>/model</code></td><td>Select a different model</td></tr>
            </tbody>
        </table>
        <p>Credential failover observes account cooldowns and avoids silently converting
        subscription usage into API-credit spending. If available accounts cannot take
        over, an interactive session can wait for the provider's reset time; press
        <code>Esc</code> to cancel that wait.</p>
        <h2 id="reload-and-recovery">Reload and recover credentials</h2>
        <table><thead><tr><th>Active authentication</th><th>What <code>/reload-auth</code> does</th></tr></thead><tbody>
            <tr><td>Token-provider-backed accounts, including xAI/OpenRouter/Gemini</td><td>Forces a fresh credential checkout by rejecting the currently active credential; can select a different eligible account. Success reports <code>auth reloaded</code> and the selected account.</td></tr>
            <tr><td>Fixed OpenAI WebSocket authentication</td><td>Cannot replace process-bound auth. Refresh <code>~/.codex/auth.json</code> and restart. OAuth pools already rotate on handshake failure.</td></tr>
            <tr><td>Claude Code</td><td>Rechecks the SDK's authentication status and reports its account label; it does not perform a new Claude login.</td></tr>
        </tbody></table>
        <p><code>/reload-auth</code> re-reads credentials after an external change. It cannot alter
        the environment inherited by an already-running process: after changing an exported variable
        in your shell, start a new CLI process from that shell. For a changed auth file, reload is the
        appropriate first step. If a browser refresh token has expired or been revoked, complete login
        again rather than repeatedly retrying the same failed request.</p>
        <p>Manual reload is different from automatic authentication recovery.
        Provider transports can reload or fail over after classified credential
        rejection, subject to their retry budget and replay-safety boundary.
        This is not an unlimited retry loop or permission to repeat a tool with an
        uncertain external outcome. If recovery fails, fix the credential source
        and recheck the selected account instead of repeatedly issuing reload.</p>
        <p>A quota error and an authentication error have different remedies. A 401 calls for
        inspecting or renewing the selected credential; a 429 calls for inspecting usage and reset
        times. Account failover stays within billing constraints instead of silently spending API
        credits when subscription usage is exhausted.</p>
        <p>For a custom endpoint, configure an environment-variable reference in the
        <a href="/customization/models/">model catalog</a> rather than putting the secret itself
        in the catalog.</p>
    |]
    }
