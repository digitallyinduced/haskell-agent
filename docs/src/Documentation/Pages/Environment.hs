{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.Environment (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/reference/environment/"
    , pageTitle = "Environment variables"
    , pageDescription = "Configure provider credentials, transport overrides, browser launching, and local runtime endpoints."
    , pageGroup = "Reference"
    , pageBody = [hsx|
        <p>Set variables in the process that starts the agent. A change in another
        terminal does not update a running process. Prefer account management for
        credentials and <code>models.json</code> for new model connections.</p>
        <p>Never put tokens in prompts, issue reports, example JSON, or commands saved
        in shell history. Endpoint overrides can send credentials and conversation
        data to another server; use only endpoints you operate or trust.</p>
        <h2 id="credentials">Credential inputs</h2>
        <table><thead><tr><th>Variable</th><th>Purpose</th></tr></thead><tbody>
            <tr><td><code>CODEX_ACCESS_TOKEN</code></td><td>External OpenAI coding bearer token; does not supply a refresh token</td></tr>
            <tr><td><code>CODEX_ACCOUNT_ID</code></td><td>Explicit account identity accompanying that token</td></tr>
            <tr><td><code>CODEX_ID_TOKEN</code></td><td>Alternative identity information used to derive account identity</td></tr>
            <tr><td><code>CODEX_AUTH_JSON</code></td><td>Serialized external OpenAI authentication record; prefer importing an existing valid login rather than manufacturing OAuth fields</td></tr>
            <tr><td><code>CODEX_HOME</code></td><td>Directory containing auth.json; default ~/.codex</td></tr>
            <tr><td><code>OPENAI_OAUTH_CLIENT_ID</code></td><td>Override the OAuth application client ID; normally leave unset and use the shipped application</td></tr>
            <tr><td><code>GROK_AUTH_JSON</code></td><td>External Grok OAuth record; when valid, preferred over GROK_ACCESS_TOKEN for the environment source</td></tr>
            <tr><td><code>GROK_ACCESS_TOKEN</code></td><td>External Grok bearer token without a refresh grant</td></tr>
            <tr><td><code>XAI_OAUTH_CLIENT_ID</code></td><td>Override xAI OAuth application client ID; normally leave unset</td></tr>
            <tr><td><code>OPENROUTER_API_KEY</code></td><td>External API-billed OpenRouter credential</td></tr>
            <tr><td><code>GOOGLE_API_KEY</code></td><td>External Gemini API credential; preferred over GEMINI_API_KEY</td></tr>
            <tr><td><code>GEMINI_API_KEY</code></td><td>Alternative external Gemini API credential</td></tr>
            <tr><td><code>MODEL_API_KEY</code></td><td>Secret named by the shipped direct Meta model connection</td></tr>
            <tr><td><code>OPENAI_API_KEY</code></td><td>Direct auxiliary OpenAI authentication such as dictation; exporting this alone is not the coding-account selector</td></tr>
        </tbody></table>
        <p>Enabled managed credentials can take precedence over environment sources.
        OpenAI combines eligible managed and external accounts and suppresses duplicate
        account identities; this is not a simple last-variable-wins rule. See
        <a href="/reference/providers/">provider authentication and billing</a>.</p>
        <h2 id="transport-overrides">Direct-provider transport overrides</h2>
        <p>These variables change existing provider clients, not the model catalog.
        Nonempty string overrides are accepted as supplied. Integer timeout strings
        must parse completely; invalid or empty values fall back to the client's default.
        Use positive timeouts rather than relying on transport-specific behavior of zero
        or negative values. The default is 600 seconds for the clients below.</p>
        <table><thead><tr><th>Variable</th><th>Default / interpretation</th></tr></thead><tbody>
            <tr><td><code>OPENROUTER_BASE_URL</code></td><td>https://openrouter.ai/api/v1</td></tr>
            <tr><td><code>OPENROUTER_MODEL_MAP</code></td><td>Empty; comma-separated exact source=target mappings</td></tr>
            <tr><td><code>OPENROUTER_DEFAULT_MODEL</code></td><td>openai/gpt-5.1; fallback when the request has no usable OpenRouter slug</td></tr>
            <tr><td><code>OPENROUTER_TIMEOUT_SECONDS</code></td><td>600; provider request timeout</td></tr>
            <tr><td><code>OPENROUTER_HTTP_REFERER</code></td><td>Absent; optional HTTP-Referer attribution header</td></tr>
            <tr><td><code>OPENROUTER_APP_TITLE</code></td><td>Absent; optional X-Title attribution header</td></tr>
            <tr><td><code>XAI_GROK_BASE_URL</code></td><td>Native xAI API base; leave unset for normal direct access</td></tr>
            <tr><td><code>XAI_GROK_MODEL_MAP</code></td><td>Empty; comma-separated exact source=target mappings</td></tr>
            <tr><td><code>XAI_GROK_DEFAULT_MODEL</code></td><td>grok-4.6</td></tr>
            <tr><td><code>XAI_GROK_TIMEOUT_SECONDS</code></td><td>600; provider request timeout</td></tr>
            <tr><td><code>XAI_GROK_CLIENT_VERSION</code></td><td>Shipped client identity; override only for a known compatibility requirement</td></tr>
            <tr><td><code>GEMINI_BASE_URL</code></td><td>Direct Gemini API endpoint; distinct from subscription Code Assist</td></tr>
            <tr><td><code>GEMINI_CODE_ASSIST_BASE_URL</code></td><td>https://cloudcode-pa.googleapis.com/v1internal</td></tr>
            <tr><td><code>GEMINI_DEFAULT_MODEL</code></td><td>gemini-3.7-flash</td></tr>
            <tr><td><code>GEMINI_TIMEOUT_SECONDS</code></td><td>600; provider request timeout</td></tr>
        </tbody></table>
        <p>Model-map entries trim whitespace around both sides and ignore malformed or
        empty entries. Duplicate source keys retain the final entry. For example,
        <code>source-a=target-a,source-b=target-b</code> describes two exact replacements,
        not wildcard rules. Do not use mapping to claim a model has another model's
        capabilities or context limit.</p>
        <p>Organization-gateway xAI routing deliberately preserves advertised model
        names, uses the gateway's native endpoint and disables redirects. Direct
        xAI environment overrides do not redirect that authenticated gateway route.</p>
        <pre><code class="language-sh">{"OPENROUTER_TIMEOUT_SECONDS=900 agent-cli --provider openrouter" :: Text}</code></pre>
        <p>Use <code>/session-info</code> and a small request to verify the intended
        provider. Increasing a timeout does not resolve an invalid key, unknown model,
        incompatible endpoint or exhausted account quota. Remove the override to return
        to the shipped transport default.</p>
        <h2 id="helpers">Local helpers and browser selection</h2>
        <table><thead><tr><th>Variable</th><th>Behavior</th></tr></thead><tbody>
            <tr><td><code>CLAUDE_CODE_EXECUTABLE</code></td><td>Select the Claude Code executable for authentication/runtime discovery; use a trusted installed program</td></tr>
            <tr><td><code>HASKELL_AGENT_APPLE_SESSION_TITLE</code></td><td>Select the on-device title helper; normal discovery uses PATH or a supported local build</td></tr>
            <tr><td><code>BROWSER</code></td><td>Single executable name or path, launched with the authorization URL as one argument; not a shell command with flags</td></tr>
            <tr><td><code>XAI_STT_LANGUAGE</code></td><td>Dictation language; defaults to English, for example pt for Portuguese</td></tr>
        </tbody></table>
        <p>Without <code>BROWSER</code>, browser launching tries <code>open</code> and
        then <code>xdg-open</code>. If you supply a browser executable and it cannot
        launch, correct or unset the variable; an arbitrary command string will not
        be interpreted by a shell. Remote/headless sessions still need a way to
        complete the provider's browser authorization.</p>
        <h2 id="runtime-paths">Local runtime paths</h2>
        <p>These are advanced local-process coordination controls. Use the same values
        in processes that must observe or contact one another. Do not point them at
        a shared, untrusted directory; these endpoints are not a network deployment API.</p>
        <table><thead><tr><th>Variable</th><th>Default / purpose</th></tr></thead><tbody>
            <tr><td><code>HASKELL_AGENT_OBSERVATION_DIRECTORY</code></td><td>~/.haskell-agent/observation; session observation socket directory</td></tr>
            <tr><td><code>HASKELL_AGENT_INBOX_DIRECTORY</code></td><td>~/.haskell-agent/inbox; session inbox socket directory</td></tr>
            <tr><td><code>HASKELL_AGENT_EXECUTABLE</code></td><td>Override the agent executable used for managed external process launches</td></tr>
        </tbody></table>
        <p>Leave directory overrides unset instead of supplying an empty value.
        Observation and inbox services are ancillary; failing to create their endpoint
        can leave execution running without that coordination facility. Check the chosen
        directory and its permissions rather than repeatedly starting duplicate agents.</p>
        <h2 id="mcp-environment">MCP entry environment is separate</h2>
        <p><code>MCP_ACCESS_TOKEN</code> and <code>MCP_OAUTH_TOKEN_FILE</code> are read
        from a server entry's <code>env</code> map. They are not interchangeable with a
        provider API key. Use the <a href="/customization/mcp/#remote-authentication">MCP
        authentication reference</a> for managed OAuth and legacy token-file behavior.</p>
    |]
    }
