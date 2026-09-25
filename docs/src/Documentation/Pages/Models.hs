{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.Models (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/customization/models/"
    , pageTitle = "Models"
    , pageDescription = "Select models, adjust reasoning effort, and connect a custom Responses-compatible endpoint."
    , pageGroup = "Customization"
    , pageBody = [hsx|
        <h2 id="select-a-model">Select a model</h2>
        <pre><code>/model</code></pre>
        <p>Choose from the available catalog, or provide a model identifier with
        <code>/model NAME</code> or <code>agent-cli --model NAME</code>. Use <code>/session-info</code> to verify the
        active choice.</p>
        <p><code>/effort</code> displays or changes reasoning effort where supported. Accepted command
        values include <code>none</code>, <code>low</code>, <code>medium</code>, <code>high</code>, <code>xhigh</code>, and <code>max</code>; individual
        models may support only a subset.</p>
        <p>Changing models does not mean all providers expose identical tools. The
        provider determines authentication and transport; the model's dialect
        determines its prompts and tool interface.</p>
        <h2 id="reasoning-controls">Reasoning effort and visible reasoning</h2>
        <pre><code class="language-sh">agent-cli --provider xai --effort high --show-raw-reasoning</code></pre>
        <p>Without an explicit or resumed effort, xAI defaults to <code>high</code>,
        OpenAI, OpenRouter and Gemini to <code>medium</code>, and Claude Code to
        <code>xhigh</code>. <code>/effort</code> opens the supported-choice picker;
        <code>/effort high</code> changes the current session. Grok's dialect excludes
        <code>max</code>: an interactive request for it is rejected without changing
        the current effort. Startup normalizes an unsupported Grok effort to
        <code>high</code>. An endpoint can still reject a level accepted by the
        harness if its own model does not implement it.</p>
        <p>By default the viewport shows provider-supplied reasoning summaries.
        <code>--show-raw-reasoning</code> also includes text content supplied in
        reasoning items. For example, a supplied summary “Checking the tests” is
        visible in either mode; additional supplied reasoning text appears only
        with the flag. This display option neither requests disclosure of hidden
        reasoning nor reconstructs content absent from the response. If the provider
        sends no such content, there is nothing extra to display.</p>
        <h2 id="automatic-compaction">Automatic compaction thresholds</h2>
        <p><code>--compact-threshold N</code> sets a positive token threshold for
        providers with automatic compaction. It is not a larger context window,
        a billing cap, or a guarantee that an oversized initial prompt can fit.
        Current provider behavior differs:</p>
        <table><thead><tr><th>Provider</th><th>Default and bounds</th></tr></thead><tbody>
            <tr><td>OpenAI</td><td>Current Codex metadata defaults to 244,800 tokens; the override is capped at the effective context window, currently 258,400 tokens.</td></tr>
            <tr><td>xAI</td><td>80% of the resolved context window for Grok 4.5/4.6, 85% for other model names. The override is capped at the context window and bounded below by one.</td></tr>
            <tr><td>Claude Code</td><td>80% after reserving up to 20,000 output tokens, with a further up-to-13,000-token safety margin on compaction input. SDK-reported context takes precedence over catalog/default context. An override cannot exceed the safe compaction-input limit.</td></tr>
            <tr><td>Gemini / OpenRouter</td><td>The shared HTTP runtime wires manual compaction and overflow protection, not this automatic threshold. Use <code>/compact</code>; do not rely on the flag to schedule it.</td></tr>
        </tbody></table>
        <pre><code class="language-sh">agent-cli --provider xai --compact-threshold 100000</code></pre>
        <p>For a model whose resolved window exceeds 100,000 tokens, this requests
        compaction around that threshold instead of waiting for its percentage
        default. Continue the task normally; accumulated context, pending inputs
        and tool continuations determine when compaction is needed, not a message
        count. A lower threshold can compact more often and spend more on summaries.
        With a 200,000-token Claude window, the default calculation instead yields
        144,000 tokens and the safe input ceiling is 167,000. Custom portable models
        need their actual <code>context_window</code>; increasing a catalog number
        does not increase the endpoint's capacity.</p>
        <h2 id="add-a-local-or-hosted-model">Add a local or hosted model</h2>
        <p>The model picker merges its shipped catalog with:</p>
        <pre><code>~/.haskell-agent/models.json</code></pre>
        <p>The built-in <code>add-model</code> skill can configure this file. For example:</p>
        <pre><code>$add-model Configure the Responses-compatible model at http://localhost:11434/v1.</code></pre>
        <p>For manual configuration, this example assumes the server actually implements
        streaming <code>POST /v1/responses</code> and serves the specified model:</p>
        <pre><code class="language-json" data-config-schema="models">{modelConfiguration}</code></pre>
        <p>Then select:</p>
        <pre><code class="language-sh">agent-cli --model local-coder</code></pre>
        <p>This does not install or start an inference server. A Chat Completions endpoint
        alone is not sufficient for this configuration.</p>
        <h2 id="authentication-and-context-limits">Authentication and context limits</h2>
        <p>For an authenticated endpoint, set <code>"api_key_env": "MY_MODEL_API_KEY"</code> on the
        connection, supply that environment variable to the process, and remove
        <code>"api_key_optional": true</code>. Do not store the key itself in <code>models.json</code>.</p>
        <p>Set <code>context_window</code> to the endpoint's documented token limit. Inference can
        work without it, but portable-model compaction refuses to guess a limit.</p>
        <p>User model entries with an existing <code>id</code> replace shipped entries; new entries
        are appended. Built-in connection names are reserved. Custom connections are
        selected manually and are not used for automatic billing fallback.</p>
        <p>Supported dialects are <code>codex</code>, <code>grok-build</code>, and <code>generic-responses</code>. Choose a
        dialect the endpoint and model can actually support, not just the tool names
        you prefer.</p>
        <h2 id="catalog-reference">Catalog reference</h2>
        <p>The file is a JSON object with required integer <code>version: 1</code>,
        an optional <code>connections</code> object (default empty), and an optional
        <code>models</code> array (default empty). Use strict JSON, not comments or trailing commas.
        Restart the CLI after editing the catalog; invalid entries are reported at startup.</p>
        <h3 id="connection-fields">Custom Responses connection fields</h3>
        <table>
            <thead><tr><th>Field</th><th>Type and default</th><th>Meaning</th></tr></thead>
            <tbody>
                <tr><td><code>api</code></td><td>Required string</td><td>Use <code>responses</code> for a custom endpoint.</td></tr>
                <tr><td><code>base_url</code></td><td>Required string</td><td>HTTP or HTTPS API base URL, including its version prefix when required.</td></tr>
                <tr><td><code>api_key_env</code></td><td>Optional string</td><td>Name of the environment variable holding the secret, not its value. Required unless keys are optional.</td></tr>
                <tr><td><code>api_key_optional</code></td><td>Boolean; <code>false</code></td><td>Permit a connection without an API key.</td></tr>
                <tr><td><code>request_timeout_seconds</code></td><td>Positive integer; <code>600</code></td><td>Request timeout in seconds.</td></tr>
            </tbody>
        </table>
        <p><code>provider</code> belongs to built-in connection declarations, not custom Responses
        routing. Do not redefine reserved connections: <code>openai</code>, <code>xai</code>,
        <code>openrouter</code>, <code>meta</code>, <code>gemini</code>, <code>claude-code</code>,
        or <code>organization-gateway</code>.</p>
        <h3 id="model-fields">Model fields</h3>
        <table>
            <thead><tr><th>Field</th><th>Type and default</th><th>Meaning</th></tr></thead>
            <tbody>
                <tr><td><code>id</code></td><td>Required string</td><td>Local selector used by <code>/model</code> and <code>--model</code>; nonempty with no whitespace.</td></tr>
                <tr><td><code>connection</code></td><td>Required string</td><td>Existing connection name.</td></tr>
                <tr><td><code>model</code></td><td>String; defaults to <code>id</code></td><td>Model identifier sent to a custom endpoint. Built-in and gateway entries cannot remap it.</td></tr>
                <tr><td><code>dialect</code></td><td>Required string</td><td><code>codex</code>, <code>grok-build</code>, or <code>generic-responses</code>; must be compatible with the connection.</td></tr>
                <tr><td><code>context_window</code></td><td>Optional positive integer</td><td>Documented context size in tokens; required for reliable portable-model compaction.</td></tr>
                <tr><td><code>label</code></td><td>Optional string</td><td>Additional catalog label.</td></tr>
                <tr><td><code>reasoning_efforts</code></td><td>Optional array of strings</td><td>Nonempty, unique supported efforts: <code>none</code>, <code>low</code>, <code>medium</code>, <code>high</code>, <code>xhigh</code>, <code>max</code>.</td></tr>
                <tr><td><code>default_reasoning_effort</code></td><td>Optional string</td><td>Must appear in the entry's <code>reasoning_efforts</code>.</td></tr>
                <tr><td><code>supports_async_tool_calls</code></td><td>Boolean; <code>false</code></td><td>Declare support for asynchronous tool calls only when the endpoint supports them. Disabled for gateway metadata.</td></tr>
                <tr><td><code>default</code></td><td>Boolean; <code>false</code></td><td>Catalog default selection marker.</td></tr>
                <tr><td><code>fallback_priority</code></td><td>Optional nonnegative integer</td><td>Fallback ordering metadata. Does not make custom connections eligible for automatic billing fallback.</td></tr>
            </tbody>
        </table>
        <h2 id="gateway-models">Organization gateway aliases</h2>
        <p>For each built-in provider, the merged catalog must contain exactly one
        default model. Replacing a shipped default with an entry that omits
        <code>default: true</code> can invalidate the catalog; marking another default
        without clearing the existing one also fails. This catalog fallback does not
        erase an explicit launch choice or a remembered
        <a href="/reference/persisted-settings/#inheritance">project model</a>.</p>
        <p>Asynchronous tool capability is resolved against the exact connection and wire
        model. If several aliases match that transport model, all must explicitly enable
        <code>supports_async_tool_calls</code>; ambiguity fails closed. Gateway aliases
        never inherit this capability because their name resembles a direct model.
        This is a transport capability declaration, not permission to run tools without approval.</p>
        <p>For a private gateway alias, add a model entry with <code>connection</code> set to
        <code>organization-gateway</code>; do not define that connection. The gateway's live
        <code>/v1/models</code> response remains authoritative. Metadata applies only when the same
        alias is advertised, cannot change the wire name, and does not add the alias to direct-provider
        pickers.</p>
        <pre><code class="language-json" data-config-schema="models">{"{\"version\":1,\"models\":[{\"id\":\"company-coder\",\"connection\":\"organization-gateway\",\"dialect\":\"generic-responses\",\"context_window\":131072,\"label\":\"company\"}]}" :: Text}</code></pre>
        <h2 id="async-contract">Asynchronous tool protocol</h2>
        <p><code>supports_async_tool_calls</code> is a protocol capability declaration,
        not a speed setting. The Responses wire format carries an <code>async</code>
        boolean on supported tool definitions, calls and outputs. A participating
        endpoint must preserve those fields and call identities, stream complete
        asynchronous calls, and accept their later results rather than requiring
        every tool to finish before generation can continue.</p>
        <p>The harness can admit an asynchronous call during streaming and execute
        it through the tool scheduler while generation continues. Calls still need
        tool-level asynchronous support and normal approval. Once an async call
        has been observed, transport recovery must not blindly replay the response
        and duplicate its effects. Keep the flag false unless the exact endpoint
        and model implement this contract; support for ordinary function calling
        or <code>parallel_tool_calls</code> alone is insufficient.</p>
        <h2 id="provider-fallback">Automatic fallback eligibility and order</h2>
        <p><code>fallback_priority</code> is an optional nonnegative integer;
        smaller values rank first. Only built-in connections <code>openai</code>,
        <code>xai</code>, <code>openrouter</code> and <code>gemini</code> participate.
        Custom connections are manual-only, even if given a priority. Claude Code
        is excluded both as a source of automatic fallback and as a destination.</p>
        <p>A structured model-access failure first considers strictly lower-ranked
        models on the current provider, then the highest-ranked model for each
        other eligible provider. Equal-priority models retain catalog order, but
        are not a lower-ranked same-provider recovery. Provider-wide account/quota
        failures skip same-provider model changes. Providers already observed as
        exhausted are excluded. A bare failure does not make every model eligible:
        recovery requires a recognized provider-unavailable error.</p>
        <p>Review the proposed provider/model and its account before continuing.
        A different provider can change the recipient of conversation data and
        the subscription/API billing route. Priority is not a promise of access,
        equivalent capabilities, free usage or permission to bypass an account limit.</p>
        <h2 id="session-title-model">Session title model</h2>
        <p>Automatic session naming has its own model selection, independent of the coding model.
        Open its picker or restore automatic selection with:</p>
        <pre><code>{"/title-model\n/title-model --auto" :: Text}</code></pre>
        <p>Pin a catalog model with <code>/title-model NAME</code>, or choose on-device Apple
        Intelligence with <code>/title-model apple-foundationmodel</code>. The selection persists as
        <code>titleModel</code> in <code>~/.haskell-agent/settings.json</code>. A pinned provider model
        is used only when its provider matches the current session.</p>
        <p>On supported macOS systems, automatic selection first tries Apple Intelligence through
        <code>apple-session-title</code>. The Darwin Nix package builds that helper with Xcode and
        sets <code>HASKELL_AGENT_APPLE_SESSION_TITLE</code>. The macOS bundle installs it on
        <code>PATH</code>. The CLI does not compile it. If unavailable
        or unsuccessful, naming falls back to the provider's inexpensive model. Other systems use
        provider selection directly. This fallback is for naming, not a switch of the coding model.
        The same helper also decides whether a plain follow-up steers the running fullscreen turn
        or waits until that turn finishes.</p>
        <h2 id="catalog-errors">Catalog errors and recovery</h2>
        <table>
            <thead><tr><th>Error or symptom</th><th>Action</th></tr></thead>
            <tbody>
                <tr><td><code>references unknown connection</code></td><td>Match the model's connection exactly to a declared or shipped connection name.</td></tr>
                <tr><td><code>requires api_key_env unless api_key_optional is true</code></td><td>Declare a secret variable for authenticated endpoints; use optional keys only for a server that permits them.</td></tr>
                <tr><td><code>default_reasoning_effort must be listed in reasoning_efforts</code></td><td>Add the supported default to the nonempty efforts array, or remove the override.</td></tr>
                <tr><td>HTTP 404 from a custom endpoint</td><td>Check the base URL and confirm support for <code>POST /v1/responses</code>, not only Chat Completions.</td></tr>
                <tr><td>Generation works, compaction fails</td><td>Supply the server's actual positive <code>context_window</code>; do not guess a larger limit.</td></tr>
            </tbody>
        </table>
        <h2 id="example-check-a-custom-model">Example: check a custom model connection</h2>
        <ol>
            <li>Confirm your inference server is running and implements the Responses API.</li>
            <li>Adapt the catalog example to its model name, URL, and documented context limit.</li>
            <li>Start <code>agent-cli --model local-coder</code>, then inspect <code>/session-info</code>.</li>
            <li>Send <code>Reply with a short greeting. Do not use tools.</code> before asking for repository work.</li>
        </ol>
        <p><strong>Expected result:</strong> a response through the selected connection.
        A connection error calls for checking the server and URL; an authentication
        error calls for checking the environment-variable reference. Neither is
        fixed by changing tool approvals. Once basic generation works, try a
        read-only file inspection to check tool compatibility separately.</p>
    |]
    }

modelConfiguration :: Text
modelConfiguration = "{\n\
    \  \"version\": 1,\n\
    \  \"connections\": {\n\
    \    \"local-inference\": {\n\
    \      \"api\": \"responses\",\n\
    \      \"base_url\": \"http://localhost:11434/v1\",\n\
    \      \"api_key_optional\": true,\n\
    \      \"request_timeout_seconds\": 600\n\
    \    }\n\
    \  },\n\
    \  \"models\": [\n\
    \    {\n\
    \      \"id\": \"local-coder\",\n\
    \      \"connection\": \"local-inference\",\n\
    \      \"model\": \"qwen2.5-coder:32b\",\n\
    \      \"dialect\": \"generic-responses\",\n\
    \      \"context_window\": 32768,\n\
    \      \"label\": \"local\"\n\
    \    }\n\
    \  ]\n\
    \}"
