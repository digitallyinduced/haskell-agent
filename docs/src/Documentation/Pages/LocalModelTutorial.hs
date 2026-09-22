{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.LocalModelTutorial (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/tutorials/local-model/"
    , pageTitle = "Connect a local model"
    , pageDescription = "Start Ollama with Nix, download a small model, register its Responses endpoint, and verify generation and tools."
    , pageGroup = "Tutorials"
    , pageBody = [hsx|
        <p>This walkthrough starts a local Ollama server, downloads a small model, and connects
        it to Haskell Agent. Each stage has a separate verification: server readiness, generated
        text, model registration, and an actual file-reading tool call.</p>
        <p><strong>Verification scope:</strong> Ollama <code>0.32.13</code> with
        <code>qwen3:0.6b</code> passed startup, local streaming generation, and a
        function-call/result replay check on Apple silicon on 22 September 2026.
        The separate Haskell Agent file-reading exercise below was not executed in that
        verification. API success is not an end-to-end harness verification.</p>
        <h2 id="prerequisites">Prerequisites</h2>
        <ul>
            <li>Haskell Agent installed and Nix flakes enabled.</li>
            <li>Two terminals: one remains occupied by the server.</li>
            <li>Internet access for Nix packages and the initial model download.</li>
            <li>About 523 MB for the example model weights, additional space for Nix packages,
            and sufficient free memory for the model plus a 32,768-token context.</li>
        </ul>
        <p>The example uses <code>qwen3:0.6b</code> to keep the initial download small.
        It is a connection and tool-protocol exercise, <strong>not a recommendation for
        autonomous coding</strong>. A small model can answer a greeting while still failing
        to select or use tools correctly. Use a more capable model after verifying the
        connection, and review its memory requirements before downloading it.</p>
        <h2 id="start-ollama">1. Obtain and start Ollama</h2>
        <p>In terminal A, enter this Nix environment. The revision pins Ollama to
        <code>0.32.13</code> rather than silently following a changing package registry:</p>
        <pre><code class="language-sh">{nixEnvironment}</code></pre>
        <p>Then start the server in the foreground:</p>
        <pre><code class="language-sh">{serverStartup}</code></pre>
        <p>Leave terminal A open. Expected result: the server reports that it is listening
        on <code>127.0.0.1:11434</code>. These settings keep it on loopback, disable Ollama's
        cloud features, select one concurrent inference request, and explicitly configure
        the context size used later in the catalog. They do not install a background service.</p>
        <p>If the address is already in use, inspect your existing Ollama process instead of
        starting a second server or terminating an unrelated process. Its existing context
        and cloud settings may differ. Do not bind an unauthenticated model endpoint to a
        public network interface.</p>
        <h2 id="download-model">2. Download and inspect the model</h2>
        <p>In terminal B, enter the same Nix environment:</p>
        <pre><code class="language-sh">{nixEnvironment}</code></pre>
        <p>Point the client at the server, confirm its version, and download the model:</p>
        <pre><code class="language-sh">{modelDownload}</code></pre>
        <p>Expected result: <code>ollama pull</code> completes successfully, <code>ollama list</code>
        includes <code>qwen3:0.6b</code>, and <code>ollama show</code> lists the model's
        capabilities and context limit. The download is stored by the server, normally under
        <code>~/.ollama/models</code>; leaving the Nix shell does not delete it. The upstream
        model tag can change independently of the pinned Ollama package. Record the model
        identifier displayed by <code>ollama list</code> when reproducing a problem.</p>
        <h2 id="verify-endpoint">3. Verify the Responses endpoint independently</h2>
        <p>Run this in terminal B, where curl is already supplied by Nix:</p>
        <pre><code class="language-sh">{endpointVerification}</code></pre>
        <p>Expected result: an HTTP success response and a stream of Responses events containing
        generated text, ending in <code>response.completed</code>. The wording may vary.
        A refused connection means the server is not
        reachable at that address. A 404 usually means the path or API implementation differs.
        An unknown-model error means the server's loaded identifier differs from the request.</p>
        <p><a href="https://docs.ollama.com/api/openai-compatibility">Ollama's compatibility
        reference</a> documents Responses support from version <code>0.13.3</code>.
        A server offering only <code>/v1/chat/completions</code> is not sufficient.
        Haskell Agent's portable Responses connection uses stateless streaming; it does
        not require Ollama to store a conversation through <code>previous_response_id</code>.</p>
        <p>For an authenticated endpoint, supply its required authorization header from a secret
        environment variable. Do not paste the secret into a command saved in shell history.
        Do not disable TLS verification to make a hosted endpoint work.</p>
        <h2 id="register-model">4. Register the connection and model</h2>
        <p>Open <code>~/.haskell-agent/models.json</code> in your editor. If it already exists,
        merge the following connection and model into its existing objects and arrays instead
        of overwriting other entries:</p>
        <pre><code class="language-json" data-config-schema="models">{catalogExample}</code></pre>
        <p><code>local-coder</code> is the name used by Haskell Agent. The separate
        <code>model</code> field is sent to the server. <code>generic-responses</code> selects
        the portable Responses tool interface; it does not convert a Chat Completions server
        into a Responses server.</p>
        <p>The optional-key setting is appropriate only for this explicitly unauthenticated local
        example. For a protected service, replace it with <code>api_key_env</code> naming the
        secret variable. See the <a href="/customization/models/#connection-fields">connection
        field reference</a>.</p>
        <h2 id="test-generation">5. Test generation through the harness</h2>
        <pre><code class="language-sh">agent-cli --model local-coder</code></pre>
        <p>Run <code>/session-info</code> and verify the selected model, then submit:</p>
        <pre><code>Reply with a short greeting. Do not use tools.</code></pre>
        <p>Expected result: the selected connection produces a reply. Inspect session information
        rather than asking the model to identify itself. If direct curl worked but the harness
        fails, compare the registered URL and wire model name with the successful request, then
        check startup catalog errors and required environment variables.</p>
        <h2 id="test-tools">6. Test a read-only tool</h2>
        <p>Use a disposable directory with a known README rather than your main repository.
        In a separate shell, create the fixture and launch the configured model there:</p>
        <pre><code class="language-sh">{toolFixture}</code></pre>
        <p>Submit:</p>
        <pre><code>Read README.md using the file-reading tool. Quote its first heading and report the file path. Do not modify files or run shell commands.</code></pre>
        <p>Expected result: a visible file-reading tool call followed by an answer based on that
        result quoting <code>Local model verification</code>. Text claiming to have read the file without a tool call does not establish tool
        compatibility. A generation-only test can pass while function-call serialization or tool
        result handling still fails.</p>
        <p>If the model produces malformed tool calls, verify the inference server's Responses
        function-call support and the model's tool-use capability before changing permissions.
        Approving more operations cannot repair an incompatible API.</p>
        <p>The fixture deliberately disables filesystem skills and automatic
        project instructions. Inspect <code>/mcp</code> and disable any unrelated server
        you do not want available during the test. These launch flags
        are not a filesystem sandbox. Do not enable <code>--yolo</code> to compensate for a
        model that misunderstands the task.</p>
        <p>Record this harness check separately from the API checks. A successful protocol
        exchange does not establish reliable coding behavior or prove that the harness
        executed the requested file-reading tool.</p>
        <h2 id="context-and-recovery">7. Confirm limits and recovery</h2>
        <p>Keep <code>context_window</code> aligned with the context actually configured on the
        server, not merely the model architecture's maximum. Portable-model compaction refuses
        to guess a missing limit. A larger catalog value does not allocate more server memory.</p>
        <table>
            <thead><tr><th>Failure</th><th>Next check</th></tr></thead>
            <tbody>
                <tr><td>Connection refused</td><td>Server process, listening address, and port; rerun the direct request.</td></tr>
                <tr><td>404 or unsupported API</td><td>Version prefix and streaming Responses support.</td></tr>
                <tr><td>401</td><td>Endpoint authentication and the variable named by <code>api_key_env</code>.</td></tr>
                <tr><td>Timeout</td><td>Server logs and model loading. Raise the positive <code>request_timeout_seconds</code> only after establishing that the server is making progress.</td></tr>
                <tr><td>Tool schema or parsing failure</td><td>Responses function calls and model compatibility; do not treat this as an approval failure.</td></tr>
                <tr><td>Unexpected model selection</td><td>Use the local catalog <code>id</code>, restart after editing, and inspect <code>/session-info</code>.</td></tr>
                <tr><td>Out of memory or prompt truncation</td><td>Inspect server logs and <code>ollama ps</code>. Lower the server context and catalog value together, or choose a model and machine with sufficient capacity.</td></tr>
            </tbody>
        </table>
        <p>To stop using this connection, select another model with <code>/model</code>. Remove
        only the corresponding model entry and unused connection when editing the catalog.
        Custom connections are manually selected and are not automatic billing-fallback targets.</p>
        <h2 id="stop-and-remove">8. Stop the server and remove optional data</h2>
        <p>Exit the test agent with <code>/quit</code>. While Ollama is still running, unload
        the model; remove its downloaded weights only if you no longer need them:</p>
        <pre><code class="language-sh">{"ollama stop qwen3:0.6b\n# Optional: delete this model from the server's model storage.\nollama rm qwen3:0.6b" :: Text}</code></pre>
        <p>Press <code>Ctrl+C</code> in terminal A to stop the foreground server.
        Neither stopping it nor removing weights deletes your Haskell Agent sessions or
        the catalog entry. Remove that entry separately if it is no longer useful.</p>
    |]
    }

endpointVerification :: Text
endpointVerification = "curl --fail-with-body --no-buffer --max-time 120 \\\n\
    \  http://127.0.0.1:11434/v1/responses \\\n\
    \  -H 'Content-Type: application/json' \\\n\
    \  --data '{\"model\":\"qwen3:0.6b\",\"input\":\"Reply with a short greeting.\",\"stream\":true,\"store\":false,\"reasoning\":{\"effort\":\"none\"}}'"

nixEnvironment :: Text
nixEnvironment = "NIXPKGS=github:NixOS/nixpkgs/afe3d8ac4395617bdcdac9f188ac8717a062e014\n\
    \nix shell \"$NIXPKGS#ollama\" \"$NIXPKGS#curl\""

serverStartup :: Text
serverStartup = "OLLAMA_HOST=127.0.0.1:11434 \\\n\
    \OLLAMA_CONTEXT_LENGTH=32768 \\\n\
    \OLLAMA_NUM_PARALLEL=1 \\\n\
    \OLLAMA_NO_CLOUD=1 \\\n\
    \ollama serve"

modelDownload :: Text
modelDownload = "export OLLAMA_HOST=127.0.0.1:11434\n\
    \curl --fail --silent --show-error http://127.0.0.1:11434/api/version\n\
    \ollama pull qwen3:0.6b\n\
    \ollama list\n\
    \ollama show qwen3:0.6b"

toolFixture :: Text
toolFixture = "verification_directory=$(mktemp -d \"${TMPDIR:?}/local-model-verification.XXXXXX\")\n\
    \printf '# Local model verification\\n\\nThis is a read-only connection test.\\n' > \"$verification_directory/README.md\"\n\
    \agent-cli --model local-coder --cwd \"$verification_directory\" \\\n\
    \  --no-skills --no-agents-md"

catalogExample :: Text
catalogExample = "{\n\
    \  \"version\": 1,\n\
    \  \"connections\": {\n\
    \    \"local-inference\": {\n\
    \      \"api\": \"responses\",\n\
    \      \"base_url\": \"http://127.0.0.1:11434/v1\",\n\
    \      \"api_key_optional\": true,\n\
    \      \"request_timeout_seconds\": 600\n\
    \    }\n\
    \  },\n\
    \  \"models\": [{\n\
    \    \"id\": \"local-coder\",\n\
    \    \"connection\": \"local-inference\",\n\
    \    \"model\": \"qwen3:0.6b\",\n\
    \    \"dialect\": \"generic-responses\",\n\
    \    \"context_window\": 32768,\n\
    \    \"label\": \"local\"\n\
    \  }]\n\
    \}"
