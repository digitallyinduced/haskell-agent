{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.LanguageServers (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/customization/language-servers/"
    , pageTitle = "Language servers"
    , pageDescription = "Configure local language servers for definitions, references, hover information, and symbol discovery."
    , pageGroup = "Customization"
    , pageBody = [hsx|
        <p>Language Server Protocol (LSP) support gives the agent semantic information that text
        search cannot reliably provide: which declaration a name refers to, its type, and its
        references. The language server runs locally as a subprocess. This is separate from
        <a href="/customization/mcp/">MCP</a>; an LSP server is not an MCP server.</p>
        <h2 id="prerequisites">Prerequisites</h2>
        <p>Install a stdio language server in your project's Nix development environment, and
        launch the agent from that environment. The language server also needs the project's
        compiler, dependencies, and configuration. A globally installed executable does not
        guarantee that it can understand the project.</p>
        <p>The following example uses <code>nil</code> for Nix files. Add <code>pkgs.nil</code> to
        your flake development shell's packages, enter <code>nix develop</code>, and check:</p>
        <pre><code class="language-sh">{"command -v nil\nnil --version" :: Text}</code></pre>
        <h2 id="configure-a-server">Configure a server</h2>
        <p>Merge this <code>lsp</code> object into <code>~/.haskell-agent/config.json</code>.
        Preserve your existing settings. Start a new agent session after editing it.</p>
        <pre><code class="language-json" data-config-schema="harness">{configurationExample}</code></pre>
        <p>The key <code>nix</code> identifies this configuration entry; <code>.nix</code> selects
        files by extension and the value <code>nix</code> is the language identifier sent to the
        server. Replace these together when configuring another language.</p>
        <h2 id="server-options">Server options</h2>
        <table>
            <thead><tr><th>Field</th><th>Default</th><th>Meaning</th></tr></thead>
            <tbody>
                <tr><td><code>lsp.enabled</code></td><td><code>false</code></td><td>Enable the language-server runtime</td></tr>
                <tr><td><code>lsp.servers</code></td><td>Empty object</td><td>Named server configurations</td></tr>
                <tr><td><code>command</code></td><td>Required</td><td>Executable name or path, not a combined shell command</td></tr>
                <tr><td><code>args</code></td><td>Empty array</td><td>Separate command arguments</td></tr>
                <tr><td><code>env</code></td><td>Empty object</td><td>Environment values for the server; diagnostics redact this field</td></tr>
                <tr><td><code>extensionToLanguage</code></td><td>Empty object</td><td>Filename-extension to language-identifier mapping</td></tr>
                <tr><td><code>initializationOptions</code></td><td>Absent</td><td>Server-specific JSON sent during initialization</td></tr>
                <tr><td><code>settings</code></td><td>Absent</td><td>Server-specific workspace configuration</td></tr>
                <tr><td><code>workspaceFolder</code></td><td>Absent</td><td>Override the workspace folder</td></tr>
                <tr><td><code>startupTimeoutMilliseconds</code></td><td><code>15000</code></td><td>Initialization deadline</td></tr>
                <tr><td><code>shutdownTimeoutMilliseconds</code></td><td><code>5000</code></td><td>Graceful shutdown deadline</td></tr>
            </tbody>
        </table>
        <p>Only <code>transport: "stdio"</code> is supported. Automatic crash restart is not
        implemented: <code>restartOnCrash: true</code> and any <code>maxRestarts</code> setting
        are rejected rather than silently ignored.</p>
        <h2 id="verify-semantic-navigation">Verify semantic navigation</h2>
        <ol>
            <li>Launch the agent in the repository's Nix shell.</li>
            <li>Choose a declaration or reference in an existing Nix file.</li>
            <li>Ask: <code>Use the language server to find the definition of this symbol in flake.nix. Report its file and location; do not edit anything.</code></li>
            <li>Inspect the tool result, not just the final prose. A successful result identifies
            a source location or explicitly reports that the server returned no locations.</li>
        </ol>
        <p>Tool availability and naming follow the active model's tool dialect. When the
        <code>lsp</code> tool is available, it supports <code>goToDefinition</code>,
        <code>findReferences</code>, <code>hover</code>, <code>goToImplementation</code>,
        <code>documentSymbol</code>, and <code>workspaceSymbol</code>. Position-based requests use
        an absolute <code>file_path</code> and zero-based <code>line</code> and <code>character</code>;
        workspace-symbol searches use a non-empty <code>query</code>.</p>
        <h2 id="operation-payloads">Operation payloads</h2>
        <p>Use <code>operation</code> to select the request. This example asks for hover
        information at the first character of the first line; replace the path and position with
        a real symbol in the configured project.</p>
        <pre><code class="language-json">{"{\"operation\":\"hover\",\"file_path\":\"/absolute/path/to/project/flake.nix\",\"line\":0,\"character\":0}" :: Text}</code></pre>
        <table><thead><tr><th>Operation</th><th>Inputs</th><th>Result to inspect</th></tr></thead><tbody>
            <tr><td><code>goToDefinition</code>, <code>goToImplementation</code></td><td>File and zero-based line/character</td><td>Destination source locations</td></tr>
            <tr><td><code>findReferences</code></td><td>File and zero-based line/character</td><td>Reference locations, not an authorization to rename them</td></tr>
            <tr><td><code>hover</code></td><td>File and zero-based line/character</td><td>Type/documentation supplied by the server</td></tr>
            <tr><td><code>documentSymbol</code></td><td>Absolute <code>file_path</code></td><td>Symbols in that document</td></tr>
            <tr><td><code>workspaceSymbol</code></td><td>Non-empty <code>query</code></td><td>Matching symbols across the workspace</td></tr>
        </tbody></table>
        <p>An empty result can mean no matching symbol or an unsupported capability. Confirm
        the position and project environment before falling back to text search.</p>
        <h2 id="diagnose-language-server-failures">Diagnose failures</h2>
        <table>
            <thead><tr><th>Symptom</th><th>Check</th><th>Recovery</th></tr></thead>
            <tbody>
                <tr><td>Executable not found</td><td>Run <code>command -v nil</code> in the same shell</td><td>Add the server to the project flake and launch the agent inside <code>nix develop</code></td></tr>
                <tr><td>No configured server for the file</td><td>Check <code>enabled</code> and the extension mapping</td><td>Map the actual extension, including its leading dot, and restart the session</td></tr>
                <tr><td>Startup timeout</td><td>Run the server's version command; inspect compiler/dependency availability</td><td>Fix the project environment first; increase the deadline only if initialization is legitimately slow</td></tr>
                <tr><td>Empty definition or references</td><td>Check the symbol position and whether the server supports the operation</td><td>Try a known local declaration; an empty result is not a transport failure</td></tr>
                <tr><td>Server exits during work</td><td>Check its configuration and project logs</td><td>Correct the cause and start a new agent session; do not configure unsupported restart options</td></tr>
            </tbody>
        </table>
        <p>Language-server output helps investigation; it does not replace compilation or tests.
        Run the project's normal validation after changing code.</p>
    |]
    }

configurationExample :: Text
configurationExample = "{\n  \"version\": 1,\n  \"lsp\": {\n    \"enabled\": true,\n    \"servers\": {\n      \"nix\": {\n        \"command\": \"nil\",\n        \"args\": [],\n        \"extensionToLanguage\": { \".nix\": \"nix\" },\n        \"startupTimeoutMilliseconds\": 15000,\n        \"shutdownTimeoutMilliseconds\": 5000\n      }\n    }\n  }\n}"
