{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.DocumentationSite (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)

page :: Page
page = Page
    { pagePath = "/guides/documentation/"
    , pageTitle = "Using and hosting the documentation"
    , pageDescription = "Search the guides, export their text, and operate the Haskell documentation service."
    , pageGroup = "Operations"
    , pageBody = [hsx|
        <p>The documentation is a Haskell WAI application served by Warp. HTML is rendered
        with <code>ihp-hsx</code>. It needs no database, JavaScript framework, Node.js,
        external search service, or CDN. The site is separate from the agent runtime:
        reading a page does not start an agent or connect a model account.</p>
        <h2 id="find-a-guide">Find a guide</h2>
        <p>Use the navigation groups to select a page. Previous and next links follow the
        guide order. On narrow screens, open Menu for the navigation and On this page
        for section links. These section links also work without JavaScript.</p>
        <p>Search submits your query to this documentation server, not to a model.
        All space-separated terms must occur in the title, description, or page text.
        Matching ignores letter case and uses substrings, not regular expressions or
        fuzzy spelling. Title matches are ranked first. Queries are limited to 200
        characters; an empty query does not return every page.</p>
        <p>For example, search <code>session export</code>. If no guide matches, remove
        a term or search the exact command name. Search indexes the published pages;
        engineering Markdown files elsewhere in the repository are not included.</p>
        <p>Published operator procedures are available in <a href="/guides/deployment/">Deployment</a>,
        <a href="/reference/server/">HTTP server</a>,
        <a href="/reference/runtime-daemon/">runtime daemon</a>, and
        <a href="/reference/native-integration/">native integration</a>.
        These distinguish implemented interfaces from application UX that is not
        shipped by this repository. Design notes remain separate rather than being
        presented as supported user workflows.</p>
        <h2 id="reading-controls">Reading controls and exports</h2>
        <p>Ctrl-K or Command-K focuses search when JavaScript is available. Theme selects
        Auto, Light, or Dark and is remembered in this browser. Copy buttons preserve
        the displayed code text; clipboard access requires HTTPS or localhost. Without
        JavaScript, reading, form-based search, section links and text exports still work.</p>
        <p>Each page has a text export derived from the same HTML content. The
        <a href="/llms.txt">documentation index for agents</a> lists all exports.
        These are plain text, not a separate Markdown source tree. A copied example
        remains an instruction to review, not proof it has run on your machine.</p>
        <h2 id="start-locally">Start locally</h2>
        <p>From a checkout of this repository:</p>
        <pre><code class="language-sh">{"nix build .#docs\nnix run .#docs\ncurl --fail http://127.0.0.1:4321/" :: Text}</code></pre>
        <p>The server runs in the foreground. Open a second terminal for the curl check.
        Expect an HTML document and HTTP 200. Stop the foreground server with Ctrl-C.
        The build produces <code>result/bin/documentation-server</code> and installed
        assets; it does not produce a static directory to upload.</p>
        <h2 id="server-settings">Server settings</h2>
        <table><thead><tr><th>Environment variable</th><th>Default</th><th>Behavior</th></tr></thead>
        <tbody>
            <tr><td><code>DOCS_HOST</code></td><td><code>127.0.0.1</code></td><td>Listening address. Keep loopback behind a reverse proxy.</td></tr>
            <tr><td><code>DOCS_PORT</code></td><td><code>4321</code></td><td>Integer TCP port from 1 to 65535.</td></tr>
            <tr><td><code>DOCS_ASSET_DIRECTORY</code></td><td>Installed public directory</td><td>Override only when supplying a complete matching asset set.</td></tr>
        </tbody></table>
        <p>Assets are read at application startup. Restart after changing an override.
        Only registered asset names are served; request paths do not become arbitrary
        filesystem reads. To deliberately listen on all interfaces:</p>
        <pre><code class="language-sh">DOCS_HOST=0.0.0.0 DOCS_PORT=8080 nix run .#docs</code></pre>
        <p>This exposes the site to other machines. There is no built-in authentication
        or TLS. Do not place private documentation on a public listener without an
        authenticated reverse proxy and appropriate network restrictions.</p>
        <h2 id="supervise-service">Supervise a service</h2>
        <p>On a Linux host with systemd, first build and retain a release outside a temporary
        checkout. An out-link keeps its Nix closure reachable by garbage collection:</p>
        <pre><code class="language-sh">nix build .#docs --out-link /srv/haskell-agent-documentation/release</code></pre>
        <p>The directory must already exist and be writable by your deployment account.
        Create a dedicated unprivileged service user, then adapt this unit:</p>
        <pre><code>{serviceUnit}</code></pre>
        <p>Install it as <code>/etc/systemd/system/haskell-agent-documentation.service</code>,
        reload systemd, and start it:</p>
        <pre><code class="language-sh">{"sudo systemctl daemon-reload\nsudo systemctl enable --now haskell-agent-documentation\nsystemctl status haskell-agent-documentation\njournalctl -u haskell-agent-documentation -n 50\ncurl --fail http://127.0.0.1:4321/" :: Text}</code></pre>
        <p>This is a deployment example, not a recorded systemd deployment test. On macOS,
        use a launchd service with the same installed executable and environment; do
        not install a Linux unit.</p>
        <h2 id="reverse-proxy">Reverse proxy and readiness</h2>
        <p>Serve the application at the domain root. Root-relative links do not support
        mounting it below an arbitrary URL prefix. In an existing TLS-enabled Nginx
        server, proxy to loopback:</p>
        <pre><code>{proxyConfiguration}</code></pre>
        <p>Provision the domain and certificate using your normal operator procedure.
        Check both the local upstream and the public HTTPS address. There is no separate
        health endpoint: use <code>GET /</code> and a known asset such as
        <code>/documentation.css</code>. A 502 usually means the proxy cannot reach the
        listener. Missing styles suggest mismatched assets or an unsupported URL prefix.</p>
        <h2 id="upgrade-and-recover">Upgrade and recover</h2>
        <ol>
            <li>Record the current release store path and retain its out-link.</li>
            <li>Build the reviewed revision to a separate candidate out-link.</li>
            <li>Run the candidate on a different loopback port and check a page, search,
            text export and stylesheet.</li>
            <li>Update the service executable path to the candidate and restart the service.</li>
            <li>Check the public address. If it fails, restore the previous executable
            path and restart. Keep the previous release until recovery is no longer needed.</li>
        </ol>
        <p>No documentation database migration is needed. A process restart interrupts
        in-flight requests; browsers can retry read requests. An invalid port, occupied
        address, or incomplete asset directory can prevent startup. Inspect the service
        log rather than repeatedly restarting without correcting the cause.</p>
        <h2 id="maintain-pages">Maintain pages</h2>
        <p>Edit HSX under <code>docs/src/Documentation/Pages</code>. Register each module
        in <code>Documentation.Content</code> and the Cabal exposed-module list. Add stable
        heading IDs, cross-links, examples and regression assertions. Regenerate
        <code>docs/package.nix</code> with <code>cabal2nix</code> after Cabal changes.</p>
        <p>New assets require both the Cabal data-file entry and the application's explicit
        asset registration, plus an asset-route regression test in <code>docs/test/Main.hs</code>.
        Follow <code>docs/README.md</code> for the GHCi development
        loop and validation commands. Page tests verify rendering and links, not every
        documented product behavior.</p>
    |]
    }

serviceUnit :: Text
serviceUnit = "[Unit]\nDescription=Haskell Agent documentation\nAfter=network.target\n\n[Service]\nUser=documentation\nGroup=documentation\nExecStart=/srv/haskell-agent-documentation/release/bin/documentation-server\nEnvironment=DOCS_HOST=127.0.0.1\nEnvironment=DOCS_PORT=4321\nRestart=on-failure\nRestartSec=5\nNoNewPrivileges=true\nPrivateTmp=true\nProtectSystem=strict\nProtectHome=true\n\n[Install]\nWantedBy=multi-user.target"

proxyConfiguration :: Text
proxyConfiguration = "location / {\n    proxy_pass http://127.0.0.1:4321;\n    proxy_set_header Host $host;\n    proxy_set_header X-Forwarded-Proto $scheme;\n}"
