{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Documentation.Pages.Deployment (page) where

import Data.Text (Text)
import Documentation.Types (Page (..))
import IHP.HSX.QQ (hsx)
import Text.Blaze.Html (Html)

page :: Page
page = Page
    { pagePath = "/guides/deployment/"
    , pageTitle = "Deployment and Nix outputs"
    , pageDescription = "Choose packaged interfaces, host this documentation, and configure Telegram NixOS service instances."
    , pageGroup = "Using the agent"
    , pageBody = [hsx|
        <h2 id="outputs">Choose a flake output</h2>
        <p>Pin the flake revision in the consuming deployment. An output containing a library
        is not an executable application. Platform-conditional outputs are absent on other
        platforms; verify the pinned Nixpkgs platform support before planning an upgrade.</p>
        <table><thead><tr><th>Output</th><th>Purpose</th></tr></thead><tbody>
            <tr><td><code>default</code>, <code>agent-cli-static</code>, <code>agent-cli</code></td><td>CLI packages. The default package uses the static Linux harness with runtime tools; <code>agent-cli</code> is the native build. The default app runs the CLI.</td></tr>
            <tr><td><code>docs</code></td><td>Haskell documentation server package/app and development shell.</td></tr>
            <tr><td><code>agent-telegram</code></td><td>Telegram gateway package and app; <code>nixosModules.telegram</code> supplies managed instances.</td></tr>
            <tr><td><code>agent-server</code></td><td>HTTP server package/app; <code>nixosModules.agent-server</code> supplies the tenant service boundary.</td></tr>
            <tr><td><code>agent-runtime-daemon</code></td><td>Local Unix-socket scheduler package/app.</td></tr>
            <tr><td><code>agent-server-client</code></td><td>Haskell client library, not a web frontend.</td></tr>
            <tr><td><code>agent-sandbox-runner</code>, <code>agent-sandbox-rootfs</code></td><td>Linux-only tenant execution artifacts. Use the server module; do not bypass its admission checks.</td></tr>
            <tr><td><code>agent-native-bridge</code></td><td>Darwin native foreign library. <code>agent-native-bridge-library</code> is the Haskell package.</td></tr>
            <tr><td><code>agent-cli-macos-bundle</code>, <code>agent-cli-macos-archive</code></td><td>Darwin CLI release artifacts, not proof of a native conversation GUI.</td></tr>
            <tr><td><code>agent-openai-login</code></td><td>Packaged OpenAI login executables and app.</td></tr>
        </tbody></table>
        <p>Other named Haskell package outputs support embedding/development. Use
        <code>nix develop</code> for the harness or <code>nix develop .#docs</code> for this
        site's focused GHCi environment.</p>
        <h2 id="sandbox-provisioning">Provision the Linux sandbox boundary</h2>
        <p>The runner and root filesystem are a matched pair selected by the flake.
        On a Linux builder, inspect the artifacts without starting a tenant:</p>
        <pre><code class="language-sh">{sandboxArtifacts}</code></pre>
        <p>The rootfs output is an unpacked immutable directory, not a Docker archive
        or bootable machine image. It contains the guest worker, shell, Git, Nix,
        document tools, certificates and a Nix registration seed. The runner contains
        the selected rootfs store path; do not substitute a separately modified tree.
        These build commands establish neither isolation nor a running server.</p>
        <ol>
            <li>Use a Linux NixOS host with unified cgroup v2, the cpu/memory/pids
            controllers and working unprivileged user/network namespaces. The packaged
            gVisor uses systrap; the runner uses slirp4netns and namespace-local nftables.</li>
            <li>Import <code>haskell-agent.nixosModules.agent-server</code> from the
            locked input. Provision non-overlapping workspaces, owner-only registry
            and credential files as described in
            <a href="/reference/server/#tenant-provisioning">tenant provisioning</a>.
            List every workspace in the module's <code>workspaceRoots</code>.</li>
            <li>Apply the module fragment below in that NixOS configuration. Replace
            example paths with the already provisioned paths; this fragment does
            not create tenants or credentials.</li>
        </ol>
        <pre><code class="language-nix">{sandboxService}</code></pre>
        <p>Deploy with <code>sudo nixos-rebuild switch --flake .#agent-host</code>,
        using your configuration's actual host name. Keep the initial listener on
        loopback. Remote exposure additionally requires TLS termination, authentication
        rate limits, <code>allowRemote</code> and an explicit listener configuration.</p>
        <p>The module creates a dedicated unprivileged account without supplementary
        groups, removes every capability, sets <code>NoNewPrivileges</code>, and delegates
        exactly cpu/memory/pids to its supervisor subgroup. It installs the runner under
        root-owned <code>/run/haskell-agent-server-runners/STATE/GENERATION/</code>.
        A direct runner path in a conventional group-writable Nix-store ancestry is
        intentionally rejected. Do not relax the ancestry check or run as root to
        bypass a failure. Launching the runner from an ordinary shell does not reproduce
        the module's security boundary.</p>
        <h3 id="sandbox-protocol">Runner protocol and resource boundary</h3>
        <p>This is a private, revision-coupled server/worker protocol, not an additional
        public automation API. The server invokes <code>serve</code> with
        <code>--protocol-version 1</code>, tenant UUID, workspace path, recorded workspace
        device/inode and state path. Stdin must be a read-only pipe; stdout carries
        newline-delimited JSON and stderr carries private diagnostics. Never insert
        logging on protocol stdout.</p>
        <table><thead><tr><th>Phase</th><th>Required interpretation</th></tr></thead><tbody>
            <tr><td>Readiness</td><td><code>type: ready</code>, version, tenantId, generation UUID, workspace <code>/workspace</code> and state <code>/state</code>. The broker validates identity and mounts before admitting tools.</td></tr>
            <tr><td>Request</td><td><code>type: tool</code>, version, tenantId, generation, requestId, sessionId, cwd, dialect and call. The call carries id, name, arguments, kind and argumentsEncrypted; async is optional. Encrypted arguments are rejected.</td></tr>
            <tr><td>Progress and completion</td><td><code>output</code> frames carry output; <code>result</code> frames carry ok/output and optional images. Both bind tenantId, generation and requestId. A progress frame is not successful completion.</td></tr>
            <tr><td>Limits</td><td>4 MiB requests, 16 MiB response frames and accumulated streamed output, 60-second readiness and 15-minute tool deadlines. Schema, identity, timeout or transport failures fail closed, never executing the tool on the host instead.</td></tr>
        </tbody></table>
        <p>Only the tenant workspace and guest-data directory are writable host binds.
        The runner pins their directory descriptors and rejects workspace device/inode
        substitution. Each sandbox tree, including its network helper, is limited to
        two CPUs, 2 GiB RAM without swap and 512 processes; the rootfs has a private
        4 GiB overlay and 256 MiB mutable Nix-state tmpfs. Workspace/state disk quotas
        and database quotas/backups remain operator responsibilities. Outbound networking
        rejects private, loopback, link-local, metadata, reserved, IPv6 and captured host
        addresses. Host address changes retire the sandbox; no inbound service is exposed.</p>
        <h3 id="sandbox-recovery">Verify and recover without weakening isolation</h3>
        <pre><code class="language-sh">{"systemctl status haskell-agent-server\njournalctl -u haskell-agent-server -n 100 --no-pager\nsystemctl show haskell-agent-server -p User -p Group -p NoNewPrivileges -p Delegate -p ControlGroup\ncurl --fail http://127.0.0.1:4096/healthz" :: Text}</code></pre>
        <p>A healthy HTTP listener does not prove a sandbox can start. After authentication,
        run a read-only request that actually uses a workspace inspection tool and verify
        its result. For startup failures, inspect namespace/cgroup availability, runner
        generation, workspace ownership and registry identity; diagnostics may contain
        sensitive paths and belong in a private operator channel.</p>
        <p>If cleanup cannot prove descendant termination, the runner retains the tenant
        lock and fail-stops; a stale tenant cgroup blocks replacement. Stop the service
        and verify that its complete control group is empty before investigating retained
        state. Never delete a lock or cgroup while processes remain. Preserve diagnostic
        state, correct the host prerequisite, then restart and revalidate a read-only
        tool call. An interrupted mutation may already have changed files or remote
        systems; inspect before retrying. Upgrades and rollback use generation-addressed
        runner paths, so existing processes never silently switch runner executables.</p>
        <h2 id="darwin-artifacts">Build and inspect Darwin artifacts</h2>
        <p>Run these commands in the pinned checkout on a supported Darwin builder.
        The bridge output contains <code>include/HaskellAgentBridge.h</code> and
        <code>lib/libhaskell-agent-bridge.dylib</code>. It is not the relocatable CLI
        bundle, an application bundle, or a signed/notarized GUI release.</p>
        <pre><code class="language-sh">{darwinArtifacts}</code></pre>
        <p>Compile native consumers against that output's header, not a header from
        another revision. For the <code>highlight.c</code> example in
        <a href="/reference/native-integration/#first-host-call">native integration</a>,
        a linker invocation is:</p>
        <pre><code class="language-sh">{"nix develop -c cc -I \"$bridge/include\" highlight.c \\\n  -L \"$bridge/lib\" -lhaskell-agent-bridge \\\n  -Wl,-rpath,\"$bridge/lib\" -o highlight" :: Text}</code></pre>
        <p>Inspect <code>otool -L</code> output before distribution: the foreign-library
        output preserves its Nix dependency closure and is not independently portable.
        Keep that closure available on the development host. A distributing native
        application must separately supply data files, dependency relocation, signing
        and entitlement policy, and the host callbacks documented by the bridge.
        This command is a build recipe, not evidence that an external application was tested.</p>
        <h3 id="portable-cli-installation">Install the complete portable CLI directory</h3>
        <p>The CLI archive is <code>haskell-agent-macos-arm64.tar.gz</code> on Apple
        Silicon or <code>haskell-agent-macos-x86_64.tar.gz</code> on Intel, if that
        architecture is supported by the pinned Nixpkgs. A sibling <code>.sha256</code>
        file accompanies it. Verify the release source independently; a checksum
        detects corruption but is not a publisher signature.</p>
        <pre><code class="language-sh">{portableInstallation}</code></pre>
        <p>The example assumes an Apple Silicon archive copied into the current
        directory and a previously unused installation destination. Use a fresh
        versioned destination for upgrades rather than merging two releases.
        Keep the complete extracted tree: <code>bin</code> includes agent-cli,
        FFmpeg, Bun, ripgrep and zstd; <code>lib/deps</code> contains relocated libraries;
        <code>libexec/postgresql</code> contains PostgreSQL 18; <code>share</code> contains
        prompts, skills, runtime configuration, syntax definitions, time zones, the
        portable marker and license notices. Copying only agent-cli loses these
        runtime dependencies. The bundle build applies ad-hoc signatures, not an
        Apple Developer ID/notarization guarantee. Do not instruct users to disable
        Gatekeeper; verify the distributor's release/signing procedure.</p>
        <p>The archive targets the CLI, not an installable conversation GUI. Nix is
        needed to build these artifacts; the portable archive is designed to run
        without a Nix installation. Project-specific tools, provider access and
        operating-system permissions remain separate prerequisites. Preserve user
        data and back it up before changing application or database versions.
        These artifact and sandbox recipes were checked against source, not deployed
        to a Linux host or a clean non-Nix Mac as part of this documentation change.</p>
        <h2 id="documentation">Host the documentation</h2>
        <p>For the full deployment and upgrade walkthrough, see
        <a href="/guides/documentation/">self-hosting the documentation</a>.
        The following is a compact NixOS service example.</p>
        <pre><code class="language-sh">{"nix build .#docs\nnix run .#docs\n# Optional alternative listener:\nDOCS_HOST=127.0.0.1 DOCS_PORT=8080 nix run .#docs" :: Text}</code></pre>
        <p>The default is <code>127.0.0.1:4321</code>. <code>DOCS_PORT</code> must be 1–65535;
        <code>DOCS_HOST</code> changes the listener. <code>DOCS_ASSET_DIRECTORY</code>
        overrides the installed asset directory for development; normally leave it unset.
        The build output is an application with packaged assets, not a static HTML directory.</p>
        <p>For a NixOS service, pass the pinned flake input as <code>haskell-agent</code>
        through your module arguments and use this module fragment:</p>
        <pre><code>{documentationService}</code></pre>
        <p>After applying your system configuration, inspect
        <code>systemctl status haskell-agent-documentation</code> and
        <code>journalctl -u haskell-agent-documentation -n 100 --no-pager</code>.
        Verify the loopback URL before configuring a proxy. In an existing TLS-enabled Nginx
        virtual host, proxy the root path to the listener:</p>
        <pre><code>{"location / {\n    proxy_pass http://127.0.0.1:4321;\n    proxy_set_header Host $host;\n    proxy_set_header X-Forwarded-Proto $scheme;\n}" :: Text}</code></pre>
        <p>The site uses root-relative links; host it at a domain root rather than assuming
        a subpath prefix works. A public documentation service needs no model credentials,
        workspace access or PostgreSQL. Binding to <code>0.0.0.0</code> exposes it to other
        machines. These deployment fragments have been reviewed against the application,
        not exercised on your production host.</p>
        <h2 id="telegram-options">Telegram NixOS option reference</h2>
        <p>Import <code>haskell-agent.nixosModules.telegram</code>. All options below belong
        under <code>services.haskell-agent.telegram.instances.NAME</code>. Names start with
        a lowercase letter, contain lowercase letters/digits/hyphens, and have at most
        16 characters. See <a href="/guides/telegram/#nixos-deployment">the setup example</a>
        before customizing an instance. Rebuild/restart the service after changing its
        declaration. Keep credentials outside the Nix store.</p>
        <table><thead><tr><th>Option</th><th>Type and default</th><th>Effect</th></tr></thead>
        <tbody>{foldMap optionRow telegramOptions}</tbody></table>
        <h2 id="declarative-mcp">Declarative MCP ownership</h2>
        <p><code>mcpServers = null</code> leaves the existing catalog untouched.
        An attribute set takes ownership of the complete <code>mcpServers</code> value in
        the service user's configuration. <strong>An empty attribute set clears that
        catalog.</strong> Do not combine ad-hoc edits with a declaration and expect both
        sources to merge.</p>
        <pre><code>{"services.haskell-agent.telegram.instances.assistant.mcpServers = {\n  project = {\n    command = \"/absolute/path/to/mcp-server\";\n    args = [ \"--stdio\" ];\n    enabled = true;\n    startupTimeoutSeconds = 30;\n    requestTimeoutSeconds = 60;\n  };\n};" :: Text}</code></pre>
        <p>The command is a placeholder for an installed stdio MCP server, not supplied by
        this example. Declare exactly one of command or URL per server.</p>
        <table><thead><tr><th>Server field</th><th>Type/default</th><th>Meaning</th></tr></thead><tbody>
            <tr><td><code>enabled</code></td><td>Boolean, true</td><td>Connect this server.</td></tr>
            <tr><td><code>command</code></td><td>Nullable string, null</td><td>Local stdio executable; mutually exclusive with URL.</td></tr>
            <tr><td><code>url</code></td><td>Nullable string, null</td><td>Remote endpoint; mutually exclusive with command.</td></tr>
            <tr><td><code>args</code></td><td>String list, empty</td><td>Local command arguments.</td></tr>
            <tr><td><code>cwd</code></td><td>Nullable string, null</td><td>Local server working directory.</td></tr>
            <tr><td><code>environment</code></td><td>String attribute set, empty</td><td>Non-secret server environment. Nix values are store-readable.</td></tr>
            <tr><td><code>startupTimeoutSeconds</code></td><td>Positive integer, 30</td><td>Startup deadline.</td></tr>
            <tr><td><code>requestTimeoutSeconds</code></td><td>Positive integer, 60</td><td>Request deadline.</td></tr>
        </tbody></table>
        <h2 id="operations">Operate and upgrade services</h2>
        <ol>
            <li>Record the current flake lock and service configuration before upgrading.</li>
            <li>Back up PostgreSQL using database tools; do not copy a live data directory.
            Preserve gateway state separately and protect credential recovery.</li>
            <li>Apply the new declaration and inspect the service journal. A running process
            is not proof of valid provider authentication or correct project ownership.</li>
            <li>Run a read-only message/request and confirm the intended workspace and account
            before allowing mutations.</li>
        </ol>
        <p>For an existing service user, set <code>createUser = false</code> only after
        provisioning its user/group and private home yourself. The project must already
        exist and be writable when mutations are allowed. Do not solve ownership failures
        with automatic approval. PostgreSQL package upgrades require an explicit database
        migration plan; changing <code>postgresPackage</code> does not itself migrate data.
        A failed migration is not fixed by deleting the database.</p>
        <h3 id="restore-checklist">Database and gateway restore checklist</h3>
        <p>For a PostgreSQL major-version change, retain the old package and database
        directory until recovery is proven. Stop the Telegram gateway to prevent new
        requests while taking a consistent logical database dump with the old version's
        database tools. Record database roles/ownership and the configured port; protect
        the dump as conversation data. Back up the gateway state and credentials under
        the same service identity separately.</p>
        <ol>
            <li>Restore the logical dump into a newly initialized target-version database
            in an isolated test environment. Never point two PostgreSQL versions at the
            same data directory, and never run two gateway pollers with the same bot token.</li>
            <li>Check conversation counts and representative session history, service-user
            ownership and database connectivity before promoting the restored database.</li>
            <li>Keep the gateway stopped while switching its database configuration.
            Start one gateway, inspect startup logs, then issue a read-only message.</li>
            <li>Inspect retained replies and pending work before using retry: restoring
            earlier state cannot retract Telegram deliveries or remote tool effects.</li>
            <li>If validation fails, stop the new gateway and database, preserve their
            diagnostic data, and return to the untouched old database/package and matching
            gateway backup. Reconcile any post-cutover effects before resuming work.</li>
        </ol>
        <p>Use your PostgreSQL version's documented dump/restore commands with the actual
        connection details; the package override supplies binaries, not an automated
        major-version migrator. This operational sequence is source-reviewed guidance,
        not a claim that a production restore was exercised.</p>
        <h3 id="environment-troubleshooting">Environment conflicts and restarts</h3>
        <p>Keep each secret in one configured environment file rather than also assigning
        it in the Nix <code>environment</code> map. An optional file prefixed with
        <code>-</code> may be absent; use a required file when absence should block startup.
        A changed file does not update an already running process: restart the instance,
        inspect its journal and verify a read-only request. If the wrong account remains
        selected, compare configured variable names and file ordering, not printed secret
        values; provider credential stores may also supply authentication. Remove the
        duplicate assignment at its source and restart again. Do not dump the service
        environment into a public issue.</p>
        <p>For multi-tenant API hosting, use the separate
        <a href="/reference/server/#tenants">server deployment requirements</a>. Its sandbox,
        authentication and storage policy are not the Telegram module's policy.</p>
    |]
    }

optionRow :: (Text, Text, Text) -> Html
optionRow (name, value, description) = [hsx|<tr><td><code>{name}</code></td><td>{value}</td><td>{description}</td></tr>|]

telegramOptions :: [(Text, Text, Text)]
telegramOptions =
    [ ("enable", "Boolean, false", "Enable this instance.")
    , ("package", "Package, flake agent-telegram", "Executable distribution.")
    , ("user", "String, haskell-agent-NAME", "Service account.")
    , ("group", "String, user value", "Service group.")
    , ("createUser", "Boolean, true", "Create dedicated system user/group.")
    , ("homeDirectory", "String, /var/lib/haskell-agent-telegram-NAME", "Private persistent home.")
    , ("workingDirectory", "Required string", "Existing absolute project path.")
    , ("tokenFile", "Required string", "Absolute secret file loaded through systemd credentials.")
    , ("provider", "Enum, openai", "openai, xai, openrouter, gemini or claude-code.")
    , ("model", "Nullable string, null", "Null selects the provider default.")
    , ("effort", "Nullable string, null", "Optional reasoning effort.")
    , ("yolo", "Boolean, false", "Automatic mutation approval; review host access first.")
    , ("allowedUsers", "Unsigned integer list, empty", "Numeric Telegram user IDs.")
    , ("respondToAllGroupMessages", "Boolean, false", "Ambient participation; does not disable allowlisting.")
    , ("codexHome", "String, HOME/.codex", "OpenAI credential directory.")
    , ("postgresPackage", "Package, PostgreSQL 18", "Managed private database executable.")
    , ("postgresPort", "Port, 55432", "Cluster port; Unix-socket isolation permits reuse across instances.")
    , ("extraPackages", "Package list, empty", "Additional tools on the agent's PATH.")
    , ("environment", "String attribute set, empty", "Non-secret service variables.")
    , ("environmentFiles", "String list, empty", "Systemd environment files read at startup; prefix an absolute path with '-' to allow a missing file.")
    , ("mcpInitStrategy", "Enum, auto", "auto or progressive.")
    , ("mcpServers", "Nullable server attribute set, null", "Null preserves existing catalog; an attrset owns the entire catalog.")
    ]

documentationService :: Text
documentationService = "{ pkgs, haskell-agent, ... }: {\n\
    \  systemd.services.haskell-agent-documentation = {\n\
    \    description = \"Haskell Agent documentation\";\n\
    \    wantedBy = [ \"multi-user.target\" ];\n\
    \    environment = { DOCS_HOST = \"127.0.0.1\"; DOCS_PORT = \"4321\"; };\n\
    \    serviceConfig = {\n\
    \      ExecStart = \"${haskell-agent.packages.${pkgs.stdenv.hostPlatform.system}.docs}/bin/documentation-server\";\n\
    \      DynamicUser = true;\n\
    \      Restart = \"on-failure\";\n\
    \      NoNewPrivileges = true;\n\
    \      ProtectSystem = \"strict\";\n\
    \      ProtectHome = true;\n\
    \      PrivateTmp = true;\n\
    \    };\n\
    \  };\n\
    \}"

sandboxArtifacts :: Text
sandboxArtifacts = "runner=$(nix build .#agent-sandbox-runner --no-link --print-out-paths)\n\
    \rootfs=$(nix build .#agent-sandbox-rootfs --no-link --print-out-paths)\n\
    \test -x \"$runner/bin/agent-sandbox-runner\"\n\
    \test -x \"$rootfs/bin/agent-sandbox-worker\"\n\
    \test -s \"$rootfs/nix-state-seed/db/db.sqlite\""

sandboxService :: Text
sandboxService = "{ haskell-agent, ... }: {\n\
    \  imports = [ haskell-agent.nixosModules.agent-server ];\n\
    \  services.haskell-agent.server = {\n\
    \    enable = true;\n\
    \    tenantRegistryFile = \"/run/credentials/agent-tenants.json\";\n\
    \    workspaceRoots = [ \"/srv/agent-workspaces/acme\" ];\n\
    \    host = \"127.0.0.1\";\n\
    \    maxActiveTenants = 16;\n\
    \  };\n\
    \}"

darwinArtifacts :: Text
darwinArtifacts = "bridge=$(nix build .#agent-native-bridge --no-link --print-out-paths)\n\
    \bundle=$(nix build .#agent-cli-macos-bundle --no-link --print-out-paths)\n\
    \archive=$(nix build .#agent-cli-macos-archive --no-link --print-out-paths)\n\
    \test -f \"$bridge/include/HaskellAgentBridge.h\"\n\
    \otool -L \"$bridge/lib/libhaskell-agent-bridge.dylib\"\n\
    \\"$bundle/bin/agent-cli\" --version\n\
    \ls \"$archive\""

portableInstallation :: Text
portableInstallation = "shasum -a 256 -c haskell-agent-macos-arm64.tar.gz.sha256\n\
    \mkdir -p \"$HOME/Applications\"\n\
    \test ! -e \"$HOME/Applications/haskell-agent-macos-arm64\" && \\\n\
    \  tar -xzf haskell-agent-macos-arm64.tar.gz -C \"$HOME/Applications\"\n\
    \\"$HOME/Applications/haskell-agent-macos-arm64/bin/agent-cli\" --version"
