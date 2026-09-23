# Documentation application

The user documentation is a Haskell WAI application served by Warp. Pages and
the shared layout are HTML written directly with `ihp-hsx`. There is no
JavaScript framework, Markdown renderer, npm dependency, or Node.js build step.
Existing engineering notes in this directory remain separate from the website.

The source-linked [coverage audit](audit/README.md) records known omissions,
conflicting claims, and priorities across the website and implemented interfaces.

## Content standards

The website covers implemented terminal-client behavior and explicitly labeled
operator and embedding interfaces, not planned application screens. References
explain exact settings, defaults, precedence, persistence,
and failure behavior. Tutorials give prerequisites, complete commands or files,
observable results, and recovery steps. Do not substitute generic prompting
advice for a configuration procedure.

When changing behavior, update its owning guide and cross-link it rather than
duplicating the rules. Check the implementation and tests; engineering notes
are supporting material and may lag behind the source. In particular:

| Area | Source of truth |
| --- | --- |
| Runtime settings, MCP, LSP, web access | `Agent/Runtime/Config.hs` and the corresponding runtime implementation |
| Model catalogs and authentication | Provider/account packages, model-catalog parsing, `docs/models.md` |
| Filesystem skills | `Agent/Skills.hs` discovery, parser, and validation |
| Learned skills | `Agent/CLI/LearnedSkills.hs` and the learned-skill store |
| CLI commands and permissions | CLI option parser, command registry, approval handling |
| Telegram and dictation | Telegram package and CLI voice-input implementation |

These module paths are located under `packages/`. Verify examples locally where
possible. Account-dependent integrations and microphone behavior require their
own live checks; passing website tests does not certify those external systems.
Never present illustrative output as a captured product result.

### Verification boundaries

For local-model tutorials, distinguish three checks: the server generates text,
its API completes a function-call/result exchange, and the harness actually
executes a tool. Passing one does not establish the next. Record the server
version, model identifier, configured context size, and any unexecuted steps.
Use isolated model storage and configuration when testing examples.

Interactive illustrations must come from a real terminal session. Keep a text
transcript and capture provenance, including the executable version and any
cropping or rendering conversion. Do not manufacture accounts or tool results
to fill a missing screen. An older installed build must be labeled as such.

## Build and run

From the repository root:

```sh
nix build .#docs
nix run .#docs
```

Open <http://127.0.0.1:4321>. The build produces
`result/bin/documentation-server` with its styles, script, and icon installed as
Cabal data files. It is an application, not a static HTML directory.

Configuration is through environment variables:

| Variable | Default | Purpose |
| --- | --- | --- |
| `DOCS_HOST` | `127.0.0.1` | Listening address |
| `DOCS_PORT` | `4321` | TCP port, 1–65535 |
| `DOCS_ASSET_DIRECTORY` | Installed `public` directory | Explicit asset override for development |

```sh
DOCS_HOST=0.0.0.0 DOCS_PORT=8080 nix run .#docs
```

Binding to `0.0.0.0` exposes the documentation to other machines. For public
hosting, run the application under a service supervisor and terminate TLS at
a reverse proxy. For example, inside an existing TLS-enabled Nginx server:

```nginx
location / {
    proxy_pass http://127.0.0.1:4321;
    proxy_set_header Host $host;
    proxy_set_header X-Forwarded-Proto $scheme;
}
```

Serve it at the domain root; links and assets use root-relative paths.
No database, external search service, CDN, or runtime network dependency is
required. Search runs in Haskell and works without JavaScript. Plain CSS and a
small vanilla script provide themes, mobile navigation, a table of contents,
and code copying. Clipboard access requires HTTPS or localhost.

## Edit pages

Pages are in `src/Documentation/Pages/`, registered in
`src/Documentation/Content.hs`. Each page declares its route, title, description,
navigation group, and an HSX body. Edit HTML directly inside `[hsx| … |]`.
Use stable `id` attributes for section headings. Interpolated Haskell values
are escaped by HSX; code examples should be interpolated as `Text` to preserve
whitespace and literal braces.

Mark shell examples with `class="language-sh"` (or `language-bash`) and JSON
examples with `class="language-json"` on the `code` element. The optional local
script highlights these explicit languages using text nodes; unmarked prompts
and transcripts remain unchanged. Copying always preserves the example text.
Ctrl-K or Command-K focuses the server-backed search field. Previous/next links
and documentation feedback links are rendered by HSX and work without scripts.
Section navigation is also rendered on the server, including a collapsible
mobile table of contents. Every content heading must have a stable identifier.

The shared layout is `src/Documentation/Layout.hs`; request handling and local
search are in `src/Documentation/Application.hs`. Assets are in `public/`.
Only explicitly registered assets are served; arbitrary filesystem paths are
not exposed. `/llms.txt` lists plain-text exports derived from the same HSX
content under `/text/`, avoiding a second copy of the documentation.

## GHCi development

From the repository root, load the application without rebuilding an executable:

```sh
nix develop .#docs -c ghci -idocs/src docs/src/Documentation/Application.hs
```

In GHCi:

```haskell
import qualified Network.Wai.Handler.Warp as Warp
loadApplication "docs/public" >>= Warp.run 4321
```

Interrupt the server with Ctrl-C, use `:reload`, and run it again after edits.
Assets are read when `loadApplication` runs. For the Cabal executable entry
point, from `docs/` use `nix develop ..#docs -c cabal repl exe:documentation-server`.
This package has its own `cabal.project` and does not load the agent packages.

After changing the Cabal manifest, regenerate its Nix expression from `docs/`:

```sh
nix develop ..#docs -c cabal2nix . > package.nix
```

## Validation

From the repository root:

```sh
nix develop .#docs -c ghci -idocs/src docs/test/Main.hs -e main
nix build .#docs
```

The Haskell suite verifies routing, HTTP methods, search, escaping, text exports,
assets, and internal links/anchors. The Nix package also runs this suite.

For optional browser verification, start `nix run .#docs` in another terminal,
then run the Haskell browser checker with a local Chrome binary:

```sh
CHROME_EXECUTABLE='/Applications/Google Chrome.app/Contents/MacOS/Google Chrome' \
DOCUMENTATION_SCREENSHOTS="$TMPDIR/documentation-screenshots" \
nix develop .#docs -c env TMPDIR="$TMPDIR" runghc docs/scripts/VerifyDocumentationBrowser.hs
```

Set `DOCUMENTATION_URL` to test another address. Tests use a fresh browser
profile, exercise desktop/mobile layouts and interactions, and verify reading
and searching with JavaScript disabled. The checker talks directly to Chrome's
DevTools protocol; neither Python nor Playwright is required.

### Coverage regression checks

With the current documentation server running, compare published article text
with the CLI registries and configuration decoders:

```sh
nix develop .#docs -c runghc docs/scripts/VerifyDocumentationCoverage.hs --self-test
nix develop .#docs -c env TMPDIR="$TMPDIR" runghc docs/scripts/VerifyDocumentationCoverage.hs \
  --url http://127.0.0.1:4321 --export-examples "$TMPDIR/documentation-examples"
nix develop .#docs -c runghc docs/audit/VerifyAudit.hs
nix develop .#docs -c runghc docs/scripts/ExportInterfaceContracts.hs --check
```

The other script regression fixtures run without a browser or model server:

```sh
nix develop .#docs -c runghc docs/audit/VerifyAudit.hs --self-test
nix develop .#docs -c runghc docs/scripts/MaterializeInterfaceAudit.hs --self-test
nix develop .#docs -c runghc docs/scripts/RunConfigurationExamples.hs --self-test
nix develop .#docs -c runghc docs/scripts/VerifyLocalModel.hs --self-test
```

Preserving `TMPDIR` explicitly keeps the exported examples in the caller's
temporary directory rather than the development shell's temporary directory.
The coverage check detects absent names, not inadequate explanations or incorrect
behavior. The audit checker validates matrix structure and source citations,
not product behavior. Review the detailed audit rows as well as the exit status.
Tagged complete JSON examples are exported for the separate product-decoder
helper in `scripts/VerifyConfigurationExamples.hs`; ordinary JSON syntax checks
alone do not establish schema validity or endpoint compatibility.

Build that helper from the current repository sources and validate the exported
examples:

```sh
runtime=$(nix build .#agent-runtime --no-link --print-out-paths)
nix develop .#docs -c env TMPDIR="$TMPDIR" runghc docs/scripts/RunConfigurationExamples.hs \
  --runtime-package "$runtime" --examples "$TMPDIR/documentation-examples"
```

The runner compiles in a fresh temporary directory, runs positive and negative
decoder fixtures, then checks the manifest. The runtime package supplies matching
dependencies and generated path metadata; decoder modules compile from the working
tree. No global package database or Nix-store files are changed. Settings examples
are checked against the loaded values as well, so silent fallback cannot masquerade
as successful decoding. These checks do not exercise authentication or integrations.

Registry coverage includes CLI names and aliases, launch flags, machine/model/settings
decoder keys, and selected `agent-tools` JSON descriptors. It does not inventory every
provider-native or dynamically discovered MCP tool.

Mark complete configuration examples on their `<code>` element with
`data-config-schema="harness"`, `"models"`, or `"settings"`, matching the product
file being illustrated. Model examples are overlays on the checked-in defaults.
Do not tag tool arguments or protocol messages as configuration. Keep examples
self-contained, with placeholders instead of credentials; decoder acceptance
does not prove that an endpoint exists or that an executable is installed.

The native header is linked directly from the source repository, not duplicated.
The downloadable HTTP OpenAPI document is a verbatim copy of its repository
contract. After reviewing a contract change, regenerate it
with `nix develop .#docs -c runghc docs/scripts/ExportInterfaceContracts.hs`;
`--check` detects drift.
These references preserve the source contract's limitations rather than claiming
that loosely typed response objects have exhaustive schemas.
