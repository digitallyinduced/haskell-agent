# Documentation coverage audit

Date: 2026-09-22. Repository baseline:
`3100a05e06f23c442fe9b6c6b4c037a69c2ec668`, including the existing uncommitted
documentation application. Source references are repository-relative paths and
line numbers in this checkout, not claims about another release.

## Remediation delivered

The website now contains 40 pages, including expanded CLI and configuration
references, tool availability and lifecycle contracts, storage recovery,
deployment, REST/SSE, runtime daemon and native integration guides. The original
Missing, Conflict and Partial findings have source-backed documentation remedies.
The September 24 follow-up adds per-family native workflows, native media,
history variant contracts and sandbox/Darwin installation procedures.

Current reconciliation: **533 Covered, 0 Partial, 0 Missing, 0 Conflict** across
533 audit rows. Covered means the inventoried documentation contract is addressed,
not that every integration has run successfully. Open provider payloads remain
open; external native applications and deployed infrastructure still require
host-specific acceptance testing.

Current validation includes 185 Haskell website tests, desktop/mobile browser
checks across 40 pages, 15 coverage-check fixtures and all 33 accepted history
tags. The native C example passes syntax checking, and the Haskell history
consumer passes eight display fixtures. Earlier validation also includes
registry-derived presence checks and 11 rendered configuration examples accepted
by the actual product decoders with positive/negative fixtures. The bundled OpenAPI
contract is checked byte-for-byte against its repository source; the native C
header is linked directly from the source repository rather than duplicated.
These checks do not execute authenticated provider, deployment, native-host,
database recovery or externally mutating workflows. See `docs/README.md` for
repeatable commands and individual rows for remaining acceptance boundaries.

## Original audit conclusion and remediation priorities

The conclusion and priorities below describe the pre-remediation audit. The
individual matrices are being updated with source-backed remediation evidence;
their current row statuses, rather than this historical summary, describe the
remaining documentation gaps. Run `VerifyAudit.hs` for current counts. Coverage
does not imply that account-dependent or destructive workflows were executed.

**No: the website does not yet cover all implemented user-facing capabilities.**
It is strongest on onboarding, ordinary terminal use, and core machine/model
configuration. Naming commands is substantially more complete than explaining
their operational lifecycle. Broader server/embedding surfaces are mostly absent
from the website, although substantial engineering documentation already exists.

Recommended work, in order:

1. **Correct misleading claims first.** WEB-008: consolidate the local-model
   tutorial's confusing stage-specific verification statements. Preserve the
   distinction between successful API tests and the unexecuted full-harness task.
   CLI-O21 contradicts the desktop-control default: a TTY alone is not
   sufficient; one-shot invocations are excluded unless explicitly enabled.
2. **Complete safety and recovery instructions.** Expand rewind/worktree recovery,
   scheduled work cancellation, persistent settings and account recovery. Include
   what survives interruption and how to inspect state before retrying mutations.
   See [CLI priorities](cli.md), SET-CFG and AUTH-CFG rows in
   [configuration](configuration.md), and TOOL-046–048 in [tools](tools.md).
3. **Document missing terminal capability families.** Structured memory
   (TOOL-053–055), durable independent sessions (TOOL-032 and adjacent rows),
   persistent GHCi (TOOL-007), code-mode orchestration (TOOL-017), and
   dialect-specific goal/workflow operations need usable examples and limits.
4. **Publish an availability matrix before widening the guides.** TOOL-112:
   distinguish CLI, native host, provider, dialect and feature switches. Browser
   bridge functions are not proof of a shipped browser client; mail library code
   is not proof of a locally available model tool.
5. **Fill configuration and authentication reference gaps.** The finite machine
   and model decoders are comparatively well documented. The larger omissions
   are persisted `settings.json`, inheritance/reset behavior, transport environment
   overrides and multi-account recovery; see [configuration priorities](configuration.md).
6. **Promote operator material where the surface is supported.** Reconcile existing
   server/daemon engineering notes with source, then publish setup, authentication,
   tenant isolation, streaming and recovery guides. Complete Telegram delivery
   recovery and NixOS options, especially `mcpServers = null` versus an empty
   attribute set (IF-DEPLOY-12). Native application UX requires its own source
   repositories. See [interface scope and priorities](interfaces.md).
7. **Make completeness testable.** WEB-009/010: validate examples with product
   decoders and compare documentation inventories to actual registries. Keep
   per-example execution records; passing page tests is not feature verification.

These are documentation recommendations, not fixes applied by this audit.
Each detailed row records the concrete acceptance boundary. Broader API/native
coverage is a public-support scope decision, not automatically a defect in a
terminal-focused guide.

## Scope and method

This audit works from implemented user-facing entry points toward the website,
not from the existing navigation toward a guessed feature list. It inventories
command parsers and registries, configuration decoders, tool registrations,
public protocol handlers, and deployment outputs. Existing tests and engineering
notes corroborate behavior. The audit does not execute account-dependent or
destructive workflows.

The assessed documentation surface is the HSX website registered by
`docs/src/Documentation/Content.hs:33`. Repository Markdown notes and README
instructions are identified separately. They can be valuable existing material
without being available to a website reader or the website search engine.

“Exhaustive” here means a systematic entry-point inventory within this repository,
with explicit exclusions below. It does not mean every internal Haskell function,
every possible natural-language task, every provider response, or every
combination of failures has been exercised. Individual matrices state where
aliases, operation families, or settings are grouped.

## Coverage matrices

- [CLI commands and interaction](cli.md): launch options, subcommands, slash
  commands, keyboard behavior, session and worktree operations.
- [Configuration and providers](configuration.md): machine settings, catalog
  fields, environment overrides, credentials, and MCP configuration.
- [Tools and reusable workflows](tools.md): model-facing operations, approval
  boundaries, skills, memory, browser/computer use, and agent coordination.
- [Other interfaces and deployment](interfaces.md): native bridge, daemon,
  server/client, integration API, Telegram, voice/WebRTC, and Nix deployment.
- [Website and verification](website.md): hosting, navigation, exports,
  documentation checks, and verification inconsistencies.

Each row supplies an identifier, source evidence, existing documentation,
coverage judgment, concrete omission, and verification boundary. References to
tests mean they were inspected unless an executed result is explicitly stated.

| Status | Meaning |
| --- | --- |
| Covered | Usable instructions describe the named operation at the row's stated scope; not a certification of every edge case. |
| Partial | The operation is mentioned or partly explained, but important setup, behavior, limits, persistence, or recovery information is absent. |
| Missing | No usable published website instructions were found; engineering-only material may exist and is noted. |
| Conflict | Published claims contradict the implementation or another recorded statement. |

Counts are audit rows, not distinct product functions. For example, configuring
an MCP server and invoking one of its tools are separate user concerns, while
aliases may share a row. Do not convert the totals into a purported percentage
of the entire product documented.

## Original counted results

| Matrix | Covered | Partial | Missing | Conflict | Rows |
| --- | ---: | ---: | ---: | ---: | ---: |
| CLI | 47 | 94 | 18 | 1 | 160 |
| Configuration/providers | 74 | 28 | 30 | 0 | 132 |
| Tools/workflows | 30 | 50 | 40 | 0 | 120 |
| Interfaces/deployment | 8 | 15 | 86 | 0 | 109 |
| Website/verification | 3 | 7 | 2 | 0 | 12 |
| **Total** | **162** | **194** | **176** | **1** | **533** |

These are grouped, sometimes overlapping coverage judgments, not 533 distinct
functions. Missing means missing from the website, not necessarily from all
repository documentation. Counts describe this dated snapshot.

## Package accounting

All 33 package directories were assigned a disposition so that lack of a website
page is not confused with a missing implementation:

| Package group | Audit disposition |
| --- | --- |
| `agent-cli`, `agent-tui`, `agent-repository`, `agent-external-session` | CLI interactions, session/worktree behavior, historical-session tools. |
| `agent-accounts`, `agent-claude`, `agent-gemini`, `agent-openai`, `agent-openrouter`, `agent-xai`, `claude-agent-sdk-haskell` | Authentication, provider configuration and capabilities; SDK implementation details are not separate end-user functions. |
| `agent-tools`, `agent-computer-use`, `agent-mail`, `agent-mcp` | Tools, permissions, workflow and connection entry points. |
| `agent-native-bridge`, `agent-runtime-daemon`, `agent-server`, `agent-server-client`, `agent-integration-api`, `agent-telegram`, `agent-webrtc` | Non-CLI protocol/deployment surfaces; availability is distinguished from a finished native/web/mobile application. |
| `agent-core`, `agent-runtime`, `agent-store`, `agent-process`, `agent-connectivity` | User-visible persistence, lifecycle, configuration and execution behavior traced through the entry points above; internal helpers are excluded. |
| `agent-codex-dialect`, `agent-grok-build-dialect`, `agent-responses`, `agent-responses-types` | Dialect-specific tools and provider behavior; internal wire types are not separate documentation requirements. |
| `agent-json`, `agent-syntax` | Internal serialization/highlighting support; no standalone end-user surface counted. |

## Verification and exclusions

- This pass verifies source/document correspondence, not the behavior of every
  running feature. It does not connect accounts, send messages, change security
  permissions, start infrastructure, or rerun inference.
- Earlier website tests establish rendering/routing and selected browser
  interactions. JSON syntax checks do not establish product decoder acceptance.
- `docs/local-model-verification.md` records successful local API generation and
  function-result replay. It explicitly does not verify the harness file-reading
  exercise. The confusing tutorial verification wording is tracked as WEB-008.
- Third-party MCP servers and arbitrary installed skills are open-ended. The
  harness integration and bundled skills are in scope; every remote service's
  independent functions are not.
- Native application repositories, hosted gateway implementations, and platform
  release processes not present here cannot be audited from this checkout.
  Protocol support is not proof that an end-user application ships.
- Benchmarks, compiler internals, development-only diagnostics, vendored SDK
  internals, and every internal database column are not a user-guide checklist.
- Failure combinations remain a verification backlog. A source inventory cannot
  prove behavior under every network outage, account expiration, concurrent
  modification, operating system, or terminal emulator.

## Maintaining the audit

Keep existing identifiers stable. When closing a gap, add the owning website
section and its validation evidence, then update the status. Do not mark a row
Covered merely because a command name was added to a table.

Run the artifact checker from the repository root:

```sh
nix develop .#docs -c runghc docs/audit/VerifyAudit.hs
```

It checks row identifiers, status presence, and repository line citations, and
recomputes counts. It does not determine semantic completeness. Before claiming
an exhaustive inventory after code changes, re-read the enumerated registries;
the checker alone does not discover new product features.
