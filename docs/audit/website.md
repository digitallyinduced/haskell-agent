# Website and verification coverage

Audit date: 2026-09-22. Source baseline: `3100a05e06f23c442fe9b6c6b4c037a69c2ec668`
plus the existing documentation working tree. This is a source/document audit,
not a new execution of product workflows.

## Remediation evidence

The published `/guides/documentation/` page now covers build/start/readiness,
all three server variables and startup failure recovery, a labeled systemd
example, root-mounted TLS proxying, retained releases and rollback, exact search
semantics, reading controls, exports, and all page/asset registration locations.
These resolve WEB-001 through WEB-004 and WEB-007 as documentation omissions;
the service example has not been deployed in this session.

WEB-008 is corrected in `Documentation.Pages.LocalModelTutorial`: local API
streaming/function replay passed in the dated record, while the separate
Haskell Agent file-reading exercise remains explicitly unexecuted.
No additional model verification is claimed.

## Method and scope

Read the documentation application, entry point, route registration, Haskell
tests, browser checks, maintenance instructions, and recorded local-model and
terminal verification. The rows below concern the documentation product and
the reliability of its examples. Other audit files cover harness behavior.

Covered means usable instructions for the named operation exist. It does not
mean that an example was executed. Maintenance-only instructions are identified
explicitly; they are not counted as published website guides.

| ID | Function or use case | Source evidence | Existing documentation | Coverage | Missing details or required correction | Validation evidence |
| --- | --- | --- | --- | --- | --- | --- |
| WEB-001 | Build and launch the documentation server | `docs/app/Main.hs:12`; `flake.nix:1503` | `/guides/documentation/#start-locally` | Covered | Published foreground startup, executable output and second-terminal readiness check. | Source review; earlier Nix build recorded in session. |
| WEB-002 | Configure host, port, and asset directory | `docs/app/Main.hs:14` | `/guides/documentation/#server-settings`, upgrade-and-recover | Covered | Variables, defaults, port bounds, exposure warning, asset restart and startup failure diagnosis published. | Source only. |
| WEB-003 | Deploy behind TLS at the domain root | `docs/src/Documentation/Application.hs:49`; `docs/README.md:76` | `/guides/documentation/#supervise-service`, reverse-proxy, upgrade-and-recover | Covered | Labeled systemd example, root proxy, asset/readiness checks and retained-release rollback published. Certificate provisioning remains operator-owned. | No deployment test recorded. |
| WEB-004 | Search guides locally without JavaScript | `docs/src/Documentation/Application.hs:67` | `/guides/documentation/#find-a-guide` | Covered | All-term substring behavior, title priority, empty query and 200-character cap explained. | `docs/test/Main.hs:105`; `docs/scripts/verify_documentation_browser.py:73` exercise search. |
| WEB-005 | Read text exports and agent index | `docs/src/Documentation/Application.hs:49`; `docs/src/Documentation/Application.hs:93` | `docs/README.md`, Edit pages; visible site export/index links | Covered | No omission for the basic export operation. These are plain-text exports, not a separate Markdown content source. | Haskell routing/index/link tests; browser checks export link, not all exported content semantics. |
| WEB-006 | Navigate, select theme, copy examples, and use mobile layout | `docs/scripts/verify_documentation_browser.py:24` | Visible controls; `docs/README.md`, Edit pages | Covered | Basic operations exposed and tested. This does not establish screen-reader compatibility or every browser/viewport combination. | Remediation browser pass: 39 pages, 47 JSON examples, mobile overflow, section navigation, server search, themes, copy, text exports, and JavaScript-disabled reading/search. |
| WEB-007 | Add or update an HSX page and packaged assets | `docs/src/Documentation/Content.hs:33`; `docs/src/Documentation/Application.hs:24` | `/guides/documentation/#maintain-pages` | Covered | Content/Cabal module and asset/test registration locations explicitly named. | Source inspection. |
| WEB-008 | Know which product examples have actually passed | `docs/src/Documentation/Pages/LocalModelTutorial.hs:19`; `docs/src/Documentation/Pages/LocalModelTutorial.hs:115` | Local-model Verification scope; `docs/local-model-verification.md` | Covered | Consolidated stage-specific evidence: API passed; separate harness README exercise unexecuted. | Existing dated record; no inference rerun. |
| WEB-009 | Validate JSON examples against accepted product configuration | `docs/scripts/VerifyConfigurationExamples.hs:1` | `docs/README.md`, Validation | Covered | Tagged configuration examples exercise current harness, model-catalog and persisted-settings decoders, with positive and negative fixtures. This does not validate remote endpoints or execute example workflows. | Eleven rendered configuration examples accepted by actual product decoders; repeatable runner in `docs/scripts/verify_configuration_examples.py`. |
| WEB-010 | Detect undocumented new commands, fields, and tools automatically | `docs/scripts/verify_documentation_coverage.py:1` | `docs/README.md`, Validation | Covered | Registry-derived checks cover slash commands/aliases, launch options, machine/model/settings fields and agent-tools JSON descriptors. Dynamic MCP catalogs and other host/provider tool registries are explicitly outside this finite check. Presence is not prose-quality validation. | All checked names present: 87 commands/aliases, 32 launch options, 45 machine keys, 20 model keys, 14 persisted settings and 24 tool descriptors. |
| WEB-011 | Recognize capture provenance and version differences | `docs/public/interactive-terminal-captures.txt:1`; `docs/src/Documentation/Pages/Authentication.hs:22` | Authentication captions and accessible transcript | Covered | Basic provenance is explicit, including older build and standalone/fullscreen differences. Only login screens are captured; these cannot verify task/approval/review interactions. | Existing tmux capture record, not a fresh terminal run. |
| WEB-012 | Find engineering-only documentation through website search | `docs/src/Documentation/Application.hs:68`; `docs/src/Documentation/Content.hs:33` | `/guides/documentation/#find-a-guide`, `/guides/deployment/`, `/reference/server/`, `/reference/runtime-daemon/`, `/reference/native-integration/` | Covered | User-facing operator procedures are selectively published and indexed; design notes remain explicitly separate. Interface-specific depth is tracked in interfaces.md rather than claiming all engineering notes are public guides. | Haskell route, search and link checks; source-reviewed promotion, not deployment verification. |

## Acceptance gates

1. Resolve WEB-008 without upgrading API verification into harness verification.
2. Make configuration examples exercise actual decoders, and command/tool
   completeness checks exercise their actual registries.
3. Record per-example prerequisites, revision, execution result, and untested
   boundaries. Website rendering success is not product behavior success.
4. Publish operational instructions only for supported deployment surfaces.
   Keep engineering internals identifiable and separate from user procedures.

## Limits

No accessibility certification, load test, fresh deployment, model inference,
authentication, microphone permission, external mutation, or account connection
was performed during this audit. Their absence is not evidence of a product bug.
