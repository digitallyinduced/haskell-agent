# CLI, command, and session coverage audit

Date: 2026-09-22. Source baseline: `3100a05e06f23c442fe9b6c6b4c037a69c2ec668`, including the current uncommitted documentation.

## Method and boundaries

This is a source-to-documentation audit, not an execution report. Every row below has **source inspection only** as its verification level. Existing automated documentation route/browser checks do not establish that a product command or recovery procedure works. No accounts, sessions, checkouts, or provider calls were modified during this audit.

The inventory exhausts the static `agent-cli` run-option parser, its administrative subcommands, and all 67 canonical entries of the slash-command catalog. Aliases are listed alongside their canonical command. Keyboard coverage inventories the fullscreen composer and main navigation functions and the separate inline editor; widget-specific account/MCP/agent selectors remain grouped behavior inventories rather than an exhaustive terminal-byte sequence listing. Dynamic third-party skill names cannot be statically enumerated.

Coverage means:

- **Covered**: a reader can carry out the ordinary use case from the documentation, with its important safety boundary.
- **Partial**: the command is mentioned, but arguments, outcomes, limitations, recovery, or important branches are absent.
- **Missing**: no usable documentation for the particular function.
- **Conflict**: documentation makes a statement contradicted by inspected implementation.

The counted rows are audit units, not distinct implementation functions; workflow rows deliberately revisit interactions across individual commands. Do not interpret a row percentage as a measured percentage of the entire product.

### Source reference abbreviations

References such as `O:585` mean the following repository-relative file at that line:

| Reference | File |
| --- | --- |
| O | `packages/agent-cli/src/Agent/CLI/Options.hs` |
| C | `packages/agent-cli/src/Agent/CLI/Command/Catalog.hs` |
| P | `packages/agent-cli/src/Agent/CLI/Command.hs` |
| I | `packages/agent-cli/src/Agent/CLI/Command/Instructions.hs` |
| S | `packages/agent-cli/src/Agent/CLI/Runtime/Repl/Session.hs` |
| A | `packages/agent-cli/src/Agent/CLI/Afk.hs` |
| K | `packages/agent-cli/src/Agent/CLI/TUI/Composer.hs` |
| B | `packages/agent-cli/src/Agent/CLI/TUI/Bridge.hs` |
| D | `packages/agent-cli/src/Agent/CLI/Input/KeyDecoder.hs` |

Documentation references name files under `docs/src/Documentation/Pages/` and their HTML section identifiers. `Commands#launch-options`, for example, means `Commands.hs`, section `launch-options`.

## Run-option inventory

The parser has 32 run-option names, plus the two global help/version switches handled before parsing. Both positive and negative switches are included below, with equivalent pairs grouped. The internal managed-turn flags are inventoried but should be explicitly designated internal rather than encouraged as public automation.

| ID | Option/function | Source | Documentation | Coverage | Missing detail or assessment |
| --- | --- | --- | --- | --- | --- |
| CLI-O01 | `--provider NAME` | O:587 | Commands#launch-options; Providers#provider-identifiers | Covered | Supported identifiers and explicit selection documented. |
| CLI-O02 | `--model NAME` | O:590 | Commands#launch-options; Models | Covered | Catalog identifier and saved selection distinction documented. |
| CLI-O03 | `--cwd DIR` | O:592 | Commands#launch-options; Installation#open-a-project | Covered | Example and working-directory purpose provided. |
| CLI-O04 | `--worktree` | O:594; O:790 | ParallelAgents#repair-the-worktree-base | Covered | Source-checked: Git-directory prerequisite, resume exclusion, fetch-error repair and base policy documented. |
| CLI-O05 | `--yolo`, `--no-yolo` | O:596–599; O:284 | Commands#one-shot-automation; Approvals | Covered | Explicit approval boundary and safe non-TTY example provided. |
| CLI-O06 | `--managed-deny-mutations` | O:600 | Commands#argument-contract | Covered | Named and explicitly designated internal, distinct from public automation policy; source-only verification. |
| CLI-O07 | `--max-turns N` | O:605; O:786; `packages/agent-core/src/Agent/Loop.hs:80` | Commands#argument-contract; Commands#one-shot-automation | Covered | Positive validation, default 2000, bounded example and distinction from elapsed/cost limits; source-only verification. |
| CLI-O08 | `--max-concurrent-agents N` | O:608 | ParallelAgents#delegate-independent-tasks; Configuration#limit-concurrent-agents | Covered | Launch/session controls and precedence covered. |
| CLI-O09 | `--compact-threshold N` | O:612 | Models#automatic-compaction | Covered | Source-verified provider defaults, caps, unsupported automatic paths and numerical example supplied by provider remediation. |
| CLI-O10 | `--effort LEVEL` | O:616 | Models#reasoning-controls | Covered | Consolidated source-verified defaults, Grok startup normalization versus interactive rejection and endpoint limits. |
| CLI-O11 | `--show-raw-reasoning` | O:618 | Models#reasoning-controls | Covered | Summary versus additional supplied text example; cannot reconstruct or expose absent reasoning. |
| CLI-O12 | `-p`, `--prompt TEXT` | O:620 | Commands#argument-contract; Commands#one-shot-automation | Covered | Long alias and all prompt-source exclusions documented; source-only verification. |
| CLI-O13 | `--prompt-file FILE` | O:627; O:778–785; `packages/agent-runtime/src/Agent/Runtime/ManagedTurn.hs:145–148` | Commands#one-shot-automation; Commands#local-command-boundaries | Covered | Source-checked text decoding, whitespace trimming, input failures and prompt-source exclusions. |
| CLI-O14 | `--managed-turn-file FILE` | O:630; `packages/agent-runtime/src/Agent/Runtime/ManagedTurn.hs:108–204` | Commands#managed-turn-payload | Covered | Version/defaults, required/optional context and media fields, local byte loading, trusted producer and failure recovery documented; source inspection only. |
| CLI-O15 | `--resume ID` | O:634; O:790 | Sessions#resume-previous-work; Sessions#resume-troubleshooting; Commands#argument-contract; ParallelAgents#restore-a-checkout | Covered | Source-checked ID selection, worktree exclusion, missing checkout restoration and path-collision safety. |
| CLI-O16 | `--save-session` | O:636 | Commands#one-shot-automation; Sessions#export-a-conversation | Covered | One-shot persistence distinction explicitly explained. |
| CLI-O17 | `--agents-md`, `--no-agents-md` | O:638–641 | Commands#launch-defaults; Projects | Covered | Discovery switch and default documented. |
| CLI-O18 | `--skills`, `--no-skills` | O:642–645 | Commands#launch-defaults; Skills | Covered | Discovery default and disable operation documented. |
| CLI-O19 | `--ghci`, `--no-ghci` | O:646–649 | Commands#launch-defaults; Tools | Covered | Optional tool and disabled default documented. |
| CLI-O20 | `--bash`, `--no-bash` | O:650–653 | Commands#launch-defaults; Tools | Covered | Explicit shell execution availability documented. |
| CLI-O21 | `--computer-use`, `--no-computer-use` and default | O:272–278; O:654–662 | Commands#launch-defaults | Covered | Corrected: default requires a TTY and non-one-shot interactive launch. Explicit TTY `-p` counterexample; source-only verification. |
| CLI-O22 | `--code-mode`, `--no-code-mode` | O:663–666 | Commands#launch-defaults | Covered | Catalog precedence and model-facing purpose described. |
| CLI-O23 | `--fullscreen`, `--minimal` | O:667–670; `packages/agent-cli/src/Agent/CLI/Runtime/Orchestration/Flow.hs:971–975` | Terminal#choose-an-interface; Commands#local-command-boundaries | Covered | Source-checked stdin/stdout TTY and non-one-shot gating even for explicit fullscreen. |
| CLI-O24 | `--motion full/reduced/off` | O:671; O:799; `packages/agent-tui/src/Agent/TUI/Motion.hs:145–198` | Commands#local-command-boundaries | Covered | Source-checked static versus animated indicators, refresh distinction and launch examples. |
| CLI-O25 | `--help`, `-h` | O:318 | Commands introduction | Covered | User knows where to obtain option help. |
| CLI-O26 | `--version` | O:319 | Commands#argument-contract | Covered | Invocation and bug-report purpose documented; source-only verification. |
| CLI-O27 | Repeated options, ordering, invalid combinations | O:580; O:778–797 | Commands#argument-contract | Covered | Last assignment, positive integers and incompatible sources documented; source-only verification. |

## Administrative commands

There are six top-level administrative groups: `login`, `gateway`, `sessions`, `mcp`, `storage`, and `worktree`. Their 24 executable leaves are individually listed; bare `sessions` is an additional alias for human-readable listing.

| ID | Command/function | Source | Documentation | Coverage | Missing detail or assessment |
| --- | --- | --- | --- | --- | --- |
| CLI-A01 | `login` | O:389 | Authentication#connect-interactively; Commands#gateway-and-maintenance | Covered | Standalone invocation, actual empty-account capture, and chooser are available. |
| CLI-A02 | `gateway connect --url HTTPS_URL` | O:436 | Providers#gateway-command-lifecycle | Covered | Source-verified URL constraints, browser/device code authorization, credential location, MCP reconciliation and failure recovery. |
| CLI-A03 | `gateway status` | O:445 | Providers#gateway-command-lifecycle | Covered | Connected/disconnected/error output distinguished; local credential status is not server-health evidence. |
| CLI-A04 | `gateway disconnect` | O:448 | Providers#gateway-command-lifecycle | Covered | Local credential and gateway MCP removal, runtime invalidation, status verification, direct-provider restart and remote revocation distinction. |
| CLI-A05 | `sessions [list] [--json]` | O:453–465; `packages/agent-cli/src/Agent/CLI/SessionAdmin.hs:133–157`; `packages/agent-store/src/Agent/Store/Postgres/Session/Read.hs:857–865` | Commands#session-json | Covered | Source-checked fields, ordering, archived/deleted filtering, working-directory scope and metadata warnings. |
| CLI-A06 | `sessions show ID [--json] [--before INDEX] [--limit N]` | O:358–384; O:762–775; `packages/agent-cli/src/Agent/CLI/SessionAdmin.hs:159–240` | Commands#session-json | Covered | Source-checked schema, default/maximum, nonnegative exclusive index, JSON-only restriction and final-page detection. |
| CLI-A07 | `sessions wait ID` | O:469; `packages/agent-cli/src/Agent/CLI/SessionAdmin.hs:483–493` | Commands#session-subcommands | Covered | Source-checked lifetime-lock polling, silent return, missing-directory error, no task-success guarantee. |
| CLI-A08 | `sessions import [--cwd DIR]` | O:477,490; `packages/agent-cli/src/Agent/CLI/SessionAdmin.hs:495–511`; `packages/agent-runtime/src/Agent/Runtime/Session/Types.hs:182–208,327–376,423–447`; `packages/agent-cli/src/Agent/CLI/Afk.hs:70–103` | Commands#session-transfer | Covered | Source-checked raw-transfer structure, producer/consumer AFK pipeline, version coupling, ID collision refusal, cwd-only override and recovery verification. |
| CLI-A09 | `mcp list [--json]` | O:516; `packages/agent-cli/src/Agent/CLI/McpCatalog.hs:89–97,236–237,282–293` | Commands#mcp-subcommands; Mcp#configure-from-the-command-line | Covered | JSON array fields, disabled entries, nullability, secret-value omission, empty/error distinction and saved-versus-live semantics documented; source inspection only. |
| CLI-A10 | `mcp add NAME [-t/--transport stdio/http] COMMAND_OR_URL [ARGS]` | O:550–575 | Commands#mcp-subcommands; Mcp#configure-from-the-command-line | Covered | Transport inference, delimiter and local/remote examples provided. |
| CLI-A11 | `mcp enable NAME` | O:520 | Commands#mcp-subcommands; Mcp | Covered | Runtime restart and persisted configuration distinction provided. |
| CLI-A12 | `mcp disable NAME` | O:524 | Commands#mcp-subcommands; Mcp | Covered | Disable versus removal and runtime refresh described. |
| CLI-A13 | `mcp login URL [--scope SCOPE]...` | O:532–541; `packages/agent-runtime/src/Agent/Runtime/McpOAuth.hs:223–251`; `packages/agent-mcp/src/Agent/MCP/OAuth.hs:770–797` | Commands#mcp-oauth-identity | Covered | Repeated scope union, issuer/resource reuse boundaries, offline_access advertised-only, consent and read verification documented; source inspection only. |
| CLI-A14 | `mcp logout URL` | O:543; `packages/agent-runtime/src/Agent/Runtime/McpOAuth.hs:506–511`; `packages/agent-runtime/src/Agent/Runtime/McpOAuthStore.hs:27–33` | Commands#mcp-oauth-identity | Covered | Exact-string local credential identity, missing-file no-op, no remote revocation/config removal, running-process cache/restart and external-effect boundaries documented; source inspection only. |
| CLI-A15 | `storage status` | O:504; `packages/agent-cli/src/Agent/CLI/Database/Storage.hs:53–96` | Commands#storage-diagnostics; Troubleshooting#storage-diagnostic-output | Covered | Exact state strings and status-versus-doctor distinction, initialization/stopped/running recovery documented; source inspection only. |
| CLI-A16 | `storage start` | O:505; `packages/agent-store/src/Agent/Store/Postgres/Managed.hs:90–144,234–241`; `packages/agent-store/src/Agent/Store/Postgres/Config.hs:55–72,123–130` | Commands#storage-subcommands; Commands#storage-diagnostics; Troubleshooting#storage-diagnostic-output | Covered | PostgreSQL18/owner/writability prerequisites, initialize/start/migrate behavior, log location, Unix-socket versus TCP conflict and version failure recovery documented; source inspection only. |
| CLI-A17 | `storage stop` | O:506 | Commands#storage-subcommands | Covered | Effect and other-agent warning provided. |
| CLI-A18 | `storage migrate` | O:507; `packages/agent-cli/src/Agent/CLI/Database/Storage.hs:69–74` | Commands#storage-migration-recovery; Commands#storage-diagnostics; Troubleshooting#storage-migration-recovery | Covered | Preflight maintenance/verified backup, auto-start and success string, connection/session verification, preserve failure evidence and no automatic rollback/major upgrade documented; doctor does not verify migrations; operational guidance, not an executed recovery test. |
| CLI-A19 | `storage doctor` | O:508; `packages/agent-cli/src/Agent/CLI/Database/Storage.hs:75–92` | Commands#storage-diagnostics; Troubleshooting#storage-diagnostic-output | Covered | Exact healthy prefix/socket and stopped/uninitialized resolution; connection-pool check is not full integrity audit or migration verification; source inspection only. |
| CLI-A20 | `worktree gc [--dry-run] [--inactivity-days DAYS]` | O:412–421 | Commands#gateway-and-maintenance; ParallelAgents#retention-and-recovery | Covered | Safe preview, default, positive override, protection against expiring unmerged work explained. |
| CLI-A21 | `worktree enroll PATH` | O:422; `packages/agent-cli/src/Agent/CLI/Worktree.hs:307–324,378–414` | ParallelAgents#enroll-an-existing-checkout | Covered | Source-checked linked managed-root eligibility, reciprocal metadata, symlink/primary/active-lease rejections, idempotence and inactivity-clock semantics. |
| CLI-A22 | `worktree restore PATH` | O:423 | ParallelAgents#restore-a-checkout | Covered | Source/README-checked restore command and original-repository, path collision, detached HEAD and moved-branch boundaries. |
| CLI-A23 | `worktree protect PATH` | O:424 | ParallelAgents#retention-and-recovery | Covered | Concrete protection use case and command. |
| CLI-A24 | `worktree unprotect PATH` | O:425 | Commands#gateway-and-maintenance | Covered | Effect on collection clearly stated alongside policy. |

## Complete static slash-command catalog

This table enumerates every canonical entry of `C:10–76`, in registry order. Source references to C locate the registration and usage contract; P and S provide additional behavior evidence for gaps. An alias not mentioned in the site is noted even when the primary command is usable.

Follow-up: `Commands#command-aliases` now lists all 20 registered aliases with their canonical command and shared argument/availability rules. Alias omissions and subsequent behavioral gaps from the original assessments are resolved by the sections recorded below.

| ID | Command and aliases | Source | Documentation | Coverage | Missing detail or assessment |
| --- | --- | --- | --- | --- | --- |
| CLI-S01 | `/help [NAME]` | C:10; P:441 | Commands introduction | Covered | Discovery and per-command help explained. |
| CLI-S02 | `/init` | C:11; I:17; `packages/agent-cli/src/Agent/CLI/Runtime/Repl/Commands.hs:952–976` | Commands#initialize-and-update | Covered | Source-checked existing-entry preservation, generated outline, variable output and review/verification boundary. |
| CLI-S03 | `/review [INSTRUCTIONS]` | C:12 | Terminal#inspect-and-reuse-output; Tutorials | Covered | Review use case, scope and inspection included. |
| CLI-S04 | `/diff` | C:13 | Terminal#inspect-and-reuse-output | Covered | Includes untracked files and practical review step. |
| CLI-S05 | `/fork [--worktree/--no-worktree] [DIRECTIVE]` | C:14; P:553; S:215–244; S:620–690 | ParallelAgents#branch-the-conversation | Covered | Source-checked default dialog selection, cancellation, peer switch/directive and original-ID return/integration procedure. |
| CLI-S06 | `/export [PATH]` | C:15; `packages/agent-cli/src/Agent/CLI/Runtime/Repl/Transcript.hs:95–206`; `packages/agent-cli/src/Agent/CLI/TranscriptExport.hs:107` | Sessions#export-a-conversation | Covered | Source-checked chooser, default filename, relative paths, no-clobber, persisted-session requirement, error recovery and sensitive export boundary. |
| CLI-S07 | `/history` | C:16; `packages/agent-cli/src/Agent/CLI/Runtime/Repl/Commands.hs:819–853` | Terminal#reuse-and-edit-prompts | Covered | Source-checked shared history, filter/select, draft-only reuse, empty-history outcome and cancellation. |
| CLI-S08 | `/find [TEXT]` | C:17; `packages/agent-cli/src/Agent/CLI/Transcript.hs:112–125`; `packages/agent-cli/src/Agent/CLI/Runtime/Repl/Transcript.hs:488–506` | Terminal#pager-search | Covered | Source-checked empty-query full transcript, case-insensitive block filtering, pager controls and error outcomes. |
| CLI-S09 | `/permissions` | C:18 | Approvals#choose-the-approval-policy; Commands#configuration-and-tools | Covered | Exact three choices, p/r/f keys, confirmation/cancel, session versus persistent project scope and launch overrides documented; source-verified by approvals remediation. |
| CLI-S10 | `/model [NAME]`, `/m` | C:19 | Models | Covered | Selection and identity checks provided. |
| CLI-S11 | `/title-model [NAME/--auto]` | C:20 | Sessions#give-a-session-a-useful-title | Covered | Independent naming model and automatic restore described. |
| CLI-S12 | `/theme [NAME]`, `/t` | C:21; `packages/agent-cli/src/Agent/CLI/Runtime/Repl/Selection.hs:583–627`; `packages/agent-core/src/Agent/Theme.hs:13–53` | Terminal#themes | Covered | Source-checked six choices, direct example, fullscreen requirement, live preview, cancellation and saved preference. |
| CLI-S13 | `/mouse [on/off]` | C:22; P:352 | Terminal#select-text-with-the-mouse | Covered | Native-selection tradeoff and saved preference explained. |
| CLI-S14 | `/effort [LEVEL]` | C:23 | Models#reasoning-controls | Covered | No-argument picker, provider defaults and unsupported interactive versus startup behavior supplied. |
| CLI-S15 | `/fast` | C:24; P:100–110; `packages/agent-cli/src/Agent/CLI/Runtime/Repl/Selection.hs:352–377` | Commands#local-command-boundaries | Covered | Source-checked priority/default toggle, matching model metadata requirement, unsupported result and pricing caution; no external billing verification claimed. |
| CLI-S16 | `/plan [description]` | C:25 | Projects; Approvals | Covered | Plan-only editing boundary and approval flow described. |
| CLI-S17 | `/view-plan`, `/show-plan`, `/plan-view` | C:26; `packages/agent-cli/src/Agent/CLI/Runtime/Repl/Commands.hs:747–762` | Commands#local-command-boundaries | Covered | Source-checked saved Markdown display, empty-plan outcome and view versus approve distinction. |
| CLI-S18 | `/queue [prompt]` | C:27; P:358; K:389–429 | Terminal#steer-or-queue-a-follow-up; Terminal#attach-images | Covered | Source-checked post-turn queue path, attachment exception, inspection and explicit oldest-prompt promotion/cancellation boundary. |
| CLI-S19 | `/steer <prompt>` | C:28; K:418–429 | Terminal#steer-or-queue-a-follow-up | Covered | Text steering at model boundary distinguished from immediate tool interruption; attached prompts use normal queue path. |
| CLI-S20 | `/transcript`, `/log` | C:29; `packages/agent-cli/src/Agent/CLI/Runtime/Repl/Transcript.hs:356–382` | Terminal#pager-search | Covered | Source-checked saved transcript, PAGER/less selection, search/quit controls and view versus export distinction. |
| CLI-S21 | `/edit-prompt` | C:30; `packages/agent-cli/src/Agent/CLI/Runtime/Repl/Commands.hs:1361–1395` | Terminal#reuse-and-edit-prompts | Covered | Source-checked VISUAL/EDITOR/vi order, draft-only result, failed-editor handling and editor-controlled discard. |
| CLI-S22 | `/context` | C:31; `packages/agent-cli/src/Agent/CLI/Context.hs:27–108` | Sessions#inspect-session-state | Covered | Source-checked metric fields, illustrative estimate, occupancy validity and unknown-window treatment. |
| CLI-S23 | `/btw <QUESTION>` | C:32; `packages/agent-cli/src/Agent/CLI/Btw.hs:83–96`; `packages/agent-cli/src/Agent/CLI/Session/Interaction.hs:190–240` | Commands#local-command-boundaries | Covered | Source-checked single snapshot request, no client tools, usage caution and independent error/cancel result. |
| CLI-S24 | `/meta <REQUEST>`, `/configure` | C:33; `packages/agent-cli/src/Agent/CLI/MetaConsole.hs:77–95,419–451`; `packages/agent-cli/src/Agent/CLI/Runtime/Repl/MetaConsole.hs:93–120,330` | Terminal#configure-without-interrupting-the-topic | Covered | Typed mutation catalog, exact command allowlist, limits, alias, secret/account boundaries, preview/cancel and partial-failure recovery documented; source inspection only. |
| CLI-S25 | `/recap`, `/summarize` | C:34; `packages/agent-cli/src/Agent/CLI/Runtime/Repl/Commands.hs:855–869` | Commands#local-command-boundaries; Commands#command-aliases | Covered | Source-checked recap versus context compaction and alias. |
| CLI-S26 | `/retry` | C:35; `packages/agent-cli/src/Agent/CLI/Session/Lifecycle.hs:130–175` | Terminal#provider-waits; Commands#local-command-boundaries | Covered | Source-checked retained inputs/attachments, restored plan state, checkpoint continuation, no duplicate user prompt and external-effect non-idempotence. |
| CLI-S27 | `/session` | C:36 | Sessions#resume-previous-work | Covered | ID retrieval/resume workflow present. |
| CLI-S28 | `/session-info`, `/status`, `/info` | C:37; S:722–770 | Sessions#inspect-session-state; Commands#command-aliases | Covered | Source-checked output fields and three persistence states, connection identity interpretation and aliases. |
| CLI-S29 | `/desktop` | C:38; `packages/agent-cli/src/Agent/CLI/Desktop.hs:20–66`; `packages/agent-cli/src/Agent/CLI/Runtime/Repl/Commands.hs:672–697` | Terminal#open-desktop | Covered | Source-checked platform, persisted-session and private-app prerequisites, deep link and recovery; no shipped GUI promise. |
| CLI-S30 | `/voice` | C:39 | Voice | Covered | Dedicated setup and distinction from dictation. |
| CLI-S31 | `/afk [HOST:PATH]` | C:40; A:38–105; S:769 | Sessions#afk-handoff | Covered | Local/remote prerequisites, transfer boundaries, printed reconnect command and partial-failure recovery documented; source-only verification. |
| CLI-S32 | `/worktree` | C:41 | ParallelAgents#start-an-isolated-checkout | Covered | Fresh-session and remote-base behavior explained. |
| CLI-S33 | `/rename TITLE/--auto`, `/title` | C:42; P:377–385 | Sessions#give-a-session-a-useful-title | Covered | Alias and 100-character limit added to examples/automatic reset; source-only verification. |
| CLI-S34 | `/login`, `/accounts` | C:43 | Authentication; Commands#gateway-and-maintenance | Covered | Account workflow and standalone/fullscreen distinction described. |
| CLI-S35 | `/resume [ID]` | C:44; `packages/agent-cli/src/Agent/CLI/Session/Selection.hs:91–154`; `packages/agent-cli/src/Agent/CLI/TUI/App/Overlay.hs:190–309` | Sessions#resume-troubleshooting; Sessions#session-browser | Covered | Source-checked browser keys, failed-load/boundary preservation and missing checkout recovery. |
| CLI-S36 | `/home`, `/welcome` | C:45; S:143 | Sessions#session-browser | Covered | Source-checked same browser as resume, no implicit clear/new session, cancellation and draft caution. |
| CLI-S37 | `/search QUERY` | C:46; `packages/agent-cli/src/Agent/CLI/Session/Selection.hs:156–239` | Sessions#session-browser | Covered | Source-checked indexed-turn search, identity boundary, 100-result request, resume example and no-result outcome. |
| CLI-S38 | `/compact [FOCUS]` | C:47 | Sessions#manage-long-conversations | Covered | Focus example, context limit requirement, provider distinction and information-loss caveat supplied. |
| CLI-S39 | `/rewind`, `/undo` | C:48; S:265–336 | Sessions#rewind-and-delete | Covered | Conversation truncation, returned draft, confirmation and unchanged-files safety documented; source-only verification. |
| CLI-S40 | `/clear` | C:49; S:484–562 | Sessions#start-over-without-confusing-the-operations; Sessions#inspect-session-state | Covered | Source-checked reset marker, metadata/task-plan effects, not secure erasure and write-failure preservation. |
| CLI-S41 | `/new` | C:50; S:968 | Sessions#start-over-without-confusing-the-operations | Covered | New identity versus clear/delete clearly distinguished. |
| CLI-S42 | `/delete` | C:51; S:353–385 | Sessions#rewind-and-delete | Covered | Permanent transcript/artifact loss, export-first, cancel default, and filesystem distinction documented; source-only verification. |
| CLI-S43 | `/usage` | C:52 | Providers#usage-output | Covered | Reserve/pacing/reset examples, fresh snapshots versus lag, error/no windows and provider-specific unavailable paths supplied. |
| CLI-S44 | `/reload-auth` | C:53 | Authentication#reload-and-recovery | Covered | Credential-source-specific action, inherited environment limitation, manual versus automatic auth recovery and repair guidance supplied. |
| CLI-S45 | `/paste [--send] [TEXT]` | C:54; `packages/agent-cli/src/Agent/CLI/Runtime/Repl/Attachments.hs:164–223`; `packages/agent-cli/src/Agent/CLI/Clipboard.hs:121–161,195–209` | Terminal#attach-images | Covered | Source-checked explicit image precedence, text rejection, immediate caption/default, host clipboard prerequisites, staging/consumption and bounds. |
| CLI-S46 | `/attachments` | C:55 | Terminal#attach-images | Covered | Lists queued images in documented attach/clear workflow. |
| CLI-S47 | `/clear-attachments` | C:56 | Terminal#attach-images | Covered | Clears queued images without sending them. |
| CLI-S48 | `/copy [N] [PATH]`, `/copy-last` | C:57; P:587; `packages/agent-cli/src/Agent/CLI/Terminal.hs:375–381` | Terminal#copy-output; Terminal#clipboard-recovery | Covered | Source-checked indexes, alias, file destination and errors; OSC52-only behavior, emission versus clipboard receipt and file fallback documented. |
| CLI-S49 | `/copy-code [N]` | C:58; P:635; `packages/agent-cli/src/Agent/CLI/Runtime/Repl/Transcript.hs:313–325` | Terminal#copy-output | Covered | Source-checked default 1, latest-response provenance and missing/out-of-range errors. |
| CLI-S50 | `/copy-diff` | C:59; `packages/agent-cli/src/Agent/CLI/Runtime/Repl/Transcript.hs:327–336`; `packages/agent-cli/src/Agent/CLI/Terminal.hs:375–381` | Terminal#copy-output; Terminal#clipboard-recovery | Covered | Source-checked last-response diff provenance plus clipboard diagnostics and file-export fallback. |
| CLI-S51 | `/copy-path` | C:60 | ParallelAgents#start-an-isolated-checkout | Covered | Active checkout path use case clear. |
| CLI-S52 | `/copy-session` | C:61 | Sessions#resume-previous-work | Covered | Current identity copy use case clear. |
| CLI-S53 | `/terminal`, `/ghostty` | C:62; `packages/agent-cli/src/Agent/CLI/Terminal.hs:150–176` | Terminal#terminal-diagnostics | Covered | Source-checked fields, meaning/limits and multiplexer troubleshooting. |
| CLI-S54 | `/changelog` | C:63 | Commands#clipboard-and-navigation | Covered | Simple display operation sufficiently described. |
| CLI-S55 | `/agents [limit [N]]`, `/a` | C:64; `packages/agent-cli/src/Agent/CLI/Session/Selection.hs:311–336`; `packages/agent-cli/src/Agent/CLI/AgentViewport.hs:174–192,305–328`; `packages/agent-cli/src/Agent/CLI/TUI/App/Overlay.hs:479–529` | AgentLifecycle; Keybindings#agent-picker; ParallelAgents#delegate-independent-tasks | Covered | Source-verified fullscreen/minimal/non-TTY selector behavior, alias, cap and viewport versus lifecycle distinction. |
| CLI-S56 | `/mcp [prompt SERVER NAME key=value...]`, `/mcps` | C:65; P:397,1057–1060; `packages/agent-cli/src/Agent/CLI/Runtime/Repl/Commands.hs:649–670` | Mcp#manage-connections; Commands#mcp-server-prompts | Covered | Alias, server-specific example, string-token parsing and empty values, failure return and immediate expanded-turn submission documented; source inspection only. |
| CLI-S57 | `/loop [interval] prompt` | C:66; I:68 | Commands#recurring-work | Covered | Source-checked detached-fire example, units/minimum/no default, immediate fire, task ID, parent cancellation and seven-day expiry. |
| CLI-S58 | `/goal objective [--budget N]`, status/pause/resume/clear | C:67; P:457–513; `packages/agent-grok-build-dialect/src/Agent/GrokBuild/Dialect/Goal.hs:54–145` | Commands#goals-and-workflows; ScheduledWork#goals | Covered | Source-checked advisory token units, unset default, memory-only state, replacement, pause/resume preconditions and completion/clear boundaries. |
| CLI-S59 | `/workflow runs`, `/workflow name [input]` | C:68; P:516; I:105; `packages/agent-cli/src/Agent/CLI/Runtime/Repl/Workflow.hs:150–180` | Commands#goals-and-workflows; Commands#workflow-lifecycle; ScheduledWork#named-workflows | Covered | Source-checked supported name/input, validation, unsupported management, child interruption boundaries and in-memory run index; no live workflow execution claimed. |
| CLI-S60 | `/deep-research query` | C:69; I:124; `packages/agent-grok-build-dialect/src/Agent/GrokBuild/Dialect/Workflow.hs:264–410` | Commands#goals-and-workflows; Commands#workflow-lifecycle | Covered | Source-checked immediate launch, child result rather than fixed report path, status meanings, citation verification and restart/cancellation limits. |
| CLI-S61 | `/skills [reload]` | C:70 | Skills | Covered | Discovery, reload, installation and authored skill verification covered. |
| CLI-S62 | `/shell [ghci/bash/both/none]` | C:71; `packages/agent-cli/src/Agent/CLI/Session/Runner/Execution.hs:1281–1292` | Commands#local-command-boundaries | Covered | Source-checked tool availability, GHCi suspension, process-lifetime boundary and switching examples. |
| CLI-S63 | `/computer-use [on/off]` | C:72; `packages/agent-cli/src/Agent/CLI/Session/Runner/Execution.hs:1293–1310` | Commands#computer-control | Covered | Source-checked inspection/on/off sequence, capability rejection, fresh approval and no-undo boundary. |
| CLI-S64 | `/codemod`, `/code-mode` | C:73; `packages/agent-cli/src/Agent/CLI/Runtime/Repl/Commands.hs:1579–1599`; `packages/agent-cli/src/Agent/CLI/Runtime/Orchestration/Flow.hs:321–326` | Commands#local-command-boundaries | Covered | Source-checked persisted-session restart, enable-only behavior, launch override and provider limits. |
| CLI-S65 | `/always-approve`, `/yolo` | C:74 | Approvals; Commands#configuration-and-tools | Covered | Persistent project policy and separate computer-use approval addressed. |
| CLI-S66 | `/update-and-restart` | C:75; `packages/agent-cli/src/Agent/CLI/Runtime/Orchestration/Flow.hs:306–320,361–377` | Commands#initialize-and-update | Covered | Source-checked Nix profile remove/add/exec implementation, non-atomic failure repair, existing-runtime fallback and transient-flag loss. |
| CLI-S67 | `/quit`, `/exit` | C:76; `packages/agent-cli/src/Agent/CLI/Runtime/Orchestration/Session.hs:1452–1469,1500–1518`; `packages/agent-runtime/src/Agent/Runtime/Collaboration.hs:85–101` | Terminal#exit-aliases | Covered | Source-checked aliases, stderr resume hint, tool cleanup, child interrupt/snapshot/join and forced-kill limitation. |
| CLI-S68 | Bare `exit`/`quit`; `:q`, `:q!`, `:quit`, `:wq`, `:wq!`, `:reload`, `:yolo` | P:213–246 | Terminal#exit-aliases | Covered | Source-checked complete exit list and developer reload versus auth reload distinction. |
| CLI-S69 | Dynamically discovered `/SKILL [ARGS]` and `$SKILL` | P:437; Skills source module | Skills#discover-and-invoke-skills | Covered | Invocation and reload procedure documented; third-party names are intentionally not a static inventory. |

## Keyboard and input workflows

The table deliberately separates editing a prompt, navigating output, and controlling a running turn. These are materially different contexts; a generic “arrows and Enter” sentence does not cover all bindings.

| ID | Function/keys | Source | Documentation | Coverage | Missing detail or assessment |
| --- | --- | --- | --- | --- | --- |
| CLI-K01 | Enter submit/steer, Shift+Enter newline | K:506–512 | Keybindings#composer; Terminal#steer-or-queue-a-follow-up | Covered | Main idle/running distinction documented. |
| CLI-K02 | Ctrl+Enter / Ctrl+O interruptive send | B:152–163; K:448 | Keybindings#editing-and-interruption | Covered | Source-checked interrupt/steer distinction and empty-draft queue promotion. |
| CLI-K03 | Ctrl+Q exit; Ctrl+D empty exit/nonempty forward-delete | K:450–459 | Keybindings#editing-and-interruption | Covered | Source-checked empty/nonempty distinction. |
| CLI-K04 | Ctrl+C and Escape in composer | K:464–477; `packages/agent-cli/src/Agent/CLI/Interrupt.hs:52–85` | Keybindings#cancel-or-exit | Covered | Source-checked active cancellation/escalation, idle two-second exit confirmation and Escape menu/draft behavior. |
| CLI-K05 | Shift+Tab cycle ask/plan/always-approve while idle | K:478–480; O:874 | Keybindings#editing-and-interruption; Approvals | Covered | Idle-only shortcut documented; source-only verification. |
| CLI-K06 | Up/Down multiline cursor, history fallback, slash-menu selection | K:481–496 | Keybindings#editing-and-interruption | Covered | Source-checked draft/history/menu distinctions. |
| CLI-K07 | Tab completion versus scrollback | K:497–505 | Keybindings#editing-and-interruption | Covered | Source-checked menu precedence. |
| CLI-K08 | Backspace/Delete, Ctrl+D forward delete | K:453–459; K:513; K:574 | Keybindings#editing-and-interruption | Covered | Source-checked editing map. |
| CLI-K09 | Ctrl+W or modified Backspace kill previous word | K:515–520 | Keybindings#editing-and-interruption | Covered | Source-checked modified keys and consecutive kill buffer behavior. |
| CLI-K10 | Alt/Meta+D kill next word | K:460–463 | Keybindings#editing-and-interruption | Covered | Source-checked key/action. |
| CLI-K11 | Ctrl+U / Ctrl+K kill to line start/end | K:521–526 | Keybindings#editing-and-interruption | Covered | Source-checked composer context. |
| CLI-K12 | Ctrl+Y yank killed text | K:527–529 | Keybindings#editing-and-interruption | Covered | Source-checked composer insertion versus scrollback copy. |
| CLI-K13 | Ctrl+L redraw | K:530–532 | Keybindings#editing-and-interruption | Covered | Source-checked cache invalidation/redraw. |
| CLI-K14 | Ctrl+R dictate | K:533–537 | Keybindings#composer; Voice | Covered | Dictation functionality and configuration distinguished from voice call. |
| CLI-K15 | Ctrl+A/Home and Ctrl+E/End line endpoints | K:538–543; K:588–591 | Keybindings#editing-and-interruption | Covered | Source-checked current-line endpoints. |
| CLI-K16 | Ctrl+B/F and Left/Right character movement | K:544–546; K:551–553; K:584–587 | Keybindings#editing-and-interruption | Covered | Source-checked character movement. |
| CLI-K17 | Alt/Meta+B/F or Alt/Meta+Left/Right word movement | K:547–557; K:576–583 | Keybindings#editing-and-interruption | Covered | Source-checked word movement variants. |
| CLI-K18 | Ctrl+_ prompt undo | K:558–562 | Keybindings#editing-and-interruption | Covered | Source-checked draft undo distinguished from conversation/file undo. |
| CLI-K19 | Ctrl/Cmd+V and bracketed paste | K:389–417,563–573,598,605; `packages/agent-cli/src/Agent/CLI/Clipboard.hs:81–119,168–183` | Terminal#attach-images | Covered | Source-checked explicit text-first versus bracketed image classification, running-turn draft insertion and multiline example. |
| CLI-K20 | Page Up/Down history scroll | K:592–595 | Keybindings#composer | Covered | Key/function documented. |
| CLI-K21 | Scrollback block/line/page movement and copy | `packages/agent-cli/src/Agent/CLI/TUI/App/Navigation.hs:380–420` | Keybindings#scrollback | Covered | Main operations and return-to-prompt tutorial present. |
| CLI-K22 | Cmd/Alt+K Meta Console | `packages/agent-cli/src/Agent/CLI/TUI/App/Overlay.hs:1050` | Keybindings#composer; Terminal#configure-without-interrupting-the-topic | Covered | Compatibility fallback provided. |
| CLI-K23 | Inline/minimal editor mapping, including Ctrl+N/P | D:117–143 | Keybindings#inline-editor | Covered | Separate source-checked inline mapping including Ctrl+N/P and terminal reporting boundary. |
| CLI-K24 | Picker-specific account, session, MCP, agent actions | `packages/agent-cli/src/Agent/CLI/Login/Types.hs:99–130`; `packages/agent-cli/src/Agent/CLI/Login/Internal/Manager.hs:69–201`; C:43–65 | Keybindings#dialogs; Keybindings#account-picker; Keybindings#agent-picker; Sessions#session-browser; Mcp#manage-connections | Covered | Source-checked standalone letters versus fullscreen account menus, cancel/focus boundaries, session actions, agent view selection and MCP actions; no live account mutation claimed. |
| CLI-K25 | Ctrl+U/D scrollback half-pages | `packages/agent-cli/src/Agent/CLI/TUI/App/Navigation.hs:404–407` | Keybindings#scrollback | Covered | Source-checked context-specific half-pages. |
| CLI-K26 | Home/End scrollback start/follow latest | `packages/agent-cli/src/Agent/CLI/TUI/App/Navigation.hs:408–413` | Keybindings#scrollback | Covered | Source-checked oldest/latest navigation. |
| CLI-K27 | Left/Right/Enter expand or collapse selected output block | `packages/agent-cli/src/Agent/CLI/TUI/App/Navigation.hs:414–416` | Keybindings#scrollback | Covered | Each key toggles, rather than directional expand/collapse; source-checked. |

## Cross-command use cases and failure paths

| ID | Use case | Source | Documentation | Coverage | Missing detail or assessment |
| --- | --- | --- | --- | --- | --- |
| CLI-W01 | Recover conversation without rolling back files | S:265–336 | Sessions#rewind-and-delete | Covered | Source-checked workflow narrows a returned draft after inspecting unchanged project files; confirmation defaults to cancel. |
| CLI-W02 | Delete a session and artifacts safely | S:353–385; S:563 | Sessions#rewind-and-delete | Covered | Source-checked export-first and permanent-loss warning with confirm/cancel. |
| CLI-W03 | Move work to local or remote tmux | A:38–105; A:112–169 | Sessions#afk-handoff | Covered | Source-checked prerequisites, transfer scope, reconnect and non-atomic failure recovery. |
| CLI-W04 | Start fresh without losing earlier session | S:968 | Sessions#start-over-without-confusing-the-operations | Covered | New versus clear/delete distinction supports ordinary use. |
| CLI-W05 | Branch peer conversation with/without worktree | S:215–244; S:623 | ParallelAgents#branch-the-conversation | Covered | Source-checked dialog/default and stepwise original-ID return, deliberate isolated integration and shared-checkout caution. |
| CLI-W06 | Handle invalid command/argument without submitting to model | P:314–435; O:778–797; `packages/agent-cli/src/Agent/CLI/Runtime/Repl/Commands.hs:503–505` | Commands#local-command-boundaries | Covered | Source-checked error/retry-help path and no implicit model turn for command errors. |
| CLI-W07 | Operate bounded headless review automation | O:284–312; `packages/agent-cli/src/Agent/CLI/Session/Lifecycle.hs:95–102,205–232`; `packages/agent-cli/src/Agent/CLI/Render.hs:528–556` | Commands#one-shot-automation | Covered | Source-checked stream versus administrative JSON contract, unsuccessful failed turn, normal-quit cancellation caveat, external deadline and non-idempotent retry verification. |
| CLI-W08 | Inspect full transcript through pagination | O:358–384; O:762–775; `packages/agent-cli/src/Agent/CLI/SessionAdmin.hs:229–240` | Commands#session-json | Covered | Source-checked exclusive smallest-index cursor, hasOlder stop condition and indexed/whole-output distinction. |
| CLI-W09 | Recover a collected checkout | O:423; S resume handlers | ParallelAgents#restore-a-checkout; Sessions#resume-troubleshooting | Covered | Source-checked restore invocation, missing originals, detached HEAD and existing-path/interrupted-restore boundary. No live restore test claimed. |
| CLI-W10 | Interrupt and redirect active work while preserving queue | K:418–429; B:152–163 | Terminal#steer-or-queue-a-follow-up; Keybindings#editing-and-interruption | Covered | Source-checked interruptive send, queue inspection and empty-draft promotion. |
| CLI-W11 | Select a worktree base and recover from a changed remote default branch | `packages/agent-cli/src/Agent/CLI/Worktree.hs:182–188`; `packages/agent-cli/src/Agent/CLI/Worktree.hs:820–858` | ParallelAgents#repair-the-worktree-base | Covered | README:294–329/source detail migrated: cached symbolic ref, retained-old-default case, explicit fetch then set-head repair. Source-only verification. |
| CLI-W12 | Interpret retention/adoption proofs and restore failure safely | `packages/agent-cli/src/Agent/CLI/Worktree/Incorporation.hs:71`; README:351–433 | ParallelAgents#collection-safety; ParallelAgents#restore-a-checkout | Covered | Source/README-checked inactivity/adoption, merge proof, no-fetch GC, ignored-data exceptions, estimates, clock, detached restore and lease races. |
| CLI-W13 | Distinguish short provider cooldown, usage reset wait, fallback and cancellation | `packages/agent-cli/src/Agent/CLI/ProviderFallback.hs:64–118`; `packages/agent-cli/src/Agent/CLI/Session/Retry.hs:142–190`; `packages/agent-cli/src/Agent/CLI/Session/Lifecycle.hs:255–278` | Terminal#provider-waits | Covered | Source-checked 120-second cooldown/retry limit, reset distinction, cancellation draft/exit behavior and billing restriction. |

### Engineering documentation versus website coverage

The initial audit found `README.md:294–329` and `README.md:351–433` materially richer than the website on worktree base discovery and recovery. Their operational qualifications are now reflected in `ParallelAgents#repair-the-worktree-base`, `ParallelAgents#collection-safety`, and `ParallelAgents#restore-a-checkout`; the README remains corroborating engineering material. No live deletion/restoration was run to validate these procedures.

Grouped run-option rows explicitly include both positive and negative names. Administrative option names are attached to their individual leaf rows: `--url`, `--json`, `--before`, `--limit`, import `--cwd`, `-t`/`--transport`, repeated `--scope`, `--dry-run`, and `--inactivity-days`. No statically declared command option is intentionally omitted from this CLI inventory. The complete slash catalog is listed, including dialect/tool-gated entries; dynamic skills and terminal protocol encodings remain the declared limits.

## Follow-up implementation and remaining priorities

The MCP owner's supplementary reference covers CLI-A09 at `Mcp#catalog-json`, CLI-A13 and CLI-A14 at `Mcp#cli-oauth-identity`, and CLI-S56 at `Mcp#prompts-and-resources`. These expand the same source-backed catalog JSON, OAuth scope/credential identity/local logout, and expanded-prompt contracts recorded in the command-reference rows.

Cross-owner reference additions supplement the row-level sections: CLI-O09 is also covered by `Models#automatic-compaction`; CLI-O10, CLI-O11 and CLI-S14 by `Models#reasoning-controls`; CLI-S43 by `Providers#usage-output`; CLI-S44 by `Authentication#reload-and-recovery`; and CLI-A02, CLI-A03 and CLI-A04 by `Providers#gateway-command-lifecycle`. These document provider-specific limits and defaults, usage freshness, replay-safe recovery boundaries, and gateway authorization/status/disconnection contracts. They are source-backed prose, not live account verification.

The follow-up updated Commands, Keybindings, Sessions, Terminal, and ParallelAgents. The subsequent passes add command recovery contracts, session and agent picker actions, context/status interpretation, clipboard classification and attachment limits, themes and editor behavior, update/init boundaries, desktop launch, computer approval reset, goal lifecycle, session-transfer schema, managed integration payloads, headless output/exit boundaries, worktree enrollment and provider-specific defaults. The final prose pass adds MCP scripting/authentication/prompt contracts, storage diagnostic and migration recovery boundaries, the Meta Console mutation catalog, workflow lifecycle and specialized account controls. The matrix records 160 Covered entries, with no Partial, Missing or Conflict entries. Compilation evidence concerns documentation modules, not product behavior.

All originally enumerated prose acceptance criteria have source-backed coverage. All five owned HSX modules loaded successfully with GHCi through `nix develop .#docs` after the final edits; the matrix checker confirmed 160 unique IDs and every referenced page section exists, and the scoped `git diff --check` passed. This is not a claim of exhaustive runtime verification, every terminal protocol encoding, dynamically installed command behavior or future implementation changes. Missing runtime execution alone is not used to label complete prose Partial.

## Open questions and verification

- The static CLI and slash registries are fully enumerated; terminal byte encodings, every widget binding and dynamically installed skills are not claimed exhaustive.
- `packages/agent-cli/test/Agent/CLI/OptionsSpec.hs`, `CommandSpec.hs`, `TUIComposerSpec.hs`, `TUIComposerUndoSpec.hs`, `TUIBridgeSpec.hs`, `SessionStateSpec.hs`, and `SessionHistorySpec.hs` are relevant executable specifications. They were not executed in this audit.
- A runtime follow-up should use isolated storage, a real tmux TTY, and deliberate confirmation before any destructive session/worktree operation.
- Coverage conclusions concern the self-hosted documentation pages, not comments/help text embedded in the product; in-product help is evidence, not a substitute for the requested website.
