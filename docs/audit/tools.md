# Tools, permissions, skills, and delegated work coverage

Audit date: 2026-09-22. Source baseline: `3100a05e0` plus the current uncommitted documentation. This is a **source audit**, not a runtime certification.

## Implementation follow-up

Original counts were **30 Covered, 50 Partial, 40 Missing**.
Current row-by-row reconciliation counts are **120 Covered, 0 Partial, 0 Missing**.
Covered means source-reviewed usable documentation, not successful live integration testing.
All inventoried rows now have source-reviewed usable documentation.
The following source-backed documentation was added after the original snapshot.

- `Pages/ToolExecution.hs`: availability by host/dialect; GHCi inputs, helpers and state;
  code-mode cells and cancellation; artifact schemas and continuations; stdin payloads;
  image presentation, chart schema/limits/example and conditional image generation.
  Addresses TOOL-006–007, 009–013, 015, 017–018. Remaining depth: actual provider/host
  execution examples and exhaustive evaluator recovery behavior are not runtime verified.
- `Pages/StructuredMemory.hs`: all four structured-memory operations and four persisted
  session operations, scope/ownership, query/mutation boundaries, privacy, payload examples,
  bounded waiting and verification. Addresses TOOL-032–035 and 053–056. This is not an
  external database connection guide or a transcript access authorization grant.
- `Pages/ScheduledWork.hs`: Grok terminal/task control, typed tasks, monitor, goals,
  recurring schedules and research workflow. Addresses TOOL-038, 040–049. Unsupported
  durable/foreground/one-shot scheduler and custom workflow modes are explicitly identified.
  Runtime budget accounting and host integration remain source-backed, not exercised.
- `Pages/BrowserControl.hs`: all thirteen native browser operations and nine desktop action
  variants, parameters, observe/act/verify flow, consent and recovery. Addresses TOOL-083–101.
  Explicitly conditional on native host support; no claim these tools exist in every CLI session.
- `Pages/Tools.hs#planning-and-delegation` and `#grok-replacement`: checklist states and
  payload, structured question inputs, Grok todo semantics and exact-string editing.
  Addresses TOOL-019, 023, 037, 039. Additional patch add/move/delete examples and
  explicit exit-plan outcomes now address TOOL-004 and TOOL-022.
- `Pages/AgentLifecycle.hs#monitor-and-limit-concurrency`: wait timeout, prefix filtering,
  canonical task references and links distinguishing persisted sessions. Addresses TOOL-026, 029.
- `Pages/Skills.hs#bundled-workflows` and `#remote-skills`: all ten packaged names,
  consent and provenance boundaries, CI waiting, historical imports, remote loading/integrity
  failures. Addresses TOOL-064, 068–074. Exact external-reader fields, all four root overrides,
  ambiguity and stale-history verification now appear in `#resume-external-history`.
  `#inspect-a-skill` covers unified catalog lookup and `#profile-and-ci-workflows` covers
  profile consent and exact-revision CI completion. `Pages/LearnedSkills.hs#post-task-review` explains
  existing-skill search, the two-mutation cap, evidence threshold and silent no-op outcome.
- `Pages/LanguageServers.hs#operation-payloads`: all six operations, position example and
  result interpretation. Addresses TOOL-050.
- `Pages/Mcp.hs#tool-payloads`: three discovery dialects, two dispatch schemas, direct
  discovery, resource list/read payloads, stale catalog and uncertain mutation recovery.
  Addresses TOOL-075–079, 081–082.
- `Pages/WebAccess.hs#hosted-search`: provider-hosted web/X search versus local fetch,
  source verification and unsupported-provider recovery. Addresses TOOL-052, 120 without
  inventing a universal provider-owned parameter schema.

Validation performed during this follow-up: focused `nix develop .#docs -c ghci -idocs/src`
loaded all eleven changed page modules successfully, and `git diff --check`
passed. Website registration/link checks remain the integrator's responsibility.
No live database, schedule, image generation, browser,
desktop, MCP mutation or external API execution was performed.

Telegram revocation versus already-running work (TOOL-118) is now documented in
`Pages/Telegram.hs#group-access-example`. Other Telegram operation payloads and delivery
boundaries have been reconciled against `Pages/Telegram.hs#gateway-tools`. Email library/integration
availability, local transport fields/limits and send safety are now documented in
`Pages/ToolExecution.hs#connected-email` and `#email-mutations`, without claiming universal
CLI mail tool registration. Permission/root/classification
details are now in `Pages/ToolExecution.hs#filesystem-and-classification`, linked from
`Pages/Approvals.hs#execution-boundaries`; the latter module also loaded successfully
in a focused GHCi check.

## Method and boundaries

Started at the actual assembly in `packages/agent-cli/src/Agent/CLI/Runtime/Orchestration/Tools.hs`, followed the Codex and Grok tool groups, host extensions, MCP fleet, native browser bridge, database and learned-skill registries, and compared every named operation below with the rendered-page sources. Constructors, schemas, descriptions, and handlers were inspected; no model, external-service, desktop, or mutation test was executed during this audit.

Registry cross-checks:

- Codex base: 12 tools before optional collaboration (`packages/agent-codex-dialect/src/Agent/Codex/Dialect/Tools.hs:136`).
- Shared collaboration: 7 tools (`packages/agent-tools/src/Agent/Tools/MultiAgents.hs:182` onwards).
- Grok base: 12 execution tools plus 3 plan/question host tools; root-only goal, 3 scheduler tools, workflow, and optional task tool are conditional (`packages/agent-grok-build-dialect/src/Agent/GrokBuild/Dialect/Tools.hs:79`).
- Retained output: 3 tools plus optional analysis delegation (`packages/agent-tools/src/Agent/Tools/OutputArtifact.hs:118`).
- Structured memory: 4 tools; learned skills: 6; persisted agent sessions: 4.
- Native browser bridge: 13 tools; computer control: 9 action variants under one tool.
- Packaged filesystem skills: 10 `SKILL.md` files under `packages/agent-cli/skills`.

These counts overlap across dialects and are **not a sum of tools available to one model**. Native hosts can replace execution groups; policy, provider, credentials, roots, feature switches, MCP availability, and embedding capabilities further restrict availability. Dynamic third-party MCP tools and native host-injected tools cannot be exhaustively enumerated from this checkout.

Each row is a behavior coverage unit, not necessarily a unique tool. **Covered** means a usable account of that stated operation exists; **Partial** means important operational details are absent; **Missing** means no usable guide was found; **Conflict** requires contradictory claims. A covered row does not imply all schema limits are documented.

Documentation references below use `Pages/Filename.hs#section`, meaning `docs/src/Documentation/Pages/Filename.hs` and its HTML heading identifier. Source paths are repository-relative. Validation for **every row is source-only**; test modules listed at the end are corroborating fixtures, not tests run for this audit.

Statuses measure the **HSX documentation website**, not the absence of prose anywhere in the repository. Existing engineering-only material supplies additional coverage outside site navigation/search:

- Computer control: `docs/computer-use.md` (TOOL-096–101, TOOL-110).
- Email accounts and operations: `docs/email-accounts.md` (TOOL-103–104).
- Chart presentation: `docs/native-charts.md` (TOOL-010).
- GHCi versus shell operation: `docs/evals/ghci-vs-bash.md` (TOOL-007, TOOL-108).
- MCP contracts/authentication: `docs/mcp.md`, `docs/mcp-oauth.md` (TOOL-064, TOOL-075–082).
- Managed/local Telegram: `docs/telegram.md` (TOOL-067, TOOL-102, TOOL-113–119).
- Session lifecycle/ownership: `docs/session-engine.md`, `docs/session-worker-ownership.md` (TOOL-024–036).
- Isolation and tool scheduling: `docs/shell-sandbox-escalation.md`, `docs/tool-resource-arbitration.md`, `docs/sandbox-pdf-inspector.md` (TOOL-105–112).

These documents are migration/reference inputs, not evidence that a website reader can find an operational guide. Their presence does not change the website coverage status.

## Core inspection, editing, execution, and output

| ID | Implemented function/use case | Source evidence | Existing documentation | Coverage | Missing details or acceptance boundary |
|---|---|---|---|---|---|
| TOOL-001 | Read a bounded file range with `read_file` | `packages/agent-tools/src/Agent/Tools/FileSystem/ReadFile.hs:63` | Pages/Tools.hs#read-file | Covered | Usable offset, limit, roots, example and reread guidance; edge-case error examples still useful. |
| TOOL-002 | Inspect directories with `list_dir` | `packages/agent-tools/src/Agent/Tools/FileSystem/ListDir.hs:54` | Pages/Tools.hs#directory-search | Covered | Ignore and hidden-file limitations are explained. |
| TOOL-003 | Search files with `grep` | `packages/agent-tools/src/Agent/Tools/FileSystem/Grep.hs:107` | Pages/Tools.hs#directory-search | Covered | Arguments, regex escaping, context and truncation are described. |
| TOOL-004 | Add/update/delete/move files through `apply_patch` | `packages/agent-codex-dialect/src/Agent/Codex/Dialect/Tools.hs:437` | Pages/Tools.hs#patch-files | Covered | Add/update/delete/move examples, binary restrictions and partial-failure inspection documented. |
| TOOL-005 | Start a process with `shell_command` | `packages/agent-codex-dialect/src/Agent/Codex/Dialect/Tools.hs:176` | Pages/Tools.hs#shell-processes | Covered | Working directory, timing distinction, escalation and final-status interpretation are usable. |
| TOOL-006 | Send input/snapshot/interrupt with `write_stdin` | `packages/agent-codex-dialect/src/Agent/Codex/Dialect/Tools.hs:311` | Pages/ToolExecution.hs#stdin-and-identifiers | Covered | Payload, control character, time limits and stale-ID recovery documented. |
| TOOL-007 | Persistent Haskell evaluator `run_ghci` | `packages/agent-tools/src/Agent/Tools/Ghci/Tool.hs:59` | Pages/ToolExecution.hs | Covered | Persistent bindings, import/helper examples, time limits, approval and uncertain-state recovery documented. |
| TOOL-008 | Inspect local image with `view_image` | `packages/agent-tools/src/Agent/Tools/ViewImage.hs:69` | Pages/Tools.hs#images-and-secrets | Covered | Existing-file and disclosure boundary explained; supported image/size details remain reference expansion. |
| TOOL-009 | Show an image to the user with `show_image` | `packages/agent-tools/src/Agent/Tools/ShowImage.hs:88` | Pages/ToolExecution.hs#images-and-charts | Covered | Presentation versus inspection, path/caption, supported formats and allowed roots now documented; source-only validation. |
| TOOL-010 | Render terminal chart with `render_chart` | `packages/agent-tools/src/Agent/Tools/RenderChart.hs:34` | Pages/ToolExecution.hs#images-and-charts | Covered | Schema, supported kinds, valid JSON example, axis ordering, data limits and host rendering boundary documented; no live renderer test. |
| TOOL-011 | Generate/edit images with `imagegen` | `packages/agent-openai/src/Agent/OpenAI/ImageGeneration.hs:198` | Pages/ToolExecution.hs#image-generation; Pages/ToolExecution.hs#availability | Covered | Exact accepted inputs, conditional registration, disclosure/account usage, artifact inspection and uncertain-request recovery documented; not live provider tested. |
| TOOL-012 | Read retained output with `read_tool_output` | `packages/agent-tools/src/Agent/Tools/OutputArtifact.hs:125` | Pages/ToolExecution.hs#retained-output-parameters | Covered | Cursor/line modes, limits, continuation example and invalid-handle recovery documented. |
| TOOL-013 | Search retained output with `search_tool_output` | `packages/agent-tools/src/Agent/Tools/OutputArtifact.hs:139` | Pages/ToolExecution.hs#retained-output-parameters | Covered | Literal search, context/case controls and continuation contract documented. |
| TOOL-014 | Export retained output with `export_tool_output` | `packages/agent-tools/src/Agent/Tools/OutputArtifact.hs:153` | Pages/Tools.hs#retained-output | Covered | Purpose, private scratch output, data-not-code and API pagination distinction usable. |
| TOOL-015 | Delegate retained-output analysis with `analyze_tool_output` | `packages/agent-tools/src/Agent/Tools/OutputArtifact.hs:161` | Pages/ToolExecution.hs#retained-output-parameters | Covered | Optional availability, handle/instruction, tracked child and wait contract documented. |
| TOOL-016 | Masked input through `ask_secret` | `packages/agent-tools/src/Agent/Tools/Secret.hs:125` | Pages/Tools.hs#images-and-secrets; Pages/Approvals.hs#secrets-and-stored-data | Covered | File-path handoff, cleanup and disclosure caveat explained. |
| TOOL-017 | JavaScript orchestration `exec` | `packages/agent-tools/src/Agent/Tools/CodeMode/Tool.hs:400` | Pages/ToolExecution.hs#code-mode | Covered | Fresh isolate versus session store/load, nested approvals, content helpers, retained cells and cancellation documented. |
| TOOL-018 | Resume asynchronous code-mode execution with `wait` | `packages/agent-tools/src/Agent/Tools/CodeMode/Tool.hs:514` | Pages/ToolExecution.hs#code-mode | Covered | Cell identifier, parameter defaults, JSON example, terminate behavior and no rollback now documented. |

## Planning and concurrent agents

| ID | Implemented function/use case | Source evidence | Existing documentation | Coverage | Missing details or acceptance boundary |
|---|---|---|---|---|---|
| TOOL-019 | Checklist `update_plan` | `packages/agent-codex-dialect/src/Agent/Codex/Dialect/Tools.hs:504` | Pages/Tools.hs#planning-and-delegation | Covered | Payload, three statuses, one-active invariant and plan-mode incompatibility now documented. |
| TOOL-020 | Enter restricted planning with `enter_plan_mode` | `packages/agent-tools/src/Agent/Tools/PlanMode.hs:236` | Pages/Projects.hs; Pages/Approvals.hs#planning-is-a-separate-restriction | Covered | User approval and implementation restriction are explained. |
| TOOL-021 | Save `plan.md` with `write_plan` | `packages/agent-tools/src/Agent/Tools/PlanMode.hs:289` | Pages/Tools.hs#planning-and-delegation | Covered | Plan-only editing boundary and active-mode requirement explained. |
| TOOL-022 | Explicit Grok `exit_plan_mode` | `packages/agent-tools/src/Agent/Tools/PlanMode.hs:324` | Pages/Tools.hs#planning-and-delegation | Covered | Optional summary, on-disk plan, approve/revise/cancel distinctions and inactive-mode error documented. |
| TOOL-023 | Structured choices through `ask_user_question` | `packages/agent-tools/src/Agent/Tools/PlanMode.hs:438` | Pages/Tools.hs#planning-and-delegation | Covered | Question/options, multi-select, preview, free text and cancellation/non-plan usage documented. |
| TOOL-024 | Shared-checkout `spawn_agent` | `packages/agent-tools/src/Agent/Tools/MultiAgents.hs:182` | Pages/AgentLifecycle.hs | Covered | Context/fork restrictions, ownership, naming, no automatic integration explained. |
| TOOL-025 | Isolated-worktree `spawn_agent_in_worktree` | `packages/agent-tools/src/Agent/Tools/MultiAgents.hs:190` | Pages/AgentLifecycle.hs; Pages/ParallelAgents.hs | Covered | Worktree versus external-resource isolation and integration/retention guidance usable. |
| TOOL-026 | Await child updates with `wait_agent` | `packages/agent-tools/src/Agent/Tools/MultiAgents.hs:501` | Pages/AgentLifecycle.hs#monitor-and-limit-concurrency | Covered | timeout_ms default, notification versus failure and no fresh child turn documented. |
| TOOL-027 | Queue information with `send_message` | `packages/agent-tools/src/Agent/Tools/MultiAgents.hs:587` | Pages/AgentLifecycle.hs | Covered | Explicit distinction from starting a turn documented. |
| TOOL-028 | Start another child turn with `followup_task` | `packages/agent-tools/src/Agent/Tools/MultiAgents.hs:618` | Pages/AgentLifecycle.hs | Covered | Idle/running distinction documented. |
| TOOL-029 | List child state with `list_agents` | `packages/agent-tools/src/Agent/Tools/MultiAgents.hs:728` | Pages/AgentLifecycle.hs#monitor-and-limit-concurrency | Covered | States, path_prefix JSON example, no trailing slash and canonical task names documented. |
| TOOL-030 | Interrupt child with `interrupt_agent` | `packages/agent-tools/src/Agent/Tools/MultiAgents.hs:794` | Pages/AgentLifecycle.hs | Covered | Previous-status result, cancellation not rollback and subsequent followup explained. |
| TOOL-031 | Close ownership tree; root abort and resource cleanup | `packages/agent-core/src/Agent/Subagents/Registry/Internal/Operations.hs:182` | Pages/AgentLifecycle.hs | Covered | Closure versus interruption, descendant lifetime and integration boundary explained. |
| TOOL-032 | Create durable independent session `create_agent_session` | `packages/agent-cli/src/Agent/CLI/AgentSessions.hs:212` | Pages/StructuredMemory.hs#independent-sessions | Covered | Parameters, durable versus ephemeral lifecycle, workspace and ownership boundaries documented. |
| TOOL-033 | Read durable session `read_agent_session` | `packages/agent-cli/src/Agent/CLI/AgentSessions.hs:362` | Pages/StructuredMemory.hs#independent-sessions | Covered | ID, limit, status and transcript privacy documented. |
| TOOL-034 | Message durable session `send_agent_session_message` | `packages/agent-cli/src/Agent/CLI/AgentSessions.hs:417` | Pages/StructuredMemory.hs#independent-sessions | Covered | Running-owner delivery versus turn launch and conflicts documented. |
| TOOL-035 | Wait for durable session `wait_agent_session` | `packages/agent-cli/src/Agent/CLI/AgentSessions.hs:142` | Pages/StructuredMemory.hs#independent-sessions | Covered | Ownership, timeout, completion and non-cancellation documented. |
| TOOL-036 | Read external harness history `read_external_session` | `packages/agent-external-session/src/Agent/CLI/ExternalSession.hs:145` | Pages/Skills.hs#resume-external-history | Covered | Provider roots, filters, parsing limits, ambiguity and inert-history safety documented. |

## Grok-specific work execution

These are implemented dialect operations, not promises of availability in every provider session.

| ID | Implemented function/use case | Source evidence | Existing documentation | Coverage | Missing details or acceptance boundary |
|---|---|---|---|---|---|
| TOOL-037 | Replace matched text with `search_replace` | `packages/agent-grok-build-dialect/src/Agent/GrokBuild/Dialect/SearchReplace.hs:76` | Pages/Tools.hs#grok-replacement | Covered | Exact arguments/example, unique-match rule, replace_all, empty-string creation, roots and reread/plan boundaries documented. |
| TOOL-038 | Run foreground/background command `run_terminal_cmd` | `packages/agent-grok-build-dialect/src/Agent/GrokBuild/Dialect/Terminal.hs:65` | Pages/ScheduledWork.hs | Covered | Grok-specific command/description/background/timeout and returned task IDs documented. |
| TOOL-039 | Maintain Grok checklist `todo_write` | `packages/agent-grok-build-dialect/src/Agent/GrokBuild/Dialect/Todo.hs:76` | Pages/Tools.hs#planning-and-delegation | Covered | IDs, optional content/status, four states and merge versus replacement documented. |
| TOOL-040 | Read task output `get_task_output` | `packages/agent-grok-build-dialect/src/Agent/GrokBuild/Dialect/TaskControl.hs:67` | Pages/ScheduledWork.hs | Covered | Task IDs, snapshots, timeout and output inspection documented. |
| TOOL-041 | Await tasks `wait_tasks` | `packages/agent-grok-build-dialect/src/Agent/GrokBuild/Dialect/TaskControl.hs:220` | Pages/ScheduledWork.hs | Covered | Task identifiers, wait_any/wait_all and time bounds documented. |
| TOOL-042 | Stop task `kill_task` | `packages/agent-grok-build-dialect/src/Agent/GrokBuild/Dialect/TaskControl.hs:314` | Pages/ScheduledWork.hs | Covered | Cancellation and no-rollback boundary documented. |
| TOOL-043 | Monitor command with `monitor` | `packages/agent-grok-build-dialect/src/Agent/GrokBuild/Dialect/Monitor.hs:36` | Pages/ScheduledWork.hs | Covered | Command, timeout, retained output and stopping documented. |
| TOOL-044 | Typed delegation with `task` | `packages/agent-grok-build-dialect/src/Agent/GrokBuild/Dialect/Task.hs:230` | Pages/ScheduledWork.hs | Covered | Agent types, background/resume/cwd/model/isolation and availability documented. |
| TOOL-045 | Autonomous objective `update_goal` | `packages/agent-grok-build-dialect/src/Agent/GrokBuild/Dialect/Goal.hs:165` | Pages/ScheduledWork.hs#goals | Covered | Advisory token budget, in-memory state, completion/blocking and pause/resume/clear documented. |
| TOOL-046 | Create/update session-only recurring work `scheduler_create` | `packages/agent-grok-build-dialect/src/Agent/GrokBuild/Dialect/Scheduler.hs:251` | Pages/ScheduledWork.hs | Covered | Interval/task/expiry bounds, immediate fire, update and unsupported modes documented. |
| TOOL-047 | Delete schedule `scheduler_delete` | `packages/agent-grok-build-dialect/src/Agent/GrokBuild/Dialect/Scheduler.hs:462` | Pages/ScheduledWork.hs | Covered | ID, missing-ID recovery and already-started effects documented. |
| TOOL-048 | Inspect schedules `scheduler_list` | `packages/agent-grok-build-dialect/src/Agent/GrokBuild/Dialect/Scheduler.hs:503` | Pages/ScheduledWork.hs | Covered | No-argument inspection and returned schedule timing documented. |
| TOOL-049 | Validate/run named `workflow` (`deep-research`) | `packages/agent-grok-build-dialect/src/Agent/GrokBuild/Dialect/Workflow.hs:140` | Pages/ScheduledWork.hs | Covered | Input, validation example and unsupported script/budget/resume fields documented. |
| TOOL-050 | LSP: definition, references, hover, implementation, document symbols, workspace symbols | `packages/agent-grok-build-dialect/src/Agent/GrokBuild/Dialect/Lsp.hs:134` | Pages/LanguageServers.hs#operation-payloads | Covered | Six operation input/result contracts and zero-based position example documented. |
| TOOL-051 | Retrieve a public URL with `web_fetch` | `packages/agent-grok-build-dialect/src/Agent/GrokBuild/Dialect/WebFetch.hs:27` | Pages/WebAccess.hs | Covered | Public URL versus authenticated integration boundary, HTTPS and retained content explained. |
| TOOL-052 | Provider-native `web_search` | `packages/agent-cli/src/Agent/CLI/Tools.hs:94` | Pages/WebAccess.hs#hosted-search | Covered | Hosted availability, provider-owned controls, citation checking and missing-support recovery documented. |

## Durable data and reusable instructions

| ID | Implemented function/use case | Source evidence | Existing documentation | Coverage | Missing details or acceptance boundary |
|---|---|---|---|---|---|
| TOOL-053 | Inspect structured memory with `database_schema` | `packages/agent-runtime/src/Agent/Runtime/Database.hs:138` | Pages/StructuredMemory.hs#scopes; Pages/StructuredMemory.hs#inspect-before-querying | Covered | Four scopes, harness read-only boundary, schema-first workflow and actual scope example documented. |
| TOOL-054 | Read structured memory with `database_query` | `packages/agent-runtime/src/Agent/Runtime/Database.hs:151` | Pages/StructuredMemory.hs#inspect-before-querying | Covered | Scope/sql, read-only single statement, SELECT example, explicit LIMIT and no transaction-control contract documented. |
| TOOL-055 | Mutate structured memory with `database_execute` | `packages/agent-runtime/src/Agent/Runtime/Database.hs:169` | Pages/StructuredMemory.hs#durable-changes | Covered | Scope/sql/purpose, transactional mutation, read-only harness, approval and read-back/recovery documented. |
| TOOL-056 | Search prior conversations with `conversation_search` | `packages/agent-runtime/src/Agent/Runtime/Database.hs:193` | Pages/StructuredMemory.hs#conversation-search | Covered | Full-text query example, nondeleted user/assistant text, limit defaults/bounds and privacy now documented. |
| TOOL-057 | Search learned skills `skill_search` | `packages/agent-cli/src/Agent/CLI/LearnedSkills.hs:443` | Pages/LearnedSkills.hs#management-tools | Covered | Scope applicability, active-only results and limits documented. |
| TOOL-058 | Read filesystem/MCP/learned skill with `view_skill` | `packages/agent-cli/src/Agent/CLI/LearnedSkills.hs:468` | Pages/Skills.hs#inspect-a-skill | Covered | Scope/revision and catalog-name lookup, ambiguity and remote failure documented. |
| TOOL-059 | Create learned skill `skill_create` | `packages/agent-cli/src/Agent/CLI/LearnedSkills.hs:553` | Pages/LearnedSkills.hs#capture-a-verified-procedure | Covered | Payload, evidence, scope, activation and priority documented. |
| TOOL-060 | Update learned skill `skill_update` | `packages/agent-cli/src/Agent/CLI/LearnedSkills.hs:586` | Pages/LearnedSkills.hs#revise-and-restore | Covered | Revision conflict/reconciliation and changed fields described. |
| TOOL-061 | Archive learned skill `skill_archive` | `packages/agent-cli/src/Agent/CLI/LearnedSkills.hs:618` | Pages/LearnedSkills.hs#revise-and-restore | Covered | History retention and future-context boundary described. |
| TOOL-062 | Restore revision with `skill_rollback` | `packages/agent-cli/src/Agent/CLI/LearnedSkills.hs:639` | Pages/LearnedSkills.hs#revise-and-restore | Covered | New revision rather than erased history, status restoration described. |
| TOOL-063 | Discover/install/invoke/reload filesystem skills and front matter | `packages/agent-cli/src/Agent/CLI/Skills.hs:299` | Pages/Skills.hs | Covered | Search precedence, metadata, examples and installer boundaries supplied. |
| TOOL-064 | MCP-provided skills, manifests and entry resources | `packages/agent-cli/src/Agent/CLI/Skills.hs:134` | Pages/Skills.hs#remote-skills | Covered | Metadata versus on-demand instructions, resource/manifest verification, unavailable/malformed cases and untrusted instruction boundary now documented. |
| TOOL-065 | Packaged `$add-model` | `packages/agent-cli/skills/add-model/SKILL.md:1` | Pages/Models.hs; Pages/Skills.hs | Covered | A discoverable model-configuration workflow exists. |
| TOOL-066 | Packaged `$skill-installer` | `packages/agent-cli/skills/skill-installer/SKILL.md:1` | Pages/Skills.hs#install-a-skill | Covered | Input sources, destinations and review requirement covered. |
| TOOL-067 | Packaged `$telegram-agent` | `packages/agent-cli/skills/telegram-agent/SKILL.md:1` | Pages/Telegram.hs | Covered | Setup and operation have dedicated guide. |
| TOOL-068 | Packaged `$resume-claude` | `packages/agent-cli/skills/resume-claude/SKILL.md:1` | Pages/Skills.hs#resume-external-history | Covered | Claude root/override, candidate selection, trust and verification documented. |
| TOOL-069 | Packaged `$resume-codex` | `packages/agent-cli/skills/resume-codex/SKILL.md:1` | Pages/Skills.hs#resume-external-history | Covered | Codex invocation, root/override, selection and verification documented. |
| TOOL-070 | Packaged `$resume-cursor` | `packages/agent-cli/skills/resume-cursor/SKILL.md:1` | Pages/Skills.hs#resume-external-history | Covered | Cursor stores, format limitations, selection and verification documented. |
| TOOL-071 | Packaged `$resume-grok` | `packages/agent-cli/skills/resume-grok/SKILL.md:1` | Pages/Skills.hs#resume-external-history | Covered | Grok root/override, selection, trust and verification documented. |
| TOOL-072 | Packaged `$learn-about-user` | `packages/agent-cli/skills/learn-about-user/SKILL.md:1` | Pages/Skills.hs#profile-and-ci-workflows | Covered | Confirmed identity, public scope, review consent and durable profile revision documented. |
| TOOL-073 | Packaged `$wait-for-ci` | `packages/agent-cli/skills/wait-for-ci/SKILL.md:1` | Pages/Skills.hs#profile-and-ci-workflows | Covered | Activation, exact revision, terminal outcome and unresolved states documented. |
| TOOL-074 | Always-active post-task learning review | `packages/agent-cli/skills/post-task-learning-review/SKILL.md:1` | Pages/LearnedSkills.hs#post-task-review; Pages/Skills.hs#bundled-workflows | Covered | Top-level activation, search before create, evidence threshold, at most two mutations and silent no-op documented. |

## Integrations and native host extensions

| ID | Implemented function/use case | Source evidence | Existing documentation | Coverage | Missing details or acceptance boundary |
|---|---|---|---|---|---|
| TOOL-075 | Codex discovery `tool_search` | `packages/agent-mcp/src/Agent/MCP/Fleet.hs:925` | Pages/Mcp.hs#tool-payloads | Covered | Query/limit/default, next-request declarations, direct invocation and partial catalog handling documented. |
| TOOL-076 | Generic discovery `mcp_search` | `packages/agent-mcp/src/Agent/MCP/Fleet.hs:1105` | Pages/Mcp.hs#tool-payloads | Covered | Optional query/server/limit, limits and discovery-only JSON example documented. |
| TOOL-077 | Grok discovery `search_tool` | `packages/agent-mcp/src/Agent/MCP/Fleet.hs:1149` | Pages/Mcp.hs#tool-payloads | Covered | Query, bounded/default limit, subsequent use_tool and partial catalogs documented. |
| TOOL-078 | Generic invocation `mcp_call` | `packages/agent-mcp/src/Agent/MCP/Fleet.hs:1550` | Pages/Mcp.hs#tool-payloads | Covered | Qualified name/arguments, actual schema discovery, reconnect changes and inspect-before-mutation-retry documented. |
| TOOL-079 | Grok invocation `use_tool` | `packages/agent-mcp/src/Agent/MCP/Fleet.hs:1576` | Pages/Mcp.hs#tool-payloads | Covered | Required tool_name/tool_input, qualified discovery, schema adherence and uncertain outcome recovery documented. |
| TOOL-080 | Direct discovered server tool invocation | `packages/agent-mcp/src/Agent/MCP/Client/Internal/Operations.hs:538` | Pages/Mcp.hs#permissions | Covered | Read-only annotation trust and independent mutation approval described; external schemas remain server-owned. |
| TOOL-081 | List MCP resources/templates `mcp_list_resources` | `packages/agent-mcp/src/Agent/MCP/Fleet.hs:1608` | Pages/Mcp.hs#tool-payloads | Covered | Optional server versus all connected resource servers, template instantiation and separate discovery documented. |
| TOOL-082 | Read MCP resource `mcp_read_resource` | `packages/agent-mcp/src/Agent/MCP/Fleet.hs:1645` | Pages/Mcp.hs#tool-payloads | Covered | Required server/uri, payload example, listing/resource_link flow, non-HTTP and text/binary result trust documented. |
| TOOL-083 | Native browser navigate | `packages/agent-native-bridge/src/Agent/CLI/BrowserTools.hs:68` | Pages/BrowserControl.hs | Covered | URL payload, native availability, destination review and observation workflow supplied. |
| TOOL-084 | Native browser DOM snapshot | `packages/agent-native-bridge/src/Agent/CLI/BrowserTools.hs:77` | Pages/BrowserControl.hs | Covered | No-argument snapshot, fresh refs and untrusted content boundary supplied. |
| TOOL-085 | Native browser click | `packages/agent-native-bridge/src/Agent/CLI/BrowserTools.hs:82` | Pages/BrowserControl.hs | Covered | ref input, stale-ref recovery and observe-before-retry supplied. |
| TOOL-086 | Native browser type/submit | `packages/agent-native-bridge/src/Agent/CLI/BrowserTools.hs:91` | Pages/BrowserControl.hs | Covered | ref/text/submit fields, submission consequences and privacy supplied. |
| TOOL-087 | Native browser key | `packages/agent-native-bridge/src/Agent/CLI/BrowserTools.hs:104` | Pages/BrowserControl.hs#key-scroll-and-display-details | Covered | Key spellings, focused-control boundary and observation documented. |
| TOOL-088 | Native browser scroll | `packages/agent-native-bridge/src/Agent/CLI/BrowserTools.hs:113` | Pages/BrowserControl.hs#key-scroll-and-display-details | Covered | CSS pixels, signs and down-scroll example documented. |
| TOOL-089 | Native browser back | `packages/agent-native-bridge/src/Agent/CLI/BrowserTools.hs:124` | Pages/BrowserControl.hs | Covered | No-input history operation and form/navigation state caution supplied. |
| TOOL-090 | Native browser forward | `packages/agent-native-bridge/src/Agent/CLI/BrowserTools.hs:129` | Pages/BrowserControl.hs | Covered | No-input history operation, expected existing history and reobservation supplied. |
| TOOL-091 | Native browser reload | `packages/agent-native-bridge/src/Agent/CLI/BrowserTools.hs:134` | Pages/BrowserControl.hs | Covered | No-input refresh, reference invalidation and state-loss caution supplied. |
| TOOL-092 | Native browser screenshot | `packages/agent-native-bridge/src/Agent/CLI/BrowserTools.hs:139` | Pages/BrowserControl.hs | Covered | No-input visual observation, selected-page context and visible-data disclosure supplied. |
| TOOL-093 | Native browser list tabs | `packages/agent-native-bridge/src/Agent/CLI/BrowserTools.hs:144` | Pages/BrowserControl.hs | Covered | No-input native-host listing and returned IDs versus arbitrary desktop tabs supplied. |
| TOOL-094 | Native browser switch tab | `packages/agent-native-bridge/src/Agent/CLI/BrowserTools.hs:149` | Pages/BrowserControl.hs | Covered | Returned tab_id, relist-on-closed-tab recovery and fresh observation supplied. |
| TOOL-095 | Native browser downloads | `packages/agent-native-bridge/src/Agent/CLI/BrowserTools.hs:158` | Pages/BrowserControl.hs | Covered | No-input listing, inspect completion/actual path and untrusted downloaded data supplied. |
| TOOL-096 | `computer` screenshot | `packages/agent-computer-use/src/Agent/ComputerUse.hs:259` | Pages/BrowserControl.hs#key-scroll-and-display-details | Covered | Action array, consent and backend-selected display/no-selector limitation documented. |
| TOOL-097 | `computer` click/double_click | `packages/agent-computer-use/src/Agent/ComputerUse.hs:260` | Pages/BrowserControl.hs | Covered | Coordinates, button/keys fields, action batch and verify-after-action workflow supplied. |
| TOOL-098 | `computer` scroll | `packages/agent-computer-use/src/Agent/ComputerUse.hs:272` | Pages/BrowserControl.hs | Covered | Anchor coordinates, scroll deltas, keys and fresh observation supplied. |
| TOOL-099 | `computer` move/drag | `packages/agent-computer-use/src/Agent/ComputerUse.hs:279` | Pages/BrowserControl.hs | Covered | Pointer fields, bounded path and nonrollback recovery supplied. |
| TOOL-100 | `computer` type/keypress | `packages/agent-computer-use/src/Agent/ComputerUse.hs:301` | Pages/BrowserControl.hs | Covered | text versus keys, length limit, focus and privacy supplied. |
| TOOL-101 | `computer` wait | `packages/agent-computer-use/src/Agent/ComputerUse.hs:308` | Pages/BrowserControl.hs#key-scroll-and-display-details | Covered | Fixed two seconds and need to verify readiness documented. |
| TOOL-102 | Telegram document delivery `send_telegram_document` | `packages/agent-runtime/src/Agent/Runtime/GatewayBridge.hs:177` | Pages/Telegram.hs#gateway-tools | Covered | Managed context, private canonical path, caption/filename and uncertain-send recovery documented. |
| TOOL-103 | Email library: mailbox list/search/read/attachment | `packages/agent-mail/src/Agent/Mail/Transport.hs:1` | Pages/ToolExecution.hs#connected-email | Covered | Integration-only exposure, account/mailbox selection, transport fields/limits, incomplete output and untrusted attachment handling documented. |
| TOOL-104 | Email library: create/update draft, reply, send | `packages/agent-mail/src/Agent/Mail/Transport.hs:194` | Pages/ToolExecution.hs#email-mutations | Covered | Draft/reply/send fresh approvals, exact content, Gmail/Microsoft versus non-sending IMAP, limits and Sent reconciliation documented. |

## Cross-cutting authorization coverage

| ID | Implemented function/use case | Source evidence | Existing documentation | Coverage | Missing details or acceptance boundary |
|---|---|---|---|---|---|
| TOOL-105 | Read-only, ask and project auto-approval policies | `packages/agent-cli/src/Agent/CLI/Approval/Decision.hs:94` | Pages/Approvals.hs#choose-the-approval-policy | Covered | Session/project persistence and non-TTY restriction described. |
| TOOL-106 | Allow once, remember tool, project-wide approval, deny | `packages/agent-cli/src/Agent/CLI/Approval.hs:281` | Pages/Approvals.hs#approval-decisions | Covered | Scope is explicit; no live card-verification claim made. |
| TOOL-107 | Command danger checks despite broad tool access | `packages/agent-tools/src/Agent/Tools/Dangerous.hs:48` | Pages/ToolExecution.hs#filesystem-and-classification | Covered | Hard-deny deletion/temp patterns, non-bypass and bounded recovery documented. |
| TOOL-108 | Read-only shell/GHCi classification | `packages/agent-tools/src/Agent/Tools/ShellReadOnly.hs:1`; `packages/agent-tools/src/Agent/Tools/Ghci/Classify.hs:1` | Pages/ToolExecution.hs#filesystem-and-classification | Covered | Conservative test/read classification, GHCi pure/effectful and uncertain-type checking documented. |
| TOOL-109 | Per-invocation sandbox escalation | `packages/agent-tools/src/Agent/Tools/ShellPermission.hs:1` | Pages/Approvals.hs#shell-isolation-and-escalation | Covered | Fresh justification/confirmation versus full access and OS permissions are differentiated. |
| TOOL-110 | Computer workflow consent and safety checks | `packages/agent-computer-use/src/Agent/ComputerUse.hs:1530` | Pages/Approvals.hs#computer-use | Covered | Independent approval, toggle reset and provider safety checks described. |
| TOOL-111 | Allowed filesystem roots and session scratch | `packages/agent-tools/src/Agent/Tools/FileSystem.hs:1` | Pages/ToolExecution.hs#filesystem-and-classification | Covered | Canonical roots/symlinks, read-only skill roots, optional host grants and scratch/host boundaries documented. |
| TOOL-112 | Tool availability versus embedding/provider policy | `packages/agent-cli/src/Agent/CLI/Runtime/Orchestration/Tools.hs:1` | Pages/ToolExecution.hs | Covered | Dialect/host/provider availability table distinguishes conditional capabilities. |
| TOOL-113 | Telegram photo delivery `send_telegram_photo` | `packages/agent-runtime/src/Agent/Runtime/GatewayBridge.hs:181` | Pages/Telegram.hs#gateway-tools | Covered | Managed context, private path/caption/name, format failure and fallback documented. |
| TOOL-114 | Telegram voice delivery `send_telegram_voice` | `packages/agent-runtime/src/Agent/Runtime/GatewayBridge.hs:185` | Pages/Telegram.hs#gateway-tools | Covered | Prepared audio versus synthesis, format limits, private path and host gate documented. |
| TOOL-115 | Telegram reaction `react_to_telegram_message` | `packages/agent-runtime/src/Agent/Runtime/GatewayBridge.hs:212` | Pages/Telegram.hs#gateway-tools | Covered | Emoji, optional message ID/current-message default and invalid target recovery documented. |
| TOOL-116 | Telegram choice prompt `ask_telegram_choice` | `packages/agent-runtime/src/Agent/Runtime/GatewayBridge.hs:227` | Pages/Telegram.hs#gateway-tools; #choice-recovery | Covered | Question/options, 1–8 labels, expiry, user/chat binding and stale/cancel recovery documented. |
| TOOL-117 | Telegram allow user `allow_telegram_user` | `packages/agent-runtime/src/Agent/Runtime/GatewayBridge.hs:241` | Pages/Telegram.hs#gateway-tools; #allowlisted-users | Covered | Query/user ID/reply targeting, managed versus local controls and list verification documented. |
| TOOL-118 | Telegram deny user `deny_telegram_user` | `packages/agent-runtime/src/Agent/Runtime/GatewayBridge.hs:255` | Pages/Telegram.hs#gateway-tools; #group-access-example | Covered | Target selection, subsequent admission versus running turns, separate interruption and no rollback documented. |
| TOOL-119 | Telegram list users `list_telegram_users` | `packages/agent-runtime/src/Agent/Runtime/GatewayBridge.hs:269` | Pages/Telegram.hs#gateway-tools | Covered | Empty arguments, managed scope and allowed/recent users output documented. |
| TOOL-120 | Provider-hosted X search `x_search` | `packages/agent-cli/src/Agent/CLI/Tools.hs:99` | Pages/WebAccess.hs#hosted-search | Covered | Grok gate, hosted execution, citations and non-publishing/private-access boundary documented. |

## Priority findings

1. **Prose reconciliation complete:** all 120 inventoried rows now have usable documentation.
2. **Runtime validation remains separate:** documentation-site checks validate rendering/links, not successful external mutations, native permissions or provider execution.
3. **Ongoing maintenance:** compare these source-backed operation guides against registry/schema changes rather than assuming every host exposes the same capabilities.

## Corroborating tests and residual limits

Existing test sources worth using as acceptance fixtures include:

- `packages/agent-tools/test/Agent/ToolDispatchSpec.hs`
- `packages/agent-cli/test/Agent/CLI/ToolsSpec.hs`
- `packages/agent-native-bridge/test/Agent/CLI/BrowserToolsSpec.hs`
- `packages/agent-openai/test/Agent/OpenAI/ImageGenerationSpec.hs`
- `packages/agent-tui/test/Agent/TUI/PresentationSpec.hs`

They were **not executed** for this audit. The enumerated constructor/operation coverage does not prove every platform branch, every numeric schema bound, every provider API behavior, or every error path. External MCP and native app implementations have their own release/version boundaries. Mail is deliberately identified as implemented library behavior with unproven local CLI exposure, not counted as a set of available model tools. Future work should generate a schema-level field inventory and live capability snapshots per supported host/dialect to close those residual boundaries.
