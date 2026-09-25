# Configuration, model, provider, and MCP coverage audit

Source baseline: `3100a05e06f23c442fe9b6c6b4c037a69c2ec668`, plus the current uncommitted documentation, inspected 2026-09-22.

## Method and evidence

This is a source-to-documentation audit, not a new execution test. Every row below has **source inspection** evidence only. Existing tests corroborate intended behavior but were not executed for this audit. “Covered” means the documentation supplies usable instructions for that item; it does not mean every error branch or external service has been verified.

Coverage statuses and “None” refer to the **published HSX documentation website**, not to the entire repository. Additional engineering-only material exists in `docs/models.md` (catalog/local connections), `docs/mcp.md` (protocol and capabilities), `docs/mcp-oauth.md` (authorization, step-up scopes, login/logout), and `docs/meta-console.md` (typed configuration actions and application boundaries). These documents are useful migration sources, but are not registered website pages. In particular, partial website OAuth/recovery coverage must not be read as “no repository documentation exists.”

The finite configuration inventory was read from every decoder in `Config.hs`, `ModelConfig.hs`, and `Project.hs`, including rejected compatibility keys. Environment inventory was obtained from provider option loaders, account credential loaders, and runtime environment lookups. Related pages were read rather than treating a keyword hit as coverage.

Source abbreviations below expand to these repository-relative files:

- **HC** — `packages/agent-runtime/src/Agent/Runtime/Config.hs`
- **MC** — `packages/agent-runtime/src/Agent/Runtime/ModelConfig.hs`
- **PS** — `packages/agent-runtime/src/Agent/Runtime/Project.hs`
- **OA** — `packages/agent-accounts/src/Agent/Accounts/Auth/OpenAI.hs`
- **XA** — `packages/agent-accounts/src/Agent/Accounts/Auth/Grok.hs`
- **AA** — `packages/agent-accounts/src/Agent/Accounts/Auth.hs`
- **OR** — `packages/agent-openrouter/src/Agent/OpenRouter/Options.hs`
- **XO** — `packages/agent-xai/src/Agent/XAI/Options.hs`
- **GO** — `packages/agent-gemini/src/Agent/Gemini/Options.hs`

`HC:427` therefore means `packages/agent-runtime/src/Agent/Runtime/Config.hs:427`. Documentation abbreviations expand to `docs/src/Documentation/Pages/<name>.hs`; text after `#` names the actual section identifier.

Registry totals, counting accepted object-container keys and rejected compatibility keys separately: **50 machine-configuration keys** (8 root, 17 MCP server, 4 OAuth, 5 web fetch, 2 LSP container, 12 LSP server, 2 worktree); **20 model-catalog keys** (3 root, 6 connection, 11 model); **15 persisted-settings keys** (7 root, 3 account, 5 model). Some rows group fields sharing exactly the same coverage gap; totals are field counts, not row counts.

## Machine configuration

| ID | Field / use case | Source | Documentation | Coverage | Missing details or conclusion |
|---|---|---|---|---|---|
| CFG-001 | `version` (default 1, only schema 1 accepted) | HC:555, HC:885 | Configuration#machine-settings | Covered | Integer and accepted value documented. |
| CFG-002 | `theme` (default midnight; six choices) | HC:556, HC:574 | Configuration#machine-settings | Covered | Allowed strings and default documented. |
| CFG-003 | `mcpInitStrategy` | HC:557 | Configuration#machine-settings; Mcp#startup-and-tool-discovery | Covered | Auto/progressive/blocking and interactive distinction documented. |
| CFG-004 | `mcpServers` named object | HC:559 | Configuration#mcp-settings | Covered | Linked complete entry reference. |
| CFG-005 | `webFetch` object | HC:561 | Configuration#web-fetch-settings | Covered | Example, scope, enablement and boundaries documented. |
| CFG-006 | `lsp` object | HC:563 | Configuration#language-server-settings | Covered | Linked language-server operational guide. |
| CFG-007 | `worktree` object | HC:565; `packages/agent-cli/src/Agent/CLI/Worktree.hs:198` | Configuration#worktree-policy | Covered | Complete object example and fresh-read versus startup-snapshot behavior. |
| CFG-008 | `maxConcurrentAgents` optional positive integer | HC:567, HC:960 | Configuration#limit-concurrent-agents | Covered | Scope precedence and CLI example supplied. |
| CFG-009 | Missing/blank/invalid machine file | HC:599, HC:640 | Configuration#machine-settings | Covered | Defaults versus validation errors distinguished. |
| CFG-010 | Reading configuration may assign and persist missing remote MCP identities | HC:646, HC:723 | Configuration#verify-a-change | Covered | Migration writes, permissions and preserve-identity guidance supplied. |
| CFG-011 | Concurrent updates, atomic replacement, private config/revision-key files | HC:661, HC:688, HC:764 | Configuration#verify-a-change | Covered | Locking, atomic replacement, private sidecars and stale-preview recovery supplied. |
| CFG-012 | `webFetch.enabled` false | HC:497 | Configuration#web-fetch-settings | Covered | Explicitly disabled by default. |
| CFG-013 | `webFetch.allowedDomains` empty list denies | HC:498, HC:985 | Configuration#web-fetch-settings; WebAccess | Covered | Domain restrictions and operational examples present. |
| CFG-014 | `webFetch.timeoutSeconds` 60; 1–300 | HC:499, HC:968 | Configuration#web-fetch-settings | Covered | Unit/default/bounds supplied. |
| CFG-015 | `webFetch.maxContentBytes` 10485760; 1–52428800 | HC:501, HC:972 | Configuration#web-fetch-settings | Covered | Unit/default/bounds supplied. |
| CFG-016 | `webFetch.maxInlineBytes` 100000; 1–1048576 and at most content limit | HC:503, HC:976 | Configuration#web-fetch-settings | Covered | Cross-field constraint supplied. |
| CFG-017 | `lsp.enabled` false | HC:540 | Configuration#language-server-settings | Covered | Default and prerequisite supplied. |
| CFG-018 | `lsp.servers` empty object | HC:541 | Configuration#language-server-settings | Covered | Naming and Nix executable setup supplied. |
| CFG-019 | LSP `transport` stdio-only | HC:509 | Configuration#language-server-settings | Covered | Unsupported transports explicitly rejected. |
| CFG-020 | LSP `restartOnCrash` false; true rejected | HC:514 | Configuration#language-server-settings | Covered | Rejection documented. |
| CFG-021 | LSP `maxRestarts` rejected if present | HC:519 | Configuration#language-server-settings | Covered | Rejection documented. |
| CFG-022 | LSP `command` required nonblank | HC:524, HC:992 | Configuration#language-server-settings | Covered | Type and constraints supplied. |
| CFG-023 | LSP `args` empty string array | HC:525 | Configuration#language-server-settings | Covered | Executable arguments versus shell string distinguished. |
| CFG-024 | LSP `env` empty string map | HC:526 | Configuration#language-server-settings | Covered | Overrides and diagnostic redaction supplied. |
| CFG-025 | LSP `extensionToLanguage` required nonempty map | HC:527, HC:994 | Configuration#language-server-settings | Covered | Decoder initially defaults empty but validation rejects empty; docs correctly describe usable requirement. |
| CFG-026 | LSP `initializationOptions` optional JSON | HC:528 | Configuration#language-server-settings | Covered | Server-specific ownership documented. |
| CFG-027 | LSP `settings` optional JSON | HC:529 | Configuration#language-server-settings | Covered | Workspace settings documented. |
| CFG-028 | LSP `workspaceFolder` optional string | HC:530; `packages/agent-runtime/src/Agent/Runtime/Lsp/Path.hs:25`; `packages/agent-runtime/src/Agent/Runtime/Lsp.hs:404` | Configuration#lsp-workspace-folder | Covered | Relative base, existing canonical-contained directory, symlink boundary, single-root contract and recovery supplied. |
| CFG-029 | LSP `startupTimeoutMilliseconds` 15000; 1–120000 | HC:531, HC:1009 | Configuration#language-server-settings | Covered | Default/unit/bounds supplied. |
| CFG-030 | LSP `shutdownTimeoutMilliseconds` 5000; 1–120000 | HC:533, HC:1019 | Configuration#language-server-settings | Covered | Default/unit/bounds supplied. |
| CFG-031 | `worktree.fetchLatestUpstream` true | HC:548; `packages/agent-cli/src/Agent/CLI/Worktree.hs:820` | Configuration#worktree-policy | Covered | Remote precedence, cached default branch, isolated fetch, hard failure, and offline local HEAD behavior. |
| CFG-032 | `worktree.inactivityDays` 1; positive | HC:549, HC:883; `packages/agent-cli/src/Agent/CLI/Worktree.hs:585`; `packages/agent-cli/src/Agent/CLI/Runtime/Orchestration/Tools/Scratch.hs:135` | Configuration#worktree-policy; ParallelAgents#retention-and-recovery | Covered | Activity sources, elapsed-day threshold, startup cleanup timing/budgets and safety exclusions. |

## MCP configuration

| ID | Field / use case | Source | Documentation | Coverage | Missing details or conclusion |
|---|---|---|---|---|---|
| MCP-CFG-001 | `enabled` true | HC:427 | Mcp#server-field-reference | Covered | Disabled records retained but not started. |
| MCP-CFG-002 | `url` optional | HC:428, HC:901 | Mcp#server-field-reference | Covered | HTTP versus stdio exclusivity documented. |
| MCP-CFG-003 | `connectionId` optional managed identity | HC:429, HC:905 | Configuration#mcp-lifecycle | Covered | Format, preserve-only/non-reuse boundary and migration recovery supplied. |
| MCP-CFG-004 | `connectionCredentials` optional protected-store selector | HC:430, HC:741 | Configuration#mcp-lifecycle | Covered | Identity-dependent default and legacy migration explicit false supplied. |
| MCP-CFG-005 | `connectionGeneration` optional lifecycle marker | HC:431; `packages/agent-native-bridge/src/Agent/CLI/McpConnection.hs:146` | Configuration#mcp-lifecycle | Covered | Authorization/enablement generation replacement and stale callback boundary supplied. |
| MCP-CFG-006 | `displayName` optional nonblank | HC:432, HC:912 | Mcp#server-field-reference | Covered | Type and presentation purpose documented. |
| MCP-CFG-007 | `command` empty default; required without URL | HC:433, HC:901 | Mcp#server-field-reference | Covered | Executable versus shell and stdout protocol discipline documented. |
| MCP-CFG-008 | `args` empty list | HC:434 | Mcp#server-field-reference | Covered | Separate argument list shown in example. |
| MCP-CFG-009 | `cwd` optional | HC:435; `packages/agent-runtime/src/Agent/Runtime/Mcp/Startup.hs:54` | Configuration#mcp-lifecycle | Covered | Default startup workspace and explicit relative process-directory interpretation supplied. |
| MCP-CFG-010 | `env` empty map | HC:436 | Mcp#server-field-reference; Mcp#remote-authentication | Covered | Secret-storage warning and HTTP token keys explained. |
| MCP-CFG-011 | `startupTimeoutSeconds` 30, positive | HC:437 | Mcp#server-field-reference | Covered | Initial Nix build troubleshooting supplied. |
| MCP-CFG-012 | `requestTimeoutSeconds` 60, positive | HC:439 | Mcp#progress-and-user-input | Covered | Idle timer, progress extension and cancellation limits documented. |
| MCP-CFG-013 | `oauth` optional HTTP-only object | HC:441, HC:932 | Mcp#remote-authentication | Covered | Interactive setup and object example supplied. |
| MCP-CFG-014 | `protocol` auto/modern/legacy | HC:442, HC:447 | Mcp#protocol-negotiation | Covered | Probe/fallback versions and deadline explained. |
| MCP-CFG-015 | `roots` false | HC:443 | Mcp#permissions | Covered | Disclosure boundary and per-server enablement documented. |
| MCP-CFG-016 | `sampling` false | HC:444; `packages/agent-cli/src/Agent/CLI/McpSampling.hs:34` | Configuration#mcp-sampling | Covered | Text/roles, isolation, token clamp, tool rejection and active billing supplied. |
| MCP-CFG-017 | `logLevel` optional eight-value enum | HC:445, HC:466 | Mcp#server-field-reference | Covered | Values and absence semantics supplied. |
| MCP-CFG-018 | `oauth.clientId` optional nonblank | HC:488, HC:934 | Mcp#remote-authentication | Covered | Issued-identifier advice supplied. |
| MCP-CFG-019 | `oauth.clientSecret` requires clientId | HC:489, HC:936 | Mcp#remote-authentication | Covered | Constraint documented. |
| MCP-CFG-020 | `oauth.clientIdMetadataUrl` HTTPS URL with path | HC:490, HC:944 | Mcp#remote-authentication | Covered | Constraint documented. |
| MCP-CFG-021 | `oauth.scopes` empty string array; no blanks | HC:491, HC:953 | Mcp#remote-authentication | Covered | Default, example, challenge/reauthorization path supplied. |

## Model catalog

| ID | Field / use case | Source | Documentation | Coverage | Missing details or conclusion |
|---|---|---|---|---|---|
| MOD-CFG-001 | `version` required integer 1 | MC:159 | Models#catalog-reference | Covered | Required version and strict JSON documented. |
| MOD-CFG-002 | `connections` empty object | MC:160 | Models#catalog-reference | Covered | Definition and example supplied. |
| MOD-CFG-003 | `models` empty array | MC:162 | Models#catalog-reference | Covered | Definition and example supplied. |
| MOD-CFG-004 | Connection `api` required | MC:168 | Models#connection-fields | Covered | Custom Responses connection constraint supplied. |
| MOD-CFG-005 | Connection `provider` optional internal declaration | MC:169 | Models#connection-fields | Covered | Not a custom routing control; reserved connections documented. |
| MOD-CFG-006 | Connection `base_url` | MC:170 | Models#connection-fields | Covered | Custom endpoint requirement and version prefix documented. |
| MOD-CFG-007 | Connection `api_key_env` | MC:171 | Models#connection-fields | Covered | Variable name versus secret and optional-key relationship documented. |
| MOD-CFG-008 | Connection `api_key_optional` false | MC:172 | Models#connection-fields | Covered | Local unauthenticated example and protected-service warning supplied. |
| MOD-CFG-009 | Connection `request_timeout_seconds` 600 | MC:173 | Models#connection-fields | Covered | Positive seconds specified. |
| MOD-CFG-010 | Model `id` required | MC:179 | Models#model-fields | Covered | Nonempty/no whitespace and local selector meaning supplied. |
| MOD-CFG-011 | Model `connection` required | MC:180 | Models#model-fields | Covered | Existing connection requirement supplied. |
| MOD-CFG-012 | Model `model` defaults to id | MC:181 | Models#model-fields | Covered | Wire name and no remapping built-in/gateway rule supplied. |
| MOD-CFG-013 | Model `dialect` required | MC:182 | Models#model-fields | Covered | Three dialects and compatibility requirement supplied. |
| MOD-CFG-014 | Model `context_window` optional positive | MC:183 | Models#model-fields | Covered | Compaction requirement and server consistency supplied. |
| MOD-CFG-015 | Model `label` optional | MC:184 | Models#model-fields | Covered | Presentation purpose supplied. |
| MOD-CFG-016 | Model `reasoning_efforts` optional | MC:185 | Models#model-fields | Covered | Allowed values/nonempty/unique constraints supplied. |
| MOD-CFG-017 | Model `default_reasoning_effort` optional | MC:187 | Models#model-fields | Covered | Must be in supported array; troubleshooting supplied. |
| MOD-CFG-018 | Model `supports_async_tool_calls` false | MC:188; `packages/agent-openai/src/Agent/OpenAI/LoopBackend.hs:895` | Models#async-contract | Covered | Wire async flag, streamed scheduling, delayed outputs, tool approval and replay boundary supplied. |
| MOD-CFG-019 | Model `default` false | MC:189 | Models#gateway-models | Covered | Exact-one built-in default validation and remembered/explicit choice distinction supplied. |
| MOD-CFG-020 | Model `fallback_priority` optional nonnegative | MC:190; `packages/agent-cli/src/Agent/CLI/ProviderFallback.hs:127` | Models#provider-fallback | Covered | Eligible connections, stable ties, strictly lower same-provider rank, exhaustion and billing/data route supplied. |
| MOD-CFG-021 | User/shipped catalog merging and gateway metadata | MC:438 | Models#catalog-reference; Models#gateway-models | Covered | Replacements, reserved names and authoritative live gateway inventory distinguished. |
| MOD-CFG-022 | Concrete local endpoint, generation and tool protocol verification | MC:164 | LocalModelTutorial#test-generation; LocalModelTutorial#test-tools; `docs/local-model-verification.md` | Covered | Complete endpoint, generation and disposable README tool-call procedures and expected outcomes supplied. Full harness exercise remains explicitly unexecuted; that is a validation limit, not a missing documentation procedure. |

## Persisted project and user settings

These settings are a different file and failure model from machine `config.json`. Configuration now links both locations to the dedicated persisted-settings reference.

| ID | Field / use case | Source | Documentation | Coverage | Missing details or conclusion |
|---|---|---|---|---|---|
| SET-CFG-001 | Settings `version` default 1 | PS:224 | PersistedSettings#root-fields | Covered | Separate schema, defaults and recovery behavior supplied. |
| SET-CFG-002 | `autoApprove` false | PS:225 | PersistedSettings#root-fields | Covered | Checkout policy, default, preferred UI and safe inspection supplied. |
| SET-CFG-003 | `mouseCapture` true | PS:226, PS:366 | PersistedSettings#root-fields | Covered | User preference, default and recovery supplied. |
| SET-CFG-004 | `lastModel` optional | PS:227, PS:330 | PersistedSettings#inheritance | Covered | Checkout → primary clone → user precedence, write events and reset instructions supplied. |
| SET-CFG-005 | `titleModel` optional | PS:228, PS:244 | Models#session-title-model | Covered | Supported picker, persistence and automatic fallback supplied. |
| SET-CFG-006 | `lastAccounts` empty list | PS:229 | PersistedSettings#account-records | Covered | Account preference persistence, no-secrets guarantee and reset behavior supplied. |
| SET-CFG-007 | `maxConcurrentAgents` optional | PS:231, PS:374 | PersistedSettings#root-fields | Covered | Precedence, settings path and sample project JSON supplied. |
| SET-CFG-008 | `lastAccounts[].provider` | PS:139 | PersistedSettings#account-records | Covered | Required recognized provider supplied. |
| SET-CFG-009 | `lastAccounts[].selectionId` required nonblank | PS:143 | PersistedSettings#account-records | Covered | Identity versus secret distinction supplied. |
| SET-CFG-010 | `lastAccounts[].accountId` empty default | PS:144 | PersistedSettings#account-records | Covered | Account identity purpose and default supplied. |
| SET-CFG-011 | Saved model `provider`, `connection`, `model`, `transportModel`, `dialect` | PS:166 | PersistedSettings#saved-model | Covered | Five-field saved selection, compatibility defaults and catalog distinction supplied. |
| SET-CFG-012 | Unreadable/malformed settings use defaults; malformed selection entries discarded independently | PS:262, PS:302 | PersistedSettings#recovery | Covered | Contrast with machine errors and safe recovery procedure supplied. |
| SET-CFG-013 | Worktree-local policy versus shared primary-clone remembered model | PS:269, PS:344 | PersistedSettings#inheritance | Covered | Exact persistence boundary, switch events and reset supplied. |

## Provider environment configuration

The following are actual loader inputs, not suggested variables from other tools. Values should be documented with precedence and security implications; environment override support is not a promise that arbitrary third-party endpoints are compatible.

| ID | Variable / use case | Source | Documentation | Coverage | Missing details or conclusion |
|---|---|---|---|---|---|
| ENV-CFG-001 | `CODEX_ACCESS_TOKEN` | OA:373 | Providers#openai | Covered | Coding auth source and non-refreshable token limitation supplied. |
| ENV-CFG-002 | `CODEX_AUTH_JSON` | OA:374; `packages/agent-accounts/src/Agent/Accounts/Auth/Types.hs:224` | Providers#external-credential-formats | Covered | Flat/tokens object fields, array first-only behavior, decode rejection and source precedence supplied. |
| ENV-CFG-003 | `CODEX_HOME` | OA:376 | Providers#openai | Covered | Auth-file directory override supplied. |
| ENV-CFG-004 | `CODEX_ACCOUNT_ID` | OA:725 | Environment#credentials | Covered | Explicit account identity used with token environment credentials supplied. |
| ENV-CFG-005 | `CODEX_ID_TOKEN` | OA:726 | Environment#credentials | Covered | Identity derivation from ID token supplied. |
| ENV-CFG-006 | `OPENAI_OAUTH_CLIENT_ID` | OA:311 | Environment#credentials | Covered | OAuth application override and leave-unset guidance supplied. |
| ENV-CFG-007 | `GROK_AUTH_JSON` | XA:90; `packages/agent-accounts/src/Agent/Accounts/Auth/Types.hs:167` | Providers#external-credential-formats | Covered | Flat/one-level nesting, key precedence, all optional fields and expiry order supplied. |
| ENV-CFG-008 | `GROK_ACCESS_TOKEN` | XA:91 | Providers#xai | Covered | Token source and refresh limitation supplied. |
| ENV-CFG-009 | `XAI_OAUTH_CLIENT_ID` | XA:159 | Environment#credentials | Covered | OAuth application override and leave-unset guidance supplied. |
| ENV-CFG-010 | `OPENROUTER_API_KEY` | AA:531 | Providers#openrouter | Covered | Setup, billing and managed-credential precedence supplied. |
| ENV-CFG-011 | `GOOGLE_API_KEY` | AA:590 | Providers#gemini | Covered | Precedence over Gemini variable and managed credentials supplied. |
| ENV-CFG-012 | `GEMINI_API_KEY` | AA:591 | Providers#gemini | Covered | Same explicit precedence supplied. |
| ENV-CFG-013 | `OPENROUTER_BASE_URL` | OR:47 | Environment#transport-overrides | Covered | Default endpoint and credential destination warning supplied. |
| ENV-CFG-014 | `OPENROUTER_MODEL_MAP` | OR:48 | Environment#transport-overrides | Covered | Exact mapping grammar, malformed/duplicate entries and catalog distinction supplied. |
| ENV-CFG-015 | `OPENROUTER_DEFAULT_MODEL` | OR:49 | Environment#transport-overrides | Covered | Absent/non-slug fallback and default supplied. |
| ENV-CFG-016 | `OPENROUTER_TIMEOUT_SECONDS` | OR:50 | Environment#transport-overrides | Covered | Default, parsing, positive-value advice and example supplied. |
| ENV-CFG-017 | `OPENROUTER_HTTP_REFERER` | OR:51 | Environment#transport-overrides | Covered | Optional attribution header and default supplied. |
| ENV-CFG-018 | `OPENROUTER_APP_TITLE` | OR:52 | Environment#transport-overrides | Covered | Optional attribution header and default supplied. |
| ENV-CFG-019 | `XAI_GROK_BASE_URL` | XO:154 | Environment#transport-overrides | Covered | Native override, gateway exclusion and destination warning supplied. |
| ENV-CFG-020 | `XAI_GROK_MODEL_MAP` | XO:155 | Environment#transport-overrides | Covered | Mapping grammar and gateway exclusion supplied. |
| ENV-CFG-021 | `XAI_GROK_DEFAULT_MODEL` | XO:156 | Environment#transport-overrides | Covered | Transport default and catalog distinction supplied. |
| ENV-CFG-022 | `XAI_GROK_TIMEOUT_SECONDS` | XO:157 | Environment#transport-overrides | Covered | Seconds, default and parsing supplied. |
| ENV-CFG-023 | `XAI_GROK_CLIENT_VERSION` | XO:158 | Environment#transport-overrides | Covered | Shipped default and compatibility-only advice supplied. |
| ENV-CFG-024 | `GEMINI_BASE_URL` | GO:35 | Environment#transport-overrides | Covered | Direct versus subscription endpoint distinction supplied. |
| ENV-CFG-025 | `GEMINI_CODE_ASSIST_BASE_URL` | GO:37 | Environment#transport-overrides | Covered | Subscription endpoint and default supplied. |
| ENV-CFG-026 | `GEMINI_DEFAULT_MODEL` | GO:38 | Environment#transport-overrides | Covered | Transport default supplied. |
| ENV-CFG-027 | `GEMINI_TIMEOUT_SECONDS` | GO:39 | Environment#transport-overrides | Covered | Timeout/default/parsing supplied. |
| ENV-CFG-028 | `CLAUDE_CODE_EXECUTABLE` | `packages/agent-claude/src/Agent/Claude/Auth.hs:295` | Environment#helpers | Covered | Explicit executable override and trusted-program boundary supplied. |
| ENV-CFG-029 | `BROWSER` for authentication URLs | `packages/agent-runtime/src/Agent/Runtime/Browser.hs:19` | Environment#helpers | Covered | Single-executable contract, defaults and launch recovery supplied. |
| ENV-CFG-030 | `HASKELL_AGENT_OBSERVATION_DIRECTORY` | `packages/agent-runtime/src/Agent/Runtime/Session/Observation.hs:145` | Environment#runtime-paths | Covered | Socket-directory relocation/default and ancillary failure supplied. |
| ENV-CFG-031 | `HASKELL_AGENT_INBOX_DIRECTORY` | `packages/agent-runtime/src/Agent/Runtime/Session/Inbox.hs:95` | Environment#runtime-paths | Covered | Socket-directory relocation/default and shared process configuration supplied. |
| ENV-CFG-032 | `HASKELL_AGENT_EXECUTABLE` | `packages/agent-runtime/src/Agent/Runtime/AgentSessions/Process.hs:619` | Environment#runtime-paths | Covered | Managed process executable override supplied. |
| ENV-CFG-033 | `HASKELL_AGENT_APPLE_SESSION_TITLE` | `packages/agent-cli/src/Agent/CLI/AppleTitle.hs:77` | Models#session-title-model | Covered | Helper override and fallback documented. |
| ENV-CFG-034 | `XAI_STT_LANGUAGE` | `packages/agent-xai/src/Agent/XAI/Transcription.hs:216` | Voice | Covered | Default English and Portuguese environment example supplied. |
| ENV-CFG-035 | `MODEL_API_KEY` | `packages/agent-runtime/config/models.default.json:26` | Providers#meta-model-api | Covered | Direct Meta versus OpenRouter routes distinguished. |
| ENV-CFG-036 | `OPENAI_API_KEY` for direct auxiliary authentication | OA:190 | Providers#openai; Voice | Covered | Coding account distinction and dictation setup supplied. |
| ENV-CFG-037 | MCP entry `env.MCP_ACCESS_TOKEN` | `packages/agent-mcp/src/Agent/MCP/Client/Internal/Runtime.hs:1735` | Mcp#remote-authentication | Covered | Bearer-token configuration documented; this is per-entry config, not an arbitrary process environment lookup. |
| ENV-CFG-038 | MCP entry `env.MCP_OAUTH_TOKEN_FILE` | `packages/agent-mcp/src/Agent/MCP/Client/Internal/Runtime.hs:1721` | Mcp#remote-authentication | Covered | Legacy file shape, refresh/retry and privacy documented. |

## Authentication use-case completeness

| ID | Use case | Source | Documentation | Coverage | Missing details or conclusion |
|---|---|---|---|---|---|
| AUTH-CFG-001 | OpenAI managed plus external account pool; subscription versus API billing | OA:373; OA:698; `packages/agent-openai/src/Agent/OpenAI/Auth.hs:193` | Providers#multiple-accounts; Providers#credential-refresh; PersistedSettings#saved-accounts | Covered | Account ranking/reset, source writeback, identity invariant, static token limits and separate auth/quota cooldown supplied. Source-reviewed, not live credential validation. |
| AUTH-CFG-002 | xAI managed/external OAuth and token refresh | XA:90, XA:159; XA:244 | Providers#external-credential-formats; Providers#credential-refresh; Providers#account-recovery | Covered | Source precedence, rotation persistence, refresh-token retention and persistence failure recovery supplied. |
| AUTH-CFG-003 | OpenRouter key connection | AA:531 | Providers#openrouter | Covered | Key provisioning, process environment, managed precedence, billing and model choice supplied. |
| AUTH-CFG-004 | Gemini API key versus subscription OAuth | AA:590; `packages/agent-gemini/src/Agent/Gemini/Auth.hs:394` | Providers#gemini; Providers#gemini-eligibility | Covered | Credential precedence, tier/project/onboarding decisions and endpoint-specific failure procedure supplied. |
| AUTH-CFG-005 | Claude Code executable authentication | `packages/agent-claude/src/Agent/Claude/Auth.hs:295` | Providers#claude-diagnostics; Authentication#claude-code | Covered | Override/PATH discovery, exact auth probe, failure recovery and native fallback/account limits supplied. |
| AUTH-CFG-006 | Gateway-authoritative transport and credentials | AA:215; `packages/agent-accounts/src/Agent/Accounts/Gateway/Credentials.hs:415` | Providers#gateway-credentials; Models#gateway-models | Covered | Precedence, allowed explicit routes, origin identity and local disconnect versus remote revocation supplied. |

## Priority and residual boundaries

1. **Addressed:** Separate persisted-settings reference covers all 15 keys, checkout-local approval, model inheritance, account identity and default recovery.
2. **Addressed:** Environment reference covers the 38 inventoried inputs, credential precedence, mapping grammar and endpoint safety boundaries.
3. **Addressed:** Provider-specific formats, refresh/cooldowns, Gemini eligibility, Claude discovery and local versus remote gateway revocation are source-reviewed references.
4. **Addressed:** Managed MCP identity/generation, credential defaults, migration, process paths and isolated sampling behavior are documented.
5. **Addressed documentation / residual validation:** Model default/fallback/async semantics and the local-model full-harness procedure are documented. Live full-harness local-model verification remains unexecuted.
6. **Addressed:** Worktree policy, offline/fetch failure behavior, activity thresholds and cleanup timing are documented alongside LSP workspace resolution.

This file exhaustively enumerates the three finite JSON decoders above, not every serialized internal structure. Provider environment inputs listed here are a substantial explicit inventory, **not a proof of every environment variable in the repository**: terminal capability variables, developer/benchmark switches, server/daemon/mail/Telegram deployment settings, shell subprocess variables, and provider-native subprocess configuration belong to their corresponding surface audits. No general HTTP proxy or custom-CA support is inferred from dependency behavior. Future provider model IDs are intentionally not counted as separate product functions.

Useful source test suites for follow-up validation: `packages/agent-runtime/test/Agent/Runtime/ModelConfigSpec.hs`, `packages/agent-accounts/test/Agent/Accounts/SelectionSpec.hs`, and `packages/agent-accounts/test/Agent/Accounts/CredentialStoreSpec.hs`. These paths are pointers to test evidence, not claims that this audit ran the tests.

Implementation follow-up validation: the five changed/new page modules loaded together under `nix develop .#docs -c ghci -idocs/src`, and `git diff --check` passed. This is documentation compilation, not provider-service, account-refresh or endpoint compatibility validation.
