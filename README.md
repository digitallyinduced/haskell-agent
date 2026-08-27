# haskell-agent

An independent agent harness, written in Haskell.

<img width="1426" height="871" alt="Screenshot 2026-08-23 at 10 43 49 PM" src="https://github.com/user-attachments/assets/9da99007-484a-4c8a-9bb1-ca35abf8ae05" />

## Try it out

```bash
nix run "github:digitallyinduced/haskell-agent"
```

## Supported LLM Providers

- OpenAI (Subscription)
- xAI (Subscription)
- Claude (Subscription)
- OpenRouter (API billing)

## What is distinctive

Most agent harnesses are effectively untyped imperative programming
environments. A model emits loosely structured commands that mutate files,
processes, conversation state, and other shared resources. Correctness depends
on conventions enforced at runtime, often after effects have already begun.

`haskell-agent` is an exploration in a different direction. Model output is
treated as untrusted input at the boundary. Accepted actions are decoded into
typed values, state changes are expressed as pure transformations where
possible, and effects are interpreted explicitly by the runtime. The model
remains probabilistic; the environment in which its actions execute does not
have to be.

- **A functional agent runtime:** protocol states, tool policies, transport
  ownership, UI transitions, and agent lifecycles are modeled with algebraic
  data types. Pure transformations are separated from effectful boundaries,
  while STM coordinates shared concurrent state.
- **GHCi as part of the agent architecture:** every model gets a persistent
  typed workspace. The harness distinguishes pure expressions from effectful
  actions, preserves bindings across calls, and recovers or restarts GHCi when
  interruption makes its state uncertain.
- **First-class model dialects:** providers own authentication, billing, and
  transport, while dialects own the model-facing prompt, tool surface, schema
  conventions, project-instruction formatting, and subagent protocol. This
  keeps Codex-style and Grok Build behavior intact even when a transport such
  as OpenRouter serves models from several families.
- **Cross-provider state and billing policy:** provider transitions preserve
  the pending turn and durable session state. Credential failover understands
  account cooldowns and prevents automatic fallback from silently converting
  subscription usage into API-credit spending.
- **Explicit response ownership:** reusable WebSocket requests carry
  generation-scoped ownership. If an exchange is interrupted, malformed, or
  returned before its terminal frame, the connection is poisoned rather than
  risking old frames entering a later response.
- **Types as a path toward safer agency:** typed tool decoding, approval rules,
  and execution policies are the current foundation for deeper work with
  LLMs, ADTs, type checkers, effect systems, and program verification.

## Features

- **Choice of models and billing:** use OpenAI/Codex, xAI/Grok, OpenRouter, or
  Claude Code through subscriptions or API keys, and add local or hosted
  Responses-compatible models through the user model catalog.
- **Interactive terminal workflow:** choose between fullscreen and inline
  interfaces with streaming Markdown, live todo progress, one-shot operation,
  and image attachments from files or the clipboard.
- **PostgreSQL-backed memory and portable sessions:** persist conversations and
  scoped learned guidance, resume or search past work, compact long histories,
  and switch supported providers without losing the pending turn or durable
  session state.
- **Efficient long-running agents:** page persisted history on demand and
  virtualize TUI scrolling to bound memory and rendering work as conversations
  grow.
- **Parallel agents and isolated work:** delegate to persisted subagents with a
  configurable concurrency limit and create fresh sessions in managed Git
  worktrees.
- **Built-in coding tools:** run shell commands, opt into a persistent GHCi
  workspace, search the web, and connect local MCP servers. Approval policies
  keep mutating operations under user control.
- **Guided agent workflows:** use plan mode, reusable skills, and scoped learned
  guidance for repeatable tasks and project or user preferences.
- **Multimodal input and live voice dictation:** attach images and files, or
  press `Ctrl+R` on macOS to stream microphone audio to xAI and insert the live
  transcript into the prompt.
- **Telegram access:** run a durable, allowlisted Telegram gateway with
  per-conversation sessions, multimodal messages, approvals, retries, and
  bounded concurrent processing.

These are important product features, but not the core differentiation.

## Install

Install [Nix](https://nixos.org/download/) with flakes enabled, then install
`haskell-agent`:

```console
nix profile add github:digitallyinduced/haskell-agent
```

## Run

Start an interactive session:

```console
agent-cli
```

The provider's Bash/shell execution tool is enabled by default. Enable the
persistent `run_ghci` tool when needed:

```console
agent-cli --ghci
```

For GHCi-only operation, disable Bash explicitly:

```console
agent-cli --ghci --no-bash
```

During an interactive session, switch the available shell tools without
restarting:

```console
/shell ghci
/shell bash
```

Use `/shell` to show the current selection. `/shell both` and `/shell none`
are also supported.

Run a one-shot task:

```console
agent-cli -p \
  "inspect this Cabal project, explain its architecture, and run its tests"
```

Start in an isolated Git worktree:

```console
agent-cli --worktree
```

Use `--provider openai`, `--provider xai`, `--provider openrouter`, or
`--provider claude-code` to override automatic provider detection. Claude Code
is selected explicitly rather than by auto-detection.

### Telegram

Create a bot with BotFather, find your numeric Telegram user ID, and run the
interactive setup command:

```console
agent-telegram setup --provider openai --cwd /path/to/project \
  --allowed-user 123456789
agent-telegram start
agent-telegram status
```

Setup reads the BotFather token without terminal echo, validates it against
Telegram, and stores it separately from the non-secret gateway configuration.
Never paste the bot token into an agent conversation.

Only allowlisted users are handled. Each private chat, group, and forum topic
gets its own persisted session; work survives restarts and separate chats run
concurrently. Mutating tools request approval through inline buttons by
default. Use `agent-telegram users` to manage the allowlist and the bot's
`/new`, `/session`, `/status`, and `/retry` commands to manage sessions.

The gateway supports multimodal messages, files, reactions, and group chats.
Ask the normal agent to “set up a Telegram agent” to activate the built-in
`telegram-agent` setup skill.

On NixOS, use the flake's reusable multi-instance service module instead of
maintaining the systemd and PostgreSQL runtime configuration by hand. See
[`docs/nixos.md`](docs/nixos.md).

### Model catalog and local models

The model picker is driven by a versioned catalog. The application ships its
default OpenAI, xAI, and OpenRouter entries, then merges an optional user file:

```text
~/.haskell-agent/models.json
```

User entries with the same `id` replace shipped entries; new entries are
appended. The `id` is the stable name accepted by `/model` and `--model`.
Secrets are not stored in this file: custom connections name an environment
variable containing their API key.

For example, an unauthenticated local server exposing the streaming OpenAI
Responses API at `POST /v1/responses` can be configured as:

```json
{
  "version": 1,
  "connections": {
    "ollama": {
      "api": "responses",
      "base_url": "http://localhost:11434/v1",
      "api_key_optional": true,
      "request_timeout_seconds": 600
    }
  },
  "models": [
    {
      "id": "qwen-local",
      "connection": "ollama",
      "model": "qwen2.5-coder:32b",
      "dialect": "generic-responses",
      "context_window": 32768,
      "label": "local"
    }
  ]
}
```

Select it with `agent-cli --model qwen-local` or from `/model`. For an
authenticated endpoint, set `"api_key_env": "MY_MODEL_API_KEY"` and export
that variable. Omit `"api_key_optional": true` when the key is required.
Set `context_window` to the model endpoint's documented token limit so
`/compact` can bound both its summary request and the installed snapshot.
Inference still works when this metadata is absent, but `/compact` refuses to
guess a portable model's limit.

Supported dialects are:

- `codex` for Codex-style prompts and tools
- `grok-build` for the Grok Build protocol
- `generic-responses` for portable Responses-compatible models

Custom connections are selected manually and are not considered for automatic
billing fallback. Built-in connection names (`openai`, `xai`, and
`openrouter`) are reserved. A malformed catalog is reported at startup with
the file and invalid field instead of being silently ignored.

The built-in `add-model` skill handles requests such as “use the model running
at this URL”, “add this OpenRouter model”, or “OpenAI released a new model”.
Invoke it explicitly with `/add-model`, `$add-model`, or describe the request
naturally and let the agent activate it.

The built-in `learn-about-user` skill can derive consent-reviewed technical
defaults from a confirmed public GitHub profile. Invoke it with
`/learn-about-user`, `$learn-about-user`, or a natural-language request.

### Authentication

Works with your Codex, Grok, and Claude subscriptions, plus provider API keys.

### Voice dictation

Press `Ctrl+R` in the prompt composer, speak, and press `Enter` to stop
(or `Esc` to cancel). Recording stays in the TUI; it does not suspend or close
the session. On macOS, audio is streamed to xAI and the live transcript is
inserted at the cursor. Dictation uses the configured xAI credentials; set
`XAI_STT_LANGUAGE` to override the default `en`.

### Claude Code subscription

Install Claude Code, authenticate it with a first-party Claude subscription,
and select the provider:

```console
claude auth login
agent-cli --provider claude-code --model sonnet
```

The integration keeps a `claude -p` process alive through the reusable
[`claude-agent-sdk-haskell`](packages/claude-agent-sdk-haskell/README.md)
package. Claude Code owns tool execution and compaction while the harness
renders events and persists its session. Pass `--yolo` to bypass Claude Code's
permission checks.

Anthropic's [June 15, 2026 subscription-policy
update](https://support.claude.com/en/articles/15036540-use-the-claude-agent-sdk-with-your-claude-plan)
says that Claude Agent SDK, `claude -p`, and third-party app usage currently
draw from Claude subscription usage limits. Anthropic's current
[Agent SDK documentation](https://platform.claude.com/docs/en/agent-sdk/overview)
also says third-party developers need prior approval to offer Claude.ai login
or subscription rate limits in their products. Technical availability does
not replace that approval requirement; consult the linked documents for
current terms. See [`packages/agent-claude/README.md`](packages/agent-claude/README.md)
for details.

### Local MCP servers

Configure local stdio MCP servers in `~/.haskell-agent/config.json`:

```json
{
  "version": 1,
  "mcpInitStrategy": "auto",
  "mcpServers": {
    "seo-mcp": {
      "command": "nix",
      "args": ["run", "/absolute/path/to/seo-mcp"],
      "env": {
        "GOOGLE_APPLICATION_CREDENTIALS": "/absolute/path/to/credentials.json"
      },
      "startupTimeoutSeconds": 120,
      "requestTimeoutSeconds": 60
    }
  }
}
```

In an interactive session, `/mcp` opens a local-server manager. Use the arrow
keys or `j`/`k` to navigate, Enter to inspect discovered tools, `a` to add a
server, Space to enable or disable it, `x` to remove it, and `r` to restart the
MCP runtime. Saved changes restart the runtime while resuming the same session.
Environment variable values are never displayed.

`mcpInitStrategy` accepts `auto`, `progressive`, or `blocking`. `auto`
starts MCP servers progressively for interactive sessions so the prompt is
available immediately, while one-shot commands wait for MCP initialization.

Enabled servers are shared with subagents. Only tools annotated
`readOnlyHint: true` are exposed. Blocking startup publishes them as
`server__tool`; progressive startup makes `mcp_search` and `mcp_call`
available while servers connect.

### Secret entry

The built-in `ask_secret` tool reads secrets through a masked prompt and gives
the model only a private temporary-file path, keeping values out of chat and
tool arguments. Files are removed when the tool runtime closes.

## Ideas and direction

Why an independent harness matters, why code and Haskell are useful
foundations, and how types, effects, and verification could make agents safer
are discussed in [`IDEAS.md`](IDEAS.md).

## Architecture

```text
                 agent-cli / future native clients
                              |
                   provider-neutral events
                              |
     +------------------- agent-core -------------------+
     | agent loop | tools | approvals | agents | state |
     +-------------------------+------------------------+
                               |
                    canonical Responses model
                               |
       +---------------+---------------+---------------+
       |               |               |               |
 agent-openai      agent-xai    agent-openrouter  agent-claude
       |               |               |               |
OpenAI / ChatGPT       xAI          OpenRouter      Claude Code
```

The provider-neutral loop sees typed turns, tool calls, tool results, usage,
and streamed events. Provider packages own wire formats, authentication,
transport, and provider-specific continuation. Presentation consumes the same
events through renderer-independent state.

`agent-claude` delegates its generic process transport, protocol decoding, and
session client to
[`claude-agent-sdk-haskell`](packages/claude-agent-sdk-haskell/README.md),
leaving subscription policy and `Agent.Loop` translation in the provider
adapter.

Model targets resolve independently to a provider transport and a model-facing
dialect. OpenAI models use the Codex dialect, xAI models use the Grok Build
dialect, and OpenRouter selects Codex, Grok Build, or a portable Responses
dialect from the model family.

## Development

All compiler and package dependencies come from the pinned Nix flake.

```console
nix develop
cabal test all
```

From the development shell, `repl` opens the agent under GHCi. Edit the
harness, leave the running agent, reload the changed modules, and resume the
same session without rebuilding the executable.

See [`AGENTS.md`](AGENTS.md) for the complete development workflow, including
multi-package GHCi sessions, Nix package maintenance, and CLI testing.

## License

MIT. See [`LICENSE`](LICENSE).
