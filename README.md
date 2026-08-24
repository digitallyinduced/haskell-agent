# haskell-agent

<img width="1426" height="871" alt="Screenshot 2026-08-23 at 10 43 49 PM" src="https://github.com/user-attachments/assets/9da99007-484a-4c8a-9bb1-ca35abf8ae05" />

**An independent agent harness, written in Haskell.**

`haskell-agent` is a coding agent built in Haskell. Use OpenAI, xAI,
OpenRouter, and Claude Code models with first-class GHCi integration and a
runtime designed around types, pure functions, explicit effects, and
composable concurrency.

## Try it out

```console
nix run github:digitallyinduced/haskell-agent
```

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

The harness also includes the capabilities expected of a modern coding agent:
persistent sessions, subagents, worktrees, skills, plan mode, multimodal input,
web search, and interactive terminal interfaces. Those are important product
features, but not the core differentiation.

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

Only messages from allowlisted Telegram users are handled. Repeat
`--allowed-user` during setup, or manage the local allowlist later with
`agent-telegram users list|add ID|remove ID`; running gateways must be restarted
after an allowlist change. Private chats work directly. In groups and
supergroups, mention the bot, use a command addressed to its username (for
example `/new@your_bot`), or reply to one of its messages. Ambient group traffic
and messages from non-allowlisted members are ignored.

Each private chat, group, and forum topic is mapped to its own persisted agent
session under `~/.haskell-agent`; `/new` starts a fresh session, `/session`
shows the current session ID, `/status` reports queued/retrying/failed work,
and `/retry` requeues the latest failed turn. Group replies include the
sender's identity in the agent prompt and are posted as replies to the
triggering Telegram message.

The default approval mode asks through Telegram inline buttons when a mutating
tool is requested. `--deny-mutations` disables those tools and `--yolo`
auto-approves them. Approval and choice callbacks are scoped to the originating
conversation and allowlisted user.

Incoming updates and pending replies are persisted before they are processed.
Polling continues while agent turns run, conversations are processed in order,
and separate chats can run concurrently through a bounded worker pool. Pending
work, callback bindings, retry schedules, delivery checkpoints, and dead
letters resume when the gateway is restarted. Telegram 429/5xx responses and
transient turn failures use bounded backoff; agent turns have a 20-minute
deadline.

The gateway accepts edited messages, reactions, photos, documents, audio,
video, video notes, animations, stickers, locations, contacts, venues, polls,
and dice. Images are sent to multimodal providers natively; other downloaded
files are attached through Responses `input_file` content or a private local
path fallback. The agent can send documents, photos, and voice files, react to
messages, and ask generic inline-button questions through gateway-scoped tools.
Bot credentials remain in the parent gateway process and are never inherited
by the agent child.

The built-in `telegram-agent` skill lets the normal agent guide this setup.
Ask it to “set up a Telegram agent”; it will explain the BotFather steps,
direct secret entry to the interactive setup command, and start the configured
gateway after setup is complete.

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
      "label": "local"
    }
  ]
}
```

Select it with `agent-cli --model qwen-local` or from `/model`. For an
authenticated endpoint, set `"api_key_env": "MY_MODEL_API_KEY"` and export
that variable. Omit `"api_key_optional": true` when the key is required.

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

### Authentication

Works with your Codex, Grok, and Claude subscriptions, plus provider API keys.

### Claude Code subscription

Install Claude Code, authenticate it with a first-party Claude subscription,
and select the provider:

```console
claude auth login
agent-cli --provider claude-code --model sonnet
```

The reusable
[`claude-agent-sdk-haskell`](packages/claude-agent-sdk-haskell/README.md)
package keeps one `claude -p` process alive and exchanges structured messages
through Claude Code's bidirectional `stream-json` protocol. The thin
`agent-claude` adapter enforces subscription authentication and translates SDK
messages into provider-neutral harness events. Claude Code owns tool execution
and context compaction; the harness renders its assistant and tool events and
persists its session UUID. Clipboard and file image attachments are forwarded
as structured multimodal content.

The default permission mode is Claude Code's non-blocking `dontAsk` mode. Pass
`--yolo` to bypass Claude Code's permission checks. Permission mode is fixed
when the child process starts, and the harness's dynamic auto-approve,
plan-mode, and `/compact` controls are unavailable for this provider.

The integration disables Claude-specific project and user customizations and
MCP servers so it cannot block on hidden prompts. It also rejects API-key and
third-party cloud authentication, keeping this path restricted to first-party
subscription sessions.

Anthropic's [June 15, 2026 subscription-policy
update](https://support.claude.com/en/articles/15036540-use-the-claude-agent-sdk-with-your-claude-plan)
says that Claude Agent SDK, `claude -p`, and third-party app usage currently
draw from Claude subscription usage limits. Anthropic's current
[Agent SDK documentation](https://platform.claude.com/docs/en/agent-sdk/overview)
also says third-party developers need prior approval to offer Claude.ai login
or subscription rate limits in their products. Technical availability does
not replace that approval requirement; consult the linked documents for
current terms. See
[`packages/agent-claude/README.md`](packages/agent-claude/README.md)
for implementation and embedding details.

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

The harness starts enabled servers once per root session and shares their tools
with subagents. Blocking startup exposes read-only tools as
`server__tool`. Progressive startup exposes stable `mcp_search` and `mcp_call`
tools immediately, then publishes each server's read-only catalog as it
becomes ready. Only tools explicitly annotated `readOnlyHint: true` are
available. Live MCP fleets are reused across provider/session rebuilds when
their configuration is unchanged. Progressive `mcp_call` invocations also
restart a failed stdio transport once and retry the read-only call.

### Secret entry

Interactive root agents can request API keys and tokens with the built-in
`ask_secret` tool. The harness reads the value through a masked terminal
prompt, writes it to a private temporary file, and returns only the file path
to the model. This keeps the secret out of chat history, tool arguments,
transcripts, and command text.

Secret files are created below the session scratch directory with owner-only
permissions and removed when the agent's tool runtime closes. Commands should
delete them sooner after consumption when possible. This protects against
accidental persistence; it is not an isolation boundary against commands
running unsandboxed as the same operating-system user.

## Vision

### The agent harness is the interface

We believe the agent harness will become the primary interface through which
people use computers.

Instead of learning which application, menu, command, or workflow to use,
people will describe the outcome they want. Their harness will assemble
context, choose models, invoke tools, coordinate agents, manage permissions,
and carry work across devices and sessions.

A model can reason, but the harness turns that reasoning into useful work. The
harness is the layer that owns:

- identity, preferences, instructions, and long-term context
- access to files, processes, applications, services, and devices
- permissions and boundaries for consequential actions
- model selection, routing, retries, and billing policy
- concurrent agents that can divide work and communicate
- sessions that persist, resume, move between clients, and produce artifacts

Models will change. Providers will change. User interfaces will change. The
harness should remain the stable layer that the user controls.

### Code is the universal control surface

It is a coding harness because code is the universal control surface of the
computer. Through files, processes, protocols, APIs, compilers, and operating
system interfaces, an agent that can write and execute programs can use the
hardware and perform general digital work.

Coding is not one temporary vertical on the way to a broader agent. It is the
substrate that makes a general computer agent possible.

That is also why this project does not depend on one vendor CLI or bind its
core runtime to one model family. The goal is an independent system that can
use the best available model while preserving one coherent tool, session,
permission, and agent environment.

### Why Haskell

An agent harness is a concurrent, stateful program that manages untrusted
inputs and long-lived effects:

- streamed protocol events arrive incrementally
- tools read, write, and execute concurrently
- users interrupt work at arbitrary points
- credentials fail and accounts enter cooldown
- subagents start, communicate, persist, and terminate
- sessions must recover without mixing old and new state

These problems map naturally to Haskell:

- algebraic data types make protocol states and valid transitions explicit
- pure functions keep decoding, policy, state reduction, and assembly
  understandable
- effectful provider, tool, process, and filesystem operations stay at narrow
  boundaries
- STM makes mailboxes, cancellation, capacity, and shared state composable
- managed resource lifetimes give connections, subprocesses, and agents clear
  owners and shutdown paths

The point is not Haskell for its own sake. The point is a harness whose
behavior can be reasoned about when many agents, tools, streams, and failures
are active at once.

### LLMs, types, effects, and verification

We believe there is a large unexplored design space at the intersection of
LLMs and programming languages:

- **ADTs can define the agent's action language.** Instead of interpreting
  arbitrary text, the harness can ask a model to construct values from a
  closed set of valid operations and states.
- **Type checking can become part of the reasoning loop.** A model can propose
  a program, query its type, receive structured compiler feedback, and refine
  the proposal before any effect is executed.
- **Plans can become typed programs.** Dependencies, resources, permissions,
  concurrency, and expected outputs can be represented explicitly rather than
  hidden in prose.
- **Effect systems can make consequences explicit.** A model should describe
  not only what a program computes, but which files, processes, networks,
  credentials, and external services it may affect.
- **Verification can guard the effect boundary.** Preconditions, invariants,
  capability constraints, and postconditions can be checked before the
  harness commits an action to the outside world.
- **Compiler feedback is high-quality supervision.** Type errors, failed
  proofs, and violated properties give models precise signals before mistakes
  reach execution.

Today, this begins with typed protocol states, strict tool decoding, explicit
approval and concurrency policies, pure reducers, and GHCi-based type
exploration. The direction is deeper: agents that synthesize typed programs,
use type checkers, effect systems, and proof systems as collaborators, and
execute only after the runtime has established the required guarantees.

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
