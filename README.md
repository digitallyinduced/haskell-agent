# haskell-agent

**An independent agent harness, written in Haskell.**

`haskell-agent` is a coding agent built in Haskell. Use OpenAI, xAI, and
OpenRouter models with first-class GHCi integration and a runtime designed
around types, pure functions, explicit effects, and composable concurrency.

## Try it out

```console
nix run "git+ssh://git@github.com/digitallyinduced/haskell-agent"
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

Install [Nix](https://nixos.org/download/) with flakes enabled, make sure your
GitHub SSH access is configured, then install `haskell-agent`:

```console
nix profile add "git+ssh://git@github.com/digitallyinduced/haskell-agent"
```

## Run

Start an interactive session:

```console
agent-cli
```

Run a one-shot task:

```console
agent-cli -p \
  "inspect this Cabal project, explain its architecture, and run its tests"
```

Start in an isolated Git worktree:

```console
agent-cli --worktree
```

Use `--provider openai`, `--provider xai`, or `--provider openrouter` to
override automatic provider detection.

### Authentication

Works with your Codex subscription, Grok subscription, and provider API keys.

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

That is also why this project does not wrap one vendor CLI or bind its core
runtime to one model family. The goal is an independent system that can use
the best available model while preserving one coherent tool, session,
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
          +--------------------+--------------------+
          |                    |                    |
     agent-openai          agent-xai        agent-openrouter
          |                    |                    |
   OpenAI / ChatGPT            xAI              OpenRouter
```

The provider-neutral loop sees typed turns, tool calls, tool results, usage,
and streamed events. Provider packages own wire formats, authentication,
transport, and provider-specific continuation. Presentation consumes the same
events through renderer-independent state.

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
