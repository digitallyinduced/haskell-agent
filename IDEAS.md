# Ideas and direction

This document collects the longer-term ideas behind `haskell-agent`. They are
directions to explore, not a list of features that are already implemented.

## The agent harness is the interface

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

## Code is the universal control surface

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

## Why Haskell

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

## LLMs, types, effects, and verification

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
