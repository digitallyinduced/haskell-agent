# about

coding harnesses are going to be the primary interface for humans to work with the computer.
we are building the independent agent harness that works with any llm model.

the agent harness will provide acess to latest frontier models and open source models

we will support cli, native macos desktop, windows, ios, android and web.

while we are starting out as a coding harness, we plan to expand the harness to deal with all kinds of digital work.

# architecture

we are using haskell and ghc as the primary runtime system for the agent.
type safety and the approach of functional program maps well to the problem space. monads and haskels concurrency system seem well suited for agent harnesses that need to deal with many concurrent agents.

we follow the tool defintions that are used by the first party lab harnesses. e.g. for oai we use the tool defintions that codex provides out of the box, for grok we use the tool definitions that grok build provides out of the box. This way


# ghci

use ghci instead of compiling the code. E.g. instead of nix flake check start a ghci and load in the necessary modules. This is way faster than doing a full compile.

# haskell
- Prefer Control.Exception.Safe over Control.Exception
