# claude-agent-sdk-haskell

A typed Haskell client for the Claude Agent SDK stream protocol.

The package follows the same architecture as Anthropic's Python and
TypeScript Agent SDKs:

```text
Haskell application
    -> claude-agent-sdk-haskell
        -> official `claude` CLI subprocess
            -> Anthropic
```

It does not reimplement Claude Code's agent loop and does not call private
Claude.ai endpoints. The official CLI continues to own authentication,
model execution, built-in tools, permissions, context management, and
session persistence. This package owns the typed Haskell API, process
lifecycle, newline-delimited stream JSON transport, message parsing, and
session-safe response assembly.

This release implements queries, persistent clients, typed messages, errors,
replaceable transports, and the bidirectional control-protocol foundation
needed by `haskell-agent`. Permission and in-process MCP callbacks are
supported. Hooks and the complete background-task lifecycle are not yet a
drop-in replacement for every Python or TypeScript SDK feature.

## Requirements

- The official `claude` executable must be installed.
- Claude Code authentication must already be configured, or its documented
  API-key environment must be available to the child process.
- The embedding application remains responsible for following Anthropic's
  authentication and product policies.

## One-shot queries

```haskell
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

import Claude.Agent.SDK
import qualified Data.Text.IO as Text

main :: IO ()
main = do
    let options =
            (defaultClaudeAgentOptions "claude" ".")
                { model = Just "claude-sonnet-4-6"
                , permissionMode = Just PermissionDontAsk
                }

    query options "Explain this repository." print >>= \case
        Left err ->
            Text.putStrLn (renderClaudeSDKError err)
        Right result ->
            print result.result
```

`query` creates a client, submits one prompt, waits for a terminal result, and
then closes the complete Claude Code process group.

## Persistent clients

Use `withClaudeSDKClient` when multiple turns should share one Claude Code
process and session:

```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Claude.Agent.SDK

runTurn client previous prompt =
    withClaudeSDKTurn
        client
        (pure True) -- whether the host transcript still matches
        previous
        Nothing     -- optional model override
        Nothing     -- optional effort override
        \turn -> do
            response <- queryTurn turn prompt print
            pure ((, pure ()) <$> response)

main = do
    let options = defaultClaudeAgentOptions "claude" "."
    withClaudeSDKClient options \client -> do
        first <- runTurn client Nothing "Hello"
        case first of
            Left err -> print err
            Right result -> do
                second <-
                    runTurn
                        client
                        (Just result.sessionId)
                        "Continue"
                print second
```

The callback passed to `withClaudeSDKTurn` returns a value together with an
`IO ()` commit action. A failed callback or commit terminates the active child
and invalidates its continuation. This prevents the next turn from resuming
Claude-side context that the embedding application rolled back.

Legacy clients use `abort` to force-close the active process and start the next
turn on a fresh session. Handler-aware clients first use the in-band
`interrupt` control operation and fall back to force-close when it fails. If
the interrupted turn reaches the host commit boundary, its continuation is
invalidated so a discarded result can never leave the reusable process ahead
of authoritative host state.

## Control handlers

Use `withClaudeSDKClientWithHandlers` to enable the SDK initialize handshake,
permission callbacks, in-process MCP messages, and outbound controls:

```haskell
let handlers =
        defaultClaudeAgentHandlers
            { canUseTool = Just \request ->
                if request.toolName == "Read"
                    then pure (ToolPermissionAllow Nothing [])
                    else pure (ToolPermissionDeny "Denied by host policy" False)
            }

withClaudeSDKClientWithHandlers options handlers \client ->
    withClaudeSDKTurn client (pure True) Nothing Nothing Nothing \turn -> do
        -- `interrupt`, `setModel`, `setPermissionMode`, `getContextUsage`,
        -- `stopTask`, and raw `sendControlRequest` are available here.
        result <- queryTurn turn "Inspect the project." print
        pure ((, pure ()) <$> result)
```

Handler-aware clients have one supervised stdout reader. It demultiplexes
ordinary messages, concurrent control requests, matching control responses,
and cancellation frames. Writes are serialized, missing handlers fail closed,
and initialization/control/shutdown waits are bounded by
`ClaudeAgentHandlers`.

## Response semantics

Claude Code emits newline-delimited JSON messages. The SDK parses them into:

- `SystemMessage`
- `UserMessage`
- `AssistantMessage`
- `ResultMessage`
- `StreamEvent`
- `ConversationResetMessage`
- forward-compatible unknown message and content constructors

Visible messages are buffered until a successful `ResultMessage`. Before
invoking the callback, the SDK:

1. deduplicates messages by UUID;
2. applies `assistant.supersedes` replacements;
3. applies refusal-fallback retractions;
4. rejects interactive control requests;
5. validates that the terminal session ID matches the active turn.

Consequently, a failed or wrong-session turn cannot publish partial,
subsequently retracted output through the callback.

`query`, `queryClient`, and `queryTurn` return the canonical completed
response and therefore omit noncanonical `StreamEvent` partials. Applications
that deliberately implement live partial-message and retraction handling can
use the lower-level `sendQuery` and `receiveMessage` functions.

The result's `usage` field is available directly. `resolveTurnUsage` converts
Claude Code's process-cumulative `modelUsage` snapshots into per-turn usage
without double-counting earlier fallback usage.

## Options

`ClaudeAgentOptions` covers the commonly used CLI controls:

- executable and working directory;
- minimal (empty), custom, or full Claude Code system prompt and tool selection;
- allowed and disallowed tools;
- permission mode and bypass acknowledgement;
- model and effort;
- new, resumed, or continued conversations;
- settings and MCP configuration;
- safe mode, partial messages, slash commands, and browser integration;
- exact child environment and client application label;
- prompt-write, startup, inactivity, and whole-turn timeouts;
- a maximum structured-output record size.

`extraArgs` allows new Claude Code flags to be supplied before the typed
options surface catches up.

`withClaudeSDKClientWithoutTools` is a convenience wrapper for auxiliary
queries that must disable Claude Code's built-in tools.

## Transport

`Claude.Agent.SDK.Transport` exposes the low-level asynchronous transport
contract. The built-in client currently constructs the official CLI
subprocess transport, using:

```text
claude -p \
  --input-format stream-json \
  --output-format stream-json \
  --verbose
```

The transport uses ordinary pipes, drains stderr continuously, bounds writes
and shutdown, and terminates the child process group on release. The public
`Transport` record and `TransportFactory` type also make the wire boundary
replaceable. Use `withClaudeSDKClientWithTransport` to supply an in-memory,
remote, or otherwise custom transport. The factory receives a
`TransportRequest` for every fresh start or resume, including the session ID,
resume flag, model, and effort. The SDK connects, closes, and otherwise owns
the lifecycle of each returned transport.

## Errors

Expected failures use `ClaudeSDKError`:

- `CLINotFoundError`
- `CLIConnectionError`
- `CLIProtocolError`
- `ProcessError`
- `ResultError`
- `CLIJSONDecodeError`
- `MessageParseError`

Use `renderClaudeSDKError` for a human-readable diagnostic. Stderr diagnostics
are bounded so an unexpectedly noisy child cannot grow memory indefinitely.

## Tests

The package test suite uses small fake `claude` executables. It exercises the
public query/client APIs, subprocess arguments and environment, input
encoding, persistent process reuse, typed parsing, forward compatibility,
deduplication, retractions, terminal errors, and session validation without
requiring a Claude account or network access.
