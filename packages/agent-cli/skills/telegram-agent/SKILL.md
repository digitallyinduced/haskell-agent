---
name: telegram-agent
description: Set up, start, stop, or troubleshoot a Telegram bot backed by the local agent harness.
when-to-use: Use when the user asks to create, configure, run, or manage a Telegram agent or Telegram bot.
argument-hint: "[setup|start|stop|status]"
user-invocable: true
---

# Telegram agent

Use the dedicated `agent-telegram` executable. Do not use an `agent-cli`
Telegram subcommand.

## Security rule

Never ask the user to paste a BotFather token into chat, a prompt, or a tool
argument. Agent conversations and tool calls may be persisted. Secret entry
belongs exclusively to the interactive `agent-telegram setup` command, which
disables terminal echo and stores the token in a private gateway file.

## Setup workflow

1. Explain that the user must open Telegram and message `@BotFather`.
2. Tell them to run `/newbot`, choose the bot name and username, and keep the
   returned token private.
3. Help them obtain their numeric Telegram user ID, for example by messaging
   `@userinfobot`. This ID is an allowlist entry, not a secret.
4. Determine the desired provider and project working directory. Default to
   the current project and a non-mutating approval policy. Only enable
   `--yolo` when the user explicitly requests it.
5. Ask the user to run this command in their own interactive terminal:

   ```sh
   agent-telegram setup --provider <provider> --cwd <project> \
     --allowed-user <numeric-id>
   ```

   Add `--model`, `--effort`, or `--yolo` only when requested. The command
   validates the token using Telegram's `getMe` API.
6. Wait until the user confirms setup completed. Do not attempt to pipe or
   inject the token through a shell tool.
7. Start the configured gateway with:

   ```sh
   agent-telegram start
   ```

8. Verify it with:

   ```sh
   agent-telegram status
   ```

9. Tell the user to open their new bot and send `/start`. The bot supports
   `/new` for a fresh agent session and `/session` for the current session ID.
   Text, voice messages, and message reactions are persisted before processing.
   Voice transcription uses the user's existing Codex subscription, so Codex
   must already be logged in on the machine running the gateway.

## Telegram delivery behavior

- The gateway shows typing and a native rich-message draft while an agent turn
  is running.
- Agent Markdown is converted to Telegram-safe HTML, with a plain-text fallback.
- A reply containing exactly one supported Telegram reaction emoji is delivered
  as a reaction to the triggering message instead of as a separate message.
- Inbound reaction changes become ordinary durable agent turns, including
  reaction removals.
- Voice messages are limited to 10 minutes and 20 MB. They are downloaded to a
  private temporary gateway file, transcribed through `codex app-server`, then
  deleted before the transcript enters the normal durable agent-session path.

## Management

Use `agent-telegram stop` to stop the background gateway. Logs and durable
queue state live below `~/.haskell-agent/gateways/telegram/`.
