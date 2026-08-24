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
4. Determine the desired provider and project working directory. The default
   approval mode asks through Telegram inline buttons. Use
   `--deny-mutations` when the user wants a read-only gateway, and only enable
   `--yolo` when the user explicitly requests full auto-approval.
5. Ask the user to run this command in their own interactive terminal:

   ```sh
   agent-telegram setup --provider <provider> --cwd <project> \
     --allowed-user <numeric-id>
   ```

   Repeat `--allowed-user` for multiple users. Add `--model`, `--effort`,
   `--deny-mutations`, or `--yolo` only when requested. The command validates
   the token using Telegram's `getMe` API.
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
   `/new` for a fresh agent session, `/session` for the current session ID,
   `/status` for queue/retry state, and `/retry` for the latest failed turn.
   To use it in a group, add the bot and mention its `@username`, reply to one
   of its messages, or address commands to it (for example
   `/new@your_bot_username`). Each group or forum topic has a shared agent
   session. Only messages from allowlisted users are accepted; ambient group
   traffic is ignored. Text, edited messages, reactions, photos, documents,
   audio, video, video notes, animations, stickers, locations, contacts,
   venues, polls, dice, and voice messages are persisted before processing.
   Voice transcription uses the user's existing Codex subscription, so Codex
   must already be logged in on the machine running the gateway.

## Telegram delivery behavior

- The gateway shows typing and a native rich-message draft whose text follows
  current model/tool/retry activity.
- Agent Markdown is converted to Telegram-safe HTML, with a plain-text fallback.
- Group and supergroup responses reply to the triggering message. Forum topics
  are isolated from one another.
- A reply containing exactly one supported Telegram reaction emoji is delivered
  as a reaction to the triggering message instead of as a separate message.
- Inbound reaction changes become ordinary durable agent turns, including
  reaction removals. Group reactions are accepted only for messages recorded
  as bot output.
- Mutating-tool approvals and generic choices use scoped, expiring inline
  buttons. The agent can also send Telegram documents, photos, voice files,
  and reactions through parent-owned gateway tools.
- Transient Telegram and agent failures use bounded backoff and durable retry
  metadata. `/retry` restores the latest dead-lettered turn.
- Voice messages are limited to 10 minutes and 20 MB. They are downloaded to a
  private temporary gateway file, transcribed through `codex app-server`, then
  deleted before the transcript enters the normal durable agent-session path.

## Management

Use `agent-telegram users list|add ID|remove ID` to manage the local allowlist;
restart the running gateway after changes. Use `agent-telegram stop` to stop
the background gateway. Redacted JSON logs and durable queue state live below
`~/.haskell-agent/gateways/telegram/`.
