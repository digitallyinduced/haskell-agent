# Telegram gateway

Create a bot with BotFather, find your numeric Telegram user ID, and run:

```console
agent-telegram setup --provider openai --cwd /path/to/project \
  --allowed-user 123456789
agent-telegram start
agent-telegram status
```

Setup reads the BotFather token without terminal echo, validates it against
Telegram, and stores it separately from the non-secret gateway configuration.
Never paste the bot token into an agent conversation.

## Users and groups

Only messages from allowlisted Telegram users are handled. Repeat
`--allowed-user` during setup, or manage the local allowlist later with
`agent-telegram users list|add ID|remove ID`. CLI allowlist edits require a
gateway restart.

In a group, an allowed member can grant access with `/allow` by name,
`@username`, or by replying to a message. `/users` lists the allowlist and
people the bot has seen; `/deny` removes someone. Mention the bot, address a
command to its username, or reply to one of its messages. Ambient group
traffic is ignored by default. Pass `--all-group-messages` during setup to let
the agent consider every group message from an allowed user. Disable BotFather
privacy mode and re-add the bot if Telegram must deliver ambient messages.

The bot stays in a group or channel only when it was added by an allowlisted
Telegram administrator of that chat. Anonymous-admin adds are accepted only
when an allowlisted user is already an administrator.

## Sessions and approvals

Each private chat, group, and forum topic maps to a persisted session under
`~/.haskell-agent`. `/new` starts a fresh session, `/session` shows its ID,
`/status` reports work, and `/retry` requeues the latest failed turn.

Mutating tools request approval through inline buttons by default.
`--deny-mutations` disables them and `--yolo` auto-approves them. Callbacks are
scoped to the originating conversation and allowlisted user.

Incoming updates and pending replies are persisted before processing. Work,
callback bindings, retries, delivery checkpoints, and dead letters resume
after restarts. Conversations are processed in order while separate chats run
concurrently through a bounded worker pool.

## Attachments and tools

The gateway accepts edited messages, reactions, photos, documents, audio,
video, video notes, animations, stickers, locations, contacts, venues, polls,
and dice. Images are sent to multimodal providers natively; other files use
Responses `input_file` content or a private local-path fallback.

The agent can send documents, photos, and voice files, react to messages, ask
inline-button questions, and manage users through gateway-scoped tools. Bot
credentials remain in the parent gateway process and are never inherited by
the agent child.

The built-in `telegram-agent` skill can guide setup. Ask the normal agent to
“set up a Telegram agent”.

## NixOS

Use the flake's reusable multi-instance service module instead of maintaining
the systemd and PostgreSQL runtime configuration by hand. See
[`nixos.md`](nixos.md).
