# Connected email accounts

Haskell Agent can keep connection metadata for Gmail, Microsoft 365/Outlook,
and custom IMAP mailboxes. It reads mail and can save drafts, but it never
sends email. The runtime has no send tool or send endpoint, and does not
delete, move, or label messages.

## Security model

- Gmail authorization requests `gmail.readonly` and `gmail.compose`;
  Microsoft requests `Mail.ReadWrite` and `User.Read`, explicitly not
  `Mail.Send`. Gmail's `gmail.compose` consent scope technically includes the
  ability to send, so the product guarantee is enforced by the trusted
  runtime: it exposes and implements only draft operations, never a send
  operation. Both browser flows use a random PKCE verifier, state validation,
  a short-lived loopback listener bound to `127.0.0.1`, and an opaque flow ID.
  Accounts connected with the earlier read-only scopes must be reconnected
  once before draft tools become available.
- OAuth client IDs are public application identifiers supplied by the native
  application. No confidential OAuth client secret is embedded in this
  repository.
- Custom IMAP permits implicit TLS or STARTTLS only. Certificate validation
  and SNI are enabled, authentication occurs only after TLS is established,
  and credentials are persisted only after an authenticated `LIST` probe.
- The authoritative, versioned account snapshot is
  `~/.haskell-agent/mail/store.json`; it is owner-only and atomically replaced
  under private cross-process locks so credentials cannot be paired with stale
  connection metadata after a crash. A separate `accounts.json` mirror contains
  metadata only. Secrets never appear in account-list results, tool arguments,
  errors, or `Show` output.

## Agent tools

These capabilities are registered as first-party tools, rather than through a
user-configured MCP subprocess. This keeps OAuth tokens and IMAP passwords out
of MCP configuration, environment variables, and protocol payloads while still
giving every agent backend the same provider-neutral function surface. The
normal MCP collision check reserves these names.

The provider-neutral tool contract is:

- `email_list_accounts`
- `email_list_mailboxes`
- `email_search`
- `email_get`
- `email_download_attachment`
- `email_create_draft`
- `email_update_draft`
- `email_reply_draft`

The runtime integration registers tools only when an enabled, verified
account exists. IDs are opaque and must come from a preceding email tool
result. Provider-local mailbox, message, attachment, and draft references are
authenticated with a per-runtime key and bound to both the originating
account and capability kind. A reference cannot therefore be moved to another
account, substituted for a different kind of item, or reused after restart.
Search accepts structured mailbox, sender, recipient, subject, date,
attachment, and limit fields. Search and get results expose `reply_to` as the
effective bare reply address when provider metadata is safe and unambiguous.
Results, message bodies, attachments, provider requests, and wall clock
duration are bounded. Downloads go to the current session's private temporary
directory rather than being returned to the model.

Gmail and Microsoft support all eight tools. Compatible custom IMAP servers
also support drafts when they advertise a Drafts mailbox and the UIDPLUS
capability needed for safe updates; unsupported servers remain read-only.
`email_create_draft`, `email_update_draft`, and `email_reply_draft` are
turn-sequential mailbox writes that require an explicit approval every time.
They save a provider-side draft only, return `sent: false`, and cannot send
email. Reply drafts require the exact `reply_to` address returned by
`email_search` or `email_get`, verify the explicitly approved recipient
against the source message's current provider metadata, and derive thread
metadata from that message; changing recipients is a separate, explicitly
approved `email_update_draft` call. Custom IMAP accounts otherwise support
account/mailbox listing, structured search, bounded MIME message retrieval,
and bounded attachment-part downloads. The runtime checks RFC822 size before
fetching, caps each raw message at 8 MiB, decodes MIME locally, and never
performs the unbounded full-mailbox fetch used by the older belege.ai
integration.

All mailbox text is untrusted external content. Tool descriptions and results
explicitly warn the model not to follow instructions found in messages or
attachments and not to disclose secrets because an email asks it to.

The built-in transport implements Gmail API, Microsoft Graph, and secure IMAP
mailbox listing, structured search, bounded message reads, and draft-only
writes. Gmail and Graph attachments retain their provider attachment IDs. For
custom IMAP, the runtime checks `RFC822.SIZE` before reading the message
literal, parses that bounded MIME message locally, and returns only the
requested decoded attachment part. Oversized messages or parts fail before a
file is written.

The Haskell `MailToolsEnv` record remains the boundary between this public,
model-facing contract and provider transports. Transports apply limits while
reading the remote response; truncating only after an unbounded MIME fetch is
not sufficient.

## Native account management

The native application starts a browser authorization, opens the returned
URL, and polls with the opaque flow ID. It may cancel the flow at any time.
Google must be configured with an installed-app OAuth client ID; Microsoft
must use a public desktop/native application registration. Their public client
IDs are injected into the release `Info.plist`; neither provider uses a client
secret. The runtime reads the loopback HTTP callback incrementally, ignores
unrelated local requests, validates OAuth state, and exchanges the code itself.
Custom IMAP settings and a password are passed once for validation; the UI
must clear its password field immediately. Account listing exposes metadata
only. Enable/disable and delete operations address accounts by opaque local
ID.

Changes to connected accounts affect the next newly-created agent runtime.
The native application creates that tool set for each new run; an already
running turn keeps the snapshot with which it started. Every tool invocation
also rechecks account enabled/state metadata, so disabling or deleting an
account revokes access even for an existing snapshot.
