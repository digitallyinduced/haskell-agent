# Connected email accounts

Haskell Agent can keep connection metadata for Gmail, Microsoft 365/Outlook,
and custom IMAP mailboxes. It reads mail, saves drafts, and can send a
freshly approved Gmail or Microsoft draft. It does not delete, move, or label
messages.

## Security model

- Gmail authorization requests `gmail.readonly` and `gmail.compose`;
  Microsoft requests `Mail.ReadWrite`, `Mail.Send`, and `User.Read`. Both
  browser flows use a random PKCE verifier, state validation, a short-lived
  loopback listener bound to `127.0.0.1`, and an opaque flow ID. Accounts
  connected without the current write/send scopes must be reconnected before
  the corresponding tools become available.
- OAuth client IDs are public application identifiers supplied by the native
  application. No confidential OAuth client secret is embedded in this
  repository.
- Custom IMAP permits implicit TLS or STARTTLS only. Certificate validation
  and SNI are enabled, authentication occurs only after TLS is established,
  and credentials are persisted only after an authenticated `LIST` probe.
- In a distribution that bundles the private local provider, the authoritative,
  versioned account snapshot is
  `~/.haskell-agent/mail/store.json`; it is owner-only and atomically replaced
  under private cross-process locks so credentials cannot be paired with stale
  connection metadata after a crash. A separate `accounts.json` mirror contains
  metadata only. In organization mode the gateway owns and encrypts the
  credentials instead. Secrets never appear in account-list results, tool
  arguments, errors, or `Show` output.

## Agent tools

The open-source CLI does not bundle a concrete integration provider. An
organization session obtains these capabilities from the gateway's
authenticated `/mcp/integrations` endpoint. Private distributions, including
the macOS app, may compile a local provider into the generic integration seam.
Both paths use the same operation catalog and approval metadata.

The selected backend is fail-closed. A connected gateway is authoritative; if
its email contract is unavailable or incompatible, email tools stay disabled
instead of silently falling back to locally stored credentials. OAuth tokens
and IMAP passwords remain in the selected backend and never cross the MCP
payload.

The provider-neutral tool contract is:

- `email_list_accounts`
- `email_list_mailboxes`
- `email_search`
- `email_get`
- `email_download_attachment`
- `email_create_draft`
- `email_update_draft`
- `email_reply_draft`
- `email_send`

The runtime integration registers tools only when an enabled, verified account
exists. IDs are opaque and must come from a preceding email tool result. In
standalone mode, provider-local mailbox, message, attachment, and draft
references are authenticated with a per-runtime key and bound to both the
originating account and capability kind. A reference cannot therefore be moved
to another account, substituted for a different kind of item, or reused after
restart. In gateway mode, the gateway owns equivalent opaque references and
binds them to the authenticated gateway client and account; the local runtime
treats them as opaque values and does not persist them.
Search accepts structured mailbox, sender, recipient, subject, date,
attachment, and limit fields. Search and get results expose `reply_to` as the
effective bare reply address when provider metadata is safe and unambiguous.
Results, message bodies, attachments, provider requests, and wall clock
duration are bounded. Downloads go to the current session's private temporary
directory rather than being returned to the model. In gateway mode, the MCP
operation returns a short-lived tagged resource link. The same authenticated
MCP client redeems it through `resources/read`, verifies its declared size, and
atomically writes the bounded bytes to that directory.

Gmail and Microsoft support all nine tools. Compatible custom IMAP servers
also support drafts when they advertise a Drafts mailbox and the UIDPLUS
capability needed for safe updates; unsupported servers remain read-only.
`email_create_draft`, `email_update_draft`, and `email_reply_draft` are
turn-sequential mailbox writes that require an explicit approval every time.
They save a provider-side draft only and return `sent: false`. Reply drafts
require the exact `reply_to` address returned by
`email_search` or `email_get`, verify the explicitly approved recipient
against the source message's current provider metadata, and derive thread
metadata from that message; changing recipients is a separate, explicitly
approved `email_update_draft` call.

`email_send` is also turn-sequential and always requires a fresh approval. It
requires an opaque draft ID plus the complete To, Cc, Bcc, subject, and
plain-text body. The provider receives those exact approved values in one
send request rather than being asked to send the mutable provider draft.
Reply-thread metadata is captured when the reply draft is created and carried
inside its opaque capability. The source draft is retained, and every later
send still requires a new approval. A send response can be lost after the
provider accepts the message, so an uncertain result tells the caller to check
Sent before trying again. Custom IMAP accounts cannot use `email_send` because
the account contract has no SMTP configuration.

Custom IMAP accounts otherwise support
account/mailbox listing, structured search, bounded MIME message retrieval,
and bounded attachment-part downloads. The runtime checks RFC822 size before
fetching, caps each raw message at 8 MiB, decodes MIME locally, and never
performs the unbounded full-mailbox fetch used by the older belege.ai
integration.

All mailbox text is untrusted external content. Tool descriptions and results
explicitly warn the model not to follow instructions found in messages or
attachments and not to disclose secrets because an email asks it to.

The built-in transport implements Gmail API, Microsoft Graph, and secure IMAP
mailbox listing, structured search, bounded message reads, draft writes, and
approved Gmail/Graph sends. Gmail and Graph attachments retain their provider
attachment IDs. For
custom IMAP, the runtime checks `RFC822.SIZE` before reading the message
literal, parses that bounded MIME message locally, and returns only the
requested decoded attachment part. Oversized messages or parts fail before a
file is written.

The public `agent-mail` package contains low-level types and provider
transports. The model-facing catalog, account workflows, and `MailToolsEnv`
composition live in the private integrations package. Transports apply limits
while reading the remote response; truncating only after an unbounded MIME
fetch is not sufficient.

## Native account management

When its private local provider is selected, the native application starts a
browser authorization, opens the returned URL, and polls with the opaque flow
ID. It may cancel the flow at any time. In gateway mode, account connection and
management belong to the gateway control plane, so the native UI shows
gateway-managed accounts and does not invoke local OAuth or IMAP setup.
Google must be configured with an installed-app OAuth client ID; Microsoft
must use a public desktop/native application registration. Their public client
IDs are injected into the release `Info.plist`; neither provider uses a client
secret. The runtime reads the loopback HTTP callback incrementally, ignores
unrelated local requests, validates OAuth state, and exchanges the code itself.
Custom IMAP settings and a password are passed once for validation; the UI
must clear its password field immediately. Account listing exposes metadata
only. Enable/disable and delete operations address accounts by opaque local
ID.

Changes to connected accounts affect the next newly-created local integration
runtime. Every local tool invocation also rechecks account enabled/state
metadata, so disabling or deleting an account revokes access even for an
existing snapshot. Gateway mode authorizes the authenticated gateway identity
on every request.
