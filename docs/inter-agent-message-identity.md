# Inter-agent message identity

New assignments, queued messages, follow-ups, and scheduled wake-ups receive a
local `amsg_` UUIDv7 when constructed. Child completion messages derive their
identity from the work message with a `:completion` suffix. Retrying
a pending batch or moving unacknowledged steering back to a mailbox preserves
the original message and its identifier. Separate messages retain separate
identifiers even when their text is identical.

Metadata records the originating tool-call ID and registry root-turn token
when available. The root-turn token is scoped to its registry; it is not a
provider response or model-turn identifier. Durable host delivery hooks receive
the metadata alongside the trusted tool call.

Canonical `agent_message` items store this information in
`haskell_agent_message_metadata`, including through storage conversion and full
history forks. Older items load with absent metadata. The Responses item `id`
remains reserved for provider identity. Native request and compaction adapters
remove the local metadata before serialization to a provider.

Restoring a stored transcript keeps its original metadata. Recovery from only
a prompt string reconstructs a new message and allocates a new identifier;
this change does not add a durable inbox for work that has not been checkpointed.

Message identity does not acknowledge delivery. Queue admission, provider
acceptance, and durable completion retain their existing separate contracts.
Untargeted `wait_agent` observes already queued root or child mail, including
host steering, and returns without consuming it. Acknowledged mail no longer
wakes subsequent waits.
Regression tests cover retry identity, canonical and storage round trips,
mail queued before `wait_agent` starts, acknowledgement, and wire projection.
