# agent-integration-api

The public, provider-neutral embedding boundary for integrations.

The standalone CLI has an empty local provider. Organization sessions use
the gateway's authenticated `/mcp/integrations` endpoint; they never acquire
the local provider, including when the remote service is unavailable.

An embedding distribution supplies an `IntegrationProvider` when creating
its native process runtime. The supervisor creates one local runtime lazily,
shares it across sessions, and closes it once on process shutdown. Providers
must close their subscriptions and cancel/join owned workers. The provider
receives a stable process-private scratch directory through its `ToolEnv`;
session preparation grants read access without changing that directory.

Concrete account models, OAuth flows, tools and integration registration do
not belong in this package. Generic administration uses opaque `RawJson` at
the host boundary, leaving typed decoding to the provider.

Native distributions can extend `legacyPackages.${system}` from the public
Nix flake, replacing only `Agent.CLI.MacOS.BundledIntegrations` at build time.
The replacement exports `bundledIntegrationProvider :: IntegrationProvider`.
It and its dependencies must be built inside that same Haskell package set;
runtime plugin loading and mixed GHC package databases are not supported.
