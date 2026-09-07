-- | Distribution composition root. The public bridge has no bundled provider.
-- Private distributions replace this module at build time and link their
-- provider in the same GHC package set; no runtime plugin loading is involved.
module Agent.CLI.MacOS.BundledIntegrations (bundledIntegrationProvider) where

import Agent.Integration.API (IntegrationProvider, emptyIntegrationProvider)

bundledIntegrationProvider :: IntegrationProvider
bundledIntegrationProvider = emptyIntegrationProvider
