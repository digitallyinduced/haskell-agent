-- | Public API for process-owned, typed, in-memory integrations.
module Agent.Integrations
    ( IntegrationAuthority(..)
    , IntegrationRuntime
    , IntegrationSupervisor
    , newIntegrationSupervisor
    , acquireIntegrationRuntime
    , prepareIntegrationSupervisorForSession
    , closeIntegrationSupervisor
    , integrationRuntimeMcpServer
    , integrationRuntimeAdminDefinitions
    , callIntegrationRuntimeAdmin
    , IntegrationAdminDefinition(..)
    , IntegrationError(..)
    ) where

import Agent.Integrations.Admin (IntegrationAdminDefinition(..))
import Agent.Integrations.Runtime
import Agent.Integrations.Types (IntegrationError(..))
