-- | Application-owned resources shared by server and native session hosts.
module Agent.Runtime.Process
    ( NativeProcessRuntime(nativeProcessCore, nativeIntegrationSupervisor, nativeLocalIntegrationSupervisor)
    , newNativeProcessRuntime
    , newNativeProcessRuntimeWithIntegrations
    , newNativeProcessRuntimeWithOrganizationIntegrations
    , newNativeProcessRuntimeWithCredentialRuntime
    , closeNativeProcessRuntime
    , restartNativeMcpRuntime
    , nativeProcessIntegrationSupervisor
    , acquireNativeLocalIntegrationRuntime
    , nativeAgentProcessRuntime
    ) where

import Agent.Connectivity.NetworkPath (networkRecovery)
import Agent.Integration.API
import Agent.MCP qualified as MCP
import Agent.ResourceScope (allocateResource, closeResourceScope, newResourceScope)
import Agent.Runtime.Host (AgentProcessRuntime(..))
import Agent.Runtime.McpConnectionCredentials (CredentialRuntime, newCredentialRuntime)
import Agent.Runtime.McpConnectionRuntime
    ( mcpConnectionCredentials, observeMcpConnectionInfo, registerMcpConnectionRuntime )
import Agent.Runtime.NativeProcess qualified as NativeProcess
import Agent.Tools.Types (defaultToolEnv)
import Control.Exception.Safe (bracketOnError, finally)
import Data.Text (Text)
import System.OsPath (OsPath)

data NativeProcessRuntime = NativeProcessRuntime
    { nativeProcessCore :: !NativeProcess.NativeProcessRuntime
    , nativeIntegrationSupervisor :: !IntegrationSupervisor
    , nativeLocalIntegrationSupervisor :: !IntegrationSupervisor
    , nativeCloseProcess :: !(IO ())
    }

newNativeProcessRuntime :: OsPath -> IO NativeProcessRuntime
newNativeProcessRuntime = newNativeProcessRuntimeWithIntegrations emptyIntegrationProvider

newNativeProcessRuntimeWithIntegrations :: IntegrationProvider -> OsPath -> IO NativeProcessRuntime
newNativeProcessRuntimeWithIntegrations provider =
    newNativeProcessRuntimeWithOrganizationIntegrations provider Nothing

newNativeProcessRuntimeWithOrganizationIntegrations
    :: IntegrationProvider -> Maybe OrganizationIntegrationProvider -> OsPath -> IO NativeProcessRuntime
newNativeProcessRuntimeWithOrganizationIntegrations provider organizationProvider root = do
    credentials <- newCredentialRuntime Nothing
    newNativeProcessRuntimeWithCredentialRuntime credentials provider organizationProvider root

newNativeProcessRuntimeWithCredentialRuntime
    :: CredentialRuntime -> IntegrationProvider -> Maybe OrganizationIntegrationProvider
    -> OsPath -> IO NativeProcessRuntime
newNativeProcessRuntimeWithCredentialRuntime credentials provider organizationProvider root =
    bracketOnError newResourceScope closeResourceScope \scope -> do
        integrationToolEnv <- defaultToolEnv root
        (_, localIntegrations) <- allocateResource scope
            (newIntegrationSupervisor provider integrationToolEnv) closeIntegrationSupervisor
        -- Local account administration and direct turns borrow one owner.
        -- Organization changes must not retire an unrelated local OAuth flow.
        let borrowedLocalProvider _ =
                fmap (fmap (\runtime -> runtime { closeIntegrationRuntime = pure () }))
                    (acquireIntegrationRuntime localIntegrations LocalIntegrationAuthority)
        (_, integrations) <- allocateResource scope
            (newIntegrationSupervisorWithOrganizationProvider
                borrowedLocalProvider organizationProvider integrationToolEnv)
            closeIntegrationSupervisor
        (_, core) <- allocateResource scope
            (NativeProcess.newNativeProcessRuntimeWithMcpHooks
                MCP.defaultMcpHostHooks
                    { MCP.mcpHostCredentials = mcpConnectionCredentials credentials
                    , MCP.mcpHostServerInfo = observeMcpConnectionInfo
                    } root)
            NativeProcess.closeNativeProcessRuntime
        _ <- allocateResource scope
            (registerMcpConnectionRuntime (NativeProcess.restartNativeMcpRuntime core)) id
        pure NativeProcessRuntime
            { nativeProcessCore = core
            , nativeIntegrationSupervisor = integrations
            , nativeLocalIntegrationSupervisor = localIntegrations
            , nativeCloseProcess = closeResourceScope scope
            }

closeNativeProcessRuntime :: NativeProcessRuntime -> IO ()
closeNativeProcessRuntime runtime = runtime.nativeCloseProcess

restartNativeMcpRuntime :: NativeProcessRuntime -> IO ()
restartNativeMcpRuntime runtime =
    NativeProcess.restartNativeMcpRuntime runtime.nativeProcessCore
        `finally` resetIntegrationSupervisor runtime.nativeIntegrationSupervisor

nativeProcessIntegrationSupervisor :: NativeProcessRuntime -> IntegrationSupervisor
nativeProcessIntegrationSupervisor = (.nativeIntegrationSupervisor)

acquireNativeLocalIntegrationRuntime :: NativeProcessRuntime -> IO (Either Text IntegrationRuntime)
acquireNativeLocalIntegrationRuntime runtime =
    acquireIntegrationRuntime runtime.nativeLocalIntegrationSupervisor LocalIntegrationAuthority

nativeAgentProcessRuntime :: NativeProcessRuntime -> AgentProcessRuntime
nativeAgentProcessRuntime runtime = AgentProcessRuntime
    { processMcpSupervisor = runtime.nativeProcessCore.nativeMcpSupervisor
    , processIntegrationSupervisor = runtime.nativeIntegrationSupervisor
    , processSessionThreads = runtime.nativeProcessCore.nativeSessionThreads
    , processToolResourceArbiter = runtime.nativeProcessCore.nativeToolResourceArbiter
    , processStartCleanup = runtime.nativeProcessCore.nativeStartCleanup
    , processMcpElicitation = runtime.nativeProcessCore.nativeMcpElicitation
    , processMcpRoots = runtime.nativeProcessCore.nativeMcpRoots
    , processMcpSampling = runtime.nativeProcessCore.nativeMcpSampling
    , processNetworkRecovery = networkRecovery runtime.nativeProcessCore.nativeNetworkRecovery
    }
