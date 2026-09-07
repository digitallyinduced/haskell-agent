-- | Provider-neutral embedding seam. Concrete integrations are supplied by
-- the distribution, never discovered or loaded from an untrusted plugin.
module Agent.Integration.API
    ( IntegrationError(..)
    , IntegrationEndpoint(..)
    , IntegrationRuntime(..)
    , IntegrationProvider
    , OrganizationIntegrationProvider
    , IntegrationAuthority(..)
    , IntegrationSupervisor
    , emptyIntegrationProvider
    , newIntegrationSupervisor
    , newIntegrationSupervisorWithOrganizationProvider
    , resetIntegrationSupervisor
    , acquireIntegrationRuntime
    , closeIntegrationSupervisor
    ) where

import Agent.Json (RawJson, rawJsonFromEncoding)
import Agent.Integration.Connection (IntegrationConnections)
import Agent.MCP (McpServerConfig, McpToolServer)
import Agent.Tools.Types (ToolEnv)
import Control.Concurrent.MVar (MVar, withMVar, newMVar)
import Control.Exception.Safe (mask, onException)
import qualified Data.Aeson as Aeson
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)

data IntegrationError
    = IntegrationInvalidInput !Text
    | IntegrationUnavailable !Text
    | IntegrationOperationFailed !Text
    deriving (Eq, Show)

-- | The provider transfers ownership of its runtime to the supervisor.
-- Cleanup must unsubscribe its MCP host and cancel/join its owned workers.
-- Returned callbacks must reject use after cleanup. Hosts retain the gateway
-- lease for the complete operation, not merely for acquisition.
data IntegrationEndpoint
    = NoIntegrationEndpoint
    | LocalIntegrationEndpoint !McpToolServer
    | RemoteIntegrationEndpoint !McpServerConfig
    | CombinedIntegrationEndpoint !McpServerConfig !McpToolServer

data IntegrationRuntime = IntegrationRuntime
    { integrationRuntimeEndpoint :: !IntegrationEndpoint
    , integrationRuntimeAdminDefinitions :: !RawJson
    , callIntegrationRuntimeAdmin
        :: !(Text -> RawJson -> IO (Either IntegrationError RawJson))
    , integrationRuntimeConnections :: !(Maybe IntegrationConnections)
    , closeIntegrationRuntime :: !(IO ())
    }

type IntegrationProvider = ToolEnv -> IO (Either Text IntegrationRuntime)

-- | Explicit distribution opt-in, separate from the ordinary local provider.
-- The factory must capture this exact authenticated gateway configuration;
-- it must never resolve credentials from mutable process-global state.
-- It may supply only integrations authorized for organization-local execution.
type OrganizationIntegrationProvider = McpServerConfig -> IntegrationProvider

data IntegrationAuthority
    = LocalIntegrationAuthority
    | OrganizationIntegrationAuthority !McpServerConfig
    deriving (Eq, Show)

data SupervisorState
    = Open !(Maybe (IntegrationAuthority, IntegrationRuntime))
    | Closed

data IntegrationSupervisor = IntegrationSupervisor
    { supervisorLock :: !(MVar ())
    , supervisorState :: !(IORef SupervisorState)
    , supervisorProvider :: !IntegrationProvider
    , supervisorOrganizationProvider :: !(Maybe OrganizationIntegrationProvider)
    , supervisorToolEnv :: !ToolEnv
    }

-- | Public distributions deliberately contain no local implementation.
emptyIntegrationProvider :: IntegrationProvider
emptyIntegrationProvider _ = pure (Right emptyRuntime)

emptyRuntime :: IntegrationRuntime
emptyRuntime = IntegrationRuntime
    { integrationRuntimeEndpoint = NoIntegrationEndpoint
    , integrationRuntimeAdminDefinitions =
        rawJsonFromEncoding (Aeson.toEncoding ([] :: [Aeson.Value]))
    , callIntegrationRuntimeAdmin = \_ _ ->
        pure (Left (IntegrationUnavailable
            "Local integrations are not available in this distribution."))
    , closeIntegrationRuntime = pure ()
    , integrationRuntimeConnections = Nothing
    }

newIntegrationSupervisor :: IntegrationProvider -> ToolEnv -> IO IntegrationSupervisor
newIntegrationSupervisor provider =
    newIntegrationSupervisorWithOrganizationProvider provider Nothing

newIntegrationSupervisorWithOrganizationProvider
    :: IntegrationProvider -> Maybe OrganizationIntegrationProvider
    -> ToolEnv -> IO IntegrationSupervisor
newIntegrationSupervisorWithOrganizationProvider
    supervisorProvider supervisorOrganizationProvider supervisorToolEnv = do
    supervisorLock <- newMVar ()
    supervisorState <- newIORef (Open Nothing)
    pure IntegrationSupervisor{..}

-- | Organization acquisition never evaluates the ordinary local provider.
-- Identity includes the entire configuration (including authentication), not
-- just the URL. Retire the previous owner before constructing its replacement.
-- The caller must hold its authenticated gateway lease while using the runtime.
acquireIntegrationRuntime
    :: IntegrationSupervisor -> IntegrationAuthority
    -> IO (Either Text IntegrationRuntime)
acquireIntegrationRuntime supervisor authority =
    withMVar supervisor.supervisorLock \_ -> mask \restore ->
        readIORef supervisor.supervisorState >>= \case
            Closed -> pure (Left "The integration runtime is already closed.")
            Open (Just (previous, runtime)) | previous == authority ->
                pure (Right runtime)
            Open previous -> do
                -- Never resurrect a retired runtime if cleanup or acquisition
                -- throws, including asynchronous cancellation.
                writeIORef supervisor.supervisorState (Open Nothing)
                closePrevious previous
                    `onException` writeIORef supervisor.supervisorState Closed
                restore acquire >>= \case
                    Left err -> pure (Left err)
                    Right runtime -> do
                        writeIORef supervisor.supervisorState
                            (Open (Just (authority, runtime)))
                        pure (Right runtime)
  where
    acquire = case authority of
        LocalIntegrationAuthority ->
            supervisor.supervisorProvider supervisor.supervisorToolEnv
        OrganizationIntegrationAuthority config ->
            case supervisor.supervisorOrganizationProvider of
                Nothing -> pure (Right (remoteRuntime config))
                Just provider ->
                    provider config supervisor.supervisorToolEnv >>= \case
                        Left err -> pure (Left err)
                        Right runtime -> case integrationRuntimeEndpoint runtime of
                            NoIntegrationEndpoint -> pure (Right runtime
                                { integrationRuntimeEndpoint = RemoteIntegrationEndpoint config })
                            LocalIntegrationEndpoint server -> pure (Right runtime
                                { integrationRuntimeEndpoint = CombinedIntegrationEndpoint config server })
                            _ -> do
                                closeIntegrationRuntime runtime
                                pure (Left "Organization-local providers must not supply remote endpoints.")

remoteRuntime :: McpServerConfig -> IntegrationRuntime
remoteRuntime config = emptyRuntime
    { integrationRuntimeEndpoint = RemoteIntegrationEndpoint config
    , callIntegrationRuntimeAdmin = \_ _ ->
        pure (Left (IntegrationUnavailable
            "Manage organization integrations through the gateway."))
    }

closePrevious :: Maybe (IntegrationAuthority, IntegrationRuntime) -> IO ()
closePrevious = maybe (pure ()) (closeIntegrationRuntime . snd)

-- | Invalidate owned work immediately on logout/credential replacement,
-- without acquiring any replacement provider. The host must first stop/join
-- callers using the old gateway lease and retire its MCP fleet.
resetIntegrationSupervisor :: IntegrationSupervisor -> IO ()
resetIntegrationSupervisor supervisor =
    withMVar supervisor.supervisorLock \_ -> mask \_ ->
        readIORef supervisor.supervisorState >>= \case
            Closed -> pure ()
            Open previous -> do
                writeIORef supervisor.supervisorState (Open Nothing)
                closePrevious previous
                    `onException` writeIORef supervisor.supervisorState Closed

closeIntegrationSupervisor :: IntegrationSupervisor -> IO ()
closeIntegrationSupervisor supervisor =
    withMVar supervisor.supervisorLock \_ -> mask \_ -> do
        previous <- readIORef supervisor.supervisorState
        writeIORef supervisor.supervisorState Closed
        case previous of
            Closed -> pure ()
            Open runtime -> closePrevious runtime
