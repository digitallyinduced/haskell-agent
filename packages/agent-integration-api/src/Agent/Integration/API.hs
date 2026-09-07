-- | Provider-neutral embedding seam. Concrete integrations are supplied by
-- the distribution, never discovered or loaded from an untrusted plugin.
module Agent.Integration.API
    ( IntegrationError(..)
    , IntegrationEndpoint(..)
    , IntegrationRuntime(..)
    , IntegrationProvider
    , IntegrationAuthority(..)
    , IntegrationSupervisor
    , emptyIntegrationProvider
    , newIntegrationSupervisor
    , acquireIntegrationRuntime
    , closeIntegrationSupervisor
    ) where

import Agent.Json (RawJson, rawJsonFromEncoding)
import Agent.MCP (McpServerConfig, McpToolServer)
import Agent.Tools.Types (ToolEnv)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import qualified Data.Aeson as Aeson
import Data.Text (Text)

data IntegrationError
    = IntegrationInvalidInput !Text
    | IntegrationUnavailable !Text
    | IntegrationOperationFailed !Text
    deriving (Eq, Show)

-- | The provider transfers ownership of its runtime to the supervisor.
-- Cleanup must unsubscribe its MCP host and cancel/join its owned workers.
data IntegrationEndpoint
    = NoIntegrationEndpoint
    | LocalIntegrationEndpoint !McpToolServer
    | RemoteIntegrationEndpoint !McpServerConfig

data IntegrationRuntime = IntegrationRuntime
    { integrationRuntimeEndpoint :: !IntegrationEndpoint
    , integrationRuntimeAdminDefinitions :: !RawJson
    , callIntegrationRuntimeAdmin
        :: !(Text -> RawJson -> IO (Either IntegrationError RawJson))
    , closeIntegrationRuntime :: !(IO ())
    }

type IntegrationProvider = ToolEnv -> IO (Either Text IntegrationRuntime)

data IntegrationAuthority
    = LocalIntegrationAuthority
    | OrganizationIntegrationAuthority !McpServerConfig
    deriving (Eq, Show)

data SupervisorState = Open !(Maybe IntegrationRuntime) | Closed

data IntegrationSupervisor = IntegrationSupervisor
    { supervisorState :: !(MVar SupervisorState)
    , supervisorProvider :: !IntegrationProvider
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
    }

newIntegrationSupervisor :: IntegrationProvider -> ToolEnv -> IO IntegrationSupervisor
newIntegrationSupervisor supervisorProvider supervisorToolEnv = do
    supervisorState <- newMVar (Open Nothing)
    pure IntegrationSupervisor{..}

-- | Organization acquisition never evaluates the local provider, even when
-- the remote endpoint is unavailable. The ordinary MCP supervisor owns that
-- connection and its credential-scoped identity.
acquireIntegrationRuntime
    :: IntegrationSupervisor -> IntegrationAuthority
    -> IO (Either Text IntegrationRuntime)
acquireIntegrationRuntime supervisor authority =
    modifyMVar supervisor.supervisorState \case
        Closed -> pure (Closed, Left "The integration runtime is already closed.")
        state@(Open local) -> case authority of
            OrganizationIntegrationAuthority config ->
                pure (state, Right emptyRuntime
                    { integrationRuntimeEndpoint = RemoteIntegrationEndpoint config
                    , callIntegrationRuntimeAdmin = \_ _ ->
                        pure (Left (IntegrationUnavailable
                            "Manage organization integrations through the gateway."))
                    })
            LocalIntegrationAuthority -> case local of
                Just runtime -> pure (state, Right runtime)
                Nothing ->
                    supervisor.supervisorProvider supervisor.supervisorToolEnv >>= \case
                        Left err -> pure (state, Left err)
                        Right runtime -> pure (Open (Just runtime), Right runtime)

closeIntegrationSupervisor :: IntegrationSupervisor -> IO ()
closeIntegrationSupervisor supervisor = do
    previous <- modifyMVar supervisor.supervisorState \state -> pure (Closed, state)
    case previous of
        Closed -> pure ()
        Open local -> maybe (pure ()) closeIntegrationRuntime local
