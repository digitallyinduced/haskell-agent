-- | Provider-neutral authenticated gateway authority. Organization-local
-- extensions require a separate explicit distribution opt-in.
module Agent.Runtime.IntegrationGateway
    ( gatewayIntegrationAuthority
    , sessionIntegrationAuthority
    , prepareSessionIntegrationRuntime
    , gatewayIntegrationMcpConfig
    , availableIntegrationServerName
    , integrationEndpointServers
    ) where

import Agent.Runtime.GatewayClient (GatewayCredential(..))
import Agent.Integration.API
    ( IntegrationAuthority(..), IntegrationEndpoint(..), IntegrationRuntime
    , IntegrationSupervisor, acquireIntegrationRuntime )
import Agent.MCP (McpServerConfig(..), McpProtocolPreference(..), McpToolServer)
import qualified Data.Set as Set
import qualified Data.Text as Text

gatewayIntegrationAuthority :: Maybe GatewayCredential -> IntegrationAuthority
gatewayIntegrationAuthority =
    maybe LocalIntegrationAuthority
        (OrganizationIntegrationAuthority . gatewayIntegrationMcpConfig)

-- | The integration authority of a session, or none when the host withholds
-- the connected gateway's organization integrations. A connected gateway stays
-- authoritative even then: the session does not fall back to local accounts.
sessionIntegrationAuthority :: Bool -> Maybe GatewayCredential -> Maybe IntegrationAuthority
sessionIntegrationAuthority gatewayIntegrations connected = case connected of
    Just _ | not gatewayIntegrations -> Nothing
    _ -> Just (gatewayIntegrationAuthority connected)

-- | Process-owned integration acquisition is independent of a session's cwd.
-- Startup may prepare it while Git is fetching, then borrow the same result
-- after the workspace has been finalized. Warnings are presented by the owner.
prepareSessionIntegrationRuntime
    :: Bool
    -> Maybe GatewayCredential
    -> IntegrationSupervisor
    -> IO (Either Text.Text (Maybe IntegrationRuntime))
prepareSessionIntegrationRuntime enabled connected supervisor =
    case sessionIntegrationAuthority enabled connected of
        Nothing -> pure (Right Nothing)
        Just authority -> fmap (fmap Just) (acquireIntegrationRuntime supervisor authority)

gatewayIntegrationMcpConfig :: GatewayCredential -> McpServerConfig
gatewayIntegrationMcpConfig credential = McpServerConfig
    { mcpServerName = "integrations"
    , mcpServerUrl = Just
        (Text.dropWhileEnd (== '/') (Text.strip credential.gatewayBaseUrl)
            <> "/mcp/integrations")
    , mcpServerCommand = ""
    , mcpServerArgs = []
    , mcpServerCwd = Nothing
    , mcpServerEnv =
        [("MCP_ACCESS_TOKEN", Text.unpack credential.gatewayAccessToken)]
    , mcpServerStartupTimeoutSeconds = 15
    , mcpServerRequestTimeoutSeconds = 60
    , mcpServerProtocol = McpProtocolLegacy
    , mcpServerRootsEnabled = False
    , mcpServerSamplingEnabled = False
    , mcpServerLogLevel = Nothing
    , mcpServerExcludedTools = []
    , mcpServerConnection = Nothing
    }

availableIntegrationServerName :: [Text.Text] -> Text.Text
availableIntegrationServerName configuredNames =
    choose 1
  where
    configured = Set.fromList configuredNames
    choose index
        | Set.member candidate configured = choose (index + 1)
        | otherwise = candidate
      where
        candidate
            | index == 1 = "integrations"
            | otherwise = "integrations-" <> Text.pack (show index)

-- | Explicit overlays own the canonical namespace. Remote tools retain a
-- separate namespace, with reserved names denied even if local tools disappear.
-- Plain combined endpoints deliberately retain their existing remote-first names.
integrationEndpointServers
    :: [Text.Text] -> IntegrationEndpoint
    -> ([McpServerConfig], [(Text.Text, McpToolServer)])
integrationEndpointServers configured endpoint = case endpoint of
    NoIntegrationEndpoint -> ([], [])
    LocalIntegrationEndpoint server -> ([], [(primary, server)])
    LocalOverlayIntegrationEndpoint _ server -> ([], [(primary, server)])
    RemoteIntegrationEndpoint config -> ([named primary config], [])
    CombinedIntegrationEndpoint config server ->
        ([named primary config], [(secondary, server)])
    CombinedOverlayIntegrationEndpoint config names server ->
        ([named secondary config
            { mcpServerExcludedTools = config.mcpServerExcludedTools <> names }],
            [(primary, server)])
  where
    primary = availableIntegrationServerName configured
    secondary = availableIntegrationServerName (primary : configured)
    named name config = config { mcpServerName = name }
