-- | The public CLI knows only the authenticated aggregate MCP endpoint.
module Agent.CLI.IntegrationGateway
    ( gatewayIntegrationAuthority
    , gatewayIntegrationMcpConfig
    ) where

import Agent.CLI.GatewayClient (GatewayCredential(..))
import Agent.Integration.API (IntegrationAuthority(..))
import Agent.MCP (McpServerConfig(..), McpProtocolPreference(..))
import qualified Data.Text as Text

gatewayIntegrationAuthority :: Maybe GatewayCredential -> IntegrationAuthority
gatewayIntegrationAuthority =
    maybe LocalIntegrationAuthority
        (OrganizationIntegrationAuthority . gatewayIntegrationMcpConfig)

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
    }
