-- | Gateway-provided MCP endpoints and their machine-wide managed catalog entries.
module Agent.Runtime.Gateway.McpDistribution
    ( GatewayMcpServer(..)
    , gatewayMcpConnectionPrefix
    , gatewayMcpServerDecoder
    , checkGatewayMcpServersInstallable
    , installGatewayMcpServers
    , reconcileGatewayMcpServers
    , removeGatewayMcpServers
    , validateGatewayMcpServers
    ) where

import Agent.Accounts.Gateway.Origin
    ( parseGatewayOrigin, parseGatewayResourceOrigin, validateBaseUrl, whenEither )
import Agent.Json.Decode qualified as Hermes
import Agent.MCP (McpProtocolPreference(..))
import Agent.Runtime.Config
    ( HarnessConfig(..), McpServerConfig(..), modifyHarnessConfig
    , withHarnessConfigSnapshot
    )
import Data.Char (isAsciiLower, isDigit)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory.OsPath (getHomeDirectory)

data GatewayMcpServer = GatewayMcpServer
    { gatewayMcpName :: !Text
    , gatewayMcpUrl :: !Text
    , gatewayMcpTransport :: !Text
    , gatewayMcpAuthentication :: !Text
    }
    deriving (Eq, Show)

gatewayMcpServerDecoder :: Hermes.Decoder GatewayMcpServer
gatewayMcpServerDecoder = Hermes.object $
    GatewayMcpServer
        <$> Hermes.atKey "name" Hermes.text
        <*> Hermes.atKey "url" Hermes.text
        <*> Hermes.atKey "transport" Hermes.text
        <*> Hermes.atKey "authentication" Hermes.text

gatewayMcpConnectionPrefix :: Text
gatewayMcpConnectionPrefix = "gateway-distributed-"

validateGatewayMcpServers :: Text -> [GatewayMcpServer] -> Either Text [GatewayMcpServer]
validateGatewayMcpServers rawBaseUrl servers = do
    baseUrl <- validateBaseUrl rawBaseUrl
    gatewayOrigin <- parseGatewayOrigin "Gateway URL is invalid." baseUrl
    validated <- traverse (validateOne gatewayOrigin) servers
    whenEither
        (length names /= Map.size (Map.fromList [(name, ()) | name <- names]))
        "The gateway returned duplicate MCP server names."
    pure validated
  where
    names = map (Text.strip . (.gatewayMcpName)) servers
    validateOne gatewayOrigin server = do
        let name = Text.strip server.gatewayMcpName
        whenEither
            ( Text.null name || Text.length name > 80
                || not (Text.all validNameCharacter name) )
            "The gateway returned an invalid MCP server name."
        whenEither (server.gatewayMcpTransport /= "streamable-http")
            "The gateway returned an unsupported MCP transport."
        whenEither (server.gatewayMcpAuthentication /= "connection-access-token")
            "The gateway returned an unsupported MCP authentication method."
        endpointOrigin <- parseGatewayResourceOrigin
            "The gateway returned an invalid MCP server URL." server.gatewayMcpUrl
        whenEither (endpointOrigin /= gatewayOrigin)
            "The gateway returned an MCP server URL for a different origin."
        pure server
            { gatewayMcpName = name
            , gatewayMcpUrl = Text.strip server.gatewayMcpUrl
            }
    validNameCharacter character =
        isAsciiLower character || isDigit character || character == '-'

installGatewayMcpServers :: [GatewayMcpServer] -> IO (Either Text ())
installGatewayMcpServers servers = do
    home <- getHomeDirectory
    fmap (fmap (const ())) $ modifyHarnessConfig home \_ config ->
        (,()) <$> reconcileGatewayMcpServers servers config

-- | Reject deterministic catalog conflicts before rotating the credential.
checkGatewayMcpServersInstallable :: [GatewayMcpServer] -> IO (Either Text ())
checkGatewayMcpServersInstallable servers = do
    home <- getHomeDirectory
    result <- withHarnessConfigSnapshot home \_ config ->
        pure (() <$ reconcileGatewayMcpServers servers config)
    pure (result >>= id)

reconcileGatewayMcpServers
    :: [GatewayMcpServer]
    -> HarnessConfig
    -> Either Text HarnessConfig
reconcileGatewayMcpServers servers config = do
    let retained = Map.filter (not . isGatewayManaged) config.configMcpServers
        collisions = filter (`Map.member` retained) (map (.gatewayMcpName) servers)
    whenEither (not (null collisions))
        ("Cannot install gateway MCP server because the name is already configured: "
            <> Text.intercalate ", " collisions)
    pure config
        { configMcpServers = foldr insertServer retained servers }
  where
    insertServer server = Map.insert server.gatewayMcpName (managedConfig server)

removeGatewayMcpServers :: IO (Either Text ())
removeGatewayMcpServers = do
    home <- getHomeDirectory
    fmap (fmap (const ())) $ modifyHarnessConfig home \_ config ->
        Right
            ( config { configMcpServers = Map.filter
                (not . isGatewayManaged) config.configMcpServers }
            , ()
            )

isGatewayManaged :: McpServerConfig -> Bool
isGatewayManaged server = maybe False
    (gatewayMcpConnectionPrefix `Text.isPrefixOf`) server.mcpConnectionId

managedConfig :: GatewayMcpServer -> McpServerConfig
managedConfig server = McpServerConfig
    { mcpEnabled = True
    , mcpUrl = Just server.gatewayMcpUrl
    , mcpConnectionId = Just (gatewayMcpConnectionPrefix <> server.gatewayMcpName)
    , mcpConnectionCredentials = Just True
    , mcpConnectionGeneration = Nothing
    , mcpDisplayName = Just "Gateway managed"
    , mcpCommand = ""
    , mcpArgs = []
    , mcpCwd = Nothing
    , mcpEnv = Map.empty
    , mcpStartupTimeoutSeconds = 30
    , mcpRequestTimeoutSeconds = 60
    , mcpOAuth = Nothing
    , mcpProtocol = McpProtocolAuto
    , mcpRoots = False
    , mcpSampling = False
    , mcpLogLevel = Nothing
    }
