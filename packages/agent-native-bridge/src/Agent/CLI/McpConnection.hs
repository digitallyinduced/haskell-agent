-- | Independent remote MCP connections in the shared machine catalog.
-- Labels and endpoints are presentation/configuration, never credential keys.
module Agent.CLI.McpConnection
    ( McpConnection(..)
    , listMcpConnections
    , createMcpConnection
    , renameMcpConnection
    , setMcpConnectionEnabled
    , removeMcpConnection
    , removeMcpConnectionWith
    , readMcpConnectionConfig
    , connectionServerName
    , McpConnectionAuthorizationHost(..)
    , McpConnectionProbe(..)
    , authorizeMcpConnectionWith
    , authorizeMcpConnection
    , probeMcpConnection
    ) where

import Agent.CLI.Config (HarnessConfig(..), McpServerConfig(..), withHarnessConfigSnapshot, mcpUsesConnectionCredentials)
import Agent.CLI.McpAdmin
import Agent.CLI.McpConnectionCredentials (loadMcpConnectionRecord, saveMcpConnectionRecord, deleteMcpConnectionRecord)
import Agent.CLI.McpConnectionRuntime (invalidateMcpConnectionRuntimes, observeMcpConnectionInfo)
import Agent.CLI.McpOAuth (McpOAuthHost(..), authorizeMcpWith, defaultLoginOptions)
import Agent.MCP (McpProtocolPreference(..))
import qualified Agent.MCP as MCP
import Agent.MCP.OAuth (OAuthTokenFile(..), OAuthTokenFileExtra)
import qualified Agent.MCP.OAuth as OAuth
import Control.Exception.Safe (bracket, tryAny)
import Control.Monad (when)
import Data.Char (isControl)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Data.Word (Word64)
import Network.HTTP.Client (closeManager)
import Network.HTTP.Client.TLS (newTlsManager)
import System.OsPath (OsPath)

data McpConnection = McpConnection
    { connectionId :: !Text
    , connectionDisplayName :: !Text
    , connectionUrl :: !Text
    , connectionEnabled :: !Bool
    , connectionGeneration :: !(Maybe Text)
    }
    deriving (Eq, Show)

authorizeMcpConnection
    :: OsPath -> Word64 -> Text -> (Text -> IO (Either Text ()))
    -> IO (Either McpAdminError (McpAdminSnapshot McpConnection))
authorizeMcpConnection = authorizeMcpConnectionWith McpConnectionAuthorizationHost
    { connectionLoadCredential = loadMcpConnectionRecord
    , connectionSaveCredential = \identifier (record, extra) ->
        saveMcpConnectionRecord identifier record extra
    , connectionProbe = probeMcpConnection
    }

data McpConnectionProbe = McpConnectionReady | McpConnectionAuthorizationRequired
    deriving (Eq, Show)

-- | Perform only protocol initialization and tool discovery, never tool calls.
-- Candidate credentials remain memory-only until verification has succeeded.
probeMcpConnection
    :: McpServerConfig -> Maybe (OAuthTokenFile, OAuthTokenFileExtra)
    -> IO (Either Text McpConnectionProbe)
probeMcpConnection server credential = do
    result <- tryAny do
        challenge <- case (server.mcpUrl, credential) of
            (Just url, Nothing) ->
                bracket newTlsManager closeManager \manager ->
                    OAuth.probeAuthorizationChallenge manager url
            _ -> pure (Right (OAuth.AuthorizationProbe 200 Nothing))
        case challenge of
            Left _ -> pure (Left "MCP server could not be reached")
            Right response | response.probeStatus == 401 ->
                pure (Right McpConnectionAuthorizationRequired)
            Right _ -> bracket
                (MCP.startMcpFleetWithProgressHooks hooks (const (pure ())) [runtimeConfig])
                MCP.closeMcpFleet \fleet -> do
                    statuses <- MCP.mcpFleetStatuses fleet
                    pure case statuses of
                        [status] | status.mcpStatusState == MCP.McpReady ->
                            Right McpConnectionReady
                        _ -> Left "MCP initialization or tool discovery failed"
    pure case result of
        Left _ -> Left "MCP connection verification failed"
        Right value -> value
  where
    hooks = MCP.defaultMcpHostHooks
        { MCP.mcpHostServerInfo = observeMcpConnectionInfo
        , MCP.mcpHostCredentials = const $ pure $ Just MCP.McpCredentialProvider
            { MCP.mcpCredentialAccessToken =
                pure (Right ((.tokenAccessToken) . fst <$> credential))
            , MCP.mcpCredentialRefreshAccessToken =
                pure (Left "MCP authorization is required")
            }
        }
    runtimeConfig = MCP.McpServerConfig
        { MCP.mcpServerName = connectionServerName (fromMaybe "probe" server.mcpConnectionId)
        , MCP.mcpServerConnection = (\identifier -> MCP.McpConnectionIdentity identifier server.mcpConnectionGeneration Nothing) <$> server.mcpConnectionId
        , MCP.mcpServerUrl = server.mcpUrl
        , MCP.mcpServerCommand = ""
        , MCP.mcpServerArgs = []
        , MCP.mcpServerCwd = Nothing
        , MCP.mcpServerEnv = []
        , MCP.mcpServerStartupTimeoutSeconds = server.mcpStartupTimeoutSeconds
        , MCP.mcpServerRequestTimeoutSeconds = server.mcpRequestTimeoutSeconds
        , MCP.mcpServerProtocol = server.mcpProtocol
        , MCP.mcpServerRootsEnabled = False
        , MCP.mcpServerSamplingEnabled = False
        , MCP.mcpServerLogLevel = Nothing
        , MCP.mcpServerExcludedTools = []
        }

-- | Native hosts supply a protected per-ID store and a read-only MCP handshake.
-- No serialized credential representation crosses the host boundary.
data McpConnectionAuthorizationHost = McpConnectionAuthorizationHost
    { connectionLoadCredential
        :: Text -> IO (Either Text (Maybe (OAuthTokenFile, OAuthTokenFileExtra)))
    , connectionSaveCredential
        :: Text -> (OAuthTokenFile, OAuthTokenFileExtra) -> IO (Either Text ())
    , connectionProbe
        :: McpServerConfig
        -> Maybe (OAuthTokenFile, OAuthTokenFileExtra)
        -> IO (Either Text McpConnectionProbe)
    }

-- | Browser authorization and network verification run outside the catalog
-- lock. The final credential write is linearized with the original revision
-- check, so removal/disable/edit during authorization cannot install tokens.
authorizeMcpConnectionWith
    :: McpConnectionAuthorizationHost -> OsPath -> Word64 -> Text
    -> (Text -> IO (Either Text ()))
    -> IO (Either McpAdminError (McpAdminSnapshot McpConnection))
authorizeMcpConnectionWith host home expected identifier openBrowser = do
    generation <- UUID.toText <$> UUID.nextRandom
    started <- invalidateAfter $ changeConnection home expected identifier \server -> do
        when (not (mcpUsesConnectionCredentials server)) $
            Left (McpAdminInvalid "This connection uses CLI-configured credentials. Use agent-cli mcp login to update its authorization.")
        when (not server.mcpEnabled) $
            Left (McpAdminInvalid "Enable the MCP connection before authorizing")
        pure server { mcpConnectionGeneration = Just generation }
    case started of
        Left err -> pure (Left err)
        Right snapshot -> authorize generation snapshot.mcpAdminRevision
  where
   authorize generation revision = do
    loaded <- readMcpConnectionConfig home revision identifier
    case loaded of
        Left err -> pure (Left err)
        Right snapshot
            | not snapshot.mcpAdminValue.mcpEnabled ->
                pure (Left (McpAdminInvalid "Enable the MCP connection before authorizing"))
            | otherwise -> do
                let server = snapshot.mcpAdminValue
                probed <- host.connectionProbe server Nothing
                case probed of
                    Left err -> pure (Left (McpAdminInvalid err))
                    Right McpConnectionReady -> commit generation revision Nothing
                    Right McpConnectionAuthorizationRequired ->
                        case server.mcpUrl of
                            Nothing -> pure (Left (McpAdminInvalid "MCP connection has no endpoint"))
                            Just url -> do
                                authorized <- authorizeMcpWith McpOAuthHost
                                    { oauthLoadPrevious = host.connectionLoadCredential identifier
                                    , oauthOpenBrowser = openBrowser
                                    } defaultLoginOptions server.mcpOAuth url
                                case authorized of
                                    Left err -> pure (Left (McpAdminInvalid err))
                                    Right credential -> do
                                        verified <- host.connectionProbe server (Just credential)
                                        case verified of
                                            Left err -> pure (Left (McpAdminInvalid err))
                                            Right McpConnectionAuthorizationRequired ->
                                                pure (Left (McpAdminInvalid "MCP server rejected the authorization"))
                                            Right McpConnectionReady -> commit generation revision (Just credential)
   commit generation revision credential = invalidateAfter $
        withHarnessConfigSnapshot home
            (\current config ->
                if current /= revision
                    then pure (Left (McpAdminConflict current))
                    else case findConnection identifier config of
                        Left err -> pure (Left err)
                        Right (_, _, server)
                            | server.mcpConnectionGeneration /= Just generation || not server.mcpEnabled ->
                                pure (Left (McpAdminInvalid "MCP authorization was superseded by a connection change"))
                        Right (_, url, server) -> do
                            saved <- maybe (pure (Right ()))
                                (host.connectionSaveCredential identifier) credential
                            pure case saved of
                                Left err -> Left (McpAdminInvalid err)
                                Right () -> Right McpAdminSnapshot
                                    { mcpAdminRevision = current
                                    , mcpAdminValue = publicConnection identifier url server
                                    })
            >>= \case
                Left err -> pure (Left (McpAdminInvalid err))
                Right result -> pure result

connectionServerName :: Text -> Text
connectionServerName identifier = "connection_" <> identifier

listMcpConnections
    :: OsPath -> IO (Either McpAdminError (McpAdminSnapshot [McpConnection]))
listMcpConnections home =
    loadSnapshot home \config ->
        [ publicConnection identifier url server
        | server <- Map.elems config.configMcpServers
        , Just identifier <- [server.mcpConnectionId]
        , Just url <- [server.mcpUrl]
        ]

createMcpConnection
    :: OsPath -> Word64 -> Text -> Text
    -> IO (Either McpAdminError (McpAdminSnapshot McpConnection))
createMcpConnection home expected label endpoint = do
    identifier <- UUID.toText <$> UUID.nextRandom
    generation <- UUID.toText <$> UUID.nextRandom
    invalidateAfter $ mutate home expected \config -> do
        displayName <- validateDisplayName label
        url <- validateEndpoint endpoint
        let name = connectionServerName identifier
            server = McpServerConfig
                { mcpEnabled = True
                , mcpUrl = Just url
                , mcpConnectionId = Just identifier
                , mcpConnectionCredentials = Just True
                , mcpConnectionGeneration = Just generation
                , mcpDisplayName = Just displayName
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
        when (Map.member name config.configMcpServers) $
            Left (McpAdminAlreadyExists identifier)
        pure
            ( config { configMcpServers = Map.insert name server config.configMcpServers }
            , publicConnection identifier url server
            )

renameMcpConnection
    :: OsPath -> Word64 -> Text -> Text
    -> IO (Either McpAdminError (McpAdminSnapshot McpConnection))
renameMcpConnection home expected identifier label =
    invalidateAfter $ changeConnection home expected identifier \server -> do
        displayName <- validateDisplayName label
        pure server { mcpDisplayName = Just displayName }

setMcpConnectionEnabled
    :: OsPath -> Word64 -> Text -> Bool
    -> IO (Either McpAdminError (McpAdminSnapshot McpConnection))
setMcpConnectionEnabled home expected identifier enabled = do
    generation <- UUID.toText <$> UUID.nextRandom
    invalidateAfter $ changeConnection home expected identifier \server ->
        Right server { mcpEnabled = enabled, mcpConnectionGeneration = Just generation }

removeMcpConnection
    :: OsPath -> Word64 -> Text
    -> IO (Either McpAdminError (McpAdminSnapshot ()))
removeMcpConnection = removeMcpConnectionWith deleteMcpConnectionRecord

removeMcpConnectionWith
    :: (Text -> IO (Either Text ())) -> OsPath -> Word64 -> Text
    -> IO (Either McpAdminError (McpAdminSnapshot ()))
removeMcpConnectionWith deleteCredential home expected identifier =
    -- Match credential writers' lock order: catalog, then short store lock.
    -- A failed secure deletion leaves the catalog row available for retry.
    invalidateAfter $ mutateEffect home expected \config ->
        case findConnection identifier config of
            Left err -> pure (Left err)
            Right (name, _, server) ->
                (if mcpUsesConnectionCredentials server
                    then deleteCredential identifier else pure (Right ())) >>= \case
                    Left err -> pure (Left (McpAdminInvalid err))
                    Right () -> pure $ Right
                        ( config { configMcpServers =
                            Map.delete name config.configMcpServers }
                        , ()
                        )

invalidateAfter :: IO (Either McpAdminError a) -> IO (Either McpAdminError a)
invalidateAfter action = do
    result <- action
    case result of
        Right _ -> invalidateMcpConnectionRuntimes
        Left _ -> pure ()
    pure result

readMcpConnectionConfig
    :: OsPath -> Word64 -> Text
    -> IO (Either McpAdminError (McpAdminSnapshot McpServerConfig))
readMcpConnectionConfig home expected identifier = do
    loaded <- loadSnapshot home id
    pure do
        snapshot <- loaded
        when (snapshot.mcpAdminRevision /= expected) $
            Left (McpAdminConflict snapshot.mcpAdminRevision)
        (_, _, server) <- findConnection identifier snapshot.mcpAdminValue
        pure snapshot { mcpAdminValue = server }

changeConnection
    :: OsPath -> Word64 -> Text
    -> (McpServerConfig -> Either McpAdminError McpServerConfig)
    -> IO (Either McpAdminError (McpAdminSnapshot McpConnection))
changeConnection home expected identifier change =
    mutate home expected \config -> do
        (name, url, existing) <- findConnection identifier config
        server <- change existing
        pure
            ( config { configMcpServers =
                Map.insert name server config.configMcpServers }
            , publicConnection identifier url server
            )

findConnection :: Text -> HarnessConfig -> Either McpAdminError (Text, Text, McpServerConfig)
findConnection identifier config =
    case [(name, url, server)
         | (name, server) <- Map.toList config.configMcpServers
         , server.mcpConnectionId == Just identifier
         , Just url <- [server.mcpUrl]] of
        [connection] -> Right connection
        _ -> Left (McpAdminNotFound identifier)

publicConnection :: Text -> Text -> McpServerConfig -> McpConnection
publicConnection identifier url server = McpConnection
    { connectionId = identifier
    , connectionDisplayName = fromMaybe identifier server.mcpDisplayName
    , connectionUrl = url
    , connectionEnabled = server.mcpEnabled
    , connectionGeneration = server.mcpConnectionGeneration
    }

validateDisplayName :: Text -> Either McpAdminError Text
validateDisplayName value = do
    let label = Text.strip value
    when (Text.null label || Text.length label > 200 || Text.any isControl label) $
        Left (McpAdminInvalid "Connection name must contain 1–200 characters and no control characters")
    pure label

validateEndpoint :: Text -> Either McpAdminError Text
validateEndpoint value = do
    let endpoint = Text.strip value
    either (Left . McpAdminInvalid) pure (OAuth.validateOAuthEndpoint endpoint)
    pure endpoint
