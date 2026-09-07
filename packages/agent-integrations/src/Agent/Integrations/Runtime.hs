-- | Process-owned integration runtime and its in-memory MCP endpoint.
module Agent.Integrations.Runtime
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
    ) where

import Agent.CLI.GatewayClient
    ( GatewayCredential
    , gatewayCredentialIdentity
    )
import Agent.Integrations.Admin
import Agent.Integrations.Email.Gateway
import Agent.Integrations.Email.Module
import Agent.Integrations.Email.OAuth
import Agent.Integrations.Email.Tools
import Agent.Integrations.Email.Transport
import Agent.Integrations.Registry
import Agent.Integrations.Server
import Agent.Integrations.Types (IntegrationError)
import Agent.Json (RawJson)
import qualified Agent.MCP as MCP
import Agent.Tools.Types
    ( ToolEnv
    , addToolAllowedRoot
    , setToolSessionTmp
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , newMVar
    )
import Control.Exception.Safe (finally, tryAny)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.Directory (removePathForcibly)
import System.IO.Temp
    ( createTempDirectory
    , getCanonicalTemporaryDirectory
    )
import System.OsPath (unsafeEncodeUtf)

data IntegrationAuthority
    = LocalIntegrationAuthority
    | OrganizationIntegrationAuthority !GatewayCredential
    deriving (Eq, Show)

data IntegrationBackend
    = LocalIntegrationBackend !MailOAuthRuntime
    | OrganizationIntegrationBackend !GatewayMailRuntime

data IntegrationRuntime = IntegrationRuntime
    { integrationHost :: !IntegrationHost
    , integrationAdminRegistry :: !IntegrationAdminRegistry
    , integrationBackend :: !IntegrationBackend
    , integrationClosed :: !(MVar Bool)
    }

data IntegrationSupervisorState = IntegrationSupervisorState
    { integrationSupervisorClosed :: !Bool
    , integrationSupervisorRuntimes ::
        !(Map.Map IntegrationRuntimeKey IntegrationRuntime)
    }

data IntegrationRuntimeKey
    = LocalIntegrationRuntimeKey
    | OrganizationIntegrationRuntimeKey !Text
    deriving (Eq, Ord)

data IntegrationSupervisor = IntegrationSupervisor
    { integrationSupervisorState :: !(MVar IntegrationSupervisorState)
    , integrationSupervisorToolEnv :: !ToolEnv
    , integrationSupervisorTemporaryDirectory :: !FilePath
    }

newIntegrationSupervisor :: ToolEnv -> IO IntegrationSupervisor
newIntegrationSupervisor integrationSupervisorToolEnv = do
    temporaryRoot <- getCanonicalTemporaryDirectory
    integrationSupervisorTemporaryDirectory <-
        createTempDirectory temporaryRoot "haskell-agent-integrations-"
    setToolSessionTmp
        integrationSupervisorToolEnv
        (Just (unsafeEncodeUtf integrationSupervisorTemporaryDirectory))
    integrationSupervisorState <- newMVar IntegrationSupervisorState
        { integrationSupervisorClosed = False
        , integrationSupervisorRuntimes = Map.empty
        }
    pure IntegrationSupervisor{..}

-- | Return the single runtime owned by this process for an authority. The
-- runtime remains alive until the supervisor closes, so provider restarts and
-- concurrent sessions share OAuth flows, account state, and MCP invalidation.
acquireIntegrationRuntime
    :: IntegrationSupervisor
    -> IntegrationAuthority
    -> IO (Either Text IntegrationRuntime)
acquireIntegrationRuntime supervisor authority =
    modifyMVar supervisor.integrationSupervisorState \state ->
        if state.integrationSupervisorClosed
            then pure
                ( state
                , Left "The integration runtime is already closed."
                )
            else
                case Map.lookup key state.integrationSupervisorRuntimes of
                    Just runtime -> pure (state, Right runtime)
                    Nothing ->
                        newIntegrationRuntime
                            supervisor.integrationSupervisorToolEnv
                            authority >>= \case
                            Left err -> pure (state, Left err)
                            Right runtime ->
                                pure
                                    ( state
                                        { integrationSupervisorRuntimes =
                                            Map.insert
                                                key
                                                runtime
                                                state.integrationSupervisorRuntimes
                                        }
                                    , Right runtime
                                    )
  where
    key = case authority of
        LocalIntegrationAuthority -> LocalIntegrationRuntimeKey
        OrganizationIntegrationAuthority credential ->
            OrganizationIntegrationRuntimeKey
                (gatewayCredentialIdentity credential)

-- | Permit a session to consume files produced by the process-owned
-- integration host. The integration host retains its own stable scratch
-- directory; only the session's allowed-root set changes, so concurrent
-- sessions cannot redirect each other's integration writes.
prepareIntegrationSupervisorForSession
    :: IntegrationSupervisor
    -> ToolEnv
    -> IO ()
prepareIntegrationSupervisorForSession supervisor sessionToolEnv =
    addToolAllowedRoot
        sessionToolEnv
        (unsafeEncodeUtf supervisor.integrationSupervisorTemporaryDirectory)

closeIntegrationSupervisor :: IntegrationSupervisor -> IO ()
closeIntegrationSupervisor supervisor = do
    runtimes <- modifyMVar supervisor.integrationSupervisorState \state ->
        pure
            ( state
                { integrationSupervisorClosed = True
                , integrationSupervisorRuntimes = Map.empty
                }
            , Map.elems state.integrationSupervisorRuntimes
            )
    mapM_ closeIntegrationRuntime runtimes
        `finally` do
            _ <- tryAny
                (removePathForcibly
                    supervisor.integrationSupervisorTemporaryDirectory)
            pure ()

newIntegrationRuntime
    :: ToolEnv
    -> IntegrationAuthority
    -> IO (Either Text IntegrationRuntime)
newIntegrationRuntime toolEnv authority =
    case authority of
        LocalIntegrationAuthority ->
            mailToolsEnvForStore toolEnv productionMailTransport >>= \case
                Left err -> pure (Left err)
                Right mailEnvironment -> do
                    oauthRuntime <- newMailOAuthRuntime
                    buildRuntime
                        mailEnvironment
                        (LocalIntegrationBackend oauthRuntime)
                        (Just oauthRuntime)
        OrganizationIntegrationAuthority credential ->
            gatewayMailTools toolEnv credential >>= \case
                Left err -> pure (Left err)
                Right gatewayRuntime ->
                    buildRuntime
                        gatewayRuntime.gatewayMailRuntimeEnvironment
                        (OrganizationIntegrationBackend gatewayRuntime)
                        Nothing
  where
    buildRuntime mailEnvironment backend maybeOAuth =
        case emailIntegrationModule mailEnvironment of
            Left err -> do
                closeBackend backend
                pure (Left err)
            Right emailModule ->
                case createIntegrationRegistry [emailModule] of
                    Left err -> do
                        closeBackend backend
                        pure (Left err)
                    Right registry -> do
                        host <- newIntegrationHost registry
                        let adminOperations =
                                maybe
                                    (Right [])
                                    (`emailAdminOperations` host)
                                    maybeOAuth
                        case adminOperations >>= createIntegrationAdminRegistry of
                            Left err -> do
                                closeIntegrationHost host
                                closeBackend backend
                                pure (Left err)
                            Right adminRegistry -> do
                                closed <- newMVar False
                                pure . Right $ IntegrationRuntime
                                    { integrationHost = host
                                    , integrationAdminRegistry = adminRegistry
                                    , integrationBackend = backend
                                    , integrationClosed = closed
                                    }

integrationRuntimeMcpServer :: IntegrationRuntime -> MCP.McpToolServer
integrationRuntimeMcpServer =
    integrationMcpServer . (.integrationHost)

integrationRuntimeAdminDefinitions
    :: IntegrationRuntime
    -> RawJson
integrationRuntimeAdminDefinitions =
    encodeIntegrationAdminDefinitions . (.integrationAdminRegistry)

callIntegrationRuntimeAdmin
    :: IntegrationRuntime
    -> Text
    -> RawJson
    -> IO (Either IntegrationError RawJson)
callIntegrationRuntimeAdmin runtime =
    callIntegrationAdmin runtime.integrationAdminRegistry

closeIntegrationRuntime :: IntegrationRuntime -> IO ()
closeIntegrationRuntime runtime = do
    shouldClose <- modifyMVar runtime.integrationClosed \closed ->
        pure (True, not closed)
    if shouldClose
        then
            closeIntegrationHost runtime.integrationHost
                `finally` closeBackend runtime.integrationBackend
        else pure ()

closeBackend :: IntegrationBackend -> IO ()
closeBackend = \case
    LocalIntegrationBackend oauthRuntime ->
        closeMailOAuthRuntime oauthRuntime
    OrganizationIntegrationBackend gatewayRuntime ->
        gatewayRuntime.gatewayMailRuntimeClose
