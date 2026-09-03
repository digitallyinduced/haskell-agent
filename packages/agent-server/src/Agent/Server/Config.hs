-- | Command-line configuration and canonical tenant workspace policy.
module Agent.Server.Config
    ( ServerConfig(..)
    , ResolvedServerConfig(..)
    , ResolvedServerMode(..)
    , MultiTenantConfig(..)
    , defaultServerConfig
    , parseServerConfig
    , resolveServerConfig
    , resolveServerConfigWithTrustPolicy
    , resolveWorkspacePath
    , resolveTenantWorkspacePath
    , lookupResolvedTenant
    ) where

import Agent.Server.Auth
    ( AuthConfig(..)
    , AuthMode(..)
    )
import Agent.Server.PrivateFile
    ( TrustedPathPolicy
    , fullTrustedPathPolicy
    , readPrivateTokenFile
    , validateTrustedPathWithPolicy
    )
import Agent.Server.Tenant
    ( ResolvedTenant(..)
    , TenantRegistry
    , loadTenantRegistryWithTrustPolicy
    , lookupTenant
    , tenantRegistryCredentials
    , tenantRegistryTenants
    )
import Agent.Server.Tenant qualified as Tenant
import Agent.Server.Types
    ( ApiError(..)
    , TenantId
    , localTenantId
    )
import Control.Applicative (many)
import Control.Monad (when)
import Data.ByteString.Char8 qualified as ByteString8
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Options.Applicative
    ( Parser
    , ParserInfo
    , auto
    , execParser
    , fullDesc
    , header
    , help
    , helper
    , info
    , long
    , metavar
    , option
    , optional
    , progDesc
    , showDefault
    , strOption
    , switch
    , value
    , (<**>)
    )
import System.Directory
    ( canonicalizePath
    , doesDirectoryExist
    , doesFileExist
    , executable
    , getCurrentDirectory
    , getHomeDirectory
    , getPermissions
    , makeAbsolute
    )
import System.Environment
    ( lookupEnv
    , unsetEnv
    )
import System.FilePath
    ( (</>)
    , isAbsolute
    , makeRelative
    , normalise
    , splitDirectories
    )
import Control.Exception.Safe (tryIO)

data ServerConfig = ServerConfig
    { serverHost :: !String
    , serverPort :: !Int
    , serverAllowRemote :: !Bool
    , serverTokenFile :: !(Maybe FilePath)
    , serverTenantRegistry :: !(Maybe FilePath)
    , serverTenantStateRoot :: !(Maybe FilePath)
    , serverSandboxRunner :: !(Maybe FilePath)
    , serverCorsOrigins :: ![String]
    , serverWorkspaceRoots :: ![FilePath]
    , serverMaxConcurrentTurns :: !Int
    , serverMaxConcurrentTurnsPerTenant :: !Int
    , serverMaxQueuedTurns :: !Int
    , serverMaxQueuedTurnsPerTenant :: !Int
    , serverMaxActiveTenants :: !Int
    , serverMaxEventSubscribers :: !Int
    , serverMaxEventSubscribersPerTenant :: !Int
    , serverEventReplayLimit :: !Int
    , serverMaximumRequestBytes :: !Int
    }
    deriving (Eq, Show)

data MultiTenantConfig = MultiTenantConfig
    { multiTenantRegistry :: !TenantRegistry
    , multiTenantStateRoot :: !FilePath
    , multiTenantSandboxRunner :: !FilePath
    }

data ResolvedServerMode
    = LocalSingleUserMode
    | MultiTenantMode !MultiTenantConfig

data ResolvedServerConfig = ResolvedServerConfig
    { resolvedHost :: !String
    , resolvedPort :: !Int
    , resolvedAuth :: !AuthConfig
    , resolvedWorkspaceRoots :: ![FilePath]
    , resolvedDefaultCwd :: !FilePath
    , resolvedHome :: !FilePath
    , resolvedStateDirectory :: !FilePath
    , resolvedServerMode :: !ResolvedServerMode
    , resolvedMaxConcurrentTurns :: !Int
    , resolvedMaxConcurrentTurnsPerTenant :: !Int
    , resolvedMaxQueuedTurns :: !Int
    , resolvedMaxQueuedTurnsPerTenant :: !Int
    , resolvedMaxActiveTenants :: !Int
    , resolvedMaxEventSubscribers :: !Int
    , resolvedMaxEventSubscribersPerTenant :: !Int
    , resolvedEventReplayLimit :: !Int
    , resolvedMaximumRequestBytes :: !Int
    }

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
    { serverHost = "127.0.0.1"
    , serverPort = 4096
    , serverAllowRemote = False
    , serverTokenFile = Nothing
    , serverTenantRegistry = Nothing
    , serverTenantStateRoot = Nothing
    , serverSandboxRunner = Nothing
    , serverCorsOrigins = []
    , serverWorkspaceRoots = []
    , serverMaxConcurrentTurns = 3
    , serverMaxConcurrentTurnsPerTenant = 2
    , serverMaxQueuedTurns = 100
    , serverMaxQueuedTurnsPerTenant = 25
    , serverMaxActiveTenants = 16
    , serverMaxEventSubscribers = 256
    , serverMaxEventSubscribersPerTenant = 8
    , serverEventReplayLimit = 1000
    , serverMaximumRequestBytes = 1024 * 1024
    }

parseServerConfig :: IO ServerConfig
parseServerConfig = execParser serverConfigInfo

serverConfigInfo :: ParserInfo ServerConfig
serverConfigInfo =
    info
        (serverConfigParser <**> helper)
        ( fullDesc
            <> progDesc
                "Run the haskell-agent REST and SSE server"
            <> header "agent-server"
        )

serverConfigParser :: Parser ServerConfig
serverConfigParser =
    ServerConfig
        <$> strOption
            ( long "host"
                <> metavar "HOST"
                <> value defaultServerConfig.serverHost
                <> showDefault
                <> help "Address to bind"
            )
        <*> option auto
            ( long "port"
                <> metavar "PORT"
                <> value defaultServerConfig.serverPort
                <> showDefault
                <> help "TCP port to bind"
            )
        <*> switch
            ( long "allow-remote"
                <> help
                    "Permit a non-loopback bind (requires bearer authentication)"
            )
        <*> optional
            (strOption
                ( long "token-file"
                    <> metavar "PATH"
                    <> help
                        "Single-user bearer token in an owner-only file"
                ))
        <*> optional
            (strOption
                ( long "tenant-registry"
                    <> metavar "PATH"
                    <> help
                        "Owner-only versioned tenant registry (enables multi-tenant mode)"
                ))
        <*> optional
            (strOption
                ( long "tenant-state-root"
                    <> metavar "PATH"
                    <> help
                        "Server-owned root for tenant homes and VM state"
                ))
        <*> optional
            (strOption
                ( long "sandbox-runner"
                    <> metavar "PATH"
                    <> help
                        "Prebuilt per-tenant microVM runner executable"
                ))
        <*> manyStringOption
            "cors-origin"
            "ORIGIN"
            "Explicitly allow a browser Origin"
        <*> manyStringOption
            "workspace-root"
            "PATH"
            "Allow a canonical workspace root in local mode (repeatable)"
        <*> positiveOption
            "max-concurrent-turns"
            "N"
            defaultServerConfig.serverMaxConcurrentTurns
            "Maximum concurrently running turns"
        <*> positiveOption
            "max-concurrent-turns-per-tenant"
            "N"
            defaultServerConfig.serverMaxConcurrentTurnsPerTenant
            "Maximum concurrently running turns for one tenant"
        <*> positiveOption
            "max-queued-turns"
            "N"
            defaultServerConfig.serverMaxQueuedTurns
            "Maximum queued turns"
        <*> positiveOption
            "max-queued-turns-per-tenant"
            "N"
            defaultServerConfig.serverMaxQueuedTurnsPerTenant
            "Maximum queued turns for one tenant"
        <*> positiveOption
            "max-active-tenants"
            "N"
            defaultServerConfig.serverMaxActiveTenants
            "Maximum active tenant runtimes and microVMs"
        <*> positiveOption
            "max-event-subscribers"
            "N"
            defaultServerConfig.serverMaxEventSubscribers
            "Maximum concurrent SSE event subscribers"
        <*> positiveOption
            "max-event-subscribers-per-tenant"
            "N"
            defaultServerConfig.serverMaxEventSubscribersPerTenant
            "Maximum concurrent SSE event subscribers for one tenant"
        <*> positiveOption
            "event-replay-limit"
            "N"
            defaultServerConfig.serverEventReplayLimit
            "SSE events retained per authenticated access boundary"
        <*> positiveOption
            "maximum-request-bytes"
            "BYTES"
            defaultServerConfig.serverMaximumRequestBytes
            "Maximum JSON request body size"

manyStringOption :: String -> String -> String -> Parser [String]
manyStringOption name placeholder description =
    many $
        strOption
            ( long name
                <> metavar placeholder
                <> help description
            )

positiveOption :: String -> String -> Int -> String -> Parser Int
positiveOption name placeholder defaultValue description =
    option auto
        ( long name
            <> metavar placeholder
            <> value defaultValue
            <> showDefault
            <> help description
        )

resolveServerConfig
    :: ServerConfig
    -> IO (Either Text ResolvedServerConfig)
resolveServerConfig =
    resolveServerConfigWithTrustPolicy fullTrustedPathPolicy

-- | Resolve configuration with an explicit, structurally bounded ancestry
-- policy. The executable uses 'resolveServerConfig', which always validates to
-- the filesystem root.
resolveServerConfigWithTrustPolicy
    :: TrustedPathPolicy
    -> ServerConfig
    -> IO (Either Text ResolvedServerConfig)
resolveServerConfigWithTrustPolicy trustPolicy config
    | config.serverPort < 1 || config.serverPort > 65535 =
        pure (Left "port must be between 1 and 65535")
    | any (< 1)
        [ config.serverMaxConcurrentTurns
        , config.serverMaxConcurrentTurnsPerTenant
        , config.serverMaxQueuedTurns
        , config.serverMaxQueuedTurnsPerTenant
        , config.serverMaxActiveTenants
        , config.serverMaxEventSubscribers
        , config.serverMaxEventSubscribersPerTenant
        , config.serverEventReplayLimit
        , config.serverMaximumRequestBytes
        ] =
        pure (Left "server limits must be positive")
    | config.serverMaxConcurrentTurnsPerTenant
        > config.serverMaxConcurrentTurns =
        pure
            (Left
                "per-tenant concurrency cannot exceed global concurrency")
    | config.serverMaxQueuedTurnsPerTenant > config.serverMaxQueuedTurns =
        pure
            (Left
                "per-tenant queue capacity cannot exceed global capacity")
    | config.serverMaxEventSubscribersPerTenant
        > config.serverMaxEventSubscribers =
        pure
            (Left
                "per-tenant event subscribers cannot exceed global capacity")
    | not loopback && not config.serverAllowRemote =
        pure
            (Left
                "a non-loopback bind requires --allow-remote and bearer authentication")
    | otherwise = do
        cwd <- getCurrentDirectory
        home <- getHomeDirectory
        environmentToken <- lookupEnv "AGENT_SERVER_TOKEN"
        -- Never leave the server bearer in the environment inherited by tools
        -- or microVM launchers.
        when (isJust environmentToken) (unsetEnv "AGENT_SERVER_TOKEN")
        resolvedMode <-
            case config.serverTenantRegistry of
                Nothing ->
                    resolveLocalMode config cwd environmentToken
                Just registryPath ->
                    resolveMultiTenantMode
                        trustPolicy
                        config
                        home
                        environmentToken
                        registryPath
        pure do
            (auth, roots, mode) <- resolvedMode
            Right ResolvedServerConfig
                { resolvedHost = config.serverHost
                , resolvedPort = config.serverPort
                , resolvedAuth = auth
                , resolvedWorkspaceRoots = roots
                , resolvedDefaultCwd = cwd
                , resolvedHome = home
                , resolvedStateDirectory = home </> ".haskell-agent"
                , resolvedServerMode = mode
                , resolvedMaxConcurrentTurns =
                    config.serverMaxConcurrentTurns
                , resolvedMaxConcurrentTurnsPerTenant =
                    config.serverMaxConcurrentTurnsPerTenant
                , resolvedMaxQueuedTurns = config.serverMaxQueuedTurns
                , resolvedMaxQueuedTurnsPerTenant =
                    config.serverMaxQueuedTurnsPerTenant
                , resolvedMaxActiveTenants =
                    config.serverMaxActiveTenants
                , resolvedMaxEventSubscribers =
                    config.serverMaxEventSubscribers
                , resolvedMaxEventSubscribersPerTenant =
                    config.serverMaxEventSubscribersPerTenant
                , resolvedEventReplayLimit =
                    config.serverEventReplayLimit
                , resolvedMaximumRequestBytes =
                    config.serverMaximumRequestBytes
                }
  where
    loopback = isLoopbackHost config.serverHost

resolveLocalMode
    :: ServerConfig
    -> FilePath
    -> Maybe String
    -> IO
        (Either
            Text
            (AuthConfig, [FilePath], ResolvedServerMode))
resolveLocalMode config cwd environmentToken
    | config.serverTenantStateRoot /= Nothing
        || config.serverSandboxRunner /= Nothing =
        pure
            (Left
                "--tenant-state-root and --sandbox-runner require --tenant-registry")
    | otherwise = do
        roots <- canonicalRoots cwd config.serverWorkspaceRoots
        auth <-
            resolveLocalAuth
                config
                (isLoopbackHost config.serverHost)
                environmentToken
        pure do
            resolvedRoots <- roots
            resolvedAuth <- auth
            Right
                ( resolvedAuth
                , resolvedRoots
                , LocalSingleUserMode
                )

resolveMultiTenantMode
    :: TrustedPathPolicy
    -> ServerConfig
    -> FilePath
    -> Maybe String
    -> FilePath
    -> IO
        (Either
            Text
            (AuthConfig, [FilePath], ResolvedServerMode))
resolveMultiTenantMode
        trustPolicy config home environmentToken registryPath
    | config.serverTokenFile /= Nothing || environmentToken /= Nothing =
        pure
            (Left
                "multi-tenant mode accepts credentials only from the tenant registry")
    | not (null config.serverWorkspaceRoots) =
        pure
            (Left
                "--workspace-root is local-mode only; configure roots per tenant")
    | otherwise =
        case config.serverSandboxRunner of
            Nothing ->
                pure
                    (Left
                        "multi-tenant mode requires --sandbox-runner")
            Just runner -> do
                resolvedRunner <-
                    tryIO (canonicalizePath =<< makeAbsolute runner)
                case resolvedRunner of
                    Left _ ->
                        pure
                            (Left
                                "the configured sandbox runner is unavailable")
                    Right canonicalRunner -> do
                        runnerExists <- doesFileExist canonicalRunner
                        runnerPermissions <- tryIO (getPermissions canonicalRunner)
                        if
                            not runnerExists
                                || either
                                    (const True)
                                    (not . executable)
                                    runnerPermissions
                            then
                                pure
                                    (Left
                                        "the configured sandbox runner is not an executable file")
                            else do
                                validateTrustedPathWithPolicy trustPolicy
                                    canonicalRunner >>= \case
                                        Left err ->
                                            pure
                                                (Left
                                                    ("the configured sandbox runner is not trusted: "
                                                        <> err))
                                        Right () -> do
                                            let stateRoot =
                                                    maybe
                                                        (home
                                                            </> ".haskell-agent"
                                                            </> "server-tenants")
                                                        id
                                                        config.serverTenantStateRoot
                                            loadTenantRegistryWithTrustPolicy
                                                trustPolicy
                                                stateRoot
                                                registryPath >>= \case
                                                    Left err -> pure (Left err)
                                                    Right registry
                                                        | runnerOverlapsTenant
                                                            canonicalRunner
                                                            registry ->
                                                            pure
                                                                (Left
                                                                    "the configured sandbox runner must be outside tenant-writable roots")
                                                        | length
                                                            (tenantRegistryTenants
                                                                registry)
                                                            > config.serverMaxActiveTenants ->
                                                            pure
                                                                (Left
                                                                    "the tenant registry exceeds --max-active-tenants")
                                                        | otherwise ->
                                                            pure
                                                                (Right
                                                                    ( AuthConfig
                                                                    { authMode =
                                                                        TenantBearerAuth
                                                                            (tenantRegistryCredentials
                                                                                registry)
                                                                    , authCorsOrigins =
                                                                        corsOrigins
                                                                            config
                                                                    }
                                                                , []
                                                                , MultiTenantMode
                                                                    MultiTenantConfig
                                                                        { multiTenantRegistry =
                                                                            registry
                                                                        , multiTenantStateRoot =
                                                                            stateRoot
                                                                        , multiTenantSandboxRunner =
                                                                            canonicalRunner
                                                                        }
                                                                    ))

runnerOverlapsTenant :: FilePath -> TenantRegistry -> Bool
runnerOverlapsTenant runner =
    any overlaps . tenantRegistryTenants
  where
    overlaps tenant =
        any
            (`containsPath` runner)
            [ tenant.resolvedTenantWorkspaceRoot
            , tenant.resolvedTenantStateDirectory
            , tenant.resolvedTenantHome
            ]

resolveLocalAuth
    :: ServerConfig
    -> Bool
    -> Maybe String
    -> IO (Either Text AuthConfig)
resolveLocalAuth config loopback environmentToken =
    if loopback
        && config.serverTokenFile == Nothing
        && environmentToken == Nothing
        then
            pure $
                Right AuthConfig
                    { authMode =
                        LoopbackHostAuth
                            (allowedLoopbackHosts config.serverPort)
                    , authCorsOrigins = corsOrigins config
                    }
        else
            loadRemoteToken config environmentToken >>= \case
                Left err -> pure (Left err)
                Right token ->
                    pure $
                        Right AuthConfig
                            { authMode = BearerTokenAuth token
                            , authCorsOrigins = corsOrigins config
                            }

loadRemoteToken
    :: ServerConfig
    -> Maybe String
    -> IO (Either Text ByteString8.ByteString)
loadRemoteToken config environmentToken =
    case (config.serverTokenFile, environmentToken) of
        (Just _, Just _) ->
            pure (Left "set only one of --token-file or AGENT_SERVER_TOKEN")
        (Nothing, Nothing) ->
            pure
                (Left
                    "remote mode requires --token-file or AGENT_SERVER_TOKEN")
        (Nothing, Just token) ->
            pure $
                if null token
                    then Left "the bearer token must not be empty"
                    else Right (ByteString8.pack token)
        (Just path, Nothing) -> readPrivateTokenFile path

resolveWorkspacePath
    :: ResolvedServerConfig
    -> Maybe FilePath
    -> IO (Either ApiError FilePath)
resolveWorkspacePath config requested =
    case config.resolvedServerMode of
        MultiTenantMode _ ->
            pure $
                Left ApiError
                    { apiErrorStatus = 500
                    , apiErrorCode = "tenant_context_required"
                    , apiErrorMessage =
                        "a tenant context is required for workspace resolution"
                    , apiErrorDetails = Nothing
                    }
        LocalSingleUserMode ->
            resolveWorkspacePathIn
                config.resolvedDefaultCwd
                config.resolvedWorkspaceRoots
                requested

resolveTenantWorkspacePath
    :: ResolvedServerConfig
    -> TenantId
    -> Maybe FilePath
    -> IO (Either ApiError FilePath)
resolveTenantWorkspacePath config tenantId requested =
    case config.resolvedServerMode of
        LocalSingleUserMode
            | tenantId == localTenantId ->
                resolveWorkspacePath config requested
            | otherwise -> pure (Left tenantUnavailable)
        MultiTenantMode multi ->
            case lookupTenant multi.multiTenantRegistry tenantId of
                Nothing -> pure (Left tenantUnavailable)
                Just tenant ->
                    Tenant.resolveTenantWorkspacePath tenant requested

lookupResolvedTenant
    :: ResolvedServerConfig
    -> TenantId
    -> Either ApiError ResolvedTenant
lookupResolvedTenant config tenantId =
    case config.resolvedServerMode of
        LocalSingleUserMode -> Left tenantUnavailable
        MultiTenantMode multi ->
            maybe
                (Left tenantUnavailable)
                Right
                (lookupTenant multi.multiTenantRegistry tenantId)

resolveWorkspacePathIn
    :: FilePath
    -> [FilePath]
    -> Maybe FilePath
    -> IO (Either ApiError FilePath)
resolveWorkspacePathIn defaultCwd roots requested = do
    let raw = case requested of
            Nothing -> defaultCwd
            Just path
                | isAbsolute path -> path
                | otherwise -> defaultCwd </> path
    exists <- doesDirectoryExist raw
    if not exists
        then pure (Left invalidWorkspace)
        else do
            result <- tryIO (canonicalizePath =<< makeAbsolute raw)
            pure case result of
                Left _ -> Left invalidWorkspace
                Right canonical
                    | any (`containsPath` canonical) roots -> Right canonical
                    | otherwise ->
                        Left ApiError
                            { apiErrorStatus = 403
                            , apiErrorCode = "workspace_not_allowed"
                            , apiErrorMessage =
                                "the working directory is outside the configured workspace roots"
                            , apiErrorDetails = Nothing
                            }
  where
    invalidWorkspace = ApiError
        { apiErrorStatus = 422
        , apiErrorCode = "invalid_workspace"
        , apiErrorMessage =
            "the working directory must be an existing directory"
        , apiErrorDetails = Nothing
        }

canonicalRoots
    :: FilePath
    -> [FilePath]
    -> IO (Either Text [FilePath])
canonicalRoots cwd configured = go [] candidates
  where
    candidates
        | null configured = [cwd]
        | otherwise = configured

    go accumulated = \case
        [] -> pure (Right (reverse accumulated))
        path : rest -> do
            let absolute
                    | isAbsolute path = path
                    | otherwise = cwd </> path
            exists <- doesDirectoryExist absolute
            if not exists
                then
                    pure
                        (Left
                            ("workspace root is not an existing directory: "
                                <> Text.pack path))
                else do
                    result <- tryIO (canonicalizePath absolute)
                    case result of
                        Left _ ->
                            pure
                                (Left
                                    ("could not canonicalize workspace root: "
                                        <> Text.pack path))
                        Right canonical ->
                            go (canonical : accumulated) rest

tenantUnavailable :: ApiError
tenantUnavailable = ApiError
    { apiErrorStatus = 401
    , apiErrorCode = "tenant_unavailable"
    , apiErrorMessage = "the authenticated tenant is unavailable"
    , apiErrorDetails = Nothing
    }

corsOrigins :: ServerConfig -> Set.Set ByteString8.ByteString
corsOrigins config =
    Set.fromList
        (map
            (TextEncoding.encodeUtf8 . Text.pack)
            config.serverCorsOrigins)

containsPath :: FilePath -> FilePath -> Bool
containsPath root candidate =
    let relative = normalise (makeRelative root candidate)
        components = splitDirectories relative
    in not (isAbsolute relative)
        && case components of
            ".." : _ -> False
            _ -> True

allowedLoopbackHosts :: Int -> Set.Set ByteString8.ByteString
allowedLoopbackHosts port =
    Set.fromList
        [ ByteString8.pack ("127.0.0.1:" <> show port)
        , ByteString8.pack ("localhost:" <> show port)
        , ByteString8.pack ("[::1]:" <> show port)
        ]

isLoopbackHost :: String -> Bool
isLoopbackHost host =
    map asciiLowerChar host
        `elem` ["127.0.0.1", "localhost", "::1"]

asciiLowerChar :: Char -> Char
asciiLowerChar character
    | character >= 'A' && character <= 'Z' =
        toEnum (fromEnum character + 32)
    | otherwise = character
