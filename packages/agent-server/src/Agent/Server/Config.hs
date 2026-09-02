-- | Command-line configuration and canonical workspace-root policy.
module Agent.Server.Config
    ( ServerConfig(..)
    , ResolvedServerConfig(..)
    , defaultServerConfig
    , parseServerConfig
    , resolveServerConfig
    , resolveWorkspacePath
    ) where

import Agent.Server.Auth
    ( AuthConfig(..)
    , AuthMode(..)
    )
import Agent.Server.Types (ApiError(..))
import Control.Applicative (many)
import Control.Exception.Safe
    ( bracket
    , throwIO
    , tryIO
    )
import Control.Monad (when)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString8
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Maybe (isJust)
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
    , getCurrentDirectory
    , getHomeDirectory
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
import System.IO.Error (isEOFError)
import System.Posix.Files
    ( fileMode
    , fileOwner
    , getFdStatus
    , isRegularFile
    )
import System.Posix.IO
    ( OpenFileFlags(..)
    , OpenMode(ReadOnly)
    , closeFd
    , defaultFileFlags
    , openFd
    )
import System.Posix.IO.ByteString qualified as Posix
import System.Posix.Types (Fd)
import System.Posix.User (getEffectiveUserID)

data ServerConfig = ServerConfig
    { serverHost :: !String
    , serverPort :: !Int
    , serverAllowRemote :: !Bool
    , serverTokenFile :: !(Maybe FilePath)
    , serverCorsOrigins :: ![String]
    , serverWorkspaceRoots :: ![FilePath]
    , serverMaxConcurrentTurns :: !Int
    , serverMaxQueuedTurns :: !Int
    , serverEventReplayLimit :: !Int
    , serverMaximumRequestBytes :: !Int
    }
    deriving (Eq, Show)

data ResolvedServerConfig = ResolvedServerConfig
    { resolvedHost :: !String
    , resolvedPort :: !Int
    , resolvedAuth :: !AuthConfig
    , resolvedWorkspaceRoots :: ![FilePath]
    , resolvedDefaultCwd :: !FilePath
    , resolvedHome :: !FilePath
    , resolvedStateDirectory :: !FilePath
    , resolvedMaxConcurrentTurns :: !Int
    , resolvedMaxQueuedTurns :: !Int
    , resolvedEventReplayLimit :: !Int
    , resolvedMaximumRequestBytes :: !Int
    }

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
    { serverHost = "127.0.0.1"
    , serverPort = 4096
    , serverAllowRemote = False
    , serverTokenFile = Nothing
    , serverCorsOrigins = []
    , serverWorkspaceRoots = []
    , serverMaxConcurrentTurns = 3
    , serverMaxQueuedTurns = 100
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
                "Run the local haskell-agent REST and SSE server"
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
                    "Permit a non-loopback bind (requires bearer token)"
            )
        <*> optional
            (strOption
                ( long "token-file"
                    <> metavar "PATH"
                    <> help
                        "Require a bearer token read from a private file"
                ))
        <*> manyStringOption
            "cors-origin"
            "ORIGIN"
            "Explicitly allow a browser Origin"
        <*> manyStringOption
            "workspace-root"
            "PATH"
            "Allow a canonical workspace root (repeatable)"
        <*> positiveOption
            "max-concurrent-turns"
            "N"
            defaultServerConfig.serverMaxConcurrentTurns
            "Maximum concurrently running turns"
        <*> positiveOption
            "max-queued-turns"
            "N"
            defaultServerConfig.serverMaxQueuedTurns
            "Maximum queued turns"
        <*> positiveOption
            "event-replay-limit"
            "N"
            defaultServerConfig.serverEventReplayLimit
            "SSE events retained per gateway boundary"
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
resolveServerConfig config
    | config.serverPort < 1 || config.serverPort > 65535 =
        pure (Left "port must be between 1 and 65535")
    | any (< 1)
        [ config.serverMaxConcurrentTurns
        , config.serverMaxQueuedTurns
        , config.serverEventReplayLimit
        , config.serverMaximumRequestBytes
        ] =
        pure (Left "server limits must be positive")
    | not loopback && not config.serverAllowRemote =
        pure
            (Left
                "a non-loopback bind requires --allow-remote and bearer authentication")
    | otherwise = do
        cwd <- getCurrentDirectory
        home <- getHomeDirectory
        rootsResult <- canonicalRoots cwd config.serverWorkspaceRoots
        authResult <- resolveAuth config loopback
        pure do
            roots <- rootsResult
            auth <- authResult
            Right ResolvedServerConfig
                { resolvedHost = config.serverHost
                , resolvedPort = config.serverPort
                , resolvedAuth = auth
                , resolvedWorkspaceRoots = roots
                , resolvedDefaultCwd = cwd
                , resolvedHome = home
                , resolvedStateDirectory = home </> ".haskell-agent"
                , resolvedMaxConcurrentTurns =
                    config.serverMaxConcurrentTurns
                , resolvedMaxQueuedTurns = config.serverMaxQueuedTurns
                , resolvedEventReplayLimit =
                    config.serverEventReplayLimit
                , resolvedMaximumRequestBytes =
                    config.serverMaximumRequestBytes
                }
  where
    loopback = isLoopbackHost config.serverHost

resolveWorkspacePath
    :: ResolvedServerConfig
    -> Maybe FilePath
    -> IO (Either ApiError FilePath)
resolveWorkspacePath config requested = do
    let raw = case requested of
            Nothing -> config.resolvedDefaultCwd
            Just path
                | isAbsolute path -> path
                | otherwise -> config.resolvedDefaultCwd </> path
    exists <- doesDirectoryExist raw
    if not exists
        then pure (Left invalidWorkspace)
        else do
            result <- tryIO (canonicalizePath =<< makeAbsolute raw)
            pure case result of
                Left _ -> Left invalidWorkspace
                Right canonical
                    | any (`containsPath` canonical)
                        config.resolvedWorkspaceRoots ->
                            Right canonical
                    | otherwise ->
                        Left ApiError
                            { apiErrorStatus = 403
                            , apiErrorCode = "workspace_not_allowed"
                            , apiErrorMessage =
                                "the canonical working directory is outside the configured workspace roots"
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
                            go
                                (canonical : accumulated)
                                rest

containsPath :: FilePath -> FilePath -> Bool
containsPath root candidate =
    let relative = normalise (makeRelative root candidate)
        components = splitDirectories relative
    in not (isAbsolute relative)
        && case components of
            ".." : _ -> False
            _ -> True

resolveAuth
    :: ServerConfig
    -> Bool
    -> IO (Either Text AuthConfig)
resolveAuth config loopback = do
    environmentToken <- lookupEnv "AGENT_SERVER_TOKEN"
    -- Agent tools inherit the server environment. Remove the bearer secret
    -- even in loopback mode so shell subprocesses cannot read a stale value.
    when (isJust environmentToken) (unsetEnv "AGENT_SERVER_TOKEN")
    if loopback
        && config.serverTokenFile == Nothing
        && environmentToken == Nothing
        then
            pure $
                Right AuthConfig
                    { authMode =
                        LoopbackHostAuth
                            (allowedLoopbackHosts config.serverPort)
                    , authCorsOrigins = origins
                    }
        else
            loadRemoteToken config environmentToken >>= \case
                Left err -> pure (Left err)
                Right token ->
                    pure $
                        Right AuthConfig
                            { authMode = BearerTokenAuth token
                            , authCorsOrigins = origins
                            }
  where
    origins =
        Set.fromList
            (map
                (TextEncoding.encodeUtf8 . Text.pack)
                config.serverCorsOrigins)

loadRemoteToken
    :: ServerConfig
    -> Maybe String
    -> IO (Either Text ByteString)
loadRemoteToken config environmentToken =
    case (config.serverTokenFile, environmentToken) of
        (Just _, Just _) ->
            pure
                (Left
                    "set only one of --token-file or AGENT_SERVER_TOKEN")
        (Nothing, Nothing) ->
            pure
                (Left
                    "remote mode requires --token-file or AGENT_SERVER_TOKEN")
        (Nothing, Just token) ->
            pure (validateToken (ByteString8.pack token))
        (Just path, Nothing) -> do
            readPrivateTokenFile path >>= \case
                Left err -> pure (Left err)
                Right bytes -> pure (validateToken bytes)

readPrivateTokenFile :: FilePath -> IO (Either Text ByteString)
readPrivateTokenFile path = do
    inspected <- tryIO $
        bracket
            (openFd
                path
                ReadOnly
                defaultFileFlags
                    { nofollow = True
                    , cloexec = True
                    })
            closeFd
            \descriptor -> do
                status <- getFdStatus descriptor
                user <- getEffectiveUserID
                if not (isRegularFile status)
                    then
                        pure $
                            Left
                                "the bearer token path must be a regular file"
                    else if fileOwner status /= user
                        then
                            pure $
                                Left
                                    "the bearer token file must be owned by the current user"
                        else if fileMode status .&. 0o077 /= 0
                            then
                                pure $
                                    Left
                                        "the bearer token file must not be accessible by group or other users"
                            else Right <$> readTokenBytes descriptor
    pure case inspected of
        Left _ -> Left "could not inspect the bearer token file"
        Right result -> result

readTokenBytes :: Fd -> IO ByteString
readTokenBytes descriptor = go 4097 []
  where
    go remaining chunks
        | remaining <= 0 =
            pure (ByteString.concat (reverse chunks))
        | otherwise = do
            tryIO
                (Posix.fdRead descriptor (fromIntegral remaining))
                >>= \case
                    Left err
                        | isEOFError err ->
                            pure (ByteString.concat (reverse chunks))
                        | otherwise -> throwIO err
                    Right chunk
                        | ByteString.null chunk ->
                            pure (ByteString.concat (reverse chunks))
                        | otherwise ->
                            go
                                (remaining - ByteString.length chunk)
                                (chunk : chunks)

validateToken :: ByteString -> Either Text ByteString
validateToken raw =
    let token = stripAsciiSpace raw
    in if ByteString.length raw > 4096
        then Left "the bearer token is too large"
        else if ByteString.null token
        then Left "the bearer token must not be empty"
        else Right token

stripAsciiSpace :: ByteString -> ByteString
stripAsciiSpace =
    ByteString8.dropWhileEnd isAsciiSpace
        . ByteString8.dropWhile isAsciiSpace
  where
    isAsciiSpace character =
        character == ' '
            || character == '\t'
            || character == '\r'
            || character == '\n'

allowedLoopbackHosts :: Int -> Set.Set ByteString
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
