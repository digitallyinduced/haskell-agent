{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Initialization and start-up of the harness-owned PostgreSQL server.
--
-- The server is deliberately long-lived: releasing a Hasql pool does not stop
-- it. A filesystem lock serializes concurrent harness processes during first
-- start and recovery.
module Agent.Store.Postgres.Managed
    ( ManagedPostgresStatus(..)
    , managedPostgresStatus
    , ensureManagedPostgres
    , stopManagedPostgres
    ) where

import Control.Exception.Safe (SomeException, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    )
import System.Exit (ExitCode(..))
import qualified System.FileLock as FileLock
import System.FilePath ((</>))
import System.Posix.Files (setFileMode)
import System.Process (readProcessWithExitCode)

import Agent.Store.Postgres.Config
import Agent.Store.Types

data ManagedPostgresStatus
    = PostgresNotInitialized
    | PostgresStopped
    | PostgresRunning
    deriving (Eq, Show)

managedPostgresStatus
    :: ManagedPostgresConfig
    -> IO (Either StoreError ManagedPostgresStatus)
managedPostgresStatus config = do
    initialized <- doesFileExist
        (config.postgresPaths.postgresDataDirectory </> "PG_VERSION")
    if not initialized
        then pure (Right PostgresNotInitialized)
        else runCommand config "pg_ctl"
            [ "-D", config.postgresPaths.postgresDataDirectory
            , "status"
            ] >>= \case
                Left err -> pure (Left err)
                Right (ExitSuccess, _, _) -> pure (Right PostgresRunning)
                Right (ExitFailure 3, _, _) -> pure (Right PostgresStopped)
                Right result -> pure $ Left $ commandFailure "pg_ctl status" result

-- | Initialize, start, and bootstrap the configured database.
ensureManagedPostgres
    :: ManagedPostgresConfig
    -> IO (Either StoreError ManagedPostgresStatus)
ensureManagedPostgres config =
    case validateConfig config of
        Left err -> pure (Left err)
        Right () -> do
            prepareDirectories config
            withLifecycleLock config do
                validateClusterVersion config >>= \case
                    Left err -> pure (Left err)
                    Right () ->
                        managedPostgresStatus config >>= \case
                            Left err -> pure (Left err)
                            Right PostgresNotInitialized ->
                                initializeCluster config >>= continueAfterInitialization
                            Right PostgresStopped ->
                                startCluster config >>= continueAfterStart
                            Right PostgresRunning ->
                                ensureDatabase config >>= finish
  where
    continueAfterInitialization = \case
        Left err -> pure (Left err)
        Right () -> startCluster config >>= continueAfterStart
    continueAfterStart = \case
        Left err -> pure (Left err)
        Right () -> ensureDatabase config >>= finish
    finish = \case
        Left err -> pure (Left err)
        Right () -> pure (Right PostgresRunning)

validateClusterVersion
    :: ManagedPostgresConfig
    -> IO (Either StoreError ())
validateClusterVersion config = do
    let versionFile =
            config.postgresPaths.postgresDataDirectory </> "PG_VERSION"
    exists <- doesFileExist versionFile
    if not exists
        then pure (Right ())
        else try (ByteString.readFile versionFile) >>= \case
            Left (exception :: SomeException) ->
                pure $ Left $ StoreProcessError $
                    "Could not read managed PostgreSQL version: "
                        <> Text.pack (show exception)
            Right contents
                | majorVersion contents == "18" -> pure (Right ())
                | otherwise ->
                    pure $ Left $ StoreConfigurationError $
                        "Managed PostgreSQL requires major version 18, but "
                            <> "the existing data directory was initialized by "
                            <> "PostgreSQL "
                            <> Text.pack
                                (ByteString.Char8.unpack
                                    (majorVersion contents))
                            <> ". Migrate that cluster with pg_upgrade or move "
                            <> "it aside before restarting the agent."
  where
    majorVersion =
        ByteString.Char8.takeWhile
            (\char -> char /= '\n' && char /= '\r' && char /= ' ')

stopManagedPostgres
    :: ManagedPostgresConfig
    -> IO (Either StoreError ())
stopManagedPostgres config = do
    prepareDirectories config
    withLifecycleLock config do
        managedPostgresStatus config >>= \case
            Left err -> pure (Left err)
            Right PostgresRunning ->
                runCommand config "pg_ctl"
                    [ "-D", config.postgresPaths.postgresDataDirectory
                    , "-m", "fast"
                    , "-w"
                    , "stop"
                    ] >>= expectSuccess "pg_ctl stop"
            Right _ -> pure (Right ())

validateConfig :: ManagedPostgresConfig -> Either StoreError ()
validateConfig config
    | ByteString.length socketPath > 90 =
        Left $ StoreConfigurationError $
            "PostgreSQL socket directory is too long: "
                <> Text.pack config.postgresPaths.postgresSocketDirectory
    | config.postgresMaxConnections < 2 =
        Left $
            StoreConfigurationError "PostgreSQL max_connections must be at least 2"
    | otherwise = Right ()
  where
    socketPath :: ByteString
    socketPath = Text.encodeUtf8 $
        Text.pack config.postgresPaths.postgresSocketDirectory

prepareDirectories :: ManagedPostgresConfig -> IO ()
prepareDirectories config = do
    createPrivateDirectory config.postgresPaths.postgresRootDirectory
    createPrivateDirectory config.postgresPaths.postgresSocketDirectory
    createPrivateDirectory
        config.postgresPaths.postgresServerTurnActionLockDirectory
    createPrivateDirectory (serverTurnActionLockDirectory config)

createPrivateDirectory :: FilePath -> IO ()
createPrivateDirectory path = do
    createDirectoryIfMissing True path
    setFileMode path 0o700

withLifecycleLock
    :: ManagedPostgresConfig
    -> IO (Either StoreError a)
    -> IO (Either StoreError a)
withLifecycleLock config action =
    try
        (FileLock.withFileLock
            config.postgresPaths.postgresLifecycleLockFile
            FileLock.Exclusive
            (const action)) >>= \case
                Left (exception :: SomeException) -> pure $ Left $ StoreProcessError $
                    "Could not lock managed PostgreSQL lifecycle: "
                        <> Text.pack (show exception)
                Right result -> pure result

initializeCluster
    :: ManagedPostgresConfig
    -> IO (Either StoreError ())
initializeCluster config = do
    result <- runCommand config "initdb"
        [ "-D", config.postgresPaths.postgresDataDirectory
        , "--username", Text.unpack config.postgresOwnerRole
        , "--encoding", "UTF8"
        , "--locale", "C"
        , "--auth-local", "trust"
        , "--auth-host", "reject"
        ]
    expectSuccess "initdb" result >>= \case
        Left err -> pure (Left err)
        Right () -> do
            setFileMode config.postgresPaths.postgresDataDirectory 0o700
            writeFile
                (config.postgresPaths.postgresDataDirectory </> "postgresql.conf")
                (Text.unpack (postgresqlConf config))
            writeFile
                (config.postgresPaths.postgresDataDirectory </> "pg_hba.conf")
                (Text.unpack pgHbaConf)
            pure (Right ())

startCluster :: ManagedPostgresConfig -> IO (Either StoreError ())
startCluster config = do
    -- Reassert the no-TCP policy before every start. This also makes recovery
    -- safe if a process was interrupted after initdb wrote its defaults.
    writeManagedConfig config
    runCommand config "pg_ctl"
        [ "-D", config.postgresPaths.postgresDataDirectory
        , "-l", config.postgresPaths.postgresLogFile
        , "-w"
        , "-t", "30"
        , "start"
        ] >>= expectSuccess "pg_ctl start"

writeManagedConfig :: ManagedPostgresConfig -> IO ()
writeManagedConfig config = do
    writeFile
        (config.postgresPaths.postgresDataDirectory </> "postgresql.conf")
        (Text.unpack (postgresqlConf config))
    writeFile
        (config.postgresPaths.postgresDataDirectory </> "pg_hba.conf")
        (Text.unpack pgHbaConf)

ensureDatabase :: ManagedPostgresConfig -> IO (Either StoreError ())
ensureDatabase config = do
    queryResult <- runCommand config "psql"
        (connectionArguments config "postgres"
            <> [ "--tuples-only"
               , "--no-align"
               , "--command"
               , "SELECT 1 FROM pg_database WHERE datname = "
                    <> quoteSqlLiteral (Text.unpack config.postgresDatabase)
               ])
    case queryResult of
        Left err -> pure (Left err)
        Right (ExitSuccess, stdout, _)
            | any (== "1") (lines stdout) -> pure (Right ())
            | otherwise ->
                runCommand config "createdb"
                    [ "--host", config.postgresPaths.postgresSocketDirectory
                    , "--port", show config.postgresPort
                    , "--username", Text.unpack config.postgresOwnerRole
                    , "--owner", Text.unpack config.postgresOwnerRole
                    , Text.unpack config.postgresDatabase
                    ] >>= expectSuccess "createdb"
        Right result -> pure $ Left $ commandFailure "psql database check" result

connectionArguments
    :: ManagedPostgresConfig
    -> String
    -> [String]
connectionArguments config database =
    [ "--host", config.postgresPaths.postgresSocketDirectory
    , "--port", show config.postgresPort
    , "--username", Text.unpack config.postgresOwnerRole
    , "--dbname", database
    ]

quoteSqlLiteral :: String -> String
quoteSqlLiteral value =
    "'" <> concatMap escape value <> "'"
  where
    escape '\'' = "''"
    escape character = [character]

runCommand
    :: ManagedPostgresConfig
    -> FilePath
    -> [String]
    -> IO (Either StoreError (ExitCode, String, String))
runCommand config executable arguments =
    try
        (readProcessWithExitCode
            (postgresExecutable config executable)
            arguments
            "") >>= \case
                Left (exception :: SomeException) -> pure $ Left $ StoreProcessError $
                    "Could not run " <> Text.pack executable <> ": "
                        <> Text.pack (show exception)
                Right result -> pure (Right result)

expectSuccess
    :: Text
    -> Either StoreError (ExitCode, String, String)
    -> IO (Either StoreError ())
expectSuccess label = \case
    Left err -> pure (Left err)
    Right (ExitSuccess, _, _) -> pure (Right ())
    Right result -> pure (Left (commandFailure label result))

commandFailure :: Text -> (ExitCode, String, String) -> StoreError
commandFailure label (exitCode, stdout, stderr) =
    StoreProcessError $
        label <> " failed (" <> Text.pack (show exitCode) <> "): "
            <> cleanOutput stderr stdout

cleanOutput :: String -> String -> Text
cleanOutput preferred fallback =
    let output = if null preferred then fallback else preferred
    in Text.strip (Text.pack output)
