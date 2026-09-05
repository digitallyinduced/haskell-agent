{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Filesystem and server configuration for the private PostgreSQL cluster.
module Agent.Store.Postgres.Config
    ( ManagedPostgresPaths(..)
    , ManagedPostgresConfig(..)
    , defaultManagedPostgresConfig
    , managedPostgresConfigFromEnv
    , serverTurnActionLockDirectory
    , postgresExecutable
    , postgresSocketPath
    , socketHost
    , postgresqlConf
    , pgHbaConf
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word16)
import Numeric (showHex)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Text.Read (readMaybe)

data ManagedPostgresPaths = ManagedPostgresPaths
    { postgresRootDirectory :: !FilePath
    , postgresDataDirectory :: !FilePath
    , postgresSocketDirectory :: !FilePath
    , postgresLogFile :: !FilePath
    , postgresLifecycleLockFile :: !FilePath
    , postgresServerTurnActionLockDirectory :: !FilePath
    }
    deriving (Eq, Show)

data ManagedPostgresConfig = ManagedPostgresConfig
    { postgresPaths :: !ManagedPostgresPaths
    , postgresBinDirectory :: !FilePath
    , postgresPort :: !Word16
    , postgresDatabase :: !Text
    , postgresOwnerRole :: !Text
    , postgresMaxConnections :: !Int
    }
    deriving (Eq, Show)

-- | Construct the default cluster below an agent state directory.
--
-- The second argument is the PostgreSQL @bin@ directory. An empty value uses
-- the executable names from @PATH@.
defaultManagedPostgresConfig
    :: FilePath
    -> FilePath
    -> ManagedPostgresConfig
defaultManagedPostgresConfig stateDirectory binDirectory =
    ManagedPostgresConfig
        { postgresPaths = ManagedPostgresPaths
            { postgresRootDirectory = root
            , postgresDataDirectory = root </> "data"
            , postgresSocketDirectory = root </> "run"
            , postgresLogFile = root </> "postgres.log"
            , postgresLifecycleLockFile = root </> "lifecycle.lock"
            , postgresServerTurnActionLockDirectory =
                root </> "server-turn-actions"
            }
        , postgresBinDirectory = binDirectory
        , postgresPort = 55432
        , postgresDatabase = "haskell_agent"
        , postgresOwnerRole = "ha_owner"
        , postgresMaxConnections = 32
        }
  where
    root = stateDirectory </> "postgres"

-- | Read optional process-level overrides.
--
-- @AGENT_POSTGRES_BIN@ names the PostgreSQL bin directory and
-- @AGENT_POSTGRES_PORT@ overrides the private server port.
managedPostgresConfigFromEnv :: FilePath -> IO ManagedPostgresConfig
managedPostgresConfigFromEnv stateDirectory = do
    binDirectory <- maybe "" id <$> lookupEnv "AGENT_POSTGRES_BIN"
    portValue <- lookupEnv "AGENT_POSTGRES_PORT"
    let config = defaultManagedPostgresConfig stateDirectory binDirectory
    pure case portValue >>= readMaybe of
        Just port -> config { postgresPort = port }
        Nothing -> config

-- | Isolate host-level owner fences by the exact database in one cluster.
serverTurnActionLockDirectory :: ManagedPostgresConfig -> FilePath
serverTurnActionLockDirectory config =
    config.postgresPaths.postgresServerTurnActionLockDirectory
        </> "database-" <> databaseNameHex
  where
    databaseNameHex =
        concatMap
            encodeByte
            (ByteString.unpack (Text.encodeUtf8 config.postgresDatabase))
    encodeByte byte
        | byte < 16 = '0' : showHex byte ""
        | otherwise = showHex byte ""

postgresExecutable :: ManagedPostgresConfig -> FilePath -> FilePath
postgresExecutable config executable
    | null config.postgresBinDirectory = executable
    | otherwise = config.postgresBinDirectory </> executable

-- | Filesystem path used by PostgreSQL for this cluster's Unix socket.
postgresSocketPath :: ManagedPostgresConfig -> FilePath
postgresSocketPath config =
    config.postgresPaths.postgresSocketDirectory
        </> (".s.PGSQL." <> show config.postgresPort)

socketHost :: ManagedPostgresConfig -> ByteString
socketHost config =
    Text.encodeUtf8 (Text.pack config.postgresPaths.postgresSocketDirectory)

postgresqlConf :: ManagedPostgresConfig -> Text
postgresqlConf config = Text.unlines
    [ "# Managed by haskell-agent. Local Unix socket access only."
    , "listen_addresses = ''"
    , "unix_socket_directories = "
        <> quoteSetting (Text.pack config.postgresPaths.postgresSocketDirectory)
    , "unix_socket_permissions = 0700"
    , "port = " <> Text.pack (show config.postgresPort)
    , "max_connections = " <> Text.pack (show config.postgresMaxConnections)
    , "password_encryption = 'scram-sha-256'"
    , "fsync = on"
    , "synchronous_commit = on"
    ]

pgHbaConf :: Text
pgHbaConf = Text.unlines
    [ "# Managed by haskell-agent. The socket directory is mode 0700."
    , "local all all trust"
    ]

quoteSetting :: Text -> Text
quoteSetting value =
    "'" <> Text.replace "'" "''" value <> "'"
