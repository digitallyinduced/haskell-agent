-- | Dispatch for top-level PostgreSQL lifecycle and diagnostics commands.
--
-- The concrete store package supplies the actions. Keeping this small boundary
-- separate from argument parsing lets the CLI expose the administrative
-- command surface before depending on a particular server manager.
module Agent.CLI.Database.Storage
    ( StorageCommandEnv(..)
    , postgresStorageCommandEnv
    , runStorageCommand
    ) where

import Agent.CLI.Options (StorageCommand(..))
import Agent.Store.Postgres
    ( ManagedPostgresConfig(..)
    , ManagedPostgresPaths(..)
    , ManagedPostgresStatus(..)
    , withStore
    )
import Agent.Store.Postgres.Connection
    ( closeStorePool
    , defaultPoolConfig
    , openStorePool
    )
import Agent.Store.Postgres.Managed
    ( managedPostgresStatus
    , stopManagedPostgres
    )
import Agent.Store.Types (renderStoreError)
import Data.Text (Text)
import qualified Data.Text as Text

data StorageCommandEnv = StorageCommandEnv
    { storageStatusAction :: !(IO (Either Text Text))
    , storageStartAction :: !(IO (Either Text Text))
    , storageStopAction :: !(IO (Either Text Text))
    , storageMigrateAction :: !(IO (Either Text Text))
    , storageDoctorAction :: !(IO (Either Text Text))
    }

runStorageCommand
    :: StorageCommandEnv
    -> StorageCommand
    -> IO (Either Text Text)
runStorageCommand env = \case
    StorageStatus -> env.storageStatusAction
    StorageStart -> env.storageStartAction
    StorageStop -> env.storageStopAction
    StorageMigrate -> env.storageMigrateAction
    StorageDoctor -> env.storageDoctorAction

-- | Administrative actions backed by the managed local PostgreSQL server.
postgresStorageCommandEnv :: ManagedPostgresConfig -> StorageCommandEnv
postgresStorageCommandEnv config = StorageCommandEnv
    { storageStatusAction =
        managedPostgresStatus config >>= \case
            Left err -> pure (Left (renderStoreError err))
            Right status -> pure (Right (renderStatus status))
    , storageStartAction =
        withStore config (\_ -> pure ()) >>= \case
            Left err -> pure (Left (renderStoreError err))
            Right () ->
                pure $
                    Right
                        "managed PostgreSQL is running and migrations are up to date"
    , storageStopAction =
        stopManagedPostgres config >>= \case
            Left err -> pure (Left (renderStoreError err))
            Right () -> pure (Right "managed PostgreSQL stopped")
    , storageMigrateAction =
        withStore config (\_ -> pure ()) >>= \case
            Left err -> pure (Left (renderStoreError err))
            Right () ->
                pure (Right "managed PostgreSQL migrations are up to date")
    , storageDoctorAction =
        managedPostgresStatus config >>= \case
            Left err -> pure (Left (renderStoreError err))
            Right PostgresNotInitialized ->
                pure (Left "managed PostgreSQL is not initialized; run `agent-cli storage start`")
            Right PostgresStopped ->
                pure (Left "managed PostgreSQL is stopped; run `agent-cli storage start`")
            Right PostgresRunning ->
                openStorePool config defaultPoolConfig >>= \case
                    Left err -> pure (Left (renderStoreError err))
                    Right pool -> do
                        closeStorePool pool
                        pure $ Right $
                            "managed PostgreSQL is healthy; socket: "
                                <> Text.pack
                                    config.postgresPaths.postgresSocketDirectory
    }

renderStatus :: ManagedPostgresStatus -> Text
renderStatus = \case
    PostgresNotInitialized -> "managed PostgreSQL is not initialized"
    PostgresStopped -> "managed PostgreSQL is stopped"
    PostgresRunning -> "managed PostgreSQL is running"
