{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Hasql pools for trusted harness and scope-role connections.
module Agent.Store.Postgres.Connection
    ( StorePool
    , storePool
    , PoolConfig(..)
    , defaultPoolConfig
    , connectionSettingsForRole
    , openStorePool
    , openStorePoolWithConnectionTimeout
    , openRoleStorePool
    , closeStorePool
    , withStorePool
    , withSession
    , runSession
    , runTransaction
    ) where

import Control.Exception.Safe (bracket, mask, onException)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (DiffTime)
import qualified Hasql.Connection.Settings as ConnectionSettings
import qualified Hasql.Pool as Pool
import qualified Hasql.Pool.Config as PoolConfig
import qualified Hasql.Session as Session
import qualified Hasql.Transaction as Transaction
import qualified Hasql.Transaction.Sessions as Transactions
import qualified Pqi.Ffi as Pqi

import Agent.Store.Postgres.Config
import Agent.Store.Types

newtype StorePool = StorePool Pool.Pool

storePool :: StorePool -> Pool.Pool
storePool (StorePool pool) = pool

data PoolConfig = PoolConfig
    { poolSize :: !Int
    , poolAcquisitionTimeout :: !DiffTime
    , poolAgingTimeout :: !DiffTime
    , poolIdlenessTimeout :: !DiffTime
    }
    deriving (Eq, Show)

defaultPoolConfig :: PoolConfig
defaultPoolConfig = PoolConfig
    { poolSize = 8
    , poolAcquisitionTimeout = 10
    , poolAgingTimeout = 60 * 60
    , poolIdlenessTimeout = 60
    }

connectionSettingsForRole
    :: ManagedPostgresConfig
    -> Text
    -> ConnectionSettings.Settings
connectionSettingsForRole config role =
    connectionSettingsForRoleWithTimeout config role 10

connectionSettingsForRoleWithTimeout
    :: ManagedPostgresConfig
    -> Text
    -> Int
    -> ConnectionSettings.Settings
connectionSettingsForRoleWithTimeout config role timeoutSeconds =
    ConnectionSettings.hostAndPort
        (Text.pack config.postgresPaths.postgresSocketDirectory)
        config.postgresPort
        <> ConnectionSettings.user role
        <> ConnectionSettings.dbname config.postgresDatabase
        <> ConnectionSettings.other
            "connect_timeout"
            (Text.pack (show timeoutSeconds))
        <> ConnectionSettings.applicationName "haskell-agent"

openStorePool
    :: ManagedPostgresConfig
    -> PoolConfig
    -> IO (Either StoreError StorePool)
openStorePool config =
    openRoleStorePool config config.postgresOwnerRole

-- | Open the owner pool with a bounded libpq connection attempt. This is used
-- for an optimistic warm-start probe before lifecycle management takes over.
openStorePoolWithConnectionTimeout
    :: ManagedPostgresConfig
    -> Int
    -> PoolConfig
    -> IO (Either StoreError StorePool)
openStorePoolWithConnectionTimeout config timeoutSeconds =
    openRoleStorePoolWithConnectionTimeout
        config
        config.postgresOwnerRole
        timeoutSeconds

openRoleStorePool
    :: ManagedPostgresConfig
    -> Text
    -> PoolConfig
    -> IO (Either StoreError StorePool)
openRoleStorePool config role =
    openRoleStorePoolWithConnectionTimeout config role 10

openRoleStorePoolWithConnectionTimeout
    :: ManagedPostgresConfig
    -> Text
    -> Int
    -> PoolConfig
    -> IO (Either StoreError StorePool)
openRoleStorePoolWithConnectionTimeout
    config
    role
    timeoutSeconds
    options = mask \restore -> do
    pool <- Pool.acquire Pqi.adapter $ PoolConfig.settings
        [ PoolConfig.size options.poolSize
        , PoolConfig.acquisitionTimeout options.poolAcquisitionTimeout
        , PoolConfig.agingTimeout options.poolAgingTimeout
        , PoolConfig.idlenessTimeout options.poolIdlenessTimeout
        , PoolConfig.staticConnectionSettings
            (connectionSettingsForRoleWithTimeout
                config
                role
                timeoutSeconds)
        ]
    validationResult <-
        restore (Pool.use pool (pure ()))
            `onException` Pool.release pool
    case validationResult of
        Left err -> do
            Pool.release pool
            pure $ Left $ StoreConnectionError $
                "Could not connect to managed PostgreSQL: "
                    <> Text.pack (show err)
        Right () -> pure (Right (StorePool pool))

closeStorePool :: StorePool -> IO ()
closeStorePool = Pool.release . storePool

withStorePool
    :: ManagedPostgresConfig
    -> PoolConfig
    -> (StorePool -> IO (Either StoreError a))
    -> IO (Either StoreError a)
withStorePool config options action =
    openStorePool config options >>= \case
        Left err -> pure (Left err)
        Right pool -> bracket
            (pure pool)
            closeStorePool
            action

-- | Check out a pooled connection for one Hasql session and return it
-- automatically when the session finishes or fails.
withSession
    :: StorePool
    -> Session.Session a
    -> IO (Either StoreError a)
withSession pool session =
    Pool.use (storePool pool) session >>= \case
        Left err -> pure $ Left $ StoreConnectionError $
            "PostgreSQL session failed: " <> Text.pack (show err)
        Right value -> pure (Right value)

-- | Backwards-compatible name for 'withSession'.
runSession
    :: StorePool
    -> Session.Session a
    -> IO (Either StoreError a)
runSession = withSession

runTransaction
    :: StorePool
    -> Transactions.IsolationLevel
    -> Transactions.Mode
    -> Transaction.Transaction a
    -> IO (Either StoreError a)
runTransaction pool isolation mode transaction =
    withSession pool (Transactions.transaction isolation mode transaction)
