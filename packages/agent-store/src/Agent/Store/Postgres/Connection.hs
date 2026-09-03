{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Hasql pools for trusted harness and scope-role connections.
module Agent.Store.Postgres.Connection
    ( StorePool
    , storePool
    , StoreConnection
    , PoolConfig(..)
    , defaultPoolConfig
    , connectionSettingsForRole
    , openStorePool
    , openRoleStorePool
    , closeStorePool
    , openStoreConnection
    , closeStoreConnection
    , withConnectionSession
    , withStorePool
    , withSession
    , runSession
    , runTransaction
    ) where

import Control.Exception.Safe (bracket, mask, onException)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (DiffTime)
import qualified Hasql.Connection as Connection
import qualified Hasql.Connection.Settings as ConnectionSettings
import qualified Hasql.Pool as Pool
import qualified Hasql.Pool.Config as PoolConfig
import qualified Hasql.Session as Session
import qualified Hasql.Transaction as Transaction
import qualified Hasql.Transaction.Sessions as Transactions
import qualified Pqi.Ffi as Pqi

import Agent.Store.Postgres.Config
import Agent.Store.Types

data StorePool = StorePool
    { storePoolInternal :: !Pool.Pool
    , storePoolConnectionSettings :: !ConnectionSettings.Settings
    }

storePool :: StorePool -> Pool.Pool
storePool = (.storePoolInternal)

newtype StoreConnection = StoreConnection Connection.Connection

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
    ConnectionSettings.hostAndPort
        (Text.pack config.postgresPaths.postgresSocketDirectory)
        config.postgresPort
        <> ConnectionSettings.user role
        <> ConnectionSettings.dbname config.postgresDatabase
        <> ConnectionSettings.other "connect_timeout" "10"
        <> ConnectionSettings.applicationName "haskell-agent"

openStorePool
    :: ManagedPostgresConfig
    -> PoolConfig
    -> IO (Either StoreError StorePool)
openStorePool config =
    openRoleStorePool config config.postgresOwnerRole

openRoleStorePool
    :: ManagedPostgresConfig
    -> Text
    -> PoolConfig
    -> IO (Either StoreError StorePool)
openRoleStorePool config role options = mask \restore -> do
    let settings = connectionSettingsForRole config role
    pool <- Pool.acquire Pqi.adapter $ PoolConfig.settings
        [ PoolConfig.size options.poolSize
        , PoolConfig.acquisitionTimeout options.poolAcquisitionTimeout
        , PoolConfig.agingTimeout options.poolAgingTimeout
        , PoolConfig.idlenessTimeout options.poolIdlenessTimeout
        , PoolConfig.staticConnectionSettings settings
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
        Right () -> pure $ Right StorePool
            { storePoolInternal = pool
            , storePoolConnectionSettings = settings
            }

closeStorePool :: StorePool -> IO ()
closeStorePool = Pool.release . storePool

{- | Open a connection outside the reusable pool.

This is reserved for connection-lifetime PostgreSQL leases. A session-level
advisory lock must never be returned to the pool where another request
could inherit it.
-}
openStoreConnection :: StorePool -> IO (Either StoreError StoreConnection)
openStoreConnection pool =
    Connection.acquire
        Pqi.adapter
        pool.storePoolConnectionSettings
        >>= \case
            Left err ->
                pure . Left . StoreConnectionError $
                    "Could not open dedicated PostgreSQL connection: "
                        <> Text.pack (show err)
            Right connection ->
                pure (Right (StoreConnection connection))

closeStoreConnection :: StoreConnection -> IO ()
closeStoreConnection (StoreConnection connection) =
    Connection.release connection

withConnectionSession ::
    StoreConnection ->
    Session.Session a ->
    IO (Either StoreError a)
withConnectionSession (StoreConnection connection) session =
    Connection.use connection session >>= \case
        Left err ->
            pure . Left . StoreConnectionError $
                "Dedicated PostgreSQL session failed: "
                    <> Text.pack (show err)
        Right value -> pure (Right value)

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
