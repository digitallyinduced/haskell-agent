{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Hasql pools for trusted harness and scope-role connections.
module Agent.Store.Postgres.Connection
    ( StorePool
    , storePool
    , storePoolServerTurnActionLockDirectory
    , StoreConnection
    , PoolConfig(..)
    , defaultPoolConfig
    , connectionSettingsForRole
    , openStorePool
    , openRoleStorePool
    , closeStorePool
    , openStoreConnection
    , closeStoreConnection
    , withStoreConnectionFailureMonitor
    , waitForStoreConnectionFailure
    , withConnectionSession
    , withStorePool
    , withSession
    , runSession
    , runTransaction
    ) where

import Control.Concurrent.MVar
    ( newEmptyMVar
    , putMVar
    , takeMVar
    , tryTakeMVar
    )
import Control.Concurrent.STM (atomically, orElse)
import Control.Exception.Safe
    ( bracket
    , displayException
    , finally
    , mask
    , onException
    , tryAny
    )
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (DiffTime)
import GHC.Conc (threadWaitReadSTM)
import qualified Hasql.Connection as Connection
import qualified Hasql.Connection.Settings as ConnectionSettings
import qualified Hasql.Pool as Pool
import qualified Hasql.Pool.Config as PoolConfig
import qualified Hasql.Session as Session
import qualified Hasql.Transaction as Transaction
import qualified Hasql.Transaction.Sessions as Transactions
import qualified Pqi
import qualified Pqi.Ffi as PqiFfi

import Agent.Store.Postgres.Config
import Agent.Store.Types

data StorePool = StorePool
    { storePoolInternal :: !Pool.Pool
    , storePoolConnectionSettings :: !ConnectionSettings.Settings
    , storePoolServerTurnActionLockDirectoryInternal :: !FilePath
    }

storePool :: StorePool -> Pool.Pool
storePool = (.storePoolInternal)

storePoolServerTurnActionLockDirectory :: StorePool -> FilePath
storePoolServerTurnActionLockDirectory =
    (.storePoolServerTurnActionLockDirectoryInternal)

data StoreConnection = StoreConnection
    { storeConnectionHasql :: !Connection.Connection
    , storeConnectionDriver :: !Pqi.Connection
    }

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
    pool <- Pool.acquire PqiFfi.adapter $ PoolConfig.settings
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
            , storePoolServerTurnActionLockDirectoryInternal =
                serverTurnActionLockDirectory config
            }

closeStorePool :: StorePool -> IO ()
closeStorePool = Pool.release . storePool

{- | Open a connection outside the reusable pool.

This is reserved for connection-lifetime PostgreSQL leases. A session-level
advisory lock must never be returned to the pool where another request
could inherit it.
-}
openStoreConnection :: StorePool -> IO (Either StoreError StoreConnection)
openStoreConnection pool = mask \restore -> do
    capturedConnection <- newEmptyMVar
    let baseAdapter = PqiFfi.adapter
        capturingAdapter =
            baseAdapter
                { Pqi.connectdb = \settings -> mask \restoreConnect -> do
                    connection <-
                        restoreConnect (Pqi.connectdb baseAdapter settings)
                    putMVar capturedConnection connection
                    pure connection
                }
        closeCapturedConnection =
            tryTakeMVar capturedConnection >>= \case
                Nothing -> pure ()
                Just connection -> Pqi.finish connection
    acquired <-
        restore
            (Connection.acquire capturingAdapter pool.storePoolConnectionSettings)
            `onException` closeCapturedConnection
    case acquired of
        Left err -> do
            closeCapturedConnection
            pure . Left . StoreConnectionError $
                "Could not open dedicated PostgreSQL connection: "
                    <> Text.pack (show err)
        Right connection -> do
            driverConnection <- takeMVar capturedConnection
            pure $ Right StoreConnection
                { storeConnectionHasql = connection
                , storeConnectionDriver = driverConnection
                }

closeStoreConnection :: StoreConnection -> IO ()
closeStoreConnection connection =
    Connection.release connection.storeConnectionHasql

{- | Arm a monitor for the exact PostgreSQL socket underlying this connection
before invoking the supplied action.

A dedicated idle lease connection receives no application data, so socket
readiness means PostgreSQL has closed or invalidated the connection. The GHC
event-manager registration is installed synchronously before the action starts.
The supplied wait is asynchronously interruptible and may therefore be raced
against the guarded action without delaying normal completion.
-}
withStoreConnectionFailureMonitor ::
    StoreConnection ->
    (IO () -> IO a) ->
    IO (Either StoreError a)
withStoreConnectionFailureMonitor connection action =
    mask \restore -> do
        prepared <- tryAny prepareMonitor
        case prepared of
            Left err ->
                pure . Left . StoreConnectionError $
                    "PostgreSQL connection monitor failed: "
                        <> Text.pack (displayException err)
            Right (Left err) -> pure (Left err)
            Right (Right (waitForFailure, unregister)) -> do
                alreadyFailed <-
                    atomically $
                        (True <$ waitForFailure)
                            `orElse` pure False
                if alreadyFailed
                    then do
                        unregister
                        pure . Left . StoreConnectionError $
                            "PostgreSQL connection socket was already readable"
                    else
                        restore (Right <$> action (atomically waitForFailure))
                            `finally` unregister
  where
    driverConnection = connection.storeConnectionDriver
    prepareMonitor =
        Pqi.status driverConnection >>= \case
            Pqi.ConnectionOk ->
                Pqi.socket driverConnection >>= \case
                    Nothing ->
                        pure . Left . StoreConnectionError $
                            "PostgreSQL connection has no monitorable socket"
                    Just socket -> do
                        (waitForFailure, unregister) <-
                            threadWaitReadSTM socket
                        pure (Right (waitForFailure, unregister))
            status ->
                pure . Left . StoreConnectionError $
                    "PostgreSQL connection is unavailable: "
                        <> Text.pack (show status)

-- | Wait until the monitored PostgreSQL connection fails.
waitForStoreConnectionFailure ::
    StoreConnection ->
    IO (Either StoreError ())
waitForStoreConnectionFailure connection =
    withStoreConnectionFailureMonitor connection id

withConnectionSession
    :: StoreConnection
    -> Session.Session a
    -> IO (Either StoreError a)
withConnectionSession connection session =
    Connection.use connection.storeConnectionHasql session >>= \case
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
