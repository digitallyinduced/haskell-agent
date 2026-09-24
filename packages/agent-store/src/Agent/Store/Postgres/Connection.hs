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
    , postgresApplicationName
    , formatPostgresApplicationName
    , postgresApplicationNameFromEnvironment
    , connectionSettingsForRole
    , openStorePool
    , openStorePoolWithConnectionTimeout
    , openRoleStorePool
    , closeStorePool
    , openStoreConnection
    , closeStoreConnection
    , withStoreConnectionFailureMonitor
    , waitForStoreConnectionFailure
    , withConnectionSession
    , withStorePool
    , withSession
    , withSessionSingleAttempt
    , runSession
    , runTransaction
    , ReconnectionPolicy(..)
    , defaultReconnectionPolicy
    , noReconnectionPolicy
    , isTransientUsageError
    , retryTransientUsageErrors
    ) where

import Control.Concurrent (threadDelay)
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
    , mask
    , onException
    , tryAny
    )
import Control.Monad.Except (catchError, throwError)
import Data.Char (isAlphaNum, isAscii)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (DiffTime)
import GHC.Conc (threadWaitReadSTM)
import System.Environment (lookupEnv)
import qualified Hasql.Connection as Connection
import qualified Hasql.Connection.Settings as ConnectionSettings
import qualified Hasql.Errors as Errors
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
    , storePoolReconnectionPolicy :: !ReconnectionPolicy
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
    , poolReconnectionPolicy :: !ReconnectionPolicy
    }
    deriving (Eq, Show)

defaultPoolConfig :: PoolConfig
defaultPoolConfig = PoolConfig
    { poolSize = 8
    , poolAcquisitionTimeout = 10
    , poolAgingTimeout = 60 * 60
    , poolIdlenessTimeout = 60
    , poolReconnectionPolicy = defaultReconnectionPolicy
    }

{- | Delays, in microseconds, between successive attempts to run a pooled
session after an attempt failed before the server could act on it.

The policy is exhausted together with the list: an empty list permits exactly
one attempt.
-}
newtype ReconnectionPolicy = ReconnectionPolicy
    { reconnectionDelays :: [Int]
    }
    deriving (Eq, Show)

{- | Wait out a routine PostgreSQL restart.

The first retries follow closely so a fast restart costs little; later ones
are spaced further apart. The attempts span about one minute in total, after
which the last transient error is reported to the caller.
-}
defaultReconnectionPolicy :: ReconnectionPolicy
defaultReconnectionPolicy = ReconnectionPolicy
    { reconnectionDelays = map seconds ([1, 2, 3, 4] <> replicate 10 5)
    }
  where
    seconds :: Int -> Int
    seconds = (* 1000000)

-- | Report the first failure immediately.
noReconnectionPolicy :: ReconnectionPolicy
noReconnectionPolicy = ReconnectionPolicy { reconnectionDelays = [] }

-- | Product prefix of PostgreSQL @application_name@.
postgresApplicationName :: Text
postgresApplicationName = "haskell-agent"

-- | PostgreSQL stores at most @NAMEDATALEN - 1@ bytes in @application_name@.
postgresApplicationNameLimit :: Int
postgresApplicationNameLimit = 63

-- | Build @application_name@ from a build commit.
--
-- Missing or invalid commits become @development@. The result is printable
-- ASCII and at most 63 bytes, which is PostgreSQL's @application_name@ limit.
formatPostgresApplicationName :: Text -> Text
formatPostgresApplicationName rawCommit =
    Text.take postgresApplicationNameLimit
        (postgresApplicationName <> "/" <> commit)
  where
    commit
        | not (Text.null rawCommit)
        , Text.all validCommitCharacter rawCommit
        = rawCommit
        | otherwise = "development"

validCommitCharacter :: Char -> Bool
validCommitCharacter character =
    isAscii character
        && (isAlphaNum character || character `elem` ("-._" :: String))

-- | Read @AGENT_BUILD_COMMIT@, the same revision the CLI and native runtime
-- export for gateway identity. This stays an environment lookup so agent-store
-- is not rebuilt on every commit.
postgresApplicationNameFromEnvironment :: IO Text
postgresApplicationNameFromEnvironment = do
    value <- lookupEnv "AGENT_BUILD_COMMIT"
    pure $ formatPostgresApplicationName (maybe "" Text.pack value)

connectionSettingsForRole
    :: ManagedPostgresConfig
    -> Text
    -> ConnectionSettings.Settings
connectionSettingsForRole config role =
    connectionSettingsForRoleWithTimeout
        config
        role
        10
        postgresApplicationName

connectionSettingsForRoleWithTimeout
    :: ManagedPostgresConfig
    -> Text
    -> Int
    -> Text
    -> ConnectionSettings.Settings
connectionSettingsForRoleWithTimeout
    config
    role
    timeoutSeconds
    applicationName =
    ConnectionSettings.hostAndPort
        (Text.pack config.postgresPaths.postgresSocketDirectory)
        config.postgresPort
        <> ConnectionSettings.user role
        <> ConnectionSettings.dbname config.postgresDatabase
        <> ConnectionSettings.other
            "connect_timeout"
            (Text.pack (show timeoutSeconds))
        <> ConnectionSettings.applicationName applicationName

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
    applicationName <- postgresApplicationNameFromEnvironment
    let settings =
            connectionSettingsForRoleWithTimeout
                config
                role
                timeoutSeconds
                applicationName
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
            , storePoolReconnectionPolicy = options.poolReconnectionPolicy
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
    bracket
        (tryAny prepareMonitor)
        (\case
            Right (Right (_, unregister)) -> unregister
            _ -> pure ())
        \case
            Left err ->
                pure . Left . StoreConnectionError $
                    "PostgreSQL connection monitor failed: "
                        <> Text.pack (displayException err)
            Right (Left err) -> pure (Left err)
            Right (Right (waitForFailure, _)) -> do
                alreadyFailed <-
                    atomically $
                        (True <$ waitForFailure)
                            `orElse` pure False
                if alreadyFailed
                    then
                        pure . Left . StoreConnectionError $
                            "PostgreSQL connection socket was already readable"
                    else
                        Right <$> action (atomically waitForFailure)
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
    bracket
        (openStorePool config options)
        (either (const (pure ())) closeStorePool)
        (either (pure . Left) action)

{- | Check out a pooled connection for one Hasql session and return it
automatically when the session finishes or fails.

A failure to reach the server, or a connection that the server closed, is
retried under the pool's 'ReconnectionPolicy' so that a routine PostgreSQL
restart stalls the caller instead of failing it. Only errors that Hasql
classifies as transient are retried; server-side statement errors and pool
acquisition timeouts are reported at once.
-}
withSession
    :: StorePool
    -> Session.Session a
    -> IO (Either StoreError a)
withSession pool = withSessionUnderPolicy pool.storePoolReconnectionPolicy pool

{- | Run one pooled session without waiting for the server to return.

Use this where a failure must be reported promptly, such as the warm-start
probe that falls back to lifecycle management.
-}
withSessionSingleAttempt
    :: StorePool
    -> Session.Session a
    -> IO (Either StoreError a)
withSessionSingleAttempt = withSessionUnderPolicy noReconnectionPolicy

withSessionUnderPolicy
    :: ReconnectionPolicy
    -> StorePool
    -> Session.Session a
    -> IO (Either StoreError a)
withSessionUnderPolicy policy pool session =
    retryTransientUsageErrors policy discardIdleConnectionsAndWait attempt >>= \case
        Left err -> pure $ Left $ StoreConnectionError $
            "PostgreSQL session failed: " <> Text.pack (show err)
        Right value -> pure (Right value)
  where
    attempt =
        Pool.use (storePool pool) (verifyConnectionAfterFailure session)
    -- A restarted server closed every connection established before the
    -- failure. Discarding the idle ones lets the retry connect afresh instead
    -- of failing once per stale pooled connection; the pool stays usable.
    discardIdleConnectionsAndWait delay = do
        Pool.release (storePool pool)
        threadDelay delay

{- | Distinguish a rejected statement from a connection the server closed.

A fast shutdown leaves a termination notice on every open connection. Hasql
reports the statement that first reads that notice as a decoding failure or
as an empty server error rather than as a connection loss, so the pool would
return the dead connection for reuse and the caller would see a permanent
failure. After any failure that Hasql does not already consider transient, a
probe on the same connection settles the question: a probe the connection
cannot deliver replaces the original error, so the pool discards the
connection and the reconnection policy retries the session. Otherwise the
original error stands.
-}
verifyConnectionAfterFailure :: Session.Session a -> Session.Session a
verifyConnectionAfterFailure session =
    session `catchError` \sessionError ->
        if Errors.isTransient sessionError
            then throwError sessionError
            else do
                probeOutcome <-
                    (Right <$> Session.script "SELECT 1")
                        `catchError` (pure . Left)
                throwError case probeOutcome of
                    Left probeError
                        | Errors.isTransient probeError -> probeError
                    _ -> sessionError

{- | Whether a pool usage error permits running the same session again.

Hasql marks an error transient when the operation can be retried on a new
connection: the server could not be reached, or the connection was lost. Such
failures precede any effect of the session on the server, or reach a session
whose open transaction the server rolled back with the connection.
-}
isTransientUsageError :: Pool.UsageError -> Bool
isTransientUsageError = \case
    Pool.ConnectionUsageError err -> Errors.isTransient err
    Pool.SessionUsageError err -> Errors.isTransient err
    Pool.AcquisitionTimeoutUsageError -> False

{- | Repeat an attempt while it fails transiently and the policy has delays
left. The supplied action receives each scheduled delay, in microseconds,
before the corresponding retry. The last transient error is returned when the
policy is exhausted.
-}
retryTransientUsageErrors
    :: ReconnectionPolicy
    -> (Int -> IO ())
    -> IO (Either Pool.UsageError a)
    -> IO (Either Pool.UsageError a)
retryTransientUsageErrors policy beforeRetry attempt =
    go policy.reconnectionDelays
  where
    go delays =
        attempt >>= \case
            Left err
                | isTransientUsageError err
                , delay : remaining <- delays -> do
                    beforeRetry delay
                    go remaining
            result -> pure result

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
