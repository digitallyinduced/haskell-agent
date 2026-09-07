module Agent.Store.Postgres
    ( Store
    , ManagedPostgresConfig(..)
    , ManagedPostgresPaths(..)
    , ManagedPostgresStatus(..)
    , StoreError(..)
    , defaultManagedPostgresConfig
    , managedPostgresConfigFromEnv
    , openStore
    , openStoreWithRuntimeRole
    , openStartupOwnerPool
    , closeStore
    , withStore
    , storeConfig
    , provisioningPool
    , trustedPool
    , scopePool
    , normalizePostgresTimestamp
    ) where

import Control.Concurrent.MVar
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM
import qualified Control.Exception as Exception
import Control.Exception.Safe (bracket, mask, onException, tryAny)
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock
    ( UTCTime(..)
    , diffTimeToPicoseconds
    , picosecondsToDiffTime
    )
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement
import System.Directory (canonicalizePath)

import Agent.Store.Postgres.Config
import Agent.Store.Postgres.Connection
import Agent.Store.Postgres.Managed
import Agent.Store.Postgres.Migrations
import Agent.Store.PoolCache
import Agent.Store.Types

data Store = Store
    { storeConfigInternal :: !ManagedPostgresConfig
    , provisioningPoolInternal :: !StorePool
    , trustedPoolInternal :: !StorePool
    , scopePoolsInternal :: !(PoolCache Text StoreError StorePool)
    , storeCloseInternal :: !(MVar StoreCloseState)
    }

data StoreCloseState
    = StoreOpen
    | StoreClosing !(TMVar (Either Exception.SomeException ()))
    | StoreClosed !(Either Exception.SomeException ())

openStore :: ManagedPostgresConfig -> IO (Either StoreError Store)
openStore config =
    openStoreWithRuntimeRole config "ha_runtime"

openStoreWithRuntimeRole
    :: ManagedPostgresConfig
    -> Text
    -> IO (Either StoreError Store)
openStoreWithRuntimeRole config runtimeRole = mask \restore ->
    restore (openStartupOwnerPool config) >>= \case
        Left err -> pure (Left err)
        Right ownerPool -> do
            migrationResult <-
                restore
                    (runCoreMigrationsForRuntimeRole ownerPool runtimeRole)
                    `onException` closeStorePool ownerPool
            case migrationResult of
                Left err ->
                    closeStorePool ownerPool >> pure (Left err)
                Right () -> do
                    runtimeResult <-
                        restore
                            (openRoleStorePool
                                config
                                runtimeRole
                                defaultPoolConfig)
                            `onException` closeStorePool ownerPool
                    case runtimeResult of
                        Left err -> do
                            closeStorePool ownerPool
                            pure (Left err)
                        Right runtimePool -> do
                            pools <- newPoolCache
                                8
                                storeClosedError
                                storeExceptionError
                                (\role ->
                                    openRoleStorePool
                                        config
                                        role
                                        (defaultPoolConfig
                                            { poolSize = 2
                                            }))
                                closeStorePool
                            closeState <- newMVar StoreOpen
                            pure $ Right Store
                                { storeConfigInternal = config
                                , provisioningPoolInternal = ownerPool
                                , trustedPoolInternal = runtimePool
                                , scopePoolsInternal = pools
                                , storeCloseInternal = closeState
                                }

-- | Reuse a direct owner connection when the managed cluster is already
-- running. The socket check avoids paying even the short connection timeout
-- on cold startup; any failed optimistic connection falls back to the existing
-- lifecycle lock and provisioning commands.
openStartupOwnerPool
    :: ManagedPostgresConfig
    -> IO (Either StoreError StorePool)
openStartupOwnerPool config =
    prepareManagedPostgres config >>= \case
        Left err -> pure (Left err)
        Right socketExists
            | socketExists ->
                mask \restore -> do
                    connectionResult <- restore $
                        openStorePoolWithConnectionTimeout
                            config
                            1
                            defaultPoolConfig
                    case connectionResult of
                        Right pool -> do
                            matches <-
                                restore (warmPoolMatchesConfig config pool)
                                    `onException` closeStorePool pool
                            if matches
                                then pure (Right pool)
                                else
                                    closeStorePool pool
                                        >> restore ensureAndOpen
                        Left _ -> restore ensureAndOpen
            | otherwise -> ensureAndOpen
  where
    ensureAndOpen =
        ensureManagedPostgres config >>= \case
            Left err -> pure (Left err)
            Right _ -> openStorePool config defaultPoolConfig

-- | A socket path can outlive its intended configuration or be shared by a
-- different PostgreSQL instance. Before bypassing lifecycle management, prove
-- that the connected server is backed by this configuration's data directory.
warmPoolMatchesConfig :: ManagedPostgresConfig -> StorePool -> IO Bool
warmPoolMatchesConfig config pool =
    withSession
        pool
        (Session.statement () managedDataDirectoryStatement) >>= \case
        Left _ -> pure False
        Right actualDirectory -> do
            expectedResult <- tryAny $
                canonicalizePath
                    config.postgresPaths.postgresDataDirectory
            actualResult <- tryAny $
                canonicalizePath (Text.unpack actualDirectory)
            pure case (expectedResult, actualResult) of
                (Right expected, Right actual) -> expected == actual
                _ -> False

managedDataDirectoryStatement :: Statement.Statement () Text
managedDataDirectoryStatement = Statement.preparable
    "SELECT current_setting('data_directory')"
    Encoders.noParams
    (Decoders.singleRow
        (Decoders.column (Decoders.nonNullable Decoders.text)))

closeStore :: Store -> IO ()
closeStore store =
    Exception.mask \restore -> do
        decision <- modifyMVar store.storeCloseInternal \case
            state@(StoreClosed outcome) ->
                pure (state, Left outcome)
            state@(StoreClosing completion) ->
                pure (state, Right (Left completion))
            StoreOpen -> do
                completion <- newEmptyTMVarIO
                pure
                    ( StoreClosing completion
                    , Right (Right completion)
                    )
        case decision of
            Left outcome -> replayStoreClose outcome
            Right (Left completion) ->
                restore (atomically (readTMVar completion))
                    >>= replayStoreClose
            Right (Right completion) -> do
                outcome <-
                    tryAnyException (restore (closeAllStorePools store))
                modifyMVar_ store.storeCloseInternal \_ ->
                    pure (StoreClosed outcome)
                atomically $ void (tryPutTMVar completion outcome)
                replayStoreClose outcome

-- | Open a store for the duration of an action and always release every pool.
withStore
    :: ManagedPostgresConfig
    -> (Store -> IO a)
    -> IO (Either StoreError a)
withStore config action =
    openStore config >>= \case
        Left err -> pure (Left err)
        Right store ->
            bracket
                (pure store)
                closeStore
                (fmap Right . action)

storeConfig :: Store -> ManagedPostgresConfig
storeConfig = (.storeConfigInternal)

-- | Owner-authenticated pool reserved for role and schema provisioning.
provisioningPool :: Store -> StorePool
provisioningPool = (.provisioningPoolInternal)

-- | Restricted harness runtime pool for normal persistence operations.
trustedPool :: Store -> StorePool
trustedPool = (.trustedPoolInternal)

scopePool :: Store -> Text -> IO (Either StoreError StorePool)
scopePool store role =
    acquirePoolCache store.scopePoolsInternal role

closeAllStorePools :: Store -> IO ()
closeAllStorePools store = do
    outcomes <- mapConcurrently
        tryAnyException
        [ closePoolCache store.scopePoolsInternal
        , closeStorePool store.trustedPoolInternal
        , closeStorePool store.provisioningPoolInternal
        ]
    case [exception | Left exception <- outcomes] of
        exception : _ -> Exception.throwIO exception
        [] -> pure ()

replayStoreClose :: Either Exception.SomeException () -> IO ()
replayStoreClose = either Exception.throwIO pure

tryAnyException
    :: IO a
    -> IO (Either Exception.SomeException a)
tryAnyException = Exception.try

storeClosedError :: StoreError
storeClosedError =
    StoreConnectionError "PostgreSQL store is closed"

storeExceptionError :: Exception.SomeException -> StoreError
storeExceptionError exception =
    StoreConnectionError
        ("PostgreSQL scope pool initialization failed: "
            <> Text.pack (Exception.displayException exception))

-- | Match PostgreSQL's @timestamptz@ microsecond precision before retaining
-- timestamps in application state.
normalizePostgresTimestamp :: UTCTime -> UTCTime
normalizePostgresTimestamp (UTCTime day dayTime) =
    UTCTime day
        (picosecondsToDiffTime
            (diffTimeToPicoseconds dayTime `div` picosecondsPerMicrosecond
                * picosecondsPerMicrosecond))
  where
    picosecondsPerMicrosecond = 1000000
