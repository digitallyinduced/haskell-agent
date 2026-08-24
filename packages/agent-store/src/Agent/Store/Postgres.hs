module Agent.Store.Postgres
    ( Store
    , ManagedPostgresConfig(..)
    , ManagedPostgresPaths(..)
    , ManagedPostgresStatus(..)
    , StoreError(..)
    , defaultManagedPostgresConfig
    , managedPostgresConfigFromEnv
    , openStore
    , closeStore
    , withStore
    , storeConfig
    , provisioningPool
    , trustedPool
    , scopePool
    , normalizePostgresTimestamp
    ) where

import Control.Concurrent.MVar
import Control.Exception.Safe (bracket, mask, onException)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Clock
    ( UTCTime(..)
    , diffTimeToPicoseconds
    , picosecondsToDiffTime
    )

import Agent.Store.Postgres.Config
import Agent.Store.Postgres.Connection
import Agent.Store.Postgres.Managed
import Agent.Store.Postgres.Migrations
import Agent.Store.Types

data Store = Store
    { storeConfigInternal :: !ManagedPostgresConfig
    , provisioningPoolInternal :: !StorePool
    , trustedPoolInternal :: !StorePool
    , scopePoolsInternal :: !(MVar (Map Text StorePool))
    }

openStore :: ManagedPostgresConfig -> IO (Either StoreError Store)
openStore config = mask \restore ->
    restore (ensureManagedPostgres config) >>= \case
        Left err -> pure (Left err)
        Right _ -> do
            restore (openStorePool config defaultPoolConfig) >>= \case
                Left err -> pure (Left err)
                Right ownerPool -> do
                    migrationResult <-
                        restore (runCoreMigrations ownerPool)
                            `onException` closeStorePool ownerPool
                    case migrationResult of
                        Left err ->
                            closeStorePool ownerPool >> pure (Left err)
                        Right () -> do
                            runtimeResult <-
                                restore
                                    (openRoleStorePool
                                        config
                                        "ha_runtime"
                                        defaultPoolConfig)
                                    `onException` closeStorePool ownerPool
                            case runtimeResult of
                                    Left err -> do
                                        closeStorePool ownerPool
                                        pure (Left err)
                                    Right runtimePool -> do
                                        pools <- newMVar Map.empty
                                        pure $ Right Store
                                            { storeConfigInternal = config
                                            , provisioningPoolInternal =
                                                ownerPool
                                            , trustedPoolInternal = runtimePool
                                            , scopePoolsInternal = pools
                                            }

closeStore :: Store -> IO ()
closeStore store = do
    pools <- takeMVar store.scopePoolsInternal
    mapM_ closeStorePool (Map.elems pools)
    putMVar store.scopePoolsInternal Map.empty
    closeStorePool store.trustedPoolInternal
    closeStorePool store.provisioningPoolInternal

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
    modifyMVar store.scopePoolsInternal \pools ->
        case Map.lookup role pools of
            Just pool -> pure (pools, Right pool)
            Nothing -> do
                let options = defaultPoolConfig { poolSize = 2 }
                openRoleStorePool store.storeConfigInternal role options >>= \case
                    Left err -> pure (pools, Left err)
                    Right pool ->
                        pure (Map.insert role pool pools, Right pool)

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
