{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | Bounded database-per-tenant provisioning and store ownership.
module Agent.Store.Postgres.Tenant
    ( TenantDatabase
    , tenantDatabase
    , tenantDatabaseName
    , tenantDatabaseRuntimeRole
    , TenantStoreManager
    , openTenantStoreManager
    , acquireTenantStore
    , checkTenantStoreManager
    , closeTenantStoreManager
    ) where

import Agent.Store.Postgres
    ( Store
    , closeStore
    , openStoreWithRuntimeRole
    )
import Agent.Store.Postgres.Config
    ( ManagedPostgresConfig(..)
    , ManagedPostgresPaths(..)
    )
import Agent.Store.Postgres.Connection
    ( StorePool
    , defaultPoolConfig
    , openStorePool
    , closeStorePool
    , runSession
    )
import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.Postgres.Managed (ensureManagedPostgres)
import Agent.Store.Types (StoreError(..))
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , newEmptyMVar
    , newMVar
    , putMVar
    , readMVar
    )
import Control.Exception.Safe
    ( SomeException
    , finally
    , mask
    , onException
    , tryAny
    )
import Control.Monad (forM_, void)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString8
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)
import qualified System.FileLock as FileLock
import System.FilePath ((</>))

data TenantDatabase = TenantDatabase
    { tenantDatabaseNameInternal :: !Text
    , tenantDatabaseRuntimeRoleInternal :: !Text
    }
    deriving (Eq, Ord, Show)

tenantDatabase :: Text -> Text -> Either StoreError TenantDatabase
tenantDatabase databaseName runtimeRole
    | not (validIdentifier databaseName) =
        Left
            (StoreConfigurationError
                "the tenant database name is not a safe PostgreSQL identifier")
    | not (validIdentifier runtimeRole) =
        Left
            (StoreConfigurationError
                "the tenant runtime role is not a safe PostgreSQL identifier")
    | otherwise =
        Right TenantDatabase
            { tenantDatabaseNameInternal = databaseName
            , tenantDatabaseRuntimeRoleInternal = runtimeRole
            }

tenantDatabaseName :: TenantDatabase -> Text
tenantDatabaseName = (.tenantDatabaseNameInternal)

tenantDatabaseRuntimeRole :: TenantDatabase -> Text
tenantDatabaseRuntimeRole = (.tenantDatabaseRuntimeRoleInternal)

data TenantSlot
    = TenantOpening !(MVar (Either StoreError Store))
    | TenantReady !Store

data ManagerState = ManagerState
    { managerClosed :: !Bool
    , managerSlots :: !(Map TenantDatabase TenantSlot)
    }

data TenantStoreManager = TenantStoreManager
    { managerBaseConfig :: !ManagedPostgresConfig
    , managerOwnerPool :: !StorePool
    , managerMaximumStores :: !Int
    , managerState :: !(MVar ManagerState)
    }

data Acquisition
    = AcquisitionRejected !StoreError
    | AcquisitionReady !Store
    | AcquisitionWait !(MVar (Either StoreError Store))
    | AcquisitionOpen !(MVar (Either StoreError Store))

openTenantStoreManager
    :: ManagedPostgresConfig
    -> Int
    -> IO (Either StoreError TenantStoreManager)
openTenantStoreManager config maximumStores
    | maximumStores < 1 =
        pure
            (Left
                (StoreConfigurationError
                    "the tenant store limit must be positive"))
    | otherwise =
        ensureManagedPostgres config >>= \case
            Left err -> pure (Left err)
            Right _ ->
                openStorePool config defaultPoolConfig >>= \case
                    Left err -> pure (Left err)
                    Right ownerPool -> do
                        hardened <- hardenBaseDatabase config ownerPool
                        case hardened of
                            Left err -> do
                                closeStorePool ownerPool
                                pure (Left err)
                            Right () -> do
                                state <- newMVar ManagerState
                                    { managerClosed = False
                                    , managerSlots = Map.empty
                                    }
                                pure $
                                    Right TenantStoreManager
                                        { managerBaseConfig = config
                                        , managerOwnerPool = ownerPool
                                        , managerMaximumStores = maximumStores
                                        , managerState = state
                                        }

acquireTenantStore
    :: TenantStoreManager
    -> TenantDatabase
    -> IO (Either StoreError Store)
acquireTenantStore manager database = mask \restore -> do
    decision <- modifyMVar manager.managerState \state ->
        if state.managerClosed
            then pure (state, AcquisitionRejected storeManagerClosed)
            else case Map.lookup database state.managerSlots of
                Just (TenantReady store) ->
                    pure (state, AcquisitionReady store)
                Just (TenantOpening completion) ->
                    pure (state, AcquisitionWait completion)
                Nothing
                    | Map.size state.managerSlots
                        >= manager.managerMaximumStores ->
                        pure
                            ( state
                            , AcquisitionRejected
                                (StoreConnectionError
                                    "the active tenant store limit has been reached")
                            )
                    | otherwise -> do
                        completion <- newEmptyMVar
                        pure
                            ( state
                                { managerSlots =
                                    Map.insert
                                        database
                                        (TenantOpening completion)
                                        state.managerSlots
                                }
                            , AcquisitionOpen completion
                            )
    case decision of
        AcquisitionRejected err -> pure (Left err)
        AcquisitionReady store -> pure (Right store)
        AcquisitionWait completion -> restore (readMVar completion)
        AcquisitionOpen completion -> do
            outcome <-
                (tryAny
                    (restore (provisionAndOpen manager database))
                    >>= pure . either unexpectedFailure id)
                    `onException`
                        void
                            (publishOpenedStore
                                manager
                                database
                                completion
                                (Left
                                    (StoreProcessError
                                        "tenant database provisioning was cancelled")))
            publishOpenedStore manager database completion outcome

checkTenantStoreManager
    :: TenantStoreManager
    -> IO (Either StoreError ())
checkTenantStoreManager manager =
    readMVar manager.managerState >>= \state ->
        if state.managerClosed
            then pure (Left storeManagerClosed)
            else runSession manager.managerOwnerPool (pure ())

closeTenantStoreManager :: TenantStoreManager -> IO ()
closeTenantStoreManager manager = mask \restore -> do
    (stores, openings) <- modifyMVar manager.managerState \state ->
        if state.managerClosed
            then pure (state, ([], []))
            else
                pure
                    ( state
                        { managerClosed = True
                        , managerSlots = Map.empty
                        }
                    , ( [store
                        | TenantReady store <- Map.elems state.managerSlots
                        ]
                      , [completion
                        | TenantOpening completion <- Map.elems state.managerSlots
                        ]
                      )
                    )
    let closeReady = forM_ stores \store ->
            void (tryAny (restore (closeStore store)))
        waitOpening = forM_ openings \completion ->
            void (restore (readMVar completion))
    (closeReady `finally` waitOpening)
        `finally` closeStorePool manager.managerOwnerPool

publishOpenedStore
    :: TenantStoreManager
    -> TenantDatabase
    -> MVar (Either StoreError Store)
    -> Either StoreError Store
    -> IO (Either StoreError Store)
publishOpenedStore manager database completion outcome = do
    effective <- modifyMVar manager.managerState \state ->
        if state.managerClosed
            then
                pure
                    ( state
                    , case outcome of
                        Left err -> Left err
                        Right _ -> Left storeManagerClosed
                    )
            else
                pure
                    ( state
                        { managerSlots =
                            case outcome of
                                Left _ ->
                                    Map.delete database state.managerSlots
                                Right store ->
                                    Map.insert
                                        database
                                        (TenantReady store)
                                        state.managerSlots
                        }
                    , outcome
                    )
    case (outcome, effective) of
        (Right store, Left _) -> void (tryAny (closeStore store))
        _ -> pure ()
    putMVar completion effective
    pure effective

provisionAndOpen
    :: TenantStoreManager
    -> TenantDatabase
    -> IO (Either StoreError Store)
provisionAndOpen manager database =
    withProvisioningLock manager \_ -> do
        provisionTenantDatabase
            manager.managerBaseConfig
            manager.managerOwnerPool
            database >>= \case
                Left err -> pure (Left err)
                Right () ->
                    openStoreWithRuntimeRole
                        manager.managerBaseConfig
                            { postgresDatabase =
                                database.tenantDatabaseNameInternal
                            }
                        database.tenantDatabaseRuntimeRoleInternal

withProvisioningLock
    :: TenantStoreManager
    -> (FileLock.FileLock -> IO (Either StoreError value))
    -> IO (Either StoreError value)
withProvisioningLock manager action =
    tryAny
        (FileLock.withFileLock
            lockPath
            FileLock.Exclusive
            action) >>= \case
                Left _ ->
                    pure
                        (Left
                            (StoreProcessError
                                "could not acquire the tenant database provisioning lock"))
                Right result -> pure result
  where
    lockPath =
        manager.managerBaseConfig.postgresPaths.postgresRootDirectory
            </> "tenant-provisioning.lock"

provisionTenantDatabase
    :: ManagedPostgresConfig
    -> StorePool
    -> TenantDatabase
    -> IO (Either StoreError ())
provisionTenantDatabase config ownerPool database = do
    let databaseName = database.tenantDatabaseNameInternal
        runtimeRole = database.tenantDatabaseRuntimeRoleInternal
        ownerRole = config.postgresOwnerRole
    if not (validIdentifier ownerRole)
        then
            pure
                (Left
                    (StoreConfigurationError
                        "the PostgreSQL owner role is not a safe identifier"))
        else
            runSqlStatements ownerPool
                (createRuntimeRoleSql runtimeRole) >>= \case
                    Left err -> pure (Left err)
                    Right () ->
                        runSession
                            ownerPool
                            (Session.statement
                                databaseName
                                databaseExistsStatement) >>= \case
                                    Left err -> pure (Left err)
                                    Right exists -> do
                                        created <-
                                            if exists
                                                then pure (Right ())
                                                else
                                                    runSql
                                                        ownerPool
                                                        ("CREATE DATABASE "
                                                            <> encodeIdentifier
                                                                databaseName
                                                            <> " OWNER "
                                                            <> encodeIdentifier
                                                                ownerRole)
                                        case created of
                                            Left err -> pure (Left err)
                                            Right () ->
                                                runSqlStatements
                                                    ownerPool
                                                    [ "REVOKE CONNECT ON DATABASE "
                                                        <> encodeIdentifier
                                                            databaseName
                                                        <> " FROM PUBLIC"
                                                    , "GRANT CONNECT ON DATABASE "
                                                        <> encodeIdentifier
                                                            databaseName
                                                        <> " TO "
                                                        <> encodeIdentifier
                                                            runtimeRole
                                                    ]

hardenBaseDatabase
    :: ManagedPostgresConfig
    -> StorePool
    -> IO (Either StoreError ())
hardenBaseDatabase config ownerPool
    | not (validIdentifier config.postgresDatabase) =
        pure
            (Left
                (StoreConfigurationError
                    "the managed PostgreSQL database is not a safe identifier"))
    | otherwise =
        runSql ownerPool $
            "REVOKE CONNECT ON DATABASE "
                <> encodeIdentifier config.postgresDatabase
                <> " FROM PUBLIC"

runSql :: StorePool -> ByteString -> IO (Either StoreError ())
runSql pool statement =
    runSession pool
        (Session.statement
            ()
            (mkStatement
                (TextEncoding.decodeUtf8 statement)
                Encoders.noParams
                Decoders.noResult
                False))

runSqlStatements
    :: StorePool
    -> [ByteString]
    -> IO (Either StoreError ())
runSqlStatements _ [] = pure (Right ())
runSqlStatements pool (statement : remaining) =
    runSql pool statement >>= \case
        Left err -> pure (Left err)
        Right () -> runSqlStatements pool remaining

createRuntimeRoleSql :: Text -> [ByteString]
createRuntimeRoleSql role =
    [ "DO $ha$ BEGIN\
      \ IF NOT EXISTS (\
      \   SELECT 1 FROM pg_catalog.pg_roles WHERE rolname = "
        <> quoteLiteral role
        <> "\
           \ ) THEN\
           \   CREATE ROLE "
        <> encodedRole
        <> " LOGIN NOSUPERUSER NOCREATEDB NOCREATEROLE\
           \ NOINHERIT NOREPLICATION NOBYPASSRLS;\
           \ END IF;\
           \ END $ha$"
    , "ALTER ROLE "
        <> encodedRole
        <> " LOGIN NOSUPERUSER NOCREATEDB NOCREATEROLE\
           \ NOINHERIT NOREPLICATION NOBYPASSRLS"
    , "ALTER ROLE " <> encodedRole <> " SET search_path TO pg_catalog"
    , "ALTER ROLE " <> encodedRole <> " SET statement_timeout TO '30s'"
    , "ALTER ROLE " <> encodedRole <> " SET lock_timeout TO '5s'"
    , "ALTER ROLE "
        <> encodedRole
        <> " SET idle_in_transaction_session_timeout TO '30s'"
    ]
  where
    encodedRole = encodeIdentifier role

databaseExistsStatement :: Statement Text Bool
databaseExistsStatement = mkStatement
    "SELECT EXISTS (\
    \ SELECT 1 FROM pg_catalog.pg_database WHERE datname = $1\
    \ )"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.singleRow
        (Decoders.column (Decoders.nonNullable Decoders.bool)))
    True

validIdentifier :: Text -> Bool
validIdentifier value =
    Text.length value >= 1
        && Text.length value <= 63
        && case Text.uncons value of
            Just (first, _) ->
                asciiLower first
                    && Text.all
                        (\character ->
                            asciiLower character
                                || asciiDigit character
                                || character == '_')
                        value
            Nothing -> False
  where
    asciiLower character =
        character >= 'a' && character <= 'z'
    asciiDigit character =
        character >= '0' && character <= '9'

encodeIdentifier :: Text -> ByteString
encodeIdentifier = ByteString8.pack . Text.unpack

quoteLiteral :: Text -> ByteString
quoteLiteral value =
    ByteString8.pack $
        "'" <> concatMap escape (Text.unpack value) <> "'"
  where
    escape '\'' = "''"
    escape character = [character]

storeManagerClosed :: StoreError
storeManagerClosed =
    StoreConnectionError "the tenant store manager is closed"

unexpectedFailure :: SomeException -> Either StoreError value
unexpectedFailure _ =
    Left
        (StoreProcessError
            "tenant database provisioning terminated unexpectedly")
