-- | Adapter from the CLI database tools to the Hasql-backed PostgreSQL store.
module Agent.CLI.Database.Store
    ( DatabaseScopes
    , DatabaseBrowsePage(..)
    , deriveDatabaseScopes
    , scopeForDatabase
    , applicableDatabaseScopes
    , databaseToolsEnvForStore
    , listDatabaseObjects
    , loadDatabaseRows
    ) where

import Agent.CLI.Database
    ( DatabaseScope(..)
    , DatabaseToolsEnv(..)
    )
import Agent.Store.Postgres
    ( Store
    , provisioningPool
    , scopePool
    , trustedPool
    )
import Agent.Store.Postgres.Connection (storePool)
import Agent.Store.Postgres.Custom
    ( CatalogColumn(..)
    , CatalogConstraint(..)
    , CatalogDefinition(..)
    , CatalogIndex(..)
    , CatalogObject(..)
    , CustomAuditContext(..)
    , CustomExecutionResult(..)
    , CustomQueryResult(..)
    , QueryLimits(..)
    , defaultQueryLimits
    , executeCustom
    , inspectCustomSchema
    , queryCustom
    )
import Agent.Store.Postgres.Session
    ( ConversationSearchResult(..)
    , searchConversationTurns
    )
import Agent.Store.Postgres.Scope
    ( Scope(..)
    , ScopeDatabase(..)
    , ScopeId
    , ScopeKind(..)
    , lookupScopeDatabase
    , mkScopeId
    , provisionScope
    )
import Agent.Store.Types (renderStoreError)
import Control.Exception.Safe (SomeException, try)
import Data.Aeson (Value, toJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import Data.Bits (xor)
import qualified Data.ByteString as ByteString
import Data.Int (Int64)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word64)
import qualified Hasql.Pool
import Numeric (showHex)
import System.Exit (ExitCode(..))
import System.FilePath (normalise)
import System.Process (readProcessWithExitCode)

data DatabaseScopes = DatabaseScopes
    { userScope :: !Scope
    , repositoryScope :: !Scope
    , checkoutScope :: !Scope
    }
    deriving (Eq, Show)

data DatabaseBrowsePage = DatabaseBrowsePage
    { databaseBrowseRows :: ![[Value]]
    , databaseBrowseHasMore :: !Bool
    }
    deriving (Eq, Show)

-- | Derive stable, non-secret identifiers for the three durable scopes.
--
-- The user scope is local to the harness state directory. Repository scope
-- prefers the origin URL so separate clones share data, then falls back to the
-- common Git directory so linked worktrees share data. Checkout scope follows
-- the canonical checkout root.
deriveDatabaseScopes
    :: FilePath
    -- ^ Harness state directory.
    -> FilePath
    -- ^ Canonical project/checkout root.
    -> IO (Either Text DatabaseScopes)
deriveDatabaseScopes stateDirectory projectRoot = do
    repositoryIdentity <- discoverRepositoryIdentity projectRoot
    pure do
        userId <- stableScopeId ("user:" <> Text.pack (normalise stateDirectory))
        repositoryId <- stableScopeId
            ("repository:" <> repositoryIdentity)
        checkoutId <- stableScopeId
            ("checkout:" <> Text.pack (normalise projectRoot))
        pure DatabaseScopes
            { userScope = Scope UserScope userId
            , repositoryScope = Scope RepositoryScope repositoryId
            , checkoutScope = Scope CheckoutScope checkoutId
            }

databaseToolsEnvForStore
    :: Store
    -> DatabaseScopes
    -> IO (Maybe Text)
    -- ^ Current root session id, when persistence has started.
    -> DatabaseToolsEnv
databaseToolsEnvForStore store scopes currentSessionId = DatabaseToolsEnv
    { databaseDescribeScope = \selected ->
        withScopeDatabase store (scopeForDatabase scopes selected) \database pool ->
            fmap (toJSON . map catalogObjectValue)
                <$> inspectCustomSchema pool database
    , databaseRunQuery = \selected sql ->
        withScopeDatabase store (scopeForDatabase scopes selected) \database pool ->
            queryCustom pool database defaultQueryLimits sql >>= \case
                Left err -> pure (Left err)
                Right result -> pure (queryResultValue result)
    , databaseRunExecute = \selected purpose sql ->
        withScopeDatabase store (scopeForDatabase scopes selected) \database pool -> do
            sessionId <- currentSessionId
            fmap executionResultValue <$> executeCustom
                (storePool (trustedPool store))
                pool
                database
                CustomAuditContext
                    { customAuditSessionId = sessionId
                    , customAuditAgentId = Nothing
                    }
                defaultQueryLimits
                purpose
                sql
    , databaseSearchConversations = \query limit ->
        searchConversationTurns (trustedPool store) query limit >>= \case
            Left err -> pure (Left (renderStoreError err))
            Right results -> pure $ Right $ toJSON (map searchResultValue results)
    }

-- | List the table-like objects exposed by one user-defined scope. Sequences
-- are intentionally omitted from the native data browser.
listDatabaseObjects
    :: Store
    -> DatabaseScopes
    -> DatabaseScope
    -> IO (Either Text [CatalogObject])
listDatabaseObjects store scopes selected =
    withExistingScopeDatabase
        store
        (scopeForDatabase scopes selected)
        (Right [])
        \database pool ->
            fmap (filter isBrowseableObject)
                <$> inspectCustomSchema pool database

-- | Load one bounded preview in catalog column order. A preview is read by one
-- PostgreSQL statement, rather than multiple OFFSET queries whose snapshots can
-- drift while a table is changing. The object name must first resolve through
-- the isolated custom-schema catalog, so quoting cannot expose harness-owned
-- or sibling-scope relations.
loadDatabaseRows
    :: Store
    -> DatabaseScopes
    -> DatabaseScope
    -> Text
    -> Int64
    -> Int
    -> IO (Either Text DatabaseBrowsePage)
loadDatabaseRows store scopes selected objectName offset limit
    | offset /= 0 = pure (Left "data preview offset must be zero")
    | limit <= 0 || limit > 500 =
        pure (Left "data page size must be between 1 and 500")
    | otherwise =
        withExistingScopeDatabase
            store
            (scopeForDatabase scopes selected)
            (Left "the selected data table no longer exists")
            \database pool ->
                inspectCustomSchema pool database >>= \case
                    Left err -> pure (Left err)
                    Right catalog ->
                        case find matchesObject catalog of
                            Nothing ->
                                pure (Left "the selected data table no longer exists")
                            Just object -> do
                                let columns =
                                        object.catalogObjectDefinition.definitionColumns
                                    requested = fromIntegral limit + 1
                                    limits = defaultQueryLimits
                                        { queryMaxRows = requested
                                        , queryMaxOutputBytes = 8 * 1024 * 1024
                                        }
                                    sql =
                                        "select * from "
                                            <> quoteBrowseIdentifier objectName
                                            <> " limit " <> Text.pack (show requested)
                                queryCustom pool database limits sql >>= \case
                                    Left err -> pure (Left err)
                                    Right result ->
                                        pure $ do
                                            rows <- decodeBrowseRows
                                                columns
                                                result.customQueryRows
                                            pure DatabaseBrowsePage
                                                { databaseBrowseRows =
                                                    take limit rows
                                                , databaseBrowseHasMore =
                                                    length rows > limit
                                                }
  where
    matchesObject object =
        object.catalogObjectName == objectName
            && isBrowseableObject object

isBrowseableObject :: CatalogObject -> Bool
isBrowseableObject object =
    object.catalogObjectKind
        `elem` ["table", "partitioned_table", "view", "materialized_view"]

decodeBrowseRows :: [CatalogColumn] -> Text -> Either Text [[Value]]
decodeBrowseRows columns encoded =
    case Aeson.eitherDecodeStrict' (Text.encodeUtf8 encoded) of
        Left err -> Left ("database rows: " <> Text.pack err)
        Right values -> traverse rowValues (values :: [Value])
  where
    rowValues (Aeson.Object object) =
        Right
            [ fromMaybe Aeson.Null $
                AesonKeyMap.lookup
                    (AesonKey.fromText column.columnName)
                    object
            | column <- columns
            ]
    rowValues _ = Left "database rows did not have the expected shape"

quoteBrowseIdentifier :: Text -> Text
quoteBrowseIdentifier value =
    "\"" <> Text.replace "\"" "\"\"" value <> "\""

queryResultValue :: CustomQueryResult -> Either Text Value
queryResultValue result = do
    rows <- decodeJsonText "custom query rows" result.customQueryRows
    pure $ Aeson.object
        [ "rows" Aeson..= rows
        , "truncated" Aeson..= result.customQueryTruncated
        ]

executionResultValue :: CustomExecutionResult -> Value
executionResultValue result = Aeson.object
    [ "audit_id" Aeson..= result.customExecutionAuditId
    , "catalog_before" Aeson..=
        map catalogObjectValue result.customExecutionCatalogBefore
    , "catalog_after" Aeson..=
        map catalogObjectValue result.customExecutionCatalogAfter
    , "warning" Aeson..= result.customExecutionWarning
    ]

catalogObjectValue :: CatalogObject -> Value
catalogObjectValue object = Aeson.object
    [ "kind" Aeson..= object.catalogObjectKind
    , "name" Aeson..= object.catalogObjectName
    , "definition" Aeson..= catalogDefinitionValue
        object.catalogObjectDefinition
    ]

catalogDefinitionValue :: CatalogDefinition -> Value
catalogDefinitionValue definition = Aeson.object
    [ "owner" Aeson..= definition.definitionOwner
    , "comment" Aeson..= definition.definitionComment
    , "view_definition" Aeson..= definition.definitionView
    , "columns" Aeson..= map catalogColumnValue definition.definitionColumns
    , "constraints" Aeson..=
        map catalogConstraintValue definition.definitionConstraints
    , "indexes" Aeson..= map catalogIndexValue definition.definitionIndexes
    ]

catalogColumnValue :: CatalogColumn -> Value
catalogColumnValue column = Aeson.object
    [ "name" Aeson..= column.columnName
    , "type" Aeson..= column.columnType
    , "nullable" Aeson..= column.columnNullable
    , "default" Aeson..= column.columnDefault
    , "identity" Aeson..= column.columnIdentity
    , "generated" Aeson..= column.columnGenerated
    , "comment" Aeson..= column.columnComment
    ]

catalogConstraintValue :: CatalogConstraint -> Value
catalogConstraintValue constraint = Aeson.object
    [ "name" Aeson..= constraint.constraintName
    , "type" Aeson..= constraint.constraintType
    , "definition" Aeson..= constraint.constraintDefinition
    ]

catalogIndexValue :: CatalogIndex -> Value
catalogIndexValue index = Aeson.object
    [ "name" Aeson..= index.indexName
    , "definition" Aeson..= index.indexDefinition
    ]

decodeJsonText :: Text -> Text -> Either Text Value
decodeJsonText label value =
    case Aeson.eitherDecodeStrict' (Text.encodeUtf8 value) of
        Left err -> Left (label <> ": " <> Text.pack err)
        Right decoded -> Right decoded

searchResultValue :: ConversationSearchResult -> Value
searchResultValue result = Aeson.object
    [ "session_id" Aeson..= result.searchSessionId
    , "turn_index" Aeson..= result.searchTurnIndex
    , "occurred_at" Aeson..= result.searchOccurredAt
    , "user_text" Aeson..= result.searchUserText
    , "assistant_text" Aeson..= result.searchAssistantText
    , "rank" Aeson..= result.searchRank
    ]

withScopeDatabase
    :: Store
    -> Scope
    -> (ScopeDatabase -> HasqlPool -> IO (Either Text value))
    -> IO (Either Text value)
withScopeDatabase store scope action =
    provisionScope (storePool (provisioningPool store)) scope >>= \case
        Left err -> pure (Left err)
        Right database ->
            scopePool store database.scopeDatabaseRole >>= \case
                Left err -> pure (Left (renderStoreError err))
                Right pool -> action database (storePool pool)

-- | Run a native browser read only when the scope already exists. Unlike the
-- agent database tools, merely opening Data must not create a role, schema, or
-- scope-registry row.
withExistingScopeDatabase
    :: Store
    -> Scope
    -> Either Text value
    -> (ScopeDatabase -> HasqlPool -> IO (Either Text value))
    -> IO (Either Text value)
withExistingScopeDatabase store scope missing action =
    lookupScopeDatabase (storePool (provisioningPool store)) scope >>= \case
        Left err -> pure (Left err)
        Right Nothing -> pure missing
        Right (Just database) ->
            scopePool store database.scopeDatabaseRole >>= \case
                Left err -> pure (Left (renderStoreError err))
                Right pool -> action database (storePool pool)

type HasqlPool = Hasql.Pool.Pool

scopeForDatabase :: DatabaseScopes -> DatabaseScope -> Scope
scopeForDatabase scopes = \case
    DatabaseUserScope -> scopes.userScope
    DatabaseRepositoryScope -> scopes.repositoryScope
    DatabaseCheckoutScope -> scopes.checkoutScope

applicableDatabaseScopes :: DatabaseScopes -> [Scope]
applicableDatabaseScopes scopes =
    [ scopes.userScope
    , scopes.repositoryScope
    , scopes.checkoutScope
    ]

stableScopeId :: Text -> Either Text ScopeId
stableScopeId identity =
    mkScopeId (hex64 first <> hex64 second)
  where
    bytes = Text.encodeUtf8 identity
    first = fnv1a 14695981039346656037 bytes
    second = fnv1a 7809847782465536322 (ByteString.reverse bytes)

fnv1a :: Word64 -> ByteString.ByteString -> Word64
fnv1a seed =
    ByteString.foldl'
        (\value byte -> (value `xor` fromIntegral byte) * 1099511628211)
        seed

hex64 :: Word64 -> Text
hex64 value =
    let encoded = showHex value ""
    in Text.pack (replicate (16 - length encoded) '0' <> encoded)

discoverRepositoryIdentity :: FilePath -> IO Text
discoverRepositoryIdentity projectRoot =
    sanitizeRepositoryIdentity <$> firstSuccessful
        [ ["-C", projectRoot, "config", "--get", "remote.origin.url"]
        , ["-C", projectRoot, "rev-parse", "--path-format=absolute", "--git-common-dir"]
        ]
        (Text.pack (normalise projectRoot))

sanitizeRepositoryIdentity :: Text -> Text
sanitizeRepositoryIdentity raw =
    case Text.breakOn "://" raw of
        (scheme, rest)
            | not (Text.null rest) ->
                let afterScheme = Text.drop 3 rest
                    (authority, path) = Text.breakOn "/" afterScheme
                    host = dropUserInfo authority
                in scheme <> "://" <> host <> path
        _ -> dropUserInfo raw
  where
    dropUserInfo value =
        case Text.breakOnEnd "@" value of
            ("", _) -> value
            (_, suffix) -> suffix

firstSuccessful :: [[String]] -> Text -> IO Text
firstSuccessful commands fallback = go commands
  where
    go = \case
        [] -> pure fallback
        command : rest ->
            try (readProcessWithExitCode "git" command "")
                >>= \case
                    Left (_ :: SomeException) -> go rest
                    Right (ExitSuccess, output, _) ->
                        let value = Text.strip (Text.pack output)
                        in if Text.null value
                            then go rest
                            else pure value
                    Right _ -> go rest
