-- | Adapter from the CLI database tools to the Hasql-backed PostgreSQL store.
module Agent.CLI.Database.Store
    ( DatabaseScopes
    , deriveDatabaseScopes
    , scopeForDatabase
    , applicableDatabaseScopes
    , databaseToolsEnvForStore
    ) where

import Agent.CLI.Database
    ( ConversationSearchMatch(..)
    , DatabaseScope(..)
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
    , mkScopeId
    , provisionScope
    )
import Agent.Store.Types (renderStoreError)
import Agent.Json (RawJson)
import Agent.Json.Decode (JsonError(..), validateRawJson)
import Control.Exception.Safe (SomeException, try)
import Data.Aeson (Value, toJSON)
import qualified Data.Aeson as Aeson
import Data.Bits (xor)
import qualified Data.ByteString as ByteString
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
            Right results -> pure $ Right $ map searchResultValue results
    }

queryResultValue :: CustomQueryResult -> Either Text RawJson
queryResultValue result = do
    _ <- decodeJsonText "custom query rows" result.customQueryRows
    case validateRawJson $ Text.encodeUtf8 $
            "{\"rows\":" <> result.customQueryRows
                <> ",\"truncated\":"
                <> (if result.customQueryTruncated then "true" else "false")
                <> "}" of
        Left err -> Left ("custom query result: " <> jsonErrorMessage err)
        Right raw -> Right raw

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

decodeJsonText :: Text -> Text -> Either Text RawJson
decodeJsonText label value =
    case validateRawJson (Text.encodeUtf8 value) of
        Left err -> Left (label <> ": " <> jsonErrorMessage err)
        Right decoded -> Right decoded

searchResultValue :: ConversationSearchResult -> ConversationSearchMatch
searchResultValue result = ConversationSearchMatch
    result.searchSessionId
    (fromIntegral result.searchTurnIndex)
    (Just (Text.pack (show result.searchOccurredAt)))
    result.searchUserText
    result.searchAssistantText

withScopeDatabase
    :: forall value. Store
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
