{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Direct, scope-isolated PostgreSQL access for agent-created structured data.
--
-- The caller supplies a pool connected as the selected scope role.  PostgreSQL
-- privileges isolate that role from harness-owned and sibling scope schemas.
module Agent.Store.Postgres.Custom
    ( QueryLimits(..)
    , defaultQueryLimits
    , CatalogObject(..)
    , CustomQueryResult(..)
    , CustomAuditContext(..)
    , CustomExecutionResult(..)
    , inspectCustomSchema
    , queryCustom
    , executeCustom
    , normalizeCustomQuery
    , normalizeCustomExecution
    ) where

import Control.Monad (forM_, unless)
import Data.Aeson (FromJSON, ToJSON, Value)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Pool (Pool)
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as TxSessions

import Agent.Store.Postgres.Scope
    ( Scope(..)
    , ScopeDatabase(..)
    , scopeIdText
    )
import Agent.Store.Postgres.Hasql (mkStatement)

data QueryLimits = QueryLimits
    { queryMaxRows :: !Int64
    , queryMaxOutputBytes :: !Int
    , queryStatementTimeoutMs :: !Int
    , queryLockTimeoutMs :: !Int
    }
    deriving (Eq, Show)

instance FromJSON CustomQueryResult where
    parseJSON = Aeson.withObject "CustomQueryResult" \object ->
        CustomQueryResult
            <$> object Aeson..: "rows"
            <*> object Aeson..: "truncated"

instance ToJSON CustomQueryResult where
    toJSON result = Aeson.object
        [ "rows" Aeson..= result.customQueryRows
        , "truncated" Aeson..= result.customQueryTruncated
        ]

defaultQueryLimits :: QueryLimits
defaultQueryLimits = QueryLimits
    { queryMaxRows = 500
    , queryMaxOutputBytes = 100000
    , queryStatementTimeoutMs = 30000
    , queryLockTimeoutMs = 5000
    }

data CatalogObject = CatalogObject
    { catalogObjectKind :: !Text
    , catalogObjectName :: !Text
    , catalogObjectDefinition :: !Value
    }
    deriving (Eq, Show)

instance FromJSON CatalogObject where
    parseJSON = Aeson.withObject "CatalogObject" \object ->
        CatalogObject
            <$> object Aeson..: "kind"
            <*> object Aeson..: "name"
            <*> object Aeson..: "definition"

instance ToJSON CatalogObject where
    toJSON object = Aeson.object
        [ "kind" Aeson..= object.catalogObjectKind
        , "name" Aeson..= object.catalogObjectName
        , "definition" Aeson..= object.catalogObjectDefinition
        ]

data CustomQueryResult = CustomQueryResult
    { customQueryRows :: !Value
    , customQueryTruncated :: !Bool
    }
    deriving (Eq, Show)

data CustomAuditContext = CustomAuditContext
    { customAuditSessionId :: !(Maybe Text)
    , customAuditAgentId :: !(Maybe Text)
    }
    deriving (Eq, Show)

data CustomExecutionResult = CustomExecutionResult
    { customExecutionAuditId :: !Text
    , customExecutionCatalogBefore :: ![CatalogObject]
    , customExecutionCatalogAfter :: ![CatalogObject]
    , customExecutionWarning :: !(Maybe Text)
    }
    deriving (Eq, Show)

instance ToJSON CustomExecutionResult where
    toJSON result = Aeson.object
        [ "audit_id" Aeson..= result.customExecutionAuditId
        , "catalog_before" Aeson..= result.customExecutionCatalogBefore
        , "catalog_after" Aeson..= result.customExecutionCatalogAfter
        , "warning" Aeson..= result.customExecutionWarning
        ]

inspectCustomSchema
    :: Pool
    -> ScopeDatabase
    -> IO (Either Text [CatalogObject])
inspectCustomSchema scopePool database =
    runPool scopePool session >>= \case
        Left err -> pure (Left err)
        Right (Left err) -> pure (Left err)
        Right (Right value) -> pure (decodeCatalogValue value)
  where
    session = do
        expectedIdentity <- Session.statement
            database.scopeDatabaseRole
            scopeIdentityStatement
        if not expectedIdentity
            then pure (Left scopeIdentityError)
            else Right <$> Session.statement
                database.scopeDatabaseSchema
                catalogStatement

queryCustom
    :: Pool
    -> ScopeDatabase
    -> QueryLimits
    -> Text
    -> IO (Either Text CustomQueryResult)
queryCustom scopePool database limits rawQuery =
    case validateLimits limits *> normalizeCustomQuery rawQuery of
        Left err -> pure (Left err)
        Right query -> do
            let statement = customQueryStatement limits query
                transaction = do
                    expectedIdentity <- Tx.statement
                        database.scopeDatabaseRole
                        scopeIdentityStatement
                    if not expectedIdentity
                        then pure (Left scopeIdentityError)
                        else do
                            Tx.sql (confinementSql database limits)
                            Right <$> Tx.statement () statement
                session =
                    TxSessions.transactionNoRetry
                        TxSessions.ReadCommitted
                        TxSessions.Read
                        transaction
            runPool scopePool session >>= \case
                Left err -> pure (Left err)
                Right (Left err) -> pure (Left err)
                Right (Right value) ->
                    case decodeQueryEnvelope value of
                        Left err -> pure (Left err)
                        Right result
                            | encodedSize result.customQueryRows
                                > limits.queryMaxOutputBytes ->
                                pure $ Left $
                                    "database query result exceeds "
                                        <> Text.pack
                                            (show limits.queryMaxOutputBytes)
                                        <> " encoded bytes; select fewer rows or columns"
                            | otherwise -> pure (Right result)

executeCustom
    :: Pool
    -- ^ Trusted harness pool, used only for audit/catalog projection.
    -> Pool
    -- ^ Pool authenticated directly as the selected scope role.
    -> ScopeDatabase
    -> CustomAuditContext
    -> QueryLimits
    -> Text
    -- ^ Human-readable purpose supplied with the model tool call.
    -> Text
    -- ^ Direct DDL/DML SQL batch.  Every top-level statement is validated
    -- before the batch is sent, keeping transaction and session control under
    -- the harness.
    -> IO (Either Text CustomExecutionResult)
executeCustom trustedPool scopePool database audit limits purpose rawSql
    | Text.null (Text.strip purpose) =
        pure (Left "database execution purpose must not be empty")
    | Left err <- validateLimits limits =
        pure (Left err)
    | Left err <- normalizeCustomExecution rawSql =
        pure (Left err)
    | otherwise = do
        let sql =
                either
                    (const rawSql)
                    id
                    (normalizeCustomExecution rawSql)
        startedAt <- getCurrentTime
        beforeResult <- inspectCustomSchema scopePool database
        let before = either (const []) id beforeResult
        recordAuditStarted
            trustedPool database audit purpose rawSql startedAt before
            >>= \case
                Left err ->
                    pure $ Left $
                        "custom SQL was not executed because its durable audit "
                            <> "attempt could not be recorded: " <> err
                Right auditId ->
                    case beforeResult of
                        Left err ->
                            finalizeFailedAttempt
                                trustedPool auditId err before
                        Right catalogBefore -> do
                            executionResult <- runPool scopePool $
                                TxSessions.transactionNoRetry
                                    TxSessions.Serializable
                                    TxSessions.Write
                                    do
                                        expectedIdentity <- Tx.statement
                                            database.scopeDatabaseRole
                                            scopeIdentityStatement
                                        if not expectedIdentity
                                            then pure (Left scopeIdentityError)
                                            else do
                                                Tx.sql
                                                    (confinementSql database limits)
                                                Tx.sql (Text.encodeUtf8 sql)
                                                pure (Right ())
                            case executionResult of
                                Left err ->
                                    finalizeFailedAttempt
                                        trustedPool auditId err catalogBefore
                                Right (Left err) ->
                                    finalizeFailedAttempt
                                        trustedPool auditId err catalogBefore
                                Right (Right ()) ->
                                    finishCommittedAttempt
                                        trustedPool
                                        scopePool
                                        database
                                        auditId
                                        catalogBefore

-- | Strip whitespace and trailing semicolons before nesting a user query in a
-- single JSONB-producing Hasql statement.
normalizeCustomQuery :: Text -> Either Text Text
normalizeCustomQuery raw =
    let stripped = Text.dropWhileEnd isTrailing (Text.strip raw)
    in if Text.null stripped
        then Left "database query SQL must not be empty"
        else if Text.any (== '\NUL') stripped
            then Left "database query SQL contains a NUL byte"
            else Right stripped
  where
    isTrailing char = char == ';' || char == ' ' || char == '\t'
        || char == '\r' || char == '\n'

normalizeCustomExecution :: Text -> Either Text Text
normalizeCustomExecution raw = do
    sql <- normalizeCustomQuery raw
    statements <- splitTopLevelStatements sql
    keywords <- traverse leadingSqlKeyword statements
    if not (null keywords)
        && all (`elem` allowedExecutionKeywords) keywords
        then pure sql
        else Left $
            "database execution only accepts DDL/DML statements; "
                <> "transaction and session control are not allowed"

allowedExecutionKeywords :: [Text]
allowedExecutionKeywords =
    [ "alter"
    , "analyze"
    , "comment"
    , "create"
    , "delete"
    , "drop"
    , "insert"
    , "merge"
    , "refresh"
    , "reindex"
    , "truncate"
    , "update"
    , "with"
    ]

leadingSqlKeyword :: Text -> Either Text Text
leadingSqlKeyword sql = do
    stripped <- stripLeadingSqlComments sql
    let keyword = Text.toLower (Text.takeWhile isKeywordChar stripped)
    if Text.null keyword
        then Left "database execution SQL does not start with a SQL keyword"
        else Right keyword
  where
    isKeywordChar char = isAlpha char || char == '_'

stripLeadingSqlComments :: Text -> Either Text Text
stripLeadingSqlComments input =
    let stripped = Text.dropWhile isSpace input
    in case Text.stripPrefix "--" stripped of
        Just rest ->
            stripLeadingSqlComments $
                case Text.break (== '\n') rest of
                    (_, remaining) -> Text.drop 1 remaining
        Nothing ->
            case Text.stripPrefix "/*" stripped of
                Nothing -> Right stripped
                Just rest ->
                    consumeBlockComment 1 rest >>= stripLeadingSqlComments

consumeBlockComment :: Int -> Text -> Either Text Text
consumeBlockComment depth input
    | Text.null input =
        Left "database execution SQL contains an unterminated block comment"
    | Just rest <- Text.stripPrefix "/*" input =
        consumeBlockComment (depth + 1) rest
    | Just rest <- Text.stripPrefix "*/" input =
        if depth == 1
            then Right rest
            else consumeBlockComment (depth - 1) rest
    | otherwise =
        consumeBlockComment depth (Text.drop 1 input)

data SqlLexState
    = SqlNormal
    | SqlSingleQuoted !Bool
    | SqlDoubleQuoted
    | SqlDollarQuoted !String
    | SqlLineComment
    | SqlBlockComment !Int
    deriving (Eq, Show)

-- | Split a PostgreSQL batch only at actual top-level terminators.  In
-- particular, semicolons in function bodies, strings, quoted identifiers, and
-- nested comments remain part of their containing statement.
splitTopLevelStatements :: Text -> Either Text [Text]
splitTopLevelStatements input = do
    scanned <- go SqlNormal [] [] (Text.unpack input)
    statements <- finish scanned
    pure $
        filter (not . Text.null . Text.strip) (map Text.pack statements)
  where
    go
        :: SqlLexState
        -> [Char]
        -> [[Char]]
        -> [Char]
        -> Either Text (SqlLexState, [Char], [[Char]])
    go state current statements remainingChars =
        case (state, remainingChars) of
            (SqlNormal, []) ->
                Right (state, current, statements)
            (SqlLineComment, []) ->
                Right (SqlNormal, current, statements)
            (SqlSingleQuoted _, []) ->
                Left "database execution SQL contains an unterminated string"
            (SqlDoubleQuoted, []) ->
                Left "database execution SQL contains an unterminated quoted identifier"
            (SqlDollarQuoted _, []) ->
                Left "database execution SQL contains an unterminated dollar quote"
            (SqlBlockComment _, []) ->
                Left "database execution SQL contains an unterminated block comment"

            (SqlNormal, '-' : '-' : rest) ->
                go SqlLineComment ('-' : '-' : current) statements rest
            (SqlNormal, '/' : '*' : rest) ->
                go (SqlBlockComment 1) ('*' : '/' : current) statements rest
            (SqlNormal, '\'' : rest) ->
                go
                    (SqlSingleQuoted (escapeStringPrefix current))
                    ('\'' : current)
                    statements
                    rest
            (SqlNormal, '"' : rest) ->
                go SqlDoubleQuoted ('"' : current) statements rest
            (SqlNormal, '$' : rest)
                | Just (delimiterTail, remaining) <-
                    dollarDelimiter rest ->
                        let delimiter = '$' : delimiterTail
                        in go
                            (SqlDollarQuoted delimiter)
                            (reverse delimiter <> current)
                            statements
                            remaining
            (SqlNormal, ';' : rest) ->
                go SqlNormal [] (reverse current : statements) rest
            (SqlNormal, char : rest) ->
                go SqlNormal (char : current) statements rest

            (SqlLineComment, '\n' : rest) ->
                go SqlNormal ('\n' : current) statements rest
            (SqlLineComment, char : rest) ->
                go SqlLineComment (char : current) statements rest

            (SqlBlockComment depth, '/' : '*' : rest) ->
                go
                    (SqlBlockComment (depth + 1))
                    ('*' : '/' : current)
                    statements
                    rest
            (SqlBlockComment depth, '*' : '/' : rest)
                | depth == 1 ->
                    go SqlNormal ('/' : '*' : current) statements rest
                | otherwise ->
                    go
                        (SqlBlockComment (depth - 1))
                        ('/' : '*' : current)
                        statements
                        rest
            (SqlBlockComment depth, char : rest) ->
                go (SqlBlockComment depth) (char : current) statements rest

            (SqlSingleQuoted _, '\'' : '\'' : rest) ->
                go state ('\'' : '\'' : current) statements rest
            (SqlSingleQuoted True, '\\' : char : rest) ->
                go state (char : '\\' : current) statements rest
            (SqlSingleQuoted _, '\'' : rest) ->
                go SqlNormal ('\'' : current) statements rest
            (SqlSingleQuoted escaped, char : rest) ->
                go (SqlSingleQuoted escaped) (char : current) statements rest

            (SqlDoubleQuoted, '"' : '"' : rest) ->
                go SqlDoubleQuoted ('"' : '"' : current) statements rest
            (SqlDoubleQuoted, '"' : rest) ->
                go SqlNormal ('"' : current) statements rest
            (SqlDoubleQuoted, char : rest) ->
                go SqlDoubleQuoted (char : current) statements rest

            (SqlDollarQuoted delimiter, remaining)
                | Just rest <- stripListPrefix delimiter remaining ->
                    go
                        SqlNormal
                        (reverse delimiter <> current)
                        statements
                        rest
            (SqlDollarQuoted delimiter, char : rest) ->
                go
                    (SqlDollarQuoted delimiter)
                    (char : current)
                    statements
                    rest

    finish (state, current, statements)
        | state /= SqlNormal =
            Left "database execution SQL ended in an invalid lexical state"
        | otherwise =
            Right (reverse (reverse current : statements))

    escapeStringPrefix ['e'] = True
    escapeStringPrefix ('e' : before : _) =
        not (isIdentifierChar before)
    escapeStringPrefix ['E'] = True
    escapeStringPrefix ('E' : before : _) =
        not (isIdentifierChar before)
    escapeStringPrefix _ = False

    isIdentifierChar char = isAlphaNum char || char == '_' || char == '$'

-- Input begins immediately after the first '$'.  The returned delimiter tail
-- includes its closing '$'.
dollarDelimiter :: String -> Maybe (String, String)
dollarDelimiter input =
    let (tag, remaining) = span isTagChar input
        validTag = case tag of
            [] -> True
            first : _ -> isAlpha first || first == '_'
    in case remaining of
        '$' : rest
            | validTag -> Just (tag <> "$", rest)
        _ -> Nothing
  where
    isTagChar char = isAlphaNum char || char == '_'

stripListPrefix :: Eq value => [value] -> [value] -> Maybe [value]
stripListPrefix [] values = Just values
stripListPrefix _ [] = Nothing
stripListPrefix (expected : expectedRest) (actual : actualRest)
    | expected == actual =
        stripListPrefix expectedRest actualRest
    | otherwise = Nothing

validateLimits :: QueryLimits -> Either Text ()
validateLimits limits
    | limits.queryMaxRows <= 0 =
        Left "database query row limit must be positive"
    | limits.queryMaxOutputBytes <= 0 =
        Left "database query output-byte limit must be positive"
    | limits.queryStatementTimeoutMs <= 0 =
        Left "database statement timeout must be positive"
    | limits.queryLockTimeoutMs <= 0 =
        Left "database lock timeout must be positive"
    | otherwise = Right ()

catalogStatement :: Statement Text Value
catalogStatement = mkStatement
    (Text.decodeUtf8 catalogSql)
    (Encoders.param (Encoders.nonNullable Encoders.text))
    jsonbSingleRow
    True

catalogSql :: ByteString.ByteString
catalogSql =
    "with relations as (\
    \  select c.oid, c.relname, c.relkind,\
    \    case c.relkind\
    \      when 'r' then 'table'\
    \      when 'p' then 'partitioned_table'\
    \      when 'v' then 'view'\
    \      when 'm' then 'materialized_view'\
    \      when 'S' then 'sequence'\
    \      else c.relkind::text\
    \    end as object_kind,\
    \    pg_catalog.pg_get_userbyid(c.relowner) as owner,\
    \    obj_description(c.oid, 'pg_class') as comment\
    \  from pg_catalog.pg_class c\
    \  join pg_catalog.pg_namespace n on n.oid = c.relnamespace\
    \  where n.nspname = $1 and c.relkind in ('r','p','v','m','S')\
    \), objects as (\
    \  select r.object_kind as kind, r.relname as name,\
    \    jsonb_build_object(\
    \      'owner', r.owner,\
    \      'comment', r.comment,\
    \      'view_definition', case when r.relkind in ('v','m')\
    \        then pg_catalog.pg_get_viewdef(r.oid, true) else null end,\
    \      'columns', coalesce((\
    \        select jsonb_agg(jsonb_build_object(\
    \          'name', a.attname,\
    \          'type', pg_catalog.format_type(a.atttypid, a.atttypmod),\
    \          'nullable', not a.attnotnull,\
    \          'default', pg_catalog.pg_get_expr(d.adbin, d.adrelid),\
    \          'identity', case a.attidentity\
    \            when 'a' then 'always' when 'd' then 'by_default' else null end,\
    \          'generated', case a.attgenerated\
    \            when 's' then 'stored' when 'v' then 'virtual' else null end,\
    \          'comment', col_description(a.attrelid, a.attnum)\
    \        ) order by a.attnum)\
    \        from pg_catalog.pg_attribute a\
    \        left join pg_catalog.pg_attrdef d\
    \          on d.adrelid = a.attrelid and d.adnum = a.attnum\
    \        where a.attrelid = r.oid and a.attnum > 0 and not a.attisdropped\
    \      ), '[]'::jsonb),\
    \      'constraints', coalesce((\
    \        select jsonb_agg(jsonb_build_object(\
    \          'name', con.conname,\
    \          'type', case con.contype\
    \            when 'p' then 'primary_key'\
    \            when 'u' then 'unique'\
    \            when 'f' then 'foreign_key'\
    \            when 'c' then 'check'\
    \            when 'x' then 'exclusion'\
    \            when 'n' then 'not_null'\
    \            else con.contype::text end,\
    \          'definition', pg_catalog.pg_get_constraintdef(con.oid, true)\
    \        ) order by con.conname)\
    \        from pg_catalog.pg_constraint con\
    \        where con.conrelid = r.oid\
    \      ), '[]'::jsonb),\
    \      'indexes', coalesce((\
    \        select jsonb_agg(jsonb_build_object(\
    \          'name', i.relname,\
    \          'definition', pg_catalog.pg_get_indexdef(ix.indexrelid)\
    \        ) order by i.relname)\
    \        from pg_catalog.pg_index ix\
    \        join pg_catalog.pg_class i on i.oid = ix.indexrelid\
    \        where ix.indrelid = r.oid\
    \      ), '[]'::jsonb)\
    \    ) as definition\
    \  from relations r\
    \)\
    \select coalesce(jsonb_agg(jsonb_build_object(\
    \  'kind', kind, 'name', name, 'definition', definition\
    \) order by kind, name), '[]'::jsonb)\
    \from objects"

customQueryStatement :: QueryLimits -> Text -> Statement () Value
customQueryStatement limits query = mkStatement
    sql
    Encoders.noParams
    jsonbSingleRow
    False
  where
    rowCap = limits.queryMaxRows
    overflowCap
        | rowCap == maxBound = rowCap
        | otherwise = rowCap + 1
    cap = Text.pack (show rowCap)
    capPlusOne = Text.pack (show overflowCap)
    sql =
        "select jsonb_build_object("
            <> "'rows', coalesce(jsonb_agg("
            <> "q._ha_row order by q._ha_row_number"
            <> ") filter (where q._ha_row_number <= " <> cap <> "), '[]'::jsonb),"
            <> "'truncated', coalesce(max(q._ha_row_number), 0) > " <> cap
            <> ") from ("
            <> "select to_jsonb(_ha_data) as _ha_row, "
            <> "row_number() over () as _ha_row_number "
            <> "from (" <> query <> ") as _ha_data "
            <> "limit " <> capPlusOne
            <> ") as q"

jsonbSingleRow :: Decoders.Result Value
jsonbSingleRow =
    Decoders.singleRow $
        Decoders.column (Decoders.nonNullable Decoders.jsonb)

scopeIdentityStatement :: Statement Text Bool
scopeIdentityStatement = mkStatement
    "select session_user = $1 and current_user = $1"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.singleRow $
        Decoders.column (Decoders.nonNullable Decoders.bool))
    True

scopeIdentityError :: Text
scopeIdentityError =
    "custom database pool is not authenticated directly as the selected scope role"

decodeCatalogValue :: Value -> Either Text [CatalogObject]
decodeCatalogValue value =
    case Aeson.fromJSON value of
        Aeson.Success objects -> Right objects
        Aeson.Error err ->
            Left ("could not decode PostgreSQL catalog response: " <> Text.pack err)

decodeQueryEnvelope :: Value -> Either Text CustomQueryResult
decodeQueryEnvelope value =
    case Aeson.fromJSON value of
        Aeson.Success result -> Right result
        Aeson.Error err ->
            Left ("could not decode PostgreSQL query response: " <> Text.pack err)

replaceCatalogProjection
    :: Pool
    -> ScopeDatabase
    -> [CatalogObject]
    -> IO (Either Text ())
replaceCatalogProjection pool database objects =
    case prepareCatalog objects of
        Left err -> pure (Left err)
        Right prepared ->
            runPool pool $
                TxSessions.transactionNoRetry
                    TxSessions.Serializable
                    TxSessions.Write
                    do
                        Tx.statement scopeKey deleteCurrentCatalogStatement
                        _ <- insertCatalogSnapshot scopeKey "current" prepared
                        pure ()
  where
    scopeKey = scopeIdText database.scopeDatabaseScope.scopeId

data CatalogDefinition = CatalogDefinition
    { definitionOwner :: !(Maybe Text)
    , definitionComment :: !(Maybe Text)
    , definitionView :: !(Maybe Text)
    , definitionColumns :: ![CatalogColumn]
    , definitionConstraints :: ![CatalogConstraint]
    , definitionIndexes :: ![CatalogIndex]
    }

data CatalogColumn = CatalogColumn
    { columnName :: !Text
    , columnType :: !Text
    , columnNullable :: !Bool
    , columnDefault :: !(Maybe Text)
    , columnIdentity :: !(Maybe Text)
    , columnGenerated :: !(Maybe Text)
    , columnComment :: !(Maybe Text)
    }

data CatalogConstraint = CatalogConstraint
    { constraintName :: !Text
    , constraintType :: !Text
    , constraintDefinition :: !Text
    }

data CatalogIndex = CatalogIndex
    { indexName :: !Text
    , indexDefinition :: !Text
    }

instance FromJSON CatalogDefinition where
    parseJSON = Aeson.withObject "CatalogDefinition" \object ->
        CatalogDefinition
            <$> object Aeson..:? "owner"
            <*> object Aeson..:? "comment"
            <*> object Aeson..:? "view_definition"
            <*> (object Aeson..:? "columns" Aeson..!= [])
            <*> (object Aeson..:? "constraints" Aeson..!= [])
            <*> (object Aeson..:? "indexes" Aeson..!= [])

instance FromJSON CatalogColumn where
    parseJSON = Aeson.withObject "CatalogColumn" \object ->
        CatalogColumn
            <$> object Aeson..: "name"
            <*> object Aeson..: "type"
            <*> object Aeson..: "nullable"
            <*> object Aeson..:? "default"
            <*> object Aeson..:? "identity"
            <*> object Aeson..:? "generated"
            <*> object Aeson..:? "comment"

instance FromJSON CatalogConstraint where
    parseJSON = Aeson.withObject "CatalogConstraint" \object ->
        CatalogConstraint
            <$> object Aeson..: "name"
            <*> object Aeson..: "type"
            <*> object Aeson..: "definition"

instance FromJSON CatalogIndex where
    parseJSON = Aeson.withObject "CatalogIndex" \object ->
        CatalogIndex
            <$> object Aeson..: "name"
            <*> object Aeson..: "definition"

data PreparedCatalogObject = PreparedCatalogObject
    { preparedKind :: !Text
    , preparedName :: !Text
    , preparedDefinition :: !CatalogDefinition
    }

prepareCatalog :: [CatalogObject] -> Either Text [PreparedCatalogObject]
prepareCatalog = traverse \object ->
    case Aeson.fromJSON object.catalogObjectDefinition of
        Aeson.Error err ->
            Left $
                "could not normalize catalog object "
                    <> object.catalogObjectName <> ": " <> Text.pack err
        Aeson.Success definition ->
            Right PreparedCatalogObject
                { preparedKind = object.catalogObjectKind
                , preparedName = object.catalogObjectName
                , preparedDefinition = definition
                }

insertCatalogSnapshot
    :: Text
    -> Text
    -> [PreparedCatalogObject]
    -> Tx.Transaction Text
insertCatalogSnapshot scopeKey purpose objects = do
    snapshotId <- Tx.statement
        (scopeKey, purpose)
        insertCatalogSnapshotStatement
    forM_ objects \object -> do
        let definition = object.preparedDefinition
        objectId <- Tx.statement
            CatalogObjectParams
                { objectSnapshotId = snapshotId
                , objectKind = object.preparedKind
                , objectName = object.preparedName
                , objectOwner = definition.definitionOwner
                , objectComment = definition.definitionComment
                , objectViewDefinition = definition.definitionView
                }
            insertCatalogObjectStatement
        forM_ (zip [0 ..] definition.definitionColumns) \(ordinal, column) ->
            Tx.statement
                CatalogColumnParams
                    { catalogColumnObjectId = objectId
                    , catalogColumnOrdinal = ordinal
                    , catalogColumnName = column.columnName
                    , catalogColumnType = column.columnType
                    , catalogColumnNullable = column.columnNullable
                    , catalogColumnDefault = column.columnDefault
                    , catalogColumnIdentity = column.columnIdentity
                    , catalogColumnGenerated = column.columnGenerated
                    , catalogColumnComment = column.columnComment
                    }
                insertCatalogColumnStatement
        forM_ (zip [0 ..] definition.definitionConstraints)
            \(ordinal, constraint) ->
                Tx.statement
                    CatalogConstraintParams
                        { catalogConstraintObjectId = objectId
                        , catalogConstraintOrdinal = ordinal
                        , catalogConstraintName = constraint.constraintName
                        , catalogConstraintType = constraint.constraintType
                        , catalogConstraintDefinition =
                            constraint.constraintDefinition
                        }
                    insertCatalogConstraintStatement
        forM_ (zip [0 ..] definition.definitionIndexes) \(ordinal, index) ->
            Tx.statement
                CatalogIndexParams
                    { catalogIndexObjectId = objectId
                    , catalogIndexOrdinal = ordinal
                    , catalogIndexName = index.indexName
                    , catalogIndexDefinition = index.indexDefinition
                    }
                insertCatalogIndexStatement
    pure snapshotId

deleteCurrentCatalogStatement :: Statement Text ()
deleteCurrentCatalogStatement = mkStatement
    "delete from harness.custom_catalog_snapshots snapshot\
    \ using harness.custom_scopes scope\
    \ where snapshot.scope_id = scope.scope_id\
    \ and scope.scope_key = $1\
    \ and snapshot.snapshot_purpose = 'current'"
    textParam
    Decoders.noResult
    True

insertCatalogSnapshotStatement :: Statement (Text, Text) Text
insertCatalogSnapshotStatement = mkStatement
    "insert into harness.custom_catalog_snapshots\
    \ (scope_id, snapshot_purpose)\
    \ select scope_id, $2 from harness.custom_scopes where scope_key = $1\
    \ returning snapshot_id::text"
    ( (fst >$< textParam)
        <> (snd >$< textParam)
    )
    textSingleRow
    True

data CatalogObjectParams = CatalogObjectParams
    { objectSnapshotId :: !Text
    , objectKind :: !Text
    , objectName :: !Text
    , objectOwner :: !(Maybe Text)
    , objectComment :: !(Maybe Text)
    , objectViewDefinition :: !(Maybe Text)
    }

insertCatalogObjectStatement :: Statement CatalogObjectParams Text
insertCatalogObjectStatement = mkStatement
    "insert into harness.custom_catalog_objects\
    \ (snapshot_id, object_kind, object_name, owner_name,\
    \  object_comment, view_definition)\
    \ values ($1::uuid, $2, $3, $4, $5, $6)\
    \ returning catalog_object_id::text"
    ( ((.objectSnapshotId) >$< textParam)
        <> ((.objectKind) >$< textParam)
        <> ((.objectName) >$< textParam)
        <> ((.objectOwner) >$< nullableTextParam)
        <> ((.objectComment) >$< nullableTextParam)
        <> ((.objectViewDefinition) >$< nullableTextParam)
    )
    textSingleRow
    True

data CatalogColumnParams = CatalogColumnParams
    { catalogColumnObjectId :: !Text
    , catalogColumnOrdinal :: !Int32
    , catalogColumnName :: !Text
    , catalogColumnType :: !Text
    , catalogColumnNullable :: !Bool
    , catalogColumnDefault :: !(Maybe Text)
    , catalogColumnIdentity :: !(Maybe Text)
    , catalogColumnGenerated :: !(Maybe Text)
    , catalogColumnComment :: !(Maybe Text)
    }

insertCatalogColumnStatement :: Statement CatalogColumnParams ()
insertCatalogColumnStatement = mkStatement
    "insert into harness.custom_catalog_columns\
    \ (catalog_object_id, column_ordinal, column_name, data_type,\
    \  is_nullable, default_expression, identity_kind, generated_kind,\
    \  column_comment)\
    \ values ($1::uuid, $2, $3, $4, $5, $6, $7, $8, $9)"
    ( ((.catalogColumnObjectId) >$< textParam)
        <> ((.catalogColumnOrdinal) >$< intParam)
        <> ((.catalogColumnName) >$< textParam)
        <> ((.catalogColumnType) >$< textParam)
        <> ((.catalogColumnNullable) >$< boolParam)
        <> ((.catalogColumnDefault) >$< nullableTextParam)
        <> ((.catalogColumnIdentity) >$< nullableTextParam)
        <> ((.catalogColumnGenerated) >$< nullableTextParam)
        <> ((.catalogColumnComment) >$< nullableTextParam)
    )
    Decoders.noResult
    True

data CatalogConstraintParams = CatalogConstraintParams
    { catalogConstraintObjectId :: !Text
    , catalogConstraintOrdinal :: !Int32
    , catalogConstraintName :: !Text
    , catalogConstraintType :: !Text
    , catalogConstraintDefinition :: !Text
    }

insertCatalogConstraintStatement :: Statement CatalogConstraintParams ()
insertCatalogConstraintStatement = mkStatement
    "insert into harness.custom_catalog_constraints\
    \ (catalog_object_id, constraint_ordinal, constraint_name,\
    \  constraint_kind, constraint_definition)\
    \ values ($1::uuid, $2, $3, $4, $5)"
    ( ((.catalogConstraintObjectId) >$< textParam)
        <> ((.catalogConstraintOrdinal) >$< intParam)
        <> ((.catalogConstraintName) >$< textParam)
        <> ((.catalogConstraintType) >$< textParam)
        <> ((.catalogConstraintDefinition) >$< textParam)
    )
    Decoders.noResult
    True

data CatalogIndexParams = CatalogIndexParams
    { catalogIndexObjectId :: !Text
    , catalogIndexOrdinal :: !Int32
    , catalogIndexName :: !Text
    , catalogIndexDefinition :: !Text
    }

insertCatalogIndexStatement :: Statement CatalogIndexParams ()
insertCatalogIndexStatement = mkStatement
    "insert into harness.custom_catalog_indexes\
    \ (catalog_object_id, index_ordinal, index_name, index_definition)\
    \ values ($1::uuid, $2, $3, $4)"
    ( ((.catalogIndexObjectId) >$< textParam)
        <> ((.catalogIndexOrdinal) >$< intParam)
        <> ((.catalogIndexName) >$< textParam)
        <> ((.catalogIndexDefinition) >$< textParam)
    )
    Decoders.noResult
    True

finishCommittedAttempt
    :: Pool
    -> Pool
    -> ScopeDatabase
    -> Text
    -> [CatalogObject]
    -> IO (Either Text CustomExecutionResult)
finishCommittedAttempt trustedPool scopePool database auditId before =
    inspectCustomSchema scopePool database >>= \case
        Left err ->
            pure $ Right CustomExecutionResult
                { customExecutionAuditId = auditId
                , customExecutionCatalogBefore = before
                , customExecutionCatalogAfter = before
                , customExecutionWarning = Just $
                    unknownAuditWarning auditId
                        ("catalog refresh failed: " <> err)
                }
        Right after ->
            replaceCatalogProjection trustedPool database after >>= \case
                Left err ->
                    pure $ Right CustomExecutionResult
                        { customExecutionAuditId = auditId
                        , customExecutionCatalogBefore = before
                        , customExecutionCatalogAfter = after
                        , customExecutionWarning = Just $
                            unknownAuditWarning auditId
                                ("catalog projection failed: " <> err)
                        }
                Right () -> do
                    finishedAt <- getCurrentTime
                    recordAuditFinished
                        trustedPool auditId finishedAt True Nothing after
                        >>= \case
                            Left err ->
                                pure $ Right CustomExecutionResult
                                    { customExecutionAuditId = auditId
                                    , customExecutionCatalogBefore = before
                                    , customExecutionCatalogAfter = after
                                    , customExecutionWarning = Just $
                                        unknownAuditWarning auditId
                                            ("audit finalization failed: " <> err)
                                    }
                            Right () ->
                                pure $ Right CustomExecutionResult
                                    { customExecutionAuditId = auditId
                                    , customExecutionCatalogBefore = before
                                    , customExecutionCatalogAfter = after
                                    , customExecutionWarning = Nothing
                                    }

finalizeFailedAttempt
    :: Pool
    -> Text
    -> Text
    -> [CatalogObject]
    -> IO (Either Text result)
finalizeFailedAttempt trustedPool auditId executionError after = do
    finishedAt <- getCurrentTime
    recordAuditFinished
        trustedPool
        auditId
        finishedAt
        False
        (Just executionError)
        after
        >>= \case
            Left auditError ->
                pure $ Left $
                    executionError
                        <> " (audit_id=" <> auditId
                        <> "; failed attempt finalization was not confirmed: "
                        <> auditError <> ")"
            Right () ->
                pure $ Left $
                    executionError
                        <> " (audit_id=" <> auditId <> ")"

recordAuditStarted
    :: Pool
    -> ScopeDatabase
    -> CustomAuditContext
    -> Text
    -> Text
    -> UTCTime
    -> [CatalogObject]
    -> IO (Either Text Text)
recordAuditStarted pool database audit purpose sql startedAt before =
    case prepareCatalog before of
        Left err -> pure (Left err)
        Right prepared ->
            runPool pool $
                TxSessions.transactionNoRetry
                    TxSessions.Serializable
                    TxSessions.Write
                    do
                        snapshotId <- insertCatalogSnapshot
                            (scopeIdText database.scopeDatabaseScope.scopeId)
                            "audit_before"
                            prepared
                        Tx.statement
                            AuditStartedParams
                                { startSessionId = audit.customAuditSessionId
                                , startAgentId = audit.customAuditAgentId
                                , startScopeKey =
                                    scopeIdText database.scopeDatabaseScope.scopeId
                                , startPurpose = purpose
                                , startSql = sql
                                , startAt = startedAt
                                , startSnapshotId = snapshotId
                                }
                            auditStartedStatement

recordAuditFinished
    :: Pool
    -> Text
    -> UTCTime
    -> Bool
    -> Maybe Text
    -> [CatalogObject]
    -> IO (Either Text ())
recordAuditFinished pool auditId finishedAt succeeded errorText after =
    case prepareCatalog after of
        Left err -> pure (Left err)
        Right prepared -> do
            result <- runPool pool $
                TxSessions.transactionNoRetry
                    TxSessions.Serializable
                    TxSessions.Write
                    do
                        scopeKey <- Tx.statement auditId auditScopeStatement
                        snapshotId <- insertCatalogSnapshot
                            scopeKey
                            "audit_after"
                            prepared
                        updated <- Tx.statement
                            AuditFinishedParams
                                { finishAuditId = auditId
                                , finishAt = finishedAt
                                , finishSucceeded = succeeded
                                , finishError = errorText
                                , finishSnapshotId = snapshotId
                                }
                            auditFinishedStatement
                        unless updated Tx.condemn
                        pure updated
            case result of
                Left err -> pure (Left err)
                Right False ->
                    pure $ Left $
                        "audit row was not in started state: " <> auditId
                Right True -> pure (Right ())

data AuditStartedParams = AuditStartedParams
    { startSessionId :: !(Maybe Text)
    , startAgentId :: !(Maybe Text)
    , startScopeKey :: !Text
    , startPurpose :: !Text
    , startSql :: !Text
    , startAt :: !UTCTime
    , startSnapshotId :: !Text
    }

auditStartedStatement :: Statement AuditStartedParams Text
auditStartedStatement = mkStatement
    "insert into harness.custom_sql_audit\
    \ (session_id, agent_id, scope_id, purpose, sql_text, sql_sha256,\
    \  started_at, status, catalog_before_snapshot_id)\
    \ select $1, $2, scope_id, $4, $5,\
    \  encode(public.digest(convert_to($5, 'UTF8'), 'sha256'), 'hex'),\
    \  $6, 'started', $7::uuid\
    \ from harness.custom_scopes where scope_key = $3\
    \ returning audit_id::text"
    ( ((.startSessionId) >$< nullableTextParam)
        <> ((.startAgentId) >$< nullableTextParam)
        <> ((.startScopeKey) >$< textParam)
        <> ((.startPurpose) >$< textParam)
        <> ((.startSql) >$< textParam)
        <> ((.startAt) >$< timeParam)
        <> ((.startSnapshotId) >$< textParam)
    )
    textSingleRow
    True

auditScopeStatement :: Statement Text Text
auditScopeStatement = mkStatement
    "select scope.scope_key\
    \ from harness.custom_sql_audit audit\
    \ join harness.custom_scopes scope on scope.scope_id = audit.scope_id\
    \ where audit.audit_id = $1::uuid"
    textParam
    textSingleRow
    True

data AuditFinishedParams = AuditFinishedParams
    { finishAuditId :: !Text
    , finishAt :: !UTCTime
    , finishSucceeded :: !Bool
    , finishError :: !(Maybe Text)
    , finishSnapshotId :: !Text
    }

auditFinishedStatement :: Statement AuditFinishedParams Bool
auditFinishedStatement = mkStatement
    "update harness.custom_sql_audit\
    \ set finished_at = $2,\
    \     status = case when $3 then 'succeeded' else 'failed' end,\
    \     succeeded = $3,\
    \     error_text = $4,\
    \     catalog_after_snapshot_id = $5::uuid\
    \ where audit_id = $1::uuid and status = 'started'"
    ( ((.finishAuditId) >$< textParam)
        <> ((.finishAt) >$< timeParam)
        <> ((.finishSucceeded) >$< boolParam)
        <> ((.finishError) >$< nullableTextParam)
        <> ((.finishSnapshotId) >$< textParam)
    )
    (fmap (> 0) Decoders.rowsAffected)
    True

textParam :: Encoders.Params Text
textParam = Encoders.param (Encoders.nonNullable Encoders.text)

nullableTextParam :: Encoders.Params (Maybe Text)
nullableTextParam = Encoders.param (Encoders.nullable Encoders.text)

intParam :: Encoders.Params Int32
intParam = Encoders.param (Encoders.nonNullable Encoders.int4)

boolParam :: Encoders.Params Bool
boolParam = Encoders.param (Encoders.nonNullable Encoders.bool)

timeParam :: Encoders.Params UTCTime
timeParam = Encoders.param (Encoders.nonNullable Encoders.timestamptz)

textSingleRow :: Decoders.Result Text
textSingleRow =
    Decoders.singleRow $
        Decoders.column (Decoders.nonNullable Decoders.text)

confinementSql :: ScopeDatabase -> QueryLimits -> ByteString.ByteString
confinementSql database limits =
    Text.encodeUtf8 $
        "set local search_path = "
            <> quoteIdentifier database.scopeDatabaseSchema
            <> ", pg_catalog;"
            <> " set local standard_conforming_strings = on;"
            <> " set local statement_timeout = "
            <> Text.pack (show (max 1 limits.queryStatementTimeoutMs))
            <> "; set local lock_timeout = "
            <> Text.pack (show (max 1 limits.queryLockTimeoutMs))
            <> ";"

quoteIdentifier :: Text -> Text
quoteIdentifier value =
    "\"" <> Text.replace "\"" "\"\"" value <> "\""

encodedSize :: Value -> Int
encodedSize = fromIntegral . LazyByteString.length . Aeson.encode

runPool :: Pool -> Session.Session a -> IO (Either Text a)
runPool pool session =
    Pool.use pool session >>= \case
        Left err -> pure (Left (Text.pack (show err)))
        Right value -> pure (Right value)

unknownAuditWarning :: Text -> Text -> Text
unknownAuditWarning auditId detail =
    "SQL committed, but post-commit bookkeeping was not completed; "
        <> "audit_id=" <> auditId
        <> " remains started/unknown for storage doctor reconciliation: "
        <> detail
