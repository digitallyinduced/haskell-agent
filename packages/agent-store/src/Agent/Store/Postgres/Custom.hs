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
    , CatalogDefinition(..)
    , CatalogColumn(..)
    , CatalogConstraint(..)
    , CatalogIndex(..)
    , CustomQueryResult(..)
    , CustomAuditContext(..)
    , CustomExecutionResult(..)
    , inspectCustomSchema
    , inspectCustomSchemaSequential
    , queryCustom
    , queryCustomJson
    , executeCustom
    , normalizeCustomQuery
    , normalizeCustomExecution
    ) where

import Control.Monad (forM_, unless)
import qualified Data.ByteString as ByteString
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Pipeline as Pipeline
import Hasql.Pool (Pool)
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as TxSessions

import Agent.Store.Custom.QueryResult
    ( CustomQueryResult(..)
    , customJsonQueryStatement
    , customQueryStatement
    )
import Agent.Store.Postgres.Scope
    ( Scope(..)
    , ScopeDatabase(..)
    , scopeIdText
    )
import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.Postgres.Sql (quoteIdentifier)
import Agent.Store.Postgres.Custom.Sql
    ( normalizeCustomExecution
    , normalizeCustomQuery
    )
import Agent.Store.Postgres.Custom.Types

inspectCustomSchema
    :: Pool
    -> ScopeDatabase
    -> IO (Either Text [CatalogObject])
inspectCustomSchema =
    inspectCustomSchemaWith catalogSchemaRowsPipelined

-- | Sequential baseline retained for the catalog-inspection benchmark.
inspectCustomSchemaSequential
    :: Pool
    -> ScopeDatabase
    -> IO (Either Text [CatalogObject])
inspectCustomSchemaSequential =
    inspectCustomSchemaWith catalogSchemaRowsSequential

inspectCustomSchemaWith
    :: (Text -> Session.Session CatalogSchemaRows)
    -> Pool
    -> ScopeDatabase
    -> IO (Either Text [CatalogObject])
inspectCustomSchemaWith loadCatalogRows scopePool database =
    runPool scopePool session >>= \case
        Left err -> pure (Left err)
        Right (Left err) -> pure (Left err)
        Right (Right value) -> pure (Right value)
  where
    session = do
        expectedIdentity <- Session.statement
            database.scopeDatabaseRole
            scopeIdentityStatement
        if not expectedIdentity
            then pure (Left scopeIdentityError)
            else do
                (objects, columns, constraints, indexes) <-
                    loadCatalogRows database.scopeDatabaseSchema
                pure $ Right $
                    assembleCatalog objects columns constraints indexes

type CatalogSchemaRows =
    ( [CatalogObjectRow]
    , [CatalogColumnRow]
    , [CatalogConstraintRow]
    , [CatalogIndexRow]
    )

catalogSchemaRowsPipelined :: Text -> Session.Session CatalogSchemaRows
catalogSchemaRowsPipelined schema =
    Session.pipeline $
        (,,,)
            <$> Pipeline.statement schema catalogObjectsStatement
            <*> Pipeline.statement schema catalogColumnsStatement
            <*> Pipeline.statement schema catalogConstraintsStatement
            <*> Pipeline.statement schema catalogIndexesStatement

catalogSchemaRowsSequential :: Text -> Session.Session CatalogSchemaRows
catalogSchemaRowsSequential schema = do
    objects <- Session.statement schema catalogObjectsStatement
    columns <- Session.statement schema catalogColumnsStatement
    constraints <- Session.statement schema catalogConstraintsStatement
    indexes <- Session.statement schema catalogIndexesStatement
    pure (objects, columns, constraints, indexes)

queryCustom
    :: Pool
    -> ScopeDatabase
    -> QueryLimits
    -> Text
    -> IO (Either Text CustomQueryResult)
queryCustom = queryCustomWith customQueryStatement

-- | Execute a confined read-only query and return its rows as one JSON array.
-- This is reserved for trusted structured clients; agent-facing database
-- queries continue to use the bounded human-readable representation.
queryCustomJson
    :: Pool
    -> ScopeDatabase
    -> QueryLimits
    -> Text
    -> IO (Either Text CustomQueryResult)
queryCustomJson = queryCustomWith customJsonQueryStatement

queryCustomWith
    :: (Int64 -> Text -> Statement () CustomQueryResult)
    -> Pool
    -> ScopeDatabase
    -> QueryLimits
    -> Text
    -> IO (Either Text CustomQueryResult)
queryCustomWith statementFor scopePool database limits rawQuery =
    case validateLimits limits *> normalizeCustomQuery rawQuery of
        Left err -> pure (Left err)
        Right query -> do
            let statement =
                    statementFor limits.queryMaxRows query
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
                Right (Right result)
                    | encodedSize result.customQueryOutput
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

data CatalogObjectRow = CatalogObjectRow
    { catalogRowId :: !Text
    , catalogRowKind :: !Text
    , catalogRowName :: !Text
    , catalogRowOwner :: !(Maybe Text)
    , catalogRowComment :: !(Maybe Text)
    , catalogRowView :: !(Maybe Text)
    }

data CatalogColumnRow = CatalogColumnRow
    { catalogColumnRowObjectId :: !Text
    , catalogColumnRowValue :: !CatalogColumn
    }

data CatalogConstraintRow = CatalogConstraintRow
    { catalogConstraintRowObjectId :: !Text
    , catalogConstraintRowValue :: !CatalogConstraint
    }

data CatalogIndexRow = CatalogIndexRow
    { catalogIndexRowObjectId :: !Text
    , catalogIndexRowValue :: !CatalogIndex
    }

assembleCatalog
    :: [CatalogObjectRow]
    -> [CatalogColumnRow]
    -> [CatalogConstraintRow]
    -> [CatalogIndexRow]
    -> [CatalogObject]
assembleCatalog objects columns constraints indexes =
    map assembleObject objects
  where
    columnsByObject =
        Map.map reverse $
            Map.fromListWith (<>)
                [ (row.catalogColumnRowObjectId, [row.catalogColumnRowValue])
                | row <- columns
                ]
    constraintsByObject =
        Map.map reverse $
            Map.fromListWith (<>)
                [ (row.catalogConstraintRowObjectId, [row.catalogConstraintRowValue])
                | row <- constraints
                ]
    indexesByObject =
        Map.map reverse $
            Map.fromListWith (<>)
                [ (row.catalogIndexRowObjectId, [row.catalogIndexRowValue])
                | row <- indexes
                ]
    assembleObject :: CatalogObjectRow -> CatalogObject
    assembleObject row = CatalogObject
        { catalogObjectKind = row.catalogRowKind
        , catalogObjectName = row.catalogRowName
        , catalogObjectDefinition = CatalogDefinition
            { definitionOwner = row.catalogRowOwner
            , definitionComment = row.catalogRowComment
            , definitionView = row.catalogRowView
            , definitionColumns =
                Map.findWithDefault [] row.catalogRowId columnsByObject
            , definitionConstraints =
                Map.findWithDefault [] row.catalogRowId constraintsByObject
            , definitionIndexes =
                Map.findWithDefault [] row.catalogRowId indexesByObject
            }
        }

catalogObjectsStatement :: Statement Text [CatalogObjectRow]
catalogObjectsStatement = mkStatement
    "select c.oid::text,\
    \ case c.relkind\
    \   when 'r' then 'table'\
    \   when 'p' then 'partitioned_table'\
    \   when 'v' then 'view'\
    \   when 'm' then 'materialized_view'\
    \   when 'S' then 'sequence'\
    \   else c.relkind::text\
    \ end,\
    \ c.relname::text,\
    \ pg_catalog.pg_get_userbyid(c.relowner)::text,\
    \ obj_description(c.oid, 'pg_class'),\
    \ case when c.relkind in ('v','m')\
    \   then pg_catalog.pg_get_viewdef(c.oid, true) else null end\
    \ from pg_catalog.pg_class c\
    \ join pg_catalog.pg_namespace n on n.oid = c.relnamespace\
    \ where n.nspname = $1 and c.relkind in ('r','p','v','m','S')\
    \ order by 2, c.relname"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowList $
        CatalogObjectRow
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text))
    True

catalogColumnsStatement :: Statement Text [CatalogColumnRow]
catalogColumnsStatement = mkStatement
    "select a.attrelid::text, a.attname::text,\
    \ pg_catalog.format_type(a.atttypid, a.atttypmod),\
    \ not a.attnotnull,\
    \ pg_catalog.pg_get_expr(d.adbin, d.adrelid),\
    \ case a.attidentity\
    \   when 'a' then 'always' when 'd' then 'by_default' else null end,\
    \ case a.attgenerated\
    \   when 's' then 'stored' when 'v' then 'virtual' else null end,\
    \ col_description(a.attrelid, a.attnum)\
    \ from pg_catalog.pg_attribute a\
    \ join pg_catalog.pg_class c on c.oid = a.attrelid\
    \ join pg_catalog.pg_namespace n on n.oid = c.relnamespace\
    \ left join pg_catalog.pg_attrdef d\
    \   on d.adrelid = a.attrelid and d.adnum = a.attnum\
    \ where n.nspname = $1 and c.relkind in ('r','p','v','m','S')\
    \   and a.attnum > 0 and not a.attisdropped\
    \ order by a.attrelid, a.attnum"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowList $
        CatalogColumnRow
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> (CatalogColumn
                <$> Decoders.column (Decoders.nonNullable Decoders.text)
                <*> Decoders.column (Decoders.nonNullable Decoders.text)
                <*> Decoders.column (Decoders.nonNullable Decoders.bool)
                <*> Decoders.column (Decoders.nullable Decoders.text)
                <*> Decoders.column (Decoders.nullable Decoders.text)
                <*> Decoders.column (Decoders.nullable Decoders.text)
                <*> Decoders.column (Decoders.nullable Decoders.text)))
    True

catalogConstraintsStatement :: Statement Text [CatalogConstraintRow]
catalogConstraintsStatement = mkStatement
    "select con.conrelid::text, con.conname::text,\
    \ case con.contype\
    \   when 'p' then 'primary_key'\
    \   when 'u' then 'unique'\
    \   when 'f' then 'foreign_key'\
    \   when 'c' then 'check'\
    \   when 'x' then 'exclusion'\
    \   when 'n' then 'not_null'\
    \   else con.contype::text end,\
    \ pg_catalog.pg_get_constraintdef(con.oid, true)\
    \ from pg_catalog.pg_constraint con\
    \ join pg_catalog.pg_class c on c.oid = con.conrelid\
    \ join pg_catalog.pg_namespace n on n.oid = c.relnamespace\
    \ where n.nspname = $1 and c.relkind in ('r','p','v','m','S')\
    \ order by con.conrelid, con.conname"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowList $
        CatalogConstraintRow
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> (CatalogConstraint
                <$> Decoders.column (Decoders.nonNullable Decoders.text)
                <*> Decoders.column (Decoders.nonNullable Decoders.text)
                <*> Decoders.column (Decoders.nonNullable Decoders.text)))
    True

catalogIndexesStatement :: Statement Text [CatalogIndexRow]
catalogIndexesStatement = mkStatement
    "select ix.indrelid::text, i.relname::text,\
    \ pg_catalog.pg_get_indexdef(ix.indexrelid)\
    \ from pg_catalog.pg_index ix\
    \ join pg_catalog.pg_class c on c.oid = ix.indrelid\
    \ join pg_catalog.pg_namespace n on n.oid = c.relnamespace\
    \ join pg_catalog.pg_class i on i.oid = ix.indexrelid\
    \ where n.nspname = $1 and c.relkind in ('r','p','v','m','S')\
    \ order by ix.indrelid, i.relname"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowList $
        CatalogIndexRow
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> (CatalogIndex <$> Decoders.column (Decoders.nonNullable Decoders.text) <*> Decoders.column (Decoders.nonNullable Decoders.text)))
    True

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

replaceCatalogProjection
    :: Pool
    -> ScopeDatabase
    -> [CatalogObject]
    -> IO (Either Text ())
replaceCatalogProjection pool database objects =
    runPool pool $
        TxSessions.transactionNoRetry
            TxSessions.Serializable
            TxSessions.Write
            do
                Tx.statement scopeKey deleteCurrentCatalogStatement
                _ <- insertCatalogSnapshot scopeKey "current" objects
                pure ()
  where
    scopeKey = scopeIdText database.scopeDatabaseScope.scopeId

insertCatalogSnapshot
    :: Text
    -> Text
    -> [CatalogObject]
    -> Tx.Transaction Text
insertCatalogSnapshot scopeKey purpose objects = do
    snapshotId <- Tx.statement
        (scopeKey, purpose)
        insertCatalogSnapshotStatement
    forM_ objects \object -> do
        let definition = object.catalogObjectDefinition
        objectId <- Tx.statement
            CatalogObjectParams
                { objectSnapshotId = snapshotId
                , objectKind = object.catalogObjectKind
                , objectName = object.catalogObjectName
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
    (Encoders.param (Encoders.nonNullable Encoders.text))
    Decoders.noResult
    True

insertCatalogSnapshotStatement :: Statement (Text, Text) Text
insertCatalogSnapshotStatement = mkStatement
    "insert into harness.custom_catalog_snapshots\
    \ (scope_id, snapshot_purpose)\
    \ select scope_id, $2 from harness.custom_scopes where scope_key = $1\
    \ returning snapshot_id::text"
    ( (fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.text))
    )
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))
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
    ( ((.objectSnapshotId) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.objectKind) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.objectName) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.objectOwner) >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((.objectComment) >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((.objectViewDefinition) >$< Encoders.param (Encoders.nullable Encoders.text))
    )
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))
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
    ( ((.catalogColumnObjectId) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.catalogColumnOrdinal) >$< Encoders.param (Encoders.nonNullable Encoders.int4))
        <> ((.catalogColumnName) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.catalogColumnType) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.catalogColumnNullable) >$< Encoders.param (Encoders.nonNullable Encoders.bool))
        <> ((.catalogColumnDefault) >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((.catalogColumnIdentity) >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((.catalogColumnGenerated) >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((.catalogColumnComment) >$< Encoders.param (Encoders.nullable Encoders.text))
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
    ( ((.catalogConstraintObjectId) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.catalogConstraintOrdinal) >$< Encoders.param (Encoders.nonNullable Encoders.int4))
        <> ((.catalogConstraintName) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.catalogConstraintType) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.catalogConstraintDefinition) >$< Encoders.param (Encoders.nonNullable Encoders.text))
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
    ( ((.catalogIndexObjectId) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.catalogIndexOrdinal) >$< Encoders.param (Encoders.nonNullable Encoders.int4))
        <> ((.catalogIndexName) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.catalogIndexDefinition) >$< Encoders.param (Encoders.nonNullable Encoders.text))
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
    runPool pool $
        TxSessions.transactionNoRetry
            TxSessions.Serializable
            TxSessions.Write
            do
                snapshotId <- insertCatalogSnapshot
                    (scopeIdText database.scopeDatabaseScope.scopeId)
                    "audit_before"
                    before
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
    do
        result <- runPool pool $
            TxSessions.transactionNoRetry
                TxSessions.Serializable
                TxSessions.Write
                do
                    scopeKey <- Tx.statement auditId auditScopeStatement
                    snapshotId <- insertCatalogSnapshot
                        scopeKey
                        "audit_after"
                        after
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
    ( ((.startSessionId) >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((.startAgentId) >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((.startScopeKey) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.startPurpose) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.startSql) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.startAt) >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))
        <> ((.startSnapshotId) >$< Encoders.param (Encoders.nonNullable Encoders.text))
    )
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))
    True

auditScopeStatement :: Statement Text Text
auditScopeStatement = mkStatement
    "select scope.scope_key\
    \ from harness.custom_sql_audit audit\
    \ join harness.custom_scopes scope on scope.scope_id = audit.scope_id\
    \ where audit.audit_id = $1::uuid"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))
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
    ( ((.finishAuditId) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.finishAt) >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))
        <> ((.finishSucceeded) >$< Encoders.param (Encoders.nonNullable Encoders.bool))
        <> ((.finishError) >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((.finishSnapshotId) >$< Encoders.param (Encoders.nonNullable Encoders.text))
    )
    (fmap (> 0) Decoders.rowsAffected)
    True

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

encodedSize :: Text -> Int
encodedSize = ByteString.length . Text.encodeUtf8

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
