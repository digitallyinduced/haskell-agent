{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | PostgreSQL identities for agent-created structured data.
--
-- Each logical scope owns a dedicated PostgreSQL role and schema.  Model
-- authored SQL connects directly as that role, so PostgreSQL privileges are
-- the primary boundary between user, repository, checkout, and harness data.
module Agent.Store.Postgres.Scope
    ( ScopeKind(..)
    , ScopeId
    , Scope(..)
    , ScopeDatabase(..)
    , mkScopeId
    , deriveScopeId
    , scopeIdText
    , scopeKindText
    , scopeDatabaseFor
    , customSchemaStatements
    , migrateCustomSchema
    , provisionScope
    , lookupScopeDatabase
    , openScopeStorePool
    ) where

import Control.Monad (forM_)
import qualified Data.ByteString as ByteString
import Data.Char (isHexDigit)
import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Pool (Pool)
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)
import qualified Hasql.Transaction as Tx
import qualified Hasql.Transaction.Sessions as TxSessions

import Agent.Store.Postgres.Config (ManagedPostgresConfig)
import Agent.Store.Postgres.Connection
    ( PoolConfig
    , StorePool
    , openRoleStorePool
    )
import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.Types (StoreError)

data ScopeKind
    = UserScope
    | RepositoryScope
    | CheckoutScope
    deriving (Eq, Ord, Show)

newtype ScopeId = ScopeId Text
    deriving (Eq, Ord, Show)

data Scope = Scope
    { scopeKind :: !ScopeKind
    , scopeId :: !ScopeId
    }
    deriving (Eq, Ord, Show)

data ScopeDatabase = ScopeDatabase
    { scopeDatabaseScope :: !Scope
    , scopeDatabaseRole :: !Text
    , scopeDatabaseSchema :: !Text
    }
    deriving (Eq, Show)

-- | Accept a UUID-shaped identifier and normalize it to 32 lower-case hex
-- characters.  Restricting the alphabet makes derived PostgreSQL identifiers
-- safe without accepting caller-controlled SQL syntax.
mkScopeId :: Text -> Either Text ScopeId
mkScopeId raw =
    let compact = Text.toLower (Text.filter (/= '-') (Text.strip raw))
    in if Text.length compact == 32 && Text.all isHexDigit compact
        then Right (ScopeId compact)
        else Left "scope id must be a UUID (32 hex digits, with optional hyphens)"

-- | Derive a stable 128-bit scope identifier from a caller-resolved logical
-- key.  The digest is computed by PostgreSQL's pgcrypto extension so CLI
-- callers do not need a second cryptography implementation.
--
-- Scope kind and length-delimited key material are both included in the
-- digest domain.  The caller remains responsible for choosing a canonical,
-- stable key such as a Git common directory or worktree Git directory.
deriveScopeId :: Pool -> ScopeKind -> Text -> IO (Either Text ScopeId)
deriveScopeId pool kind stableKey
    | Text.null stableKey =
        pure (Left "scope stable key must not be empty")
    | Text.any (== '\NUL') stableKey =
        pure (Left "scope stable key contains a NUL byte")
    | otherwise =
        Pool.use pool
            (Session.statement
                (scopeKindText kind, stableKey)
                deriveScopeIdStatement)
            >>= \case
                Left err -> pure (Left (Text.pack (show err)))
                Right digest -> pure (mkScopeId digest)

scopeIdText :: ScopeId -> Text
scopeIdText (ScopeId value) = value

scopeKindText :: ScopeKind -> Text
scopeKindText = \case
    UserScope -> "user"
    RepositoryScope -> "repository"
    CheckoutScope -> "checkout"

scopeDatabaseFor :: Scope -> ScopeDatabase
scopeDatabaseFor scope =
    let suffix =
            kindPrefix scope.scopeKind
                <> "_"
                <> scopeIdText scope.scopeId
    in ScopeDatabase
        { scopeDatabaseScope = scope
        , scopeDatabaseRole = "ha_scope_" <> suffix
        , scopeDatabaseSchema = "custom_" <> suffix
        }

-- | Harness-owned catalog, audit, and scope-registry DDL.  The generated
-- custom roles receive no privileges on these objects.
customSchemaStatements :: [ByteString.ByteString]
customSchemaStatements =
    [ "CREATE EXTENSION IF NOT EXISTS pgcrypto"
    , "REVOKE ALL ON SCHEMA harness FROM PUBLIC"
    , "REVOKE CREATE ON SCHEMA public FROM PUBLIC"
    , "CREATE TABLE IF NOT EXISTS harness.custom_scopes (\
      \ scope_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7(),\
      \ scope_key text NOT NULL UNIQUE,\
      \ scope_kind text NOT NULL\
      \   CHECK (scope_kind IN ('user', 'repository', 'checkout')),\
      \ role_name name NOT NULL UNIQUE,\
      \ schema_name name NOT NULL UNIQUE,\
      \ created_at timestamptz NOT NULL DEFAULT now(),\
      \ updated_at timestamptz NOT NULL DEFAULT now()\
      \ )"
    , "CREATE TABLE IF NOT EXISTS harness.custom_catalog_snapshots (\
      \ snapshot_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7(),\
      \ scope_id uuid NOT NULL\
      \   REFERENCES harness.custom_scopes(scope_id) ON DELETE CASCADE,\
      \ snapshot_purpose text NOT NULL\
      \   CHECK (snapshot_purpose IN ('current', 'audit_before', 'audit_after')),\
      \ captured_at timestamptz NOT NULL DEFAULT now()\
      \ )"
    , "CREATE UNIQUE INDEX IF NOT EXISTS custom_catalog_current_scope_idx\
      \ ON harness.custom_catalog_snapshots (scope_id)\
      \ WHERE snapshot_purpose = 'current'"
    , "CREATE TABLE IF NOT EXISTS harness.custom_catalog_objects (\
      \ catalog_object_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7(),\
      \ snapshot_id uuid NOT NULL\
      \   REFERENCES harness.custom_catalog_snapshots(snapshot_id)\
      \   ON DELETE CASCADE,\
      \ object_kind text NOT NULL,\
      \ object_name text NOT NULL,\
      \ owner_name text,\
      \ object_comment text,\
      \ view_definition text,\
      \ UNIQUE (snapshot_id, object_kind, object_name)\
      \ )"
    , "CREATE TABLE IF NOT EXISTS harness.custom_catalog_columns (\
      \ catalog_column_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7(),\
      \ catalog_object_id uuid NOT NULL\
      \   REFERENCES harness.custom_catalog_objects(catalog_object_id)\
      \   ON DELETE CASCADE,\
      \ column_ordinal integer NOT NULL CHECK (column_ordinal >= 0),\
      \ column_name text NOT NULL,\
      \ data_type text NOT NULL,\
      \ is_nullable boolean NOT NULL,\
      \ default_expression text,\
      \ identity_kind text,\
      \ generated_kind text,\
      \ column_comment text,\
      \ UNIQUE (catalog_object_id, column_ordinal),\
      \ UNIQUE (catalog_object_id, column_name)\
      \ )"
    , "CREATE TABLE IF NOT EXISTS harness.custom_catalog_constraints (\
      \ catalog_constraint_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7(),\
      \ catalog_object_id uuid NOT NULL\
      \   REFERENCES harness.custom_catalog_objects(catalog_object_id)\
      \   ON DELETE CASCADE,\
      \ constraint_ordinal integer NOT NULL CHECK (constraint_ordinal >= 0),\
      \ constraint_name text NOT NULL,\
      \ constraint_kind text NOT NULL,\
      \ constraint_definition text NOT NULL,\
      \ UNIQUE (catalog_object_id, constraint_ordinal),\
      \ UNIQUE (catalog_object_id, constraint_name)\
      \ )"
    , "CREATE TABLE IF NOT EXISTS harness.custom_catalog_indexes (\
      \ catalog_index_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7(),\
      \ catalog_object_id uuid NOT NULL\
      \   REFERENCES harness.custom_catalog_objects(catalog_object_id)\
      \   ON DELETE CASCADE,\
      \ index_ordinal integer NOT NULL CHECK (index_ordinal >= 0),\
      \ index_name text NOT NULL,\
      \ index_definition text NOT NULL,\
      \ UNIQUE (catalog_object_id, index_ordinal),\
      \ UNIQUE (catalog_object_id, index_name)\
      \ )"
    , "CREATE TABLE IF NOT EXISTS harness.custom_sql_audit (\
      \ audit_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7(),\
      \ session_id text,\
      \ agent_id text,\
      \ scope_id uuid NOT NULL\
      \   REFERENCES harness.custom_scopes(scope_id),\
      \ purpose text NOT NULL CHECK (length(btrim(purpose)) > 0),\
      \ sql_text text NOT NULL CHECK (length(btrim(sql_text)) > 0),\
      \ sql_sha256 text NOT NULL CHECK (length(sql_sha256) = 64),\
      \ started_at timestamptz NOT NULL,\
      \ finished_at timestamptz,\
      \ status text NOT NULL DEFAULT 'started'\
      \   CHECK (status IN ('started', 'succeeded', 'failed')),\
      \ succeeded boolean,\
      \ error_text text,\
      \ catalog_before_snapshot_id uuid NOT NULL\
      \   REFERENCES harness.custom_catalog_snapshots(snapshot_id),\
      \ catalog_after_snapshot_id uuid\
      \   REFERENCES harness.custom_catalog_snapshots(snapshot_id),\
      \ CHECK (finished_at IS NULL OR finished_at >= started_at),\
      \ CHECK (\
      \   (status = 'started' AND finished_at IS NULL\
      \     AND succeeded IS NULL AND catalog_after_snapshot_id IS NULL)\
      \   OR (status = 'succeeded' AND finished_at IS NOT NULL\
      \     AND succeeded = true AND catalog_after_snapshot_id IS NOT NULL)\
      \   OR (status = 'failed' AND finished_at IS NOT NULL\
      \     AND succeeded = false AND error_text IS NOT NULL\
      \     AND catalog_after_snapshot_id IS NOT NULL)\
      \ )\
      \ )"
    , "CREATE INDEX IF NOT EXISTS custom_sql_audit_scope_time_idx\
      \ ON harness.custom_sql_audit (scope_id, started_at DESC)"
    , "CREATE OR REPLACE FUNCTION harness.reject_custom_audit_mutation()\
      \ RETURNS trigger\
      \ LANGUAGE plpgsql\
      \ AS $$ BEGIN\
      \ IF TG_OP = 'DELETE' THEN\
      \   RAISE EXCEPTION 'custom SQL audit rows cannot be deleted';\
      \ END IF;\
      \ IF OLD.status <> 'started' OR NEW.status = 'started' THEN\
      \   RAISE EXCEPTION 'custom SQL audit rows can only finalize once';\
      \ END IF;\
      \ IF NEW.audit_id IS DISTINCT FROM OLD.audit_id\
      \   OR NEW.session_id IS DISTINCT FROM OLD.session_id\
      \   OR NEW.agent_id IS DISTINCT FROM OLD.agent_id\
      \   OR NEW.scope_id IS DISTINCT FROM OLD.scope_id\
      \   OR NEW.purpose IS DISTINCT FROM OLD.purpose\
      \   OR NEW.sql_text IS DISTINCT FROM OLD.sql_text\
      \   OR NEW.sql_sha256 IS DISTINCT FROM OLD.sql_sha256\
      \   OR NEW.started_at IS DISTINCT FROM OLD.started_at\
      \   OR NEW.catalog_before_snapshot_id\
      \     IS DISTINCT FROM OLD.catalog_before_snapshot_id THEN\
      \   RAISE EXCEPTION 'custom SQL audit attempt fields are immutable';\
      \ END IF;\
      \ RETURN NEW;\
      \ END $$"
    , "DROP TRIGGER IF EXISTS custom_sql_audit_immutable\
      \ ON harness.custom_sql_audit"
    , "CREATE TRIGGER custom_sql_audit_immutable\
      \ BEFORE UPDATE OR DELETE ON harness.custom_sql_audit\
      \ FOR EACH ROW\
      \ EXECUTE FUNCTION harness.reject_custom_audit_mutation()"
    , "REVOKE ALL ON ALL TABLES IN SCHEMA harness FROM PUBLIC"
    , "REVOKE ALL ON ALL SEQUENCES IN SCHEMA harness FROM PUBLIC"
    ]

migrateCustomSchema :: Pool -> IO (Either Text ())
migrateCustomSchema pool =
    Pool.use pool
        (forM_ customSchemaStatements (Session.script . Text.decodeUtf8))
        >>= \case
        Left err -> pure (Left (Text.pack (show err)))
        Right () -> pure (Right ())

-- | Ensure the login role, owned schema, restrictive defaults, and harness
-- catalog row exist as one transaction.
provisionScope :: Pool -> Scope -> IO (Either Text ScopeDatabase)
provisionScope pool scope = do
    let database = scopeDatabaseFor scope
        sql = provisionSql database
        session =
            TxSessions.transaction
                TxSessions.Serializable
                TxSessions.Write
                do
                    -- PostgreSQL roles are cluster-global rather than
                    -- transactional database objects.  Serialize provisioning
                    -- so concurrent first use cannot race on CREATE ROLE.
                    Tx.sql
                        "LOCK TABLE harness.custom_scopes\
                        \ IN SHARE ROW EXCLUSIVE MODE"
                    Tx.sql (Text.encodeUtf8 sql)
    Pool.use pool session >>= \case
        Left err -> pure (Left (Text.pack (show err)))
        Right () -> pure (Right database)

lookupScopeDatabase :: Pool -> Scope -> IO (Either Text (Maybe ScopeDatabase))
lookupScopeDatabase pool scope =
    Pool.use pool (Session.statement (scopeIdText scope.scopeId) lookupStatement)
        >>= \case
            Left err -> pure (Left (Text.pack (show err)))
            Right Nothing -> pure (Right Nothing)
            Right (Just (kind, roleName, schemaName))
                | kind /= scopeKindText scope.scopeKind ->
                    pure (Left "stored scope kind does not match requested scope")
                | otherwise ->
                    pure $ Right $ Just ScopeDatabase
                        { scopeDatabaseScope = scope
                        , scopeDatabaseRole = roleName
                        , scopeDatabaseSchema = schemaName
                        }

-- | Open a pool whose PostgreSQL session user is the generated scope role.
-- This intentionally delegates to a fresh role-specific Hasql pool rather
-- than using @SET ROLE@ on a trusted harness connection.
openScopeStorePool
    :: ManagedPostgresConfig
    -> PoolConfig
    -> ScopeDatabase
    -> IO (Either StoreError StorePool)
openScopeStorePool config options database =
    openRoleStorePool config database.scopeDatabaseRole options

lookupStatement :: Statement Text (Maybe (Text, Text, Text))
lookupStatement = mkStatement
    "select scope_kind, role_name::text, schema_name::text \
    \from harness.custom_scopes where scope_key = $1"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowMaybe row)
    True
  where
    row =
        (,,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)

deriveScopeIdStatement :: Statement (Text, Text) Text
deriveScopeIdStatement = mkStatement
    "select substr(encode(public.digest(convert_to(\
    \ length($1)::text || ':' || $1 || length($2)::text || ':' || $2,\
    \ 'UTF8'), 'sha256'), 'hex'), 1, 32)"
    ( (fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.text))
    )
    (Decoders.singleRow $
        Decoders.column (Decoders.nonNullable Decoders.text))
    True

provisionSql :: ScopeDatabase -> Text
provisionSql database =
    Text.unlines
        [ "DO $ha$"
        , "BEGIN"
        , "  IF NOT EXISTS (SELECT 1 FROM pg_catalog.pg_roles"
        , "                 WHERE rolname = " <> quoteLiteral roleName <> ") THEN"
        , "    CREATE ROLE " <> quoteIdentifier roleName
            <> " LOGIN NOSUPERUSER NOCREATEDB NOCREATEROLE"
            <> " NOINHERIT NOREPLICATION NOBYPASSRLS;"
        , "  END IF;"
        , "END"
        , "$ha$;"
        , "ALTER ROLE " <> quoteIdentifier roleName
            <> " LOGIN NOSUPERUSER NOCREATEDB NOCREATEROLE"
            <> " NOINHERIT NOREPLICATION NOBYPASSRLS;"
        , "CREATE SCHEMA IF NOT EXISTS " <> quoteIdentifier schemaName
            <> " AUTHORIZATION " <> quoteIdentifier roleName <> ";"
        , "ALTER SCHEMA " <> quoteIdentifier schemaName
            <> " OWNER TO " <> quoteIdentifier roleName <> ";"
        , "REVOKE ALL ON SCHEMA " <> quoteIdentifier schemaName
            <> " FROM PUBLIC;"
        , "REVOKE ALL ON SCHEMA public FROM " <> quoteIdentifier roleName <> ";"
        , "ALTER ROLE " <> quoteIdentifier roleName
            <> " SET search_path TO " <> quoteIdentifier schemaName <> ", pg_catalog;"
        , "ALTER ROLE " <> quoteIdentifier roleName
            <> " SET statement_timeout TO '30s';"
        , "ALTER ROLE " <> quoteIdentifier roleName
            <> " SET lock_timeout TO '5s';"
        , "ALTER ROLE " <> quoteIdentifier roleName
            <> " SET idle_in_transaction_session_timeout TO '30s';"
        , "INSERT INTO harness.custom_scopes"
            <> " (scope_key, scope_kind, role_name, schema_name)"
        , "VALUES ("
            <> quoteLiteral scopeId
            <> ", " <> quoteLiteral kind
            <> ", " <> quoteLiteral roleName <> "::name"
            <> ", " <> quoteLiteral schemaName <> "::name)"
        , "ON CONFLICT (scope_key) DO UPDATE"
        , "SET scope_kind = EXCLUDED.scope_kind,"
        , "    role_name = EXCLUDED.role_name,"
        , "    schema_name = EXCLUDED.schema_name,"
        , "    updated_at = now();"
        ]
  where
    scope = database.scopeDatabaseScope
    scopeId = scopeIdText scope.scopeId
    kind = scopeKindText scope.scopeKind
    roleName = database.scopeDatabaseRole
    schemaName = database.scopeDatabaseSchema

kindPrefix :: ScopeKind -> Text
kindPrefix = \case
    UserScope -> "u"
    RepositoryScope -> "r"
    CheckoutScope -> "c"

quoteIdentifier :: Text -> Text
quoteIdentifier value =
    "\"" <> Text.replace "\"" "\"\"" value <> "\""

quoteLiteral :: Text -> Text
quoteLiteral value =
    "'" <> Text.replace "'" "''" value <> "'"
