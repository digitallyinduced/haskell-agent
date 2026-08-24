-- | Model-facing tools for scoped, user-defined PostgreSQL data.
--
-- The storage package owns PostgreSQL connections, roles, schema isolation,
-- transactions, and result bounds.  This module deliberately depends only on
-- callbacks so the provider-neutral tool surface remains easy to test.
module Agent.CLI.Database
    ( DatabaseScope(..)
    , DatabaseToolsEnv(..)
    , databaseTools
    ) where

import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch (typedTool)
import Agent.Tools.Types
    ( AppTool
    , ToolExecutionPolicy(..)
    , jsonTool
    )
import Data.Aeson
    ( FromJSON(..)
    , Value
    , (.:?)
    , withObject
    , withText
    , (.:)
    )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

data DatabaseScope
    = DatabaseUserScope
    | DatabaseRepositoryScope
    | DatabaseCheckoutScope
    deriving (Eq, Show)

instance FromJSON DatabaseScope where
    parseJSON = withText "DatabaseScope" \case
        "user" -> pure DatabaseUserScope
        "repository" -> pure DatabaseRepositoryScope
        "checkout" -> pure DatabaseCheckoutScope
        value ->
            fail
                ("unknown database scope "
                    <> show value
                    <> "; expected user, repository, or checkout")

-- | Storage callbacks for the three model-facing database operations.
--
-- Successful values are encoded as JSON in the tool result.  Store errors are
-- already sanitized 'Text' because database exception details can contain SQL
-- values that should not be copied into the transcript.
data DatabaseToolsEnv = DatabaseToolsEnv
    { databaseDescribeScope
        :: !(DatabaseScope -> IO (Either Text Value))
    , databaseRunQuery
        :: !(DatabaseScope -> Text -> IO (Either Text Value))
    , databaseRunExecute
        :: !(DatabaseScope -> Text -> Text -> IO (Either Text Value))
    , databaseSearchConversations
        :: !(Text -> Int -> IO (Either Text Value))
    }

data SchemaArgs = SchemaArgs
    { schemaScope :: !DatabaseScope
    }

instance FromJSON SchemaArgs where
    parseJSON = withObject "SchemaArgs" \object ->
        SchemaArgs <$> object .: "scope"

data QueryArgs = QueryArgs
    { queryScope :: !DatabaseScope
    , querySql :: !Text
    }

instance FromJSON QueryArgs where
    parseJSON = withObject "QueryArgs" \object ->
        QueryArgs
            <$> object .: "scope"
            <*> object .: "sql"

data ExecuteArgs = ExecuteArgs
    { executeScope :: !DatabaseScope
    , executeSql :: !Text
    , executePurpose :: !Text
    }

instance FromJSON ExecuteArgs where
    parseJSON = withObject "ExecuteArgs" \object ->
        ExecuteArgs
            <$> object .: "scope"
            <*> object .: "sql"
            <*> object .: "purpose"

data ConversationSearchArgs = ConversationSearchArgs
    { conversationSearchQuery :: !Text
    , conversationSearchLimit :: !Int
    }

instance FromJSON ConversationSearchArgs where
    parseJSON = withObject "ConversationSearchArgs" \object ->
        ConversationSearchArgs
            <$> object .: "query"
            <*> (object .:? "limit" >>= pure . maybe 10 id)

databaseTools :: DatabaseToolsEnv -> [AppTool]
databaseTools env =
    [ schemaTool env
    , queryTool env
    , executeTool env
    , conversationSearchTool env
    ]

schemaTool :: DatabaseToolsEnv -> AppTool
schemaTool env = jsonTool
    "database_schema"
    ( "Inspect the user-defined PostgreSQL tables visible in one durable "
        <> "scope. Returns tables, columns, keys, constraints, indexes, and "
        <> "comments. Inspect this before querying or creating tables; "
        <> "harness-internal schemas are never exposed."
    )
    [scopeProperty]
    True
    ParallelSafe
    (typedTool "database_schema" \(SchemaArgs scope) ->
        encodeResult <$> env.databaseDescribeScope scope)

queryTool :: DatabaseToolsEnv -> AppTool
queryTool env = jsonTool
    "database_query"
    ( "Run one read-only PostgreSQL query against user-defined tables in one "
        <> "durable scope. The database enforces a read-only transaction, "
        <> "timeouts, row limits, and scope isolation. Inspect the schema "
        <> "first rather than guessing table or column names."
    )
    [ scopeProperty
    , PropertySchema "sql" PropertyString True $ Just
        "One read-only PostgreSQL query. Do not include transaction control."
    ]
    True
    ParallelSafe
    (typedTool "database_query" \(QueryArgs scope sql) ->
        if Text.null (Text.strip sql)
            then pure (Left "database query must not be empty")
            else encodeResult
                <$> env.databaseRunQuery scope sql)

executeTool :: DatabaseToolsEnv -> AppTool
executeTool env = jsonTool
    "database_execute"
    ( "Execute a transactional PostgreSQL DDL/DML batch against user-defined "
        <> "tables in one durable scope. Use this to create or alter structured "
        <> "memory such as todos and to insert, update, or delete its rows. "
        <> "Choose the narrowest correct scope, prefer existing suitable "
        <> "tables, and add UUIDv7 primary keys using "
        <> "`uuid PRIMARY KEY DEFAULT uuidv7()`, timestamps, constraints, "
        <> "indexes, and comments when creating a schema. This is a mutating tool and "
        <> "requires approval unless the active policy auto-approves it."
    )
    [ scopeProperty
    , PropertySchema "sql" PropertyString True $ Just
        "Transactional PostgreSQL DDL/DML batch for the selected custom scope."
    , PropertySchema "purpose" PropertyString True $ Just
        "Short explanation of why this schema or data change is needed."
    ]
    False
    TurnSequential
    (typedTool "database_execute" \(ExecuteArgs scope sql purpose) ->
        if Text.null (Text.strip sql)
            then pure (Left "database SQL must not be empty")
            else if Text.null (Text.strip purpose)
                then pure (Left "database change purpose must not be empty")
                else encodeResult
                    <$> env.databaseRunExecute
                        scope
                        purpose
                        sql)

conversationSearchTool :: DatabaseToolsEnv -> AppTool
conversationSearchTool env = jsonTool
    "conversation_search"
    ( "Search user and assistant messages from past, non-deleted conversations. "
        <> "Use this when earlier decisions, preferences, facts, or work may be "
        <> "relevant. Results are ranked by PostgreSQL full-text search."
    )
    [ PropertySchema "query" PropertyString True $ Just
        "Words or a natural-language web-search-style query."
    , PropertySchema "limit" PropertyInteger False $ Just
        "Maximum number of matches, from 1 to 100. Defaults to 10."
    ]
    True
    ParallelSafe
    (typedTool "conversation_search" \(ConversationSearchArgs query limit) ->
        if Text.null (Text.strip query)
            then pure (Left "conversation search query must not be empty")
            else if limit < 1 || limit > 100
                then pure (Left "conversation search limit must be between 1 and 100")
                else encodeResult <$> env.databaseSearchConversations query limit)

scopeProperty :: PropertySchema
scopeProperty = PropertySchema
    "scope"
    (PropertyEnum ["user", "repository", "checkout"])
    True
    (Just
        ( "Durable data scope: user for cross-project personal data, repository "
            <> "for data shared by clones/worktrees, or checkout for this worktree."
        ))

encodeResult :: Either Text Value -> Either Text Text
encodeResult =
    fmap (Text.decodeUtf8 . LBS.toStrict . Aeson.encode)
