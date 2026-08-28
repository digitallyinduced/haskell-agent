-- | Model-facing tools for scoped, user-defined PostgreSQL data.
--
-- The storage package owns PostgreSQL connections, roles, schema isolation,
-- transactions, and result bounds.  This module deliberately depends only on
-- callbacks so the provider-neutral tool surface remains easy to test.
module Agent.CLI.Database
    ( DatabaseScope(..)
    , databaseScopeDecoder
    , ConversationSearchMatch(..)
    , DatabaseToolsEnv(..)
    , databaseTools
    ) where

import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch (typedTool)
import Agent.Json.Decode (defaultKey)
import Agent.Json.Decode qualified as Hermes
import Agent.Json (RawJson, rawJsonBytes)
import Agent.Tools.Types
    ( AppTool
    , ToolExecutionPolicy(..)
    , jsonTool
    )
import Data.Aeson (Value)
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

databaseScopeDecoder :: Hermes.Decoder DatabaseScope
databaseScopeDecoder = Hermes.withText \case
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
-- Structured database values are encoded as JSON in the tool result. Conversation
-- search is rendered as labeled text because its results are primarily read by
-- the model and by humans rather than consumed as a machine-readable payload.
-- Store errors are already sanitized 'Text' because database exception details
-- can contain SQL values that should not be copied into the transcript.
data DatabaseToolsEnv = DatabaseToolsEnv
    { databaseDescribeScope
        :: !(DatabaseScope -> IO (Either Text Value))
    , databaseRunQuery
        :: !(DatabaseScope -> Text -> IO (Either Text RawJson))
    , databaseRunExecute
        :: !(DatabaseScope -> Text -> Text -> IO (Either Text Value))
    , databaseSearchConversations
        :: !(Text -> Int -> IO (Either Text [ConversationSearchMatch]))
    }

data SchemaArgs = SchemaArgs
    { schemaScope :: !DatabaseScope
    }

schemaArgsDecoder :: Hermes.Decoder SchemaArgs
schemaArgsDecoder = Hermes.object $
    SchemaArgs <$> Hermes.atKey "scope" databaseScopeDecoder

data QueryArgs = QueryArgs
    { queryScope :: !DatabaseScope
    , querySql :: !Text
    }

queryArgsDecoder :: Hermes.Decoder QueryArgs
queryArgsDecoder = Hermes.object $
        QueryArgs
            <$> Hermes.atKey "scope" databaseScopeDecoder
            <*> Hermes.atKey "sql" Hermes.text

data ExecuteArgs = ExecuteArgs
    { executeScope :: !DatabaseScope
    , executeSql :: !Text
    , executePurpose :: !Text
    }

executeArgsDecoder :: Hermes.Decoder ExecuteArgs
executeArgsDecoder = Hermes.object $
        ExecuteArgs
            <$> Hermes.atKey "scope" databaseScopeDecoder
            <*> Hermes.atKey "sql" Hermes.text
            <*> Hermes.atKey "purpose" Hermes.text

data ConversationSearchArgs = ConversationSearchArgs
    { conversationSearchQuery :: !Text
    , conversationSearchLimit :: !Int
    }

conversationSearchArgsDecoder :: Hermes.Decoder ConversationSearchArgs
conversationSearchArgsDecoder = Hermes.object $
        ConversationSearchArgs
            <$> Hermes.atKey "query" Hermes.text
            <*> defaultKey 10 "limit" Hermes.int

data ConversationSearchMatch
    = ConversationSearchMatch !Text !Integer !(Maybe Text) !Text !(Maybe Text)
    deriving (Eq, Show)

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
    (typedTool "database_schema" schemaArgsDecoder \(SchemaArgs scope) ->
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
    (typedTool "database_query" queryArgsDecoder \(QueryArgs scope sql) ->
        if Text.null (Text.strip sql)
            then pure (Left "database query must not be empty")
            else fmap (Text.decodeUtf8 . rawJsonBytes)
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
    (typedTool "database_execute" executeArgsDecoder \(ExecuteArgs scope sql purpose) ->
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
        <> "relevant. Results are ranked by PostgreSQL full-text search and "
        <> "returned as readable labeled text."
    )
    [ PropertySchema "query" PropertyString True $ Just
        "Words or a natural-language web-search-style query."
    , PropertySchema "limit" PropertyInteger False $ Just
        "Maximum number of matches, from 1 to 100. Defaults to 10."
    ]
    True
    ParallelSafe
    (typedTool "conversation_search" conversationSearchArgsDecoder
        \(ConversationSearchArgs query limit) ->
        if Text.null (Text.strip query)
            then pure (Left "conversation search query must not be empty")
            else if limit < 1 || limit > 100
                then pure (Left "conversation search limit must be between 1 and 100")
                else fmap (fmap renderConversationSearchResult) $
                    env.databaseSearchConversations query limit)

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

renderConversationSearchResult :: [ConversationSearchMatch] -> Text
renderConversationSearchResult matches =
    case matches of
                [] -> "(no matching conversations)"
                _ -> Text.intercalate "\n\n" $
                    zipWith renderConversationSearchMatch [1 :: Int ..] matches

renderConversationSearchMatch :: Int -> ConversationSearchMatch -> Text
renderConversationSearchMatch
    matchNumber
    (ConversationSearchMatch sessionId turnIndex occurredAt userText assistantText) =
        Text.intercalate "\n" $
            [ "Match " <> Text.pack (show matchNumber)
            , "Session: " <> sessionId
            , "Turn: " <> Text.pack (show turnIndex)
            ]
                <> maybe [] (\timestamp -> ["Occurred at: " <> timestamp]) occurredAt
                <> ["User:", indentText userText]
                <> maybe [] (\text -> ["Assistant:", indentText text]) assistantText

indentText :: Text -> Text
indentText = Text.intercalate "\n" . map ("  " <>) . Text.lines
