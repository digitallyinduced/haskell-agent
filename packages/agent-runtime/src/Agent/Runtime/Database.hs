-- | Model-facing tools for scoped PostgreSQL data.
--
-- Custom scopes (@user@, @repository@, @checkout@) are agent-created memory.
-- The local CLI also exposes the runtime @harness@ catalog as a read-only
-- query surface so the model can inspect durable sessions after compaction.
-- Multi-tenant @agent-server@ embeddings omit that catalog.  The storage
-- package owns connections, roles, isolation, transactions, and result
-- bounds.  This module depends only on callbacks so the tool surface stays
-- easy to test.
module Agent.Runtime.Database
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
import Agent.Tools.Types
    ( AppTool
    , ToolExecutionPolicy(..)
    , jsonTool
    )
import Data.Text (Text)
import qualified Data.Text as Text

data DatabaseScope
    = DatabaseUserScope
    | DatabaseRepositoryScope
    | DatabaseCheckoutScope
    | DatabaseHarnessScope
    deriving (Eq, Show)

-- | Custom-data scopes used by learned skills and the native data browser.
-- The runtime @harness@ catalog is not a provisioned custom scope.
databaseScopeDecoder :: Hermes.Decoder DatabaseScope
databaseScopeDecoder = databaseScopeDecoderWithHarness False

databaseScopeDecoderWithHarness :: Bool -> Hermes.Decoder DatabaseScope
databaseScopeDecoderWithHarness exposeHarness = Hermes.withText \case
        "user" -> pure DatabaseUserScope
        "repository" -> pure DatabaseRepositoryScope
        "checkout" -> pure DatabaseCheckoutScope
        "harness"
            | exposeHarness -> pure DatabaseHarnessScope
            | otherwise ->
                fail "the harness catalog is not available in this session"
        value ->
            fail
                ("unknown database scope "
                    <> show value
                    <> "; expected "
                    <> expectedScopes exposeHarness)

-- | Storage callbacks for the model-facing database operations.
--
-- Database and conversation-search results are rendered as labeled text because
-- they are read by the model and humans rather than consumed as an API.
-- Store errors are already sanitized 'Text' because database exception details
-- can contain SQL values that should not be copied into the transcript.
data DatabaseToolsEnv = DatabaseToolsEnv
    { databaseDescribeScope
        :: !(DatabaseScope -> IO (Either Text Text))
    , databaseRunQuery
        :: !(DatabaseScope -> Text -> IO (Either Text Text))
    , databaseRunExecute
        :: !(DatabaseScope -> Text -> Text -> IO (Either Text Text))
    , databaseSearchConversations
        :: !(Text -> Int -> IO (Either Text [ConversationSearchMatch]))
    -- | Local CLI and personal native embeddings expose the runtime catalog.
    -- @agent-server@ leaves this false so tenant agents cannot query harness
    -- tables through model-authored SQL.
    , databaseHarnessCatalogEnabled :: !Bool
    -- | Stable session key for the current conversation, when reserved.
    -- Included in harness-catalog tool text so the model can filter SQL after
    -- compaction without guessing identifiers.
    , databaseHarnessSessionId :: !(Maybe Text)
    }

data SchemaArgs = SchemaArgs !DatabaseScope

schemaArgsDecoder :: Bool -> Hermes.Decoder SchemaArgs
schemaArgsDecoder exposeHarness = Hermes.object $
    SchemaArgs <$> Hermes.atKey "scope" (databaseScopeDecoderWithHarness exposeHarness)

data QueryArgs = QueryArgs !DatabaseScope !Text

queryArgsDecoder :: Bool -> Hermes.Decoder QueryArgs
queryArgsDecoder exposeHarness = Hermes.object $
        QueryArgs
            <$> Hermes.atKey "scope" (databaseScopeDecoderWithHarness exposeHarness)
            <*> Hermes.atKey "sql" Hermes.text

data ExecuteArgs = ExecuteArgs !DatabaseScope !Text !Text

executeArgsDecoder :: Bool -> Hermes.Decoder ExecuteArgs
executeArgsDecoder exposeHarness = Hermes.object $
        ExecuteArgs
            <$> Hermes.atKey "scope" (databaseScopeDecoderWithHarness exposeHarness)
            <*> Hermes.atKey "sql" Hermes.text
            <*> Hermes.atKey "purpose" Hermes.text

data ConversationSearchArgs = ConversationSearchArgs !Text !Int

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
    (schemaToolDescription env)
    [scopeProperty env]
    True
    ParallelSafe
    (typedTool
        "database_schema"
        (schemaArgsDecoder env.databaseHarnessCatalogEnabled)
        \(SchemaArgs scope) ->
            env.databaseDescribeScope scope)

queryTool :: DatabaseToolsEnv -> AppTool
queryTool env = jsonTool
    "database_query"
    (queryToolDescription env)
    [ scopeProperty env
    , PropertySchema "sql" PropertyString True $ Just
        "One read-only PostgreSQL query. Do not include transaction control."
    ]
    True
    ParallelSafe
    (typedTool
        "database_query"
        (queryArgsDecoder env.databaseHarnessCatalogEnabled)
        \(QueryArgs scope sql) ->
        if Text.null (Text.strip sql)
            then pure (Left "database query must not be empty")
            else env.databaseRunQuery scope sql)

executeTool :: DatabaseToolsEnv -> AppTool
executeTool env = jsonTool
    "database_execute"
    (executeToolDescription env)
    [ scopeProperty env
    , PropertySchema "sql" PropertyString True $ Just
        "Transactional PostgreSQL DDL/DML batch for the selected custom scope."
    , PropertySchema "purpose" PropertyString True $ Just
        "Short explanation of why this schema or data change is needed."
    ]
    False
    TurnSequential
    (typedTool
        "database_execute"
        (executeArgsDecoder env.databaseHarnessCatalogEnabled)
        \(ExecuteArgs scope sql purpose) ->
        if scope == DatabaseHarnessScope
            then pure (Left harnessCatalogReadOnlyError)
            else if Text.null (Text.strip sql)
                then pure (Left "database SQL must not be empty")
                else if Text.null (Text.strip purpose)
                    then pure (Left "database change purpose must not be empty")
                    else env.databaseRunExecute scope purpose sql)

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

scopeProperty :: DatabaseToolsEnv -> PropertySchema
scopeProperty env = PropertySchema
    "scope"
    (PropertyEnum (scopeNames env.databaseHarnessCatalogEnabled))
    True
    (Just (scopePropertyDescription env.databaseHarnessCatalogEnabled))

scopeNames :: Bool -> [Text]
scopeNames exposeHarness =
    ["user", "repository", "checkout"]
        <> ["harness" | exposeHarness]

expectedScopes :: Bool -> String
expectedScopes exposeHarness =
    if exposeHarness
        then "user, repository, checkout, or harness"
        else "user, repository, or checkout"

scopePropertyDescription :: Bool -> Text
scopePropertyDescription exposeHarness =
    "Durable data scope: user for cross-project personal data, repository "
        <> "for data shared by clones/worktrees, or checkout for this worktree."
        <> if exposeHarness
            then
                " harness is the local runtime catalog (sessions, turns, messages, "
                    <> "tool calls). It is read-only and survives compaction."
            else ""

schemaToolDescription :: DatabaseToolsEnv -> Text
schemaToolDescription env
    | env.databaseHarnessCatalogEnabled =
        "Inspect PostgreSQL tables visible in one durable scope. user, "
            <> "repository, and checkout are agent-created memory. harness is "
            <> "the local runtime catalog. Returns tables, columns, keys, "
            <> "constraints, indexes, and comments. Inspect this before querying "
            <> "or creating tables."
    | otherwise =
        "Inspect the user-defined PostgreSQL tables visible in one durable "
            <> "scope. Returns tables, columns, keys, constraints, indexes, and "
            <> "comments. Inspect this before querying or creating tables; "
            <> "harness-internal schemas are never exposed."

queryToolDescription :: DatabaseToolsEnv -> Text
queryToolDescription env
    | env.databaseHarnessCatalogEnabled =
        "Run one read-only PostgreSQL query against one durable scope. The "
            <> "database enforces a read-only transaction, timeouts, row limits, "
            <> "and scope isolation. Inspect the schema first rather than guessing "
            <> "table or column names."
            <> harnessQueryGuidance env.databaseHarnessSessionId
    | otherwise =
        "Run one read-only PostgreSQL query against user-defined tables in one "
            <> "durable scope. The database enforces a read-only transaction, "
            <> "timeouts, row limits, and scope isolation. Inspect the schema "
            <> "first rather than guessing table or column names."

executeToolDescription :: DatabaseToolsEnv -> Text
executeToolDescription env =
    "Execute a transactional PostgreSQL DDL/DML batch against user-defined "
        <> "tables in one durable scope. Use this to create or alter structured "
        <> "memory such as todos and to insert, update, or delete its rows. "
        <> "Choose the narrowest correct scope, prefer existing suitable "
        <> "tables, and add UUIDv7 primary keys using "
        <> "`uuid PRIMARY KEY DEFAULT uuidv7()`, timestamps, constraints, "
        <> "indexes, and comments when creating a schema. This is a mutating tool and "
        <> "requires approval unless the active policy auto-approves it."
        <> if env.databaseHarnessCatalogEnabled
            then " The harness catalog is read-only; do not use this tool to change it."
            else ""

harnessQueryGuidance :: Maybe Text -> Text
harnessQueryGuidance sessionId =
    " The harness scope is the runtime catalog: sessions, session_turns, "
        <> "session_messages, session_function_calls, and related item tables. "
        <> "Compaction replaces model context only; earlier turns remain queryable. "
        <> "Filter on session_key (not the internal UUID) and turn_index. "
        <> maybe
            ""
            (\value -> " This session's key is `" <> value <> "`. ")
            sessionId
        <> "Prefer targeted columns and WHERE clauses over selecting large "
        <> "tool-output columns."

harnessCatalogReadOnlyError :: Text
harnessCatalogReadOnlyError =
    "the harness catalog is read-only; use user, repository, or checkout to change agent-created tables"

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
