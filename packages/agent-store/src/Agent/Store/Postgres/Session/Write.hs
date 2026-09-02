{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | PostgreSQL write operations for harness sessions.
module Agent.Store.Postgres.Session.Write
    ( createSession
    , createSessionWithInitialPromptEpoch
    , createSessionFromSnapshot
    , replaceSessionMetadata
    , setSessionTitle
    , setGeneratedSessionTitle
    , appendSessionPromptEpoch
    , appendSessionTurn
    , appendSessionTurnIndexed
    , appendSessionTurnIndexedWithPromptReset
    , appendSessionTurns
    , appendSessionTurnsClearingTaskPlan
    , setSessionArchived
    , deleteSession
    , importLegacySession
    , withSessionAdvisoryLock
    ) where

import Control.Monad (forM_, unless, when)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Transaction as Transaction
import qualified Hasql.Transaction.Sessions as Transactions
import Hasql.Statement (Statement)

import Agent.Store.Postgres.Connection
    ( StorePool
    , withSession
    )
import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.Postgres.Session.Types
import Agent.Store.Postgres.SessionItem (insertResponseItems)
import Agent.Store.Types (StoreError)

data TitleUpdate = TitleUpdate
    { titleUpdateKey :: !Text
    , titleUpdateTitle :: !Text
    , titleUpdateManual :: !Bool
    , titleUpdateRefreshIndex :: !Int32
    , titleUpdateOccurredAt :: !UTCTime
    , titleUpdateOnlyWhenAutomatic :: !Bool
    }

createSession
    :: StorePool
    -> SessionMetadata
    -> IO (Either StoreError Bool)
createSession pool metadata =
    withSession pool $
        Transactions.transaction Transactions.Serializable Transactions.Write do
            _ <- Transaction.statement
                metadata.sessionMetadataKey
                blockingAdvisoryLockStatement
            exists <- Transaction.statement
                metadata.sessionMetadataKey
                sessionExistsStatement
            if exists
                then pure False
                else do
                    sessionId <- Transaction.statement metadata insertSessionStatement
                    _ <- Transaction.statement
                        EventInsert
                            { eventInsertSessionId = sessionId
                            , eventInsertSequence = 0
                            , eventInsertKind = "session.created"
                            , eventInsertOccurredAt =
                                metadata.sessionMetadataCreatedAt
                            }
                        insertEventStatement
                    pure True

setSessionTitle
    :: StorePool
    -> Text
    -> Text
    -> Bool
    -> Int32
    -> UTCTime
    -> IO (Either StoreError Bool)
setSessionTitle pool sessionKey title manual refreshIndex occurredAt =
    updateSessionTitle pool TitleUpdate
        { titleUpdateKey = sessionKey
        , titleUpdateTitle = title
        , titleUpdateManual = manual
        , titleUpdateRefreshIndex = refreshIndex
        , titleUpdateOccurredAt = occurredAt
        , titleUpdateOnlyWhenAutomatic = False
        }

setGeneratedSessionTitle
    :: StorePool
    -> Text
    -> Text
    -> Int32
    -> UTCTime
    -> IO (Either StoreError Bool)
setGeneratedSessionTitle pool sessionKey title refreshIndex occurredAt =
    updateSessionTitle pool TitleUpdate
        { titleUpdateKey = sessionKey
        , titleUpdateTitle = title
        , titleUpdateManual = False
        , titleUpdateRefreshIndex = refreshIndex
        , titleUpdateOccurredAt = occurredAt
        , titleUpdateOnlyWhenAutomatic = True
        }

updateSessionTitle
    :: StorePool
    -> TitleUpdate
    -> IO (Either StoreError Bool)
updateSessionTitle pool update =
    withSession pool $
        Transactions.transaction Transactions.Serializable Transactions.Write do
            _ <- Transaction.statement
                update.titleUpdateKey
                blockingAdvisoryLockStatement
            changed <- Transaction.statement update setTitleProjectionStatement
            case changed of
                Nothing -> pure False
                Just (sessionId, sequence) -> do
                    _ <- Transaction.statement
                        EventInsert
                            { eventInsertSessionId = sessionId
                            , eventInsertSequence = sequence
                            , eventInsertKind = "session.title_updated"
                            , eventInsertOccurredAt =
                                update.titleUpdateOccurredAt
                            }
                        insertEventStatement
                    pure True

-- | Create the session and its first provider-visible prompt epoch in one
-- transaction, so a resumable session can never exist without that prefix.
createSessionWithInitialPromptEpoch
    :: StorePool
    -> SessionMetadata
    -> SessionPromptSnapshot
    -> IO (Either StoreError Bool)
createSessionWithInitialPromptEpoch pool metadata snapshot =
    withSession pool $
        Transactions.transaction Transactions.Serializable Transactions.Write do
            let sessionKey = metadata.sessionMetadataKey
            _ <- Transaction.statement sessionKey blockingAdvisoryLockStatement
            exists <- Transaction.statement sessionKey sessionExistsStatement
            if exists
                then pure False
                else do
                    sessionId <- Transaction.statement metadata insertSessionStatement
                    epoch <- Transaction.statement
                        (sessionKey, snapshot)
                        insertPromptEpochStatement
                    case epoch of
                        Just 0 -> pure ()
                        _ -> Transaction.condemn
                    _ <- Transaction.statement
                        EventInsert
                            { eventInsertSessionId = sessionId
                            , eventInsertSequence = 0
                            , eventInsertKind = "session.created"
                            , eventInsertOccurredAt =
                                metadata.sessionMetadataCreatedAt
                            }
                        insertEventStatement
                    pure True

-- | Append an immutable prompt epoch, assigning the next per-session index
-- while holding the same advisory lock used by other session writes.
appendSessionPromptEpoch
    :: StorePool
    -> Text
    -> SessionPromptSnapshot
    -> IO (Either StoreError (Maybe Int64))
appendSessionPromptEpoch pool sessionKey snapshot =
    withSession pool $
        Transactions.transaction Transactions.Serializable Transactions.Write do
            _ <- Transaction.statement sessionKey blockingAdvisoryLockStatement
            Transaction.statement
                (sessionKey, snapshot)
                insertPromptEpochStatement

-- | Atomically create a session from an already-materialized transcript.
--
-- This is the neutral counterpart to 'importLegacySession': it records the
-- ordinary session/turn events and final metadata projection, but no legacy
-- import provenance. A conflicting session key leaves the existing session
-- untouched and returns 'False'.
createSessionFromSnapshot
    :: StorePool
    -> SessionMetadata
    -> [SessionTurn]
    -> IO (Either StoreError Bool)
createSessionFromSnapshot pool metadata turns =
    withSession pool $
        Transactions.transaction Transactions.Serializable Transactions.Write do
            let sessionKey = metadata.sessionMetadataKey
            _ <- Transaction.statement sessionKey blockingAdvisoryLockStatement
            exists <- Transaction.statement sessionKey sessionExistsStatement
            if exists
                then pure False
                else do
                    _ <- insertSessionSnapshot
                        "session.snapshot_created"
                        metadata
                        turns
                    pure True

replaceSessionMetadata
    :: StorePool
    -> Text
    -> SessionMetadata
    -> IO (Either StoreError Bool)
replaceSessionMetadata pool eventKind metadata =
    withSession pool $
        Transactions.transaction Transactions.Serializable Transactions.Write do
            _ <- Transaction.statement
                metadata.sessionMetadataKey
                blockingAdvisoryLockStatement
            changed <- Transaction.statement metadata replaceProjectionStatement
            case changed of
                Nothing -> pure False
                Just (sessionId, sequence) -> do
                    _ <- Transaction.statement
                        EventInsert
                            { eventInsertSessionId = sessionId
                            , eventInsertSequence = sequence
                            , eventInsertKind = eventKind
                            , eventInsertOccurredAt =
                                metadata.sessionMetadataUpdatedAt
                            }
                        insertEventStatement
                    pure True

appendSessionTurn
    :: StorePool
    -> SessionTurn
    -> SessionMetadata
    -> IO (Either StoreError Bool)
appendSessionTurn pool turn metadata =
    fmap (fmap (maybe False (const True))) $
        appendSessionTurnIndexed pool turn metadata

appendSessionTurnIndexed
    :: StorePool
    -> SessionTurn
    -> SessionMetadata
    -> IO (Either StoreError (Maybe Int64))
appendSessionTurnIndexed =
    appendSessionTurnIndexedInternal False

-- | Append a turn and retire the current provider-visible prompt prefix in
-- the same transaction. The next request must establish a new prompt epoch.
appendSessionTurnIndexedWithPromptReset
    :: StorePool
    -> SessionTurn
    -> SessionMetadata
    -> IO (Either StoreError (Maybe Int64))
appendSessionTurnIndexedWithPromptReset =
    appendSessionTurnIndexedInternal True

appendSessionTurnIndexedInternal
    :: Bool
    -> StorePool
    -> SessionTurn
    -> SessionMetadata
    -> IO (Either StoreError (Maybe Int64))
appendSessionTurnIndexedInternal resetPrompt pool turn metadata =
    withSession pool $
        Transactions.transaction Transactions.Serializable Transactions.Write do
            _ <- Transaction.statement
                metadata.sessionMetadataKey
                blockingAdvisoryLockStatement
            changed <- Transaction.statement metadata appendProjectionStatement
            case changed of
                Nothing -> pure Nothing
                Just (sessionId, sequence, turnIndex) -> do
                    eventId <- Transaction.statement
                        EventInsert
                            { eventInsertSessionId = sessionId
                            , eventInsertSequence = sequence
                            , eventInsertKind = "turn.appended"
                            , eventInsertOccurredAt = turn.sessionTurnOccurredAt
                            }
                        insertEventStatement
                    turnId <- Transaction.statement
                        TurnInsert
                            { turnInsertSessionId = sessionId
                            , turnInsertEventId = eventId
                            , turnInsertIndex = turnIndex
                            , turnInsertEventSequence = sequence
                            , turnInsertTurn = turn
                            }
                        insertTurnStatement
                    insertResponseItems turnId
                        ( turn.sessionTurnItems
                            <> turn.sessionTurnDisplayItems
                        )
                    when resetPrompt do
                        _ <- Transaction.statement
                            ( metadata.sessionMetadataKey
                            , turn.sessionTurnOccurredAt
                            )
                            invalidatePromptEpochStatement
                        pure ()
                    pure (Just turnIndex)

-- | Append a sequence of turns as one atomic transcript transition.
--
-- This is used when an immutable transcript needs to publish a reset marker
-- and a replayed prefix together: readers observe either the entire new branch
-- or none of it.
appendSessionTurns
    :: StorePool
    -> [SessionTurn]
    -> SessionMetadata
    -> IO (Either StoreError Bool)
appendSessionTurns pool turns metadata =
    appendSessionTurnsWithTaskPlanClear False pool turns metadata

-- | Append a transcript transition and clear its current task plan in the
-- same serializable transaction.
appendSessionTurnsClearingTaskPlan
    :: StorePool
    -> [SessionTurn]
    -> SessionMetadata
    -> IO (Either StoreError Bool)
appendSessionTurnsClearingTaskPlan =
    appendSessionTurnsWithTaskPlanClear True

appendSessionTurnsWithTaskPlanClear
    :: Bool
    -> StorePool
    -> [SessionTurn]
    -> SessionMetadata
    -> IO (Either StoreError Bool)
appendSessionTurnsWithTaskPlanClear clearTaskPlan pool turns metadata =
    withSession pool $
        Transactions.transaction Transactions.Serializable Transactions.Write do
            _ <- Transaction.statement
                metadata.sessionMetadataKey
                blockingAdvisoryLockStatement
            appended <- case turns of
                [] ->
                    Transaction.statement
                        metadata.sessionMetadataKey
                        sessionExistsStatement
                _ -> appendAll turns
            when (appended && clearTaskPlan) do
                _ <- Transaction.statement
                    metadata.sessionMetadataKey
                    clearTaskPlanStatement
                pure ()
            pure appended
  where
    appendAll [] = pure True
    appendAll (turn : rest) = do
        appended <- appendTurnTransaction turn metadata
        if appended
            then appendAll rest
            else Transaction.condemn >> pure False

clearTaskPlanStatement :: Statement Text ()
clearTaskPlanStatement = mkStatement
    "DELETE FROM harness.session_task_plans p\
    \ USING harness.sessions s\
    \ WHERE p.session_id = s.session_id AND s.session_key = $1"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    Decoders.noResult
    True

setSessionArchived
    :: StorePool
    -> Text
    -> Bool
    -> UTCTime
    -> IO (Either StoreError Bool)
setSessionArchived pool sessionKey archived occurredAt =
    withSession pool $
        Transactions.transaction Transactions.Serializable Transactions.Write do
            _ <- Transaction.statement sessionKey blockingAdvisoryLockStatement
            changed <- Transaction.statement
                (sessionKey, archived, occurredAt)
                setArchivedProjectionStatement
            case changed of
                Nothing -> pure False
                Just (sessionId, sequence) -> do
                    _ <- Transaction.statement
                        EventInsert
                            { eventInsertSessionId = sessionId
                            , eventInsertSequence = sequence
                            , eventInsertKind =
                                if archived
                                    then "session.archived"
                                    else "session.restored"
                            , eventInsertOccurredAt = occurredAt
                            }
                        insertEventStatement
                    pure True

deleteSession
    :: StorePool
    -> Text
    -> UTCTime
    -> IO (Either StoreError Bool)
deleteSession pool sessionKey occurredAt =
    withSession pool $
        Transactions.transaction Transactions.Serializable Transactions.Write do
            _ <- Transaction.statement sessionKey blockingAdvisoryLockStatement
            changed <- Transaction.statement
                (sessionKey, occurredAt)
                deleteProjectionStatement
            case changed of
                Nothing -> pure False
                Just (sessionId, sequence) -> do
                    _ <- Transaction.statement
                        EventInsert
                            { eventInsertSessionId = sessionId
                            , eventInsertSequence = sequence
                            , eventInsertKind = "session.deleted"
                            , eventInsertOccurredAt = occurredAt
                            }
                        insertEventStatement
                    pure True

importLegacySession
    :: StorePool
    -> LegacySession
    -> IO (Either StoreError Bool)
importLegacySession pool legacy =
    withSession pool $
        Transactions.transaction Transactions.Serializable Transactions.Write do
            let metadata = legacy.legacyMetadata
                sessionKey = metadata.sessionMetadataKey
            _ <- Transaction.statement sessionKey blockingAdvisoryLockStatement
            exists <- Transaction.statement sessionKey sessionExistsStatement
            if exists
                then pure False
                else do
                    sessionId <- insertSessionSnapshot
                        "legacy.import_completed"
                        metadata
                        legacy.legacyTurns
                    forM_ legacy.legacyPromptSnapshot \snapshot -> do
                        epoch <- Transaction.statement
                            (sessionKey, snapshot)
                            insertPromptEpochStatement
                        case epoch of
                            Just 0 -> pure ()
                            _ -> Transaction.condemn
                    Transaction.statement
                        ( legacy.legacySourcePath
                        , legacy.legacyContentHash
                        , sessionId
                        )
                        recordLegacyImportStatement

-- | Insert a complete session while the caller owns its advisory lock and
-- transaction. The final projection replacement preserves metadata that
-- cannot be derived solely from replaying transcript turns.
insertSessionSnapshot
    :: Text
    -> SessionMetadata
    -> [SessionTurn]
    -> Transaction.Transaction Text
insertSessionSnapshot completionKind metadata turns = do
    sessionId <- Transaction.statement metadata insertSessionStatement
    _ <- Transaction.statement
        EventInsert
            { eventInsertSessionId = sessionId
            , eventInsertSequence = 0
            , eventInsertKind = "session.created"
            , eventInsertOccurredAt =
                metadata.sessionMetadataCreatedAt
            }
        insertEventStatement
    forM_ turns \turn -> do
        appended <- appendTurnTransaction turn metadata
        unless appended Transaction.condemn
    changed <- Transaction.statement metadata replaceProjectionStatement
    case changed of
        Nothing -> Transaction.condemn >> pure sessionId
        Just (internalId, sequence) -> do
            _ <- Transaction.statement
                EventInsert
                    { eventInsertSessionId = internalId
                    , eventInsertSequence = sequence
                    , eventInsertKind = completionKind
                    , eventInsertOccurredAt =
                        metadata.sessionMetadataUpdatedAt
                    }
                insertEventStatement
            pure sessionId

withSessionAdvisoryLock
    :: StorePool
    -> Text
    -> Transaction.Transaction a
    -> IO (Either StoreError (Maybe a))
withSessionAdvisoryLock pool sessionId action =
    withSession pool $
        Transactions.transaction Transactions.Serializable Transactions.Write do
            acquired <- Transaction.statement sessionId tryAdvisoryLockStatement
            if acquired then Just <$> action else pure Nothing

appendTurnTransaction
    :: SessionTurn
    -> SessionMetadata
    -> Transaction.Transaction Bool
appendTurnTransaction turn metadata = do
    changed <- Transaction.statement metadata appendProjectionStatement
    case changed of
        Nothing -> pure False
        Just (sessionId, sequence, turnIndex) -> do
            eventId <- Transaction.statement
                EventInsert
                    { eventInsertSessionId = sessionId
                    , eventInsertSequence = sequence
                    , eventInsertKind = "turn.appended"
                    , eventInsertOccurredAt = turn.sessionTurnOccurredAt
                    }
                insertEventStatement
            turnId <- Transaction.statement
                TurnInsert
                    { turnInsertSessionId = sessionId
                    , turnInsertEventId = eventId
                    , turnInsertIndex = turnIndex
                    , turnInsertEventSequence = sequence
                    , turnInsertTurn = turn
                    }
                insertTurnStatement
            insertResponseItems turnId
                (turn.sessionTurnItems <> turn.sessionTurnDisplayItems)
            pure True

data EventInsert = EventInsert
    { eventInsertSessionId :: !Text
    , eventInsertSequence :: !Int64
    , eventInsertKind :: !Text
    , eventInsertOccurredAt :: !UTCTime
    }

data TurnInsert = TurnInsert
    { turnInsertSessionId :: !Text
    , turnInsertEventId :: !Text
    , turnInsertIndex :: !Int64
    , turnInsertEventSequence :: !Int64
    , turnInsertTurn :: !SessionTurn
    }

transcriptEffectText :: TranscriptEffect -> Text
transcriptEffectText effect = case effect of
    TranscriptAppend -> "append"
    TranscriptReplace -> "replace"
    TranscriptReset -> "reset"

sessionExistsStatement :: Statement Text Bool
sessionExistsStatement = mkStatement
    "SELECT EXISTS (SELECT 1 FROM harness.sessions WHERE session_key = $1)"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
    True

setArchivedProjectionStatement
    :: Statement (Text, Bool, UTCTime) (Maybe (Text, Int64))
setArchivedProjectionStatement = mkStatement
    "UPDATE harness.sessions SET\
    \ archived_at = CASE WHEN $2 THEN $3 ELSE NULL END,\
    \ next_event_sequence = next_event_sequence + 1\
    \ WHERE session_key = $1 AND deleted_at IS NULL\
    \ RETURNING session_id::text, next_event_sequence - 1"
    ( ((\(key, _, _) -> key)
        >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((\(_, archived, _) -> archived)
            >$< Encoders.param (Encoders.nonNullable Encoders.bool))
        <> ((\(_, _, occurredAt) -> occurredAt)
            >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))
    )
    (Decoders.rowMaybe $
        (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.int8))
    True

insertPromptEpochStatement
    :: Statement (Text, SessionPromptSnapshot) (Maybe Int64)
insertPromptEpochStatement = mkStatement
    "INSERT INTO harness.session_prompt_epochs (\
    \ session_id, epoch_index, is_active, prompt_version, created_at,\
    \ provider, connection_id, model_id, dialect, cwd_text,\
    \ instructions_text, tools_text,\
    \ generated_context_text, grok_context_text, prompt_cache_key\
    \ )\
    \ SELECT session.session_id,\
    \   COALESCE((\
    \     SELECT max(existing.epoch_index) + 1\
    \     FROM harness.session_prompt_epochs existing\
    \     WHERE existing.session_id = session.session_id\
    \   ), 0),\
    \   TRUE, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13\
    \ FROM harness.sessions session\
    \ WHERE session.session_key = $1 AND session.deleted_at IS NULL\
    \ RETURNING epoch_index"
    ( (fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (snapshotField (.sessionPromptVersion)
            >$< Encoders.param (Encoders.nonNullable Encoders.int4))
        <> (snapshotField (.sessionPromptCreatedAt)
            >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))
        <> (snapshotField (.sessionPromptProvider)
            >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (snapshotField (.sessionPromptConnection)
            >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (snapshotField (.sessionPromptModel)
            >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (snapshotField (.sessionPromptDialect)
            >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (snapshotField (.sessionPromptCwd)
            >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (snapshotField (.sessionPromptInstructions)
            >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (snapshotField (.sessionPromptTools)
            >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (snapshotField (.sessionPromptGeneratedContext)
            >$< Encoders.param (Encoders.nullable Encoders.text))
        <> (snapshotField (.sessionPromptGrokContext)
            >$< Encoders.param (Encoders.nullable Encoders.text))
        <> (snapshotField (.sessionPromptCacheKey)
            >$< Encoders.param (Encoders.nonNullable Encoders.text))
    )
    (Decoders.rowMaybe (Decoders.column (Decoders.nonNullable Decoders.int8)))
    True
  where
    snapshotField field = field . snd

invalidatePromptEpochStatement
    :: Statement (Text, UTCTime) (Maybe Int64)
invalidatePromptEpochStatement = mkStatement
    "WITH latest AS (\
    \ SELECT epoch.*\
    \ FROM harness.session_prompt_epochs epoch\
    \ JOIN harness.sessions session\
    \   ON session.session_id = epoch.session_id\
    \ WHERE session.session_key = $1 AND session.deleted_at IS NULL\
    \ ORDER BY epoch.epoch_index DESC\
    \ LIMIT 1\
    \ )\
    \ INSERT INTO harness.session_prompt_epochs (\
    \ session_id, epoch_index, is_active, prompt_version, created_at,\
    \ provider, connection_id, model_id, dialect, cwd_text,\
    \ instructions_text, tools_text,\
    \ generated_context_text, grok_context_text, prompt_cache_key\
    \ )\
    \ SELECT latest.session_id, latest.epoch_index + 1, FALSE,\
    \   latest.prompt_version, $2, latest.provider, latest.connection_id,\
    \   latest.model_id, latest.dialect, latest.cwd_text,\
    \   latest.instructions_text, latest.tools_text,\
    \   latest.generated_context_text, latest.grok_context_text,\
    \   latest.prompt_cache_key\
    \ FROM latest\
    \ WHERE latest.is_active\
    \ RETURNING epoch_index"
    ( (fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (snd
            >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))
    )
    (Decoders.rowMaybe (Decoders.column (Decoders.nonNullable Decoders.int8)))
    True

insertSessionStatement :: Statement SessionMetadata Text
insertSessionStatement = mkStatement
    "INSERT INTO harness.sessions (\
    \ session_key, session_schema_version, created_at, updated_at,\
    \ provider, connection_id, gateway_identity, model_id,\
    \ transport_model_id, dialect,\
    \ legacy_target_provider, legacy_target_connection,\
    \ legacy_target_effective_model, legacy_target_dialect,\
    \ cwd, effort, title, title_is_manual, title_refresh_index,\
    \ title_user_turns, last_response_id, input_tokens, output_tokens,\
    \ cached_tokens, last_recap, last_turn_summary, last_recap_main_turns\
    \ ) VALUES (\
    \ $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13,\
    \ $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24, $25,\
    \ $26, $27\
    \ ) RETURNING session_id::text"
    metadataParams
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))
    True

replaceProjectionStatement
    :: Statement SessionMetadata (Maybe (Text, Int64))
replaceProjectionStatement = mkStatement
    (metadataUpdateSql
        <> ", next_event_sequence = next_event_sequence + 1\
           \ WHERE session_key = $1 AND deleted_at IS NULL\
           \ RETURNING session_id::text, next_event_sequence - 1")
    metadataParams
    (Decoders.rowMaybe $
        (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.int8))
    True

appendProjectionStatement
    :: Statement SessionMetadata (Maybe (Text, Int64, Int64))
appendProjectionStatement = mkStatement
    (metadataUpdateSql
        <> ", next_event_sequence = next_event_sequence + 1\
           \, next_turn_index = next_turn_index + 1\
           \ WHERE session_key = $1 AND deleted_at IS NULL\
           \ RETURNING session_id::text, next_event_sequence - 1,\
           \ next_turn_index - 1")
    metadataParams
    (Decoders.rowMaybe $
        (,,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.int8)
            <*> Decoders.column (Decoders.nonNullable Decoders.int8))
    True

metadataUpdateSql :: Text
metadataUpdateSql =
    "UPDATE harness.sessions SET\
    \ session_schema_version = $2, created_at = $3, updated_at = $4,\
    \ provider = $5, connection_id = $6, gateway_identity = $7,\
    \ model_id = $8, transport_model_id = $9, dialect = $10,\
    \ legacy_target_provider = $11, legacy_target_connection = $12,\
    \ legacy_target_effective_model = $13, legacy_target_dialect = $14,\
    \ cwd = $15, effort = $16,\
    \ title = CASE WHEN title_is_manual THEN title ELSE $17 END,\
    \ title_is_manual = CASE\
    \   WHEN title_is_manual THEN title_is_manual ELSE $18 END,\
    \ title_refresh_index = CASE\
    \   WHEN title_is_manual THEN title_refresh_index ELSE $19 END,\
    \ title_user_turns = $20,\
    \ last_response_id = $21, input_tokens = $22, output_tokens = $23,\
    \ cached_tokens = $24, last_recap = $25, last_turn_summary = $26,\
    \ last_recap_main_turns = $27"

setTitleProjectionStatement
    :: Statement TitleUpdate (Maybe (Text, Int64))
setTitleProjectionStatement = mkStatement
    "UPDATE harness.sessions SET\
    \ title = $2, title_is_manual = $3, title_refresh_index = $4,\
    \ updated_at = $5, next_event_sequence = next_event_sequence + 1\
    \ WHERE session_key = $1 AND deleted_at IS NULL\
    \ AND (NOT $6 OR NOT title_is_manual)\
    \ RETURNING session_id::text, next_event_sequence - 1"
    ( ((.titleUpdateKey)
        >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.titleUpdateTitle)
            >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.titleUpdateManual)
            >$< Encoders.param (Encoders.nonNullable Encoders.bool))
        <> ((.titleUpdateRefreshIndex)
            >$< Encoders.param (Encoders.nonNullable Encoders.int4))
        <> ((.titleUpdateOccurredAt)
            >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))
        <> ((.titleUpdateOnlyWhenAutomatic)
            >$< Encoders.param (Encoders.nonNullable Encoders.bool))
    )
    (Decoders.rowMaybe $
        (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.int8))
    True

insertEventStatement :: Statement EventInsert Text
insertEventStatement = mkStatement
    "INSERT INTO harness.session_events\
    \ (session_id, sequence, event_kind, occurred_at)\
    \ VALUES ($1::uuid, $2, $3, $4)\
    \ RETURNING event_id::text"
    ( ((.eventInsertSessionId) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.eventInsertSequence) >$< Encoders.param (Encoders.nonNullable Encoders.int8))
        <> ((.eventInsertKind) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.eventInsertOccurredAt) >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))
    )
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))
    True

insertTurnStatement :: Statement TurnInsert Text
insertTurnStatement = mkStatement
    "INSERT INTO harness.session_turns (\
    \ session_id, event_id, turn_index, event_sequence, occurred_at,\
    \ user_text, assistant_text, error_text, response_id,\
    \ transcript_effect,\
    \ usage_input_tokens, usage_output_tokens, usage_cached_tokens,\
    \ provider_telemetry_json, canonical_item_count\
    \ ) VALUES (\
    \ $1::uuid, $2::uuid, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13,\
    \ $14, $15\
    \ ) RETURNING turn_id::text"
    ( ((.turnInsertSessionId) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.turnInsertEventId) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.turnInsertIndex) >$< Encoders.param (Encoders.nonNullable Encoders.int8))
        <> ((.turnInsertEventSequence) >$< Encoders.param (Encoders.nonNullable Encoders.int8))
        <> (turnOccurredAt >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))
        <> (turnUserText >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (turnAssistantText >$< Encoders.param (Encoders.nullable Encoders.text))
        <> (turnError >$< Encoders.param (Encoders.nullable Encoders.text))
        <> (turnResponseId >$< Encoders.param (Encoders.nullable Encoders.text))
        <> (turnEffect >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((usageField (.sessionUsageInputTokens) . (.turnInsertTurn))
            >$< Encoders.param (Encoders.nullable Encoders.int8))
        <> ((usageField (.sessionUsageOutputTokens) . (.turnInsertTurn))
            >$< Encoders.param (Encoders.nullable Encoders.int8))
        <> ((usageField (.sessionUsageCachedTokens) . (.turnInsertTurn))
            >$< Encoders.param (Encoders.nullable Encoders.int8))
        <> ((.sessionTurnProviderTelemetry) . (.turnInsertTurn)
            >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ( (fromIntegral . length
                . (.sessionTurnItems)
                . (.turnInsertTurn))
            >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    )
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))
    True
  where
    usageField
        :: (SessionUsage -> Int64)
        -> SessionTurn
        -> Maybe Int64
    usageField field turn = field <$> turn.sessionTurnUsage
    turnOccurredAt :: TurnInsert -> UTCTime
    turnOccurredAt value = value.turnInsertTurn.sessionTurnOccurredAt
    turnUserText :: TurnInsert -> Text
    turnUserText value = value.turnInsertTurn.sessionTurnUserText
    turnAssistantText :: TurnInsert -> Maybe Text
    turnAssistantText value = value.turnInsertTurn.sessionTurnAssistantText
    turnError :: TurnInsert -> Maybe Text
    turnError value = value.turnInsertTurn.sessionTurnError
    turnResponseId :: TurnInsert -> Maybe Text
    turnResponseId value = value.turnInsertTurn.sessionTurnResponseId
    turnEffect :: TurnInsert -> Text
    turnEffect value = transcriptEffectText value.turnInsertTurn.sessionTurnEffect

deleteProjectionStatement
    :: Statement (Text, UTCTime) (Maybe (Text, Int64))
deleteProjectionStatement = mkStatement
    "UPDATE harness.sessions\
    \ SET updated_at = $2, deleted_at = $2,\
    \     next_event_sequence = next_event_sequence + 1\
    \ WHERE session_key = $1 AND deleted_at IS NULL\
    \ RETURNING session_id::text, next_event_sequence - 1"
    ( (fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))
    )
    (Decoders.rowMaybe $
        (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.int8))
    True

recordLegacyImportStatement :: Statement (Text, Text, Text) Bool
recordLegacyImportStatement = mkStatement
    "INSERT INTO harness.legacy_session_imports\
    \ (source_path, content_hash, session_id)\
    \ VALUES ($1, $2, $3::uuid)\
    \ ON CONFLICT DO NOTHING"
    ( ((\(value, _, _) -> value) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((\(_, value, _) -> value) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((\(_, _, value) -> value) >$< Encoders.param (Encoders.nonNullable Encoders.text))
    )
    (fmap (> 0) Decoders.rowsAffected)
    True

blockingAdvisoryLockStatement :: Statement Text Bool
blockingAdvisoryLockStatement = mkStatement
    "SELECT true FROM (\
    \ SELECT pg_advisory_xact_lock(hashtextextended($1, 684022778))\
    \ ) AS acquired"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
    True

tryAdvisoryLockStatement :: Statement Text Bool
tryAdvisoryLockStatement = mkStatement
    "SELECT pg_try_advisory_xact_lock(hashtextextended($1, 684022778))"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
    True

metadataParams :: Encoders.Params SessionMetadata
metadataParams =
    ((.sessionMetadataKey) >$< Encoders.param (Encoders.nonNullable Encoders.text))
    <> ((.sessionMetadataVersion) >$< Encoders.param (Encoders.nonNullable Encoders.int4))
    <> ((.sessionMetadataCreatedAt) >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))
    <> ((.sessionMetadataUpdatedAt) >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))
    <> ((.sessionMetadataProvider) >$< Encoders.param (Encoders.nonNullable Encoders.text))
    <> ((.sessionMetadataConnection) >$< Encoders.param (Encoders.nonNullable Encoders.text))
    <> ((.sessionMetadataGatewayIdentity) >$< Encoders.param (Encoders.nullable Encoders.text))
    <> ((.sessionMetadataModel) >$< Encoders.param (Encoders.nonNullable Encoders.text))
    <> ((.sessionMetadataTransportModel) >$< Encoders.param (Encoders.nullable Encoders.text))
    <> ((.sessionMetadataDialect) >$< Encoders.param (Encoders.nonNullable Encoders.text))
    <> ((legacyField (.sessionLegacyProvider)) >$< Encoders.param (Encoders.nullable Encoders.text))
    <> ((legacyField (.sessionLegacyConnection)) >$< Encoders.param (Encoders.nullable Encoders.text))
    <> ((legacyField (.sessionLegacyEffectiveModel)) >$< Encoders.param (Encoders.nullable Encoders.text))
    <> ((legacyField (.sessionLegacyDialect)) >$< Encoders.param (Encoders.nullable Encoders.text))
    <> ((.sessionMetadataCwd) >$< Encoders.param (Encoders.nonNullable Encoders.text))
    <> ((.sessionMetadataEffort) >$< Encoders.param (Encoders.nonNullable Encoders.text))
    <> ((.sessionMetadataTitle) >$< Encoders.param (Encoders.nonNullable Encoders.text))
    <> ((.sessionMetadataTitleIsManual) >$< Encoders.param (Encoders.nonNullable Encoders.bool))
    <> ((.sessionMetadataTitleRefreshIndex) >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    <> ((.sessionMetadataTitleUserTurns) >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    <> ((.sessionMetadataLastResponseId) >$< Encoders.param (Encoders.nullable Encoders.text))
    <> ((.sessionMetadataInputTokens) >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    <> ((.sessionMetadataOutputTokens) >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    <> ((.sessionMetadataCachedTokens) >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    <> ((.sessionMetadataLastRecap) >$< Encoders.param (Encoders.nullable Encoders.text))
    <> ((.sessionMetadataLastTurnSummary) >$< Encoders.param (Encoders.nullable Encoders.text))
    <> ((.sessionMetadataLastRecapMainTurns) >$< Encoders.param (Encoders.nonNullable Encoders.int8))
  where
    legacyField
        :: (SessionLegacyTarget -> Text)
        -> SessionMetadata
        -> Maybe Text
    legacyField field metadata =
        field <$> metadata.sessionMetadataLegacyTarget
