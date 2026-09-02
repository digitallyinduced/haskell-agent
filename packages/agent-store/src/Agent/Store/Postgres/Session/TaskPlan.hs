{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Mutable, revisioned task-plan state associated with a session.
module Agent.Store.Postgres.Session.TaskPlan
    ( loadSessionTaskPlan
    , replaceSessionTaskPlan
    , clearSessionTaskPlan
    , copySessionTaskPlan
    ) where

import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Vector as Vector
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Transaction as Transaction
import qualified Hasql.Transaction.Sessions as Transactions
import Hasql.Statement (Statement)

import Agent.Store.Postgres.Connection (StorePool, withSession)
import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.Postgres.Session.Types
import Agent.Store.Types (StoreError(..))

loadSessionTaskPlan
    :: StorePool
    -> Text
    -> IO (Either StoreError (Maybe SessionTaskPlan))
loadSessionTaskPlan pool sessionKey = do
    result <- withSession pool $
        Transactions.transaction
            Transactions.RepeatableRead
            Transactions.Read do
                Transaction.statement sessionKey loadPlanHeaderStatement >>= \case
                    Nothing -> pure Nothing
                    Just (sessionId, revision, explanation) -> do
                        items <- Transaction.statement sessionId loadPlanItemsStatement
                        pure (Just (revision, explanation, Vector.toList items))
    pure (result >>= traverse decodePlan)

replaceSessionTaskPlan
    :: StorePool
    -> Text
    -> Maybe Text
    -> [SessionTaskPlanItem]
    -> IO (Either StoreError (Maybe Int64))
replaceSessionTaskPlan pool sessionKey explanation items =
    withSession pool $
        Transactions.transaction Transactions.Serializable Transactions.Write do
            Transaction.statement
                (sessionKey, explanation)
                replacePlanHeaderStatement >>= \case
                    Nothing -> pure Nothing
                    Just (sessionId, revision) -> do
                        _ <- Transaction.statement sessionId deletePlanItemsStatement
                        insertItems sessionId items
                        pure (Just revision)

clearSessionTaskPlan
    :: StorePool
    -> Text
    -> IO (Either StoreError Bool)
clearSessionTaskPlan pool sessionKey =
    withSession pool $
        Transactions.transaction Transactions.Serializable Transactions.Write $
            Transaction.statement sessionKey clearPlanStatement

-- | Copy the source's current plan into the target. Returns 'False' when the
-- source has no plan or the target session does not exist.
copySessionTaskPlan
    :: StorePool
    -> Text
    -> Text
    -> IO (Either StoreError Bool)
copySessionTaskPlan pool sourceKey targetKey =
    withSession pool $
        Transactions.transaction Transactions.Serializable Transactions.Write do
            Transaction.statement sourceKey loadPlanHeaderStatement >>= \case
                Nothing -> pure False
                Just (sourceId, _, explanation) -> do
                    sourceItems <- Vector.toList
                        <$> Transaction.statement sourceId loadPlanItemsStatement
                    Transaction.statement
                        (targetKey, explanation)
                        replacePlanHeaderStatement >>= \case
                            Nothing -> pure False
                            Just (targetId, _) -> do
                                _ <- Transaction.statement
                                    targetId deletePlanItemsStatement
                                case traverse decodeItem sourceItems of
                                    Left _ -> Transaction.condemn >> pure False
                                    Right items -> insertItems targetId items >> pure True

insertItems :: Text -> [SessionTaskPlanItem] -> Transaction.Transaction ()
insertItems sessionId items =
    mapM_ (\(itemIndex, item) ->
        Transaction.statement
            (sessionId, itemIndex, item.sessionTaskPlanItemStep, encodeStatus item.sessionTaskPlanItemStatus)
            insertPlanItemStatement)
        (zip [0 :: Int64 ..] items)

decodePlan
    :: (Int64, Maybe Text, [(Text, Text)])
    -> Either StoreError SessionTaskPlan
decodePlan (revision, explanation, rawItems) =
    SessionTaskPlan revision explanation <$> traverse decodeItem rawItems

decodeItem :: (Text, Text) -> Either StoreError SessionTaskPlanItem
decodeItem (step, status) =
    SessionTaskPlanItem step <$> case status of
        "pending" -> Right SessionTaskPlanPending
        "in_progress" -> Right SessionTaskPlanInProgress
        "completed" -> Right SessionTaskPlanCompleted
        invalid -> Left (StoreDataError ("invalid task-plan status: " <> invalid))

encodeStatus :: SessionTaskPlanStatus -> Text
encodeStatus = \case
    SessionTaskPlanPending -> "pending"
    SessionTaskPlanInProgress -> "in_progress"
    SessionTaskPlanCompleted -> "completed"

loadPlanHeaderStatement :: Statement Text (Maybe (Text, Int64, Maybe Text))
loadPlanHeaderStatement = mkStatement
    "SELECT p.session_id::text, p.revision, p.explanation\
    \ FROM harness.session_task_plans p\
    \ JOIN harness.sessions s ON s.session_id = p.session_id\
    \ WHERE s.session_key = $1 AND s.deleted_at IS NULL"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowMaybe $
        (,,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.int8)
            <*> Decoders.column (Decoders.nullable Decoders.text))
    True

loadPlanItemsStatement :: Statement Text (Vector.Vector (Text, Text))
loadPlanItemsStatement = mkStatement
    "SELECT step_text, status\
    \ FROM harness.session_task_plan_items\
    \ WHERE session_id = $1::uuid\
    \ ORDER BY item_index"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowVector $
        (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text))
    True

replacePlanHeaderStatement
    :: Statement (Text, Maybe Text) (Maybe (Text, Int64))
replacePlanHeaderStatement = mkStatement
    "INSERT INTO harness.session_task_plans\
    \ (session_id, revision, updated_at, explanation)\
    \ SELECT session_id, 1, now(), $2\
    \ FROM harness.sessions\
    \ WHERE session_key = $1 AND deleted_at IS NULL\
    \ ON CONFLICT (session_id) DO UPDATE SET\
    \ revision = harness.session_task_plans.revision + 1,\
    \ updated_at = EXCLUDED.updated_at,\
    \ explanation = EXCLUDED.explanation\
    \ RETURNING session_id::text, revision"
    ((fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (snd >$< Encoders.param (Encoders.nullable Encoders.text)))
    (Decoders.rowMaybe $
        (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.int8))
    True

deletePlanItemsStatement :: Statement Text ()
deletePlanItemsStatement = mkStatement
    "DELETE FROM harness.session_task_plan_items WHERE session_id = $1::uuid"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    Decoders.noResult
    True

insertPlanItemStatement :: Statement (Text, Int64, Text, Text) ()
insertPlanItemStatement = mkStatement
    "INSERT INTO harness.session_task_plan_items\
    \ (session_id, item_index, step_text, status)\
    \ VALUES ($1::uuid, $2, $3, $4)"
    ( ((\(a, _, _, _) -> a)
        >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((\(_, b, _, _) -> b) >$< Encoders.param (Encoders.nonNullable Encoders.int8))
        <> ((\(_, _, c, _) -> c) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((\(_, _, _, d) -> d) >$< Encoders.param (Encoders.nonNullable Encoders.text)))
    Decoders.noResult
    True

clearPlanStatement :: Statement Text Bool
clearPlanStatement = mkStatement
    "WITH deleted AS (\
    \ DELETE FROM harness.session_task_plans p\
    \ USING harness.sessions s\
    \ WHERE p.session_id = s.session_id AND s.session_key = $1\
    \ RETURNING 1)\
    \ SELECT EXISTS (SELECT 1 FROM deleted)"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
    True
