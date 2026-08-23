{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Lossless relational storage for arbitrary Aeson values.
--
-- Values are represented as an adjacency tree.  Objects and arrays own
-- ordered child rows; scalar payloads live in typed nullable columns.  No
-- JSON or JSONB value is stored in PostgreSQL.
module Agent.Store.Postgres.NormalizedValue
    ( normalizedSupportSchemaStatements
    , normalizedValueSchemaStatements
    , insertNormalizedValue
    , loadNormalizedValue
    , loadNormalizedValueRequired
    ) where

import Control.Monad (foldM, forM)
import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int32)
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)
import qualified Hasql.Transaction as Transaction

import Agent.Store.Postgres.Hasql (mkStatement)

-- | Harness support DDL used before normalized value tables are created.
--
-- UUIDv7 values come directly from PostgreSQL 18's native @uuidv7()@.
normalizedSupportSchemaStatements :: [ByteString.ByteString]
normalizedSupportSchemaStatements =
    [ "CREATE SCHEMA IF NOT EXISTS harness"
    , "CREATE OR REPLACE FUNCTION harness.raise_normalized_value_error(\
      \ error_message text\
      \ ) RETURNS boolean\
      \ LANGUAGE plpgsql\
      \ VOLATILE\
      \ PARALLEL RESTRICTED\
      \ AS $$\
      \ BEGIN\
      \   RAISE EXCEPTION USING\
      \     ERRCODE = '22000',\
      \     MESSAGE = error_message;\
      \   RETURN false;\
      \ END\
      \ $$"
    ]

normalizedValueSchemaStatements :: [ByteString.ByteString]
normalizedValueSchemaStatements =
    normalizedSupportSchemaStatements
    <> [ "CREATE TABLE IF NOT EXISTS harness.structured_values (\
      \ value_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7(),\
      \ root_value_id uuid NOT NULL\
      \   REFERENCES harness.structured_values(value_id) ON DELETE CASCADE,\
      \ parent_value_id uuid\
      \   REFERENCES harness.structured_values(value_id) ON DELETE CASCADE,\
      \ member_key text,\
      \ element_index integer,\
      \ value_kind text NOT NULL\
      \   CHECK (value_kind IN\
      \     ('null', 'boolean', 'number', 'string', 'array', 'object')),\
      \ text_value text,\
      \ number_value text,\
      \ boolean_value boolean,\
      \ CHECK (\
      \   (parent_value_id IS NULL\
      \     AND member_key IS NULL AND element_index IS NULL)\
      \   OR\
      \   (parent_value_id IS NOT NULL\
      \     AND element_index IS NOT NULL AND element_index >= 0)\
      \ ),\
      \ CHECK (\
      \   (value_kind = 'null'\
      \     AND text_value IS NULL\
      \     AND number_value IS NULL\
      \     AND boolean_value IS NULL)\
      \   OR (value_kind = 'boolean'\
      \     AND text_value IS NULL\
      \     AND number_value IS NULL\
      \     AND boolean_value IS NOT NULL)\
      \   OR (value_kind = 'number'\
      \     AND text_value IS NULL\
      \     AND number_value IS NOT NULL\
      \     AND boolean_value IS NULL)\
      \   OR (value_kind = 'string'\
      \     AND text_value IS NOT NULL\
      \     AND number_value IS NULL\
      \     AND boolean_value IS NULL)\
      \   OR (value_kind IN ('array', 'object')\
      \     AND text_value IS NULL\
      \     AND number_value IS NULL\
      \     AND boolean_value IS NULL)\
      \ )\
      \ )"
    , "CREATE INDEX IF NOT EXISTS structured_values_root_idx\
      \ ON harness.structured_values (root_value_id)"
    , "CREATE UNIQUE INDEX IF NOT EXISTS structured_values_object_key_idx\
      \ ON harness.structured_values (parent_value_id, member_key)\
      \ WHERE member_key IS NOT NULL"
    , "CREATE UNIQUE INDEX IF NOT EXISTS structured_values_child_order_idx\
      \ ON harness.structured_values (parent_value_id, element_index)\
      \ WHERE parent_value_id IS NOT NULL"
    ]

-- | Insert an Aeson value tree and return its UUIDv7 root identifier as text.
insertNormalizedValue :: Value -> Transaction.Transaction Text
insertNormalizedValue value = do
    rootId <- Transaction.statement (scalarFields value) insertRootStatement
    insertChildren rootId rootId value
    pure rootId

insertChildren
    :: Text
    -> Text
    -> Value
    -> Transaction.Transaction ()
insertChildren rootId parentId = \case
    Object object ->
        mapM_
            (\(index, (key, child)) ->
                insertChild
                    rootId
                    parentId
                    (Just (Key.toText key))
                    index
                    child)
            (zip [0..] (KeyMap.toList object))
    Array values ->
        mapM_
            (\(index, child) ->
                insertChild rootId parentId Nothing index child)
            (zip [0..] (Foldable.toList values))
    _ -> pure ()

insertChild
    :: Text
    -> Text
    -> Maybe Text
    -> Int
    -> Value
    -> Transaction.Transaction ()
insertChild rootId parentId memberKey elementIndex value = do
    childId <- Transaction.statement
        ChildParams
            { childRootId = rootId
            , childParentId = parentId
            , childMemberKey = memberKey
            , childElementIndex = fromIntegral elementIndex
            , childFields = scalarFields value
            }
        insertChildStatement
    insertChildren rootId childId value

data ScalarFields = ScalarFields
    { scalarKind :: !Text
    , scalarText :: !(Maybe Text)
    , scalarNumber :: !(Maybe Text)
    , scalarBoolean :: !(Maybe Bool)
    }

scalarFields :: Value -> ScalarFields
scalarFields = \case
    Null -> ScalarFields "null" Nothing Nothing Nothing
    Bool value -> ScalarFields "boolean" Nothing Nothing (Just value)
    Number value ->
        ScalarFields
            "number"
            Nothing
            (Just (Text.decodeUtf8 (LazyByteString.toStrict (Aeson.encode value))))
            Nothing
    String value -> ScalarFields "string" (Just value) Nothing Nothing
    Array _ -> ScalarFields "array" Nothing Nothing Nothing
    Object _ -> ScalarFields "object" Nothing Nothing Nothing

data ChildParams = ChildParams
    { childRootId :: !Text
    , childParentId :: !Text
    , childMemberKey :: !(Maybe Text)
    , childElementIndex :: !Int32
    , childFields :: !ScalarFields
    }

insertRootStatement :: Statement ScalarFields Text
insertRootStatement = mkStatement
    "WITH generated AS (SELECT pg_catalog.uuidv7() AS value_id)\
    \ INSERT INTO harness.structured_values\
    \   (value_id, root_value_id, value_kind,\
    \    text_value, number_value, boolean_value)\
    \ SELECT value_id, value_id, $1, $2, $3, $4\
    \ FROM generated\
    \ RETURNING value_id::text"
    scalarEncoder
    textSingleRow
    True

insertChildStatement :: Statement ChildParams Text
insertChildStatement = mkStatement
    "INSERT INTO harness.structured_values\
    \ (root_value_id, parent_value_id, member_key, element_index,\
    \  value_kind, text_value, number_value, boolean_value)\
    \ VALUES ($1::uuid, $2::uuid, $3, $4, $5, $6, $7, $8)\
    \ RETURNING value_id::text"
    childEncoder
    textSingleRow
    True

scalarEncoder :: Encoders.Params ScalarFields
scalarEncoder =
    ((.scalarKind) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.scalarText) >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((.scalarNumber) >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((.scalarBoolean) >$< Encoders.param (Encoders.nullable Encoders.bool))

childEncoder :: Encoders.Params ChildParams
childEncoder =
    ((.childRootId) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.childParentId) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.childMemberKey) >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((.childElementIndex) >$< Encoders.param (Encoders.nonNullable Encoders.int4))
        <> ((.childFields) >$< scalarEncoder)

textSingleRow :: Decoders.Result Text
textSingleRow =
    Decoders.singleRow $
        Decoders.column (Decoders.nonNullable Decoders.text)

-- | Load and validate a complete normalized value tree.
--
-- A missing root returns 'Right Nothing'.  Corrupt, disconnected, cyclic, or
-- structurally inconsistent rows return a descriptive 'Left'.
loadNormalizedValue
    :: Text
    -> Transaction.Transaction (Either Text (Maybe Value))
loadNormalizedValue rootId = do
    rows <- Transaction.statement rootId loadTreeStatement
    pure $
        case rows of
            [] -> Right Nothing
            _ -> Just <$> rebuildTree rootId rows

-- | Load a value whose root must exist and be structurally valid.
--
-- Unlike 'loadNormalizedValue', failures abort the PostgreSQL transaction and
-- therefore surface through the surrounding Hasql session error.  Hasql's
-- 'Transaction' type has no public custom-error operation, so a small
-- harness-owned PostgreSQL function raises a data exception.  The fallback
-- value is unreachable after that statement fails and avoids using a partial
-- Haskell value such as 'error'.
loadNormalizedValueRequired :: Text -> Transaction.Transaction Value
loadNormalizedValueRequired rootId =
    loadNormalizedValue rootId >>= \case
        Right (Just value) -> pure value
        Right Nothing ->
            abortNormalizedValueLoad
                ("normalized value root does not exist: " <> rootId)
        Left err ->
            abortNormalizedValueLoad
                ("invalid normalized value " <> rootId <> ": " <> err)

abortNormalizedValueLoad :: Text -> Transaction.Transaction Value
abortNormalizedValueLoad message = do
    _ <- Transaction.statement message raiseLoadErrorStatement
    pure Null

raiseLoadErrorStatement :: Statement Text Bool
raiseLoadErrorStatement = mkStatement
    "SELECT harness.raise_normalized_value_error($1)"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.singleRow $
        Decoders.column (Decoders.nonNullable Decoders.bool))
    True

data StoredValue = StoredValue
    { storedId :: !Text
    , storedRootId :: !Text
    , storedParentId :: !(Maybe Text)
    , storedMemberKey :: !(Maybe Text)
    , storedElementIndex :: !(Maybe Int32)
    , storedKind :: !Text
    , storedText :: !(Maybe Text)
    , storedNumber :: !(Maybe Text)
    , storedBoolean :: !(Maybe Bool)
    }
    deriving (Eq, Show)

loadTreeStatement :: Statement Text [StoredValue]
loadTreeStatement = mkStatement
    "SELECT value_id::text, root_value_id::text, parent_value_id::text,\
    \ member_key, element_index, value_kind,\
    \ text_value, number_value, boolean_value\
    \ FROM harness.structured_values\
    \ WHERE root_value_id = $1::uuid\
    \ ORDER BY parent_value_id NULLS FIRST, element_index NULLS FIRST"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowList storedValueRow)
    True

storedValueRow :: Decoders.Row StoredValue
storedValueRow =
    StoredValue
        <$> textColumn
        <*> textColumn
        <*> nullableTextColumn
        <*> nullableTextColumn
        <*> nullableInt32Column
        <*> textColumn
        <*> nullableTextColumn
        <*> nullableTextColumn
        <*> nullableBoolColumn

rebuildTree :: Text -> [StoredValue] -> Either Text Value
rebuildTree rootId rows = do
    let byId = Map.fromList [(row.storedId, row) | row <- rows]
        children = Map.fromListWith (<>)
            [ (parentId, [row])
            | row <- rows
            , Just parentId <- [row.storedParentId]
            ]
    if Map.size byId /= length rows
        then Left "normalized value tree contains duplicate value identifiers"
        else pure ()
    root <- maybe
        (Left "normalized value tree does not contain its requested root")
        Right
        (Map.lookup rootId byId)
    if root.storedRootId /= rootId
        then Left "normalized value root points at a different tree"
        else pure ()
    if isNothing root.storedParentId
        && isNothing root.storedMemberKey
        && isNothing root.storedElementIndex
        then pure ()
        else Left "normalized value root has child relationship fields"
    (value, visited) <- buildValue byId children Set.empty root
    if Set.size visited /= length rows
        then Left "normalized value tree contains unreachable rows"
        else Right value

buildValue
    :: Map.Map Text StoredValue
    -> Map.Map Text [StoredValue]
    -> Set.Set Text
    -> StoredValue
    -> Either Text (Value, Set.Set Text)
buildValue byId children visiting row
    | Set.member row.storedId visiting =
        Left "normalized value tree contains a cycle"
    | otherwise = do
        if row.storedRootId == rootId
            then pure ()
            else Left "normalized value row points at a different root"
        let descendants = orderedChildren (Map.findWithDefault [] row.storedId children)
            visiting' = Set.insert row.storedId visiting
        case row.storedKind of
            "null" -> scalar Null descendants
            "boolean" -> case row.storedBoolean of
                Just value -> scalar (Bool value) descendants
                Nothing -> Left "normalized boolean is missing its value"
            "number" -> case row.storedNumber of
                Just value -> do
                    number <- decodeNumber value
                    scalar number descendants
                Nothing -> Left "normalized number is missing its value"
            "string" -> case row.storedText of
                Just value -> scalar (String value) descendants
                Nothing -> Left "normalized string is missing its value"
            "array" -> do
                validateOrderedChildren False descendants
                (values, visited) <- foldM
                    (\(values, seen) child -> do
                        (value, childSeen) <-
                            buildValue byId children (visiting' <> seen) child
                        pure (value : values, seen <> childSeen))
                    ([], Set.singleton row.storedId)
                    descendants
                pure (Aeson.toJSON (reverse values), visited)
            "object" -> do
                validateOrderedChildren True descendants
                (members, visited) <- foldM
                    (\(members, seen) child -> do
                        key <- maybe
                            (Left "normalized object child is missing its key")
                            Right
                            child.storedMemberKey
                        (value, childSeen) <-
                            buildValue byId children (visiting' <> seen) child
                        pure
                            ( (Key.fromText key, value) : members
                            , seen <> childSeen
                            ))
                    ([], Set.singleton row.storedId)
                    descendants
                let orderedMembers = reverse members
                if Set.size (Set.fromList (map (Key.toText . fst) orderedMembers))
                        /= length orderedMembers
                    then Left "normalized object contains duplicate member keys"
                    else pure (Object (KeyMap.fromList orderedMembers), visited)
            unknown ->
                Left ("unknown normalized value kind: " <> unknown)
  where
    rootId = row.storedRootId
    scalar value descendants
        | null descendants =
            Right (value, Set.singleton row.storedId)
        | otherwise =
            Left ("normalized scalar has child rows: " <> row.storedId)

orderedChildren :: [StoredValue] -> [StoredValue]
orderedChildren =
    List.sortOn (.storedElementIndex)

validateOrderedChildren :: Bool -> [StoredValue] -> Either Text ()
validateOrderedChildren objectChildren children = do
    indexes <- forM children \child ->
        maybe
            (Left "normalized child is missing its element index")
            (Right . fromIntegral)
            child.storedElementIndex
    if indexes == [0 .. length indexes - 1]
        then pure ()
        else Left "normalized child indexes are not contiguous"
    if objectChildren
        then if all (not . isNothing . (.storedMemberKey)) children
            then pure ()
            else Left "normalized object child is missing its member key"
        else if all (isNothing . (.storedMemberKey)) children
            then pure ()
            else Left "normalized array child unexpectedly has a member key"

decodeNumber :: Text -> Either Text Value
decodeNumber encoded =
    case Aeson.eitherDecodeStrict' (Text.encodeUtf8 encoded) of
        Right value@(Number _) -> Right value
        Right _ -> Left "normalized number decoded to a non-number"
        Left err ->
            Left ("invalid normalized number: " <> Text.pack err)

textColumn :: Decoders.Row Text
textColumn =
    Decoders.column (Decoders.nonNullable Decoders.text)

nullableTextColumn :: Decoders.Row (Maybe Text)
nullableTextColumn =
    Decoders.column (Decoders.nullable Decoders.text)

nullableInt32Column :: Decoders.Row (Maybe Int32)
nullableInt32Column =
    Decoders.column (Decoders.nullable Decoders.int4)

nullableBoolColumn :: Decoders.Row (Maybe Bool)
nullableBoolColumn =
    Decoders.column (Decoders.nullable Decoders.bool)
