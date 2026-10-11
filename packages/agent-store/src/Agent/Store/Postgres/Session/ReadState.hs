{-# LANGUAGE OverloadedStrings #-}

-- | Durable attention state, independent of imported/replaced session metadata.
module Agent.Store.Postgres.Session.ReadState
    ( SessionReadState(..)
    , loadSessionReadState
    , loadSessionReadStates
    , updateSessionReadState
    , publishSessionReadStateTransaction
    , sessionReadStateSchemaStatements
    ) where

import Data.ByteString (ByteString)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Session as S
import Hasql.Statement (Statement)
import qualified Hasql.Transaction as T

import Agent.Store.Postgres.Connection (StorePool, withSession)
import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.Types (StoreError)

data SessionReadState = SessionReadState
    { readStateRevision :: !Text
    , readStateUnread :: !Bool
    , readStateFirstUnreadTurn :: !(Maybe Int64)
    } deriving (Eq, Show)

sessionReadStateSchemaStatements :: [ByteString]
sessionReadStateSchemaStatements =
    [ "ALTER TABLE harness.sessions\
      \ ADD COLUMN IF NOT EXISTS read_state_revision uuid NOT NULL DEFAULT pg_catalog.uuidv7(),\
      \ ADD COLUMN IF NOT EXISTS unread boolean NOT NULL DEFAULT false,\
      \ ADD COLUMN IF NOT EXISTS first_unread_turn bigint,\
      \ ADD COLUMN IF NOT EXISTS last_published_turn bigint"
    ]

loadSessionReadState :: StorePool -> Text -> IO (Either StoreError (Maybe SessionReadState))
loadSessionReadState pool key = withSession pool (S.statement key loadStatement)

-- | Batch lookup for already-authorized session list pages.
loadSessionReadStates :: StorePool -> [Text] -> IO (Either StoreError [(Text, SessionReadState)])
loadSessionReadStates pool keys = withSession pool (S.statement keys loadManyStatement)

loadManyStatement :: Statement [Text] [(Text, SessionReadState)]
loadManyStatement = mkStatement
    "SELECT session_key, read_state_revision::text, unread, first_unread_turn\
    \ FROM harness.sessions WHERE session_key = ANY($1::text[]) AND deleted_at IS NULL"
    (E.param (E.nonNullable (E.foldableArray (E.nonNullable E.text))))
    (D.rowList ((,) <$> D.column (D.nonNullable D.text) <*> stateDecoder)) True

-- | Compare the revision actually displayed to the client. Nothing means a
-- conflict or an unavailable session; callers must not retry with a newer token.
updateSessionReadState
    :: StorePool -> Text -> Text -> Bool
    -> IO (Either StoreError (Maybe SessionReadState))
updateSessionReadState pool key revision unread =
    withSession pool (S.statement (key, revision, unread) updateStatement)

-- | Called within the live append transaction, never during history import.
-- Even when already unread, every new result invalidates stale acknowledgements.
publishSessionReadStateTransaction :: Text -> T.Transaction ()
publishSessionReadStateTransaction key = T.statement key publishStatement

stateDecoder :: D.Row SessionReadState
stateDecoder = SessionReadState
    <$> D.column (D.nonNullable D.text)
    <*> D.column (D.nonNullable D.bool)
    <*> D.column (D.nullable D.int8)

loadStatement :: Statement Text (Maybe SessionReadState)
loadStatement = mkStatement
    "SELECT read_state_revision::text, unread, first_unread_turn\
    \ FROM harness.sessions WHERE session_key = $1 AND deleted_at IS NULL"
    (E.param (E.nonNullable E.text)) (D.rowMaybe stateDecoder) True

updateStatement :: Statement (Text, Text, Bool) (Maybe SessionReadState)
updateStatement = mkStatement
    "UPDATE harness.sessions SET read_state_revision = pg_catalog.uuidv7(),\
    \ unread = $3, first_unread_turn = NULL\
    \ WHERE session_key = $1 AND deleted_at IS NULL AND read_state_revision::text = $2\
    \ RETURNING read_state_revision::text, unread, first_unread_turn"
    ( ((\(key, _, _) -> key) >$< E.param (E.nonNullable E.text))
        <> ((\(_, revision, _) -> revision) >$< E.param (E.nonNullable E.text))
        <> ((\(_, _, unread) -> unread) >$< E.param (E.nonNullable E.bool))
    ) (D.rowMaybe stateDecoder) True

publishStatement :: Statement Text ()
publishStatement = mkStatement
    "UPDATE harness.sessions SET read_state_revision = pg_catalog.uuidv7(),\
    \ first_unread_turn = CASE WHEN unread THEN first_unread_turn ELSE next_turn_index - 1 END,\
    \ unread = true, last_published_turn = next_turn_index - 1\
    \ WHERE session_key = $1 AND deleted_at IS NULL AND next_turn_index > 0\
    \ AND last_published_turn IS DISTINCT FROM next_turn_index - 1"
    (E.param (E.nonNullable E.text)) D.noResult True
