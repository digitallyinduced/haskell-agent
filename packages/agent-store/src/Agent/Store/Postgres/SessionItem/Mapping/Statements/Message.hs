module Agent.Store.Postgres.SessionItem.Mapping.Statements.Message
    ( MessageRow (..)
    , loadMessageStatement
    , loadMessagesStatement
    ) where

import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)

import Agent.Store.Postgres.Hasql (mkStatement)

data MessageRow = MessageRow
    { messageRowProviderItemId :: !(Maybe Text)
    , messageRowRole :: !Text
    , messageRowStatus :: !(Maybe Text)
    , messageRowPhase :: !(Maybe Text)
    , messageRowContentKind :: !Text
    , messageRowContentText :: !(Maybe Text)
    , messageRowExtraFields :: !Text
    }

loadMessageStatement :: Statement Text (Maybe MessageRow)
loadMessageStatement = mkStatement
    "SELECT provider_item_id, role_name, status_name, phase, content_kind,\
    \ content_text, extra_fields_text\
    \ FROM harness.session_messages\
    \ WHERE response_item_id = $1::uuid"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowMaybe messageRowDecoder)
    True

loadMessagesStatement :: Statement Text [(Text, MessageRow)]
loadMessagesStatement = mkStatement
    "SELECT child.response_item_id::text, child.provider_item_id,\
    \ child.role_name, child.status_name, child.phase, child.content_kind,\
    \ child.content_text, child.extra_fields_text\
    \ FROM harness.session_messages child\
    \ WHERE child.response_item_id = ANY (ARRAY(\
    \   SELECT item.response_item_id\
    \   FROM harness.session_response_items item\
    \   WHERE item.turn_id = $1::uuid\
    \ ))"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowList $
        (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> messageRowDecoder)
    True

messageRowDecoder :: Decoders.Row MessageRow
messageRowDecoder =
    MessageRow
        <$> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
