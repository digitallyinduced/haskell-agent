module Agent.Store.Postgres.SessionItem.Mapping.Statements.Message
    ( MessageRow (..)
    , loadMessageStatement
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
    (Decoders.rowMaybe $
        MessageRow
            <$> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text))
    True
