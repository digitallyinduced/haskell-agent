module Agent.Store.Postgres.SessionItem.Mapping.Statements.Reference
    ( ReferenceRow (..)
    , loadReferenceStatement
    , loadReferencesStatement
    ) where

import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)

import Agent.Store.Postgres.Hasql (mkStatement)

data ReferenceRow = ReferenceRow
    { referenceRowProviderItemId :: !Text
    , referenceRowExtraFields :: !Text
    }

loadReferenceStatement :: Statement Text (Maybe ReferenceRow)
loadReferenceStatement = mkStatement
    "SELECT provider_item_id, extra_fields_text\
    \ FROM harness.session_item_references\
    \ WHERE response_item_id = $1::uuid"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowMaybe referenceRowDecoder)
    True

loadReferencesStatement :: Statement Text [(Text, ReferenceRow)]
loadReferencesStatement = mkStatement
    "SELECT child.response_item_id::text, child.provider_item_id,\
    \ child.extra_fields_text\
    \ FROM harness.session_item_references child\
    \ WHERE child.response_item_id = ANY (ARRAY(\
    \   SELECT item.response_item_id\
    \   FROM harness.session_response_items item\
    \   WHERE item.turn_id = $1::uuid\
    \ ))"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowList $
        (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> referenceRowDecoder)
    True

referenceRowDecoder :: Decoders.Row ReferenceRow
referenceRowDecoder =
    ReferenceRow
        <$> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
