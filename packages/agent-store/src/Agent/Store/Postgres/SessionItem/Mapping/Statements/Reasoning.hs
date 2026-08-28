module Agent.Store.Postgres.SessionItem.Mapping.Statements.Reasoning
    ( ReasoningRow (..)
    , loadReasoningStatement
    , loadReasoningItemsStatement
    ) where

import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)

import Agent.Store.Postgres.Hasql (mkStatement)

data ReasoningRow = ReasoningRow
    { reasoningRowProviderItemId :: !(Maybe Text)
    , reasoningRowHasContent :: !Bool
    , reasoningRowEncryptedContent :: !(Maybe Text)
    , reasoningRowStatus :: !(Maybe Text)
    , reasoningRowExtraFields :: !Text
    }

loadReasoningStatement :: Statement Text (Maybe ReasoningRow)
loadReasoningStatement = mkStatement
    "SELECT provider_item_id, has_content, encrypted_content, status_name,\
    \ extra_fields_text\
    \ FROM harness.session_reasoning_items\
    \ WHERE response_item_id = $1::uuid"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowMaybe reasoningRowDecoder)
    True

loadReasoningItemsStatement :: Statement Text [(Text, ReasoningRow)]
loadReasoningItemsStatement = mkStatement
    "SELECT child.response_item_id::text, child.provider_item_id,\
    \ child.has_content, child.encrypted_content, child.status_name,\
    \ child.extra_fields_text\
    \ FROM harness.session_reasoning_items child\
    \ WHERE child.response_item_id = ANY (ARRAY(\
    \   SELECT item.response_item_id\
    \   FROM harness.session_response_items item\
    \   WHERE item.turn_id = $1::uuid\
    \ ))"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowList $
        (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> reasoningRowDecoder)
    True

reasoningRowDecoder :: Decoders.Row ReasoningRow
reasoningRowDecoder =
    ReasoningRow
        <$> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.bool)
        <*> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
