module Agent.Store.Postgres.SessionItem.Mapping.Statements.Reasoning
    ( ReasoningRow (..)
    , loadReasoningStatement
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
    (Decoders.rowMaybe $
        ReasoningRow
            <$> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.bool)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text))
    True
