module Agent.Store.Postgres.SessionItem.Mapping.Statements.CustomOutput
    ( CustomOutputRow (..)
    , loadCustomOutputStatement
    ) where

import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)

import Agent.Store.Postgres.Hasql (mkStatement)

data CustomOutputRow = CustomOutputRow
    { customOutputRowProviderItemId :: !(Maybe Text)
    , customOutputRowCallId :: !Text
    , customOutputRowName :: !(Maybe Text)
    , customOutputRowKind :: !Text
    , customOutputRowText :: !Text
    , customOutputRowStatus :: !(Maybe Text)
    , customOutputRowExtraFields :: !Text
    }

loadCustomOutputStatement :: Statement Text (Maybe CustomOutputRow)
loadCustomOutputStatement = mkStatement
    "SELECT provider_item_id, call_id, tool_name, output_kind, output_text,\
    \ status_name, extra_fields_text\
    \ FROM harness.session_custom_tool_call_outputs\
    \ WHERE response_item_id = $1::uuid"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowMaybe $
        CustomOutputRow
            <$> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text))
    True
