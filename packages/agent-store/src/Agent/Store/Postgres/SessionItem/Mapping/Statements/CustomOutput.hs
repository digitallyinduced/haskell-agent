module Agent.Store.Postgres.SessionItem.Mapping.Statements.CustomOutput
    ( CustomOutputRow (..)
    , loadCustomOutputStatement
    , loadCustomOutputsStatement
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
    (Decoders.rowMaybe customOutputRowDecoder)
    True

loadCustomOutputsStatement :: Statement Text [(Text, CustomOutputRow)]
loadCustomOutputsStatement = mkStatement
    "SELECT child.response_item_id::text, child.provider_item_id,\
    \ child.call_id, child.tool_name, child.output_kind, child.output_text,\
    \ child.status_name, child.extra_fields_text\
    \ FROM harness.session_custom_tool_call_outputs child\
    \ WHERE child.response_item_id = ANY (ARRAY(\
    \   SELECT item.response_item_id\
    \   FROM harness.session_response_items item\
    \   WHERE item.turn_id = $1::uuid\
    \ ))"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowList $
        (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> customOutputRowDecoder)
    True

customOutputRowDecoder :: Decoders.Row CustomOutputRow
customOutputRowDecoder =
    CustomOutputRow
        <$> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
