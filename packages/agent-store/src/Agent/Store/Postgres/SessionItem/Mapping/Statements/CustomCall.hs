module Agent.Store.Postgres.SessionItem.Mapping.Statements.CustomCall
    ( CustomCallRow (..)
    , loadCustomCallStatement
    , loadCustomCallsStatement
    ) where

import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)

import Agent.Store.Postgres.Hasql (mkStatement)

data CustomCallRow = CustomCallRow
    { customCallRowProviderItemId :: !(Maybe Text)
    , customCallRowCallId :: !Text
    , customCallRowName :: !Text
    , customCallRowInput :: !Text
    , customCallRowStatus :: !(Maybe Text)
    , customCallRowExtraFields :: !Text
    }

loadCustomCallStatement :: Statement Text (Maybe CustomCallRow)
loadCustomCallStatement = mkStatement
    "SELECT provider_item_id, call_id, tool_name, input_text, status_name,\
    \ extra_fields_text\
    \ FROM harness.session_custom_tool_calls\
    \ WHERE response_item_id = $1::uuid"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowMaybe customCallRowDecoder)
    True

loadCustomCallsStatement :: Statement Text [(Text, CustomCallRow)]
loadCustomCallsStatement = mkStatement
    "SELECT child.response_item_id::text, child.provider_item_id,\
    \ child.call_id, child.tool_name, child.input_text, child.status_name,\
    \ child.extra_fields_text\
    \ FROM harness.session_custom_tool_calls child\
    \ WHERE child.response_item_id = ANY (ARRAY(\
    \   SELECT item.response_item_id\
    \   FROM harness.session_response_items item\
    \   WHERE item.turn_id = $1::uuid\
    \ ))"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowList $
        (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> customCallRowDecoder)
    True

customCallRowDecoder :: Decoders.Row CustomCallRow
customCallRowDecoder =
    CustomCallRow
        <$> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
