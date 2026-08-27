module Agent.Store.Postgres.SessionItem.Mapping.Statements.FunctionCall
    ( FunctionCallRow (..)
    , loadFunctionCallStatement
    , loadFunctionCallsStatement
    ) where

import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)

import Agent.Store.Postgres.Hasql (mkStatement)

data FunctionCallRow = FunctionCallRow
    { functionCallRowProviderItemId :: !(Maybe Text)
    , functionCallRowCallId :: !Text
    , functionCallRowName :: !Text
    , functionCallRowArguments :: !Text
    , functionCallRowStatus :: !(Maybe Text)
    , functionCallRowExtraFields :: !Text
    }

loadFunctionCallStatement :: Statement Text (Maybe FunctionCallRow)
loadFunctionCallStatement = mkStatement
    "SELECT provider_item_id, call_id, function_name, arguments, status_name,\
    \ extra_fields_text\
    \ FROM harness.session_function_calls\
    \ WHERE response_item_id = $1::uuid"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowMaybe functionCallRowDecoder)
    True

loadFunctionCallsStatement :: Statement Text [(Text, FunctionCallRow)]
loadFunctionCallsStatement = mkStatement
    "SELECT child.response_item_id::text, child.provider_item_id,\
    \ child.call_id, child.function_name, child.arguments,\
    \ child.status_name, child.extra_fields_text\
    \ FROM harness.session_function_calls child\
    \ WHERE child.response_item_id = ANY (ARRAY(\
    \   SELECT item.response_item_id\
    \   FROM harness.session_response_items item\
    \   WHERE item.turn_id = $1::uuid\
    \ ))"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowList $
        (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> functionCallRowDecoder)
    True

functionCallRowDecoder :: Decoders.Row FunctionCallRow
functionCallRowDecoder =
    FunctionCallRow
        <$> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
