module Agent.Store.Postgres.SessionItem.Mapping.Statements.FunctionOutput
    ( FunctionOutputRow (..)
    , loadFunctionOutputStatement
    ) where

import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)

import Agent.Store.Postgres.Hasql (mkStatement)

data FunctionOutputRow = FunctionOutputRow
    { functionOutputRowProviderItemId :: !(Maybe Text)
    , functionOutputRowCallId :: !Text
    , functionOutputRowKind :: !Text
    , functionOutputRowText :: !Text
    , functionOutputRowStatus :: !(Maybe Text)
    , functionOutputRowExtraFields :: !Text
    }

loadFunctionOutputStatement :: Statement Text (Maybe FunctionOutputRow)
loadFunctionOutputStatement = mkStatement
    "SELECT provider_item_id, call_id, output_kind, output_text, status_name,\
    \ extra_fields_text\
    \ FROM harness.session_function_call_outputs\
    \ WHERE response_item_id = $1::uuid"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowMaybe $
        FunctionOutputRow
            <$> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text))
    True
