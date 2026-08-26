module Agent.Store.Postgres.SessionItem.Mapping.Statements.FunctionCall
    ( FunctionCallRow (..)
    , loadFunctionCallStatement
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
    (Decoders.rowMaybe $
        FunctionCallRow
            <$> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text))
    True
