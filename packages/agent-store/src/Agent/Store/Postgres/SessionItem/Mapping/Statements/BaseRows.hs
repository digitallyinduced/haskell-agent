module Agent.Store.Postgres.SessionItem.Mapping.Statements.BaseRows
    ( BaseRow (..)
    , loadBaseRowsStatement
    ) where

import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)

import Agent.Store.Postgres.Hasql (mkStatement)

data BaseRow = BaseRow
    { baseRowId :: !Text
    , baseRowStorageKind :: !Text
    , baseRowItemType :: !Text
    , baseRowRepresentation :: !Text
    }

loadBaseRowsStatement :: Statement Text [BaseRow]
loadBaseRowsStatement = mkStatement
    "SELECT response_item_id::text, storage_kind, item_type, representation\
    \ FROM harness.session_response_items\
    \ WHERE turn_id = $1::uuid\
    \ ORDER BY item_index"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowList $
        BaseRow
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text))
    True
