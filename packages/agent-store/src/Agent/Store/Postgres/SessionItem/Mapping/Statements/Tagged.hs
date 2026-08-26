module Agent.Store.Postgres.SessionItem.Mapping.Statements.Tagged
    ( TaggedRow (..)
    , loadTaggedStatement
    ) where

import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)

import Agent.Store.Postgres.Hasql (mkStatement)

data TaggedRow = TaggedRow
    { taggedRowWireTag :: !Text
    , taggedRowFields :: !Text
    }

loadTaggedStatement :: Statement Text (Maybe TaggedRow)
loadTaggedStatement = mkStatement
    "SELECT wire_tag, fields_text\
    \ FROM harness.session_tagged_items\
    \ WHERE response_item_id = $1::uuid"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowMaybe $
        TaggedRow
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text))
    True
