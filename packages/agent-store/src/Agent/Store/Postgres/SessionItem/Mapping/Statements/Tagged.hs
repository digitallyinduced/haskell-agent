module Agent.Store.Postgres.SessionItem.Mapping.Statements.Tagged
    ( TaggedRow (..)
    , loadTaggedStatement
    , loadTaggedItemsStatement
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
    (Decoders.rowMaybe taggedRowDecoder)
    True

loadTaggedItemsStatement :: Statement Text [(Text, TaggedRow)]
loadTaggedItemsStatement = mkStatement
    "SELECT child.response_item_id::text, child.wire_tag, child.fields_text\
    \ FROM harness.session_tagged_items child\
    \ WHERE child.response_item_id = ANY (ARRAY(\
    \   SELECT item.response_item_id\
    \   FROM harness.session_response_items item\
    \   WHERE item.turn_id = $1::uuid\
    \ ))"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowList $
        (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> taggedRowDecoder)
    True

taggedRowDecoder :: Decoders.Row TaggedRow
taggedRowDecoder =
    TaggedRow
        <$> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
