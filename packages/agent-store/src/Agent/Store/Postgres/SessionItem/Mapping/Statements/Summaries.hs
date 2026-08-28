module Agent.Store.Postgres.SessionItem.Mapping.Statements.Summaries
    ( SummaryRow (..)
    , loadSummariesStatement
    , loadTurnSummariesStatement
    ) where

import Data.Text (Text)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)

import Agent.Store.Postgres.Hasql (mkStatement)

data SummaryRow = SummaryRow
    { summaryRowType :: !Text
    , summaryRowText :: !(Maybe Text)
    , summaryRowExtraFields :: !Text
    }

loadSummariesStatement :: Statement Text [SummaryRow]
loadSummariesStatement = mkStatement
    "SELECT part_type, text_value, extra_fields_text\
    \ FROM harness.session_reasoning_summaries\
    \ WHERE response_item_id = $1::uuid\
    \ ORDER BY part_index"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowList summaryRowDecoder)
    True

loadTurnSummariesStatement :: Statement Text [(Text, SummaryRow)]
loadTurnSummariesStatement = mkStatement
    "SELECT child.response_item_id::text, child.part_type,\
    \ child.text_value, child.extra_fields_text\
    \ FROM harness.session_reasoning_summaries child\
    \ WHERE child.response_item_id = ANY (ARRAY(\
    \   SELECT item.response_item_id\
    \   FROM harness.session_response_items item\
    \   WHERE item.turn_id = $1::uuid\
    \ ))\
    \ ORDER BY child.response_item_id ASC, child.part_index ASC"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowList $
        (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> summaryRowDecoder)
    True

summaryRowDecoder :: Decoders.Row SummaryRow
summaryRowDecoder =
    SummaryRow
        <$> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
