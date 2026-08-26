module Agent.Store.Postgres.SessionItem.Mapping.Statements.Summaries
    ( SummaryRow (..)
    , loadSummariesStatement
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
    (Decoders.rowList $
        SummaryRow
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text))
    True
