{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Text rendering for arbitrary custom-query result rows.
--
-- Hasql decoders require a statically known result shape, while user queries
-- select columns dynamically. PostgreSQL therefore renders each row into one
-- labeled text value that Hasql can decode without exposing JSON at the store
-- boundary.
module Agent.Store.Custom.QueryResult
    ( CustomQueryResult(..)
    , customQueryStatement
    ) where

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)
import qualified Hasql.Statement as Statement

data CustomQueryResult = CustomQueryResult
    { customQueryOutput :: !Text
    , customQueryTruncated :: !Bool
    }
    deriving (Eq, Show)

customQueryStatement :: Int64 -> Text -> Statement () CustomQueryResult
customQueryStatement rowCap query =
    Statement.unpreparable sql Encoders.noParams decoder
  where
    overflowCap
        | rowCap == maxBound = rowCap
        | otherwise = rowCap + 1
    cap = Text.pack (show rowCap)
    capPlusOne = Text.pack (show overflowCap)
    sql =
        "select coalesce(string_agg("
            <> "'row ' || q._ha_row_number::text || E':\n' || "
            <> "coalesce((select string_agg("
            <> "'  ' || field.key || ': ' || "
            <> "replace(coalesce(field.value #>> '{}', 'null'), "
            <> "E'\n', E'\n    '), E'\n' "
            <> "order by field.key) "
            <> "from jsonb_each(q._ha_row) as field), "
            <> "'  (empty row)'), "
            <> "E'\n\n' order by q._ha_row_number) "
            <> "filter (where q._ha_row_number <= " <> cap <> "), "
            <> "'(no rows)'),"
            <> " coalesce(max(q._ha_row_number), 0) > " <> cap
            <> " from ("
            <> "select to_jsonb(_ha_data) as _ha_row, "
            <> "row_number() over () as _ha_row_number "
            <> "from (" <> query <> ") as _ha_data "
            <> "limit " <> capPlusOne
            <> ") as q"
    decoder = Decoders.singleRow $
        CustomQueryResult
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.bool)
