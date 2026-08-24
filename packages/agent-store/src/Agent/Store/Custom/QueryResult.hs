{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Boundary encoding for arbitrary custom-query result rows.
--
-- Unlike harness-owned records, the columns returned by a user query are not
-- known when the Haskell statement is compiled. This adapter turns that
-- dynamic row shape into one opaque result payload for the CLI boundary.
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
    { customQueryRows :: !Text
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
        "select coalesce(jsonb_agg("
            <> "q._ha_row order by q._ha_row_number"
            <> ") filter (where q._ha_row_number <= " <> cap
            <> "), '[]'::jsonb)::text,"
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
