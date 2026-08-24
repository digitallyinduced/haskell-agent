module Agent.Store.Postgres.Sql
    ( quoteIdentifier
    , quoteLiteral
    ) where

import Data.Text (Text)
import qualified Data.Text as Text

quoteIdentifier :: Text -> Text
quoteIdentifier value =
    "\"" <> Text.replace "\"" "\"\"" value <> "\""

quoteLiteral :: Text -> Text
quoteLiteral value =
    "'" <> Text.replace "'" "''" value <> "'"
