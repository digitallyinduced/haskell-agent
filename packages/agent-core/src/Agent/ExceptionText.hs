-- | Bounded, single-line exception details for user-facing diagnostics.
module Agent.ExceptionText (formatException) where

import Control.Exception.Safe (Exception, displayException)
import Data.Char (isControl)
import Data.Text (Text)
import qualified Data.Text as Text

formatException :: Exception exception => exception -> Text
formatException exception =
    if Text.length cleaned <= 240
        then cleaned
        else Text.take 239 cleaned <> "…"
  where
    cleaned =
        Text.unwords
            . Text.words
            . Text.map (\character -> if isControl character then ' ' else character)
            . Text.strip
            . Text.pack
            $ displayException exception
