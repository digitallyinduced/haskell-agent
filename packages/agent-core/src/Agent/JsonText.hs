-- | Extract a string field from a JSON object payload (tool arguments).
module Agent.JsonText
    ( jsonTextField
    , jsonTextFieldDefault
    ) where

import qualified Agent.Json.Decode as Json
import Data.Text (Text)

-- | Read a string field from a JSON object encoded as 'Text'.
jsonTextField :: Text -> Text -> Maybe Text
jsonTextField key arguments =
    either (const Nothing) Just $
        Json.decodeText (Json.object (Json.atKey key Json.text)) arguments

-- | Like 'jsonTextField', returning empty text when missing or not a string.
jsonTextFieldDefault :: Text -> Text -> Text
jsonTextFieldDefault key arguments =
    maybe "" id (jsonTextField key arguments)
