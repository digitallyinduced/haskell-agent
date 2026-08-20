-- | Extract a string field from a JSON object payload (tool arguments).
module Agent.JsonText
    ( jsonTextField
    , jsonTextFieldDefault
    ) where

import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text.Encoding as TextEncoding

-- | Read a string field from a JSON object encoded as 'Text'.
jsonTextField :: Text -> Text -> Maybe Text
jsonTextField key arguments = case Aeson.decodeStrict (TextEncoding.encodeUtf8 arguments) of
    Just (Object object) -> case KeyMap.lookup (Key.fromText key) object of
        Just (String value) -> Just value
        _ -> Nothing
    _ -> Nothing

-- | Like 'jsonTextField', returning empty text when missing or not a string.
jsonTextFieldDefault :: Text -> Text -> Text
jsonTextFieldDefault key arguments =
    maybe "" id (jsonTextField key arguments)
