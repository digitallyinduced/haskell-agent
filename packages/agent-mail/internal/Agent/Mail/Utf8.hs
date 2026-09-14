-- | Internal byte-bounded text operations shared by mail codecs and types.
module Agent.Mail.Utf8 (truncateUtf8, utf8Length) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as TextEncoding

-- | Keep the longest whole-codepoint prefix fitting in the UTF-8 byte limit.
-- Non-positive limits produce empty text. An incomplete final codepoint is
-- dropped, never replaced with a replacement character.
truncateUtf8 :: Int -> Text -> Text
truncateUtf8 limit = decodePrefix . BS.take (max 0 limit) . TextEncoding.encodeUtf8
  where
    decodePrefix bytes =
        case TextEncoding.decodeUtf8' bytes of
            Right value -> value
            Left _
                | BS.null bytes -> ""
                | otherwise -> decodePrefix (BS.init bytes)

utf8Length :: Text -> Int
utf8Length = BS.length . TextEncoding.encodeUtf8
