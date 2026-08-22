-- | Small HTTP header parsers shared by provider clients.
module Agent.Http.Header
    ( parseRetryAfterSeconds
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8

-- | Parse the first integer @Retry-After@ header value.
--
-- HTTP-date values and malformed integers are left unhandled. A zero or
-- negative delay is clamped to one second so retry loops always yield.
parseRetryAfterSeconds :: [ByteString] -> Maybe Int
parseRetryAfterSeconds = \case
    value : _ -> case reads (BS8.unpack value) of
        [(seconds, "")] -> Just (max 1 seconds)
        _ -> Nothing
    [] -> Nothing
