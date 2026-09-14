-- | Time-ordered unique identifiers (RFC 9562 UUID version 7).
--
-- The official Codex clients identify sessions, turns, and context windows
-- with version 7 UUIDs. Generating the same layout keeps our identifiers
-- sortable by creation time and indistinguishable in shape from theirs.
module Agent.Uuid
    ( generateUuidV7
    , uuidV7FromParts
    ) where

import Data.Bits ((.&.), (.|.), shiftR)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64, Word8)
import System.Entropy (getEntropy)

-- | Generate a fresh version 7 UUID from the current wall clock and ten
-- bytes of system entropy.
generateUuidV7 :: IO Text
generateUuidV7 = do
    now <- getPOSIXTime
    random <- getEntropy 10
    pure (uuidV7FromParts (floor (now * 1000)) random)

-- | Render the version 7 layout from a Unix millisecond timestamp and the
-- random tail. Only the first ten random bytes are used; a shorter input is
-- zero padded. Exported so the byte layout can be verified deterministically.
uuidV7FromParts :: Word64 -> BS.ByteString -> Text
uuidV7FromParts unixMillis random =
    let timeBytes =
            [ fromIntegral (unixMillis `shiftR` shift) :: Word8
            | shift <- [40, 32, 24, 16, 8, 0]
            ]
        randomBytes =
            BS.unpack (BS.take 10 (random <> BS.replicate 10 0))
        tailBytes = case randomBytes of
            (randA1 : randA2 : randB1 : rest) ->
                (0x70 .|. (randA1 .&. 0x0f))
                    : randA2
                    : (0x80 .|. (randB1 .&. 0x3f))
                    : rest
            other -> other
        hex =
            Text.decodeUtf8 $
                LBS.toStrict $
                    toLazyByteString $
                        byteStringHex (BS.pack (timeBytes <> tailBytes))
    in Text.intercalate "-"
        [ Text.take 8 hex
        , Text.take 4 (Text.drop 8 hex)
        , Text.take 4 (Text.drop 12 hex)
        , Text.take 4 (Text.drop 16 hex)
        , Text.drop 20 hex
        ]
