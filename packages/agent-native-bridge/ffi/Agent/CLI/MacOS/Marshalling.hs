-- | Callback-scoped UTF-8 buffers shared by native endpoint adapters.
module Agent.CLI.MacOS.Marshalling
    ( decodeInput
    , decodeUtf8Input
    , nonEmptyText
    , withText
    , withOptionalText
    , withNullableText
    , anyNonEmptyNull
    ) where

import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word8, Word64)
import Foreign (Ptr, castPtr, nullPtr)
import Foreign.C.String (CString)
import Foreign.C.Types (CSize)

decodeInput :: Ptr Word8 -> Word64 -> IO Text
decodeInput pointer length
    | pointer == nullPtr || length == 0 = pure ""
    | otherwise = TextEncoding.decodeUtf8 <$> BS.packCStringLen
        (castPtr pointer, fromIntegral length)

decodeUtf8Input :: Ptr Word8 -> Word64 -> IO (Either () Text)
decodeUtf8Input pointer length =
    first (const ()) . TextEncoding.decodeUtf8'
        <$> BS.packCStringLen (castPtr pointer, fromIntegral length)

nonEmptyText :: Text -> Maybe Text
nonEmptyText value
    | Text.null value = Nothing
    | otherwise = Just value

withText :: Text -> (CString -> CSize -> IO a) -> IO a
withText value action = BS.useAsCStringLen (TextEncoding.encodeUtf8 value) \(pointer, length) ->
    action pointer (fromIntegral length)

withOptionalText :: Maybe Text -> (CString -> CSize -> IO a) -> IO a
withOptionalText value action = withText (fromMaybe "" value) action

withNullableText :: Maybe Text -> (CString -> CSize -> IO a) -> IO a
withNullableText value action = case value of
    Nothing -> action nullPtr 0
    Just text -> withText text action

anyNonEmptyNull :: [(Ptr Word8, Word64)] -> Bool
anyNonEmptyNull = any \(pointer, length) ->
    pointer == nullPtr && length > 0
