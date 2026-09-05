-- | Bounded, owned UTF-8 input shared by repository bridge endpoints.
module Agent.CLI.MacOS.RepositoryInput
    ( copyRequiredText
    , copyRequiredTexts
    ) where

import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word8)
import Foreign (Ptr, castPtr, nullPtr)
import Foreign.C.Types (CSize(..))

copyRequiredText :: Ptr Word8 -> CSize -> IO (Either () Text)
copyRequiredText pointer (CSize length)
    | pointer == nullPtr || length == 0 = pure (Left ())
    | toInteger length > toInteger (maxBound :: Int) = pure (Left ())
    | toInteger length > toInteger maxRepositoryRequestItemBytes =
        pure (Left ())
    | otherwise = do
        bytes <- BS.packCStringLen (castPtr pointer, fromIntegral length)
        pure (first (const ()) (TextEncoding.decodeUtf8' bytes))

copyRequiredTexts
    :: [(Ptr Word8, CSize)]
    -> IO (Either () [Text])
copyRequiredTexts fields
    | sum (map (toInteger . snd) fields)
        > toInteger maxRepositoryRequestTotalBytes = pure (Left ())
    | otherwise =
        fmap sequence (mapM (uncurry copyRequiredText) fields)

maxRepositoryRequestItemBytes :: Int
maxRepositoryRequestItemBytes = 8 * 1024 * 1024

maxRepositoryRequestTotalBytes :: Int
maxRepositoryRequestTotalBytes = 16 * 1024 * 1024
