module Agent.Json.Internal (RawJson(..)) where

import qualified Data.ByteString as BS

newtype RawJson = RawJson BS.ByteString
    deriving stock (Eq, Ord)

instance Show RawJson where
    show (RawJson bytes) =
        "RawJson <" <> show (BS.length bytes) <> " bytes>"
