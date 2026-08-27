-- | Owned JSON bytes for protocol fields which are deliberately opaque.
--
-- The constructor is private: external bytes must first pass complete Hermes
-- validation. Aeson can emit the bytes directly through 'rawJsonEncoding'
-- without decoding them into a 'Data.Aeson.Value'.
module Agent.Json
    ( RawJson
    , rawJsonBytes
    , rawJsonEncoding
    , rawJsonFromEncoding
    , rawJsonDecoder
    ) where

import qualified Data.Aeson.Encoding.Internal as Aeson
import Agent.Json.Internal (RawJson(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Hermes as Hermes

rawJsonBytes :: RawJson -> BS.ByteString
rawJsonBytes (RawJson bytes) = bytes

rawJsonEncoding :: RawJson -> Aeson.Encoding
rawJsonEncoding (RawJson bytes) =
    Aeson.unsafeToEncoding (Builder.byteString bytes)

rawJsonFromEncoding :: Aeson.Encoding -> RawJson
rawJsonFromEncoding =
    RawJson . LBS.toStrict . Aeson.encodingToLazyByteString

-- | Validate and retain the complete current JSON value.
rawJsonDecoder :: Hermes.Decoder RawJson
rawJsonDecoder =
    Hermes.withRawJsonByteString \bytes -> do
        let owned = BS.copy bytes
        owned `seq` pure (RawJson owned)
