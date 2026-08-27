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

import Agent.Json.Internal (RawJson(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding.Internal as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Hermes as Hermes
import qualified Data.Vector as Vector
rawJsonBytes :: RawJson -> BS.ByteString
rawJsonBytes (RawJson bytes) = bytes

-- Aeson is the repository's encoder. Its 'ToJSON' compatibility API requires
-- a 'Value' even when the hot path uses 'toEncoding', so materialise that
-- value with Hermes rather than an Aeson decoder.
instance Aeson.ToJSON RawJson where
    toEncoding = rawJsonEncoding
    toJSON (RawJson bytes) =
        either
            (error . ("validated RawJson failed to materialise: " <>) . show)
            id
            (Hermes.decodeEither decoder document)
      where
        (decoder, document)
            | startsWithContainer bytes =
                (rawValueDecoder, bytes)
            | otherwise =
                ( Hermes.object
                    (Hermes.atKey "value" rawValueDecoder)
                , "{\"value\":" <> bytes <> "}"
                )

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

rawValueDecoder :: Hermes.Decoder Aeson.Value
rawValueDecoder =
    Hermes.getType >>= \case
        Hermes.VArray ->
            Aeson.Array . Vector.fromList
                <$> Hermes.list rawValueDecoder
        Hermes.VObject ->
            Aeson.Object . KeyMap.fromList
                <$> Hermes.objectAsKeyValues
                    (pure . Key.fromText)
                    rawValueDecoder
        Hermes.VNumber -> Aeson.Number <$> Hermes.scientific
        Hermes.VString -> Aeson.String <$> Hermes.text
        Hermes.VBoolean -> Aeson.Bool <$> Hermes.bool
        Hermes.VNull -> Aeson.Null <$ Hermes.nullable (pure ())

startsWithContainer :: BS.ByteString -> Bool
startsWithContainer bytes =
    case BS.find (not . isWhitespace) bytes of
        Just byte -> byte == 0x7b || byte == 0x5b
        Nothing -> False
  where
    isWhitespace byte =
        byte == 0x20 || byte == 0x09 || byte == 0x0a || byte == 0x0d

