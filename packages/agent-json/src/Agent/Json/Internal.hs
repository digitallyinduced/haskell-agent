module Agent.Json.Internal
    ( RawJson(..)
    , rawJsonEncodingInternal
    ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding.Internal as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.Hermes as Hermes
import qualified Data.Vector as Vector

newtype RawJson = RawJson BS.ByteString
    deriving stock (Eq, Ord)

instance Show RawJson where
    show (RawJson bytes) =
        "RawJson <" <> show (BS.length bytes) <> " bytes>"

instance Aeson.ToJSON RawJson where
    toEncoding = rawJsonEncodingInternal
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

rawJsonEncodingInternal :: RawJson -> Aeson.Encoding
rawJsonEncodingInternal (RawJson bytes) =
    Aeson.unsafeToEncoding (Builder.byteString bytes)

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
