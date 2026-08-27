-- | The repository's concrete JSON decoder boundary.
--
-- Decoders are native Hermes decoders. There is no portable decoder or custom
-- decoder syntax tree. Use one 'DecoderSession' per sequential stream and an
-- independent session for each concurrent stream.
module Agent.Json.Decode
    ( module Hermes
    , JsonError(..)
    , DecoderSession
    , decodeEither
    , decodeText
    , withDecoderSession
    , decodeIO
    , decodeTextIO
    , optionalKey
    , defaultKey
    , discriminatedObject
    , validateRawJson
    ) where

import Agent.Json (RawJson, rawJsonDecoder)
import Control.Exception.Safe (tryAny)
import Control.Monad (join)
import qualified Data.ByteString as BS
import Data.Hermes as Hermes hiding (decodeEither)
import qualified Data.Hermes as Hermes
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word8)

newtype JsonError = JsonError { jsonErrorMessage :: Text }
    deriving stock (Eq, Show)

newtype DecoderSession =
    DecoderSession Hermes.HermesEnv

decodeEither :: Hermes.Decoder a -> BS.ByteString -> Either JsonError a
decodeEither decoder bytes =
    firstJsonError $
        Hermes.decodeEither
            (documentDecoder decoder bytes)
            (documentBytes bytes)

decodeText :: Hermes.Decoder a -> Text -> Either JsonError a
decodeText decoder =
    decodeEither decoder . Text.encodeUtf8

withDecoderSession :: (DecoderSession -> IO a) -> IO a
withDecoderSession action =
    Hermes.withHermesEnv_ (action . DecoderSession)

decodeIO
    :: DecoderSession
    -> Hermes.Decoder a
    -> BS.ByteString
    -> IO (Either JsonError a)
decodeIO (DecoderSession environment) decoder bytes = do
    result <- tryAny $
        Hermes.parseByteStringIO
            environment
            (documentDecoder decoder bytes)
            (documentBytes bytes)
    pure $ case result of
        Left err -> Left (JsonError (Text.pack (show err)))
        Right value -> Right value

decodeTextIO
    :: DecoderSession
    -> Hermes.Decoder a
    -> Text
    -> IO (Either JsonError a)
decodeTextIO session decoder =
    decodeIO session decoder . Text.encodeUtf8

-- | Decode an object field where both a missing key and an explicit null mean
-- 'Nothing'.
optionalKey
    :: Text
    -> Hermes.Decoder a
    -> Hermes.FieldsDecoder (Maybe a)
optionalKey key decoder =
    join <$> Hermes.atKeyOptional key (Hermes.nullable decoder)

-- | Decode an object field with the same default for missing and null.
defaultKey
    :: a
    -> Text
    -> Hermes.Decoder a
    -> Hermes.FieldsDecoder a
defaultKey fallback key decoder =
    maybe fallback id <$> optionalKey key decoder

-- | Select and decode an object shape by a textual discriminator without
-- materialising a generic object.
discriminatedObject
    :: Text
    -> (Text -> Hermes.Decoder a)
    -> Hermes.Decoder a
discriminatedObject discriminator select =
    Hermes.object do
        tag <- Hermes.atKey discriminator Hermes.text
        Hermes.liftObjectDecoder (select tag)

validateRawJson :: BS.ByteString -> Either JsonError RawJson
validateRawJson =
    decodeEither rawJsonDecoder

firstJsonError :: Show error => Either error a -> Either JsonError a
firstJsonError =
    either (Left . JsonError . Text.pack . show) Right

documentDecoder
    :: Hermes.Decoder a
    -> BS.ByteString
    -> Hermes.Decoder a
documentDecoder decoder bytes
    | isContainerDocument bytes = decoder
    | otherwise =
        Hermes.object do
            value <- Hermes.atKey "value" decoder
            end <- Hermes.atKey "end" Hermes.bool
            if end then pure value else fail "invalid scalar wrapper"

documentBytes :: BS.ByteString -> BS.ByteString
documentBytes bytes
    | isContainerDocument bytes = bytes
    | otherwise = "{\"value\":" <> bytes <> ",\"end\":true}"

isContainerDocument :: BS.ByteString -> Bool
isContainerDocument bytes =
    case BS.find (not . isWhitespace) bytes of
        Just byte -> byte == openBrace || byte == openBracket
        Nothing -> False

isWhitespace :: Word8 -> Bool
isWhitespace byte =
    byte == 0x20 || byte == 0x09 || byte == 0x0a || byte == 0x0d

openBrace, openBracket :: Word8
openBrace = 0x7b
openBracket = 0x5b
