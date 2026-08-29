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
    , withOwnedRawJson
    ) where

import Agent.Json (RawJson)
import Agent.Json.Internal (RawJson(..))
import Control.Exception.Safe (tryAny)
import Control.Monad (join)
import qualified Data.ByteString as BS
import Data.Hermes as Hermes hiding
    ( decodeEither
    , withRawByteString
    , withRawJsonByteString
    )
import qualified Data.Hermes as HermesRaw (withRawJsonByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word8)
import System.IO.Unsafe (unsafePerformIO)

newtype JsonError = JsonError { jsonErrorMessage :: Text }
    deriving stock (Eq, Show)

newtype DecoderSession =
    DecoderSession Hermes.HermesEnv

decodeEither :: Hermes.Decoder a -> BS.ByteString -> Either JsonError a
decodeEither decoder bytes =
    unsafePerformIO $
        withDecoderSession \session ->
            decodeIO session decoder bytes
{-# NOINLINE decodeEither #-}

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
validateRawJson bytes = do
    () <- decodeEither validateValue bytes
    let owned = BS.copy bytes
    owned `seq` pure (RawJson owned)

-- | Run a decoder continuation with an owned copy of the current value's
-- complete JSON bytes.
--
-- Hermes hands out a zero-copy view into simdjson's padded input buffer, and
-- that buffer is released as soon as the enclosing parse returns. A result
-- that captures the view lazily and is forced later therefore reads freed
-- memory. Copying before the continuation runs keeps every downstream use
-- valid, including re-decoding the bytes with 'decodeEither'. The aliasing
-- Hermes combinators are deliberately not re-exported from this module.
withOwnedRawJson
    :: (BS.ByteString -> Hermes.Decoder a)
    -> Hermes.Decoder a
withOwnedRawJson continuation =
    HermesRaw.withRawJsonByteString \view ->
        let owned = BS.copy view
        in owned `seq` continuation owned

validateValue :: Hermes.Decoder ()
validateValue =
    Hermes.getType >>= \case
        Hermes.VArray ->
            () <$ Hermes.list validateValue
        Hermes.VObject ->
            () <$ Hermes.objectFold ()
                (\_ () -> validateValue)
        Hermes.VNumber ->
            () <$ Hermes.scientific
        Hermes.VString ->
            () <$ Hermes.text
        Hermes.VBoolean ->
            () <$ Hermes.bool
        Hermes.VNull -> do
            nil <- Hermes.isNull
            if nil then pure () else fail "expected null"

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
