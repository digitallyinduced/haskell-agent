-- | Hermes/simdjson decoder-session boundary.
module Agent.Json.Decoder.Hermes
    ( DecoderSession
    , withDecoderSession
    , decodeIO
    ) where

import qualified Agent.Json.Decoder as DecoderAPI
import Agent.Json.Decoder.Backend
import qualified Data.ByteString as BS
import qualified Data.Hermes as Hermes
import qualified Data.Text as Text
import Data.Word (Word8)
import Control.Exception.Safe (tryAny)
import Control.Monad (foldM)

newtype DecoderSession =
    DecoderSession Hermes.HermesEnv

withDecoderSession :: (DecoderSession -> IO a) -> IO a
withDecoderSession action =
    Hermes.withHermesEnv_ (action . DecoderSession)

decodeIO
    :: DecoderSession
    -> Decoder a
    -> BS.ByteString
    -> IO (Either DecoderAPI.DecodeError a)
decodeIO (DecoderSession environment) decoder bytes =
    case decoderRequiresRawCapture decoder of
        True ->
            pure (DecoderAPI.decode decoder bytes)
        False -> case firstNonWhitespace bytes of
            Just byte
                | byte /= openBrace && byte /= openBracket ->
                    pure (DecoderAPI.decode decoder bytes)
            _ -> do
                result <- tryAny $
                    Hermes.parseByteStringIO
                        environment
                        (toHermes decoder)
                        bytes
                pure $ case result of
                    Left _ ->
                        DecoderAPI.decode decoder bytes
                    Right value -> Right value

decoderRequiresRawCapture :: Decoder a -> Bool
decoderRequiresRawCapture = \case
    NullDecoder _ -> False
    BoolDecoder -> False
    TextDecoder -> False
    ScientificDecoder -> False
    ArrayDecoder elementDecoder ->
        decoderRequiresRawCapture elementDecoder
    ObjectDecoder _ fields unknown _ ->
        any namedFieldRequiresRawCapture fields
            || unknownFieldRequiresRawCapture unknown
    NullableDecoder inner ->
        decoderRequiresRawCapture inner
    ByTypeDecoder select ->
        any (decoderRequiresRawCapture . select)
            [ JsonNull
            , JsonBoolean
            , JsonNumber
            , JsonString
            , JsonArray
            , JsonObject
            ]
    RawJsonDecoder -> True
    SkipDecoder -> False
    MapDecoder _ inner ->
        decoderRequiresRawCapture inner

namedFieldRequiresRawCapture :: NamedField state -> Bool
namedFieldRequiresRawCapture (NamedField _ decoder _) =
    decoderRequiresRawCapture decoder

unknownFieldRequiresRawCapture :: UnknownField state -> Bool
unknownFieldRequiresRawCapture (UnknownField decoder _) =
    decoderRequiresRawCapture decoder

toHermes :: Decoder a -> Hermes.Decoder a
toHermes = \case
    NullDecoder value -> do
        isNull <- Hermes.isNull
        if isNull then pure value else fail "expected null"
    BoolDecoder -> Hermes.bool
    TextDecoder -> Hermes.text
    ScientificDecoder -> Hermes.scientific
    ArrayDecoder elementDecoder ->
        Hermes.list (toHermes elementDecoder)
    ObjectDecoder initialState fields unknown finish -> do
        updates <-
            Hermes.objectAsKeyValues
                (\key -> hermesObjectField key fields unknown)
                (pure ())
        state <-
            foldM
                (\current (update, ()) ->
                    either (fail . Text.unpack) pure (update current))
                initialState
                updates
        either (fail . Text.unpack) pure (finish state)
    NullableDecoder inner ->
        Hermes.nullable (toHermes inner)
    ByTypeDecoder select -> do
        valueType <- hermesJsonType
        toHermes (select valueType)
    RawJsonDecoder ->
        fail "RawJson requires the portable direct backend"
    SkipDecoder ->
        validateValue
    MapDecoder transform inner -> do
        value <- toHermes inner
        either (fail . Text.unpack) pure (transform value)

hermesObjectField
    :: Text.Text
    -> [NamedField state]
    -> UnknownField state
    -> Hermes.Decoder (state -> Either Text.Text state)
hermesObjectField key fields unknown =
    case fields of
        [] -> case unknown of
            UnknownField decoder update ->
                update key <$> toHermes decoder
        NamedField name decoder update : rest
            | name == key ->
                update <$> toHermes decoder
            | otherwise ->
                hermesObjectField key rest unknown

validateValue :: Hermes.Decoder ()
validateValue =
    Hermes.getType >>= \case
        Hermes.VArray ->
            () <$ Hermes.list validateValue
        Hermes.VObject ->
            () <$ Hermes.objectAsKeyValues
                (const validateValue)
                (pure ())
        Hermes.VNumber ->
            () <$ Hermes.scientific
        Hermes.VString ->
            () <$ Hermes.text
        Hermes.VBoolean ->
            () <$ Hermes.bool
        Hermes.VNull -> do
            isNull <- Hermes.isNull
            if isNull then pure () else fail "expected null"

hermesJsonType :: Hermes.Decoder JsonType
hermesJsonType =
    Hermes.getType >>= \case
        Hermes.VArray -> pure JsonArray
        Hermes.VObject -> pure JsonObject
        Hermes.VNumber -> pure JsonNumber
        Hermes.VString -> pure JsonString
        Hermes.VBoolean -> pure JsonBoolean
        Hermes.VNull -> pure JsonNull

firstNonWhitespace :: BS.ByteString -> Maybe Word8
firstNonWhitespace =
    BS.find (not . isWhitespace)

isWhitespace :: Word8 -> Bool
isWhitespace byte =
    byte == 0x20 || byte == 0x09 || byte == 0x0a || byte == 0x0d

openBrace, openBracket :: Word8
openBrace = 0x7b
openBracket = 0x5b
