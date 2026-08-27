-- | Hermes/simdjson decoder-session boundary.
module Agent.Json.Decoder.Hermes
    ( DecoderSession
    , withDecoderSession
    , decodeIO
    , decodeHermesIO
    ) where

import qualified Agent.Json.Decoder as DecoderAPI
import Agent.Json.Decoder.Backend
import Agent.Json (RawJson)
import qualified Data.ByteString as BS
import qualified Data.Hermes as Hermes
import qualified Data.Text as Text
import Data.Word (Word8)
import Control.Exception.Safe (tryAny)

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
    case firstNonWhitespace bytes of
        Just byte
            | byte /= openBrace && byte /= openBracket ->
                pure (DecoderAPI.decode decoder bytes)
        _ -> do
            result <- decodeHermesIO
                (DecoderSession environment)
                (toHermes decoder)
                bytes
            pure $ case result of
                Left _ ->
                    DecoderAPI.decode decoder bytes
                right -> right

decodeHermesIO
    :: DecoderSession
    -> Hermes.Decoder a
    -> BS.ByteString
    -> IO (Either DecoderAPI.DecodeError a)
decodeHermesIO (DecoderSession environment) decoder bytes = do
    result <- tryAny $
        Hermes.parseByteStringIO environment decoder bytes
    pure $ case result of
        Left err ->
            Left (DecoderAPI.DecodeError
                0
                []
                (Text.pack (show err)))
        Right value -> Right value

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
        state <- Hermes.objectFold initialState
            \key current ->
                hermesObjectField key current fields unknown
        either (fail . Text.unpack) pure (finish state)
    PlannedObjectDecoder initialPlan -> do
        plan <- Hermes.objectFold initialPlan \key current ->
            case matchPlannedField key current of
                Just (PlannedFieldMatch decoder rebuild) ->
                    do
                        isNull <- Hermes.isNull
                        value <- toHermes decoder
                        let rebuilt =
                                markPlannedFieldPresent
                                    key
                                    (rebuild value)
                        pure $
                            if isNull
                                && objectPlanCapturesExtensions rebuilt
                                then capturePlannedExtension
                                    key
                                    (unsafeRawJsonFromValidatedBytes
                                        "null")
                                    rebuilt
                                else deletePlannedExtension key rebuilt
                Nothing
                    | objectPlanCapturesExtensions current ->
                        (\raw ->
                            capturePlannedExtension key raw current)
                            <$> toHermes RawJsonDecoder
                    | otherwise ->
                        current <$ validateValue
        either (fail . Text.unpack) pure (finishObjectPlan plan)
    DiscriminatedObjectDecoder discriminator select ->
        Hermes.object do
            -- Select the branch, reset the On Demand object, then decode the
            -- selected shape with Hermes. Extracting raw JSON here would
            -- consume and reparse the complete object.
            tag <- Hermes.atKey discriminator Hermes.text
            Hermes.liftObjectDecoder (toHermes (select tag))
    NullableDecoder inner ->
        Hermes.nullable (toHermes inner)
    ByTypeDecoder select -> do
        valueType <- hermesJsonType
        toHermes (select valueType)
    RawJsonDecoder ->
        rawJsonValue
    SkipDecoder ->
        validateValue
    MapDecoder transform inner -> do
        value <- toHermes inner
        either (fail . Text.unpack) pure (transform value)
    WithRawDecoder inner ->
        hermesWithRaw inner

hermesObjectField
    :: Text.Text
    -> state
    -> [NamedField state]
    -> UnknownField state
    -> Hermes.Decoder state
hermesObjectField key current fields unknown =
    case fields of
        [] -> case unknown of
            UnknownField decoder update -> do
                value <- toHermes decoder
                either (fail . Text.unpack) pure
                    (update key value current)
        NamedField name decoder update : rest
            | name == key -> do
                value <- toHermes decoder
                either (fail . Text.unpack) pure
                    (update value current)
            | otherwise ->
                hermesObjectField key current rest unknown

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

rawJsonValue :: Hermes.Decoder RawJson
rawJsonValue =
    Hermes.withRawJsonByteString copyRawJson

hermesWithRaw :: Decoder a -> Hermes.Decoder (a, RawJson)
hermesWithRaw inner =
    Hermes.getType >>= \case
        -- raw_json() consumes an On Demand value. Objects can be reset and
        -- decoded again inside the same simdjson document, avoiding a copy
        -- followed by a second portable parse.
        Hermes.VObject ->
            Hermes.withRawJsonByteString \bytes -> do
                value <- Hermes.object $
                    Hermes.liftObjectDecoder (toHermes inner)
                raw <- copyRawJson bytes
                pure (value, raw)
        -- Hermes currently exposes object reset but not the equivalent array
        -- reset primitive. Keep the compatibility fallback for other shapes,
        -- sharing one owned copy between decoding and raw retention.
        _ ->
            Hermes.withRawJsonByteString \bytes -> do
                let owned = BS.copy bytes
                owned `seq`
                    case DecoderAPI.decode inner owned of
                        Left err ->
                            fail
                                (Text.unpack
                                    (DecoderAPI.renderDecodeError err))
                        Right value ->
                            pure
                                ( value
                                , unsafeRawJsonFromValidatedBytes owned
                                )

copyRawJson :: BS.ByteString -> Hermes.Decoder RawJson
copyRawJson bytes =
    let owned = BS.copy bytes
    in owned `seq`
        pure (unsafeRawJsonFromValidatedBytes owned)
