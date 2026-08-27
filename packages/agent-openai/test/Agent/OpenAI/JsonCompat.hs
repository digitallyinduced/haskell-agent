module Agent.OpenAI.JsonCompat
    ( emptyExtensions
    , rawValue
    , rawText
    , rawBool
    , extensionsFromValueList
    , extensionFromValue
    , lookupExtensionValue
    , decodeRawValue
    , encodedValue
    ) where

import Agent.Json
    ( Extensions
    , RawJson
    , emptyExtensions
    , extensionsFromList
    , extensionsSingleton
    , lookupExtension
    , rawJsonBytes
    )
import qualified Agent.Json.Decoder as JsonDecoder
import qualified Agent.Json.Encoder as JsonEncoder
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import Agent.Responses.Types
import qualified Agent.Responses.Codec as ResponsesCodec

instance Aeson.ToJSON ResponseCreateParams where
    toJSON = encodedValue responseCreateParamsEncoder

instance Aeson.FromJSON ResponseCreateParams where
    parseJSON value =
        either fail pure
            (ResponsesCodec.decodeResponseCreateParams
                (LBS.toStrict (Aeson.encode value)))

instance Aeson.ToJSON Response where
    toJSON = encodedValue responseEncoder

instance Aeson.FromJSON Response where
    parseJSON value =
        either fail pure
            (ResponsesCodec.decodeResponse
                (LBS.toStrict (Aeson.encode value)))

instance Aeson.ToJSON ResponseStreamEvent where
    toJSON = encodedValue responseStreamEventEncoder

instance Aeson.FromJSON ResponseStreamEvent where
    parseJSON value =
        either fail pure
            (ResponsesCodec.decodeResponseStreamEvent
                (LBS.toStrict (Aeson.encode value)))

instance Aeson.ToJSON ResponseItem where
    toJSON = encodedValue responseItemEncoder

instance Aeson.FromJSON ResponseItem where
    parseJSON value =
        case JsonDecoder.decode responseItemDecoder
                (LBS.toStrict (Aeson.encode value)) of
            Right item -> pure item
            Left err -> fail (Text.unpack (JsonDecoder.renderDecodeError err))

rawValue :: Aeson.Value -> RawJson
rawValue value =
    validated (LBS.toStrict (Aeson.encode value))

rawText :: Text -> RawJson
rawText = validated . JsonEncoder.encode JsonEncoder.text

rawBool :: Bool -> RawJson
rawBool = validated . JsonEncoder.encode JsonEncoder.bool

extensionsFromValueList :: [(Key.Key, Aeson.Value)] -> Extensions
extensionsFromValueList =
    extensionsFromList . map \(key, value) ->
        (Key.toText key, rawValue value)

extensionFromValue :: Key.Key -> Aeson.Value -> Extensions
extensionFromValue key =
    extensionsSingleton (Key.toText key) . rawValue

lookupExtensionValue :: Key.Key -> Extensions -> Maybe Aeson.Value
lookupExtensionValue key fields =
    lookupExtension (Key.toText key) fields >>= decodeRawValue

decodeRawValue :: RawJson -> Maybe Aeson.Value
decodeRawValue raw =
    Aeson.decodeStrict' (rawJsonBytes raw)

encodedValue :: JsonEncoder.Encoder value -> value -> Aeson.Value
encodedValue encoder =
    maybe (error "direct encoder produced invalid JSON") id
        . Aeson.decodeStrict'
        . JsonEncoder.encode encoder

validated bytes =
    case JsonDecoder.validateRawJson bytes of
        Right raw -> raw
        Left err -> error ("invalid test JSON: " <> show err)
