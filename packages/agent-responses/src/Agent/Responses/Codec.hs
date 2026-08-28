-- | Explicit JSON codecs for the canonical Responses API wire types.
--
-- The types also have ordinary aeson instances.  These functions make the
-- wire boundary obvious at transport call sites and provide the SSE variant
-- where the event discriminator arrives outside the JSON payload.
module Agent.Responses.Codec
    ( encodeResponseCreateParams
    , decodeResponseCreateParams
    , decodeResponse
    , decodeResponseStreamEvent
    , decodeResponseStreamEventWithType
    , withResponseStreamEventDecoder
    ) where

import qualified Agent.Json.Decode as Json
import qualified Data.ByteString as BS
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text

import Agent.Responses.Types

encodeResponseCreateParams :: ResponseCreateParams -> LBS.ByteString
encodeResponseCreateParams = Aeson.encode

decodeResponseCreateParams :: BS.ByteString -> Either String ResponseCreateParams
decodeResponseCreateParams =
    decodeDirect responseCreateParamsDecoder

decodeResponse :: BS.ByteString -> Either String Response
decodeResponse = decodeDirect responseDecoder

decodeResponseStreamEvent :: BS.ByteString -> Either String ResponseStreamEvent
decodeResponseStreamEvent =
    decodeDirect responseStreamEventDecoder

decodeResponseStreamEventWithType
    :: Text
    -> BS.ByteString
    -> Either String ResponseStreamEvent
decodeResponseStreamEventWithType eventType =
    decodeDirect (responseStreamEventDecoderWithType eventType)

withResponseStreamEventDecoder
    :: ((BS.ByteString -> IO (Either String ResponseStreamEvent)) -> IO value)
    -> IO value
withResponseStreamEventDecoder action =
    Json.withDecoderSession \session ->
        action \bytes -> do
            result <- Json.decodeIO session responseStreamEventDecoder bytes
            pure (either (Left . Text.unpack . (.jsonErrorMessage)) Right result)

decodeDirect :: Json.Decoder value -> BS.ByteString -> Either String value
decodeDirect decoder =
    either (Left . Text.unpack . (.jsonErrorMessage)) Right
        . Json.decodeEither decoder
