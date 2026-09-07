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

import Agent.Responses.Types

encodeResponseCreateParams :: ResponseCreateParams -> LBS.ByteString
encodeResponseCreateParams = Aeson.encode

decodeResponseCreateParams :: BS.ByteString -> Either Text ResponseCreateParams
decodeResponseCreateParams =
    decodeDirect responseCreateParamsDecoder

decodeResponse :: BS.ByteString -> Either Text Response
decodeResponse = decodeDirect responseDecoder

decodeResponseStreamEvent :: BS.ByteString -> Either Text ResponseStreamEvent
decodeResponseStreamEvent =
    decodeDirect responseStreamEventDecoder

decodeResponseStreamEventWithType
    :: Text
    -> BS.ByteString
    -> Either Text ResponseStreamEvent
decodeResponseStreamEventWithType eventType =
    decodeDirect (responseStreamEventDecoderWithType eventType)

withResponseStreamEventDecoder
    :: ((BS.ByteString -> IO (Either Text ResponseStreamEvent)) -> IO value)
    -> IO value
withResponseStreamEventDecoder action =
    Json.withDecoderSession \session ->
        action \bytes -> do
            result <- Json.decodeIO session responseStreamEventDecoder bytes
            pure (either (Left . (.jsonErrorMessage)) Right result)

decodeDirect :: Json.Decoder value -> BS.ByteString -> Either Text value
decodeDirect decoder =
    either (Left . (.jsonErrorMessage)) Right
        . Json.decodeEither decoder
