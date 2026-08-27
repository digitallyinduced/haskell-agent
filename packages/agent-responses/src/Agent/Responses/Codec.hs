-- | Explicit JSON codecs for the canonical Responses API wire types.
--
-- The wire codecs are deliberately built on the direct codecs exported by
-- @agent-responses-types@.  In particular, this module does not construct an
-- intermediate Aeson 'Value' tree at either side of the transport boundary.
module Agent.Responses.Codec
    ( encodeResponseCreateParams
    , decodeResponseCreateParams
    , decodeResponse
    , decodeResponseStreamEvent
    , decodeResponseStreamEventWithType
    , withResponseStreamEventDecoder
    ) where

import qualified Agent.Json.Decoder as Decoder
import qualified Agent.Json.Decoder.Hermes as Hermes
import qualified Agent.Json.Encoder as Encoder
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text

import Agent.Responses.Types

encodeResponseCreateParams :: ResponseCreateParams -> BS.ByteString
encodeResponseCreateParams =
    Encoder.encode responseCreateParamsEncoder

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
    decodeDirect (responseStreamEventDecoderWithType (Just eventType))

-- | Scope one reusable Hermes environment around a stream of Responses
-- events. The returned decoder is sequential; callers should create an
-- independent scope per concurrent stream.
withResponseStreamEventDecoder
    :: ((BS.ByteString -> IO (Either String ResponseStreamEvent)) -> IO a)
    -> IO a
withResponseStreamEventDecoder action =
    Hermes.withDecoderSession \session ->
        action \bytes -> do
            decoded <-
                Hermes.decodeIO session responseStreamEventDecoder bytes
            pure $
                either
                    (Left . Text.unpack . Decoder.renderDecodeError)
                    Right
                    decoded

decodeDirect :: Decoder.Decoder value -> BS.ByteString -> Either String value
decodeDirect decoder =
    either (Left . Text.unpack . Decoder.renderDecodeError) Right
        . Decoder.decode decoder
