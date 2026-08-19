-- | Explicit JSON codecs for the canonical Responses API wire types.
--
-- The types also have ordinary aeson instances.  These functions make the
-- wire boundary obvious at transport call sites and provide the SSE variant
-- where the event discriminator arrives outside the JSON payload.
module Agent.OpenAI.Responses.Codec
    ( encodeResponseCreateParams
    , encodeResponseCreateParamsValue
    , decodeResponseCreateParams
    , decodeResponse
    , decodeResponseValue
    , decodeResponseStreamEvent
    , decodeResponseStreamEventValue
    , decodeResponseStreamEventWithType
    ) where

import Data.Aeson (Result, Value)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)

import Agent.OpenAI.Responses.Types

encodeResponseCreateParams :: ResponseCreateParams -> LBS.ByteString
encodeResponseCreateParams = Aeson.encode

encodeResponseCreateParamsValue :: ResponseCreateParams -> Value
encodeResponseCreateParamsValue = Aeson.toJSON

decodeResponseCreateParams :: LBS.ByteString -> Either String ResponseCreateParams
decodeResponseCreateParams = Aeson.eitherDecode'

decodeResponse :: LBS.ByteString -> Either String Response
decodeResponse = Aeson.eitherDecode'

decodeResponseValue :: Value -> Result Response
decodeResponseValue = Aeson.fromJSON

decodeResponseStreamEvent :: LBS.ByteString -> Either String ResponseStreamEvent
decodeResponseStreamEvent = Aeson.eitherDecode'

decodeResponseStreamEventValue :: Value -> Result ResponseStreamEvent
decodeResponseStreamEventValue = Aeson.fromJSON

decodeResponseStreamEventWithType :: Text -> Value -> Either String ResponseStreamEvent
decodeResponseStreamEventWithType eventType =
    AesonTypes.parseEither (parseStreamEventWithType eventType)
