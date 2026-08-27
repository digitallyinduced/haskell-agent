module Agent.CLI.Json
    ( optionalKey
    , defaultKey
    , integer
    , decodeLazy
    ) where

import Agent.Json.Decode (Decoder, FieldsDecoder, JsonError(..), atKeyOptional, decodeEither, nullable)
import qualified Agent.Json.Decode as Hermes
import Control.Monad (join)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Scientific as Scientific

optionalKey :: Text -> Decoder a -> FieldsDecoder (Maybe a)
optionalKey key decoder =
    join <$> atKeyOptional key (nullable decoder)

defaultKey :: a -> Text -> Decoder a -> FieldsDecoder a
defaultKey fallback key decoder =
    maybe fallback id <$> optionalKey key decoder

integer :: Decoder Integer
integer = do
    value <- Hermes.scientific
    case Scientific.floatingOrInteger value of
        Right result -> pure result
        Left (_ :: Double) -> fail "expected an integer"

decodeLazy :: Decoder a -> LBS.ByteString -> Either Text a
decodeLazy decoder =
    either (Left . jsonErrorMessage) Right
        . decodeEither decoder
        . LBS.toStrict
