module Agent.CLI.Json
    ( optionalKey
    , defaultKey
    , integer
    , decodeLazy
    ) where

import Agent.Json.Decode
    ( Decoder
    , JsonError(..)
    , decodeEither
    , defaultKey
    , optionalKey
    )
import qualified Agent.Json.Decode as Hermes
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Scientific as Scientific

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
