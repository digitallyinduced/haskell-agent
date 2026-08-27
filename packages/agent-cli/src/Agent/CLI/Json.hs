{-# LANGUAGE LambdaCase #-}

module Agent.CLI.Json
    ( integer
    , decodeLazy
    , valueDecoder
    ) where

import Agent.Json.Decode (Decoder, JsonError(..), decodeEither)
import qualified Agent.Json.Decode as Hermes
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Scientific as Scientific
import qualified Data.Vector as Vector

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

-- | Generic Aeson values are retained only at encoding/protocol boundaries;
-- syntax validation itself still goes through Hermes.
valueDecoder :: Decoder Aeson.Value
valueDecoder =
    Hermes.getType >>= \case
        Hermes.VArray ->
            Aeson.Array . Vector.fromList <$> Hermes.list valueDecoder
        Hermes.VObject ->
            Aeson.Object . KeyMap.fromList
                <$> Hermes.objectAsKeyValues
                    (pure . Key.fromText)
                    valueDecoder
        Hermes.VNumber -> Aeson.Number <$> Hermes.scientific
        Hermes.VString -> Aeson.String <$> Hermes.text
        Hermes.VBoolean -> Aeson.Bool <$> Hermes.bool
        Hermes.VNull -> Aeson.Null <$ Hermes.nullable (pure ())
