-- | Backend interpreter API.
--
-- This module is for decoder backend implementations, not application code.
module Agent.Json.Decoder.Backend
    ( Decoder(..)
    , NamedField(..)
    , UnknownField(..)
    ) where

import Agent.Json (RawJson)
import Data.Scientific (Scientific)
import Data.Text (Text)

data Decoder a where
    NullDecoder :: a -> Decoder a
    BoolDecoder :: Decoder Bool
    TextDecoder :: Decoder Text
    ScientificDecoder :: Decoder Scientific
    ArrayDecoder :: Decoder a -> Decoder [a]
    ObjectDecoder
        :: state
        -> [NamedField state]
        -> UnknownField state
        -> (state -> Either Text a)
        -> Decoder a
    NullableDecoder :: Decoder a -> Decoder (Maybe a)
    RawJsonDecoder :: Decoder RawJson
    SkipDecoder :: Decoder ()
    MapDecoder
        :: (a -> Either Text b)
        -> Decoder a
        -> Decoder b

data NamedField state where
    NamedField
        :: Text
        -> Decoder value
        -> (value -> state -> Either Text state)
        -> NamedField state

data UnknownField state where
    UnknownField
        :: Decoder value
        -> (Text -> value -> state -> Either Text state)
        -> UnknownField state
