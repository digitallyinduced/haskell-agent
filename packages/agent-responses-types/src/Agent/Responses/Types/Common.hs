module Agent.Responses.Types.Common
    ( TaggedObject(..)
    , taggedObjectEncoder
    , taggedObjectDecoder
    , required
    ) where

import Agent.Json (Extensions, emptyExtensions, insertExtension)
import qualified Agent.Json.Decoder as Decoder
import qualified Agent.Json.Encoder as Encoder
import Data.Text (Text)

data TaggedObject = TaggedObject
    { tag :: !Text
    , fields :: !Extensions
    }
    deriving stock (Eq, Show)

taggedObjectEncoder :: Encoder.Encoder TaggedObject
taggedObjectEncoder =
    Encoder.object
        [ Encoder.field "type" Encoder.text (.tag)
        , Encoder.extensionsField (.fields)
        ]

data TaggedObjectState = TaggedObjectState
    { stateTag :: !(Maybe Text)
    , stateFields :: !Extensions
    }

taggedObjectDecoder :: Decoder.Decoder TaggedObject
taggedObjectDecoder =
    Decoder.object
        (TaggedObjectState Nothing emptyExtensions)
        [ Decoder.field "type" Decoder.text \value state ->
            Right state { stateTag = Just value }
        ]
        (Decoder.unknownField Decoder.rawJson
            \key value state ->
                Right state
                    { stateFields =
                        insertExtension key value state.stateFields
                    })
        \state ->
            TaggedObject
                <$> required "type" state.stateTag
                <*> Right state.stateFields

required :: Text -> Maybe value -> Either Text value
required label =
    maybe (Left ("missing required field " <> label)) Right
