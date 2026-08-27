module Agent.Responses.Types.Common
    ( Field
    , field
    , optionalField
    , objectWith
    , without
    , withoutNonNull
    , TaggedObject(..)
    , taggedObjectDecoder
    , optionalAtKey
    , skipValue
    , aesonValueDecoder
    , aesonObjectDecoder
    ) where

import Control.Monad (join)
import Data.Aeson hiding (TaggedObject)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import qualified Data.Vector as Vector
import Data.Maybe (catMaybes)
import qualified Data.Hermes as Hermes
import Data.Text (Text)

type Field = (Key.Key, Aeson.Value)

field :: ToJSON a => Text -> a -> Field
field name value = (Key.fromText name, toJSON value)

optionalField :: ToJSON a => Text -> Maybe a -> Maybe Field
optionalField name = fmap (field name)

objectWith :: Aeson.Object -> [Maybe Field] -> Aeson.Value
objectWith extras members =
    Aeson.Object (foldl' insert extras (catMaybes members))
  where
    insert object (key, value) = KeyMap.insert key value object

without :: [Text] -> Aeson.Object -> Aeson.Object
without names object = foldl' (flip (KeyMap.delete . Key.fromText)) object names

withoutNonNull :: [Text] -> Aeson.Object -> Aeson.Object
withoutNonNull names object = foldl' remove object names
  where
    remove current name =
        let key = Key.fromText name
        in case KeyMap.lookup key current of
            Just Aeson.Null -> current
            _ -> KeyMap.delete key current

data TaggedObject = TaggedObject
    { tag    :: !Text
    , fields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON TaggedObject where
    toJSON TaggedObject { tag, fields } = objectWith fields [Just (field "type" tag)]

taggedObjectDecoder :: Hermes.Decoder TaggedObject
taggedObjectDecoder =
    Hermes.object $
        TaggedObject
            <$> Hermes.atKey "type" Hermes.text
            <*> pure KeyMap.empty

-- | A missing or explicit @null@ member decodes to 'Nothing', matching the
-- wire semantics used by the Responses API.
optionalAtKey
    :: Text
    -> Hermes.Decoder value
    -> Hermes.FieldsDecoder (Maybe value)
optionalAtKey key decoder =
    join <$> Hermes.atKeyOptional key (Hermes.nullable decoder)

-- | Consume a JSON value without constructing an intermediate tree.
skipValue :: Hermes.Decoder ()
skipValue =
    Hermes.getType >>= \case
        Hermes.VArray -> () <$ Hermes.list skipValue
        Hermes.VObject ->
            () <$ Hermes.objectFold () (\_ () -> skipValue)
        Hermes.VNumber -> () <$ Hermes.scientific
        Hermes.VString -> () <$ Hermes.text
        Hermes.VBoolean -> () <$ Hermes.bool
        Hermes.VNull -> do
            isNull <- Hermes.isNull
            if isNull then pure () else fail "expected null"

-- Transitional decoder for public fields which still expose Aeson's JSON DOM.
-- Parsing is nevertheless performed entirely by Hermes.
aesonValueDecoder :: Hermes.Decoder Aeson.Value
aesonValueDecoder =
    Hermes.getType >>= \case
        Hermes.VArray ->
            Aeson.Array . Vector.fromList <$> Hermes.list aesonValueDecoder
        Hermes.VObject -> Aeson.Object <$> aesonObjectDecoder
        Hermes.VNumber -> Aeson.Number <$> Hermes.scientific
        Hermes.VString -> Aeson.String <$> Hermes.text
        Hermes.VBoolean -> Aeson.Bool <$> Hermes.bool
        Hermes.VNull -> Aeson.Null <$ Hermes.nullable skipValue

aesonObjectDecoder :: Hermes.Decoder Aeson.Object
aesonObjectDecoder =
    KeyMap.fromList
        <$> Hermes.objectAsKeyValues
            (pure . Key.fromText)
            aesonValueDecoder
