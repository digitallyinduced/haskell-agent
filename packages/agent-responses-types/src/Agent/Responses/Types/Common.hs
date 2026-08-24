module Agent.Responses.Types.Common
    ( Field
    , field
    , optionalField
    , objectWith
    , without
    , withoutNonNull
    , TaggedObject(..)
    ) where

import Data.Aeson hiding (TaggedObject)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (catMaybes)
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

instance FromJSON TaggedObject where
    parseJSON = withObject "TaggedObject" $ \o -> TaggedObject
        <$> o .: "type"
        <*> pure (without ["type"] o)
