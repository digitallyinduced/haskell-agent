module Agent.Responses.Types.Common
    ( Field
    , field
    , optionalField
    , objectWith
    , TaggedObject(..)
    , taggedObjectDecoder
    , optionalAtKey
    , RawJson
    , rawJsonDecoder
    , ResponseMetadata(..)
    , responseMetadataDecoder
    , EnvironmentVariables(..)
    , environmentVariablesDecoder
    ) where

import Control.Monad (join)
import Agent.Json (RawJson, rawJsonDecoder)
import Data.Aeson hiding (TaggedObject)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Hermes as Hermes
import Data.Text (Text)

type Field = (Key.Key, Aeson.Value)

field :: ToJSON a => Text -> a -> Field
field name value = (Key.fromText name, toJSON value)

optionalField :: ToJSON a => Text -> Maybe a -> Maybe Field
optionalField name = fmap (field name)

objectWith :: [Maybe Field] -> Aeson.Value
objectWith members =
    Aeson.Object (foldl' insert KeyMap.empty (catMaybes members))
  where
    insert object (key, value) = KeyMap.insert key value object

newtype TaggedObject = TaggedObject
    { tag :: Text
    } deriving stock (Eq, Show)

instance ToJSON TaggedObject where
    toJSON TaggedObject { tag } = objectWith [Just (field "type" tag)]

taggedObjectDecoder :: Hermes.Decoder TaggedObject
taggedObjectDecoder =
    Hermes.object $
        TaggedObject
            <$> Hermes.atKey "type" Hermes.text

-- | A missing or explicit @null@ member decodes to 'Nothing', matching the
-- wire semantics used by the Responses API.
optionalAtKey
    :: Text
    -> Hermes.Decoder value
    -> Hermes.FieldsDecoder (Maybe value)
optionalAtKey key decoder =
    join <$> Hermes.atKeyOptional key (Hermes.nullable decoder)

-- | Responses metadata: string keys mapped to string values.
newtype ResponseMetadata = ResponseMetadata
    { unResponseMetadata :: Map Text Text
    } deriving stock (Eq, Show)

instance ToJSON ResponseMetadata where
    toJSON (ResponseMetadata entries) = toJSON entries

responseMetadataDecoder :: Hermes.Decoder ResponseMetadata
responseMetadataDecoder = ResponseMetadata <$> textMapDecoder

-- | Environment variables supplied to a local shell action.
newtype EnvironmentVariables = EnvironmentVariables
    { unEnvironmentVariables :: Map Text Text
    } deriving stock (Eq, Show)

instance ToJSON EnvironmentVariables where
    toJSON (EnvironmentVariables entries) = toJSON entries

environmentVariablesDecoder :: Hermes.Decoder EnvironmentVariables
environmentVariablesDecoder = EnvironmentVariables <$> textMapDecoder

textMapDecoder :: Hermes.Decoder (Map Text Text)
textMapDecoder = Map.fromList
    <$> Hermes.objectAsKeyValues pure Hermes.text
