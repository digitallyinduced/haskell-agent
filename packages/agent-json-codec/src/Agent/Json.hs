-- | Opaque JSON values used at protocol boundaries.
--
-- This package intentionally does not expose a generic JSON value/object
-- representation.  'RawJson' is an owned, validated byte slice which may be
-- forwarded or passed to a typed decoder.
module Agent.Json
    ( RawJson
    , rawJsonBytes
    , Extensions
    , emptyExtensions
    , extensionsFromList
    , extensionsToList
    , extensionsSingleton
    , appendExtension
    , insertExtension
    , lookupExtension
    , deleteExtension
    , markExtensionFieldPresent
    , extensionFieldWasPresent
    ) where

import Agent.Json.Internal (Extensions(..), RawJson(..))
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)

rawJsonBytes :: RawJson -> BS.ByteString
rawJsonBytes (RawJson bytes) = bytes

emptyExtensions :: Extensions
emptyExtensions = Extensions Map.empty Set.empty

extensionsFromList :: [(Text, RawJson)] -> Extensions
extensionsFromList values =
    Extensions (Map.fromList values) Set.empty

extensionsToList :: Extensions -> [(Text, RawJson)]
extensionsToList (Extensions values _) = Map.toAscList values

extensionsSingleton :: Text -> RawJson -> Extensions
extensionsSingleton key value =
    Extensions (Map.singleton key value) Set.empty

-- | Insert an extension. Duplicate keys use the last value.
appendExtension :: Text -> RawJson -> Extensions -> Extensions
appendExtension key value (Extensions values present) =
    Extensions (Map.insert key value values) present

insertExtension :: Text -> RawJson -> Extensions -> Extensions
insertExtension = appendExtension

lookupExtension :: Text -> Extensions -> Maybe RawJson
lookupExtension key (Extensions values _) = Map.lookup key values

deleteExtension :: Text -> Extensions -> Extensions
deleteExtension key (Extensions values present) =
    Extensions (Map.delete key values) present

markExtensionFieldPresent :: Text -> Extensions -> Extensions
markExtensionFieldPresent key (Extensions values present) =
    Extensions values (Set.insert key present)

extensionFieldWasPresent :: Text -> Extensions -> Bool
extensionFieldWasPresent key (Extensions _ present) =
    key `Set.member` present
