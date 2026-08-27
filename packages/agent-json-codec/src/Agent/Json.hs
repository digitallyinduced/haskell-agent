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
    , setExtensionsSourceRaw
    , extensionsSourceRaw
    , clearExtensionsSourceRaw
    ) where

import Agent.Json.Internal (Extensions(..), RawJson(..))
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)

rawJsonBytes :: RawJson -> BS.ByteString
rawJsonBytes (RawJson bytes) = bytes

emptyExtensions :: Extensions
emptyExtensions = Extensions Map.empty Set.empty Nothing

extensionsFromList :: [(Text, RawJson)] -> Extensions
extensionsFromList values =
    Extensions (Map.fromList values) Set.empty Nothing

extensionsToList :: Extensions -> [(Text, RawJson)]
extensionsToList (Extensions values _ _) = Map.toAscList values

extensionsSingleton :: Text -> RawJson -> Extensions
extensionsSingleton key value =
    Extensions (Map.singleton key value) Set.empty Nothing

-- | Insert an extension. Duplicate keys use the last value.
appendExtension :: Text -> RawJson -> Extensions -> Extensions
appendExtension key value (Extensions values present source) =
    Extensions (Map.insert key value values) present source

insertExtension :: Text -> RawJson -> Extensions -> Extensions
insertExtension = appendExtension

lookupExtension :: Text -> Extensions -> Maybe RawJson
lookupExtension key (Extensions values _ _) = Map.lookup key values

deleteExtension :: Text -> Extensions -> Extensions
deleteExtension key (Extensions values present source) =
    Extensions (Map.delete key values) present source

markExtensionFieldPresent :: Text -> Extensions -> Extensions
markExtensionFieldPresent key (Extensions values present source) =
    Extensions values (Set.insert key present) source

extensionFieldWasPresent :: Text -> Extensions -> Bool
extensionFieldWasPresent key (Extensions _ present _) =
    key `Set.member` present

setExtensionsSourceRaw :: RawJson -> Extensions -> Extensions
setExtensionsSourceRaw source (Extensions values present _) =
    Extensions values present (Just source)

extensionsSourceRaw :: Extensions -> Maybe RawJson
extensionsSourceRaw (Extensions _ _ source) = source

clearExtensionsSourceRaw :: Extensions -> Extensions
clearExtensionsSourceRaw (Extensions values present _) =
    Extensions values present Nothing
