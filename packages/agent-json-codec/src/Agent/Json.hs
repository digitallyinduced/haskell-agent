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
    ) where

import Agent.Json.Internal (Extensions(..), RawJson(..))
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)

rawJsonBytes :: RawJson -> BS.ByteString
rawJsonBytes (RawJson bytes) = bytes

emptyExtensions :: Extensions
emptyExtensions = Extensions Map.empty

extensionsFromList :: [(Text, RawJson)] -> Extensions
extensionsFromList = Extensions . Map.fromList

extensionsToList :: Extensions -> [(Text, RawJson)]
extensionsToList (Extensions values) = Map.toAscList values

extensionsSingleton :: Text -> RawJson -> Extensions
extensionsSingleton key value = Extensions (Map.singleton key value)

-- | Insert an extension. Duplicate keys use the last value.
appendExtension :: Text -> RawJson -> Extensions -> Extensions
appendExtension key value (Extensions values) =
    Extensions (Map.insert key value values)

insertExtension :: Text -> RawJson -> Extensions -> Extensions
insertExtension = appendExtension

lookupExtension :: Text -> Extensions -> Maybe RawJson
lookupExtension key (Extensions values) = Map.lookup key values

deleteExtension :: Text -> Extensions -> Extensions
deleteExtension key (Extensions values) = Extensions (Map.delete key values)
