module Agent.Json.Internal
    ( RawJson(..)
    , Extensions(..)
    ) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)

-- | A validated JSON value kept as bytes.  The constructor is private outside
-- this package; values are made by 'validateRawJson' or the raw decoder.
newtype RawJson = RawJson BS.ByteString
    deriving stock (Eq, Ord)

instance Show RawJson where
    show (RawJson bytes) = "RawJson <" <> show (BS.length bytes) <> " bytes>"

-- | Unknown object members, indexed by their key.  Inserting a key replaces
-- the previous value (the wire policy is last-key-wins).
data Extensions = Extensions
    !(Map Text RawJson)
    !(Set Text)

instance Eq Extensions where
    Extensions left _ == Extensions right _ = left == right

instance Show Extensions where
    showsPrec precedence (Extensions values _) =
        showsPrec precedence values

instance Semigroup Extensions where
    Extensions left leftPresent <> Extensions right rightPresent =
        Extensions
            (Map.union right left)
            (leftPresent <> rightPresent)

instance Monoid Extensions where
    mempty = Extensions Map.empty mempty
