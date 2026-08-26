module Agent.Json.Internal
    ( RawJson(..)
    , Extensions(..)
    ) where

import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import Data.Text (Text)

-- | A validated JSON value kept as bytes.  The constructor is private outside
-- this package; values are made by 'validateRawJson' or the raw decoder.
newtype RawJson = RawJson BS.ByteString
    deriving stock (Eq, Ord)

instance Show RawJson where
    show (RawJson bytes) = "RawJson <" <> show (BS.length bytes) <> " bytes>"

-- | Unknown object members, indexed by their key.  Inserting a key replaces
-- the previous value (the wire policy is last-key-wins).
newtype Extensions = Extensions (Map Text RawJson)
    deriving stock (Eq, Show)
