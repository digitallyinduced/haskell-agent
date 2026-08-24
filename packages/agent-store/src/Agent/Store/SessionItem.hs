{-# LANGUAGE OverloadedStrings #-}

-- | Storage-facing representation of a response item.
--
-- The wire representation is deliberately opaque here.  JSON encoding and
-- decoding belongs to the CLI/API boundary; PostgreSQL only persists the
-- item's type, representation, and serialized payload text.
module Agent.Store.SessionItem
    ( StoredResponseItem(..)
    ) where

import Data.Text (Text)

data StoredResponseItem = StoredResponseItem
    { storedResponseItemType :: !Text
    , storedResponseItemRepresentation :: !Text
    , storedResponseItemPayload :: !Text
    }
    deriving (Eq, Show)
