{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Errors shared by the managed PostgreSQL and Hasql layers.
module Agent.Store.Types
    ( StoreError(..)
    , renderStoreError
    ) where

import Control.Exception (Exception)
import Data.Text (Text)

data StoreError
    = StoreConfigurationError !Text
    | StoreProcessError !Text
    | StoreConnectionError !Text
    | StoreMigrationError !Text
    deriving (Eq, Show)

instance Exception StoreError

renderStoreError :: StoreError -> Text
renderStoreError = \case
    StoreConfigurationError message -> message
    StoreProcessError message -> message
    StoreConnectionError message -> message
    StoreMigrationError message -> message
