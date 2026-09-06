-- | The lazily opened store owned by one native engine worker.
-- The supervisor closes it only after its running turns have been joined.
module Agent.CLI.MacOS.EngineStore
    ( acquireStore
    , closeEngineStore
    ) where

import Agent.Store.Postgres
    ( ManagedPostgresConfig, Store, closeStore, openStore )
import Agent.Store.Types (renderStoreError)
import Control.Concurrent.MVar (MVar, modifyMVar)
import qualified Data.Text as Text

acquireStore :: ManagedPostgresConfig -> MVar (Maybe Store) -> IO Store
acquireStore config state =
    modifyMVar state \case
        Just store -> pure (Just store, store)
        Nothing ->
            openStore config >>= \case
                Left err -> fail (Text.unpack (renderStoreError err))
                Right store -> pure (Just store, store)

closeEngineStore :: MVar (Maybe Store) -> IO ()
closeEngineStore state =
    modifyMVar state \case
        Nothing -> pure (Nothing, ())
        Just store -> closeStore store >> pure (Nothing, ())
