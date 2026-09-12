-- | Resolve organization-gateway model options for long-running CLI sessions
-- and native clients that require an authoritative one-shot catalog.
module Agent.CLI.GatewayModels
    ( loadGatewayModelOptionsAt
    , loadGatewayModelOptionsWithCredentialAt
    , modelOptionsForGatewayModels
    , modelOptionsForGatewayState
    , selectGatewayModelOption
    , withGatewayModelsForStartup
    ) where

import Agent.Runtime.GatewayClient
    ( GatewayCredential
    , GatewayModelAccess
    , GatewayModel
    , loadGatewayCredentialAt
    , newGatewayModelAccess
    , refreshGatewayModels
    )
import Agent.Runtime.ModelConfig
    ( ModelCatalog
    , loadModelCatalogAt
    )
import Agent.Runtime.Models (ModelOption)
import Agent.Runtime.Startup.Gateway
    ( modelOptionsForGatewayModels
    , modelOptionsForGatewayState
    , selectGatewayModelOption
    )
import Data.Text (Text)
import System.OsPath (OsPath)

-- | Resolve authoritative routing before initializing the provider runtime.
-- Cached catalogs are suitable for immediate picker presentation, but an alias
-- may have changed provider or dialect since the snapshot was persisted.
-- Refreshing only the cache after initialization cannot rebuild that runtime.
withGatewayModelsForStartup
    :: GatewayModelAccess
    -> ([GatewayModel] -> Either Text selection)
    -> (Either Text selection -> IO result)
    -> IO result
withGatewayModelsForStartup access select continue =
    refreshGatewayModels access >>= continue . (>>= select)

loadGatewayModelOptionsAt
    :: OsPath
    -> OsPath
    -> IO (Either Text (ModelCatalog, Maybe [ModelOption]))
loadGatewayModelOptionsAt home cwd =
    loadGatewayCredentialAt home >>= \case
        Left err ->
            pure (Left ("cannot load gateway credential: " <> err))
        Right credential ->
            loadGatewayModelOptionsWithCredentialAt home cwd credential

-- | Load native model options from one immutable credential snapshot. Callers
-- that already established a gateway boundary must not reload gateway.json
-- while deriving the authoritative catalog.
loadGatewayModelOptionsWithCredentialAt
    :: OsPath
    -> OsPath
    -> Maybe GatewayCredential
    -> IO (Either Text (ModelCatalog, Maybe [ModelOption]))
loadGatewayModelOptionsWithCredentialAt home cwd credential =
    loadModelCatalogAt home cwd >>= \case
        Left err -> pure (Left err)
        Right catalog -> case credential of
            Nothing -> pure (Right (catalog, Nothing))
            Just connected -> do
                access <- newGatewayModelAccess connected
                refreshGatewayModels access >>= \case
                    Left err -> pure (Left err)
                    Right [] ->
                        pure
                            (Left
                                "The organization gateway does not offer any models.")
                    Right models ->
                        pure
                            (Right
                                ( catalog
                                , Just
                                    (modelOptionsForGatewayModels
                                        catalog models)
                                ))
