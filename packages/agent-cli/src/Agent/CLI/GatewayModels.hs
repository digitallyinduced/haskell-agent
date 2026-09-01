-- | Load the authoritative model options exposed by a connected organization
-- gateway for native clients that do not own a long-running CLI session.
module Agent.CLI.GatewayModels
    ( loadGatewayModelOptionsAt
    , modelOptionsForGatewayState
    ) where

import Agent.CLI.GatewayClient
    ( loadGatewayCredentialAt
    , newGatewayModelAccess
    , refreshGatewayModels
    )
import Agent.CLI.ModelConfig
    ( ModelCatalog
    , loadModelCatalogAt
    )
import Agent.CLI.Models
    ( ModelOption
    , gatewayModelOptions
    , modelCatalog
    )
import Agent.Provider (Provider (OpenAIProvider))
import Data.Text (Text)
import System.OsPath (OsPath)

loadGatewayModelOptionsAt
    :: OsPath
    -> OsPath
    -> IO (Either Text (ModelCatalog, Maybe [ModelOption]))
loadGatewayModelOptionsAt home cwd =
    loadModelCatalogAt home cwd >>= \case
        Left err -> pure (Left err)
        Right catalog ->
            loadGatewayCredentialAt home >>= \case
                Left err ->
                    pure (Left ("cannot load gateway credential: " <> err))
                Right Nothing -> pure (Right (catalog, Nothing))
                Right (Just credential) -> do
                    access <- newGatewayModelAccess credential
                    refreshGatewayModels access >>= \case
                        Left err -> pure (Left err)
                        Right [] ->
                            pure
                                (Left
                                    "The organization gateway does not offer any models.")
                        Right modelIds ->
                            pure
                                (Right
                                    ( catalog
                                    , Just
                                        (modelOptionsForGatewayState
                                            catalog
                                            (Just modelIds))
                                    ))

modelOptionsForGatewayState
    :: ModelCatalog
    -> Maybe [Text]
    -> [ModelOption]
modelOptionsForGatewayState catalog = \case
    Nothing -> modelCatalog catalog
    Just modelIds ->
        gatewayModelOptions catalog OpenAIProvider modelIds
