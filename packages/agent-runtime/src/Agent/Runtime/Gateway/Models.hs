-- | Authoritative organization model selection shared by session hosts.
module Agent.Runtime.Gateway.Models
    ( loadGatewayModelOptionsAt
    , loadGatewayModelOptionsWithCredentialAt
    , modelOptionsForGatewayModels
    , modelOptionsForGatewayState
    , selectGatewayModelOption
    , withGatewayModelsForStartup
    , withPreparedGatewayModelsForStartup
    ) where

import Agent.Runtime.GatewayClient
    ( GatewayCredential, GatewayModelAccess, GatewayModel
    , loadGatewayCredentialAt, newGatewayModelAccess, refreshGatewayModels )
import Agent.Runtime.ModelConfig (ModelCatalog, loadModelCatalogAt)
import Agent.Runtime.Models (ModelOption)
import Agent.Runtime.Startup.Gateway
    ( modelOptionsForGatewayModels, modelOptionsForGatewayState, selectGatewayModelOption )
import Control.Concurrent.Async (wait, withAsync)
import Data.Text (Text)
import System.OsPath (OsPath)

-- | The display cache cannot authorize provider routing. Resolve the current
-- catalog before constructing its provider runtime.
withGatewayModelsForStartup
    :: GatewayModelAccess
    -> ([GatewayModel] -> Either Text selection)
    -> (Either Text selection -> IO result)
    -> IO result
withGatewayModelsForStartup access select continue =
    refreshGatewayModels access >>= continue . (>>= select)

withPreparedGatewayModelsForStartup
    :: GatewayModelAccess
    -> (IO (Either Text [GatewayModel]) -> IO result)
    -> IO result
withPreparedGatewayModelsForStartup access continue =
    withAsync (refreshGatewayModels access) (continue . wait)

loadGatewayModelOptionsAt
    :: OsPath -> OsPath -> IO (Either Text (ModelCatalog, Maybe [ModelOption]))
loadGatewayModelOptionsAt home cwd = loadGatewayCredentialAt home >>= \case
    Left err -> pure (Left ("cannot load gateway credential: " <> err))
    Right credential -> loadGatewayModelOptionsWithCredentialAt home cwd credential

-- | Consume one immutable credential snapshot throughout routing resolution.
loadGatewayModelOptionsWithCredentialAt
    :: OsPath -> OsPath -> Maybe GatewayCredential
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
                    Right [] -> pure (Left "The organization gateway does not offer any models.")
                    Right models -> pure (Right
                        (catalog, Just (modelOptionsForGatewayModels catalog models)))
