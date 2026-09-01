-- | Load the authoritative model options exposed by a connected organization
-- gateway for native clients that do not own a long-running CLI session.
module Agent.CLI.GatewayModels
    ( loadGatewayModelOptionsAt
    , modelOptionsForGatewayModels
    , modelOptionsForGatewayState
    ) where

import Agent.CLI.GatewayClient
    ( GatewayModel(..)
    , GatewayModelProtocol(..)
    , loadGatewayCredentialAt
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
import Agent.Provider
    ( Provider (ClaudeCodeProvider, OpenAIProvider) )
import Data.Text (Text)
import Data.Text qualified as Text
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
                        Right models ->
                            pure
                                (Right
                                    ( catalog
                                    , Just
                                        (modelOptionsForGatewayModels
                                            catalog models)
                                    ))

modelOptionsForGatewayModels
    :: ModelCatalog
    -> [GatewayModel]
    -> [ModelOption]
modelOptionsForGatewayModels catalog models =
    gatewayModelOptions catalog OpenAIProvider responseIds
        <> gatewayModelOptions catalog ClaudeCodeProvider anthropicIds
  where
    responseIds =
        [ model.gatewayModelId
        | model <- models
        , model.gatewayModelProtocol == GatewayResponsesProtocol
        , publicAlias model.gatewayModelId
        ]
    anthropicIds =
        [ model.gatewayModelId
        | model <- models
        , model.gatewayModelProtocol == GatewayAnthropicProtocol
        , publicAlias model.gatewayModelId
        , model.gatewayModelId `notElem` responseIds
        ]
    publicAlias modelId =
        not ("router-" `Text.isPrefixOf` modelId)
            && modelId /= "traumimmo-translation"

modelOptionsForGatewayState
    :: ModelCatalog
    -> Maybe [Text]
    -> [ModelOption]
modelOptionsForGatewayState catalog = \case
    Nothing -> modelCatalog catalog
    Just modelIds ->
        gatewayModelOptions catalog OpenAIProvider modelIds
