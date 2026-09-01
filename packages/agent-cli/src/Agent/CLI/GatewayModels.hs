-- | Load the authoritative model options exposed by a connected organization
-- gateway for native clients that do not own a long-running CLI session.
module Agent.CLI.GatewayModels
    ( loadGatewayModelOptionsAt
    , loadGatewayModelOptionsWithCredentialAt
    , modelOptionsForGatewayModels
    , modelOptionsForGatewayState
    ) where

import Agent.CLI.GatewayClient
    ( GatewayCredential
    , GatewayModel(..)
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
