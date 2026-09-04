-- | Load the authoritative model options exposed by a connected organization
-- gateway for native clients that do not own a long-running CLI session.
module Agent.CLI.GatewayModels
    ( gatewayProviderForStartup
    , loadGatewayModelOptionsAt
    , loadGatewayModelOptionsWithCredentialAt
    , modelOptionsForGatewayModels
    , modelOptionsForGatewayState
    , resolveGatewayModelTarget
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
    ( ModelOption(modelTarget)
    , ModelTarget(targetProvider)
    , gatewayModelOptions
    , modelCatalog
    , resolveModelOptionById
    )
import Agent.Provider
    ( Provider (ClaudeCodeProvider, OpenAIProvider) )
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import System.OsPath (OsPath)

gatewayProviderForStartup
    :: Maybe ModelTarget
    -> Maybe Provider
    -> Maybe Provider
    -> Provider
gatewayProviderForStartup targetHint requestedProvider resumedProvider =
    fromMaybe OpenAIProvider $
        (.targetProvider) <$> targetHint
            <|> requestedProvider
            <|> resumedProvider

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

-- | Resolve an exact alias from the live organization gateway catalog.
--
-- Local model configuration may add presentation metadata, but it cannot make
-- an unadvertised alias selectable or change the gateway-pinned transport.
resolveGatewayModelTarget
    :: ModelCatalog
    -> [GatewayModel]
    -> Text
    -> Either Text ModelTarget
resolveGatewayModelTarget catalog models modelId =
    case
        resolveModelOptionById
            (modelOptionsForGatewayModels catalog models)
            modelId
    of
        Nothing ->
            Left $
                "Model alias \""
                    <> modelId
                    <> "\" is not offered by the active organization gateway."
        Just option -> Right option.modelTarget

modelOptionsForGatewayState
    :: ModelCatalog
    -> Maybe [Text]
    -> [ModelOption]
modelOptionsForGatewayState catalog = \case
    Nothing -> modelCatalog catalog
    Just modelIds ->
        gatewayModelOptions catalog OpenAIProvider modelIds
