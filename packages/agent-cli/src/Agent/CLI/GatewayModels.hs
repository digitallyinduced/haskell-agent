-- | Load the authoritative model options exposed by a connected organization
-- gateway for native clients that do not own a long-running CLI session.
module Agent.CLI.GatewayModels
    ( loadGatewayModelOptionsAt
    , loadGatewayModelOptionsWithCredentialAt
    , modelOptionsForGatewayModels
    , modelOptionsForGatewayState
    , selectGatewayModelOption
    ) where

import Agent.CLI.GatewayClient
    ( GatewayCredential
    , GatewayModel(..)
    , GatewayModelProvider(..)
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
    , ModelTarget(targetProvider, targetModelId)
    , gatewayModelOptions
    , modelCatalog
    , resolveModelOptionById
    )
import Agent.Provider
    ( Provider (ClaudeCodeProvider, OpenAIProvider, XAIProvider) )
import Data.List (find, nubBy)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import System.OsPath (OsPath)

-- | Select from the authoritative gateway catalog before authentication.
-- Saved targets supply an alias preference, never provider identity: older
-- gateway sessions recorded Grok aliases as OpenAI targets.
selectGatewayModelOption
    :: [ModelOption]
    -> Maybe Text
    -> Maybe Provider
    -> [ModelTarget]
    -> Either Text ModelOption
selectGatewayModelOption options requestedModel requestedProvider targetHints =
    case requestedModel of
        Just modelId ->
            case resolveModelOptionById options modelId of
                Nothing ->
                    Left
                        ("Model " <> modelId
                            <> " is not offered by the organization gateway.")
                Just option
                    | matchesProvider option -> Right option
                    | otherwise ->
                        Left
                            "The selected gateway model does not use the requested provider."
        Nothing ->
            case find matchesProvider (preferredOptions <> options) of
                Just option -> Right option
                Nothing ->
                    Left
                        (case requestedProvider of
                            Just _ ->
                                "The organization gateway does not offer any models for the requested provider."
                            Nothing ->
                                "The organization gateway does not offer any models.")
  where
    preferredOptions =
        mapMaybe
            (resolveModelOptionById options . (.targetModelId))
            targetHints
    matchesProvider option =
        maybe True (== option.modelTarget.targetProvider) requestedProvider

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
    concatMap modelOptions $
        nubBy (\first second -> first.gatewayModelId == second.gatewayModelId)
            (filter (publicAlias . (.gatewayModelId)) models)
  where
    modelOptions model =
        gatewayModelOptions catalog
            (case model.gatewayModelProvider of
                GatewayOpenAIProvider -> OpenAIProvider
                GatewayXAIProvider -> XAIProvider
                GatewayAnthropicProvider -> ClaudeCodeProvider)
            [model.gatewayModelId]
    publicAlias modelId =
        not ("router-" `Text.isPrefixOf` modelId)
            && modelId /= "traumimmo-translation"

modelOptionsForGatewayState
    :: ModelCatalog
    -> Maybe [GatewayModel]
    -> [ModelOption]
modelOptionsForGatewayState catalog = \case
    Nothing -> modelCatalog catalog
    Just models ->
        modelOptionsForGatewayModels catalog models
