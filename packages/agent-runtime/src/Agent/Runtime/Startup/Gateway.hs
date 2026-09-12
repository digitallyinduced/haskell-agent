-- | Pure credential-scoped gateway catalog projection and selection.
module Agent.Runtime.Startup.Gateway
    ( modelOptionsForGatewayModels
    , modelOptionsForGatewayState
    , selectGatewayModelOption
    ) where

import Agent.Runtime.GatewayClient (GatewayModel(..), GatewayModelProvider(..))
import Agent.Runtime.ModelConfig (ModelCatalog)
import Agent.Runtime.Models
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

-- | Saved targets supply an alias preference, never provider identity: older
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
    Just models -> modelOptionsForGatewayModels catalog models
