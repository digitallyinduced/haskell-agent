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

import Agent.CLI.GatewayClient
    ( GatewayCredential
    , GatewayModelAccess
    , GatewayModel(..)
    , GatewayModelProvider(..)
    , loadGatewayCredentialAt
    , newGatewayModelAccess
    , refreshGatewayModels
    , cachedGatewayModels
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
import Control.Concurrent.Async (withAsync)
import Control.Monad (void)
import Data.List (find, nubBy)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import System.OsPath (OsPath)

-- | Start from a credential-scoped cached catalog when it satisfies selection.
-- Cache misses, including a newly requested alias absent from the snapshot,
-- retain authoritative synchronous selection. The warm refresh belongs to the
-- continuation's lifetime and is cancelled and joined when that runtime exits.
-- Cached metadata does not grant access: the gateway authorizes every request.
withGatewayModelsForStartup
    :: GatewayModelAccess
    -> ([GatewayModel] -> Either Text selection)
    -> (Either Text selection -> IO result)
    -> IO result
withGatewayModelsForStartup access select continue =
    cachedGatewayModels access >>= \case
        Just models | Right selected <- select models ->
            withAsync (void (refreshGatewayModels access)) \_ ->
                continue (Right selected)
        _ ->
            refreshGatewayModels access >>= continue . (>>= select)

-- | Select from a credential-scoped gateway catalog before authentication.
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
