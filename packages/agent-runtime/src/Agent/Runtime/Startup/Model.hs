-- | Pure model/dialect/effort resolution after the host has loaded its inputs.
-- No terminal, credential loading, or mutable session state enters this boundary.
module Agent.Runtime.Startup.Model
    ( ModelStartupInputs(..)
    , ResumedModel(..)
    , ResolvedModel(..)
    , resolveStartupModel
    ) where

import Agent.Dialect (DialectId)
import Agent.Provider (Provider(..))
import Agent.ReasoningEffort (ReasoningEffort, parseReasoningEffort)
import Agent.Runtime.ModelConfig (ModelCatalog, builtinConnectionId)
import Agent.Runtime.Models
    ( ModelOption(..), ModelTarget(..), defaultModelFor, rawModelOption
    , resolveConfiguredModel, resolvePersistedDialect )
import Agent.Runtime.Options (defaultEffortFor)
import Agent.Runtime.Startup.Policy (normalizeReasoningEffortForDialect)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

-- | Only the persisted fields that participate in startup resolution.
data ResumedModel = ResumedModel
    { resumedProvider :: Provider
    , resumedConnection :: Text
    , resumedModel :: Text
    , resumedDialect :: DialectId
    , resumedTransportModel :: Maybe Text
    , resumedEffort :: Text
    }

data ModelStartupInputs = ModelStartupInputs
    { startupProvider :: Provider
    , startupCatalog :: ModelCatalog
    , startupRequestedModel :: Maybe Text
    -- | Already resolved by the host's authentication/connection selection.
    , startupTargetHint :: Maybe ModelTarget
    -- | An authoritative, admitted gateway selection overrides local hints.
    , startupGatewaySelection :: Maybe ModelOption
    , startupTransitionTarget :: Maybe ModelTarget
    , startupResumedModel :: Maybe ResumedModel
    , startupRememberedTarget :: Maybe ModelTarget
    , startupRequestedEffort :: Maybe ReasoningEffort
    , startupCustomResponses :: Bool
    , startupOpenRouterMap :: Text -> Text
    }

data ResolvedModel = ResolvedModel
    { resolvedProvider :: Provider
    , resolvedModel :: Text
    , resolvedTransportModel :: Text -> Text
    , resolvedTarget :: ModelTarget
    , resolvedDialect :: DialectId
    , resolvedResumeTargetChanged :: Bool
    , resolvedRefreshDialectContext :: Bool
    , resolvedEffort :: ReasoningEffort
    }

resolveStartupModel :: ModelStartupInputs -> ResolvedModel
resolveStartupModel ModelStartupInputs{..} = ResolvedModel{..}
  where
    resolvedProvider = startupProvider
    fallbackModel = defaultModelFor startupCatalog startupProvider
    unrestrictedModel = fromMaybe
        (maybe fallbackModel (.targetModelId) startupTargetHint)
        startupRequestedModel
    resolvedModel = maybe unrestrictedModel (.modelTarget.targetModelId)
        startupGatewaySelection
    rawTarget = (rawModelOption startupProvider resolvedModel).modelTarget
    inferredTarget = maybe (fromMaybe rawTarget startupTargetHint) (.modelTarget)
        startupGatewaySelection
    resolvedTransportModel
        | startupCustomResponses = \name ->
            case resolveConfiguredModel startupCatalog name of
                Just option
                    | option.modelTarget.targetConnectionId == inferredTarget.targetConnectionId ->
                        option.modelTarget.targetWireModelId
                _
                    | name == resolvedModel -> inferredTarget.targetWireModelId
                    | otherwise -> name
        | startupProvider == OpenRouterProvider = startupOpenRouterMap
        | otherwise = id
    resolvedTarget = inferredTarget
        { targetWireModelId =
            if inferredTarget.targetConnectionId == builtinConnectionId OpenRouterProvider
                && inferredTarget.targetWireModelId == inferredTarget.targetModelId
                then resolvedTransportModel resolvedModel
                else inferredTarget.targetWireModelId
        }
    persistedTarget = case startupResumedModel of
        Just resumed -> Just (resumed.resumedDialect, resumed.resumedTransportModel)
        Nothing -> do
            target <- startupRememberedTarget
            if target.targetProvider == startupProvider
                then Just (target.targetDialect, Just target.targetWireModelId)
                else Nothing
    resolvedPersistedTarget =
        (\(storedDialect, storedTransportModel) ->
            resolvePersistedDialect storedDialect storedTransportModel resolvedTarget)
            <$> persistedTarget
    mappedTargetChanged = maybe False snd resolvedPersistedTarget
    resolvedDialect = case startupGatewaySelection of
        Just selected -> selected.modelTarget.targetDialect
        Nothing -> case startupTransitionTarget of
            Just target -> target.targetDialect
            Nothing -> case startupRequestedModel of
                Just _ -> resolvedTarget.targetDialect
                Nothing
                    | mappedTargetChanged -> resolvedTarget.targetDialect
                    | otherwise -> maybe resolvedTarget.targetDialect fst resolvedPersistedTarget
    resolvedResumeTargetChanged = case startupResumedModel of
        Just resumed ->
            startupProvider /= resumed.resumedProvider
                || resolvedTarget.targetConnectionId /= resumed.resumedConnection
                || resolvedModel /= resumed.resumedModel
                || mappedTargetChanged
                || resolvedDialect /= resumed.resumedDialect
        Nothing -> False
    resolvedRefreshDialectContext = case startupResumedModel of
        Just resumed -> resolvedDialect /= resumed.resumedDialect
        Nothing -> False
    resolvedEffort = normalizeReasoningEffortForDialect resolvedDialect $
        fromMaybe
            (maybe (defaultEffortFor startupProvider)
                (either (const (defaultEffortFor startupProvider)) id
                    . parseReasoningEffort . (.resumedEffort))
                startupResumedModel)
            startupRequestedEffort
