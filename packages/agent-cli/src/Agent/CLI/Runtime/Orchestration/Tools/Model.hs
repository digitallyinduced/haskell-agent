-- | Resolve the model, dialect and approval policy before acquiring tools.
module Agent.CLI.Runtime.Orchestration.Tools.Model
    ( ToolStartup(..)
    , ToolModelRuntime(..)
    , loadToolStartup
    , resolveToolModel
    ) where

import Agent.CLI.Auth (LoadedAuth(..), isGatewayLoadedAuth)
import Agent.CLI.Config (HarnessConfig)
import Agent.CLI.GatewayClient (cachedGatewayModels, gatewayCredentialIdentity)
import Agent.CLI.GatewayModels (modelOptionsForGatewayModels)
import Agent.CLI.ModelConfig (ResponsesConnection(..), builtinConnectionId)
import Agent.CLI.Models
    ( ModelOption(..), ModelTarget(..), defaultModelFor, rawModelOption
    , resolveConfiguredModel, resolveModelOptionById, resolvePersistedDialect )
import Agent.CLI.Options
    ( ApprovalPolicy(..), CliOptions(..), defaultEffortFor
    , normalizeReasoningEffortForDialect, resolveApprovalPolicy )
import Agent.CLI.Project (ProjectModel(..), ProjectSettings(..))
import Agent.CLI.Runtime.Orchestration.Tools.Request
import Agent.CLI.Runtime.Orchestration.Types
    ( NativeRunCapabilities, NativeRunHooks(..), NativeInteractionMode(..)
    , fullNativeRunCapabilities )
import Agent.CLI.Session (LegacySubagentTarget, SessionMeta(..), sessionLegacySubagentTarget)
import Agent.CLI.Session.Runtime.Types (StartupRuntime(..))
import Agent.CLI.Startup.Auth (markStartupStage, startupDie)
import Agent.Dialect (Dialect, DialectId, dialectForId)
import qualified Agent.OpenRouter as OpenRouter
import Agent.Provider (Provider(..))
import Agent.ReasoningEffort (parseReasoningEffort, reasoningEffortText)
import Agent.Responses.GenericClient (GenericClientOptions(..))
import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.IORef (readIORef)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text

data ToolStartup = ToolStartup
    { toolNativeCapabilities :: NativeRunCapabilities
    , toolOpenRouterOptions :: OpenRouter.ClientOptions
    , toolHarnessConfig :: HarnessConfig
    , toolGatewaySelection :: Maybe ModelOption
    , toolGatewayAllowedChildModels :: Maybe [Text]
    }

data ToolModelRuntime = ToolModelRuntime
    { toolProvider :: Provider
    , toolModel :: Text
    , toolTransportModel :: Text -> Text
    , toolInferredTarget :: ModelTarget
    , toolCustomGenericOptions :: Maybe GenericClientOptions
    , toolDialectId :: DialectId
    , toolDialect :: Dialect
    , toolResumeTargetChanged :: Bool
    , toolRefreshDialectContext :: Bool
    , toolLegacySubagentTarget :: Maybe LegacySubagentTarget
    , toolEffortText :: Text
    , toolPolicy :: ApprovalPolicy
    , toolClaudeBypassEnabled :: Bool
    }

loadToolStartup
    :: AgentToolsRequest windowTitleResult
    -> IO ToolStartup
loadToolStartup request@AgentToolsRequest
    { loaded
    , connectedGateway
    , gatewayIdentity
    , startup
    } = do
    let toolNativeCapabilities =
            maybe
                fullNativeRunCapabilities
                (.nativeCapabilities)
                startup.startupNativeHooks
    toolOpenRouterOptions <- OpenRouter.clientOptionsFromEnv
    -- Tool resources and initial-context discovery now share one startup
    -- frontier, so attribute the elapsed interval to both.
    markStartupStage startup "Loading tools and context…"
    when (isGatewayLoadedAuth loaded /= isJust gatewayIdentity) $
        startupDie startup
            "gateway session binding and loaded credentials disagree"
    when ((gatewayCredentialIdentity <$> connectedGateway) /= gatewayIdentity) $
        startupDie startup
            "gateway credential snapshot and session binding disagree"
    let toolHarnessConfig = startup.startupHarnessConfig
    (toolGatewaySelection, toolGatewayAllowedChildModels) <-
        selectGatewayModels request
    pure ToolStartup{..}

selectGatewayModels
    :: AgentToolsRequest windowTitleResult
    -> IO (Maybe ModelOption, Maybe [Text])
selectGatewayModels AgentToolsRequest
    { loaded
    , catalog
    , gatewayModelsRef
    , options
    , transitionTarget
    , configuredOptionTarget
    , resumedTarget
    , projectTarget
    , targetHint
    , startup
    }
    | not (isGatewayLoadedAuth loaded) = pure (Nothing, Nothing)
    | otherwise = do
        access <-
            readIORef gatewayModelsRef >>= \case
                Nothing ->
                    startupDie startup
                        "The organization gateway model catalog is unavailable."
                Just value -> pure value
        cachedGatewayModels access >>= \case
            Nothing ->
                startupDie startup
                    "The organization gateway model catalog is unavailable."
            Just models ->
                case modelOptionsForGatewayModels catalog models of
                    [] ->
                        startupDie startup
                            "The organization gateway does not offer any models."
                    firstAvailable : remainingAvailable -> do
                        let available = firstAvailable : remainingAvailable
                            resolveTarget target =
                                resolveModelOptionById
                                    available
                                    target.targetModelId
                        selected <- case options.optModel of
                            Just requested ->
                                case resolveModelOptionById available requested of
                                    Nothing ->
                                        startupDie startup $
                                            "Model '"
                                                <> requested
                                                <> "' is not available through your organization gateway."
                                    Just selected -> pure selected
                            Nothing ->
                                pure $
                                    fromMaybe firstAvailable $
                                        (transitionTarget >>= resolveTarget)
                                            <|> (configuredOptionTarget >>= resolveTarget)
                                            <|> (resumedTarget >>= resolveTarget)
                                            <|> (projectTarget >>= resolveTarget)
                                            <|> (targetHint >>= resolveTarget)
                        pure
                            ( Just selected
                            , Just (map (.modelTarget.targetModelId) available)
                            )

resolveToolModel
    :: AgentToolsRequest windowTitleResult
    -> ToolStartup
    -> ToolModelRuntime
resolveToolModel AgentToolsRequest
    { loaded
    , catalog
    , targetHint
    , options
    , customResponses
    , customBearerToken
    , resumed
    , projectSettings
    , transitionTarget
    , isTty
    , startup
    } ToolStartup
    { toolOpenRouterOptions = openRouterOptions
    , toolGatewaySelection = gatewaySelection
    } =
    ToolModelRuntime{..}
  where
    toolProvider = loaded.loadedProvider
    fallbackModel =
        defaultModelFor catalog toolProvider
    unrestrictedModel =
        fromMaybe
            (maybe fallbackModel (.targetModelId) targetHint)
            options.optModel
    toolModel =
        maybe
            unrestrictedModel
            (.modelTarget.targetModelId)
            gatewaySelection
    rawTarget = (rawModelOption toolProvider toolModel).modelTarget
    inferredTarget0 =
        maybe
            (fromMaybe rawTarget targetHint)
            (.modelTarget)
            gatewaySelection
    toolTransportModel = case customResponses of
        Just _ ->
            \name ->
                case resolveConfiguredModel catalog name of
                    Just option
                        | option.modelTarget.targetConnectionId
                            == inferredTarget0.targetConnectionId ->
                            option.modelTarget.targetWireModelId
                    _
                        | name == toolModel ->
                            inferredTarget0.targetWireModelId
                        | otherwise -> name
        _ -> case toolProvider of
            OpenRouterProvider -> OpenRouter.mapModel openRouterOptions
            _ -> id
    toolInferredTarget =
        inferredTarget0
            { targetWireModelId =
                if inferredTarget0.targetConnectionId
                    == builtinConnectionId OpenRouterProvider
                    && inferredTarget0.targetWireModelId
                        == inferredTarget0.targetModelId
                    then toolTransportModel toolModel
                    else inferredTarget0.targetWireModelId
            }
    toolCustomGenericOptions = do
        (_, responses) <- customResponses
        pure GenericClientOptions
            { baseUrl = Text.unpack responses.responsesBaseUrl
            , model = toolInferredTarget.targetWireModelId
            , bearerToken = customBearerToken
            , requestTimeoutSeconds =
                responses.responsesRequestTimeoutSeconds
            }
    persistedTarget = case fst <$> resumed of
        Just meta ->
            Just
                ( meta.metaDialect
                , meta.metaTransportModel
                )
        Nothing -> do
            remembered <- projectSettings.settingsLastModel
            let target = remembered.projectModelTarget
            if target.targetProvider == toolProvider
                then Just
                    ( target.targetDialect
                    , Just target.targetWireModelId
                    )
                else Nothing
    resolvedPersistedTarget =
        (\(storedDialect, storedTransportModel) ->
            resolvePersistedDialect
                storedDialect
                storedTransportModel
                toolInferredTarget)
            <$> persistedTarget
    mappedTargetChanged = maybe False snd resolvedPersistedTarget
    toolDialectId = case gatewaySelection of
        Just selected -> selected.modelTarget.targetDialect
        Nothing -> case transitionTarget of
            Just target -> target.targetDialect
            Nothing -> case options.optModel of
                Just _ -> toolInferredTarget.targetDialect
                Nothing
                    | mappedTargetChanged -> toolInferredTarget.targetDialect
                    | otherwise ->
                        maybe
                            toolInferredTarget.targetDialect
                            fst
                            resolvedPersistedTarget
    toolDialect = dialectForId toolDialectId
    toolResumeTargetChanged = case fst <$> resumed of
        Just meta ->
            toolProvider /= meta.metaProvider
                || toolInferredTarget.targetConnectionId /= meta.metaConnection
                || toolModel /= meta.metaModel
                || mappedTargetChanged
                || toolDialectId /= meta.metaDialect
        Nothing -> False
    toolRefreshDialectContext = case fst <$> resumed of
        Just meta -> toolDialectId /= meta.metaDialect
        Nothing -> False
    toolLegacySubagentTarget =
        sessionLegacySubagentTarget . fst <$> resumed
    effort =
        normalizeReasoningEffortForDialect toolDialectId $
            fromMaybe
                (maybe
                    (defaultEffortFor toolProvider)
                    (either
                        (const (defaultEffortFor toolProvider))
                        id
                        . parseReasoningEffort
                        . (.metaEffort))
                    (fst <$> resumed))
                options.optEffort
    toolEffortText = reasoningEffortText effort
    toolPolicy = case startup.startupNativeHooks of
        Just hooks -> case hooks.nativeInteractionMode of
            NativeYolo -> ApproveAll
            NativeAsk -> PromptMutating
            NativePlan -> PromptMutating
        Nothing ->
            resolveApprovalPolicy options isTty
                projectSettings.settingsAutoApprove
    toolClaudeBypassEnabled =
        case startup.startupNativeHooks of
            Just hooks ->
                hooks.nativeInteractionMode == NativeYolo
            Nothing ->
                not options.optNoYolo
                    && (options.optYolo
                        || projectSettings.settingsAutoApprove)
