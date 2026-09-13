-- | Resolve the model, dialect and approval policy before acquiring tools.
module Agent.CLI.Runtime.Orchestration.Tools.Model
    ( ToolStartup(..)
    , ToolModelRuntime(..)
    , loadToolStartup
    , resolveToolModel
    ) where

import Agent.Accounts.Auth (LoadedAuth(..), isGatewayLoadedAuth)
import Agent.Runtime.Config (HarnessConfig)
import Agent.Runtime.GatewayClient (cachedGatewayModels, gatewayCredentialIdentity)
import Agent.Runtime.Startup.Gateway (modelOptionsForGatewayModels, selectGatewayModelOption)
import Agent.Runtime.ModelConfig (ResponsesConnection(..))
import Agent.Runtime.Models (ModelOption(..), ModelTarget(..))
import Agent.Runtime.Startup.Model
import Agent.Runtime.Startup.Policy
    ( NativeApprovalMode(..), resolveNativeApproval, claudeBypassEnabled )
import Agent.CLI.Options
    ( ApprovalPolicy, CliOptions(..), Override(..), resolveApprovalPolicy )
import Agent.CLI.Project (ProjectModel(..), ProjectSettings(..))
import Agent.CLI.Runtime.Orchestration.Tools.Request
import Agent.CLI.Runtime.Orchestration.Types
    ( NativeRunCapabilities, NativeRunHooks(..), NativeInteractionMode(..)
    , fullNativeRunCapabilities )
import Agent.Runtime.Session (LegacySubagentTarget, SessionMeta(..), sessionLegacySubagentTarget)
import Agent.CLI.Session.Runtime.Types (StartupRuntime(..))
import Agent.CLI.Startup.Auth (markStartupStage, startupDie)
import Agent.Dialect (Dialect, DialectId, dialectForId)
import qualified Agent.OpenRouter as OpenRouter
import Agent.Provider (Provider(..))
import Agent.ReasoningEffort (reasoningEffortText)
import Agent.Responses.GenericClient (GenericClientOptions(..))
import Control.Monad (when)
import Data.IORef (readIORef)
import Data.Maybe (isJust, catMaybes)
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
                        selected <- either (startupDie startup) pure $
                            selectGatewayModelOption available options.optModel
                                (Just loaded.loadedProvider)
                                (catMaybes
                                    [ targetHint, transitionTarget, configuredOptionTarget
                                    , resumedTarget, projectTarget
                                    ])
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
    resolved = resolveStartupModel ModelStartupInputs
        { startupProvider = loaded.loadedProvider
        , startupCatalog = catalog
        , startupRequestedModel = options.optModel
        , startupTargetHint = targetHint
        , startupGatewaySelection = gatewaySelection
        , startupTransitionTarget = transitionTarget
        , startupResumedModel = toResumedModel . fst <$> resumed
        , startupRememberedTarget = (.projectModelTarget) <$> projectSettings.settingsLastModel
        , startupRequestedEffort = options.optEffort
        , startupCustomResponses = isJust customResponses
        , startupOpenRouterMap = OpenRouter.mapModel openRouterOptions
        }
    toResumedModel meta = ResumedModel
        { resumedProvider = meta.metaProvider
        , resumedConnection = meta.metaConnection
        , resumedModel = meta.metaModel
        , resumedDialect = meta.metaDialect
        , resumedTransportModel = meta.metaTransportModel
        , resumedEffort = meta.metaEffort
        }
    toolProvider = resolved.resolvedProvider
    toolModel = resolved.resolvedModel
    toolTransportModel = resolved.resolvedTransportModel
    toolInferredTarget = resolved.resolvedTarget
    toolCustomGenericOptions = do
        (_, responses) <- customResponses
        pure GenericClientOptions
            { baseUrl = Text.unpack responses.responsesBaseUrl
            , model = toolInferredTarget.targetWireModelId
            , bearerToken = customBearerToken
            , requestTimeoutSeconds =
                responses.responsesRequestTimeoutSeconds
            }
    toolDialectId = resolved.resolvedDialect
    toolDialect = dialectForId toolDialectId
    toolResumeTargetChanged = resolved.resolvedResumeTargetChanged
    toolRefreshDialectContext = resolved.resolvedRefreshDialectContext
    toolLegacySubagentTarget =
        sessionLegacySubagentTarget . fst <$> resumed
    toolEffortText = reasoningEffortText resolved.resolvedEffort
    nativeApproval = (\hooks ->
        ( case hooks.nativeInteractionMode of
            NativeYolo -> NativeApprovalYolo
            NativeAsk -> NativeApprovalAsk
            NativePlan -> NativeApprovalPlan
        , isJust hooks.nativeRegisterInteractionMode
        )) <$> startup.startupNativeHooks
    toolPolicy = case nativeApproval of
        Just (mode, _) -> resolveNativeApproval mode
        Nothing ->
            resolveApprovalPolicy options isTty
                projectSettings.settingsAutoApprove
    toolClaudeBypassEnabled = claudeBypassEnabled nativeApproval
        (case options.optYolo of
            Inherit -> Nothing
            Explicit enabled -> Just enabled)
        projectSettings.settingsAutoApprove
