{-# LANGUAGE TemplateHaskell #-}

-- | Metadata returned by the Codex @/models@ endpoint.
--
-- Unknown enum values are retained in @Other@ constructors. Unknown object
-- fields are ignored.
module Agent.OpenAI.Models.Types
    ( ReasoningEffort(..)
    , reasoningEffortText
    , ReasoningEffortPreset(..)
    , ReasoningSummary(..)
    , reasoningSummaryText
    , Verbosity(..)
    , verbosityText
    , InputModality(..)
    , inputModalityText
    , ModelVisibility(..)
    , modelVisibilityText
    , ShellToolType(..)
    , shellToolTypeText
    , ApplyPatchToolType(..)
    , applyPatchToolTypeText
    , WebSearchToolType(..)
    , webSearchToolTypeText
    , TruncationMode(..)
    , truncationModeText
    , ToolMode(..)
    , toolModeText
    , MultiAgentVersion(..)
    , multiAgentVersionText
    , TruncationPolicy(..)
    , ModelServiceTier(..)
    , ModelAvailabilityNux(..)
    , ModelInfoUpgrade(..)
    , ModelUpgrade(..)
    , ModelInstructionsVariables(..)
    , ApprovalMessages(..)
    , CollaborationModeMessages(..)
    , AutoReviewMessages(..)
    , PermissionMessages(..)
    , MultiAgentMessages(..)
    , MultiAgentRoleMessages(..)
    , MultiAgentModeMessages(..)
    , ModelTokenBudgetConfig(..)
    , ModelMessages(..)
    , ModelPersonality(..)
    , ModelInfo(..)
    , ModelsResponse(..)
    , modelsResponseDecoder
    , modelInfoDecoder
    , ModelPreset(..)
    , defaultInputModalities
    , resolvedContextWindow
    , modelAutoCompactTokenLimit
    , modelEffectiveContextWindow
    , modelSupportsPersonality
    , renderModelInstructions
    , modelSupportsReasoningEffort
    , modelSupportsServiceTier
    , modelServiceTierForRequest
    , modelPresetFromInfo
    , modelPresetSupportsFastMode
    , availableModelPresets
    , defaultModelSlug
    , findModelInfo
    , modelInfoForSlug
    , fallbackModelInfo
    , fallbackModelInstructions
    , mergeModelCatalogs
    ) where

import Control.Applicative ((<|>))
import qualified Agent.Json.Decode as Json
import Agent.OpenAI.Models.Types.Enums
import Control.Monad (join)
import Data.Aeson
    ( ToJSON(..)
    , object
    , (.=)
    )
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (Pair)
import Data.List (find, sortOn)
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Language.Haskell.TH.Syntax
    ( lift
    , makeRelativeToProject
    , qAddDependentFile
    , runIO
    )

data ModelInfoUpgrade = ModelInfoUpgrade
    { model :: !Text
    , migrationMarkdown :: !Text
    , retirementAt :: !(Maybe Text)
    } deriving (Eq, Show)

instance ToJSON ModelInfoUpgrade where
    toJSON upgrade = object $
        [ "model" .= upgrade.model
        , "migration_markdown" .= upgrade.migrationMarkdown
        ] <> maybeField "retirement_at" upgrade.retirementAt

data ModelUpgrade = ModelUpgrade
    { upgradeId :: !Text
    , migrationConfigKey :: !Text
    , modelLink :: !(Maybe Text)
    , upgradeCopy :: !(Maybe Text)
    , migrationMarkdown :: !(Maybe Text)
    , retirementAt :: !(Maybe Text)
    } deriving (Eq, Show)

instance ToJSON ModelUpgrade where
    toJSON upgrade = object $
        [ "id" .= upgrade.upgradeId
        , "migration_config_key" .= upgrade.migrationConfigKey
        ]
        <> maybeField "model_link" upgrade.modelLink
        <> maybeField "upgrade_copy" upgrade.upgradeCopy
        <> maybeField "migration_markdown" upgrade.migrationMarkdown
        <> maybeField "retirement_at" upgrade.retirementAt

data ModelInstructionsVariables = ModelInstructionsVariables
    { personalityDefault :: !(Maybe Text)
    , personalityFriendly :: !(Maybe Text)
    , personalityPragmatic :: !(Maybe Text)
    } deriving (Eq, Show)

instance ToJSON ModelInstructionsVariables where
    toJSON variables = object $
        maybeField "personality_default" variables.personalityDefault
        <> maybeField "personality_friendly" variables.personalityFriendly
        <> maybeField "personality_pragmatic" variables.personalityPragmatic

data ApprovalMessages = ApprovalMessages
    { onRequest :: !(Maybe Text)
    , onRequestAutoReview :: !(Maybe Text)
    , never :: !(Maybe Text)
    , unlessTrusted :: !(Maybe Text)
    } deriving (Eq, Show)

instance ToJSON ApprovalMessages where
    toJSON messages = object $
        maybeField "on_request" messages.onRequest
        <> maybeField "on_request_auto_review" messages.onRequestAutoReview
        <> maybeField "never" messages.never
        <> maybeField "unless_trusted" messages.unlessTrusted

data CollaborationModeMessages = CollaborationModeMessages
    { defaultMessage :: !(Maybe Text)
    , planMessage :: !(Maybe Text)
    } deriving (Eq, Show)

instance ToJSON CollaborationModeMessages where
    toJSON messages = object $
        maybeField "default" messages.defaultMessage
        <> maybeField "plan" messages.planMessage

data AutoReviewMessages = AutoReviewMessages
    { policy :: !(Maybe Text)
    , policyTemplate :: !(Maybe Text)
    , rejectionInstructions :: !(Maybe Text)
    , timeoutInstructions :: !(Maybe Text)
    } deriving (Eq, Show)

instance ToJSON AutoReviewMessages where
    toJSON messages = object $
        maybeField "policy" messages.policy
        <> maybeField "policy_template" messages.policyTemplate
        <> maybeField "rejection_instructions" messages.rejectionInstructions
        <> maybeField "timeout_instructions" messages.timeoutInstructions

data PermissionMessages = PermissionMessages
    { dangerFullAccess :: !(Maybe Text)
    , workspaceWrite :: !(Maybe Text)
    , readOnly :: !(Maybe Text)
    } deriving (Eq, Show)

instance ToJSON PermissionMessages where
    toJSON messages = object $
        maybeField "danger_full_access" messages.dangerFullAccess
        <> maybeField "workspace_write" messages.workspaceWrite
        <> maybeField "read_only" messages.readOnly

data MultiAgentMessages = MultiAgentMessages
    { role :: !(Maybe MultiAgentRoleMessages)
    , mode :: !(Maybe MultiAgentModeMessages)
    } deriving (Eq, Show)

instance ToJSON MultiAgentMessages where
    toJSON messages = object $
        maybeField "role" messages.role
        <> maybeField "mode" messages.mode

data MultiAgentRoleMessages = MultiAgentRoleMessages
    { root :: !(Maybe Text)
    , subagent :: !(Maybe Text)
    } deriving (Eq, Show)

instance ToJSON MultiAgentRoleMessages where
    toJSON messages = object $
        maybeField "root" messages.root
        <> maybeField "subagent" messages.subagent

data MultiAgentModeMessages = MultiAgentModeMessages
    { explicit :: !(Maybe Text)
    , hintText :: !(Maybe Text)
    } deriving (Eq, Show)

instance ToJSON MultiAgentModeMessages where
    toJSON messages = object $
        maybeField "explicit" messages.explicit
        <> maybeField "hint_text" messages.hintText

data ModelTokenBudgetConfig = ModelTokenBudgetConfig
    { reminderThresholdTokens :: !Int
    , reminderMessageTemplate :: !Text
    , guidanceMessage :: !Text
    , autoCompactFallbackPrompt :: !Text
    , autoCompactFallbackBufferTokens :: !Int
    } deriving (Eq, Show)

instance ToJSON ModelTokenBudgetConfig where
    toJSON config = object
        [ "reminder_threshold_tokens" .= config.reminderThresholdTokens
        , "reminder_message_template" .= config.reminderMessageTemplate
        , "guidance_message" .= config.guidanceMessage
        , "auto_compact_fallback_prompt" .= config.autoCompactFallbackPrompt
        , "auto_compact_fallback_buffer_tokens" .= config.autoCompactFallbackBufferTokens
        ]

data ModelMessages = ModelMessages
    { instructionsTemplate :: !(Maybe Text)
    , instructionsVariables :: !(Maybe ModelInstructionsVariables)
    , approvals :: !(Maybe ApprovalMessages)
    , collaborationModes :: !(Maybe CollaborationModeMessages)
    , autoReview :: !(Maybe AutoReviewMessages)
    , permissions :: !(Maybe PermissionMessages)
    , multiAgent :: !(Maybe MultiAgentMessages)
    , tokenBudget :: !(Maybe ModelTokenBudgetConfig)
    } deriving (Eq, Show)

instance ToJSON ModelMessages where
    toJSON messages = object $
        maybeField "instructions_template" messages.instructionsTemplate
        <> maybeField "instructions_variables" messages.instructionsVariables
        <> maybeField "approvals" messages.approvals
        <> maybeField "collaboration_modes" messages.collaborationModes
        <> maybeField "auto_review" messages.autoReview
        <> maybeField "permissions" messages.permissions
        <> maybeField "multi_agent" messages.multiAgent
        <> maybeField "token_budget" messages.tokenBudget

data ModelPersonality
    = ModelPersonalityDefault
    | ModelPersonalityFriendly
    | ModelPersonalityPragmatic
    | ModelPersonalityNone
    deriving (Eq, Ord, Show)

data ModelInfo = ModelInfo
    { slug :: !Text
    , displayName :: !Text
    , description :: !(Maybe Text)
    , preferWebSockets :: !Bool
    , supportVerbosity :: !Bool
    , defaultVerbosity :: !(Maybe Verbosity)
    , applyPatchToolType :: !(Maybe ApplyPatchToolType)
    , webSearchToolType :: !WebSearchToolType
    , inputModalities :: ![InputModality]
    , supportsImageDetailOriginal :: !Bool
    , truncationPolicy :: !TruncationPolicy
    , supportsParallelToolCalls :: !Bool
    , toolMode :: !(Maybe ToolMode)
    , multiAgentVersion :: !(Maybe MultiAgentVersion)
    , useResponsesLite :: !Bool
    , includeSkillsUsageInstructions :: !Bool
    , includeAppsUsageInstructions :: !Bool
    , includePluginUsageInstructions :: !Bool
    , nodeReplAutoReviewRequired :: !Bool
    , nodeReplDisabled :: !Bool
    , autoReviewModelOverride :: !(Maybe Text)
    , modelSpecialty :: !(Maybe Text)
    , contextWindow :: !(Maybe Int)
    , maxContextWindow :: !(Maybe Int)
    , autoCompactTokenLimit :: !(Maybe Int)
    , compHash :: !(Maybe Text)
    , effectiveContextWindowPercent :: !Int
    , defaultReasoningSummary :: !ReasoningSummary
    , defaultReasoningLevel :: !(Maybe ReasoningEffort)
    , supportedReasoningLevels :: ![ReasoningEffortPreset]
    , shellType :: !ShellToolType
    , visibility :: !ModelVisibility
    , supportedInApi :: !Bool
    , availabilityNux :: !(Maybe ModelAvailabilityNux)
    , upgrade :: !(Maybe ModelInfoUpgrade)
    , priority :: !Int
    , modelMessages :: !(Maybe ModelMessages)
    , experimentalSupportedTools :: ![Text]
    , availableInPlans :: ![Text]
    , supportsSearchTool :: !Bool
    , defaultServiceTier :: !(Maybe Text)
    , serviceTiers :: ![ModelServiceTier]
    , additionalSpeedTiers :: ![Text]
    , supportsReasoningSummaryParameter :: !Bool
    , supportsReasoningSummaries :: !Bool
    , baseInstructions :: !(Maybe Text)
    , usedFallbackModelMetadata :: !Bool
    } deriving (Eq, Show)

instance ToJSON ModelInfo where
    toJSON info = object $
        modelInfoFields info
        <> maybeField "base_instructions" info.baseInstructions

data ModelsResponse = ModelsResponse
    { models :: ![ModelInfo]
    , catalogGeneration :: !(Maybe Scientific)
    } deriving (Eq, Show)

-- | Decode the Codex model catalog directly with Hermes. Unknown fields are
-- intentionally ignored; Aeson remains responsible only for encoding.
modelsResponseDecoder :: Json.Decoder ModelsResponse
modelsResponseDecoder = Json.object do
    models <- Json.atKey "models" (Json.list modelInfoDecoder)
    catalogGeneration <-
        optionalField "catalog_generation" Json.scientific
    pure ModelsResponse
        { models
        , catalogGeneration
        }

modelInfoDecoder :: Json.Decoder ModelInfo
modelInfoDecoder = Json.object do
    slug <- Json.atKey "slug" Json.text
    displayName <- fieldDefault "display_name" Json.text slug
    description <- optionalField "description" Json.text
    preferWebSockets <- fieldDefault "prefer_websockets" Json.bool False
    supportVerbosity <- fieldDefault "support_verbosity" Json.bool False
    defaultVerbosity <- optionalField "default_verbosity" verbosityDecoder
    applyPatchToolType <-
        optionalField "apply_patch_tool_type" applyPatchToolTypeDecoder
    webSearchToolType <-
        fieldDefault "web_search_tool_type" webSearchToolTypeDecoder WebSearchText
    inputModalities <-
        fieldDefault "input_modalities"
            (Json.list inputModalityDecoder) defaultInputModalities
    supportsImageDetailOriginal <-
        fieldDefault "supports_image_detail_original" Json.bool False
    truncationPolicy <-
        fieldDefault "truncation_policy" truncationPolicyDecoder
            (TruncationPolicy TruncationBytes 10_000)
    supportsParallelToolCalls <-
        fieldDefault "supports_parallel_tool_calls" Json.bool False
    toolMode <- optionalField "tool_mode" toolModeDecoder
    multiAgentVersion <-
        optionalField "multi_agent_version" multiAgentVersionDecoder
    useResponsesLite <- fieldDefault "use_responses_lite" Json.bool False
    includeSkillsUsageInstructions <-
        fieldDefault "include_skills_usage_instructions" Json.bool False
    includeAppsUsageInstructions <-
        fieldDefault "include_apps_usage_instructions" Json.bool True
    includePluginUsageInstructions <-
        fieldDefault "include_plugin_usage_instructions" Json.bool False
    nodeReplAutoReviewRequired <-
        fieldDefault "node_repl_auto_review_required" Json.bool False
    nodeReplDisabled <- fieldDefault "node_repl_disabled" Json.bool False
    autoReviewModelOverride <-
        optionalField "auto_review_model_override" Json.text
    modelSpecialty <- optionalField "model_specialty" Json.text
    contextWindow <- optionalField "context_window" Json.int
    maxContextWindow <- optionalField "max_context_window" Json.int
    autoCompactTokenLimit <-
        optionalField "auto_compact_token_limit" Json.int
    compHash <- optionalField "comp_hash" Json.text
    effectiveContextWindowPercent <-
        fieldDefault "effective_context_window_percent" Json.int 95
    defaultReasoningSummary <-
        fieldDefault "default_reasoning_summary"
            reasoningSummaryDecoder ReasoningSummaryAuto
    defaultReasoningLevel <-
        optionalField "default_reasoning_level" reasoningEffortDecoder
    supportedReasoningLevels <-
        fieldDefault "supported_reasoning_levels"
            (Json.list reasoningEffortPresetDecoder) []
    shellType <-
        fieldDefault "shell_type" shellToolTypeDecoder ShellToolUnifiedExec
    visibility <-
        fieldDefault "visibility" modelVisibilityDecoder ModelVisibilityNone
    supportedInApi <- fieldDefault "supported_in_api" Json.bool True
    availabilityNux <-
        optionalField "availability_nux" modelAvailabilityNuxDecoder
    upgrade <- optionalField "upgrade" modelInfoUpgradeDecoder
    priority <- fieldDefault "priority" Json.int 99
    parsedMessages <- optionalField "model_messages" modelMessagesDecoder
    experimentalSupportedTools <-
        fieldDefault "experimental_supported_tools" (Json.list Json.text) []
    availableInPlans <-
        fieldDefault "available_in_plans" (Json.list Json.text) []
    supportsSearchTool <-
        fieldDefault "supports_search_tool" Json.bool False
    defaultServiceTier <- optionalField "default_service_tier" Json.text
    serviceTiers <-
        fieldDefault "service_tiers" (Json.list modelServiceTierDecoder) []
    additionalSpeedTiers <-
        fieldDefault "additional_speed_tiers" (Json.list Json.text) []
    supportsReasoningSummaryParameter <-
        fieldDefault "supports_reasoning_summary_parameter" Json.bool True
    supportsReasoningSummaries <-
        fieldDefault "supports_reasoning_summaries" Json.bool False
    legacyBaseInstructions <- optionalField "base_instructions" Json.text
    let modelMessages =
            promoteLegacyInstructions legacyBaseInstructions parsedMessages
        baseInstructions = Nothing
    case modelMessages >>= (.instructionsTemplate) of
        Nothing -> fail
            ("model `" <> Text.unpack slug
                <> "` is missing both base_instructions and "
                <> "model_messages.instructions_template")
        Just _ -> pure ()
    pure ModelInfo
        { usedFallbackModelMetadata = False
        , ..
        }

reasoningEffortDecoder :: Json.Decoder ReasoningEffort
reasoningEffortDecoder = nonEmptyTextEnum "reasoning_effort" \case
    "none" -> ReasoningEffortNone
    "minimal" -> ReasoningEffortMinimal
    "low" -> ReasoningEffortLow
    "medium" -> ReasoningEffortMedium
    "high" -> ReasoningEffortHigh
    "xhigh" -> ReasoningEffortXHigh
    "max" -> ReasoningEffortMax
    "ultra" -> ReasoningEffortUltra
    value -> ReasoningEffortOther value

reasoningSummaryDecoder :: Json.Decoder ReasoningSummary
reasoningSummaryDecoder = textEnum \case
    "none" -> ReasoningSummaryNone
    "auto" -> ReasoningSummaryAuto
    "concise" -> ReasoningSummaryConcise
    "detailed" -> ReasoningSummaryDetailed
    value -> ReasoningSummaryOther value

verbosityDecoder :: Json.Decoder Verbosity
verbosityDecoder = textEnum \case
    "low" -> VerbosityLow
    "medium" -> VerbosityMedium
    "high" -> VerbosityHigh
    value -> VerbosityOther value

inputModalityDecoder :: Json.Decoder InputModality
inputModalityDecoder = textEnum \case
    "text" -> InputModalityText
    "image" -> InputModalityImage
    "audio" -> InputModalityAudio
    value -> InputModalityOther value

modelVisibilityDecoder :: Json.Decoder ModelVisibility
modelVisibilityDecoder = textEnum \case
    "list" -> ModelVisibilityList
    "hide" -> ModelVisibilityHide
    "none" -> ModelVisibilityNone
    value -> ModelVisibilityOther value

shellToolTypeDecoder :: Json.Decoder ShellToolType
shellToolTypeDecoder = textEnum \case
    "unified_exec" -> ShellToolUnifiedExec
    "default" -> ShellToolUnifiedExec
    "local" -> ShellToolUnifiedExec
    "shell_command" -> ShellToolUnifiedExec
    "disabled" -> ShellToolDisabled
    value -> ShellToolOther value

applyPatchToolTypeDecoder :: Json.Decoder ApplyPatchToolType
applyPatchToolTypeDecoder = textEnum \case
    "freeform" -> ApplyPatchFreeform
    value -> ApplyPatchOther value

webSearchToolTypeDecoder :: Json.Decoder WebSearchToolType
webSearchToolTypeDecoder = textEnum \case
    "text" -> WebSearchText
    "text_and_image" -> WebSearchTextAndImage
    value -> WebSearchOther value

truncationModeDecoder :: Json.Decoder TruncationMode
truncationModeDecoder = textEnum \case
    "bytes" -> TruncationBytes
    "tokens" -> TruncationTokens
    value -> TruncationOther value

toolModeDecoder :: Json.Decoder ToolMode
toolModeDecoder = textEnum \case
    "direct" -> ToolModeDirect
    "code_mode" -> ToolModeCode
    "code_mode_only" -> ToolModeCodeOnly
    value -> ToolModeOther value

multiAgentVersionDecoder :: Json.Decoder MultiAgentVersion
multiAgentVersionDecoder = textEnum \case
    "disabled" -> MultiAgentDisabled
    "v1" -> MultiAgentV1
    "v2" -> MultiAgentV2
    value -> MultiAgentVersionOther value

reasoningEffortPresetDecoder :: Json.Decoder ReasoningEffortPreset
reasoningEffortPresetDecoder = Json.object $
    ReasoningEffortPreset
        <$> fieldDefault "effort" reasoningEffortDecoder ReasoningEffortNone
        <*> fieldDefault "description" Json.text ""

truncationPolicyDecoder :: Json.Decoder TruncationPolicy
truncationPolicyDecoder = Json.object $
    TruncationPolicy
        <$> fieldDefault "mode" truncationModeDecoder TruncationBytes
        <*> fieldDefault "limit" Json.int 10_000

modelServiceTierDecoder :: Json.Decoder ModelServiceTier
modelServiceTierDecoder = Json.object $
    ModelServiceTier
        <$> fieldDefault "id" Json.text ""
        <*> fieldDefault "name" Json.text ""
        <*> fieldDefault "description" Json.text ""

modelAvailabilityNuxDecoder :: Json.Decoder ModelAvailabilityNux
modelAvailabilityNuxDecoder =
    Json.object $ ModelAvailabilityNux <$> fieldDefault "message" Json.text ""

modelInfoUpgradeDecoder :: Json.Decoder ModelInfoUpgrade
modelInfoUpgradeDecoder = Json.object $
    ModelInfoUpgrade
        <$> fieldDefault "model" Json.text ""
        <*> fieldDefault "migration_markdown" Json.text ""
        <*> optionalField "retirement_at" Json.text

modelInstructionsVariablesDecoder :: Json.Decoder ModelInstructionsVariables
modelInstructionsVariablesDecoder = Json.object $
    ModelInstructionsVariables
        <$> optionalField "personality_default" Json.text
        <*> optionalField "personality_friendly" Json.text
        <*> optionalField "personality_pragmatic" Json.text

approvalMessagesDecoder :: Json.Decoder ApprovalMessages
approvalMessagesDecoder = Json.object $
    ApprovalMessages
        <$> optionalField "on_request" Json.text
        <*> optionalField "on_request_auto_review" Json.text
        <*> optionalField "never" Json.text
        <*> optionalField "unless_trusted" Json.text

collaborationModeMessagesDecoder :: Json.Decoder CollaborationModeMessages
collaborationModeMessagesDecoder = Json.object $
    CollaborationModeMessages
        <$> optionalField "default" Json.text
        <*> optionalField "plan" Json.text

autoReviewMessagesDecoder :: Json.Decoder AutoReviewMessages
autoReviewMessagesDecoder = Json.object $
    AutoReviewMessages
        <$> optionalField "policy" Json.text
        <*> optionalField "policy_template" Json.text
        <*> optionalField "rejection_instructions" Json.text
        <*> optionalField "timeout_instructions" Json.text

permissionMessagesDecoder :: Json.Decoder PermissionMessages
permissionMessagesDecoder = Json.object $
    PermissionMessages
        <$> optionalField "danger_full_access" Json.text
        <*> optionalField "workspace_write" Json.text
        <*> optionalField "read_only" Json.text

multiAgentMessagesDecoder :: Json.Decoder MultiAgentMessages
multiAgentMessagesDecoder = Json.object $
    MultiAgentMessages
        <$> optionalField "role" multiAgentRoleMessagesDecoder
        <*> optionalField "mode" multiAgentModeMessagesDecoder

multiAgentRoleMessagesDecoder :: Json.Decoder MultiAgentRoleMessages
multiAgentRoleMessagesDecoder = Json.object $
    MultiAgentRoleMessages
        <$> optionalField "root" Json.text
        <*> optionalField "subagent" Json.text

multiAgentModeMessagesDecoder :: Json.Decoder MultiAgentModeMessages
multiAgentModeMessagesDecoder = Json.object $
    MultiAgentModeMessages
        <$> optionalField "explicit" Json.text
        <*> optionalField "hint_text" Json.text

modelTokenBudgetConfigDecoder :: Json.Decoder ModelTokenBudgetConfig
modelTokenBudgetConfigDecoder = Json.object $
    ModelTokenBudgetConfig
        <$> fieldDefault "reminder_threshold_tokens" Json.int 0
        <*> fieldDefault "reminder_message_template" Json.text ""
        <*> fieldDefault "guidance_message" Json.text ""
        <*> fieldDefault "auto_compact_fallback_prompt" Json.text ""
        <*> fieldDefault "auto_compact_fallback_buffer_tokens" Json.int 0

modelMessagesDecoder :: Json.Decoder ModelMessages
modelMessagesDecoder = Json.object do
    instructionsTemplate <- optionalField "instructions_template" Json.text
    instructionsVariables <-
        optionalField "instructions_variables" modelInstructionsVariablesDecoder
    approvals <- optionalField "approvals" approvalMessagesDecoder
    collaborationModes <-
        optionalField "collaboration_modes" collaborationModeMessagesDecoder
    autoReview <- optionalField "auto_review" autoReviewMessagesDecoder
    permissions <- optionalField "permissions" permissionMessagesDecoder
    multiAgent <- optionalField "multi_agent" multiAgentMessagesDecoder
    tokenBudget <- optionalField "token_budget" modelTokenBudgetConfigDecoder
    pure ModelMessages
        { .. }

optionalField
    :: Text
    -> Json.Decoder value
    -> Json.FieldsDecoder (Maybe value)
optionalField key decoder =
    join <$> Json.atKeyOptional key (Json.nullable decoder)

fieldDefault
    :: Text
    -> Json.Decoder value
    -> value
    -> Json.FieldsDecoder value
fieldDefault key decoder fallback =
    fromMaybe fallback <$> optionalField key decoder

textEnum :: (Text -> value) -> Json.Decoder value
textEnum constructor =
    constructor <$> Json.text

nonEmptyTextEnum :: String -> (Text -> value) -> Json.Decoder value
nonEmptyTextEnum label constructor = Json.withText \value ->
    if Text.null value
        then fail (label <> " must not be empty")
        else pure (constructor value)

instance ToJSON ModelsResponse where
    toJSON response = object $
        [ "models" .= map ModelInfoWithLegacyBaseInstructions response.models ]
        <> maybeField "catalog_generation" response.catalogGeneration

data ModelPreset = ModelPreset
    { presetId :: !Text
    , model :: !Text
    , displayName :: !Text
    , description :: !Text
    , modelSpecialty :: !(Maybe Text)
    , defaultReasoningEffort :: !ReasoningEffort
    , supportedReasoningEfforts :: ![ReasoningEffortPreset]
    , supportsPersonality :: !Bool
    , additionalSpeedTiers :: ![Text]
    , serviceTiers :: ![ModelServiceTier]
    , defaultServiceTier :: !(Maybe Text)
    , isDefault :: !Bool
    , upgrade :: !(Maybe ModelUpgrade)
    , showInPicker :: !Bool
    , multiAgentVersion :: !(Maybe MultiAgentVersion)
    , availabilityNux :: !(Maybe ModelAvailabilityNux)
    , supportedInApi :: !Bool
    , inputModalities :: ![InputModality]
    } deriving (Eq, Show)

resolvedContextWindow :: ModelInfo -> Maybe Int
resolvedContextWindow info = info.contextWindow <|> info.maxContextWindow

modelAutoCompactTokenLimit :: ModelInfo -> Maybe Int
modelAutoCompactTokenLimit info =
    case resolvedContextWindow info of
        Nothing -> info.autoCompactTokenLimit
        Just contextLimit ->
            let maximumLimit = contextLimit * 9 `div` 10
            in Just (maybe maximumLimit (min maximumLimit) info.autoCompactTokenLimit)

modelEffectiveContextWindow :: ModelInfo -> Maybe Int
modelEffectiveContextWindow info =
    (\contextLimit ->
        contextLimit * info.effectiveContextWindowPercent `div` 100)
        <$> resolvedContextWindow info

modelSupportsPersonality :: ModelInfo -> Bool
modelSupportsPersonality info =
    case info.modelMessages of
        Just messages ->
            maybe False complete messages.instructionsVariables
                && maybe False (Text.isInfixOf personalityPlaceholder)
                    messages.instructionsTemplate
        Nothing -> False
  where
    complete variables =
        all (/= Nothing)
            [ variables.personalityDefault
            , variables.personalityFriendly
            , variables.personalityPragmatic
            ]

renderModelInstructions :: ModelPersonality -> ModelInfo -> Text
renderModelInstructions personality info =
    case info.modelMessages >>= (.instructionsTemplate) of
        Just template ->
            case info.modelMessages >>= (.instructionsVariables) of
                Nothing -> template
                Just variables ->
                    Text.replace
                        personalityPlaceholder
                        (selectedPersonality variables)
                        template
        Nothing -> fromMaybe "" info.baseInstructions
  where
    selectedPersonality variables =
        fromMaybe "" case personality of
            ModelPersonalityDefault -> variables.personalityDefault
            ModelPersonalityFriendly -> variables.personalityFriendly
            ModelPersonalityPragmatic -> variables.personalityPragmatic
            ModelPersonalityNone -> Just ""

modelSupportsReasoningEffort :: ModelInfo -> ReasoningEffort -> Bool
modelSupportsReasoningEffort info requested =
    any ((== requested) . (.effort)) info.supportedReasoningLevels

modelSupportsServiceTier :: ModelInfo -> Text -> Bool
modelSupportsServiceTier info requested =
    any ((== requested) . (.tierId)) info.serviceTiers

modelServiceTierForRequest :: ModelInfo -> Maybe Text -> Maybe Text
modelServiceTierForRequest info =
    (>>= \requested ->
        if requested /= "default" && modelSupportsServiceTier info requested
            then Just requested
            else Nothing)

modelPresetFromInfo :: ModelInfo -> ModelPreset
modelPresetFromInfo info = ModelPreset
    { presetId = info.slug
    , model = info.slug
    , displayName = info.displayName
    , description = fromMaybe "" info.description
    , modelSpecialty = info.modelSpecialty
    , defaultReasoningEffort =
        fromMaybe ReasoningEffortNone info.defaultReasoningLevel
    , supportedReasoningEfforts = info.supportedReasoningLevels
    , supportsPersonality = modelSupportsPersonality info
    , additionalSpeedTiers = info.additionalSpeedTiers
    , serviceTiers = info.serviceTiers
    , defaultServiceTier = info.defaultServiceTier
    , isDefault = False
    , upgrade = toPresetUpgrade info.slug <$> info.upgrade
    , showInPicker = info.visibility == ModelVisibilityList
    , multiAgentVersion = info.multiAgentVersion
    , availabilityNux = info.availabilityNux
    , supportedInApi = info.supportedInApi
    , inputModalities = info.inputModalities
    }

modelPresetSupportsFastMode :: ModelPreset -> Bool
modelPresetSupportsFastMode preset =
    any ((== "priority") . (.tierId)) preset.serviceTiers
        || "fast" `elem` preset.additionalSpeedTiers

availableModelPresets :: Bool -> ModelsResponse -> [ModelPreset]
availableModelPresets chatGptMode response =
    markDefault
        [ modelPresetFromInfo info
        | info <- sortOn (.priority) response.models
        , chatGptMode || info.supportedInApi
        ]
  where
    markDefault presets =
        case findIndexBy (.showInPicker) presets <|> firstIndex presets of
            Nothing -> presets
            Just selected ->
                [ preset { isDefault = index == selected }
                | (index, preset) <- zip [0 :: Int ..] presets
                ]

defaultModelSlug :: [ModelPreset] -> Maybe Text
defaultModelSlug presets =
    (.model) <$> (find (.isDefault) presets <|> firstMay presets)

findModelInfo :: Text -> ModelsResponse -> Maybe ModelInfo
findModelInfo requested response =
    longestPrefix requested response.models
        <|> let (namespace, suffix) = Text.breakOn "/" requested
            in if Text.null namespace
                    || Text.null suffix
                    || Text.isInfixOf "/" (Text.drop 1 suffix)
                    || not (Text.all namespaceCharacter namespace)
                then Nothing
                else longestPrefix (Text.drop 1 suffix) response.models
  where
    namespaceCharacter character =
        character == '-' || character == '_' || isAsciiAlphaNumeric character

modelInfoForSlug :: Text -> ModelsResponse -> ModelInfo
modelInfoForSlug requested response =
    case findModelInfo requested response of
        Just info -> info
            { slug = requested
            , usedFallbackModelMetadata = False
            }
        Nothing -> fallbackModelInfo requested

fallbackModelInfo :: Text -> ModelInfo
fallbackModelInfo requested = ModelInfo
    { slug = requested
    , displayName = requested
    , description = Nothing
    , preferWebSockets = False
    , supportVerbosity = False
    , defaultVerbosity = Nothing
    , applyPatchToolType = Nothing
    , webSearchToolType = WebSearchText
    , inputModalities = defaultInputModalities
    , supportsImageDetailOriginal = False
    , truncationPolicy = TruncationPolicy TruncationBytes 10_000
    , supportsParallelToolCalls = False
    , toolMode = Nothing
    , multiAgentVersion = Nothing
    , useResponsesLite = False
    , includeSkillsUsageInstructions = False
    , includeAppsUsageInstructions = False
    , includePluginUsageInstructions = False
    , nodeReplAutoReviewRequired = False
    , nodeReplDisabled = False
    , autoReviewModelOverride = Nothing
    , modelSpecialty = Nothing
    , contextWindow = Just 272_000
    , maxContextWindow = Just 272_000
    , autoCompactTokenLimit = Nothing
    , compHash = Nothing
    , effectiveContextWindowPercent = 95
    , defaultReasoningSummary = ReasoningSummaryAuto
    , defaultReasoningLevel = Nothing
    , supportedReasoningLevels = []
    , shellType = ShellToolUnifiedExec
    , visibility = ModelVisibilityNone
    , supportedInApi = True
    , availabilityNux = Nothing
    , upgrade = Nothing
    , priority = 99
    , modelMessages = Just (legacyModelMessages fallbackModelInstructions)
    , experimentalSupportedTools = []
    , availableInPlans = []
    , supportsSearchTool = False
    , defaultServiceTier = Nothing
    , serviceTiers = []
    , additionalSpeedTiers = []
    , supportsReasoningSummaryParameter = True
    , supportsReasoningSummaries = False
    , baseInstructions = Nothing
    , usedFallbackModelMetadata = True
    }

-- | The upstream Codex fallback instructions for unknown model slugs.
fallbackModelInstructions :: Text
fallbackModelInstructions =
    Text.pack
        $(do
            path <- makeRelativeToProject "data/prompt.md"
            qAddDependentFile path
            contents <- runIO (Prelude.readFile path)
            lift contents
         )

mergeModelCatalogs :: ModelsResponse -> ModelsResponse -> ModelsResponse
mergeModelCatalogs bundled remote = ModelsResponse
    { models = foldl' overlay bundled.models remote.models
    , catalogGeneration =
        remote.catalogGeneration <|> bundled.catalogGeneration
    }
  where
    overlay current replacement =
        if any ((== replacement.slug) . (.slug)) current
            then
                [ if existing.slug == replacement.slug
                    then replacement
                    else existing
                | existing <- current
                ]
            else current <> [replacement]

legacyModelMessages :: Text -> ModelMessages
legacyModelMessages instructions = ModelMessages
    { instructionsTemplate = Just instructions
    , instructionsVariables = Nothing
    , approvals = Nothing
    , collaborationModes = Nothing
    , autoReview = Nothing
    , permissions = Nothing
    , multiAgent = Nothing
    , tokenBudget = Nothing
    }

promoteLegacyInstructions
    :: Maybe Text
    -> Maybe ModelMessages
    -> Maybe ModelMessages
promoteLegacyInstructions legacy = \case
    Nothing -> legacyModelMessages <$> legacy
    Just messages ->
        Just messages
            { instructionsTemplate =
                messages.instructionsTemplate <|> legacy
            }

personalityPlaceholder :: Text
personalityPlaceholder = "{{ personality }}"

toPresetUpgrade :: Text -> ModelInfoUpgrade -> ModelUpgrade
toPresetUpgrade source upgrade = ModelUpgrade
    { upgradeId = upgrade.model
    , migrationConfigKey = source
    , modelLink = Nothing
    , upgradeCopy = Nothing
    , migrationMarkdown = Just upgrade.migrationMarkdown
    , retirementAt = upgrade.retirementAt
    }

-- | Upstream's ModelsResponse codec emits the deprecated top-level
-- @base_instructions@ field alongside every model, even when the canonical
-- @model_messages.instructions_template@ field is present. This is required
-- for older Codex clients that still read the legacy field.
newtype ModelInfoWithLegacyBaseInstructions =
    ModelInfoWithLegacyBaseInstructions ModelInfo

instance ToJSON ModelInfoWithLegacyBaseInstructions where
    toJSON (ModelInfoWithLegacyBaseInstructions info) =
        object $
            modelInfoFields info
            <> [ "base_instructions"
                 .= renderModelInstructions ModelPersonalityDefault info
               ]

modelInfoFields :: ModelInfo -> [Pair]
modelInfoFields info =
    [ "slug" .= info.slug
    , "display_name" .= info.displayName
    , "prefer_websockets" .= info.preferWebSockets
    , "support_verbosity" .= info.supportVerbosity
    , "web_search_tool_type" .= info.webSearchToolType
    , "input_modalities" .= info.inputModalities
    , "supports_image_detail_original" .= info.supportsImageDetailOriginal
    , "truncation_policy" .= info.truncationPolicy
    , "supports_parallel_tool_calls" .= info.supportsParallelToolCalls
    , "use_responses_lite" .= info.useResponsesLite
    , "include_skills_usage_instructions"
        .= info.includeSkillsUsageInstructions
    , "include_apps_usage_instructions" .= info.includeAppsUsageInstructions
    , "include_plugin_usage_instructions"
        .= info.includePluginUsageInstructions
    , "node_repl_auto_review_required" .= info.nodeReplAutoReviewRequired
    , "node_repl_disabled" .= info.nodeReplDisabled
    , "effective_context_window_percent"
        .= info.effectiveContextWindowPercent
    , "default_reasoning_summary" .= info.defaultReasoningSummary
    , "supported_reasoning_levels" .= info.supportedReasoningLevels
    , "shell_type" .= info.shellType
    , "visibility" .= info.visibility
    , "supported_in_api" .= info.supportedInApi
    , "priority" .= info.priority
    , "experimental_supported_tools" .= info.experimentalSupportedTools
    , "available_in_plans" .= info.availableInPlans
    , "supports_search_tool" .= info.supportsSearchTool
    , "service_tiers" .= info.serviceTiers
    , "additional_speed_tiers" .= info.additionalSpeedTiers
    , "supports_reasoning_summary_parameter"
        .= info.supportsReasoningSummaryParameter
    , "supports_reasoning_summaries" .= info.supportsReasoningSummaries
    ]
    <> maybeField "description" info.description
    <> maybeField "default_verbosity" info.defaultVerbosity
    <> maybeField "apply_patch_tool_type" info.applyPatchToolType
    <> maybeField "tool_mode" info.toolMode
    <> maybeField "multi_agent_version" info.multiAgentVersion
    <> maybeField "auto_review_model_override" info.autoReviewModelOverride
    <> maybeField "model_specialty" info.modelSpecialty
    <> maybeField "context_window" info.contextWindow
    <> maybeField "max_context_window" info.maxContextWindow
    <> maybeField "auto_compact_token_limit" info.autoCompactTokenLimit
    <> maybeField "comp_hash" info.compHash
    <> maybeField "default_reasoning_level" info.defaultReasoningLevel
    <> maybeField "availability_nux" info.availabilityNux
    <> maybeField "upgrade" info.upgrade
    <> maybeField "model_messages" info.modelMessages
    <> maybeField "default_service_tier" info.defaultServiceTier

maybeField :: ToJSON value => Text -> Maybe value -> [Pair]
maybeField name = maybe [] (\value -> [Key.fromText name .= value])

findIndexBy :: (value -> Bool) -> [value] -> Maybe Int
findIndexBy predicate = go 0
  where
    go _ [] = Nothing
    go index (value : rest)
        | predicate value = Just index
        | otherwise = go (index + 1) rest

firstIndex :: [value] -> Maybe Int
firstIndex [] = Nothing
firstIndex (_ : _) = Just 0

firstMay :: [value] -> Maybe value
firstMay [] = Nothing
firstMay (value : _) = Just value

longestPrefix :: Text -> [ModelInfo] -> Maybe ModelInfo
longestPrefix requested =
    foldl' choose Nothing
  where
    choose current candidate
        | not (candidate.slug `Text.isPrefixOf` requested) = current
        | otherwise = case current of
            Nothing -> Just candidate
            Just existing
                | Text.length candidate.slug > Text.length existing.slug ->
                    Just candidate
                | otherwise -> current

isAsciiAlphaNumeric :: Char -> Bool
isAsciiAlphaNumeric character =
    (character >= 'a' && character <= 'z')
        || (character >= 'A' && character <= 'Z')
        || (character >= '0' && character <= '9')
