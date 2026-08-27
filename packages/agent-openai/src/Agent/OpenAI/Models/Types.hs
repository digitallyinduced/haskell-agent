{-# LANGUAGE TemplateHaskell #-}

-- | Forward-compatible metadata returned by the Codex @/models@ endpoint.
--
-- Unknown enum values are retained in @Other@ constructors and unknown model
-- fields are preserved in 'extraFields'.  This lets a newer service catalog be
-- cached and round-tripped by an older client without making model discovery
-- unusable.
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
import Data.Aeson
    ( FromJSON(..)
    , Object
    , ToJSON(..)
    , Value(..)
    , object
    , withObject
    , withText
    , (.:)
    , (.:?)
    , (.!=)
    , (.=)
    )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser)
import Data.List (find, sortOn)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Language.Haskell.TH.Syntax
    ( lift
    , makeRelativeToProject
    , qAddDependentFile
    , runIO
    )

data ReasoningEffort
    = ReasoningEffortNone
    | ReasoningEffortMinimal
    | ReasoningEffortLow
    | ReasoningEffortMedium
    | ReasoningEffortHigh
    | ReasoningEffortXHigh
    | ReasoningEffortMax
    | ReasoningEffortUltra
    | ReasoningEffortOther !Text
    deriving (Eq, Ord, Show)

reasoningEffortText :: ReasoningEffort -> Text
reasoningEffortText = \case
    ReasoningEffortNone -> "none"
    ReasoningEffortMinimal -> "minimal"
    ReasoningEffortLow -> "low"
    ReasoningEffortMedium -> "medium"
    ReasoningEffortHigh -> "high"
    ReasoningEffortXHigh -> "xhigh"
    ReasoningEffortMax -> "max"
    ReasoningEffortUltra -> "ultra"
    ReasoningEffortOther value -> value

instance ToJSON ReasoningEffort where
    toJSON = String . reasoningEffortText

instance FromJSON ReasoningEffort where
    parseJSON = withText "ReasoningEffort" \value ->
        if Text.null value
            then fail "reasoning_effort must not be empty"
            else pure case value of
                "none" -> ReasoningEffortNone
                "minimal" -> ReasoningEffortMinimal
                "low" -> ReasoningEffortLow
                "medium" -> ReasoningEffortMedium
                "high" -> ReasoningEffortHigh
                "xhigh" -> ReasoningEffortXHigh
                "max" -> ReasoningEffortMax
                "ultra" -> ReasoningEffortUltra
                unknown -> ReasoningEffortOther unknown

data ReasoningSummary
    = ReasoningSummaryNone
    | ReasoningSummaryAuto
    | ReasoningSummaryConcise
    | ReasoningSummaryDetailed
    | ReasoningSummaryOther !Text
    deriving (Eq, Ord, Show)

reasoningSummaryText :: ReasoningSummary -> Text
reasoningSummaryText = \case
    ReasoningSummaryNone -> "none"
    ReasoningSummaryAuto -> "auto"
    ReasoningSummaryConcise -> "concise"
    ReasoningSummaryDetailed -> "detailed"
    ReasoningSummaryOther value -> value

instance ToJSON ReasoningSummary where
    toJSON = String . reasoningSummaryText

instance FromJSON ReasoningSummary where
    parseJSON = parseTextEnum "ReasoningSummary" \case
        "none" -> ReasoningSummaryNone
        "auto" -> ReasoningSummaryAuto
        "concise" -> ReasoningSummaryConcise
        "detailed" -> ReasoningSummaryDetailed
        value -> ReasoningSummaryOther value

data Verbosity
    = VerbosityLow
    | VerbosityMedium
    | VerbosityHigh
    | VerbosityOther !Text
    deriving (Eq, Ord, Show)

verbosityText :: Verbosity -> Text
verbosityText = \case
    VerbosityLow -> "low"
    VerbosityMedium -> "medium"
    VerbosityHigh -> "high"
    VerbosityOther value -> value

instance ToJSON Verbosity where
    toJSON = String . verbosityText

instance FromJSON Verbosity where
    parseJSON = parseTextEnum "Verbosity" \case
        "low" -> VerbosityLow
        "medium" -> VerbosityMedium
        "high" -> VerbosityHigh
        value -> VerbosityOther value

data InputModality
    = InputModalityText
    | InputModalityImage
    | InputModalityAudio
    | InputModalityOther !Text
    deriving (Eq, Ord, Show)

inputModalityText :: InputModality -> Text
inputModalityText = \case
    InputModalityText -> "text"
    InputModalityImage -> "image"
    InputModalityAudio -> "audio"
    InputModalityOther value -> value

instance ToJSON InputModality where
    toJSON = String . inputModalityText

instance FromJSON InputModality where
    parseJSON = parseTextEnum "InputModality" \case
        "text" -> InputModalityText
        "image" -> InputModalityImage
        "audio" -> InputModalityAudio
        value -> InputModalityOther value

defaultInputModalities :: [InputModality]
defaultInputModalities = [InputModalityText, InputModalityImage]

data ModelVisibility
    = ModelVisibilityList
    | ModelVisibilityHide
    | ModelVisibilityNone
    | ModelVisibilityOther !Text
    deriving (Eq, Ord, Show)

modelVisibilityText :: ModelVisibility -> Text
modelVisibilityText = \case
    ModelVisibilityList -> "list"
    ModelVisibilityHide -> "hide"
    ModelVisibilityNone -> "none"
    ModelVisibilityOther value -> value

instance ToJSON ModelVisibility where
    toJSON = String . modelVisibilityText

instance FromJSON ModelVisibility where
    parseJSON = parseTextEnum "ModelVisibility" \case
        "list" -> ModelVisibilityList
        "hide" -> ModelVisibilityHide
        "none" -> ModelVisibilityNone
        value -> ModelVisibilityOther value

data ShellToolType
    = ShellToolUnifiedExec
    | ShellToolDisabled
    | ShellToolOther !Text
    deriving (Eq, Ord, Show)

shellToolTypeText :: ShellToolType -> Text
shellToolTypeText = \case
    ShellToolUnifiedExec -> "unified_exec"
    ShellToolDisabled -> "disabled"
    ShellToolOther value -> value

instance ToJSON ShellToolType where
    toJSON = String . shellToolTypeText

instance FromJSON ShellToolType where
    parseJSON = parseTextEnum "ShellToolType" \case
        "unified_exec" -> ShellToolUnifiedExec
        "default" -> ShellToolUnifiedExec
        "local" -> ShellToolUnifiedExec
        "shell_command" -> ShellToolUnifiedExec
        "disabled" -> ShellToolDisabled
        value -> ShellToolOther value

data ApplyPatchToolType
    = ApplyPatchFreeform
    | ApplyPatchOther !Text
    deriving (Eq, Ord, Show)

applyPatchToolTypeText :: ApplyPatchToolType -> Text
applyPatchToolTypeText = \case
    ApplyPatchFreeform -> "freeform"
    ApplyPatchOther value -> value

instance ToJSON ApplyPatchToolType where
    toJSON = String . applyPatchToolTypeText

instance FromJSON ApplyPatchToolType where
    parseJSON = parseTextEnum "ApplyPatchToolType" \case
        "freeform" -> ApplyPatchFreeform
        value -> ApplyPatchOther value

data WebSearchToolType
    = WebSearchText
    | WebSearchTextAndImage
    | WebSearchOther !Text
    deriving (Eq, Ord, Show)

webSearchToolTypeText :: WebSearchToolType -> Text
webSearchToolTypeText = \case
    WebSearchText -> "text"
    WebSearchTextAndImage -> "text_and_image"
    WebSearchOther value -> value

instance ToJSON WebSearchToolType where
    toJSON = String . webSearchToolTypeText

instance FromJSON WebSearchToolType where
    parseJSON = parseTextEnum "WebSearchToolType" \case
        "text" -> WebSearchText
        "text_and_image" -> WebSearchTextAndImage
        value -> WebSearchOther value

data TruncationMode
    = TruncationBytes
    | TruncationTokens
    | TruncationOther !Text
    deriving (Eq, Ord, Show)

truncationModeText :: TruncationMode -> Text
truncationModeText = \case
    TruncationBytes -> "bytes"
    TruncationTokens -> "tokens"
    TruncationOther value -> value

instance ToJSON TruncationMode where
    toJSON = String . truncationModeText

instance FromJSON TruncationMode where
    parseJSON = parseTextEnum "TruncationMode" \case
        "bytes" -> TruncationBytes
        "tokens" -> TruncationTokens
        value -> TruncationOther value

data ToolMode
    = ToolModeDirect
    | ToolModeCode
    | ToolModeCodeOnly
    | ToolModeOther !Text
    deriving (Eq, Ord, Show)

toolModeText :: ToolMode -> Text
toolModeText = \case
    ToolModeDirect -> "direct"
    ToolModeCode -> "code_mode"
    ToolModeCodeOnly -> "code_mode_only"
    ToolModeOther value -> value

instance ToJSON ToolMode where
    toJSON = String . toolModeText

instance FromJSON ToolMode where
    parseJSON = parseTextEnum "ToolMode" \case
        "direct" -> ToolModeDirect
        "code_mode" -> ToolModeCode
        "code_mode_only" -> ToolModeCodeOnly
        value -> ToolModeOther value

data MultiAgentVersion
    = MultiAgentDisabled
    | MultiAgentV1
    | MultiAgentV2
    | MultiAgentVersionOther !Text
    deriving (Eq, Ord, Show)

multiAgentVersionText :: MultiAgentVersion -> Text
multiAgentVersionText = \case
    MultiAgentDisabled -> "disabled"
    MultiAgentV1 -> "v1"
    MultiAgentV2 -> "v2"
    MultiAgentVersionOther value -> value

instance ToJSON MultiAgentVersion where
    toJSON = String . multiAgentVersionText

instance FromJSON MultiAgentVersion where
    parseJSON = parseTextEnum "MultiAgentVersion" \case
        "disabled" -> MultiAgentDisabled
        "v1" -> MultiAgentV1
        "v2" -> MultiAgentV2
        value -> MultiAgentVersionOther value

data ReasoningEffortPreset = ReasoningEffortPreset
    { effort :: !ReasoningEffort
    , description :: !Text
    } deriving (Eq, Show)

instance FromJSON ReasoningEffortPreset where
    parseJSON = withObject "ReasoningEffortPreset" \value ->
        ReasoningEffortPreset
            <$> value .:? "effort" .!= ReasoningEffortNone
            <*> value .:? "description" .!= ""

instance ToJSON ReasoningEffortPreset where
    toJSON preset = object
        [ "effort" .= preset.effort
        , "description" .= preset.description
        ]

data TruncationPolicy = TruncationPolicy
    { mode :: !TruncationMode
    , limit :: !Int
    } deriving (Eq, Show)

instance FromJSON TruncationPolicy where
    parseJSON = withObject "TruncationPolicy" \value ->
        TruncationPolicy
            <$> value .:? "mode" .!= TruncationBytes
            <*> value .:? "limit" .!= 10_000

instance ToJSON TruncationPolicy where
    toJSON policy = object
        [ "mode" .= policy.mode
        , "limit" .= policy.limit
        ]

data ModelServiceTier = ModelServiceTier
    { tierId :: !Text
    , name :: !Text
    , description :: !Text
    } deriving (Eq, Show)

instance FromJSON ModelServiceTier where
    parseJSON = withObject "ModelServiceTier" \value ->
        ModelServiceTier
            <$> value .:? "id" .!= ""
            <*> value .:? "name" .!= ""
            <*> value .:? "description" .!= ""

instance ToJSON ModelServiceTier where
    toJSON tier = object
        [ "id" .= tier.tierId
        , "name" .= tier.name
        , "description" .= tier.description
        ]

newtype ModelAvailabilityNux = ModelAvailabilityNux
    { message :: Text
    } deriving (Eq, Show)

instance FromJSON ModelAvailabilityNux where
    parseJSON = withObject "ModelAvailabilityNux" \value ->
        ModelAvailabilityNux <$> value .:? "message" .!= ""

instance ToJSON ModelAvailabilityNux where
    toJSON nux = object ["message" .= nux.message]

data ModelInfoUpgrade = ModelInfoUpgrade
    { model :: !Text
    , migrationMarkdown :: !Text
    , retirementAt :: !(Maybe Text)
    } deriving (Eq, Show)

instance FromJSON ModelInfoUpgrade where
    parseJSON = withObject "ModelInfoUpgrade" \value ->
        ModelInfoUpgrade
            <$> value .:? "model" .!= ""
            <*> value .:? "migration_markdown" .!= ""
            <*> value .:? "retirement_at"

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

instance FromJSON ModelUpgrade where
    parseJSON = withObject "ModelUpgrade" \value ->
        ModelUpgrade
            <$> value .:? "id" .!= ""
            <*> value .:? "migration_config_key" .!= ""
            <*> value .:? "model_link"
            <*> value .:? "upgrade_copy"
            <*> value .:? "migration_markdown"
            <*> value .:? "retirement_at"

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

instance FromJSON ModelInstructionsVariables where
    parseJSON = withObject "ModelInstructionsVariables" \value ->
        ModelInstructionsVariables
            <$> value .:? "personality_default"
            <*> value .:? "personality_friendly"
            <*> value .:? "personality_pragmatic"

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

instance FromJSON ApprovalMessages where
    parseJSON = withObject "ApprovalMessages" \value ->
        ApprovalMessages
            <$> value .:? "on_request"
            <*> value .:? "on_request_auto_review"
            <*> value .:? "never"
            <*> value .:? "unless_trusted"

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

instance FromJSON CollaborationModeMessages where
    parseJSON = withObject "CollaborationModeMessages" \value ->
        CollaborationModeMessages
            <$> value .:? "default"
            <*> value .:? "plan"

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

instance FromJSON AutoReviewMessages where
    parseJSON = withObject "AutoReviewMessages" \value ->
        AutoReviewMessages
            <$> value .:? "policy"
            <*> value .:? "policy_template"
            <*> value .:? "rejection_instructions"
            <*> value .:? "timeout_instructions"

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

instance FromJSON PermissionMessages where
    parseJSON = withObject "PermissionMessages" \value ->
        PermissionMessages
            <$> value .:? "danger_full_access"
            <*> value .:? "workspace_write"
            <*> value .:? "read_only"

instance ToJSON PermissionMessages where
    toJSON messages = object $
        maybeField "danger_full_access" messages.dangerFullAccess
        <> maybeField "workspace_write" messages.workspaceWrite
        <> maybeField "read_only" messages.readOnly

data MultiAgentMessages = MultiAgentMessages
    { role :: !(Maybe MultiAgentRoleMessages)
    , mode :: !(Maybe MultiAgentModeMessages)
    } deriving (Eq, Show)

instance FromJSON MultiAgentMessages where
    parseJSON = withObject "MultiAgentMessages" \value ->
        MultiAgentMessages
            <$> value .:? "role"
            <*> value .:? "mode"

instance ToJSON MultiAgentMessages where
    toJSON messages = object $
        maybeField "role" messages.role
        <> maybeField "mode" messages.mode

data MultiAgentRoleMessages = MultiAgentRoleMessages
    { root :: !(Maybe Text)
    , subagent :: !(Maybe Text)
    } deriving (Eq, Show)

instance FromJSON MultiAgentRoleMessages where
    parseJSON = withObject "MultiAgentRoleMessages" \value ->
        MultiAgentRoleMessages
            <$> value .:? "root"
            <*> value .:? "subagent"

instance ToJSON MultiAgentRoleMessages where
    toJSON messages = object $
        maybeField "root" messages.root
        <> maybeField "subagent" messages.subagent

data MultiAgentModeMessages = MultiAgentModeMessages
    { explicit :: !(Maybe Text)
    , hintText :: !(Maybe Text)
    } deriving (Eq, Show)

instance FromJSON MultiAgentModeMessages where
    parseJSON = withObject "MultiAgentModeMessages" \value ->
        MultiAgentModeMessages
            <$> value .:? "explicit"
            <*> value .:? "hint_text"

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

instance FromJSON ModelTokenBudgetConfig where
    parseJSON = withObject "ModelTokenBudgetConfig" \value ->
        ModelTokenBudgetConfig
            <$> value .:? "reminder_threshold_tokens" .!= 0
            <*> value .:? "reminder_message_template" .!= ""
            <*> value .:? "guidance_message" .!= ""
            <*> value .:? "auto_compact_fallback_prompt" .!= ""
            <*> value .:? "auto_compact_fallback_buffer_tokens" .!= 0

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
    , guardianV2 :: !(Maybe Value)
    , extraFields :: !Object
    } deriving (Eq, Show)

instance FromJSON ModelMessages where
    parseJSON = withObject "ModelMessages" \value ->
        ModelMessages
            <$> value .:? "instructions_template"
            <*> value .:? "instructions_variables"
            <*> value .:? "approvals"
            <*> value .:? "collaboration_modes"
            <*> value .:? "auto_review"
            <*> value .:? "permissions"
            <*> value .:? "multi_agent"
            <*> value .:? "token_budget"
            <*> value .:? "guardian_v2"
            <*> pure (removeFields modelMessageFieldNames value)

instance ToJSON ModelMessages where
    toJSON messages = objectWithExtra messages.extraFields $
        catMaybes
            [ jsonField "instructions_template" messages.instructionsTemplate
            , jsonField "instructions_variables" messages.instructionsVariables
            , jsonField "approvals" messages.approvals
            , jsonField "collaboration_modes" messages.collaborationModes
            , jsonField "auto_review" messages.autoReview
            , jsonField "permissions" messages.permissions
            , jsonField "multi_agent" messages.multiAgent
            , jsonField "token_budget" messages.tokenBudget
            , jsonField "guardian_v2" messages.guardianV2
            ]

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
    , minimalClientVersion :: !(Maybe Value)
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
    , extraFields :: !Object
    } deriving (Eq, Show)

instance FromJSON ModelInfo where
    parseJSON = withObject "ModelInfo" \value -> do
        slug <- value .: "slug"
        displayName <- value .:? "display_name" .!= slug
        description <- value .:? "description"
        preferWebSockets <- value .:? "prefer_websockets" .!= False
        supportVerbosity <- value .:? "support_verbosity" .!= False
        defaultVerbosity <- value .:? "default_verbosity"
        applyPatchToolType <- value .:? "apply_patch_tool_type"
        webSearchToolType <- value .:? "web_search_tool_type" .!= WebSearchText
        inputModalities <- value .:? "input_modalities" .!= defaultInputModalities
        supportsImageDetailOriginal <-
            value .:? "supports_image_detail_original" .!= False
        truncationPolicy <-
            value .:? "truncation_policy"
                .!= TruncationPolicy TruncationBytes 10_000
        supportsParallelToolCalls <-
            value .:? "supports_parallel_tool_calls" .!= False
        toolMode <- value .:? "tool_mode"
        multiAgentVersion <- value .:? "multi_agent_version"
        useResponsesLite <- value .:? "use_responses_lite" .!= False
        includeSkillsUsageInstructions <-
            value .:? "include_skills_usage_instructions" .!= False
        includeAppsUsageInstructions <-
            value .:? "include_apps_usage_instructions" .!= True
        includePluginUsageInstructions <-
            value .:? "include_plugin_usage_instructions" .!= False
        nodeReplAutoReviewRequired <-
            value .:? "node_repl_auto_review_required" .!= False
        nodeReplDisabled <- value .:? "node_repl_disabled" .!= False
        autoReviewModelOverride <- value .:? "auto_review_model_override"
        modelSpecialty <- value .:? "model_specialty"
        contextWindow <- value .:? "context_window"
        maxContextWindow <- value .:? "max_context_window"
        autoCompactTokenLimit <- value .:? "auto_compact_token_limit"
        compHash <- value .:? "comp_hash"
        effectiveContextWindowPercent <-
            value .:? "effective_context_window_percent" .!= 95
        defaultReasoningSummary <-
            value .:? "default_reasoning_summary" .!= ReasoningSummaryAuto
        defaultReasoningLevel <- value .:? "default_reasoning_level"
        supportedReasoningLevels <-
            value .:? "supported_reasoning_levels" .!= []
        shellType <- value .:? "shell_type" .!= ShellToolUnifiedExec
        visibility <- value .:? "visibility" .!= ModelVisibilityNone
        minimalClientVersion <- value .:? "minimal_client_version"
        supportedInApi <- value .:? "supported_in_api" .!= True
        availabilityNux <- value .:? "availability_nux"
        upgrade <- value .:? "upgrade"
        priority <- value .:? "priority" .!= 99
        parsedMessages <- value .:? "model_messages"
        experimentalSupportedTools <-
            value .:? "experimental_supported_tools" .!= []
        availableInPlans <- value .:? "available_in_plans" .!= []
        supportsSearchTool <- value .:? "supports_search_tool" .!= False
        defaultServiceTier <- value .:? "default_service_tier"
        serviceTiers <- value .:? "service_tiers" .!= []
        additionalSpeedTiers <- value .:? "additional_speed_tiers" .!= []
        supportsReasoningSummaryParameter <-
            value .:? "supports_reasoning_summary_parameter" .!= True
        supportsReasoningSummaries <-
            value .:? "supports_reasoning_summaries" .!= False
        legacyBaseInstructions <- value .:? "base_instructions"
        let modelMessages =
                promoteLegacyInstructions
                    legacyBaseInstructions
                    parsedMessages
            -- The canonical V2 representation owns the instruction template.
            -- Keep the legacy field only as an input compatibility shim; the
            -- ModelsResponse serializer below re-emits it for older clients.
            baseInstructions = Nothing
        case modelMessages >>= (.instructionsTemplate) of
            Nothing ->
                fail
                    ( "model `" <> Text.unpack slug
                        <> "` is missing both base_instructions and "
                        <> "model_messages.instructions_template"
                    )
            Just _ -> pure ()
        pure ModelInfo
            { usedFallbackModelMetadata = False
            , extraFields = removeFields modelInfoFieldNames value
            , ..
            }

instance ToJSON ModelInfo where
    toJSON info = objectWithExtra info.extraFields $
        [ ("slug", toJSON info.slug)
        , ("display_name", toJSON info.displayName)
        , ("prefer_websockets", toJSON info.preferWebSockets)
        , ("support_verbosity", toJSON info.supportVerbosity)
        , ("web_search_tool_type", toJSON info.webSearchToolType)
        , ("input_modalities", toJSON info.inputModalities)
        , ("supports_image_detail_original", toJSON info.supportsImageDetailOriginal)
        , ("truncation_policy", toJSON info.truncationPolicy)
        , ("supports_parallel_tool_calls", toJSON info.supportsParallelToolCalls)
        , ("use_responses_lite", toJSON info.useResponsesLite)
        , ("include_skills_usage_instructions", toJSON info.includeSkillsUsageInstructions)
        , ("include_apps_usage_instructions", toJSON info.includeAppsUsageInstructions)
        , ("include_plugin_usage_instructions", toJSON info.includePluginUsageInstructions)
        , ("node_repl_auto_review_required", toJSON info.nodeReplAutoReviewRequired)
        , ("node_repl_disabled", toJSON info.nodeReplDisabled)
        , ("effective_context_window_percent", toJSON info.effectiveContextWindowPercent)
        , ("default_reasoning_summary", toJSON info.defaultReasoningSummary)
        , ("supported_reasoning_levels", toJSON info.supportedReasoningLevels)
        , ("shell_type", toJSON info.shellType)
        , ("visibility", toJSON info.visibility)
        , ("supported_in_api", toJSON info.supportedInApi)
        , ("priority", toJSON info.priority)
        , ("experimental_supported_tools", toJSON info.experimentalSupportedTools)
        , ("available_in_plans", toJSON info.availableInPlans)
        , ("supports_search_tool", toJSON info.supportsSearchTool)
        , ("service_tiers", toJSON info.serviceTiers)
        , ("additional_speed_tiers", toJSON info.additionalSpeedTiers)
        , ("supports_reasoning_summary_parameter", toJSON info.supportsReasoningSummaryParameter)
        , ("supports_reasoning_summaries", toJSON info.supportsReasoningSummaries)
        ] <> catMaybes
            [ jsonField "description" info.description
            , jsonField "default_verbosity" info.defaultVerbosity
            , jsonField "apply_patch_tool_type" info.applyPatchToolType
            , jsonField "tool_mode" info.toolMode
            , jsonField "multi_agent_version" info.multiAgentVersion
            , jsonField "auto_review_model_override" info.autoReviewModelOverride
            , jsonField "model_specialty" info.modelSpecialty
            , jsonField "context_window" info.contextWindow
            , jsonField "max_context_window" info.maxContextWindow
            , jsonField "auto_compact_token_limit" info.autoCompactTokenLimit
            , jsonField "comp_hash" info.compHash
            , jsonField "default_reasoning_level" info.defaultReasoningLevel
            , jsonField "minimal_client_version" info.minimalClientVersion
            , jsonField "availability_nux" info.availabilityNux
            , jsonField "upgrade" info.upgrade
            , jsonField "model_messages" info.modelMessages
            , jsonField "default_service_tier" info.defaultServiceTier
            , jsonField "base_instructions" info.baseInstructions
            ]

data ModelsResponse = ModelsResponse
    { models :: ![ModelInfo]
    , extraFields :: !Object
    } deriving (Eq, Show)

instance FromJSON ModelsResponse where
    parseJSON = withObject "ModelsResponse" \value ->
        ModelsResponse
            <$> value .: "models"
            <*> pure (removeFields ["models"] value)

instance ToJSON ModelsResponse where
    toJSON response =
        objectWithExtra response.extraFields
            [ ( "models"
              , toJSON
                    (map modelInfoWithLegacyBaseInstructions response.models)
              )
            ]

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
    , minimalClientVersion = Nothing
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
    , extraFields = KeyMap.empty
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
    , extraFields = KeyMap.union remote.extraFields bundled.extraFields
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

parseTextEnum :: String -> (Text -> value) -> Value -> Parser value
parseTextEnum label constructor = withText label (pure . constructor)

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
    , guardianV2 = Nothing
    , extraFields = KeyMap.empty
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

modelMessageFieldNames :: [Text]
modelMessageFieldNames =
    [ "instructions_template"
    , "instructions_variables"
    , "approvals"
    , "collaboration_modes"
    , "auto_review"
    , "permissions"
    , "multi_agent"
    , "token_budget"
    , "guardian_v2"
    ]

modelInfoFieldNames :: [Text]
modelInfoFieldNames =
    [ "slug"
    , "display_name"
    , "description"
    , "prefer_websockets"
    , "support_verbosity"
    , "default_verbosity"
    , "apply_patch_tool_type"
    , "web_search_tool_type"
    , "input_modalities"
    , "supports_image_detail_original"
    , "truncation_policy"
    , "supports_parallel_tool_calls"
    , "tool_mode"
    , "multi_agent_version"
    , "use_responses_lite"
    , "include_skills_usage_instructions"
    , "include_apps_usage_instructions"
    , "include_plugin_usage_instructions"
    , "node_repl_auto_review_required"
    , "node_repl_disabled"
    , "auto_review_model_override"
    , "model_specialty"
    , "context_window"
    , "max_context_window"
    , "auto_compact_token_limit"
    , "comp_hash"
    , "effective_context_window_percent"
    , "default_reasoning_summary"
    , "default_reasoning_level"
    , "supported_reasoning_levels"
    , "shell_type"
    , "visibility"
    , "minimal_client_version"
    , "supported_in_api"
    , "availability_nux"
    , "upgrade"
    , "priority"
    , "model_messages"
    , "experimental_supported_tools"
    , "available_in_plans"
    , "supports_search_tool"
    , "default_service_tier"
    , "service_tiers"
    , "additional_speed_tiers"
    , "supports_reasoning_summary_parameter"
    , "supports_reasoning_summaries"
    , "base_instructions"
    ]

removeFields :: [Text] -> Object -> Object
removeFields fields value =
    foldl' (\current name -> KeyMap.delete (Key.fromText name) current) value fields

objectWithExtra :: Object -> [(Text, Value)] -> Value
objectWithExtra extras fields =
    Object $ foldl'
        (\current (name, value) ->
            KeyMap.insert (Key.fromText name) value current)
        extras
        fields

-- | Upstream's ModelsResponse codec emits the deprecated top-level
-- @base_instructions@ field alongside every model, even when the canonical
-- @model_messages.instructions_template@ field is present. This is required
-- for older Codex clients that still read the legacy field.
modelInfoWithLegacyBaseInstructions :: ModelInfo -> Value
modelInfoWithLegacyBaseInstructions info =
    case toJSON info of
        Object fields ->
            Object
                (KeyMap.insert
                    (Key.fromText "base_instructions")
                    (String
                        (renderModelInstructions
                            ModelPersonalityDefault
                            info))
                    fields)
        value -> value

jsonField :: ToJSON value => Text -> Maybe value -> Maybe (Text, Value)
jsonField name = fmap (name,) . fmap toJSON

maybeField :: ToJSON value => Text -> Maybe value -> [(Key.Key, Value)]
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
