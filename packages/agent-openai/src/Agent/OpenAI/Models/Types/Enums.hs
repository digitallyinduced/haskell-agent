-- | Forward-compatible enumerations used by model metadata.
module Agent.OpenAI.Models.Types.Enums
    ( ReasoningEffort(..)
    , reasoningEffortText
    , ReasoningSummary(..)
    , reasoningSummaryText
    , Verbosity(..)
    , verbosityText
    , InputModality(..)
    , inputModalityText
    , defaultInputModalities
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
    , ReasoningEffortPreset(..)
    , TruncationPolicy(..)
    , ModelServiceTier(..)
    , ModelAvailabilityNux(..)
    ) where

import Data.Aeson
    ( FromJSON(..), ToJSON(..), Value(..), object, withObject, withText
    , (.:?), (.!=), (.=)
    )
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Aeson.Types (Parser)

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

parseTextEnum :: String -> (Text -> value) -> Value -> Parser value
parseTextEnum label constructor = withText label (pure . constructor)

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
