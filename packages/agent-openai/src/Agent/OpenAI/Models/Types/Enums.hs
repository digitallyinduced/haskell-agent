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

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Text (Text)

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
    toJSON = toJSON . reasoningEffortText

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
    toJSON = toJSON . reasoningSummaryText

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
    toJSON = toJSON . verbosityText

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
    toJSON = toJSON . inputModalityText

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
    toJSON = toJSON . modelVisibilityText

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
    toJSON = toJSON . shellToolTypeText

data ApplyPatchToolType
    = ApplyPatchFreeform
    | ApplyPatchOther !Text
    deriving (Eq, Ord, Show)

applyPatchToolTypeText :: ApplyPatchToolType -> Text
applyPatchToolTypeText = \case
    ApplyPatchFreeform -> "freeform"
    ApplyPatchOther value -> value

instance ToJSON ApplyPatchToolType where
    toJSON = toJSON . applyPatchToolTypeText

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
    toJSON = toJSON . webSearchToolTypeText

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
    toJSON = toJSON . truncationModeText

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
    toJSON = toJSON . toolModeText

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
    toJSON = toJSON . multiAgentVersionText

data ReasoningEffortPreset = ReasoningEffortPreset
    { effort :: !ReasoningEffort
    , description :: !Text
    } deriving (Eq, Show)

instance ToJSON ReasoningEffortPreset where
    toJSON preset = object
        [ "effort" .= preset.effort
        , "description" .= preset.description
        ]

data TruncationPolicy = TruncationPolicy
    { mode :: !TruncationMode
    , limit :: !Int
    } deriving (Eq, Show)

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

instance ToJSON ModelServiceTier where
    toJSON tier = object
        [ "id" .= tier.tierId
        , "name" .= tier.name
        , "description" .= tier.description
        ]

newtype ModelAvailabilityNux = ModelAvailabilityNux
    { message :: Text
    } deriving (Eq, Show)

instance ToJSON ModelAvailabilityNux where
    toJSON nux = object ["message" .= nux.message]
