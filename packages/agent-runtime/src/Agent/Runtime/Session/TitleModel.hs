-- | Cheap auxiliary model used for generated session titles.
module Agent.Runtime.Session.TitleModel
    ( TitleModelResolution(..)
    , TitleModelSetting(..)
    , appleFoundationTitleContextWindow
    , appleFoundationTitleModelId
    , cheapTitleModel
    , isAppleFoundationTitleModelName
    , resolveTitleModel
    , titleSourceCharBudget
    ) where

import Agent.Provider (Provider)
import Agent.Runtime.ModelConfig (ModelCatalog)
import Agent.Runtime.Models
    ( ModelOption(..)
    , ModelTarget(..)
    , defaultModelOptionFor
    , modelsForProvider
    , resolveConfiguredModel
    )
import Data.Text (Text)
import qualified Data.Text as Text

-- | Persisted title-model choice. 'Nothing' at the settings layer means auto.
data TitleModelSetting
    = TitleModelAppleFoundation
    | TitleModelPinned !ModelTarget
    deriving (Eq, Show)

-- | The model a title request should send, independent of the live session
-- model. A pinned provider model is used only while its provider matches the
-- session. On macOS, auto prefers on-device Apple Intelligence when available.
data TitleModelResolution = TitleModelResolution
    { titleModelId :: !Text
    , titleWireModelId :: !Text
    , titleContextWindow :: !(Maybe Int)
    , titleReasoningEffort :: !Text
    , titlePinned :: !Bool
    , titleUsesAppleFoundation :: !Bool
    }
    deriving (Eq, Show)

appleFoundationTitleModelId :: Text
appleFoundationTitleModelId = "apple-foundationmodel"

appleFoundationTitleContextWindow :: Int
appleFoundationTitleContextWindow = 4096

isAppleFoundationTitleModelName :: Text -> Bool
isAppleFoundationTitleModelName name =
    Text.toLower (Text.strip name)
        `elem` [appleFoundationTitleModelId, "apple-foundation-model", "apple", "apfel"]

-- | OpenAI's cheap title model. Token cost is low enough that a stronger
-- reasoning setting is worth using for a better name.
lunaTitleModelId :: Text
lunaTitleModelId = "gpt-5.6-luna"

-- | Conversation excerpt size that still fits a 4K-token on-device window
-- after the title prompt and completion. Larger catalog windows keep more
-- recent-task detail without approaching a full-session prompt.
titleSourceCharBudget :: TitleModelResolution -> Int
titleSourceCharBudget resolution =
    case resolution.titleContextWindow of
        Just window | window <= 4096 -> 2000
        Just window | window <= 8192 -> 3500
        _ -> 6000

cheapTitleModel :: ModelCatalog -> Provider -> ModelOption
cheapTitleModel catalog provider =
    case filter isCheapAuxiliary (modelsForProvider catalog provider) of
        option : _ -> option
        [] -> defaultModelOptionFor catalog provider

resolveTitleModel
    :: ModelCatalog
    -> Provider
    -> Maybe TitleModelSetting
    -> Bool
    -> TitleModelResolution
resolveTitleModel catalog provider setting appleAvailable =
    case setting of
        Just TitleModelAppleFoundation ->
            appleFoundationResolution True fallback
        Just (TitleModelPinned target)
            | target.targetProvider == provider ->
                resolutionFromPinned catalog target
        _
            | appleAvailable ->
                appleFoundationResolution False fallback
            | otherwise ->
                resolutionFromOption False fallback
  where
    fallback = cheapTitleModel catalog provider

-- | Apple Intelligence is a local Swift helper, not a provider wire model.
-- Keep the cheap same-provider model on the resolution so auto fallback can
-- call it without sending @apple-foundationmodel@ to Claude or OpenAI.
appleFoundationResolution :: Bool -> ModelOption -> TitleModelResolution
appleFoundationResolution pinned fallback =
    TitleModelResolution
        { titleModelId = appleFoundationTitleModelId
        , titleWireModelId = fallback.modelTarget.targetWireModelId
        , titleContextWindow = Just appleFoundationTitleContextWindow
        , titleReasoningEffort =
            titleReasoningEffortFor fallback.modelTarget.targetModelId
        , titlePinned = pinned
        , titleUsesAppleFoundation = True
        }

resolutionFromPinned :: ModelCatalog -> ModelTarget -> TitleModelResolution
resolutionFromPinned catalog target =
    case resolveConfiguredModel catalog target.targetModelId of
        Just option
            | option.modelTarget.targetProvider == target.targetProvider ->
                resolutionFromOption True option
        _ ->
            TitleModelResolution
                { titleModelId = target.targetModelId
                , titleWireModelId = target.targetWireModelId
                , titleContextWindow = Nothing
                , titleReasoningEffort =
                    titleReasoningEffortFor target.targetModelId
                , titlePinned = True
                , titleUsesAppleFoundation = False
                }

resolutionFromOption :: Bool -> ModelOption -> TitleModelResolution
resolutionFromOption pinned option =
    TitleModelResolution
        { titleModelId = option.modelTarget.targetModelId
        , titleWireModelId = option.modelTarget.targetWireModelId
        , titleContextWindow = option.modelContextWindow
        , titleReasoningEffort =
            titleReasoningEffortFor option.modelTarget.targetModelId
        , titlePinned = pinned
        , titleUsesAppleFoundation = False
        }

-- | Luna is cheap enough that high effort is worth it for a better name.
-- Claude Haiku stays on low so title jobs do not compete with the live
-- subscription turn.
titleReasoningEffortFor :: Text -> Text
titleReasoningEffortFor modelId
    | modelId == lunaTitleModelId = "high"
    | otherwise = "low"

isCheapAuxiliary :: ModelOption -> Bool
isCheapAuxiliary option =
    maybe False isCheapAuxiliaryLabel option.modelLabel

isCheapAuxiliaryLabel :: Text -> Bool
isCheapAuxiliaryLabel label =
    let lowered = Text.toLower label
    in any (`Text.isInfixOf` lowered) ["low cost", "free"]
