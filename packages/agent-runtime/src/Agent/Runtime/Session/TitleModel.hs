-- | Cheap auxiliary model used for generated session titles.
module Agent.Runtime.Session.TitleModel
    ( TitleModelResolution(..)
    , cheapTitleModel
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

-- | The model a title request should send, independent of the live session
-- model. A pinned setting is used only while its provider matches the session;
-- otherwise the provider's cheap auxiliary model is selected.
data TitleModelResolution = TitleModelResolution
    { titleModelId :: !Text
    , titleWireModelId :: !Text
    , titleContextWindow :: !(Maybe Int)
    , titleReasoningEffort :: !Text
    , titlePinned :: !Bool
    }
    deriving (Eq, Show)

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
    -> Maybe ModelTarget
    -> TitleModelResolution
resolveTitleModel catalog provider pinned =
    case pinned of
        Just target | target.targetProvider == provider ->
            resolutionFromPinned catalog target
        _ ->
            resolutionFromOption False (cheapTitleModel catalog provider)

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
