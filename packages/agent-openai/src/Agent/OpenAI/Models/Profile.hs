-- | Pure projections from OpenAI model metadata into shared runtime choices.
--
-- This module intentionally depends only on @agent-openai@ and @agent-core@.
-- CLI and other front ends can consume model-owned defaults without creating
-- a dependency from the provider package back into an application package.
module Agent.OpenAI.Models.Profile
    ( toolModeForInfo
    , defaultModelForCatalog
    , defaultReasoningEffortForInfo
    , defaultVerbosityForInfo
    , defaultReasoningSummaryForInfo
    , modelPresetForInfo
    , pickerModelPresets
    ) where

import Agent.OpenAI.Models.Types
    ( ModelInfo(..)
    , ModelPreset(..)
    , ModelsResponse
    , ReasoningEffort
    , ReasoningSummary
    , ToolMode(..)
    , Verbosity
    , availableModelPresets
    , defaultModelSlug
    , modelPresetFromInfo
    )
import qualified Agent.Tools.CodeMode.Tool as CodeMode
import Control.Applicative ((<|>))
import Data.Text (Text)

-- | Resolve a model-owned tool selector, retaining the caller's feature or
-- compatibility fallback when the catalog omits it or advertises a newer
-- selector this client does not yet understand.
toolModeForInfo
    :: CodeMode.ToolMode
    -> ModelInfo
    -> CodeMode.ToolMode
toolModeForInfo fallback info =
    case info.toolMode of
        Just ToolModeDirect -> CodeMode.ConventionalToolMode
        Just ToolModeCode -> CodeMode.CodeToolMode
        Just ToolModeCodeOnly -> CodeMode.CodeOnlyToolMode
        Just (ToolModeOther _) -> fallback
        Nothing -> fallback

-- | Resolve the catalog default for an authentication mode.
--
-- Picker-visible models are preferred.  Falling back to the complete
-- auth-filtered list preserves upstream behavior for a future catalog that
-- temporarily contains no visible model.
defaultModelForCatalog :: Bool -> ModelsResponse -> Maybe Text
defaultModelForCatalog chatGptMode catalog =
    defaultModelSlug (pickerModelPresets chatGptMode catalog)
        <|> defaultModelSlug (availableModelPresets chatGptMode catalog)

-- | Model-owned reasoning default.  'Nothing' means the provider or caller
-- should retain its compatibility default instead of inventing a catalog
-- value.
defaultReasoningEffortForInfo :: ModelInfo -> Maybe ReasoningEffort
defaultReasoningEffortForInfo = (.defaultReasoningLevel)

-- | Model-owned response-text verbosity default.
defaultVerbosityForInfo :: ModelInfo -> Maybe Verbosity
defaultVerbosityForInfo info
    | info.supportVerbosity = info.defaultVerbosity
    | otherwise = Nothing

-- | Only send the reasoning summary selector to models that advertise the
-- corresponding request parameter.
defaultReasoningSummaryForInfo :: ModelInfo -> Maybe ReasoningSummary
defaultReasoningSummaryForInfo info
    | info.supportsReasoningSummaryParameter =
        Just info.defaultReasoningSummary
    | otherwise = Nothing

-- | Picker-facing projection of a raw model descriptor.
modelPresetForInfo :: ModelInfo -> ModelPreset
modelPresetForInfo = modelPresetFromInfo

-- | Priority-sorted, auth-filtered models that are explicitly visible in a
-- model picker.  Hidden Daybreak, migration, and auto-review entries remain in
-- the raw bundled catalog for lookup and server-directed use.
pickerModelPresets :: Bool -> ModelsResponse -> [ModelPreset]
pickerModelPresets chatGptMode =
    filter (.showInPicker) . availableModelPresets chatGptMode
