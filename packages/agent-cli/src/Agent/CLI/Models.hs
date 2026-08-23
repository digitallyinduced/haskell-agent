-- | Curated model catalogs and pure picker navigation helpers.
module Agent.CLI.Models
    ( ModelOption(..)
    , PickerState(..)
    , PickerEvent(..)
    , modelsForProvider
    , modelCatalog
    , catalogModelIds
    , ensureCurrentInList
    , initialPickerState
    , initialPickerStateResolved
    , visibleOptions
    , selectedOption
    , applyPickerEvent
    , modelTargetRequiresRebuild
    , resolvePersistedDialect
    , resolveModelOptionDialect
    ) where

import Agent.CLI.Prompt (defaultModelFor)
import Agent.Dialect
    ( DialectId(..)
    , dialectIdForModel
    )
import qualified Agent.OpenRouter.Options as OpenRouter
import qualified Agent.OpenRouter.Request as OpenRouter
import Agent.Provider (Provider(..), providerSlug)
import Data.List (findIndex, nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

data ModelOption = ModelOption
    { modelProvider :: !Provider
    , modelId :: !Text
    , modelTransportId :: !Text
    , modelDialect :: !DialectId
    , modelLabel :: !(Maybe Text)
    }
    deriving (Eq, Show)

-- | Interactive picker state. @pickerIndex@ indexes into 'visibleOptions'.
data PickerState = PickerState
    { pickerProvider :: !Provider
    , pickerCurrent :: !Text
    , pickerCurrentDialect :: !DialectId
    , pickerAll :: ![ModelOption]
    , pickerFilter :: !Text
    , pickerIndex :: !Int
    }
    deriving (Eq, Show)

data PickerEvent
    = PickerUp
    | PickerDown
    | PickerConfirm
    | PickerCancel
    | PickerBackspace
    | PickerType Char
    deriving (Eq, Show)

modelsForProvider :: Provider -> [ModelOption]
modelsForProvider provider =
    let opts = case provider of
            XAIProvider ->
                [ opt "grok-4.6" GrokBuildDialect (Just "default")
                , opt "grok-4.5" GrokBuildDialect Nothing
                , opt "grok-4.5-mini" GrokBuildDialect (Just "faster")
                , opt "grok-3" GrokBuildDialect Nothing
                ]
            OpenAIProvider ->
                [ opt "gpt-5.6-luna" CodexDialect (Just "default · fast")
                , opt "gpt-5.6-terra" CodexDialect (Just "balanced")
                , opt "gpt-5.6-sol" CodexDialect (Just "frontier")
                ]
            OpenRouterProvider ->
                [ opt "openai/gpt-5.1" CodexDialect (Just "default")
                , opt "stealth/ox-alpha" GenericResponsesDialect
                    (Just "free · coding · 1M context")
                , opt "anthropic/claude-sonnet-4" GenericResponsesDialect Nothing
                , opt "x-ai/grok-4" GrokBuildDialect Nothing
                , opt "google/gemini-2.5-pro" GenericResponsesDialect Nothing
                ]
        -- Keep the provider default first even if the table drifts.
        def = defaultModelFor provider
    in ensureCurrentInList provider def (dialectIdForModel provider def) opts
  where
    opt mid dialect label = ModelOption
        { modelProvider = provider
        , modelId = mid
        , modelTransportId = mid
        , modelDialect = dialect
        , modelLabel = label
        }

allProviders :: [Provider]
allProviders = [OpenAIProvider, XAIProvider, OpenRouterProvider]

-- | Every curated model, grouped by provider.
modelCatalog :: [ModelOption]
modelCatalog = concatMap modelsForProvider allProviders

-- | Every curated catalog id across providers, de-duplicated, for completion.
catalogModelIds :: [Text]
catalogModelIds =
    nub (map (\opt -> opt.modelId) modelCatalog)

-- | Prepend @current@ when it is missing so the active model stays visible.
ensureCurrentInList
    :: Provider
    -> Text
    -> DialectId
    -> [ModelOption]
    -> [ModelOption]
ensureCurrentInList provider current currentDialect options
    | Text.null current || current == "(unset)" = options
    | any (isCurrent provider current currentDialect) options = options
    | otherwise =
        ModelOption
            { modelProvider = provider
            , modelId = current
            , modelTransportId = current
            , modelDialect = currentDialect
            , modelLabel = Just "current"
            }
            : options

initialPickerState :: Provider -> Text -> DialectId -> PickerState
initialPickerState provider current currentDialect =
    pickerStateFromOptions
        provider
        current
        currentDialect
        (concatMap modelsForProvider providerOrder)
  where
    providerOrder = provider : filter (/= provider) allProviders

-- | Resolve transport rewrites before displaying model dialects. This keeps
-- OpenRouter picker labels and current-target identity aligned with the model
-- that will actually be sent.
initialPickerStateResolved
    :: Provider
    -> Text
    -> DialectId
    -> IO PickerState
initialPickerStateResolved provider current currentDialect = do
    resolved <- traverse resolveModelOptionDialect
        (concatMap modelsForProvider providerOrder)
    pure (pickerStateFromOptions provider current currentDialect resolved)
  where
    providerOrder = provider : filter (/= provider) allProviders

pickerStateFromOptions
    :: Provider
    -> Text
    -> DialectId
    -> [ModelOption]
    -> PickerState
pickerStateFromOptions provider current currentDialect options =
    let allOpts =
            ensureCurrentInList provider current currentDialect options
        idx = fromMaybe 0 $
            findIndex
                (isCurrent provider current currentDialect)
                allOpts
    in PickerState
        { pickerProvider = provider
        , pickerCurrent = current
        , pickerCurrentDialect = currentDialect
        , pickerAll = allOpts
        , pickerFilter = ""
        , pickerIndex = idx
        }

visibleOptions :: PickerState -> [ModelOption]
visibleOptions state
    | Text.null needle = state.pickerAll
    | otherwise =
        filter
            (\opt -> needle `Text.isInfixOf` Text.toLower opt.modelId
                || needle `Text.isInfixOf` Text.toLower (providerSlug opt.modelProvider)
                || maybe
                    False
                    ((needle `Text.isInfixOf`) . Text.toLower)
                    opt.modelLabel)
            state.pickerAll
  where
    needle = Text.toLower state.pickerFilter

selectedOption :: PickerState -> Maybe ModelOption
selectedOption state =
    case visibleOptions state of
        [] -> Nothing
        opts ->
            let i = clampIndex (length opts) state.pickerIndex
            in Just (opts !! i)

applyPickerEvent :: PickerEvent -> PickerState -> Either (Maybe ModelOption) PickerState
applyPickerEvent event state = case event of
    PickerCancel -> Left Nothing
    PickerConfirm -> Left (selectedOption state)
    PickerUp -> Right (move (-1) state)
    PickerDown -> Right (move 1 state)
    PickerBackspace ->
        Right $ clampSelection state
            { pickerFilter = Text.dropEnd 1 state.pickerFilter
            , pickerIndex = 0
            }
    PickerType c
        | isFilterChar c ->
            Right $ clampSelection state
                { pickerFilter = state.pickerFilter <> Text.singleton c
                , pickerIndex = 0
                }
        | otherwise -> Right state

-- | Changing providers or model-facing dialects requires rebuilding tools,
-- prompts, and the transport backend. A model change within the current
-- provider and dialect can update request parameters in place.
modelTargetRequiresRebuild
    :: Provider
    -> DialectId
    -> ModelOption
    -> Bool
modelTargetRequiresRebuild provider dialect option =
    option.modelProvider /= provider
        || option.modelDialect /= dialect

-- | Keep an explicitly persisted dialect while the effective transport model
-- is unchanged. When a recorded alias/default now resolves elsewhere, use the
-- newly inferred dialect and report that the target changed. Legacy records
-- without an effective model retain their old dialect for compatibility.
resolvePersistedDialect
    :: DialectId
    -> Maybe Text
    -> ModelOption
    -> (DialectId, Bool)
resolvePersistedDialect storedDialect storedTransportModel inferred =
    case storedTransportModel of
        Just previous
            | previous /= inferred.modelTransportId ->
                (inferred.modelDialect, True)
        _ -> (storedDialect, False)

-- | Resolve the dialect from the model that the provider transport will
-- actually receive. OpenRouter may rewrite friendly aliases and exact model
-- overrides before sending a request.
resolveModelOptionDialect :: ModelOption -> IO ModelOption
resolveModelOptionDialect option = do
    transportedModel <- case option.modelProvider of
        OpenRouterProvider -> do
            options <- OpenRouter.clientOptionsFromEnv
            pure (OpenRouter.mapModel options option.modelId)
        _ -> pure option.modelId
    pure option
        { modelTransportId = transportedModel
        , modelDialect =
            dialectIdForModel option.modelProvider transportedModel
        }

move :: Int -> PickerState -> PickerState
move delta state =
    let n = length (visibleOptions state)
    in if n == 0
        then state { pickerIndex = 0 }
        else
            let i = clampIndex n state.pickerIndex
                i' = (i + delta) `mod` n
            in state { pickerIndex = i' }

clampSelection :: PickerState -> PickerState
clampSelection state =
    let n = length (visibleOptions state)
    in state { pickerIndex = clampIndex n state.pickerIndex }

clampIndex :: Int -> Int -> Int
clampIndex n i
    | n <= 0 = 0
    | i < 0 = 0
    | i >= n = n - 1
    | otherwise = i

isFilterChar :: Char -> Bool
isFilterChar c =
    c == '-' || c == '/' || c == '.' || c == '_'
        || (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9')

isCurrent :: Provider -> Text -> DialectId -> ModelOption -> Bool
isCurrent provider current dialect option =
    option.modelProvider == provider
        && option.modelId == current
        && option.modelDialect == dialect
