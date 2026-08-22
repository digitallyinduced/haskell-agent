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
    , visibleOptions
    , selectedOption
    , applyPickerEvent
    ) where

import Agent.CLI.Prompt (defaultModelFor)
import Agent.Provider (Provider(..), providerSlug)
import Data.List (find, nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

data ModelOption = ModelOption
    { modelProvider :: !Provider
    , modelId :: !Text
    , modelLabel :: !(Maybe Text)
    }
    deriving (Eq, Show)

-- | Interactive picker state. @pickerIndex@ indexes into 'visibleOptions'.
data PickerState = PickerState
    { pickerProvider :: !Provider
    , pickerCurrent :: !Text
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
                [ opt "grok-4.6" (Just "default")
                , opt "grok-4.5" Nothing
                , opt "grok-4.5-mini" (Just "faster")
                , opt "grok-3" Nothing
                ]
            OpenAIProvider ->
                [ opt "gpt-5.6-luna" (Just "default · fast")
                , opt "gpt-5.6-terra" (Just "balanced")
                , opt "gpt-5.6-sol" (Just "frontier")
                ]
            OpenRouterProvider ->
                [ opt "openai/gpt-5.1" (Just "default")
                , opt "stealth/ox-alpha" (Just "free · coding · 1M context")
                , opt "anthropic/claude-sonnet-4" Nothing
                , opt "x-ai/grok-4" Nothing
                , opt "google/gemini-2.5-pro" Nothing
                ]
            ClaudeCodeProvider ->
                [ opt "sonnet" (Just "default · subscription")
                , opt "opus" (Just "frontier · subscription")
                , opt "fable" (Just "fast · subscription")
                ]
        -- Keep the provider default first even if the table drifts.
        def = defaultModelFor provider
    in ensureCurrentInList provider def opts
  where
    opt mid label = ModelOption
        { modelProvider = provider
        , modelId = mid
        , modelLabel = label
        }

allProviders :: [Provider]
allProviders =
    [ OpenAIProvider
    , XAIProvider
    , ClaudeCodeProvider
    , OpenRouterProvider
    ]

-- | Every curated model, grouped by provider.
modelCatalog :: [ModelOption]
modelCatalog = concatMap modelsForProvider allProviders

-- | Every curated catalog id across providers, de-duplicated, for completion.
catalogModelIds :: [Text]
catalogModelIds =
    nub (map (\opt -> opt.modelId) modelCatalog)

-- | Prepend @current@ when it is missing so the active model stays visible.
ensureCurrentInList :: Provider -> Text -> [ModelOption] -> [ModelOption]
ensureCurrentInList provider current options
    | Text.null current || current == "(unset)" = options
    | any (isCurrent provider current) options = options
    | otherwise =
        ModelOption
            { modelProvider = provider
            , modelId = current
            , modelLabel = Just "current"
            }
            : options

initialPickerState :: Provider -> Text -> PickerState
initialPickerState provider current =
    let providerOrder = provider : filter (/= provider) allProviders
        allOpts = ensureCurrentInList provider current
            (concatMap modelsForProvider providerOrder)
        idx = fromMaybe 0 $
            fmap fst $
                find (\(_, opt) -> isCurrent provider current opt) (zip [0 ..] allOpts)
    in PickerState
        { pickerProvider = provider
        , pickerCurrent = current
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

isCurrent :: Provider -> Text -> ModelOption -> Bool
isCurrent provider current option =
    option.modelProvider == provider && option.modelId == current
