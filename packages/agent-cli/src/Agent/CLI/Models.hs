-- | Curated model catalogs and pure picker navigation helpers.
module Agent.CLI.Models
    ( ModelOption(..)
    , PickerState(..)
    , PickerEvent(..)
    , modelsForProvider
    , ensureCurrentInList
    , initialPickerState
    , visibleOptions
    , selectedOption
    , applyPickerEvent
    ) where

import Agent.CLI.Prompt (defaultModelFor)
import Agent.Provider (Provider(..))
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

data ModelOption = ModelOption
    { modelId :: !Text
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
                [ opt "gpt-5.6-luna" (Just "default")
                , opt "gpt-5.1-codex" Nothing
                , opt "gpt-5.1-codex-mini" (Just "faster")
                , opt "gpt-5.1" Nothing
                , opt "o3" Nothing
                ]
            OpenRouterProvider ->
                [ opt "openai/gpt-5.1" (Just "default")
                , opt "anthropic/claude-sonnet-4" Nothing
                , opt "x-ai/grok-4" Nothing
                , opt "google/gemini-2.5-pro" Nothing
                ]
        -- Keep the provider default first even if the table drifts.
        def = defaultModelFor provider
    in ensureCurrentInList def opts
  where
    opt mid label = ModelOption { modelId = mid, modelLabel = label }

-- | Prepend @current@ when it is missing so the active model stays visible.
ensureCurrentInList :: Text -> [ModelOption] -> [ModelOption]
ensureCurrentInList current options
    | Text.null current || current == "(unset)" = options
    | any (\opt -> opt.modelId == current) options = options
    | otherwise =
        ModelOption { modelId = current, modelLabel = Just "current" }
            : options

initialPickerState :: Provider -> Text -> PickerState
initialPickerState provider current =
    let allOpts = ensureCurrentInList current (modelsForProvider provider)
        idx = fromMaybe 0 $
            fmap fst $
                find (\(_, opt) -> opt.modelId == current) (zip [0 ..] allOpts)
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

applyPickerEvent :: PickerEvent -> PickerState -> Either (Maybe Text) PickerState
applyPickerEvent event state = case event of
    PickerCancel -> Left Nothing
    PickerConfirm -> Left ((.modelId) <$> selectedOption state)
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
