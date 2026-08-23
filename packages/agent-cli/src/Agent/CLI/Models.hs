-- | Configured model catalogs and pure picker navigation helpers.
module Agent.CLI.Models
    ( ModelOption(..)
    , PickerState(..)
    , PickerEvent(..)
    , modelOptionFromCatalog
    , modelsForProvider
    , modelCatalog
    , catalogModelIds
    , defaultModelFor
    , defaultModelOptionFor
    , resolveConfiguredModel
    , rawModelOption
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

import Agent.CLI.ModelConfig
    ( CatalogModel(..)
    , ConnectionKind(..)
    , ModelCatalog(..)
    , ModelConnection(..)
    , builtinConnectionId
    , catalogConnection
    , catalogDefaultForProvider
    , catalogModelById
    )
import Agent.Dialect
    ( DialectId(..)
    , dialectIdForModel
    )
import qualified Agent.OpenRouter.Options as OpenRouter
import qualified Agent.OpenRouter.Request as OpenRouter
import Agent.Provider (Provider(..))
import Data.List (findIndex, nub)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

data ModelOption = ModelOption
    { modelConnectionId :: !Text
    , modelProvider :: !Provider
    , modelId :: !Text
    , modelTransportId :: !Text
    , modelDialect :: !DialectId
    , modelLabel :: !(Maybe Text)
    , modelFallbackPriority :: !(Maybe Int)
    }
    deriving (Eq, Show)

-- | Interactive picker state. @pickerIndex@ indexes into 'visibleOptions'.
data PickerState = PickerState
    { pickerConnectionId :: !Text
    , pickerProvider :: !Provider
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

modelOptionFromCatalog :: ModelCatalog -> CatalogModel -> Maybe ModelOption
modelOptionFromCatalog catalog model = do
    connection <- catalogConnection catalog model.catalogModelConnectionId
    let provider = case connection.connectionKind of
            BuiltinConnection value -> value
            -- Provider remains the internal transport/auth family in existing
            -- persistence and tool code. Custom endpoints use the generic
            -- Responses backend and never OpenRouter auth/account behavior.
            CustomResponsesConnection _ -> OpenRouterProvider
    pure ModelOption
        { modelConnectionId = model.catalogModelConnectionId
        , modelProvider = provider
        , modelId = model.catalogModelId
        , modelTransportId = model.catalogModelWireId
        , modelDialect = model.catalogModelDialect
        , modelLabel = model.catalogModelLabel
        , modelFallbackPriority = model.catalogModelFallbackPriority
        }

modelCatalog :: ModelCatalog -> [ModelOption]
modelCatalog catalog =
    mapMaybe (modelOptionFromCatalog catalog) catalog.catalogModels

modelsForProvider :: ModelCatalog -> Provider -> [ModelOption]
modelsForProvider catalog provider =
    filter
        ((== builtinConnectionId provider) . (.modelConnectionId))
        (modelCatalog catalog)

catalogModelIds :: ModelCatalog -> [Text]
catalogModelIds =
    nub . map (.modelId) . modelCatalog

defaultModelOptionFor :: ModelCatalog -> Provider -> Maybe ModelOption
defaultModelOptionFor catalog provider =
    catalogDefaultForProvider catalog provider
        >>= modelOptionFromCatalog catalog

defaultModelFor :: ModelCatalog -> Provider -> Maybe Text
defaultModelFor catalog provider =
    (.modelId) <$> defaultModelOptionFor catalog provider

resolveConfiguredModel :: ModelCatalog -> Text -> Maybe ModelOption
resolveConfiguredModel catalog modelId =
    catalogModelById catalog modelId >>= modelOptionFromCatalog catalog

rawModelOption :: Provider -> Text -> ModelOption
rawModelOption provider model =
    ModelOption
        { modelConnectionId = builtinConnectionId provider
        , modelProvider = provider
        , modelId = model
        , modelTransportId = model
        , modelDialect = dialectIdForModel provider model
        , modelLabel = Nothing
        , modelFallbackPriority = Nothing
        }

-- | Prepend @current@ when it is missing so the active model stays visible.
ensureCurrentInList
    :: Text
    -> Provider
    -> Text
    -> DialectId
    -> [ModelOption]
    -> [ModelOption]
ensureCurrentInList connectionId provider current currentDialect options
    | Text.null current || current == "(unset)" = options
    | any (isCurrent connectionId current currentDialect) options = options
    | otherwise =
        ModelOption
            { modelConnectionId = connectionId
            , modelProvider = provider
            , modelId = current
            , modelTransportId = current
            , modelDialect = currentDialect
            , modelLabel = Just "current"
            , modelFallbackPriority = Nothing
            }
            : options

initialPickerState
    :: ModelCatalog
    -> Text
    -> Provider
    -> Text
    -> DialectId
    -> PickerState
initialPickerState catalog connectionId provider current currentDialect =
    pickerStateFromOptions
        connectionId
        provider
        current
        currentDialect
        (prioritizeCurrentConnection connectionId (modelCatalog catalog))

-- | Resolve built-in OpenRouter environment rewrites before displaying
-- dialects. Configured custom endpoints already carry an exact wire target and
-- explicit dialect.
initialPickerStateResolved
    :: ModelCatalog
    -> Text
    -> Provider
    -> Text
    -> DialectId
    -> IO PickerState
initialPickerStateResolved
        catalog connectionId provider current currentDialect = do
    resolved <- traverse resolveModelOptionDialect
        (prioritizeCurrentConnection connectionId (modelCatalog catalog))
    pure $
        pickerStateFromOptions
            connectionId provider current currentDialect resolved

prioritizeCurrentConnection :: Text -> [ModelOption] -> [ModelOption]
prioritizeCurrentConnection current options =
    filter ((== current) . (.modelConnectionId)) options
        <> filter ((/= current) . (.modelConnectionId)) options

pickerStateFromOptions
    :: Text
    -> Provider
    -> Text
    -> DialectId
    -> [ModelOption]
    -> PickerState
pickerStateFromOptions
        connectionId provider current currentDialect options =
    let allOpts =
            ensureCurrentInList
                connectionId provider current currentDialect options
        idx = fromMaybe 0 $
            findIndex
                (isCurrent connectionId current currentDialect)
                allOpts
    in PickerState
        { pickerConnectionId = connectionId
        , pickerProvider = provider
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
            (\opt ->
                needle `Text.isInfixOf` Text.toLower opt.modelId
                    || needle
                        `Text.isInfixOf`
                            Text.toLower opt.modelConnectionId
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

-- | Changing connections, providers, or model-facing dialects requires
-- rebuilding tools, prompts, auth, and the transport backend.
modelTargetRequiresRebuild
    :: Text
    -> Provider
    -> DialectId
    -> ModelOption
    -> Bool
modelTargetRequiresRebuild connectionId provider dialect option =
    option.modelConnectionId /= connectionId
        || option.modelProvider /= provider
        || option.modelDialect /= dialect

-- | Keep an explicitly persisted dialect while the effective transport model
-- is unchanged. When a recorded alias/default now resolves elsewhere, use the
-- newly inferred dialect and report that the target changed.
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

resolveModelOptionDialect :: ModelOption -> IO ModelOption
resolveModelOptionDialect option
    | option.modelConnectionId == builtinConnectionId OpenRouterProvider = do
        options <- OpenRouter.clientOptionsFromEnv
        let transportedModel
                | option.modelTransportId /= option.modelId =
                    option.modelTransportId
                | otherwise =
                    OpenRouter.mapModel options option.modelId
        pure option
            { modelTransportId = transportedModel
            , modelDialect =
                dialectIdForModel option.modelProvider transportedModel
            }
    | otherwise = pure option

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

isCurrent :: Text -> Text -> DialectId -> ModelOption -> Bool
isCurrent connectionId current dialect option =
    option.modelConnectionId == connectionId
        && option.modelId == current
        && option.modelDialect == dialect
