-- | Configured model catalogs and pure picker navigation helpers.
module Agent.CLI.Models
    ( ModelTarget(..)
    , ModelOption(..)
    , modelsCacheFilePath
    , PickerState(..)
    , PickerEvent(..)
    , modelOptionFromCatalog
    , modelsForProvider
    , modelCatalog
    , catalogModelIds
    , defaultModelFor
    , defaultModelOptionFor
    , resolveConfiguredModel
    , resolveSavedModelTarget
    , resolveModelOptionById
    , rawModelOption
    , gatewayModelOptions
    , ensureCurrentInList
    , initialPickerState
    , initialPickerStateResolved
    , initialPickerStateResolvedWith
    , initialPickerStateForOptions
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
    , organizationGatewayConnectionId
    , catalogConnection
    , catalogDefaultForProvider
    , catalogGatewayModelById
    , catalogModelById
    )
import Agent.Dialect
    ( DialectId(..)
    , dialectIdForModel
    )
import qualified Agent.OpenRouter.Options as OpenRouter
import qualified Agent.OpenRouter.Request as OpenRouter
import Agent.Provider (Provider(..))
import Data.Char (isPrint)
import Data.List (find, findIndex, nub, nubBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

-- | Complete, validated identity of a model selection. This value is passed
-- through transitions and persistence so connection, wire model, and dialect
-- cannot drift apart in separate argument lists.
data ModelTarget = ModelTarget
    { targetProvider :: !Provider
    , targetConnectionId :: !Text
    , targetModelId :: !Text
    , targetWireModelId :: !Text
    , targetDialect :: !DialectId
    }
    deriving (Eq, Show)

data ModelOption = ModelOption
    { modelTarget :: !ModelTarget
    , modelContextWindow :: !(Maybe Int)
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
    , pickerScopeLabel :: !Text
    , pickerAll :: ![ModelOption]
    , pickerFilter :: !Text
    , pickerIndex :: !Int
    }
    deriving (Eq, Show)

data PickerEvent
    = PickerUp
    | PickerDown
    | PickerLeft
    | PickerRight
    | PickerConfirm
    | PickerCancel
    | PickerBackspace
    | PickerType Char
    deriving (Eq, Show)

modelOptionFromCatalog :: ModelCatalog -> CatalogModel -> Maybe ModelOption
modelOptionFromCatalog catalog model = do
    connection <- catalogConnection catalog model.catalogModelConnectionId
    provider <- case connection.connectionKind of
            BuiltinConnection value -> Just value
            -- Custom endpoints currently reuse the provider-independent
            -- Responses plumbing hosted under the OpenRouter runtime branch.
            CustomResponsesConnection _ -> Just OpenRouterProvider
            -- Gateway-only entries provide protocol and presentation metadata
            -- for live aliases. They must never enter the direct model catalog.
            OrganizationGatewayConnection -> Nothing
    pure ModelOption
        { modelTarget = ModelTarget
            { targetProvider = provider
            , targetConnectionId = model.catalogModelConnectionId
            , targetModelId = model.catalogModelId
            , targetWireModelId = model.catalogModelWireId
            , targetDialect = model.catalogModelDialect
            }
        , modelContextWindow = model.catalogModelContextWindow
        , modelLabel = model.catalogModelLabel
        , modelFallbackPriority = model.catalogModelFallbackPriority
        }

modelCatalog :: ModelCatalog -> [ModelOption]
modelCatalog catalog =
    mapMaybe (modelOptionFromCatalog catalog) catalog.catalogModels

modelsForProvider :: ModelCatalog -> Provider -> [ModelOption]
modelsForProvider catalog provider =
    filter
        ((== builtinConnectionId provider) . (.modelTarget.targetConnectionId))
        (modelCatalog catalog)

resolveSavedModelTarget
    :: ModelCatalog
    -> Bool
    -> Provider
    -> Text
    -> Text
    -> Maybe Text
    -> DialectId
    -> Either Text ModelTarget
resolveSavedModelTarget
        catalog deferToGateway provider connection model transport dialect
    | deferToGateway =
        Right persistedTarget
    | connection == organizationGatewayConnectionId =
        Left $
            "saved model "
                <> connection <> "/" <> model
                <> " requires an active organization gateway"
    | otherwise =
        case resolveConfiguredModel catalog model of
            Just option
                | option.modelTarget.targetConnectionId == connection ->
                    Right option.modelTarget
            _
                | connection == builtinConnectionId provider ->
                    Right persistedTarget
                | otherwise ->
                    Left $
                        "saved model "
                            <> connection <> "/" <> model
                            <> " is not present in ~/.haskell-agent/models.json"
  where
    persistedTarget = ModelTarget
        { targetProvider = provider
        , targetConnectionId = connection
        , targetModelId = model
        , targetWireModelId = fromMaybe model transport
        , targetDialect = dialect
        }

catalogModelIds :: ModelCatalog -> [Text]
catalogModelIds =
    map (.modelTarget.targetModelId) . modelCatalog

defaultModelOptionFor :: ModelCatalog -> Provider -> Maybe ModelOption
defaultModelOptionFor catalog provider =
    catalogDefaultForProvider catalog provider
        >>= modelOptionFromCatalog catalog

defaultModelFor :: ModelCatalog -> Provider -> Maybe Text
defaultModelFor catalog provider =
    (.modelTarget.targetModelId) <$> defaultModelOptionFor catalog provider

resolveConfiguredModel :: ModelCatalog -> Text -> Maybe ModelOption
resolveConfiguredModel catalog modelId =
    catalogModelById catalog modelId >>= modelOptionFromCatalog catalog

resolveModelOptionById :: [ModelOption] -> Text -> Maybe ModelOption
resolveModelOptionById options modelId =
    find ((== modelId) . (.modelTarget.targetModelId)) options

rawModelOption :: Provider -> Text -> ModelOption
rawModelOption provider model =
    ModelOption
        { modelTarget = ModelTarget
            { targetProvider = provider
            , targetConnectionId = builtinConnectionId provider
            , targetModelId = model
            , targetWireModelId = model
            , targetDialect = dialectIdForModel provider model
            }
        , modelContextWindow = Nothing
        , modelLabel = Nothing
        , modelFallbackPriority = Nothing
        }

-- | Convert the exact aliases advertised by an organization gateway into
-- model options.  Catalog entries may contribute presentation metadata and
-- dialect identity, but never transport identity: every option remains pinned
-- to the active gateway connection and sends the advertised alias verbatim.
gatewayModelOptions
    :: ModelCatalog
    -> Provider
    -> [Text]
    -> [ModelOption]
gatewayModelOptions catalog provider =
    map gatewayOption
        . nub
        . filter (not . Text.null)
        . map Text.strip
  where
    gatewayOption modelId =
        let configured =
                catalogGatewayModelById catalog modelId >>= \model ->
                    if provider == OpenAIProvider
                        && model.catalogModelConnectionId
                            == organizationGatewayConnectionId
                    then Just model
                    else Nothing
        in ModelOption
            { modelTarget = ModelTarget
                { targetProvider = provider
                , targetConnectionId = organizationGatewayConnectionId
                , targetModelId = modelId
                , targetWireModelId = modelId
                , targetDialect =
                    maybe
                        (dialectIdForModel provider modelId)
                        (.catalogModelDialect)
                        configured
                }
            , modelContextWindow =
                configured >>= (.catalogModelContextWindow)
            , modelLabel = configured >>= (.catalogModelLabel)
            , modelFallbackPriority =
                configured >>= (.catalogModelFallbackPriority)
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
            { modelTarget = ModelTarget
                { targetProvider = provider
                , targetConnectionId = connectionId
                , targetModelId = current
                , targetWireModelId = current
                , targetDialect = currentDialect
                }
            , modelContextWindow = Nothing
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
        True
        "all providers"
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
        catalog connectionId provider current currentDialect =
    initialPickerStateResolvedWith
        catalog
        []
        connectionId
        provider
        current
        currentDialect

-- | Build picker state with additional, runtime-discovered models. Configured
-- entries win when the same connection/model pair also appears in the live
-- catalog, preserving custom labels and wire-model overrides.
initialPickerStateResolvedWith
    :: ModelCatalog
    -> [ModelOption]
    -> Text
    -> Provider
    -> Text
    -> DialectId
    -> IO PickerState
initialPickerStateResolvedWith
        catalog discovered connectionId provider current currentDialect = do
    let options =
            prioritizeCurrentConnection connectionId
                $ deduplicateOptions
                    (modelCatalog catalog <> discovered)
    resolved <- resolveModelOptionsDialects options
    pure $
        pickerStateFromOptions
            True
            "all providers"
            connectionId provider current currentDialect resolved

-- | Build a picker from an authoritative option list.  Unlike the general
-- catalog picker, this never re-inserts an unlisted current model.
initialPickerStateForOptions
    :: Text
    -> [ModelOption]
    -> Text
    -> Provider
    -> Text
    -> DialectId
    -> IO PickerState
initialPickerStateForOptions
        scopeLabel options connectionId provider current currentDialect = do
    resolved <- resolveModelOptionsDialects (deduplicateOptions options)
    pure $
        pickerStateFromOptions
            False
            scopeLabel
            connectionId
            provider
            current
            currentDialect
            resolved

deduplicateOptions :: [ModelOption] -> [ModelOption]
deduplicateOptions = nubBy sameIdentity
  where
    sameIdentity left right =
        left.modelTarget.targetConnectionId
            == right.modelTarget.targetConnectionId
            && left.modelTarget.targetModelId
                == right.modelTarget.targetModelId

prioritizeCurrentConnection :: Text -> [ModelOption] -> [ModelOption]
prioritizeCurrentConnection current options =
    filter ((== current) . (.modelTarget.targetConnectionId)) options
        <> filter ((/= current) . (.modelTarget.targetConnectionId)) options

pickerStateFromOptions
    :: Bool
    -> Text
    -> Text
    -> Provider
    -> Text
    -> DialectId
    -> [ModelOption]
    -> PickerState
pickerStateFromOptions
        includeCurrent scopeLabel
        connectionId provider current currentDialect options =
    let allOpts =
            if includeCurrent
                then
                    ensureCurrentInList
                        connectionId provider current currentDialect options
                else options
        idx = fromMaybe 0 $
            findIndex
                (isCurrent connectionId current currentDialect)
                allOpts
    in PickerState
        { pickerConnectionId = connectionId
        , pickerProvider = provider
        , pickerCurrent = current
        , pickerCurrentDialect = currentDialect
        , pickerScopeLabel = scopeLabel
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
                needle `Text.isInfixOf`
                    Text.toLower opt.modelTarget.targetModelId
                    || needle
                        `Text.isInfixOf`
                            Text.toLower opt.modelTarget.targetConnectionId
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
    PickerLeft -> Right state
    PickerRight -> Right state
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
    option.modelTarget.targetConnectionId /= connectionId
        || option.modelTarget.targetProvider /= provider
        || option.modelTarget.targetDialect /= dialect

-- | Keep an explicitly persisted dialect while the effective transport model
-- is unchanged. When a recorded alias/default now resolves elsewhere, use the
-- newly inferred dialect and report that the target changed.
resolvePersistedDialect
    :: DialectId
    -> Maybe Text
    -> ModelTarget
    -> (DialectId, Bool)
resolvePersistedDialect storedDialect storedTransportModel inferred =
    case storedTransportModel of
        Just previous
            | previous /= inferred.targetWireModelId ->
                (inferred.targetDialect, True)
        _ -> (storedDialect, False)

resolveModelOptionDialect :: ModelOption -> IO ModelOption
resolveModelOptionDialect option
    | isBuiltinOpenRouter option = do
        options <- OpenRouter.clientOptionsFromEnv
        pure (resolveModelOptionDialectWith options option)
    | otherwise = pure option

resolveModelOptionsDialects :: [ModelOption] -> IO [ModelOption]
resolveModelOptionsDialects options
    | any isBuiltinOpenRouter options = do
        clientOptions <- OpenRouter.clientOptionsFromEnv
        pure (map (resolveModelOptionDialectWith clientOptions) options)
    | otherwise = pure options

resolveModelOptionDialectWith
    :: OpenRouter.ClientOptions
    -> ModelOption
    -> ModelOption
resolveModelOptionDialectWith options option
    | isBuiltinOpenRouter option =
        let transportedModel
                | option.modelTarget.targetWireModelId
                    /= option.modelTarget.targetModelId =
                        option.modelTarget.targetWireModelId
                | otherwise =
                    OpenRouter.mapModel options option.modelTarget.targetModelId
        in option
            { modelTarget = option.modelTarget
                { targetWireModelId = transportedModel
                , targetDialect =
                    dialectIdForModel OpenRouterProvider transportedModel
                }
            }
    | otherwise = option

isBuiltinOpenRouter :: ModelOption -> Bool
isBuiltinOpenRouter option =
    option.modelTarget.targetConnectionId
        == builtinConnectionId OpenRouterProvider

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
isFilterChar = isPrint

isCurrent :: Text -> Text -> DialectId -> ModelOption -> Bool
isCurrent connectionId current dialect option =
    option.modelTarget.targetConnectionId == connectionId
        && option.modelTarget.targetModelId == current
        && option.modelTarget.targetDialect == dialect

-- | Disk cache location for the fetched OpenAI model catalog, kept beside the
-- other harness state under the state directory.
modelsCacheFilePath :: FilePath -> FilePath
modelsCacheFilePath stateDir = stateDir <> "/models-cache.json"
