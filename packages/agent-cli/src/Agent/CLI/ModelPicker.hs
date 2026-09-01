-- | Interactive TTY model picker for bare @/model@.
module Agent.CLI.ModelPicker
    ( ModelPickerSelection(..)
    , ModelPickerState(..)
    , pickModel
    , pickModelWithOptions
    , pickModelWithEffort
    , pickModelState
    , pickModelStateWithEffort
    , formatCatalogListing
    , initialModelPickerState
    , applyModelPickerEvent
    , modelEffortOptions
    , initialModelEffort
    , renderEffortIndicator
    , renderPickerFrame
    , renderModelPickerFrame
    , decodePickerKey
    ) where

import Agent.CLI.ModelConfig
    ( ModelCatalog
    , organizationGatewayConnectionId
    )
import Agent.CLI.Models
import Agent.CLI.Options
    ( defaultEffortFor
    , normalizeReasoningEffortForDialect
    , reasoningEffortsForDialect
    )
import Agent.CLI.Picker (PickerKey(..), runOverlayWithDecoder)
import qualified Agent.CLI.Picker as Picker
import Agent.CLI.Style
    ( glyphOk
    , roleMuted
    , rolePrompt
    , roleSelected
    , roleSuccess
    , roleWarn
    )
import Agent.Dialect (DialectId, dialectSlug)
import Agent.Provider (Provider)
import Agent.ReasoningEffort
    ( ReasoningEffort
    , reasoningEffortText
    )
import Agent.TUI.TextWidth (displayTerminalText)
import Control.Monad (join)
import Data.Char (isPrint)
import Data.List (elemIndex)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO (hFlush, hIsTerminalDevice, stderr, stdin)

-- | A confirmed model and the reasoning effort chosen alongside it.
data ModelPickerSelection = ModelPickerSelection
    { modelPickerOption :: !ModelOption
    , modelPickerEffort :: !ReasoningEffort
    }
    deriving (Eq, Show)

-- | Search/navigation state plus an independent effort choice for every model.
-- Keeping choices per model means moving through the list does not discard an
-- adjustment the user made before comparing another model.
data ModelPickerState = ModelPickerState
    { modelPickerModels :: !PickerState
    , modelPickerEfforts
        :: !(Map.Map (Text, Text, DialectId) ReasoningEffort)
    }
    deriving (Eq, Show)

-- | Decode one keypress (including CSI arrow sequences) into a picker event.
decodePickerKey :: String -> Maybe PickerEvent
decodePickerKey raw = toEvent <$> decodeModelPickerKey raw

decodeModelPickerKey :: String -> Maybe PickerKey
decodeModelPickerKey raw = case raw of
    [c]
        | isPrint c -> Just (PickerKeyChar c)
    _ -> Picker.decodePickerKey raw

toEvent :: PickerKey -> PickerEvent
toEvent = \case
    PickerKeyUp -> PickerUp
    PickerKeyDown -> PickerDown
    PickerKeyLeft -> PickerLeft
    PickerKeyRight -> PickerRight
    PickerKeyConfirm -> PickerConfirm
    PickerKeyCancel -> PickerCancel
    PickerKeyBackspace -> PickerBackspace
    PickerKeyChar c -> PickerType c

-- | Open the picker when stdin is a TTY; otherwise print the catalog.
-- Returns the provider/model choice on confirm and @Nothing@ on cancel, EOF,
-- or non-TTY input.
pickModel
    :: ModelCatalog
    -> Bool
    -> Text
    -> Provider
    -> Text
    -> DialectId
    -> IO (Maybe ModelOption)
pickModel catalog color connectionId provider current currentDialect =
    pickModelWithOptions
        catalog
        []
        color
        connectionId
        provider
        current
        currentDialect

-- | Compatibility entry point for callers that only need the model. The
-- provider default seeds the integrated effort control.
pickModelWithOptions
    :: ModelCatalog
    -> [ModelOption]
    -> Bool
    -> Text
    -> Provider
    -> Text
    -> DialectId
    -> IO (Maybe ModelOption)
pickModelWithOptions
        catalog discovered color connectionId provider current currentDialect =
    fmap (fmap (.modelPickerOption)) $
        pickModelWithEffort
            catalog
            discovered
            color
            connectionId
            provider
            current
            currentDialect
            (defaultEffortFor provider)

-- | Open the integrated model/effort picker with runtime-discovered entries.
pickModelWithEffort
    :: ModelCatalog
    -> [ModelOption]
    -> Bool
    -> Text
    -> Provider
    -> Text
    -> DialectId
    -> ReasoningEffort
    -> IO (Maybe ModelPickerSelection)
pickModelWithEffort
        catalog
        discovered
        color
        connectionId
        provider
        current
        currentDialect
        currentEffort = do
    models <-
        initialPickerStateResolvedWith
            catalog discovered connectionId provider current currentDialect
    pickModelStateWithEffort color currentEffort models

-- | Open a picker for a pre-scoped, authoritative option list.
pickModelState :: Bool -> PickerState -> IO (Maybe ModelOption)
pickModelState color models =
    fmap (fmap (.modelPickerOption)) $
        pickModelStateWithEffort
            color
            (defaultEffortFor models.pickerProvider)
            models

-- | Open the integrated model/effort picker for a pre-scoped option list.
pickModelStateWithEffort
    :: Bool
    -> ReasoningEffort
    -> PickerState
    -> IO (Maybe ModelPickerSelection)
pickModelStateWithEffort color currentEffort models = do
    isTty <- hIsTerminalDevice stdin
    let state0 = initialModelPickerState currentEffort models
    if not isTty
        then do
            Text.hPutStrLn stderr
                (formatCatalogListingState color models)
            hFlush stderr
            pure Nothing
        else do
            result <-
                runOverlayWithDecoder
                    decodeModelPickerKey
                    (renderModelPickerFrame color)
                    (\key -> applyModelPickerEvent (toEvent key))
                    state0
            pure (join result)

-- | Seed each row with the active effort for the current model and the target
-- provider's normalized default for every other model.
initialModelPickerState
    :: ReasoningEffort
    -> PickerState
    -> ModelPickerState
initialModelPickerState currentEffort models =
    ModelPickerState
        { modelPickerModels = models
        , modelPickerEfforts =
            Map.fromList
                [ (modelIdentity option, initialModelEffort models currentEffort option)
                | option <- models.pickerAll
                ]
        }

-- | Efforts supported by a model-facing dialect.
modelEffortOptions :: ModelOption -> [ReasoningEffort]
modelEffortOptions =
    reasoningEffortsForDialect . (.modelTarget.targetDialect)

-- | Initial effort for one row in a picker.
initialModelEffort
    :: PickerState
    -> ReasoningEffort
    -> ModelOption
    -> ReasoningEffort
initialModelEffort models currentEffort option =
    normalizeReasoningEffortForDialect
        option.modelTarget.targetDialect
        ( if isCurrent
                models.pickerConnectionId
                models.pickerCurrent
                models.pickerCurrentDialect
                option
            then currentEffort
            else defaultEffortFor option.modelTarget.targetProvider
        )

-- | Apply navigation, search, confirmation, or a horizontal effort change.
applyModelPickerEvent
    :: PickerEvent
    -> ModelPickerState
    -> Either (Maybe ModelPickerSelection) ModelPickerState
applyModelPickerEvent event state = case event of
    PickerCancel -> Left Nothing
    PickerConfirm ->
        Left $
            fmap
                (\option ->
                    ModelPickerSelection
                        { modelPickerOption = option
                        , modelPickerEffort = effortForOption state option
                        })
                (selectedOption state.modelPickerModels)
    PickerLeft -> Right (adjustSelectedEffort (-1) state)
    PickerRight -> Right (adjustSelectedEffort 1 state)
    _ ->
        case applyPickerEvent event state.modelPickerModels of
            Right models -> Right state { modelPickerModels = models }
            Left _ -> Right state

formatCatalogListing
    :: ModelCatalog
    -> Bool
    -> Text
    -> Provider
    -> Text
    -> DialectId
    -> IO Text
formatCatalogListing
        catalog color connectionId provider current currentDialect = do
    state <-
        initialPickerStateResolved
            catalog connectionId provider current currentDialect
    pure (formatCatalogListingState color state)

formatCatalogListingState
    :: Bool
    -> PickerState
    -> Text
formatCatalogListingState color state =
    let header =
            roleMuted color
                (glyphSessionLike
                    <> "model: "
                    <> pickerCurrentLabel state
                    <> " · "
                    <> state.pickerScopeLabel)
        rows =
            map
                (\opt ->
                    let mark
                            | isCurrent
                                state.pickerConnectionId
                                state.pickerCurrent
                                state.pickerCurrentDialect
                                opt =
                                roleSuccess color
                                    (glyphOk
                                        <> formatOptionName gatewayMode opt)
                            | otherwise =
                                roleMuted color
                                    ("  " <> formatOptionName gatewayMode opt)
                        label = case opt.modelLabel of
                            Nothing -> ""
                            Just l ->
                                roleMuted color
                                    ("  " <> displayPickerText l)
                    in mark <> label)
                state.pickerAll
        gatewayMode = pickerUsesGateway state
    in Text.intercalate "\n" (header : rows)

-- | Backwards-compatible pure frame seeded from the current provider default.
renderPickerFrame :: Bool -> PickerState -> Text
renderPickerFrame color models =
    renderModelPickerFrame color $
        initialModelPickerState
            (defaultEffortFor models.pickerProvider)
            models

-- | Pure integrated model-picker frame used by the interactive loop and tests.
renderModelPickerFrame :: Bool -> ModelPickerState -> Text
renderModelPickerFrame color state =
    Text.intercalate "\n" $
        [ header
        , searchLine
        , scrollIndicator (windowStart > 0) "↑ more"
        ]
        <> paddedBody
        <> [ scrollIndicator
                (windowStart + length window < optionCount)
                "↓ more"
           , ""
           ]
        <> selectedDetailLines gatewayMode color selected
        <> [ ""
           , roleMuted color
                "↑↓ select · ←→ reasoning effort · ↵ confirm · esc cancel"
           ]
  where
    models = state.modelPickerModels
    visible = visibleOptions models
    optionCount = length visible
    selectedIndex =
        if optionCount == 0
            then 0
            else min models.pickerIndex (optionCount - 1)
    windowStart =
        max 0 $
            min
                (max 0 (optionCount - pickerViewportSize))
                (selectedIndex - pickerViewportSize `div` 2)
    window = take pickerViewportSize (drop windowStart visible)
    body = case visible of
        [] -> [roleMuted color "  No matching models"]
        _ ->
            zipWith
                (\index option ->
                    renderRow
                        gatewayMode
                        color
                        (index == selectedIndex)
                        models.pickerConnectionId
                        models.pickerCurrent
                        models.pickerCurrentDialect
                        (effortForOption state option)
                        option)
                [windowStart ..]
                window
    paddedBody =
        body
            <> replicate
                (max 0 (pickerViewportSize - length body))
                ""
    selected = selectedOption models
    gatewayMode = pickerUsesGateway models
    header =
        rolePrompt color "Models"
            <> roleMuted color
                (displayPickerText
                    (" · "
                        <> models.pickerScopeLabel
                        <> " · current "
                        <> pickerCurrentLabel models))
    searchLine
        | Text.null models.pickerFilter =
            roleMuted color "/ Type to search"
        | otherwise =
            roleMuted color "/ "
                <> roleWarn color models.pickerFilter
    scrollIndicator visible_ label
        | visible_ = roleMuted color ("    " <> label)
        | otherwise = ""

pickerCurrentLabel :: PickerState -> Text
pickerCurrentLabel state
    | any
        (\option ->
            option.modelTarget.targetConnectionId
                == state.pickerConnectionId
                && option.modelTarget.targetModelId
                    == state.pickerCurrent)
        state.pickerAll =
            displayModelName
                (pickerUsesGateway state)
                state.pickerConnectionId
                state.pickerCurrent
    | otherwise = "(not offered in this scope)"

pickerViewportSize :: Int
pickerViewportSize = 12

renderRow
    :: Bool
    -> Bool
    -> Bool
    -> Text
    -> Text
    -> DialectId
    -> ReasoningEffort
    -> ModelOption
    -> Text
renderRow
        gatewayMode
        color
        selected
        currentConnection
        current
        currentDialect
        effort
        option =
    if selected
        then roleSelected color
            ("› " <> clippedName <> padding <> "← " <> indicator <> " →")
        else "  " <> clippedName <> padding <> roleMuted color indicator
  where
    plainName =
        displayPickerText
            (displayModelName
                gatewayMode
                option.modelTarget.targetConnectionId
                option.modelTarget.targetModelId
                <> if isCurrent currentConnection current currentDialect option
                    then " ✓"
                    else "")
    clippedName = Text.take modelNameWidth plainName
    padding =
        Text.replicate
            (max 2 (modelNameWidth - Text.length clippedName + 2))
            " "
    indicator = renderEffortIndicator option effort

modelNameWidth :: Int
modelNameWidth = 44

-- | Render a compact absolute effort gauge plus its canonical label.
renderEffortIndicator :: ModelOption -> ReasoningEffort -> Text
renderEffortIndicator _option effort =
    Text.replicate filled "■"
        <> Text.replicate (barWidth - filled) "□"
        <> " "
        <> reasoningEffortText effort
  where
    barWidth = fromEnum (maxBound :: ReasoningEffort)
    filled = min barWidth (fromEnum effort)

selectedDetailLines :: Bool -> Bool -> Maybe ModelOption -> [Text]
selectedDetailLines gatewayMode color = \case
    Nothing ->
        [ rolePrompt color "No model selected"
        , roleMuted color "Try a different search."
        ]
    Just option ->
        [ rolePrompt color $
            displayPickerText
                (displayModelName
                    gatewayMode
                    option.modelTarget.targetConnectionId
                    option.modelTarget.targetModelId)
        , roleMuted color $
            displayPickerText $
                Text.intercalate
                    " · "
                    ( maybe [] pure option.modelLabel
                        <> [dialectSlug option.modelTarget.targetDialect]
                        <> contextDetail option
                    )
        ]

contextDetail :: ModelOption -> [Text]
contextDetail option
    | maybe False
        (Text.isInfixOf "context" . Text.toCaseFold)
        option.modelLabel = []
    | otherwise =
        maybe [] (pure . formatContextLength) option.modelContextWindow

formatContextLength :: Int -> Text
formatContextLength contextLength
    | contextLength >= 1000000 =
        compactDecimal 1000000 "M context"
    | contextLength >= 1000 =
        compactDecimal 1000 "k context"
    | otherwise = Text.pack (show contextLength) <> " context"
  where
    compactDecimal unit suffix =
        let tenths = contextLength * 10 `div` unit
            whole = tenths `div` 10
            fraction = tenths `mod` 10
            amount
                | fraction == 0 = show whole
                | otherwise = show whole <> "." <> show fraction
        in Text.pack amount <> suffix

adjustSelectedEffort :: Int -> ModelPickerState -> ModelPickerState
adjustSelectedEffort delta state =
    case selectedOption state.modelPickerModels of
        Nothing -> state
        Just option ->
            let efforts = modelEffortOptions option
                current = effortForOption state option
                currentIndex = fromMaybe 0 (elemIndex current efforts)
                nextIndex =
                    max 0 (min (length efforts - 1) (currentIndex + delta))
                next = fromMaybe current (atMay nextIndex efforts)
            in state
                { modelPickerEfforts =
                    Map.insert
                        (modelIdentity option)
                        next
                        state.modelPickerEfforts
                }

effortForOption :: ModelPickerState -> ModelOption -> ReasoningEffort
effortForOption state option =
    fromMaybe
        (initialModelEffort
            state.modelPickerModels
            (defaultEffortFor state.modelPickerModels.pickerProvider)
            option)
        (Map.lookup (modelIdentity option) state.modelPickerEfforts)

modelIdentity :: ModelOption -> (Text, Text, DialectId)
modelIdentity option =
    ( option.modelTarget.targetConnectionId
    , option.modelTarget.targetModelId
    , option.modelTarget.targetDialect
    )

atMay :: Int -> [a] -> Maybe a
atMay index values
    | index < 0 = Nothing
    | otherwise = case drop index values of
        value : _ -> Just value
        [] -> Nothing

-- Matches Style.glyphSession without pulling unicode env probing here.
glyphSessionLike :: Text
glyphSessionLike = "⧉ "

formatOptionName :: Bool -> ModelOption -> Text
formatOptionName gatewayMode option =
    displayPickerText $
        ( if gatewayMode
            then option.modelTarget.targetModelId
            else
                option.modelTarget.targetConnectionId
                    <> " · "
                    <> option.modelTarget.targetModelId
        )
            <> " · "
            <> dialectSlug option.modelTarget.targetDialect

pickerUsesGateway :: PickerState -> Bool
pickerUsesGateway state =
    state.pickerConnectionId == organizationGatewayConnectionId

displayModelName :: Bool -> Text -> Text -> Text
displayModelName gatewayMode connectionId modelId
    | gatewayMode = modelId
    | otherwise = connectionId <> "/" <> modelId

displayPickerText :: Text -> Text
displayPickerText =
    Text.replace "\n" "↵" . displayTerminalText

isCurrent :: Text -> Text -> DialectId -> ModelOption -> Bool
isCurrent connectionId current dialect option =
    option.modelTarget.targetConnectionId == connectionId
        && option.modelTarget.targetModelId == current
        && option.modelTarget.targetDialect == dialect
