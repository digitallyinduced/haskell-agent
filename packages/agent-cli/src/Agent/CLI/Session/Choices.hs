-- | Interactive model, effort, and account-usage choices for a CLI session.
module Agent.CLI.Session.Choices
    ( accountUsageText
    , atMay
    , effortChoice
    , modelChoice
    , modelChoiceWithEffort
    , showAccountUsage
    , titleModelChoice
    , titleModelChoiceIndex
    , titleModelChoiceRows
    , resolveTitleModelChoice
    ) where

import Agent.Runtime.Error (formatApiErrorInlineAt)
import Agent.Runtime.Session.TitleModel (TitleModelSetting(..))
import Agent.CLI.Picker (PickerKey(..), runOverlay)
import Agent.TUI.TextWidth (displayTerminalText)
import Agent.Runtime.GatewayClient
    ( GatewayModelAccess
    , cachedGatewayModels
    , cachedGatewayUsage
    , fetchGatewayUsage
    , refreshGatewayModels
    )
import Agent.CLI.GatewayModels
    ( modelOptionsForGatewayModels
    , selectGatewayModelOption
    )
import Agent.Runtime.ModelConfig
    ( ModelCatalog
    , builtinConnectionId
    , organizationGatewayConnectionId
    )
import Agent.CLI.ModelPicker
    ( ModelPickerSelection(..)
    , initialModelEffort
    , modelEffortOptions
    , pickModelStateWithEffortAndUsage
    , pickModelStateWithUpdates
    , renderEffortIndicator
    )
import Agent.Runtime.Models
    ( ModelOption(..)
    , ModelTarget(..)
    , PickerState(..)
    , initialPickerStateForOptions
    , initialPickerStateResolvedWith
    , rawModelOption
    )
import Agent.Concurrent (mapConcurrentlyBounded)
import Agent.ReasoningEffort
    ( ReasoningEffort
    , reasoningEffortText
    )
import Agent.CLI.Style
    ( roleError
    , roleMuted
    , rolePrompt
    )
import Agent.CLI.Terminal (resolveColor)
import Agent.CLI.TUI.App
    ( FullscreenRuntime
    , requestFullscreenAdjustableFilterChoice
    , requestFullscreenDynamicAdjustableFilterChoice
    , requestFullscreenChoice
    )
import Agent.CLI.Options (defaultEffortFor)
import Agent.CLI.Usage
    ( AccountUsageLine(..)
    , formatModelUsageSummary
    , formatUsageReport
    )
import Agent.Claude
    ( ClaudeCodeAuth(..)
    , ClaudeCodeTransport(..)
    , loadClaudeCodeAuth
    )
import Agent.Dialect
    ( DialectId
    , dialectSlug
    )
import qualified Agent.OpenAI.Auth as OpenAI
import Agent.OpenAI.Usage (fetchUsage)
import qualified Agent.OpenRouter.Models as OpenRouterModels
import Agent.Provider
    ( Credential(..)
    , Provider(..)
    , TokenProvider
    , getNextToken
    )
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import Control.Monad (join, unless, void)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.List (elemIndex)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (getCurrentTime)
import System.IO (hIsTerminalDevice, stdin, stderr, stdout)
import System.Timeout (timeout)

-- | The local Apple helper is a title backend, not a provider wire model.
-- Keep it outside the ordinary model catalog and gateway validation path.
titleModelChoiceRows :: Maybe TitleModelSetting -> [(Text, Text)]
titleModelChoiceRows setting =
    [ ("Auto (default)", "Prefer Apple Foundation; fall back to a low-cost provider model")
    , ("Apple Foundation", "On-device Apple Intelligence · requires availability on this Mac")
    , ("Provider model…", case setting of
            Just (TitleModelPinned target) -> "Current: " <> target.targetModelId
            _ -> "Choose a model from your configured providers")
    ]

titleModelChoiceIndex :: Maybe TitleModelSetting -> Int
titleModelChoiceIndex = \case
    Nothing -> 0
    Just TitleModelAppleFoundation -> 1
    Just TitleModelPinned{} -> 2

-- | Outer Nothing means cancellation; inner Nothing restores automatic choice.
resolveTitleModelChoice
    :: IO (Either Text (Maybe ModelOption))
    -> Maybe Int
    -> IO (Either Text (Maybe (Maybe TitleModelSetting)))
resolveTitleModelChoice providerChoice = \case
    Just 0 -> pure (Right (Just Nothing))
    Just 1 -> pure (Right (Just (Just TitleModelAppleFoundation)))
    Just 2 ->
        fmap (fmap (fmap (Just . TitleModelPinned . (.modelTarget))))
            providerChoice
    _ -> pure (Right Nothing)

titleModelChoice
    :: Maybe FullscreenRuntime
    -> Bool
    -> Maybe TitleModelSetting
    -> IO (Either Text (Maybe ModelOption))
    -> IO (Either Text (Maybe (Maybe TitleModelSetting)))
titleModelChoice fullscreen color setting providerChoice = do
    let rows = titleModelChoiceRows setting
        initial = titleModelChoiceIndex setting
        render selected = Text.unlines $
            [rolePrompt color "Session title model", ""]
                <> concat
                    [ [ (if index == selected then rolePrompt color "› " else "  ")
                            <> label
                            <> if index == initial then " ✓" else ""
                      , "    " <> roleMuted color (Text.replace "\n" "↵" (displayTerminalText detail))
                      ]
                    | (index, (label, detail)) <- zip [0..] rows
                    ]
                <> ["", roleMuted color "↑/↓ move · Enter select · Esc cancel"]
        step key selected = case key of
            PickerKeyUp -> Right (max 0 (selected - 1))
            PickerKeyDown -> Right (min (length rows - 1) (selected + 1))
            PickerKeyConfirm -> Left (Just selected)
            PickerKeyCancel -> Left Nothing
            _ -> Right selected
    selected <- case fullscreen of
        Nothing -> do
            interactive <- (&&) <$> hIsTerminalDevice stdin <*> hIsTerminalDevice stderr
            if interactive
                then join <$> runOverlay render step initial
                else Text.hPutStrLn stderr (render initial) >> pure Nothing
        Just runtime ->
            requestFullscreenChoice runtime "Session title model" initial rows
    resolveTitleModelChoice providerChoice selected

modelChoice
    :: ModelCatalog
    -> Maybe GatewayModelAccess
    -> Maybe FullscreenRuntime
    -> Bool
    -> Text
    -> Provider
    -> Text
    -> DialectId
    -> IO (Either Text (Maybe ModelOption))
modelChoice
        catalog gatewayAccess fullscreen color connectionId provider current
        currentDialect =
    fmap (fmap (fmap (.modelPickerOption))) $
        modelChoiceWithEffort
            catalog
            gatewayAccess
            fullscreen
            color
            connectionId
            provider
            current
            currentDialect
            (defaultEffortFor provider)

-- | Choose a model and its reasoning effort in one searchable surface.
modelChoiceWithEffort
    :: ModelCatalog
    -> Maybe GatewayModelAccess
    -> Maybe FullscreenRuntime
    -> Bool
    -> Text
    -> Provider
    -> Text
    -> DialectId
    -> ReasoningEffort
    -> IO (Either Text (Maybe ModelPickerSelection))
modelChoiceWithEffort
        catalog
        gatewayAccess
        fullscreen
        color
        connectionId
        provider
        current
        currentDialect
        currentEffort =
    case gatewayAccess of
        Just access -> chooseGateway access >>= \case
            Right (Just selection) ->
                -- Cached rows are presentation hints, not routing authority.
                -- Validate only after confirmation so opening and cancelling
                -- remain immediate, even while catalog requests are blocked.
                refreshGatewayModels access >>= \case
                    Left err -> pure (Left err)
                    Right models ->
                        pure $ do
                            option <- selectGatewayModelOption
                                (modelOptionsForGatewayModels catalog models)
                                (Just selection.modelPickerOption.modelTarget.targetModelId)
                                Nothing
                                []
                            Right (Just selection { modelPickerOption = option })
            result -> pure result
        Nothing -> do
            discovered <- discoverModelOptions connectionId provider
            picker <-
                initialPickerStateResolvedWith
                    catalog discovered connectionId provider current currentDialect
            Right <$> presentPicker "Models" picker Map.empty
  where
    gatewayTitle = "Models · organization gateway"
    emptyGatewayMessage = "The organization gateway does not offer any models."

    gatewayPicker models =
        initialPickerStateForOptions
            "organization gateway"
            (modelOptionsForGatewayModels catalog models)
            organizationGatewayConnectionId provider current currentDialect

    chooseGateway access = do
        cached <- cachedGatewayModels access
        case fullscreen of
            Nothing -> do
                picker <- gatewayPicker (fromMaybe [] cached)
                usage <- cachedGatewayModelUsage access picker.pickerAll
                let notice = case cached of
                        Nothing -> "Loading models…"
                        Just [] -> emptyGatewayMessage
                        Just _ -> ""
                state <- newMVar (picker, usage, notice)
                let refresh publish = do
                        let update transform = modifyMVar_ state \previous -> do
                                let next = transform previous
                                publish next
                                pure next
                            updateUsage values = update \(models, previous, message) ->
                                (models, Map.union values previous, message)
                            refreshCatalog = refreshGatewayModels access >>= \case
                                Left err -> do
                                    retained <- cachedGatewayModels access
                                    case retained of
                                        Nothing -> do
                                            empty <- gatewayPicker []
                                            update \_ -> (empty, Map.empty, err)
                                        Just _ -> update \(models, values, _) ->
                                            (models, values, err <> " Showing cached models.")
                                Right models -> do
                                    refreshed <- gatewayPicker models
                                    let added = filter
                                            (\option -> all
                                                ((/= option.modelTarget) . (.modelTarget))
                                                picker.pickerAll)
                                            refreshed.pickerAll
                                    update \(_, values, _) ->
                                        (refreshed, values,
                                            if null refreshed.pickerAll then emptyGatewayMessage else "")
                                    refreshGatewayModelUsage access added updateUsage
                        concurrently_ refreshCatalog
                            (refreshGatewayModelUsage access picker.pickerAll updateUsage)
                        Just <$> readMVar state
                Right <$> pickModelStateWithUpdates
                    color currentEffort notice usage picker refresh
            Just runtime -> do
                initialPicker <- gatewayPicker (fromMaybe [] cached)
                initialUsage <- cachedGatewayModelUsage access initialPicker.pickerAll
                let initialBody = case cached of
                        Nothing -> "Loading models…"
                        Just [] -> emptyGatewayMessage
                        Just _ -> ""
                    optionEntries picker =
                        Map.fromList
                            [(modelOptionKey option, option) | option <- picker.pickerAll]
                -- Retain every displayed identity until the reply is consumed:
                -- selection may already be queued when a refresh removes a row.
                registry <- newIORef (optionEntries initialPicker)
                state <- newMVar (initialPicker, initialUsage, initialBody)
                let refresh publish = do
                        let update transform =
                                modifyMVar_ state \previous -> do
                                    let next@(picker, usage, body) = transform previous
                                    modifyIORef' registry (Map.union (optionEntries picker))
                                    publish body (dynamicRows picker usage)
                                    pure next
                            updateUsage usage =
                                update \(picker, previousUsage, body) ->
                                    (picker, Map.union usage previousUsage, body)
                            refreshCatalog =
                                refreshGatewayModels access >>= \case
                                    Left err -> do
                                        retained <- cachedGatewayModels access
                                        case retained of
                                            Nothing -> do
                                                emptyPicker <- gatewayPicker []
                                                update \_ -> (emptyPicker, Map.empty, err)
                                            Just _ ->
                                                update \(picker, usage, _) ->
                                                    (picker, usage, err <> " Showing cached models.")
                                    Right models -> do
                                        picker <- gatewayPicker models
                                        let body = if null picker.pickerAll
                                                then emptyGatewayMessage else ""
                                            initialKeys = optionEntries initialPicker
                                            added =
                                                filter
                                                    (\option -> Map.notMember (modelOptionKey option) initialKeys)
                                                    picker.pickerAll
                                        update \(_, usage, _) -> (picker, usage, body)
                                        refreshGatewayModelUsage access added updateUsage
                        concurrently_
                            refreshCatalog
                            (refreshGatewayModelUsage access initialPicker.pickerAll updateUsage)
                selected <-
                    requestFullscreenDynamicAdjustableFilterChoice
                        runtime gatewayTitle initialBody initialPicker.pickerIndex
                        (dynamicRows initialPicker initialUsage)
                        refresh
                options <- readIORef registry
                pure $ Right do
                    (key, effortIndex) <- selected
                    option <- Map.lookup key options
                    effort <- atMay effortIndex (modelEffortOptions option)
                    pure ModelPickerSelection
                        { modelPickerOption = option
                        , modelPickerEffort = effort
                        }

    dynamicRows picker usage =
        [ (modelOptionKey option, label, detail, efforts, initialIndex)
        | option <- picker.pickerAll
        , let (label, detail, efforts, initialIndex) = row picker usage option
        ]

    row picker usage option =
        let efforts = modelEffortOptions option
            initial = initialModelEffort picker currentEffort option
            initialIndex = fromMaybe 0 (elemIndex initial efforts)
            usageText = Map.lookup option.modelTarget.targetModelId usage
            withUsage separator text =
                text <> maybe "" (separator <>) usageText
        in ( withUsage "  " (modelRowLabel picker option)
           , withUsage "\nUsage: " (modelDetail picker option)
           , map (renderEffortIndicator option) efforts
           , initialIndex
           )

    presentPicker title picker usage =
        case fullscreen of
            Nothing ->
                pickModelStateWithEffortAndUsage
                    color
                    currentEffort
                    usage
                    picker
            Just runtime -> do
                let options = picker.pickerAll
                requestFullscreenAdjustableFilterChoice
                    runtime
                    title
                    picker.pickerIndex
                    (map (row picker usage) options)
                    >>= \case
                        Just (modelIndex, effortIndex)
                            | Just option <- atMay modelIndex options
                            , Just effort <-
                                atMay effortIndex (modelEffortOptions option) ->
                                    pure $ Just ModelPickerSelection
                                        { modelPickerOption = option
                                        , modelPickerEffort = effort
                                        }
                        _ -> pure Nothing

modelOptionKey :: ModelOption -> Text
modelOptionKey option =
    Text.pack (show option.modelTarget)

cachedGatewayModelUsage :: GatewayModelAccess -> [ModelOption] -> IO (Map.Map Text Text)
cachedGatewayModelUsage access options =
    Map.fromList . catMaybes <$> traverse load options
  where
    load option = do
        let modelId = option.modelTarget.targetModelId
        snapshot <- cachedGatewayUsage access modelId
        pure ((modelId,) <$> (snapshot >>= formatModelUsageSummary))

-- | Publish one usage update per bounded group, independently of the catalog.
-- Retain completed results on timeout without rebuilding every row for every
-- response (which would make presentation work quadratic in catalog size).
refreshGatewayModelUsage
    :: GatewayModelAccess
    -> [ModelOption]
    -> (Map.Map Text Text -> IO ())
    -> IO ()
refreshGatewayModelUsage access options publish = do
    completed <- newIORef Map.empty
    void $ timeout 2000000 $ mapConcurrentlyBounded 4 (loadUsage completed) options
    usage <- readIORef completed
    unless (Map.null usage) (publish usage)
  where
    loadUsage completed option = do
        let modelId = option.modelTarget.targetModelId
        fetchGatewayUsage access modelId >>= \case
            Left _ -> pure ()
            Right snapshot ->
                case formatModelUsageSummary snapshot of
                    Nothing -> pure ()
                    Just summary ->
                        atomicModifyIORef' completed \usage ->
                            (Map.insert modelId summary usage, ())

discoverModelOptions :: Text -> Provider -> IO [ModelOption]
discoverModelOptions connectionId provider
    | provider == OpenRouterProvider
    , connectionId == builtinConnectionId OpenRouterProvider =
        OpenRouterModels.fetchOpenRouterModels >>= \case
            Left _ -> pure []
            Right models -> pure (map openRouterModelOption models)
    | otherwise = pure []

openRouterModelOption :: OpenRouterModels.OpenRouterModel -> ModelOption
openRouterModelOption model =
    (rawModelOption OpenRouterProvider model.modelId)
        { modelContextWindow = model.modelContextLength
        , modelLabel = Just $
            Text.intercalate
                " · "
                ( [ model.modelDisplayName
                  , if model.modelSupportsTools
                            then "tools"
                            else "no tools"
                  , "OpenRouter live"
                  ]
                )
        }

modelRowLabel :: PickerState -> ModelOption -> Text
modelRowLabel picker option =
    ( if picker.pickerConnectionId == organizationGatewayConnectionId
        then option.modelTarget.targetModelId
        else
            option.modelTarget.targetConnectionId
                <> "/"
                <> option.modelTarget.targetModelId
    )
        <> if
            option.modelTarget.targetConnectionId == picker.pickerConnectionId
                && option.modelTarget.targetModelId == picker.pickerCurrent
                && option.modelTarget.targetDialect
                    == picker.pickerCurrentDialect
            then " ✓"
            else ""

modelDetail :: PickerState -> ModelOption -> Text
modelDetail picker option =
    Text.intercalate
        " · "
        ( maybe [] pure option.modelLabel
            <> [ option.modelTarget.targetConnectionId
               | picker.pickerConnectionId
                    /= organizationGatewayConnectionId
               ]
            <> [dialectSlug option.modelTarget.targetDialect]
            <> if maybe False
                    (Text.isInfixOf "context" . Text.toCaseFold)
                    option.modelLabel
                then []
                else
                    maybe
                        []
                        (pure . formatContextLength)
                        option.modelContextWindow
        )

formatContextLength :: Int -> Text
formatContextLength contextLength
    | contextLength >= 1000000 =
        let tenths = contextLength `div` 100000
            whole = tenths `div` 10
            fraction = tenths `mod` 10
            amount
                | fraction == 0 = show whole
                | otherwise = show whole <> "." <> show fraction
        in Text.pack amount <> "M context"
    | contextLength >= 1000 =
        Text.pack (show (contextLength `div` 1000)) <> "k context"
    | otherwise = Text.pack (show contextLength) <> " context"

effortChoice
    :: Maybe FullscreenRuntime
    -> [ReasoningEffort]
    -> ReasoningEffort
    -> IO (Maybe ReasoningEffort)
effortChoice fullscreen efforts current = case fullscreen of
    Nothing -> pure Nothing
    Just runtime -> do
        let initial = fromMaybe 0 (elemIndex current efforts)
        requestFullscreenChoice
            runtime
            "Reasoning effort"
            initial
            [(reasoningEffortText effort, "") | effort <- efforts]
            >>= \case
                Just index
                    | index >= 0
                    , index < length efforts ->
                        pure (Just (efforts !! index))
                _ -> pure Nothing

atMay :: Int -> [a] -> Maybe a
atMay index values
    | index < 0 = Nothing
    | otherwise = case drop index values of
        value : _ -> Just value
        [] -> Nothing

showAccountUsage
    :: Provider
    -> Maybe TokenProvider
    -> Maybe OpenAI.Pool
    -> IO ()
showAccountUsage provider tokenProvider openAiPool = do
    color <- resolveColor stdout
    accountUsageText color provider tokenProvider openAiPool
        >>= Text.putStrLn

accountUsageText
    :: Bool
    -> Provider
    -> Maybe TokenProvider
    -> Maybe OpenAI.Pool
    -> IO Text
accountUsageText color provider tokenProvider openAiPool = do
    now <- getCurrentTime
    case provider of
        OpenAIProvider ->
            case openAiPool of
                Just pool -> do
                    snapshots <- OpenAI.snapshotAccounts pool
                    lines_ <- mapConcurrentlyBounded 4 fetchSnapshot snapshots
                    pure (formatUsageReport color now lines_)
                Nothing ->
                    case tokenProvider of
                        Just provider_ ->
                            getNextToken provider_ Nothing >>= \case
                                Left err ->
                                    pure $
                                        roleError color
                                            ("usage: "
                                                <> formatApiErrorInlineAt now err)
                                Right credential -> do
                                    result <- fetchUsage
                                        credential.accessToken credential.accountId
                                    pure $
                                        formatUsageReport color now
                                            [ AccountUsageLine
                                                { usageAccountId = credential.accountId
                                                , usageCooldownUntil = Nothing
                                                , usageResult = result
                                                }
                                            ]
                        Nothing ->
                            pure $
                                roleMuted color
                                    "usage: no OpenAI credentials loaded"
        ClaudeCodeProvider ->
            loadClaudeCodeAuth >>= \case
                Left err ->
                    pure (roleError color ("usage: " <> err))
                Right auth ->
                    pure $
                        roleMuted color $ case auth.transport of
                            ClaudeCodeGateway{} ->
                                "usage: Claude gateway-managed · "
                                    <> auth.accountLabel
                            ClaudeCodeLocalSubscription ->
                                "usage: Claude Code "
                                    <> fromMaybe "subscription" auth.subscriptionType
                                    <> " · "
                                    <> auth.accountLabel
                                    <> " (run `claude /status` for live limits)"
        _ ->
            pure $
                roleMuted color
                    "usage: ChatGPT Codex windows only (xAI/OpenRouter/Gemini have no account usage API here)"

fetchSnapshot :: OpenAI.AccountSnapshot -> IO AccountUsageLine
fetchSnapshot snapshot = do
    result <- fetchUsage
        snapshot.snapshotAuth.accessToken
        snapshot.snapshotAuth.accountId
    pure AccountUsageLine
        { usageAccountId = snapshot.snapshotAuth.accountId
        , usageCooldownUntil = snapshot.snapshotCooldownUntil
        , usageResult = result
        }
