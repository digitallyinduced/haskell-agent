-- | Interactive model, effort, and account-usage choices for a CLI session.
module Agent.CLI.Session.Choices
    ( accountUsageText
    , atMay
    , effortChoice
    , modelChoice
    , modelChoiceWithEffort
    , showAccountUsage
    ) where

import Agent.CLI.Error (formatApiErrorInlineAt)
import Agent.CLI.GatewayClient
    ( GatewayModelAccess
    , fetchGatewayUsage
    , refreshGatewayModels
    )
import Agent.CLI.GatewayModels (modelOptionsForGatewayModels)
import Agent.CLI.ModelConfig
    ( ModelCatalog
    , builtinConnectionId
    , organizationGatewayConnectionId
    )
import Agent.CLI.ModelPicker
    ( ModelPickerSelection(..)
    , initialModelEffort
    , modelEffortOptions
    , pickModelStateWithEffortAndUsage
    , renderEffortIndicator
    )
import Agent.CLI.Models
    ( ModelOption(..)
    , ModelTarget(..)
    , PickerState(..)
    , gatewayModelOptions
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
    )
import Agent.CLI.Terminal (resolveColor)
import Agent.CLI.TUI.App
    ( FullscreenRuntime
    , requestFullscreenAdjustableFilterChoice
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
import Data.List (elemIndex)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (getCurrentTime)
import System.IO (stdout)

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
        currentEffort = do
    scopedPicker >>= \case
        Left err -> pure (Left err)
        Right (title, picker, usage) ->
            Right <$> presentPicker title picker usage
  where
    scopedPicker =
        case gatewayAccess of
            Just access ->
                refreshGatewayModels access >>= \case
                    Left err -> pure (Left err)
                    Right [] ->
                        pure
                            (Left
                                "The organization gateway does not offer any models.")
                    Right models -> do
                        let gatewayConnectionId =
                                organizationGatewayConnectionId
                            options =
                                modelOptionsForGatewayModels catalog models
                        picker <-
                            initialPickerStateForOptions
                                "organization gateway"
                                options
                                gatewayConnectionId
                                OpenAIProvider
                                current
                                currentDialect
                        usage <- loadGatewayModelUsage access options
                        pure
                            (Right
                                ( "Models · organization gateway"
                                , picker
                                , usage
                                ))
            Nothing -> do
                discovered <- discoverModelOptions connectionId provider
                picker <-
                    initialPickerStateResolvedWith
                        catalog
                        discovered
                        connectionId
                        provider
                        current
                        currentDialect
                pure (Right ("Models", picker, Map.empty))

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
                    row option =
                        let efforts = modelEffortOptions option
                            initial =
                                initialModelEffort picker currentEffort option
                            initialIndex =
                                fromMaybe 0 (elemIndex initial efforts)
                            usageText =
                                Map.lookup
                                    option.modelTarget.targetModelId
                                    usage
                            withUsage separator text =
                                text <> maybe "" (separator <>) usageText
                        in ( withUsage "  " (modelRowLabel picker option)
                           , withUsage "\nUsage: " (modelDetail picker option)
                           , map (renderEffortIndicator option) efforts
                           , initialIndex
                           )
                requestFullscreenAdjustableFilterChoice
                    runtime
                    title
                    picker.pickerIndex
                    (map row options)
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

loadGatewayModelUsage
    :: GatewayModelAccess
    -> [ModelOption]
    -> IO (Map.Map Text Text)
loadGatewayModelUsage access options =
    Map.fromList . concat
        <$> mapConcurrentlyBounded 4 loadUsage options
  where
    loadUsage option = do
        let modelId = option.modelTarget.targetModelId
        fetchGatewayUsage access modelId >>= \case
            Left _ -> pure []
            Right snapshot ->
                pure
                    [ (modelId, summary)
                    | summary <- maybeToList (formatModelUsageSummary snapshot)
                    ]

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
