-- | Interactive model, effort, and account-usage choices for a CLI session.
module Agent.CLI.Session.Choices
    ( accountUsageText
    , atMay
    , effortChoice
    , modelChoice
    , showAccountUsage
    ) where

import Agent.CLI.Error (formatApiErrorInlineAt)
import Agent.CLI.ModelConfig
    ( ModelCatalog
    , builtinConnectionId
    )
import Agent.CLI.ModelPicker (pickModelWithOptions)
import Agent.CLI.Models
    ( ModelOption(..)
    , ModelTarget(..)
    , PickerState(..)
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
    , requestFullscreenChoice
    , requestFullscreenFilterChoice
    )
import Agent.CLI.Usage
    ( AccountUsageLine(..)
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
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (getCurrentTime)
import System.IO (stdout)

modelChoice
    :: ModelCatalog
    -> Maybe FullscreenRuntime
    -> Bool
    -> Text
    -> Provider
    -> Text
    -> DialectId
    -> IO (Maybe ModelOption)
modelChoice
        catalog fullscreen color connectionId provider current currentDialect =
    discoverModelOptions connectionId provider >>= \(title, discovered) ->
        case fullscreen of
            Nothing ->
                pickModelWithOptions
                    catalog discovered color connectionId provider current
                    currentDialect
            Just runtime -> do
                picker <-
                    initialPickerStateResolvedWith
                        catalog
                        discovered
                        connectionId
                        provider
                        current
                        currentDialect
                let options = picker.pickerAll
                    rows =
                        [ ( option.modelTarget.targetConnectionId
                                <> " · "
                                <> option.modelTarget.targetModelId
                                <> " · "
                                <> dialectSlug option.modelTarget.targetDialect
                          , fromMaybe "" option.modelLabel
                          )
                        | option <- options
                        ]
                requestFullscreenFilterChoice
                    runtime
                    title
                    picker.pickerIndex
                    rows
                    >>= \case
                        Just index
                            | index >= 0
                            , index < length options ->
                                pure (Just (options !! index))
                        _ -> pure Nothing

discoverModelOptions :: Text -> Provider -> IO (Text, [ModelOption])
discoverModelOptions connectionId provider
    | provider == OpenRouterProvider
    , connectionId == builtinConnectionId OpenRouterProvider =
        OpenRouterModels.fetchOpenRouterModels >>= \case
            Left _ -> pure ("Model · configured only", [])
            Right models ->
                pure
                    ( "Model · OpenRouter live"
                    , map openRouterModelOption models
                    )
    | otherwise = pure ("Model", [])

openRouterModelOption :: OpenRouterModels.OpenRouterModel -> ModelOption
openRouterModelOption model =
    (rawModelOption OpenRouterProvider model.modelId)
        { modelLabel = Just $
            Text.intercalate
                " · "
                ( [model.modelDisplayName]
                    <> maybe
                        []
                        (pure . formatContextLength)
                        model.modelContextLength
                    <> [ if model.modelSupportsTools
                            then "tools"
                            else "no tools"
                       , "OpenRouter live"
                       ]
                )
        }

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
                    "usage: ChatGPT Codex windows only (xAI/OpenRouter have no account usage API here)"

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
