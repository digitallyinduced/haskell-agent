-- | Interactive model, effort, and account-usage choices for a CLI session.
module Agent.CLI.Session.Choices
    ( accountUsageText
    , atMay
    , effortChoice
    , modelChoice
    , showAccountUsage
    ) where

import Agent.CLI.Error (formatApiErrorInlineAt)
import Agent.CLI.ModelConfig (ModelCatalog)
import Agent.CLI.ModelPicker (pickModel)
import Agent.CLI.Models
    ( ModelOption(..)
    , ModelTarget(..)
    , PickerState(..)
    , initialPickerStateResolved
    )
import Agent.CLI.Options (reasoningEfforts)
import Agent.CLI.Style
    ( roleError
    , roleMuted
    )
import Agent.CLI.Terminal (resolveColor)
import Agent.CLI.TUI.App
    ( FullscreenRuntime
    , requestFullscreenChoice
    )
import Agent.CLI.Usage
    ( AccountUsageLine(..)
    , formatUsageReport
    )
import Agent.Claude
    ( ClaudeCodeAuth(..)
    , loadClaudeCodeAuth
    )
import Agent.Dialect
    ( DialectId
    , dialectSlug
    )
import qualified Agent.OpenAI.Auth as OpenAI
import Agent.OpenAI.Usage (fetchUsage)
import Agent.Provider
    ( Credential(..)
    , Provider(..)
    , TokenProvider
    , getNextToken
    )
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
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
    case fullscreen of
    Nothing ->
        pickModel
            catalog color connectionId provider current currentDialect
    Just runtime -> do
        picker <-
            initialPickerStateResolved
                catalog connectionId provider current currentDialect
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
        requestFullscreenChoice
            runtime
            "Model"
            picker.pickerIndex
            rows
            >>= \case
                Just index
                    | index >= 0
                    , index < length options ->
                        pure (Just (options !! index))
                _ -> pure Nothing

effortChoice
    :: Maybe FullscreenRuntime
    -> Text
    -> IO (Maybe Text)
effortChoice fullscreen current = case fullscreen of
    Nothing -> pure Nothing
    Just runtime -> do
        let efforts = reasoningEfforts
            initial = fromMaybe 0 (elemIndex current efforts)
        requestFullscreenChoice
            runtime
            "Reasoning effort"
            initial
            [(effort, "") | effort <- efforts]
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
                    lines_ <- mapM fetchSnapshot snapshots
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
                        roleMuted color $
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
