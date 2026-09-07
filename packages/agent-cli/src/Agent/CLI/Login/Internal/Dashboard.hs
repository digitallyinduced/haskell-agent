module Agent.CLI.Login.Internal.Dashboard
    ( LoginAccountMenuAction(..)
    , LoginDashboardAction(..)
    , LoginNotice(..)
    , accountMenuTitle
    , confirmFullscreenDisconnect
    , loginAccountActionRows
    , loginAccountBody
    , loginAccountDetail
    , loginAccountMenuEntries
    , loginDashboardBody
    , loginDashboardEntries
    , loginDashboardRows
    , markdownText
    , noticeFromResult
    , refreshAllNotice
    , refreshOneNotice
    , withLoginProgress
    ) where

import Agent.CLI.Login.Internal.Accounts (isGatewayLoginAccount)
import Agent.CLI.Login.Types
    ( AccountBilling(..)
    , AccountUsage(..)
    , LoginAccount(..)
    , UsageState(..)
    , UsageWindow(..)
    )
import Agent.CLI.TUI.App
    ( FullscreenRuntime
    , emitUiEvent
    , requestFullscreenChoiceWithBody
    )
import Agent.Provider (providerSlug)
import Agent.TUI.Model (UiEvent(UiSetNotice), progressNotice)
import Control.Exception.Safe (bracket_)
import Data.Char (isControl)
import Data.Maybe (catMaybes, isJust)
import Data.Text (Text)
import qualified Data.Text as Text

data LoginNotice = LoginNotice !Bool !Text

data LoginDashboardAction
    = LoginDashboardConnect
    | LoginDashboardGateway
    | LoginDashboardRefreshAll
    | LoginDashboardOpen !Int

data LoginAccountMenuAction
    = LoginAccountRefresh
    | LoginAccountToggle
    | LoginAccountImport
    | LoginAccountDisconnect
    | LoginAccountBack

loginDashboardRows :: [LoginAccount] -> [(Text, Text)]
loginDashboardRows = map snd . loginDashboardEntries

loginDashboardEntries
    :: [LoginAccount]
    -> [(LoginDashboardAction, (Text, Text))]
loginDashboardEntries accounts =
    [ ( LoginDashboardConnect
      , ( "＋ Connect account"
        , "OpenAI, xAI / Grok, OpenRouter, Google Gemini, or Claude Code"
        )
      )
    , ( LoginDashboardGateway
      , if gatewayConnected
            then
                ( "↻ Reconnect platform gateway"
                , "Replace the saved platform.digitallyinduced.com authorization"
                )
            else
                ( "＋ Log in to platform gateway"
                , "Route requests through platform.digitallyinduced.com"
                )
      )
    ]
        <> refreshEntry
        <> zipWith
            (\index account ->
                (LoginDashboardOpen index, loginDashboardAccountRow account))
            [0 ..]
            accounts
  where
    gatewayConnected = any isGatewayLoginAccount accounts
    refreshEntry
        | not (any (not . isGatewayLoginAccount) accounts) = []
        | otherwise =
            [ ( LoginDashboardRefreshAll
              , ( "↻ Refresh all usage"
                , Text.pack
                    (show
                        (length
                            (filter
                                (not . isGatewayLoginAccount)
                                accounts)))
                    <> " accounts"
                )
              )
            ]

loginDashboardAccountRow :: LoginAccount -> (Text, Text)
loginDashboardAccountRow account =
    ( accountHealthGlyph account
        <> providerSlug account.loginProvider
        <> "  "
        <> displayText 48 account.loginLabel
    , Text.intercalate " · " $
        [ accountIdentifier account
        , if account.loginSource == "gateway"
            then "gateway"
            else if isJust account.loginManagedId then "managed" else "external"
        , if account.loginEnabled then "enabled" else "disabled"
        , billingSummary account.loginBilling
        , if isGatewayLoginAccount account
            then case account.loginUsage of
                UsageUnavailable _ -> "needs repair"
                _ -> "connected"
            else usageSummary account.loginUsage
        ]
    )

accountIdentifier :: LoginAccount -> Text
accountIdentifier account
    | Text.null (Text.strip account.loginAccountId) = "unknown account"
    | otherwise = displayText 48 account.loginAccountId

loginDashboardBody :: Maybe LoginNotice -> [LoginAccount] -> Text
loginDashboardBody notice accounts =
    Text.intercalate "\n\n" $
        [ "Manage credentials and inspect provider usage without leaving the fullscreen UI."
        , "**" <> Text.pack (show (length providerAccounts)) <> " accounts**"
            <> " · " <> Text.pack (show managedCount) <> " managed"
            <> " · " <> Text.pack (show enabledCount) <> " enabled"
        , gatewaySummary
        ]
            <> maybe [] (pure . formatLoginNotice) notice
            <> [ if null accounts
                    then "No credentials were found. Connect an account to get started."
                    else "Select an account for usage details, importing, enable/disable, or disconnect."
               ]
  where
    providerAccounts = filter (not . isGatewayLoginAccount) accounts
    managedCount =
        length (filter (isJust . (.loginManagedId)) providerAccounts)
    enabledCount = length (filter (.loginEnabled) providerAccounts)
    gatewaySummary =
        case filter isGatewayLoginAccount accounts of
            gateway : _ ->
                "**Gateway:** " <> markdownText 160 gateway.loginAccountId
            [] ->
                "**Gateway:** not connected"

loginAccountActionRows :: LoginAccount -> [(Text, Text)]
loginAccountActionRows = map snd . loginAccountMenuEntries

loginAccountMenuEntries
    :: LoginAccount
    -> [(LoginAccountMenuAction, (Text, Text))]
loginAccountMenuEntries account =
    refreshEntries
        <> managementEntries
        <> [(LoginAccountBack, ("← Back to accounts", "Return to the dashboard"))]
  where
    refreshEntries
        | isGatewayLoginAccount account = []
        | otherwise =
            [ ( LoginAccountRefresh
              , ("↻ Refresh usage", "Fetch the latest provider limits")
              )
            ]
    managementEntries
        | isGatewayLoginAccount account =
            [ ( LoginAccountDisconnect
              , ("− Disconnect gateway", "Stop routing OpenAI requests through the gateway")
              )
            ]
        | otherwise = case account.loginManagedId of
        Just _ ->
            [ ( LoginAccountToggle
              , ( if account.loginEnabled
                    then "○ Disable credential"
                    else "● Enable credential"
                , if account.loginEnabled
                    then "Keep it stored but exclude it from selection"
                    else "Make it available for provider selection"
                )
              )
            , ( LoginAccountDisconnect
              , ("− Disconnect credential", "Delete it from the managed store")
              )
            ]
        Nothing
            | Text.null account.loginSecretPayload -> []
            | otherwise ->
                [ ( LoginAccountImport
                  , ("⇥ Import credential", "Copy it into the managed store")
                  )
                ]

accountMenuTitle :: LoginAccount -> Text
accountMenuTitle account =
    providerSlug account.loginProvider
        <> " · "
        <> displayText 48 account.loginLabel

loginAccountBody :: Maybe LoginNotice -> LoginAccount -> Text
loginAccountBody notice account =
    Text.intercalate "\n\n" $
        maybe [] (pure . formatLoginNotice) notice
            <> [loginAccountDetail account]

loginAccountDetail :: LoginAccount -> Text
loginAccountDetail account
    | isGatewayLoginAccount account =
        Text.intercalate "\n"
            [ "**" <> markdownText 100 account.loginLabel <> "**"
            , ""
            , "Gateway URL: "
                <> markdownText 160 account.loginAccountId
            , "Status: " <> gatewayStatus
            , ""
            , "The gateway controls provider, model, and account routing for "
                <> "this organization. Local accounts are unavailable until "
                <> "the gateway is disconnected."
            ]
      where
        gatewayStatus = case account.loginUsage of
            UsageUnavailable _ -> "saved credential is unreadable"
            _ -> "saved"
loginAccountDetail account =
    Text.intercalate "\n" $
        [ "**" <> markdownText 100 account.loginLabel <> "**"
        , ""
        , "Provider: " <> providerSlug account.loginProvider
        , "Account: " <> markdownText 120 accountId
        , "Source: " <> markdownText 160 account.loginSource
        , "Storage: " <>
            if isJust account.loginManagedId then "managed" else "external (read-only)"
        , "Status: " <>
            if account.loginEnabled then "enabled" else "disabled"
        , "Billing: " <> billingSummary account.loginBilling
        , ""
        , "**Usage**"
        ]
            <> usageDetail account.loginUsage
  where
    accountId
        | Text.null account.loginAccountId = "(unknown)"
        | otherwise = account.loginAccountId

usageDetail :: UsageState -> [Text]
usageDetail = \case
    UsageNotChecked ->
        ["Not checked yet. Choose **Refresh usage** to fetch current limits."]
    UsageUnavailable err ->
        ["⚠ " <> markdownText 500 ("Usage unavailable · " <> err)]
    UsageAvailable usage ->
        planLines <> windowLines <> creditDetail <> fallback
      where
        planLines = maybe [] (\plan -> ["Plan: " <> markdownText 80 plan])
            usage.usagePlan
        windowLines = concatMap formatUsageWindow usage.usageWindows
        creditDetail =
            catMaybes
                [ ("Credits remaining: " <>) . markdownText 80
                    <$> usage.creditsRemaining
                , ("Credits used: " <>) . markdownText 80
                    <$> usage.creditsUsed
                ]
        fallback
            | null planLines && null windowLines && null creditDetail =
                ["Usage is available; this provider reported no active limits."]
            | otherwise = []

formatUsageWindow :: UsageWindow -> [Text]
formatUsageWindow window =
    [ "- " <> markdownText 80 window.windowName
        <> "  `" <> usageBar window.usedPercent <> "`  "
        <> Text.pack (show window.usedPercent) <> "% used"
    , "  Resets " <> Text.pack (show window.resetsAt)
    ]

usageBar :: Int -> Text
usageBar usedPercent =
    Text.replicate filled "█" <> Text.replicate (10 - filled) "░"
  where
    clamped = max 0 (min 100 usedPercent)
    filled = (clamped + 5) `div` 10

accountHealthGlyph :: LoginAccount -> Text
accountHealthGlyph account
    | isGatewayLoginAccount account = case account.loginUsage of
        UsageUnavailable _ -> "✗ "
        _ -> "✓ "
    | not account.loginEnabled = "○ "
    | otherwise = case account.loginUsage of
        UsageNotChecked -> "◌ "
        UsageUnavailable _ -> "✗ "
        UsageAvailable _ -> "✓ "

billingSummary :: AccountBilling -> Text
billingSummary = \case
    SubscriptionBilling plan ->
        "subscription" <> maybe "" (" / " <>) plan
    ApiCreditsBilling -> "API credits"

usageSummary :: UsageState -> Text
usageSummary = \case
    UsageNotChecked -> "usage not checked"
    UsageUnavailable _ -> "usage unavailable"
    UsageAvailable usage ->
        case usage.usageWindows of
            window : _ ->
                Text.pack (show window.usedPercent)
                    <> "% " <> window.windowName
            [] -> case usage.creditsRemaining of
                Just remaining -> remaining <> " remaining"
                Nothing -> maybe "usage available" id usage.usagePlan

displayText :: Int -> Text -> Text
displayText limit =
    Text.take limit
        . Text.unwords
        . Text.words
        . Text.map (\character -> if isControl character then ' ' else character)

markdownText :: Int -> Text -> Text
markdownText limit =
    Text.concatMap escape . displayText limit
  where
    escape character
        | character `elem` ("\\`*_[]<>" :: String) =
            "\\" <> Text.singleton character
        | otherwise = Text.singleton character

formatLoginNotice :: LoginNotice -> Text
formatLoginNotice (LoginNotice successful message) =
    (if successful then "✅ " else "⚠️ ")
        <> markdownText 500 message

noticeFromResult :: Either Text Text -> LoginNotice
noticeFromResult = \case
    Left err -> LoginNotice False err
    Right message -> LoginNotice True message

refreshOneNotice :: LoginAccount -> LoginNotice
refreshOneNotice account = case account.loginUsage of
    UsageUnavailable err -> LoginNotice False err
    _ -> LoginNotice True "Usage refreshed."

refreshAllNotice :: [LoginAccount] -> LoginNotice
refreshAllNotice accounts
    | unavailable == 0 =
        LoginNotice True
            ("Usage refreshed for " <> count <> " accounts.")
    | otherwise =
        LoginNotice False
            ("Usage refreshed; " <> Text.pack (show unavailable)
                <> " of " <> count <> " accounts were unavailable.")
  where
    providerAccounts = filter (not . isGatewayLoginAccount) accounts
    count = Text.pack (show (length providerAccounts))
    unavailable = length
        [()
        | account <- providerAccounts
        , UsageUnavailable _ <- [account.loginUsage]
        ]

withLoginProgress :: FullscreenRuntime -> Text -> IO a -> IO a
withLoginProgress runtime message =
    bracket_
        (emitUiEvent runtime
            (UiSetNotice (Just (progressNotice message))))
        (emitUiEvent runtime (UiSetNotice Nothing))

confirmFullscreenDisconnect
    :: FullscreenRuntime
    -> LoginAccount
    -> IO Bool
confirmFullscreenDisconnect runtime account = do
    choice <-
        requestFullscreenChoiceWithBody
            runtime
            "Disconnect credential?"
            ( if account.loginSource == "gateway"
                then "Disconnect **" <> markdownText 100 account.loginLabel
                    <> "**?\n\nThe saved gateway credential will be removed."
                else "Delete **" <> markdownText 100 account.loginLabel
                    <> "** from the managed credential store?\n\n"
                    <> "This cannot be undone."
            )
            1
            [ ("Disconnect", if account.loginSource == "gateway"
                then "Remove the saved gateway connection"
                else "Delete the stored credential")
            , ("Keep credential", "Return without changing anything")
            ]
    pure (choice == Just 0)
