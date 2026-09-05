module Agent.CLI.Login.Internal.Formatting
    ( formatLoginAccounts
    , renderLoginFrame
    ) where

import Agent.CLI.Login.Internal.Accounts (isGatewayLoginAccount)
import Agent.CLI.Login.Types
    ( AccountBilling(..)
    , AccountUsage(..)
    , LoginAccount(..)
    , LoginState(..)
    , UsageState(..)
    , UsageWindow(..)
    , initialLoginState
    )
import Agent.CLI.Style
    ( glyphErr
    , glyphOk
    , roleError
    , roleMuted
    , rolePrompt
    , roleSuccess
    , roleWarn
    )
import Agent.Provider (providerSlug)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text

formatLoginAccounts :: Bool -> [LoginAccount] -> Text
formatLoginAccounts color accounts =
    renderLoginFrame color (initialLoginState accounts)

renderLoginFrame :: Bool -> LoginState -> Text
renderLoginFrame color state =
    Text.intercalate "\n" $
        [ rolePrompt color "credentials"
            <> roleMuted color
                (" · " <> Text.pack (show (length state.loginAccounts))
                    <> " connected")
        ]
            <> body
            <> [ roleMuted color
                    "↑↓/jk or scroll · click/enter refresh · a add · g gateway · i import · e enable/disable · d disconnect · esc/q"
               ]
  where
    body
        | null state.loginAccounts =
            [ roleWarn color "No credentials found."
            , roleMuted color
                "Press g to log in to platform.digitallyinduced.com, or a to connect a provider account."
            ]
        | otherwise =
            concat $
                zipWith
                    (renderAccount color state.loginIndex)
                    [0 ..]
                    state.loginAccounts

renderAccount :: Bool -> Int -> Int -> LoginAccount -> [Text]
renderAccount color selected index account =
    [ cursor
        <> health
        <> provider
        <> " · "
        <> label
        <> " · "
        <> billing
        <> enabledSuffix
    , "    "
        <> roleMuted color
            (accountId <> " · " <> account.loginSource)
    ]
        <> usageLines
  where
    cursor = if selected == index then roleWarn color "› " else "  "
    health
        | isGatewayLoginAccount account = case account.loginUsage of
            UsageUnavailable _ -> roleError color glyphErr
            _ -> roleSuccess color glyphOk
        | not account.loginEnabled = roleWarn color "○ "
        | otherwise = case account.loginUsage of
            UsageNotChecked -> roleMuted color "◌ "
            UsageUnavailable _ -> roleError color glyphErr
            UsageAvailable _ -> roleSuccess color glyphOk
    provider = rolePrompt color (providerSlug account.loginProvider)
    label = roleMuted color account.loginLabel
    billing = roleMuted color case account.loginBilling of
        SubscriptionBilling plan ->
            "subscription" <> maybe "" (" · " <>) plan
        ApiCreditsBilling -> "API credits"
    enabledSuffix
        | account.loginEnabled = ""
        | otherwise = roleWarn color " · disabled"
    accountId
        | Text.null account.loginAccountId = "(unknown account)"
        | otherwise = Text.take 24 account.loginAccountId
    usageLines
        | isGatewayLoginAccount account = case account.loginUsage of
            UsageUnavailable err ->
                [ "    "
                    <> roleError color
                        ("gateway needs repair · " <> Text.take 100 err)
                ]
            _ -> ["    " <> roleMuted color "gateway connected"]
        | otherwise = case account.loginUsage of
            UsageNotChecked ->
                ["    " <> roleMuted color "checking usage…"]
            UsageUnavailable err ->
                [ "    "
                    <> roleError color
                        ("usage unavailable · " <> Text.take 100 err)
                ]
            UsageAvailable usage ->
                map ("    " <>) $
                    map (roleMuted color . formatWindow) usage.usageWindows
                        <> creditLines usage

formatWindow :: UsageWindow -> Text
formatWindow window =
    window.windowName
        <> " "
        <> Text.pack (show window.usedPercent)
        <> "% used · resets "
        <> Text.pack (show window.resetsAt)

creditLines :: AccountUsage -> [Text]
creditLines usage =
    catMaybes
        [ ("credits remaining " <>) <$> usage.creditsRemaining
        , ("used " <>) <$> usage.creditsUsed
        ]
