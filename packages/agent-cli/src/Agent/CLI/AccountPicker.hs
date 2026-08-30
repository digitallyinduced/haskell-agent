module Agent.CLI.AccountPicker
    ( AccountPickerOption(..)
    , accountBillingMode
    , accountPickerMatches
    , accountPickerMatchesRequest
    , accountPickerRow
    , formatLoginUsageSummary
    , loadAllAccountPickerOptions
    ) where

import Agent.CLI.Login
    ( AccountBilling(..)
    , AccountUsage(..)
    , LoginAccount(..)
    , UsageState(..)
    , UsageWindow(..)
    , discoverSelectableLoginAccounts
    , loginAccountSelectionId
    , refreshLoginAccount
    )
import Agent.CLI.Usage (formatDuration)
import Agent.Claude
    ( ClaudeCodeAuth(..)
    , loadClaudeCodeAuth
    )
import Agent.Provider
    ( BillingMode(..)
    , Provider(..)
    , providerSlug
    )
import Control.Concurrent.Async (mapConcurrently)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)

data AccountPickerOption
    = AccountPickerAccount
        !Provider
        !BillingMode
        !Text
        !Text
        !Text
        !Text
    | AccountPickerConnect !Provider

loadAllAccountPickerOptions :: Provider -> IO [AccountPickerOption]
loadAllAccountPickerOptions currentProvider = do
    discovered <- discoverSelectableLoginAccounts
    refreshed <- mapConcurrently refreshLoginAccount discovered
    claudeAuth <- loadClaudeCodeAuth
    now <- getCurrentTime
    let providerAccounts =
            [ AccountPickerAccount
                account.loginProvider
                (accountBillingMode account.loginProvider account.loginBilling)
                (loginAccountSelectionId account)
                account.loginAccountId
                account.loginLabel
                (formatLoginUsageSummary account.loginProvider now account)
            | account <- deduplicateAccounts refreshed
            , account.loginProvider /= ClaudeCodeProvider
            ]
        claudeAccounts = case claudeAuth of
            Left _ -> []
            Right auth ->
                [ AccountPickerAccount
                    ClaudeCodeProvider
                    SubscriptionBilled
                    "claude-code"
                    "claude-code"
                    auth.accountLabel
                    (Text.intercalate " · " $
                        ["subscription"]
                            <> maybeToList auth.subscriptionType
                            <> ["usage via `claude /status`"])
                ]
        ordered =
            sortOn
                (\case
                    AccountPickerAccount optionProvider _ _ _ label _ ->
                        ( optionProvider /= currentProvider
                        , providerOrder optionProvider
                        , Text.toLower label
                        )
                    AccountPickerConnect optionProvider ->
                        ( True
                        , providerOrder optionProvider
                        , ""
                        ))
                (providerAccounts <> claudeAccounts)
    pure $
        ordered
            <> map AccountPickerConnect
                [ OpenAIProvider
                , XAIProvider
                , OpenRouterProvider
                , ClaudeCodeProvider
                ]
  where
    maybeToList = maybe [] pure
    deduplicateAccounts = foldr addUnique []
    addUnique account accounts
        | any (samePickerAccount account) accounts = accounts
        | otherwise = account : accounts
    samePickerAccount left right
        | left.loginProvider /= right.loginProvider = False
        | left.loginProvider == OpenAIProvider =
            left.loginAccountId == right.loginAccountId
        | otherwise =
            loginAccountSelectionId left == loginAccountSelectionId right
    providerOrder = \case
        OpenAIProvider -> 0 :: Int
        XAIProvider -> 1
        OpenRouterProvider -> 2
        ClaudeCodeProvider -> 3

accountBillingMode :: Provider -> AccountBilling -> BillingMode
accountBillingMode provider = case provider of
    OpenRouterProvider -> const ApiBilled
    ClaudeCodeProvider -> const SubscriptionBilled
    _ -> \case
        SubscriptionBilling _ -> SubscriptionBilled
        ApiCreditsBilling -> ApiBilled

formatLoginUsageSummary :: Provider -> UTCTime -> LoginAccount -> Text
formatLoginUsageSummary provider now account =
    Text.intercalate " · " $
        billing <> case account.loginUsage of
            UsageNotChecked -> []
            UsageUnavailable _ -> ["usage unavailable"]
            UsageAvailable usage ->
                maybeToList usage.usagePlan
                    <> map summarizeWindow usage.usageWindows
                    <> maybeToList
                        (("credits " <>) <$> usage.creditsRemaining)
                    <> maybeToList
                        (("used " <>) <$> usage.creditsUsed)
  where
    billing = case accountBillingMode provider account.loginBilling of
        ApiBilled -> ["API credits"]
        SubscriptionBilled -> case account.loginBilling of
            SubscriptionBilling plan ->
                maybe ["subscription"] (\value -> ["subscription", value]) plan
            ApiCreditsBilling -> ["subscription"]
    summarizeWindow window =
        window.windowName
            <> " "
            <> Text.pack (show (max 0 (100 - window.usedPercent)))
            <> "% left"
            <> if window.resetsAt > now
                then " · resets "
                    <> formatDuration (diffUTCTime window.resetsAt now)
                else ""
    maybeToList = maybe [] pure

accountPickerMatches
    :: Provider
    -> Text
    -> Text
    -> AccountPickerOption
    -> Bool
accountPickerMatches currentProvider currentSelectionId currentAccountId = \case
    AccountPickerAccount optionProvider _ selectionId accountId _ _ ->
        optionProvider == currentProvider
            && ( selectionId == currentSelectionId
                || accountId == currentAccountId
               )
    AccountPickerConnect _ -> False

-- | Restrict a Meta Console account request to the requested provider and,
-- when present, an exact case-insensitive label or id.  In particular, a
-- connect row can never satisfy a selection request.
accountPickerMatchesRequest
    :: Provider
    -> Maybe Text
    -> AccountPickerOption
    -> Bool
accountPickerMatchesRequest requestedProvider selector = \case
    AccountPickerAccount optionProvider _ selectionId accountId label _ ->
        optionProvider == requestedProvider
            && maybe
                True
                (\requested ->
                    normalize requested
                        `elem` map normalize [selectionId, accountId, label])
                selector
    AccountPickerConnect _ -> False
  where
    normalize = Text.toCaseFold . Text.strip

accountPickerRow
    :: Provider
    -> Text
    -> Text
    -> AccountPickerOption
    -> (Text, Text)
accountPickerRow currentProvider currentSelectionId currentAccountId = \case
    AccountPickerAccount
        optionProvider
        _
        selectionId
        accountId
        accountPickerLabel
        accountPickerUsage ->
            ( (if optionProvider == currentProvider
                    && ( selectionId == currentSelectionId
                        || accountId == currentAccountId
                       )
                    then "✓ "
                    else "")
                <> providerSlug optionProvider
                <> " · "
                <> accountPickerLabel
            , accountPickerUsage
            )
    AccountPickerConnect optionProvider ->
        ("＋ Connect " <> providerSlug optionProvider <> " account", "")
