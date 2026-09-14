-- | Usage-aware account selection for automatic startup and provider fallback.
module Agent.CLI.AccountSelection
    ( AccountCandidate(..)
    , PreparedProviderAccounts
    , SelectedAccount(..)
    , accountCapacity
    , loadedAuthSupportsUsageAccountSelection
    , prepareProviderAccounts
    , providerSupportsUsageAccountSelection
    , selectCandidates
    , selectAccount
    , selectPreparedProviderAccount
    , selectProviderAccount
    , selectProviderAccountCached
    ) where

import Agent.CLI.AccountUsageCache (refreshLoginAccountCached)
import Agent.Accounts.Selection
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
import Agent.Accounts.Auth
    ( LoadedAuth(..)
    , loadDirectOpenAiAuth
    , loadAuthForAccount
    , probeLoadedAuthCredential
    )
import qualified Agent.OpenAI.Auth as OpenAI
import Agent.Provider
    ( BillingMode(..)
    , Credential(..)
    , Provider(..)
    , providerSlug
    )
import Control.Concurrent.Async (mapConcurrently)
import Agent.Store.Postgres.Connection (StorePool)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readMaybe)

-- | Discover and check every enabled account for one provider. Automatic
-- selection only considers accounts whose usage endpoint returned a usable
-- result.
selectProviderAccount
    :: Provider
    -> Maybe BillingMode
    -> Maybe (Text, Text)
    -> IO (Either Text SelectedAccount)
selectProviderAccount =
    selectProviderAccountWith refreshLoginAccount

selectProviderAccountCached
    :: StorePool
    -> Provider
    -> Maybe BillingMode
    -> Maybe (Text, Text)
    -> IO (Either Text SelectedAccount)
selectProviderAccountCached pool =
    selectProviderAccountWith (refreshLoginAccountCached pool)

selectProviderAccountWith
    :: (LoginAccount -> IO LoginAccount)
    -> Provider
    -> Maybe BillingMode
    -> Maybe (Text, Text)
    -> IO (Either Text SelectedAccount)
selectProviderAccountWith refresh provider requiredBilling remembered =
    selectPreparedProviderAccount remembered
        <$> prepareProviderAccountsWith refresh provider requiredBilling

-- | Usage results prepared independently of project settings. The freshly
-- checked-out project can apply its remembered account only after Git setup
-- completes.
data PreparedProviderAccounts = PreparedProviderAccounts
    { preparedProvider :: !Provider
    , preparedAccounts :: ![LoginAccount]
    }

prepareProviderAccounts
    :: Provider
    -> Maybe BillingMode
    -> IO PreparedProviderAccounts
prepareProviderAccounts =
    prepareProviderAccountsWith refreshLoginAccount

prepareProviderAccountsWith
    :: (LoginAccount -> IO LoginAccount)
    -> Provider
    -> Maybe BillingMode
    -> IO PreparedProviderAccounts
prepareProviderAccountsWith refresh provider requiredBilling = do
    providerAccounts <-
        filter
            ((== provider) . (.loginProvider))
            <$> discoverSelectableLoginAccounts
    let billingAccounts = case requiredBilling of
            Just required ->
                filter ((== required) . billingMode . (.loginBilling))
                    providerAccounts
            Nothing ->
                let subscription =
                        filter
                            ((== SubscriptionBilled)
                                . billingMode . (.loginBilling))
                            providerAccounts
                in if null subscription then providerAccounts else subscription
    checked <- mapConcurrently
        (refreshSelectableAccountWith refresh)
        billingAccounts
    pure PreparedProviderAccounts
        { preparedProvider = provider
        , preparedAccounts = checked
        }

selectPreparedProviderAccount
    :: Maybe (Text, Text)
    -> PreparedProviderAccounts
    -> Either Text SelectedAccount
selectPreparedProviderAccount remembered prepared =
    case selectAccount remembered prepared.preparedAccounts of
        Just selected -> Right selected
        Nothing -> Left $
            "no "
                <> providerSlug prepared.preparedProvider
                <> " account has verified available usage"

-- | Prefer the remembered project account when it is usable; otherwise choose
-- the account with the greatest provider-local remaining-capacity score.
selectAccount
    :: Maybe (Text, Text)
    -> [LoginAccount]
    -> Maybe SelectedAccount
selectAccount remembered accounts =
    toSelected <$> selectCandidates remembered (map toCandidate accounts)
  where
    toCandidate account = AccountCandidate
        { candidateProvider = account.loginProvider
        , candidateSelectionId = loginAccountSelectionId account
        , candidateAccountId = account.loginAccountId
        , candidateBillingMode = billingMode account.loginBilling
        , candidateLabel = account.loginLabel
        , candidateCapacity = accountCapacity account
        }
    toSelected candidate = SelectedAccount
        { selectedProvider = candidate.candidateProvider
        , selectedSelectionId = candidate.candidateSelectionId
        , selectedAccountId = candidate.candidateAccountId
        , selectedBillingMode = candidate.candidateBillingMode
        , selectedLabel = candidate.candidateLabel
        }

-- | Provider-local capacity score. Subscription accounts use their tightest
-- active percentage window. Credit accounts use known remaining credit/key
-- capacity. Missing capacity data is treated as unverifiable.
accountCapacity :: LoginAccount -> Maybe Double
accountCapacity account = case account.loginUsage of
    UsageNotChecked -> Nothing
    UsageUnavailable _ -> Nothing
    UsageAvailable usage ->
        case usage.usageWindows of
            window : windows ->
                let used = maximum (map (.usedPercent) (window : windows))
                in if used >= 100
                    then Nothing
                    else Just (fromIntegral (100 - max 0 used))
            [] -> case usage.creditsRemaining >>= parseAmount of
                Just remaining
                    | remaining <= 0
                    , isFreeTier usage -> Just 1
                    | remaining <= 0 -> Nothing
                    | otherwise -> Just remaining
                Nothing -> Nothing

isFreeTier :: AccountUsage -> Bool
isFreeTier usage =
    maybe False
        ((== "free tier") . Text.toCaseFold . Text.strip)
        usage.usagePlan

billingMode :: AccountBilling -> BillingMode
billingMode = \case
    SubscriptionBilling _ -> SubscriptionBilled
    ApiCreditsBilling -> ApiBilled

parseAmount :: Text -> Maybe Double
parseAmount =
    readMaybe . Text.unpack . Text.dropWhile (`elem` ("$ " :: String))

refreshSelectableAccountWith
    :: (LoginAccount -> IO LoginAccount)
    -> LoginAccount
    -> IO LoginAccount
refreshSelectableAccountWith refresh account =
    refreshCredential account >>= \case
        Left err ->
            pure account { loginUsage = UsageUnavailable err }
        Right refreshed ->
            refresh refreshed
  where
    refreshCredential candidate = case candidate.loginProvider of
        OpenAIProvider ->
            loadDirectOpenAiAuth >>= \case
                Left err -> pure (Left err)
                Right loaded -> case loaded.loadedOpenAiPool of
                    Nothing ->
                        pure (Left "OpenAI account pool is unavailable")
                    Just pool ->
                        OpenAI.getAccessTokenForAccount
                            pool candidate.loginAccountId >>= \case
                                Left err ->
                                    pure (Left (Text.pack (show err)))
                                Right (token, accountId) ->
                                    pure $ Right candidate
                                        { loginAccessToken = token
                                        , loginAccountId = accountId
                                        }
        provider ->
            loadAuthForAccount
                provider
                (loginAccountSelectionId candidate) >>= \case
                    Left err -> pure (Left err)
                    Right loaded ->
                        probeLoadedAuthCredential loaded >>= \case
                            Left err -> pure (Left (Text.pack (show err)))
                            Right (credential, _) ->
                                pure $ Right candidate
                                    { loginAccessToken =
                                        credential.accessToken
                                    , loginAccountId =
                                        credential.accountId
                                    }
