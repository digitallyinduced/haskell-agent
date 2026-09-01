-- | PostgreSQL-backed cache for credential-safe account usage metadata.
module Agent.CLI.AccountUsageCache
    ( refreshLoginAccountCached
    ) where

import Agent.CLI.Login
    ( AccountBilling(..)
    , AccountUsage(..)
    , LoginAccount(..)
    , UsageState(..)
    , UsageWindow(..)
    , refreshLoginAccount
    )
import Agent.Provider (providerSlug)
import Agent.Store.Postgres.Connection (StorePool)
import Agent.Store.Postgres.UsageCache
    ( AccountUsageCacheEntry(..)
    , loadAccountUsageCache
    , upsertAccountUsageCache
    )
import Control.Monad (void)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Time.Clock (addUTCTime, getCurrentTime)

data CachedAccountUsage = CachedAccountUsage
    { cachedLabel :: !Text
    , cachedBilling :: !AccountBilling
    , cachedUsage :: !AccountUsage
    }

instance Aeson.ToJSON CachedAccountUsage where
    toJSON cached = Aeson.object
        [ "label" .= cached.cachedLabel
        , "billing" .= billingJSON cached.cachedBilling
        , "usage" .= usageJSON cached.cachedUsage
        ]

instance Aeson.FromJSON CachedAccountUsage where
    parseJSON = Aeson.withObject "cached account usage" \object ->
        CachedAccountUsage
            <$> object .: "label"
            <*> (object .: "billing" >>= parseBilling)
            <*> (object .: "usage" >>= parseUsage)

-- | Use a fresh cached usage result when possible. Cache payloads contain only
-- display metadata; credentials and managed-secret payloads are never stored.
refreshLoginAccountCached :: StorePool -> LoginAccount -> IO LoginAccount
refreshLoginAccountCached pool account = do
    now <- getCurrentTime
    loadAccountUsageCache pool cacheProvider account.loginAccountId >>= \case
        Right (Just entry)
            | entry.accountUsageCacheExpiresAt > now
            , Just cached <- decodeCached entry.accountUsageCachePayload ->
                pure (applyCached cached account)
        _ -> refreshAndStore now
  where
    cacheProvider = "tui:" <> providerSlug account.loginProvider
    refreshAndStore now = do
        refreshed <- refreshLoginAccount account
        case refreshed.loginUsage of
            UsageAvailable usage -> void $ upsertAccountUsageCache pool
                AccountUsageCacheEntry
                    { accountUsageCacheProvider = cacheProvider
                    , accountUsageCacheAccountId = account.loginAccountId
                    , accountUsageCachePayload = encodeCached CachedAccountUsage
                        { cachedLabel = refreshed.loginLabel
                        , cachedBilling = refreshed.loginBilling
                        , cachedUsage = usage
                        }
                    , accountUsageCacheFetchedAt = now
                    , accountUsageCacheExpiresAt = addUTCTime 300 now
                    }
            _ -> pure ()
        pure refreshed

applyCached :: CachedAccountUsage -> LoginAccount -> LoginAccount
applyCached cached account = account
    { loginLabel = cached.cachedLabel
    , loginBilling = cached.cachedBilling
    , loginUsage = UsageAvailable cached.cachedUsage
    }

encodeCached :: CachedAccountUsage -> Text
encodeCached = Text.decodeUtf8 . LBS.toStrict . Aeson.encode

decodeCached :: Text -> Maybe CachedAccountUsage
decodeCached = Aeson.decodeStrict' . Text.encodeUtf8

billingJSON :: AccountBilling -> Aeson.Value
billingJSON = \case
    ApiCreditsBilling -> Aeson.object ["kind" .= ("api" :: Text)]
    SubscriptionBilling plan -> Aeson.object
        [ "kind" .= ("subscription" :: Text)
        , "plan" .= plan
        ]

parseBilling :: Aeson.Value -> Parser AccountBilling
parseBilling = Aeson.withObject "cached billing" \object ->
    object .: "kind" >>= \case
        ("api" :: Text) -> pure ApiCreditsBilling
        "subscription" -> SubscriptionBilling <$> object .:? "plan"
        _ -> fail "unknown cached billing kind"

usageJSON :: AccountUsage -> Aeson.Value
usageJSON usage = Aeson.object
    [ "plan" .= usage.usagePlan
    , "windows" .= map windowJSON usage.usageWindows
    , "creditsRemaining" .= usage.creditsRemaining
    , "creditsUsed" .= usage.creditsUsed
    ]

parseUsage :: Aeson.Value -> Parser AccountUsage
parseUsage = Aeson.withObject "cached usage" \object ->
    AccountUsage
        <$> object .:? "plan"
        <*> (object .: "windows" >>= traverse parseWindow)
        <*> object .:? "creditsRemaining"
        <*> object .:? "creditsUsed"

windowJSON :: UsageWindow -> Aeson.Value
windowJSON window = Aeson.object
    [ "name" .= window.windowName
    , "usedPercent" .= window.usedPercent
    , "windowSeconds" .= window.windowSeconds
    , "resetsAt" .= window.resetsAt
    ]

parseWindow :: Aeson.Value -> Parser UsageWindow
parseWindow = Aeson.withObject "cached usage window" \object ->
    UsageWindow
        <$> object .: "name"
        <*> object .: "usedPercent"
        <*> object .: "windowSeconds"
        <*> object .: "resetsAt"
