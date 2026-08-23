-- | Automatic cross-provider fallback policy.
module Agent.CLI.ProviderFallback
    ( allowsAutomaticBillingFallback
    , automaticCooldownRetryDelay
    , fallbackCandidates
    , isProviderUnavailable
    , isUsageExhausted
    , rankedModels
    ) where

import Agent.CLI.ModelConfig (ModelCatalog)
import Agent.CLI.Models (ModelOption(..), ModelTarget(..), modelCatalog)
import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Provider (BillingMode(..), Provider(..))
import Data.List (nubBy, sortOn)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime)

-- | Keep brief provider cooldowns invisible to the user when no fallback
-- account is available. Longer waits return control to the prompt instead of
-- making the CLI appear hung.
maxAutomaticCooldownWait :: NominalDiffTime
maxAutomaticCooldownWait = 120

-- | Automatic fallback may use another subscription account, but must never
-- turn subscription exhaustion into API-credit spending. Manual provider
-- changes remain unrestricted because the user explicitly chose them.
allowsAutomaticBillingFallback
    :: BillingMode
    -> BillingMode
    -> Bool
allowsAutomaticBillingFallback source target =
    source /= SubscriptionBilled || target /= ApiBilled

automaticCooldownRetryDelay
    :: UTCTime
    -> ApiError
    -> Maybe NominalDiffTime
automaticCooldownRetryDelay now = \case
    CredentialsExhausted{retryAt} ->
        let delay = max 0 (diffUTCTime retryAt now)
        in if delay <= maxAutomaticCooldownWait
            then Just delay
            else Nothing
    _ -> Nothing

-- | Curated models ordered from strongest to weakest for automatic selection.
--
-- Availability is account-level: once a provider reports that all of its
-- credentials are exhausted, trying a weaker model on the same account would
-- only loop. 'fallbackCandidates' therefore keeps the highest-ranked model for
-- each still-eligible provider.
rankedModels :: ModelCatalog -> [ModelOption]
rankedModels = sortOn modelRank . filter hasPriority . modelCatalog
  where
    hasPriority option =
        option.modelFallbackPriority /= Nothing
            -- Custom connections are deliberately manual-only.
            && option.modelTarget.targetConnectionId
                `elem` ["openai", "xai", "openrouter"]
    modelRank = maybe maxBound id . (.modelFallbackPriority)

-- | Return the best model for every provider that may still have a usable
-- account. Providers already observed as exhausted, including the provider
-- that produced this error, are excluded.
fallbackCandidates
    :: ModelCatalog
    -> [Provider]
    -> Provider
    -> ApiError
    -> [ModelOption]
fallbackCandidates catalog unavailable current err
    | not (isProviderUnavailable err) = []
    | otherwise =
        filter
            (\option ->
                option.modelTarget.targetProvider /= current
                    && option.modelTarget.targetProvider `notElem` unavailable)
            (nubBy sameProvider (rankedModels catalog))
  where
    sameProvider left right =
        left.modelTarget.targetProvider
            == right.modelTarget.targetProvider

isUsageExhausted :: ApiError -> Bool
isUsageExhausted = \case
    CredentialsExhausted{} -> True
    ProviderError UsageLimitReached _ _ -> True
    ProviderError UsageBalanceExhausted _ _ -> True
    ProviderError QuotaExceeded _ _ -> True
    ProviderError UsageNotIncluded _ _ -> True
    ProviderError BillingError _ _ -> True
    _ -> False

-- | Failures for which rebuilding the same provider cannot make progress.
-- Authentication failures are included so an automatic transition whose
-- replacement backend rejects its credentials can continue to another
-- configured provider.
isProviderUnavailable :: ApiError -> Bool
isProviderUnavailable err
    | isUsageExhausted err = True
    | otherwise = case err of
        HttpError status _ -> status == 401 || status == 403
        ProviderError AuthenticationError _ _ -> True
        CredentialError{} -> True
        ProviderError PermissionError _ _ -> True
        _ -> False
