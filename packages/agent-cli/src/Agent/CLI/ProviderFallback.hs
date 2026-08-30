-- | Automatic cross-provider fallback policy.
module Agent.CLI.ProviderFallback
    ( allowsAutomaticBillingFallback
    , automaticCooldownRetryDelay
    , automaticRetryCountdownText
    , fallbackCandidates
    , isProviderUnavailable
    , isUsageExhausted
    , ProviderRecoveryPreference(..)
    , providerRecoveryPreference
    , rankedModels
    ) where

import Agent.CLI.ModelConfig (ModelCatalog)
import Agent.CLI.Models (ModelOption(..), ModelTarget(..), modelCatalog)
import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Provider (BillingMode(..), Provider(..))
import Data.Containers.ListUtils (nubOrdOn)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime)

-- | Keep brief provider cooldowns invisible to the user. Longer waits are
-- eligible for cross-provider fallback instead of making the CLI appear hung.
maxAutomaticCooldownWait :: NominalDiffTime
maxAutomaticCooldownWait = 120

-- | Whether a provider-unavailable turn should first wait for the current
-- provider or immediately enter cross-provider fallback.
--
-- A brief 'CredentialsExhausted' window commonly represents a transient 429
-- shared by otherwise usable accounts. Retrying it first prevents that
-- temporary cooldown from being recorded as permanent provider exhaustion by
-- the fallback chain. Long usage-window resets still fall back immediately.
data ProviderRecoveryPreference
    = RetryCurrentProviderAfter !NominalDiffTime
    | TryProviderFallback
    deriving (Eq, Show)

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

providerRecoveryPreference
    :: Bool
    -> UTCTime
    -> ApiError
    -> ProviderRecoveryPreference
providerRecoveryPreference allowCooldownRetry now err =
    case automaticCooldownRetryDelay now err of
        Just delay
            | allowCooldownRetry -> RetryCurrentProviderAfter delay
        _ -> TryProviderFallback

-- | Live status text for a brief automatic provider retry. Keep the remaining
-- time in seconds so a one-minute cooldown visibly counts down instead of
-- staying at the coarser "1m" duration for most of the wait.
automaticRetryCountdownText :: Int -> Text
automaticRetryCountdownText rawSeconds =
    "Provider temporarily unavailable; retrying automatically in "
        <> Text.pack (show (max 0 rawSeconds))
        <> "s · Esc to cancel"

-- | Curated models ordered from strongest to weakest for automatic selection.
--
-- Account and quota failures are provider-wide, so trying a weaker model on
-- the same account would only loop. Model-access failures are different: new
-- models are often rolled out gradually. In that case 'fallbackCandidates'
-- first walks down the ranked models for the current provider, then keeps the
-- highest-ranked model for every other eligible provider.
rankedModels :: ModelCatalog -> [ModelOption]
rankedModels = sortOn modelRank . filter hasPriority . modelCatalog
  where
    hasPriority option =
        option.modelFallbackPriority /= Nothing
            -- Custom connections are deliberately manual-only.
            && option.modelTarget.targetConnectionId
                `elem` ["openai", "xai", "openrouter", "gemini"]
    modelRank = maybe maxBound id . (.modelFallbackPriority)

-- | Return ordered recovery targets for an unavailable model or provider.
-- Providers already observed as exhausted are excluded. A structured
-- model-access failure may keep the current provider long enough to try its
-- lower-ranked models.
fallbackCandidates
    :: ModelCatalog
    -> Set Provider
    -> Provider
    -> Text
    -> ApiError
    -> [ModelOption]
fallbackCandidates catalog unavailable current currentModel err
    | current == ClaudeCodeProvider = []
    | not (isProviderUnavailable err) = []
    | otherwise = sameProviderFallbacks <> otherProviderFallbacks
  where
    ranked = rankedModels catalog
    sameProviderFallbacks
        | isModelUnavailable err =
            case currentPriority of
                Nothing -> []
                Just priority ->
                    filter
                        (\option ->
                            option.modelTarget.targetProvider == current
                                && option.modelTarget.targetModelId /= currentModel
                                && option.modelFallbackPriority > Just priority)
                        ranked
        | otherwise = []
    currentPriority =
        (.modelFallbackPriority) =<< safeHead
            (filter
                (\option ->
                    option.modelTarget.targetProvider == current
                        && option.modelTarget.targetModelId == currentModel)
                ranked)
    otherProviderFallbacks =
        filter
            (\option ->
                option.modelTarget.targetProvider /= current
                    && option.modelTarget.targetProvider
                        /= ClaudeCodeProvider
                    && option.modelTarget.targetProvider
                        `Set.notMember` unavailable)
            (nubOrdOn (.modelTarget.targetProvider) ranked)

    safeHead = \case
        [] -> Nothing
        value : _ -> Just value

-- | A structured provider response that says the selected model or feature is
-- unavailable. Unlike a bare HTTP 403, this is safe to recover from by trying
-- a lower-ranked model on the same account.
isModelUnavailable :: ApiError -> Bool
isModelUnavailable = \case
    ProviderError PermissionError _ _ -> True
    ProviderError UsageNotIncluded _ _ -> True
    _ -> False

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
