-- | Automatic cross-provider fallback policy.
module Agent.CLI.ProviderFallback
    ( automaticCooldownRetryDelay
    , fallbackCandidates
    , isProviderUnavailable
    , isUsageExhausted
    , maxAutomaticCooldownWait
    , rankedModels
    ) where

import Agent.CLI.Models (ModelOption(..), modelCatalog)
import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Provider (Provider(..))
import Data.List (nubBy, sortOn)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime)

-- | Keep brief provider cooldowns invisible to the user when no fallback
-- account is available. Longer waits return control to the prompt instead of
-- making the CLI appear hung.
maxAutomaticCooldownWait :: NominalDiffTime
maxAutomaticCooldownWait = 120

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
rankedModels :: [ModelOption]
rankedModels = sortOn modelRank modelCatalog
  where
    modelRank option = case (option.modelProvider, option.modelId) of
        (OpenAIProvider, "gpt-5.6-sol") -> 0 :: Int
        (XAIProvider, "grok-4.6") -> 10
        (OpenAIProvider, "gpt-5.6-terra") -> 20
        (OpenAIProvider, "gpt-5.6-luna") -> 30
        (XAIProvider, "grok-4.5") -> 40
        (XAIProvider, "grok-4.5-mini") -> 50
        (OpenRouterProvider, "openai/gpt-5.1") -> 60
        (OpenRouterProvider, "anthropic/claude-sonnet-4") -> 70
        (OpenRouterProvider, "x-ai/grok-4") -> 80
        (OpenRouterProvider, "google/gemini-2.5-pro") -> 90
        (XAIProvider, "grok-3") -> 100
        _ -> 1000

-- | Return the best model for every provider that may still have a usable
-- account. Providers already observed as exhausted, including the provider
-- that produced this error, are excluded.
fallbackCandidates
    :: [Provider]
    -> Provider
    -> ApiError
    -> [ModelOption]
fallbackCandidates unavailable current err
    | not (isProviderUnavailable err) = []
    | otherwise =
        filter
            (\option ->
                option.modelProvider /= current
                    && option.modelProvider `notElem` unavailable)
            (nubBy sameProvider rankedModels)
  where
    sameProvider left right =
        left.modelProvider == right.modelProvider

isUsageExhausted :: ApiError -> Bool
isUsageExhausted = \case
    CredentialsExhausted{} -> True
    ProviderError UsageLimitReached _ _ -> True
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
        ProviderError PermissionError _ _ -> True
        _ -> False
