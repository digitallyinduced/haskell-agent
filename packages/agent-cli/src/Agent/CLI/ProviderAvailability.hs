-- | Non-authoritative provider account availability checks.
--
-- Usage endpoints are useful for avoiding a predictably failed first turn,
-- but they are not the request path's source of truth. Network or decoding
-- failures therefore leave the credential usable; only a successful response
-- that definitively reports exhaustion rejects it.
module Agent.CLI.ProviderAvailability
    ( openAiUsageFailure
    , openRouterUsageFailure
    , probeLoadedAvailability
    , probeLoadedAvailabilityWith
    , xaiUsageFailure
    ) where

import Agent.CLI.Auth (LoadedAuth(..))
import Agent.Error (ApiError(..), ErrorType(..))
import qualified Agent.OpenAI.Usage as OpenAI
import qualified Agent.OpenRouter.Usage as OpenRouter
import Agent.Provider
    ( BillingMode(..)
    , Credential(..)
    , Provider(..)
    , runWithTokenProvider
    , seedTokenProvider
    , tokenProviderBillingMode
    )
import qualified Agent.XAI.Usage as XAI
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)

-- | Check the current account through the provider's available usage API and
-- preserve the successful credential checkout for later validation callers.
probeLoadedAvailability :: LoadedAuth -> IO (Either ApiError LoadedAuth)
probeLoadedAvailability loaded =
    probeLoadedAvailabilityWith checkCredential loaded
  where
    billing = tokenProviderBillingMode loaded.loadedTokenProvider
    checkCredential credential = case credential.provider of
        XAIProvider
            | billing == SubscriptionBilled ->
                XAI.fetchGrokUsage credential.accessToken >>= \case
                    Left _ -> pure Nothing
                    Right snapshot -> do
                        now <- getCurrentTime
                        pure (xaiUsageFailure now snapshot)
        OpenAIProvider
            | billing == SubscriptionBilled
            , not (nullAccountId credential.accountId) ->
                OpenAI.fetchUsage
                    credential.accessToken credential.accountId >>= \case
                        Left err
                            | isDefinitiveUsageFailure err ->
                                pure (Just err)
                            | otherwise -> pure Nothing
                        Right snapshot -> do
                            now <- getCurrentTime
                            pure (openAiUsageFailure now snapshot)
        OpenRouterProvider ->
            OpenRouter.fetchOpenRouterUsage credential.accessToken >>= \case
                Left _ -> pure Nothing
                Right snapshot -> pure (openRouterUsageFailure snapshot)
        _ -> pure Nothing

-- | Injectable core used by tests. Returning 'Nothing' means the credential
-- is usable or the remote check was inconclusive. A returned provider error is
-- fed through the normal account-failover machinery before the final result.
probeLoadedAvailabilityWith
    :: (Credential -> IO (Maybe ApiError))
    -> LoadedAuth
    -> IO (Either ApiError LoadedAuth)
probeLoadedAvailabilityWith check loaded = do
    result <-
        runWithTokenProvider loaded.loadedTokenProvider \credential ->
            check credential >>= \case
                Nothing -> pure (Right credential)
                Just err -> pure (Left err)
    case result of
        Left err -> pure (Left err)
        Right credential -> do
            provider <-
                seedTokenProvider loaded.loadedTokenProvider credential
            pure $ Right loaded { loadedTokenProvider = provider }

xaiUsageFailure
    :: UTCTime
    -> XAI.GrokUsageSnapshot
    -> Maybe ApiError
xaiUsageFailure now snapshot
    | snapshot.usedPercent < 100 = Nothing
    | otherwise =
        Just $
            ProviderError
                UsageLimitReached
                "Grok usage is exhausted for the current billing period."
                (Just (retrySecondsUntil now snapshot.resetsAt))

openAiUsageFailure
    :: UTCTime
    -> OpenAI.UsageSnapshot
    -> Maybe ApiError
openAiUsageFailure _ snapshot =
    case snapshot.rateLimit of
        Just limit
            | not limit.allowed || limit.limitReached ->
                Just $
                    ProviderError
                        UsageLimitReached
                        "Codex usage is exhausted for the current account."
                        (minimumPositive
                            (map (.resetAfterSeconds)
                                (catMaybes
                                    [ limit.primaryWindow
                                    , limit.secondaryWindow
                                    ])))
        _ -> Nothing

openRouterUsageFailure
    :: OpenRouter.OpenRouterUsage
    -> Maybe ApiError
openRouterUsageFailure snapshot
    | maybe False (<= 0) snapshot.keyLimitRemaining =
        exhausted
    | otherwise = case (snapshot.keyLimit, snapshot.keyUsage) of
        (Just limit, Just used)
            | used >= limit -> exhausted
        _ -> Nothing
  where
    exhausted =
        Just $
            ProviderError
                BillingError
                "OpenRouter key usage limit is exhausted."
                Nothing

retrySecondsUntil :: UTCTime -> UTCTime -> Int
retrySecondsUntil now resetAt =
    max 1 (ceiling (diffUTCTime resetAt now))

minimumPositive :: [Int] -> Maybe Int
minimumPositive values =
    case filter (> 0) values of
        [] -> Nothing
        positive -> Just (minimum positive)

nullAccountId :: Text -> Bool
nullAccountId = Text.null . Text.strip

isDefinitiveUsageFailure :: ApiError -> Bool
isDefinitiveUsageFailure = \case
    ProviderError AuthenticationError _ _ -> True
    ProviderError PermissionError _ _ -> True
    CredentialError{} -> True
    _ -> False
