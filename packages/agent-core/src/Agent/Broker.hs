-- | Client for the centralized credential broker.
module Agent.Broker
    ( BrokerOptions(..)
    , newBrokerTokenProvider
    , newBrokerTokenProviderWith
    , newBrokerTokenProviderWithClock
    ) where

import Agent.Http.Url (trimTrailingSlash)
import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Provider
    ( AccountFailure(..)
    , Credential(..)
    , FailedCredential(..)
    , Provider(..)
    , TokenProvider(..)
    , providerSlug
    , parseProvider
    )
import Control.Exception.Safe (tryAny)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Network.HTTP.Simple

data BrokerOptions = BrokerOptions
    { baseUrl :: !String
    , serviceToken :: !Text
    } deriving (Eq, Show)

data BrokerToken = BrokerToken
    { accessToken :: !Text
    , accountId :: !Text
    , brokerLeaseId :: !(Maybe Text)
    , brokerProvider :: !Provider
    }

data BrokerError = BrokerError
    { brokerErrorCode :: !Text
    , brokerRetryAt :: !(Maybe UTCTime)
    }

instance Aeson.FromJSON BrokerToken where
    parseJSON = Aeson.withObject "broker token" \object -> do
        accessToken <- object Aeson..: "access_token"
        accountId <- object Aeson..: "account_id"
        brokerLeaseId <- object Aeson..:? "lease_id"
        -- Absent field = a pre-provider broker, which only ever stores OpenAI
        -- accounts. An unknown provider slug is a hard error: routing such a
        -- credential to the ChatGPT backend would leak it to the wrong host.
        providerSlug <- object Aeson..:? "provider"
        brokerProvider <- case providerSlug of
            Nothing -> pure OpenAIProvider
            Just slug -> case parseProvider slug of
                Just provider -> pure provider
                Nothing -> fail ("unsupported broker provider: " <> Text.unpack slug)
        pure BrokerToken { accessToken, accountId, brokerLeaseId, brokerProvider }

-- | Build a broker-backed provider. Every acquisition asks the broker for its
-- current best account rather than maintaining a second local scheduler.
-- Every acquisition asks the broker for its current best account, excluding
-- accounts this process has just observed as unavailable.
--
-- Failure feedback is sent on the same token request, allowing a compatible
-- broker to persist the cooldown before it selects the replacement. The local
-- cooldown remains a defensive fallback for older broker deployments. A
-- lease-aware broker can include a @lease_id@ without changing transport call
-- sites.
newBrokerTokenProvider :: BrokerOptions -> IO TokenProvider
newBrokerTokenProvider options =
    newBrokerTokenProviderWith (fetchBrokerCredentialExcluding options)

-- | Construct a broker provider around a credential-fetch callback. This is
-- useful for custom broker transports and deterministic integration tests.
newBrokerTokenProviderWith
    :: (Maybe FailedCredential -> [Text] -> IO (Either ApiError (Maybe Credential)))
    -> IO TokenProvider
newBrokerTokenProviderWith = newBrokerTokenProviderWithClock getCurrentTime

-- | Clock-injected variant used by deterministic tests.
newBrokerTokenProviderWithClock
    :: IO UTCTime
    -> (Maybe FailedCredential -> [Text] -> IO (Either ApiError (Maybe Credential)))
    -> IO TokenProvider
newBrokerTokenProviderWithClock getNow fetchCredential = do
    cooldownsRef <- newIORef Map.empty
    pure $ TokenProvider \failed -> do
        now <- getNow
        activeCooldowns <- atomicModifyIORef' cooldownsRef \cooldowns ->
            let withFailure = maybe cooldowns (recordFailure now cooldowns) failed
                active = Map.filter
                    (\BrokerLocalCooldown { excludeUntil } -> excludeUntil > now)
                    withFailure
            in (active, active)
        let excludedAccountIds = Map.keys activeCooldowns
        fetchCredential failed excludedAccountIds >>= \case
            Left err -> pure (Left err)
            Right Nothing -> pure $ Left $ unavailableError now activeCooldowns
            Right (Just credential)
                | credential.accountId `Map.member` activeCooldowns ->
                    pure $ Left $ ConnectionError
                        "broker returned an explicitly excluded account"
                | otherwise -> pure (Right credential)
  where
    recordFailure now cooldowns FailedCredential { credential, failure } =
        Map.insert credential.accountId (brokerLocalCooldown now failure) cooldowns

    unavailableError now cooldowns = case Map.elems cooldowns of
        [] -> ConnectionError "broker returned no accounts"
        cooldowns' -> CredentialsExhausted
            (minimum (map (\BrokerLocalCooldown { retryAt } -> retryAt) cooldowns') `max` now)

data BrokerLocalCooldown = BrokerLocalCooldown
    { excludeUntil :: !UTCTime
    , retryAt :: !UTCTime
    }

-- The broker receives the full upstream retry interval and is authoritative
-- for long-lived account health. Keep a short local exclusion only to avoid
-- selecting the same rejected credential again within the current failover.
-- Re-probing after a minute lets a long-running worker observe early recovery
-- (for example after the broker usage monitor clears a stale quota result).
-- The separately retained retryAt still propagates the precise upstream reset
-- when no replacement is available, so durable jobs can park efficiently.
brokerLocalCooldown :: UTCTime -> AccountFailure -> BrokerLocalCooldown
brokerLocalCooldown now failure = BrokerLocalCooldown
    { excludeUntil = min retryAt (addUTCTime brokerRecoveryProbeSeconds now)
    , retryAt
    }
  where
    retryAt = failureCooldownUntil now failure

brokerRecoveryProbeSeconds :: NominalDiffTime
brokerRecoveryProbeSeconds = 60

failureCooldownUntil :: UTCTime -> AccountFailure -> UTCTime
failureCooldownUntil now = \case
    AccountRateLimited { retryAfterSeconds } ->
        addUTCTime (fromIntegral (max 1 (fromMaybe 60 retryAfterSeconds))) now
    AccountAuthenticationRejected -> addUTCTime (30 * 60) now

instance Aeson.FromJSON BrokerError where
    parseJSON = Aeson.withObject "broker error" \object -> BrokerError
        <$> object Aeson..: "error"
        <*> object Aeson..:? "retry_at"

fetchBrokerCredentialExcluding
    :: BrokerOptions
    -> Maybe FailedCredential
    -> [Text]
    -> IO (Either ApiError (Maybe Credential))
fetchBrokerCredentialExcluding options failed excludedAccountIds =
    fmap (fmap brokerCredential)
        <$> fetchBrokerTokenExcluding options
            [OpenAIProvider, XAIProvider, OpenRouterProvider]
            failed excludedAccountIds
  where
    brokerCredential BrokerToken { accessToken, accountId, brokerLeaseId, brokerProvider } =
        Credential
            { accessToken
            , accountId
            , leaseId = brokerLeaseId
            , provider = brokerProvider
            }

fetchBrokerTokenExcluding
    :: BrokerOptions
    -> [Provider]
    -> Maybe FailedCredential
    -> [Text]
    -> IO (Either ApiError (Maybe BrokerToken))
fetchBrokerTokenExcluding options supportedProviders failed excludedAccountIds =
    tryAny requestToken >>= \case
        Left exception -> pure $ Left $ ConnectionError
            ("broker request failed: " <> Text.pack (show exception))
        Right response -> do
            let status = getResponseStatusCode response
            if status == 409 && not (null excludedAccountIds)
                then pure (Right Nothing)
            else if status < 200 || status >= 300
                then pure $ Left if status == 401
                    then ProviderError AuthenticationError "Broker rejected the service token" Nothing
                    else brokerHttpError status (getResponseBody response)
                else case Aeson.eitherDecode (getResponseBody response) of
                    Left err -> pure $ Left $ ConnectionError
                        ("invalid broker response: " <> Text.pack err)
                    Right token -> pure (Right (Just token))
  where
    requestToken = do
        request <- parseRequest (trimTrailingSlash options.baseUrl <> "/api/v1/token")
        httpLBS
            $ addFailureHeaders failed
            $ setRequestMethod "POST"
            $ setRequestHeader "Authorization" ["Bearer " <> Text.encodeUtf8 options.serviceToken]
            $ setRequestHeader "X-Codex-Broker-Exclude-Accounts"
                [Text.encodeUtf8 (Text.intercalate "," excludedAccountIds)]
            -- Capability negotiation: the broker must only issue accounts of
            -- providers this client can route. Pre-provider brokers ignore
            -- the header and keep serving OpenAI accounts.
            $ setRequestHeader "X-Codex-Broker-Providers"
                [Text.encodeUtf8 (Text.intercalate "," (map providerSlug supportedProviders))]
            $ setRequestHeader "Content-Type" ["application/json"] request

    addFailureHeaders Nothing = id
    addFailureHeaders (Just FailedCredential { credential, failure }) =
        setRequestHeader "X-Codex-Broker-Failed-Account"
            [Text.encodeUtf8 credential.accountId]
        . setRequestHeader "X-Codex-Broker-Failure-Type"
            [Text.encodeUtf8 (failureType failure)]
        . addLeaseHeader credential.leaseId
        . addRetryAfterHeader failure

    addLeaseHeader Nothing = id
    addLeaseHeader (Just leaseId) =
        setRequestHeader "X-Codex-Broker-Lease-Id"
            [Text.encodeUtf8 leaseId]

    addRetryAfterHeader AccountAuthenticationRejected = id
    addRetryAfterHeader AccountRateLimited { retryAfterSeconds = Nothing } = id
    addRetryAfterHeader AccountRateLimited { retryAfterSeconds = Just seconds } =
        setRequestHeader "X-Codex-Broker-Retry-After-Seconds"
            [Text.encodeUtf8 (Text.pack (show (max 1 seconds)))]

    failureType AccountAuthenticationRejected = "authentication_rejected"
    failureType AccountRateLimited{} = "rate_limited"

brokerHttpError :: Int -> LBS.ByteString -> ApiError
brokerHttpError status body
    | status == 429
    , Right BrokerError
        { brokerErrorCode = "no_healthy_granted_account"
        , brokerRetryAt = Just retryAt
        } <- Aeson.eitherDecode body = CredentialsExhausted retryAt
    | otherwise = HttpError status "broker could not issue an access token"

