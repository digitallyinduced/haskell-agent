-- | Direct client for the ChatGPT Codex usage endpoint used by the official
-- Codex app-server. This avoids running an app-server process merely to read
-- rate-limit state.
module Agent.OpenAI.Usage
    ( UsageSnapshot(..)
    , UsageLimit(..)
    , UsageWindow(..)
    , AdditionalUsageLimit(..)
    , fetchUsage
    , decodeUsageResponse
    ) where

import Agent.Error (ApiError(..), ErrorType(..))
import Control.Exception.Safe (tryAny)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.HTTP.Simple

data UsageSnapshot = UsageSnapshot
    { planType :: !Text
    , rateLimit :: !(Maybe UsageLimit)
    , additionalRateLimits :: ![AdditionalUsageLimit]
    } deriving (Eq, Show)

data UsageLimit = UsageLimit
    { allowed :: !Bool
    , limitReached :: !Bool
    , primaryWindow :: !(Maybe UsageWindow)
    , secondaryWindow :: !(Maybe UsageWindow)
    } deriving (Eq, Show)

data UsageWindow = UsageWindow
    { usedPercent :: !Int
    , limitWindowSeconds :: !Int
    , resetAfterSeconds :: !Int
    , resetAt :: !Int
    } deriving (Eq, Show)

data AdditionalUsageLimit = AdditionalUsageLimit
    { limitName :: !Text
    , meteredFeature :: !Text
    , rateLimit :: !(Maybe UsageLimit)
    } deriving (Eq, Show)

instance Aeson.FromJSON UsageSnapshot where
    parseJSON = Aeson.withObject "Codex usage" \object -> UsageSnapshot
        <$> object Aeson..: "plan_type"
        <*> object Aeson..:? "rate_limit"
        <*> object Aeson..:? "additional_rate_limits" Aeson..!= []

instance Aeson.FromJSON UsageLimit where
    parseJSON = Aeson.withObject "Codex usage limit" \object -> UsageLimit
        <$> object Aeson..: "allowed"
        <*> object Aeson..: "limit_reached"
        <*> object Aeson..:? "primary_window"
        <*> object Aeson..:? "secondary_window"

instance Aeson.FromJSON UsageWindow where
    parseJSON = Aeson.withObject "Codex usage window" \object -> UsageWindow
        <$> object Aeson..: "used_percent"
        <*> object Aeson..: "limit_window_seconds"
        <*> object Aeson..: "reset_after_seconds"
        <*> object Aeson..: "reset_at"

instance Aeson.FromJSON AdditionalUsageLimit where
    parseJSON = Aeson.withObject "additional Codex usage limit" \object -> AdditionalUsageLimit
        <$> object Aeson..: "limit_name"
        <*> object Aeson..: "metered_feature"
        <*> object Aeson..:? "rate_limit"

-- | Read current rate-limit usage for one ChatGPT account.
--
-- Uses the same endpoint and headers as @openai/codex@. The endpoint belongs
-- to the ChatGPT backend rather than the public OpenAI API, so callers should
-- treat failures as a health signal and keep normal request-side rate-limit
-- handling as a fallback.
fetchUsage :: Text -> Text -> IO (Either ApiError UsageSnapshot)
fetchUsage accessToken accountId = tryAny requestUsage >>= \case
    Left exception -> pure $ Left $ ConnectionError
        ("Could not read Codex usage: " <> Text.pack (show exception))
    Right response -> do
        let status = getResponseStatusCode response
        pure $ if status >= 200 && status < 300
            then decodeUsageResponse (getResponseBody response)
            else Left $ if status == 401 || status == 403
                then ProviderError AuthenticationError "Codex usage authentication failed" Nothing
                else HttpError status "Codex usage endpoint returned an error"
  where
    requestUsage = do
        request <- parseRequest "https://chatgpt.com/backend-api/wham/usage"
        httpLBS
            $ setRequestHeader "Authorization" ["Bearer " <> Text.encodeUtf8 accessToken]
            $ setRequestHeader "ChatGPT-Account-ID" [Text.encodeUtf8 accountId]
            $ setRequestHeader "Accept" ["application/json"] request

decodeUsageResponse :: LBS.ByteString -> Either ApiError UsageSnapshot
decodeUsageResponse body = case Aeson.eitherDecode body of
    Left err -> Left $ ConnectionError
        ("Invalid Codex usage response: " <> Text.pack err)
    Right snapshot -> Right snapshot
