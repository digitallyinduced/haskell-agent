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
import qualified Agent.Json.Decode as Json
import Control.Exception.Safe (tryAny)
import Control.Monad (join)
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

usageSnapshotDecoder :: Json.Decoder UsageSnapshot
usageSnapshotDecoder = Json.object do
    planType <- Json.atKey "plan_type" Json.text
    rateLimit <- optionalNullable "rate_limit" usageLimitDecoder
    additionalRateLimits <-
        maybe [] id <$> Json.atKeyOptional
            "additional_rate_limits"
            (Json.list additionalUsageLimitDecoder)
    pure UsageSnapshot{..}

usageLimitDecoder :: Json.Decoder UsageLimit
usageLimitDecoder = Json.object do
    allowed <- Json.atKey "allowed" Json.bool
    limitReached <- Json.atKey "limit_reached" Json.bool
    primaryWindow <- optionalNullable "primary_window" usageWindowDecoder
    secondaryWindow <- optionalNullable "secondary_window" usageWindowDecoder
    pure UsageLimit{..}

usageWindowDecoder :: Json.Decoder UsageWindow
usageWindowDecoder = Json.object $
    UsageWindow
        <$> Json.atKey "used_percent" Json.int
        <*> Json.atKey "limit_window_seconds" Json.int
        <*> Json.atKey "reset_after_seconds" Json.int
        <*> Json.atKey "reset_at" Json.int

additionalUsageLimitDecoder :: Json.Decoder AdditionalUsageLimit
additionalUsageLimitDecoder = Json.object do
    limitName <- Json.atKey "limit_name" Json.text
    meteredFeature <- Json.atKey "metered_feature" Json.text
    rateLimit <- optionalNullable "rate_limit" usageLimitDecoder
    pure AdditionalUsageLimit{..}

optionalNullable
    :: Text
    -> Json.Decoder value
    -> Json.FieldsDecoder (Maybe value)
optionalNullable key decoder =
    join <$> Json.atKeyOptional key (Json.nullable decoder)

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
decodeUsageResponse body = case Json.decodeEither usageSnapshotDecoder (LBS.toStrict body) of
    Left err -> Left $ ConnectionError
        ("Invalid Codex usage response: " <> Json.jsonErrorMessage err)
    Right snapshot -> Right snapshot
