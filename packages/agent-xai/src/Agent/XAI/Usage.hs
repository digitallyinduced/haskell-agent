-- | Usage-window client for Grok subscription credentials.
module Agent.XAI.Usage
    ( GrokUsageSnapshot(..)
    , decodeGrokUsage
    , fetchGrokUsage
    , fetchGrokUsageFrom
    , weeklyLimitLeft
    ) where

import Control.Exception.Safe (tryAny)
import Agent.Provider (Credential(..))
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.:?))
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock (UTCTime, diffUTCTime)
import qualified Network.HTTP.Client as HttpClient
import Network.HTTP.Simple

data GrokUsageSnapshot = GrokUsageSnapshot
    { usedPercent :: !Int
    , periodType :: !Text
    , windowSeconds :: !Int
    , resetsAt :: !UTCTime
    }
    deriving (Eq, Show)

instance Aeson.FromJSON GrokUsageSnapshot where
    parseJSON = Aeson.withObject "Grok billing response" \response -> do
        config <- response .: "config"
        Aeson.withObject "Grok billing config" parseConfig config
      where
        parseConfig config = do
            -- The credits service uses proto3 JSON, which omits scalar fields
            -- at their default value. A freshly reset account can therefore
            -- omit creditUsagePercent entirely; that represents 0%, not an
            -- unreadable response.
            creditUsagePercent <-
                fromMaybe 0
                    <$> (config .:? "creditUsagePercent" :: Parser (Maybe Scientific))
            currentPeriod <- config .: "currentPeriod"
            (periodType, startsAt, resetsAt) <-
                Aeson.withObject "Grok billing period" parsePeriod currentPeriod
            let usedPercent = max 0 (min 100 (floor creditUsagePercent))
                windowSeconds =
                    max 1 (round (diffUTCTime resetsAt startsAt))
            pure GrokUsageSnapshot
                { usedPercent
                , periodType
                , windowSeconds
                , resetsAt
                }

        parsePeriod period =
            (,,)
                <$> (fromMaybe "" <$> period .:? "type")
                <*> period .: "start"
                <*> period .: "end"

decodeGrokUsage :: LBS.ByteString -> Either Text GrokUsageSnapshot
decodeGrokUsage body = case Aeson.eitherDecode body of
    Left _ ->
        Left "Grok returned an unreadable usage response."
    Right snapshot -> Right snapshot

-- | The fullscreen composer mirrors Grok Build's weekly reserve label only
-- when the billing service explicitly reports a weekly period.
weeklyLimitLeft :: GrokUsageSnapshot -> Maybe Int
weeklyLimitLeft snapshot
    | snapshot.periodType == "USAGE_PERIOD_TYPE_WEEKLY" =
        Just (max 0 (100 - snapshot.usedPercent))
    | otherwise = Nothing

fetchGrokUsage :: Credential -> IO (Either Text GrokUsageSnapshot)
fetchGrokUsage =
    fetchGrokUsageFrom
        "https://cli-chat-proxy.grok.com/v1/billing?format=credits"

-- | Injectable endpoint variant used by request-contract tests.
fetchGrokUsageFrom
    :: String
    -> Credential
    -> IO (Either Text GrokUsageSnapshot)
fetchGrokUsageFrom endpoint credential =
    tryAny requestUsage >>= \case
        Left _ ->
            pure $ Left
                "Could not load Grok usage. Check your connection and retry."
        Right response -> do
            let status = getResponseStatusCode response
            pure $
                if status >= 200 && status < 300
                    then decodeGrokUsage (getResponseBody response)
                    else Left
                        ("Grok billing returned HTTP " <> Text.pack (show status))
  where
    requestUsage = do
        request <- parseRequest endpoint
        httpLBS
            $ setRequestHeader
                "Authorization"
                ["Bearer " <> Text.encodeUtf8 credential.accessToken]
            $ setRequestHeader "X-XAI-Token-Auth" ["xai-grok-cli"]
            $ setRequestHeader
                "x-userid"
                [Text.encodeUtf8 credential.accountId]
            $ setRequestHeader "Accept" ["application/json"]
            $ setRequestHeader "x-grok-client-mode" ["shell"]
            $ setRequestHeader "x-grok-client-version" ["0.2.118"]
            $ setRequestResponseTimeout
                (HttpClient.responseTimeoutMicro (30 * 1_000_000))
                request
