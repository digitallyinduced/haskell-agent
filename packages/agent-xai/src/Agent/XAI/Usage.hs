-- | Usage-window client for Grok subscription credentials.
module Agent.XAI.Usage
    ( GrokUsageSnapshot(..)
    , decodeGrokUsage
    , fetchGrokUsage
    , fetchGrokUsageFrom
    , weeklyLimitLeft
    ) where

import Control.Exception.Safe (tryAny)
import qualified Agent.Json.Decode as Json
import Agent.Provider (Credential(..))
import Agent.XAI.Options
    ( defaultGrokClientVersion
    , grokTokenAuthValue
    , grokUserAgent
    )
import qualified Data.ByteString.Lazy as LBS
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

grokUsageSnapshotDecoder :: Json.Decoder GrokUsageSnapshot
grokUsageSnapshotDecoder = Json.object $
    Json.atKey "config" $ Json.object do
            -- The credits service uses proto3 JSON, which omits scalar fields
            -- at their default value. A freshly reset account can therefore
            -- omit creditUsagePercent entirely; that represents 0%, not an
            -- unreadable response.
            creditUsagePercent <-
                Json.defaultKey 0 "creditUsagePercent" Json.scientific
            (periodType, startsAt, resetsAt) <- Json.atKey "currentPeriod" $
                Json.object $
                    (,,)
                        <$> Json.defaultKey "" "type" Json.text
                        <*> Json.atKey "start" Json.utcTime
                        <*> Json.atKey "end" Json.utcTime
            let usedPercent = max 0 (min 100 (floor creditUsagePercent))
                windowSeconds =
                    max 1 (round (diffUTCTime resetsAt startsAt))
            pure GrokUsageSnapshot
                { usedPercent
                , periodType
                , windowSeconds
                , resetsAt
                }

decodeGrokUsage :: LBS.ByteString -> Either Text GrokUsageSnapshot
decodeGrokUsage body = case
    Json.decodeEither grokUsageSnapshotDecoder (LBS.toStrict body) of
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
            $ setRequestHeader "X-XAI-Token-Auth"
                [Text.encodeUtf8 grokTokenAuthValue]
            $ setRequestHeader
                "x-userid"
                [Text.encodeUtf8 credential.accountId]
            $ setRequestHeader "Accept" ["application/json"]
            $ setRequestHeader "x-grok-client-mode" ["shell"]
            $ setRequestHeader "x-grok-client-version"
                [Text.encodeUtf8 defaultGrokClientVersion]
            $ setRequestHeader "User-Agent"
                [Text.encodeUtf8 (grokUserAgent defaultGrokClientVersion)]
            $ setRequestResponseTimeout
                (HttpClient.responseTimeoutMicro (30 * 1_000_000))
                request
