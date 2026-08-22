-- | Usage-window client for Grok subscription credentials.
module Agent.XAI.Usage
    ( GrokUsageSnapshot(..)
    , decodeGrokUsage
    , fetchGrokUsage
    ) where

import Control.Exception.Safe (tryAny)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as LBS
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock (UTCTime, diffUTCTime)
import qualified Network.HTTP.Client as HttpClient
import Network.HTTP.Simple

data GrokUsageSnapshot = GrokUsageSnapshot
    { usedPercent :: !Int
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
            creditUsagePercent <-
                config .: "creditUsagePercent" :: Parser Scientific
            currentPeriod <- config .: "currentPeriod"
            (startsAt, resetsAt) <-
                Aeson.withObject "Grok billing period" parsePeriod currentPeriod
            let usedPercent = max 0 (min 100 (floor creditUsagePercent))
                windowSeconds =
                    max 1 (round (diffUTCTime resetsAt startsAt))
            pure GrokUsageSnapshot
                { usedPercent
                , windowSeconds
                , resetsAt
                }

        parsePeriod period =
            (,) <$> period .: "start" <*> period .: "end"

decodeGrokUsage :: LBS.ByteString -> Either Text GrokUsageSnapshot
decodeGrokUsage body = case Aeson.eitherDecode body of
    Left _ ->
        Left "Grok returned an unreadable usage response."
    Right snapshot -> Right snapshot

fetchGrokUsage :: Text -> IO (Either Text GrokUsageSnapshot)
fetchGrokUsage accessToken =
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
        request <-
            parseRequest
                "https://cli-chat-proxy.grok.com/v1/billing?format=credits"
        httpLBS
            $ setRequestHeader
                "Authorization"
                ["Bearer " <> Text.encodeUtf8 accessToken]
            $ setRequestHeader "Accept" ["application/json"]
            $ setRequestHeader "x-grok-client-mode" ["shell"]
            $ setRequestHeader "x-grok-client-version" ["0.2.118"]
            $ setRequestResponseTimeout
                (HttpClient.responseTimeoutMicro (30 * 1_000_000))
                request
