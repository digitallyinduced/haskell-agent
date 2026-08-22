-- | Credit and key-limit inspection for an OpenRouter API key.
module Agent.OpenRouter.Usage
    ( OpenRouterUsage(..)
    , decodeCredits
    , decodeKeyInfo
    , fetchOpenRouterUsage
    ) where

import Control.Exception.Safe (tryAny)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.:?))
import qualified Data.ByteString.Lazy as LBS
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Client as HttpClient
import Network.HTTP.Simple

data OpenRouterUsage = OpenRouterUsage
    { keyLabel :: !(Maybe Text)
    , keyUsage :: !(Maybe Scientific)
    , keyLimit :: !(Maybe Scientific)
    , keyLimitRemaining :: !(Maybe Scientific)
    , isFreeTier :: !(Maybe Bool)
    , totalCredits :: !(Maybe Scientific)
    , totalUsage :: !(Maybe Scientific)
    }
    deriving (Eq, Show)

data KeyInfo = KeyInfo
    { label :: !(Maybe Text)
    , usage :: !(Maybe Scientific)
    , limit :: !(Maybe Scientific)
    , limitRemaining :: !(Maybe Scientific)
    , freeTier :: !(Maybe Bool)
    }

instance Aeson.FromJSON KeyInfo where
    parseJSON = Aeson.withObject "OpenRouter key response" \outer -> do
        value <- outer .: "data"
        Aeson.withObject "OpenRouter key data" parseData value
      where
        parseData object =
            KeyInfo
                <$> object .:? "label"
                <*> object .:? "usage"
                <*> object .:? "limit"
                <*> object .:? "limit_remaining"
                <*> object .:? "is_free_tier"

data Credits = Credits
    { credits :: !(Maybe Scientific)
    , used :: !(Maybe Scientific)
    }

instance Aeson.FromJSON Credits where
    parseJSON = Aeson.withObject "OpenRouter credits response" \outer -> do
        value <- outer .: "data"
        Aeson.withObject "OpenRouter credits data" parseData value
      where
        parseData object =
            Credits
                <$> object .:? "total_credits"
                <*> object .:? "total_usage"

decodeKeyInfo
    :: LBS.ByteString
    -> Either Text
        (Maybe Text, Maybe Scientific, Maybe Scientific, Maybe Scientific, Maybe Bool)
decodeKeyInfo body = case Aeson.eitherDecode body of
    Left _ ->
        Left "OpenRouter returned an unreadable key-usage response."
    Right KeyInfo{label, usage, limit, limitRemaining, freeTier} ->
        Right (label, usage, limit, limitRemaining, freeTier)

decodeCredits
    :: LBS.ByteString
    -> Either Text (Maybe Scientific, Maybe Scientific)
decodeCredits body = case Aeson.eitherDecode body of
    Left _ ->
        Left "OpenRouter returned an unreadable credits response."
    Right Credits{credits, used} -> Right (credits, used)

fetchOpenRouterUsage :: Text -> IO (Either Text OpenRouterUsage)
fetchOpenRouterUsage apiKey = do
    keyResult <- fetch "/key" decodeKeyInfo
    creditResult <- fetch "/credits" decodeCredits
    pure $ do
        (label, usage, limit, remaining, freeTier) <- keyResult
        (credits, used) <- creditResult
        pure OpenRouterUsage
            { keyLabel = label
            , keyUsage = usage
            , keyLimit = limit
            , keyLimitRemaining = remaining
            , isFreeTier = freeTier
            , totalCredits = credits
            , totalUsage = used
            }
  where
    fetch path decode = do
        result <- tryAny do
            request <- parseRequest ("https://openrouter.ai/api/v1" <> path)
            httpLBS
                $ setRequestHeader
                    "Authorization"
                    ["Bearer " <> Text.encodeUtf8 apiKey]
                $ setRequestHeader "Accept" ["application/json"]
                $ setRequestResponseTimeout
                    (HttpClient.responseTimeoutMicro (30 * 1_000_000))
                    request
        pure case result of
            Left _ ->
                Left
                    "Could not load OpenRouter usage. Check your connection and retry."
            Right response
                | let status = getResponseStatusCode response
                , status >= 200
                , status < 300 ->
                    decode (getResponseBody response)
                | otherwise ->
                    Left
                        ("OpenRouter usage returned HTTP "
                            <> Text.pack (show (getResponseStatusCode response)))
