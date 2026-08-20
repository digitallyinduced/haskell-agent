-- | Configuration for the xAI Grok subscription proxy.
module Agent.XAI.Options
    ( ClientOptions(..)
    , defaultClientOptions
    , clientOptionsFromEnv
    ) where

import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import System.Environment (lookupEnv)

data ClientOptions = ClientOptions
    { baseUrl :: !String
      -- ^ Subscription proxy base URL including the @/v1@ segment.
    , modelOverrides :: ![(Text, Text)]
      -- ^ Exact-match request-model to xAI-model overrides.
    , defaultModel :: !Text
      -- ^ Target for non-Grok model names without an explicit override.
    , requestTimeoutSeconds :: !Int
      -- ^ Full-response timeout. Reasoning turns can stream for minutes.
    , clientVersion :: !Text
      -- ^ Value for @x-grok-client-version@.
    } deriving (Eq, Show)

defaultClientOptions :: ClientOptions
defaultClientOptions = ClientOptions
    { baseUrl = "https://cli-chat-proxy.grok.com/v1"
    , modelOverrides = []
    , defaultModel = "grok-4.6"
    , requestTimeoutSeconds = 600
    , clientVersion = "0.2.118"
    }

-- | Load optional transport overrides from the environment.
clientOptionsFromEnv :: IO ClientOptions
clientOptionsFromEnv = do
    baseUrl <- lookupEnv "XAI_GROK_BASE_URL"
    modelMap <- lookupEnv "XAI_GROK_MODEL_MAP"
    defaultModel <- lookupEnv "XAI_GROK_DEFAULT_MODEL"
    timeoutSeconds <- lookupEnv "XAI_GROK_TIMEOUT_SECONDS"
    clientVersion <- lookupEnv "XAI_GROK_CLIENT_VERSION"
    pure ClientOptions
        { baseUrl = Maybe.fromMaybe defaultClientOptions.baseUrl (nonEmpty baseUrl)
        , modelOverrides = maybe [] (parseModelMap . Text.pack) (nonEmpty modelMap)
        , defaultModel = maybe defaultClientOptions.defaultModel Text.pack (nonEmpty defaultModel)
        , requestTimeoutSeconds = Maybe.fromMaybe defaultClientOptions.requestTimeoutSeconds
            (nonEmpty timeoutSeconds >>= readMaybeInt)
        , clientVersion = maybe defaultClientOptions.clientVersion Text.pack (nonEmpty clientVersion)
        }
  where
    nonEmpty (Just value) | not (null value) = Just value
    nonEmpty _ = Nothing

    readMaybeInt value = case reads value of
        [(number, "")] -> Just number
        _ -> Nothing

parseModelMap :: Text -> [(Text, Text)]
parseModelMap raw = Maybe.mapMaybe parseEntry (Text.splitOn "," raw)
  where
    parseEntry entry = case Text.breakOn "=" entry of
        (source, target)
            | not (Text.null (Text.strip source))
            , Just stripped <- Text.stripPrefix "=" target
            , not (Text.null (Text.strip stripped)) ->
                Just (Text.strip source, Text.strip stripped)
        _ -> Nothing
