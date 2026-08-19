-- | Configuration for the OpenRouter Responses transport.
module Agent.OpenRouter.Options
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
      -- ^ OpenRouter API prefix including the @/v1@ segment.
    , modelOverrides :: ![(Text, Text)]
      -- ^ Exact-match request-model to OpenRouter-model overrides.
    , defaultModel :: !Text
      -- ^ Target when the request model is absent or not an OpenRouter slug.
    , requestTimeoutSeconds :: !Int
      -- ^ Full-response timeout. Reasoning turns can stream for minutes.
    , httpReferer :: !(Maybe Text)
      -- ^ Optional @HTTP-Referer@ for OpenRouter app rankings.
    , appTitle :: !(Maybe Text)
      -- ^ Optional @X-Title@ for OpenRouter app rankings.
    } deriving (Eq, Show)

defaultClientOptions :: ClientOptions
defaultClientOptions = ClientOptions
    { baseUrl = "https://openrouter.ai/api/v1"
    , modelOverrides = []
    , defaultModel = "openai/gpt-5.1"
    , requestTimeoutSeconds = 600
    , httpReferer = Nothing
    , appTitle = Nothing
    }

-- | Load optional transport overrides from the environment.
clientOptionsFromEnv :: IO ClientOptions
clientOptionsFromEnv = do
    baseUrl <- lookupEnv "OPENROUTER_BASE_URL"
    modelMap <- lookupEnv "OPENROUTER_MODEL_MAP"
    defaultModel <- lookupEnv "OPENROUTER_DEFAULT_MODEL"
    timeoutSeconds <- lookupEnv "OPENROUTER_TIMEOUT_SECONDS"
    httpReferer <- lookupEnv "OPENROUTER_HTTP_REFERER"
    appTitle <- lookupEnv "OPENROUTER_APP_TITLE"
    pure ClientOptions
        { baseUrl = Maybe.fromMaybe defaultClientOptions.baseUrl (nonEmpty baseUrl)
        , modelOverrides = maybe [] (parseModelMap . Text.pack) (nonEmpty modelMap)
        , defaultModel = maybe defaultClientOptions.defaultModel Text.pack (nonEmpty defaultModel)
        , requestTimeoutSeconds = Maybe.fromMaybe defaultClientOptions.requestTimeoutSeconds
            (nonEmpty timeoutSeconds >>= readMaybeInt)
        , httpReferer = Text.pack <$> nonEmpty httpReferer
        , appTitle = Text.pack <$> nonEmpty appTitle
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
